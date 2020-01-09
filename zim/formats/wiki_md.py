# -*- coding: utf-8 -*-

'''This module handles parsing and dumping mardown wiki'''

from zim.parser import *
from zim.parsing import url_re, Re
from zim.formats import *
from zim.formats.plain import Dumper as TextDumper


WIKI_FORMAT_VERSION = 'zim md 0.4'

info = {
	'name': 'wikimd',
	'desc': 'Zim Markdown Format',
	'mimetype': 'text/x-zim-markdown',
	'extension': 'txt',
	'native': True,
	'import': True,
	'export': True,
	'usebase': True,
	'header_rule': True,
}


bullet_pattern = u'(?:[\\-\\+\\*\u2022]|\\[[ \\*x]\\]|\\d+\\.|[a-zA-Z]\\.)[\\ \\t]+'
	# bullets can be '+', '-', '*' or 0x2022 for normal items
	# and '[ ]', '[*]' or '[x]' for checkbox items
	# and '1.', '10.', or 'a.' for numbered items (but not 'aa.')

bullet_line_re = re.compile(ur'^(\t*)(%s)(.*\n)$' % bullet_pattern)
	# matches list item: prefix, bullet, text

number_bullet_re = re.compile(u'^(\d+|[a-zA-Z])\.$')
def check_number_bullet(bullet):
	'''If bullet is a numbered bullet this returns the number or letter,
	C{None} otherwise
	'''
	m = number_bullet_re.match(bullet)
	if m:
		return m.group(1)
	else:
		return None

param_re = re.compile('([\w-]+)=("(?:[^"]|"{2})*"|\S*)')
	# matches parameter list for objects
	# allow name="foo bar" and name=Foo

empty_lines_re = re.compile(r'((?:^[\ \t]*\n)+)', re.M | re.U)
	# match multiple empty lines

unindented_line_re = re.compile('^\S', re.M)
	# match any unindented line


def _remove_indent(text, indent):
	return re.sub('(?m)^'+indent, '', text)
		# Specify "(?m)" instead of re.M since "flags" keyword is not
		# supported in python 2.6


class WikiMDParser(object):
	# This parser uses 3 levels of rules. The top level splits up
	# paragraphs, verbatim paragraphs, images and objects.
	# The second level further splits paragraphs in lists and indented
	# blocks. The third level does the inline formatting for all
	# text.

	BULLETS = {
		'[ ]': UNCHECKED_BOX,
		'[x]': XCHECKED_BOX,
		'[*]': CHECKED_BOX,
		'*': BULLET,
	}

	def __init__(self):
		self.inline_parser = self._init_inline_parse()
		self.list_and_indent_parser = self._init_intermediate_parser()
		self.block_parser = self._init_block_parser()

	def __call__(self, builder, text):
		builder.start(FORMATTEDTEXT)
		self.block_parser(builder, text)
		builder.end(FORMATTEDTEXT)

	def _init_inline_parse(self):
		# Rules for inline formatting, links and tags
		return (
			Rule(LINK, url_re.r, process=self.parse_url)  # FIXME need .r atribute because url_re is a Re object
			| Rule(TAG, r'(?<!\S)@\w+', process=self.parse_tag)
			| Rule(LINK, r'(!?)<([\s\S]+?)>', process=self.parse_autolink)  # <link>
			| Rule(LINK, r'(!?\[((?:\[[^^\]]*\]|[^\[\]]|\]'
						r'(?=[^\[]*\]))*)\]\(\s*<?(.+?)>?'
						r'''(?:\s+['"]([\s\S]*?)['"])?\s*\))''', process=self.parse_link)
			| Rule(STRONG, r'_{2}(.+?)_{2}(?!_)'  # __word__
						r'|'
						r'\*{2}(.+?)\*{2}(?!\*)')  # **word**
			| Rule(EMPHASIS, r'_((?:__|.)+?)_|\*((?:\*\*|.)+?)\*(?!\*)')  # *word* or _word_
			| Rule(MARK, r'/{2}(.*?)/{2}')
			| Rule(SUBSCRIPT, r'_\{(?!~)(.+?)\}')
			| Rule(SUPERSCRIPT, r'\^\{(?!~)(.+?)\}')
			| Rule(STRIKE, r'~~(?!~)(.+?)~~')
			| Rule(VERBATIM, r"(?P<backtick>`{1,2})(?!')(.+?)(?P=backtick)", process=self.parse_verbatim)
		)

	def _init_intermediate_parser(self):
		# Intermediate level, breaks up lists and indented blocks
		# TODO: deprecate this by taking lists out of the para
		#       and make a new para for each indented block
		p = Parser(
			Rule(
				'X-Bullet-List',
				r'''(
					^ %s .* \n								# Line starting with bullet
					(?:
						^ \t* %s .* \n						# Line with same or more indent and bullet
					)*										# .. repeat
				)''' % (bullet_pattern, bullet_pattern),
				process=self.parse_list
			),
			Rule(
				'X-Indented-Bullet-List',
				r'''(
					^(?P<list_indent>\t+) %s .* \n			# Line with indent and bullet
					(?:
						^(?P=list_indent) \t* %s .* \n		# Line with same or more indent and bullet
					)*										# .. repeat
				)''' % (bullet_pattern, bullet_pattern),
				process=self.parse_list
			),
			Rule(
				'X-Indented-Block',
				r'''(
					^(?P<block_indent>\t+) .* \n			# Line with indent
					(?:
						^(?P=block_indent) (?!\t|%s) .* \n	# Line with _same_ indent, no bullet
					)*										# .. repeat
				)''' % bullet_pattern,
				process=self.parse_indent
			),
		)
		p.process_unmatched = self.inline_parser
		return p

	def _init_block_parser(self):
		# Top level parser, to break up block level items
		p = Parser(
			# Rule(VERBATIM_BLOCK,
			# 	r'^((?:(\ {4}|\t)[^\n]+\n)+)',
			# 	process=self.parse_pre
			# ),
			Rule(VERBATIM_BLOCK,r'''
				^\ *(?P<pre_tag>`{3,}|~{3,})\ *(\S+)?\ *\n	# ```lang
				([\s\S]+?)\s*									# code
				^(?P=pre_tag)\ *(?:\n|$)						# matching opening tag
			''',
				process=self.parse_fancy
			),
			Rule(OBJECT, r'''
				^(?P<obj_indent>\t*) \{\{\{ \s*? (\S+:.*\n)		# "{{{ object_type: attrib=..."
				( (?:^.*\n)*? ) 								# multi-line body
				^(?P=obj_indent) \}\}\} \s*? \n					# "}}}" with matching indent
				''',
				process=self.parse_object
			),
			Rule(HEADING,
				 r'^(\#{1,6}\s*)([^\n]+?)(\ *\#*\ *)(?:\n|$)',
				 process=self.parse_heading
			),
			Rule(HEADING,
				 r'^([^\n]+)(\n\ *(?:=|-)+\ *)(?:\n|$)',
				 process=self.parse_lheading
			),
			# standard table format
			Rule(TABLE, r'''
				^(\|.*\|)$\n								# starting and ending with |
				^( (?:\| [ \|\-:]+ \|$\n)? )				# column align
				( (?:^\|.*\|$\n)+ )							# multi-lines: starting and ending with |
				^(?= \s*? \n)							# empty line / only spaces
				''',
				process=self.parse_table
			)
		)
		p.process_unmatched = self.parse_para
		return p

	@staticmethod
	def parse_heading(builder, ltag, title, rtag):
		'''Parse heading and determine it's level'''
		assert ltag.startswith('#')
		builder.append(HEADING, {'level': ltag.count('#'), 'ltag': ltag, 'rtag': rtag}, title)

	@staticmethod
	def parse_lheading(builder, title, lheader):
		'''Parse heading and determine it's level'''
		assert lheader.find('===') > -1 or lheader.find('---') > -1
		nb_level = 1 if lheader.find('===') > -1 else 2
		builder.append(HEADING, {'level': nb_level, 'lheader': lheader}, title)

	@staticmethod
	def parse_verbatim(builder, tag, text):
		'''Parse inline verbatim'''
		assert tag.startswith('`')
		builder.append(VERBATIM, {'nb_backtick': len(tag)}, text)

	@staticmethod
	def parse_fancy(builder, tag, text, code=None):
		'''Verbatim block with indenting'''
		if not code:
			code = text
			text = ''
		attrib = {}
		if tag:
			attrib['tag'] = tag
		if text:
			attrib['lang'] = text

		builder.append(VERBATIM_BLOCK, attrib, code)

	@staticmethod
	def parse_pre(builder, text, indent):
		'''Verbatim block with indenting'''
		if indent:
			text = re.sub('^' + indent, '', text, flags=re.M)  # remove indent
			attrib = {'indent-tag': indent}
		else:
			attrib = None

		builder.append(VERBATIM_BLOCK, attrib, text)

	def parse_object(self, builder, indent, header, body):
		'''Custom object'''
		type, param = header.split(':', 1)
		type = type.strip().lower()

		attrib = {}
		for match in param_re.finditer(param):
			key = match.group(1).lower()
			value = match.group(2)
			if value.startswith('"') and len(value) > 1: # Quoted string
				value = value[1:-1].replace('""', '"')
			attrib[key] = value

		# Defined after parsing head, so these attrib can not be overruled
		# accidentally
		### FIXME FIXME FIXME - need to separate two types of attrib ###
		attrib['type'] = type
		if indent:
			body = _remove_indent(body, indent)
			attrib['indent'] = len(indent)
		builder.append(OBJECT, attrib, body)

	def check_multi_attribute(self, attrib, key, default, list_length):
		'''
		Correct multi-attributes, so they do fit with column length of table
		:param attrib: key-value store
		:param key: key to select of attribute
		:param default: default value for one list entry
		:param list_length: required length of selected attribute
		:return: attribute-value as list of different options
		'''
		if attrib and key in attrib and attrib[key]:
			values = attrib[key].split(',')
		else:
			values = []

		while len(values) > list_length:
			values.pop()
		while len(values) < list_length:
			values.append(default)
		return ','.join(values)

	def parse_table(self, builder, headerrow, alignstyle, body):
		'''Table parsing'''
		body = body.replace('\\|', '#124;')  # escaping
		rows = body.split('\n')[:-1]
		# get maximum number of columns - each columns must have same size
		nrcols = max([headerrow.count('|')]+[row.count('|') for row in rows])-1

		# transform header separator line into alignment definitions
		aligns = []
		while alignstyle.count('|') < nrcols+1:
			alignstyle += '|'  # fill cells thus they match with nr of headers
		for celltext in alignstyle.split('|')[1:-1]:
			celltext = celltext.strip()
			if celltext.startswith(':') and celltext.endswith(':'):
				alignment = 'center'
			elif celltext.startswith(':'):
				alignment = 'left'
			elif celltext.endswith(':'):
				alignment = 'right'
			else:
				alignment = 'normal'
			aligns.append(alignment)

		# collect wrap settings from first table row
		headers = []
		wraps = []
		for celltext in headerrow.split('|')[1:-1]:
			if celltext.rstrip().endswith('<'):
				celltext = celltext.rstrip().rstrip('<')
				wraps.append(1)
			else:
				wraps.append(0)
			headers.append(celltext)

		attrib = {'aligns': ','.join(aligns), 'wraps': ','.join(map(str, wraps))}


		# build table
		builder.start(TABLE, attrib)

		builder.start(HEADROW)
		for celltext in headers:
			celltext = celltext.replace('#124;', '|').replace('\\n', '\n').strip()
			if not celltext:
				celltext = ' '  # celltext must contain at least one character
			builder.append(HEADDATA, {}, celltext)
		builder.end(HEADROW)

		for bodyrow in rows:
			while bodyrow.count('|') < nrcols+1:
				bodyrow += '|'  # fill cells thus they match with nr of headers
			builder.start(TABLEROW)
			for celltext in bodyrow.split('|')[1:-1]:
				builder.start(TABLEDATA)
				celltext = celltext.replace('#124;', '|').replace('\\n', '\n').strip()  # cleanup cell
				if not celltext:
					celltext = ' '  # celltext must contain at least one character
				self.inline_parser(builder, celltext)
				builder.end(TABLEDATA)
			builder.end(TABLEROW)

		builder.end(TABLE)

	def parse_para(self, builder, text):
		'''Split a text into paragraphs and empty lines'''
		if text.isspace():
			builder.text(text)
		else:
			for block in empty_lines_re.split(text):
				if not block: # empty string due to split
					pass
				elif block.isspace():
					builder.text(block)
				elif self.backward \
				and not unindented_line_re.search(block):
					# Before zim 0.29 all indented paragraphs were
					# verbatim.
					builder.append(VERBATIM_BLOCK, None, block)
				else:
					builder.start(PARAGRAPH)
					self.list_and_indent_parser(builder, block)
					builder.end(PARAGRAPH)

	def parse_list(self, builder, text, indent=None):
		'''Parse lists into items and recurse to get inline formatting
		per list item
		'''
		if indent:
			text = _remove_indent(text, indent)
			attrib = {'indent': len(indent)}
		else:
			attrib = None

		lines = text.splitlines(True)
		self.parse_list_lines(builder, lines, 0, attrib)

	def parse_list_lines(self, builder, lines, level, attrib=None):
		listtype = None
		first = True
		while lines:
			line = lines[0]
			m = bullet_line_re.match(line)
			assert m, 'Line does not match a list item: >>%s<<' % line
			prefix, bullet, text = m.groups()
			bullet = bullet.rstrip()

			if first:
				number = check_number_bullet(bullet)
				if number:
					listtype = NUMBEREDLIST
					if not attrib:
						attrib = {}
					attrib['start'] = number
				else:
					listtype = BULLETLIST
				builder.start(listtype, attrib)
				first = False

			mylevel = len(prefix)
			if mylevel > level:
				self.parse_list_lines(builder, lines, level+1) # recurs
			elif mylevel < level:
				builder.end(listtype)
				return
			else:
				if listtype == NUMBEREDLIST:
					attrib = None
				elif bullet in self.BULLETS: # BULLETLIST
					attrib = {'bullet': self.BULLETS[bullet]}
				else: # BULLETLIST
					attrib = {'bullet': BULLET}
				builder.start(LISTITEM, attrib)
				self.inline_parser(builder, text)
				builder.end(LISTITEM)

				lines.pop(0)

		builder.end(listtype)

	def parse_indent(self, builder, text, indent):
		'''Parse indented blocks and turn them into 'div' elements'''
		text = _remove_indent(text, indent)
		builder.start(BLOCK, {'indent': len(indent)})
		self.inline_parser(builder, text)
		builder.end(BLOCK)

	@staticmethod
	def parse_autolink(builder, prefix, href):
		if prefix == '!':
			WikiMDParser.parse_image(builder, '', '', href, '')
			return
		builder.append(LINK, {'href': href, 'brackets': True}, href)

	@staticmethod
	def parse_link(builder, text, name, href, title=None):  # TODO: name is to be used when page is not edited
		if text[0] == '!':
			WikiMDParser.parse_image(builder, text, name, href, title)
			return
		attribs = {}
		if title:
			attribs['title'] = title
		if not name:
			name = href
		attribs['name'] = name
		if not href or href == '':
			href = text
		attribs['href'] = href
		if '<' in text and '>' in text:
			attribs['brackets'] = True
		# builder.append(LINK, attribs, text)
		builder.append(LINK, attribs, name)

	@staticmethod
	def parse_image(builder, text, name, url, title=None):
		if not url or url == '':
			url = text
		attribs = ParserClass.parse_image_url(url)
		if title:
			attribs['alt'] = title
		if name:
			attribs['name'] = name

		builder.append(IMAGE, attribs)

	@staticmethod
	def parse_url(builder, text):
		builder.append(LINK, {'href': text}, text)

	@staticmethod
	def parse_tag(builder, text):
		builder.append(TAG, {'name': text[1:]}, text)


wikiparser = WikiMDParser()  #: singleton instance


# FIXME FIXME we are redefining Parser here !
class Parser(ParserClass):

	def __init__(self, version=WIKI_FORMAT_VERSION):
		self.backward = False

	def parse(self, input, partial=False):
		if not isinstance(input, basestring):
			input = ''.join(input)

		if not partial:
			input = fix_line_end(input)

		builder = ParseTreeBuilder(partial=partial)
		wikiparser.backward = self.backward # HACK
		wikiparser(builder, input)
		return builder.get_parsetree()


class Dumper(TextDumper):

	BULLETS = {
		UNCHECKED_BOX:	u'[ ]',
		XCHECKED_BOX:	u'[x]',
		CHECKED_BOX:	u'[*]',
		BULLET:			u'*',
	}

	TAGS = {
		EMPHASIS:		('_', '_'),
		STRONG:			('**', '**'),
		MARK:			('//', '//'),
		STRIKE:			('~~', '~~'),
		VERBATIM:		('``', '``'),
		TAG:			('', ''), # No additional annotation (apart from the visible @)
		SUBSCRIPT:		('_{', '}'),
		SUPERSCRIPT:	('^{', '}'),
	}

	def dump_pre(self, tag, attrib, strings):
		# Indent and wrap with "'''" lines
		strings.insert(0, '```\n')
		strings.append('```\n')
		strings = self.dump_indent(tag, attrib, strings)
		return strings

	def dump_h(self, tag, attrib, strings):
		assert 'level' in attrib, \
			'BUG: h? misses level: %s "%s"' % (attrib, strings)
		level = int(attrib['level'])
		if level < 1:
			level = 1
		elif level > 5:
			level = 5
		strings.insert(0, level * '#' + ' ')
		return strings

	def dump_link(self, tag, attrib, strings=None):
		assert 'href' in attrib, \
			'BUG: link misses href: %s "%s"' % (attrib, strings)
		href = attrib['href']

		if not strings or href == u''.join(strings):
			if url_re.match(href) and not attrib.get('brackets'):
				return (href,) # no markup needed
			else:
				return ('<', href, '>')
		else:
			if attrib.get('brackets'):
				href = '<' + href + '>'
			if attrib.get('title'):
				return ('[', ) + tuple(strings) + ('](', href, ' "', attrib.get('title'), '")',)
			else:
				return ('[', ) + tuple(strings) + ('](', href, ')',)

	def dump_img(self, tag, attrib, strings=None):
		src = attrib['src']
		alt = attrib.get('alt')
		opts = []
		items = sorted(attrib.items())
		for k, v in items:
			if k in ('src', 'alt') or k.startswith('_'):
				continue
			elif v: # skip None, "" and 0
				data = url_encode(unicode(v), mode=URL_ENCODE_DATA)
				opts.append('%s=%s' % (k, data))
		if opts:
			src += '?%s' % '&'.join(opts)

		if alt:
			return ('![', alt, '](', src, ')')
		else:
			return('![](', src, ')')

		# TODO use text for caption (with full recursion)

	def dump_object(self, tag, attrib, strings=None):
		logger.debug("Dumping object: %s, %s", attrib, strings)
		assert "type" in attrib, "Undefined type of object"

		opts = []
		for key, value in attrib.items():
			if key in ('type', 'indent') or value is None:
				continue
			# double quotes are escaped by doubling them
			opts.append(' %s="%s"' % (key, str(value).replace('"', '""')))

		if not strings:
			strings = []
		return ['{{{', attrib['type'], ':'] + opts + ['\n'] + strings + ['}}}\n']

		# TODO put content in attrib, use text for caption (with full recursion)
		# See img

	def dump_table(self, tag, attrib, strings):
		# logger.debug("Dumping table: %s, %s", attrib, strings)

		table = []  # result table
		rows = strings

		aligns, wraps = TableParser.get_options(attrib)
		maxwidths = TableParser.width2dim(rows)
		headsep = TableParser.headsep(maxwidths, aligns, x='|', y='-')
		def rowline(row): return TableParser.rowline(row, maxwidths, aligns)

		# print table
		table += [TableParser.headline(rows[0], maxwidths, aligns, wraps)]
		table.append(headsep)
		table += [rowline(row) for row in rows[1:]]
		return map(lambda line: line+"\n", table)

	def dump_thead(self, tag, attrib, strings):
		return [strings]

	def dump_th(self, tag, attrib, strings):
		strings = map(lambda s: s.replace('\n', '\\n').replace('|', '\\|'), strings)
		return strings

	def dump_trow(self, tag, attrib, strings):
		return [strings]

	def dump_td(self, tag, attrib, strings):
		strings = map(lambda s: s.replace('\n', '\\n').replace('|', '\\|'), strings)
		strings = map(lambda s: s.replace('<br>', '\\n'), strings)
		return strings


class AutoFormatRegexes(AutoFormatRegexesClass):
	heading_re = Re(r'^(#{1,7})\s*(.*)\s*(\1)?$')
	page_re = Re(r'''(
		  [\w\.\-\(\)]*(?: :[\w\.\-\(\)]{2,} )+:?
		| \+\w[\w\.\-\(\)]+(?: :[\w\.\-\(\)]{2,} )*:?
	)$''', re.X | re.U) # e.g. namespace:page or +subpage, but not word without ':' or '+'
	interwiki_re = Re(r'\w[\w\+\-\.]+\?\w\S+$', re.U) # name?page, where page can be any url style
	file_re = Re(r'''(
		  ~/[^/\s]
		| ~[^/\s]*/
		| \.\.?/
		| /[^/\s]
	)\S*$''', re.X | re.U) # ~xxx/ or ~name/xxx or ../xxx  or ./xxx  or /xxx

	markup_re = {#'style-strong' : Re(r'(\*{2})(.*)\1'),
				'style-strong' : Re(r'(_{2}|\*{2})(.*?)\1'),  # __word__ or **word**
				#'style-emphasis' : Re(r'(_|\*)(.*?)\1'),
				 'style-emphasis' : Re(r'(_|\*)((?!\1).+?)\1'),  # _word_ or *word*
				'style-mark' : Re(r'(\/{2})(.*)\1'),
				'style-pre' : Re(r'(\`{1,2})(.*)\1'),
				'style-strike' : Re(r'(~{2})(.*)\1')}

	tag_re = Re(r'^(@\w+)$', re.U)

			#| Rule(EMPHASIS, r'_((?:__|.)+?)_|\*((?:\*\*|.)+?)\*(?!\*)')  # *word* or _word_
