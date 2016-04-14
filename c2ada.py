# c2Ada hosted on https://github.com/elgor/c2ada
#
# Quick & dirty python script to translate ansi C to Ada.
# The script is based on regular expressions and will not properly parse c according to its specification.
# Patterns are recognized and substituted to Ada expressions


# upper bound for recursive block search
C2ADA_MAX_NESTED_BLOCKS = 15

# own replace patterns
reAvrExtra = [('sei\(\);', 'Interrupt_dis/en'), ('cli\(\);', 'Interrupt_dis/en')]
reExtras = reAvrExtra


# regular expressions to find certain patterns
# ==============================================================

reNL = '(?<=[\n])'    # newline
reBC = '('            # begin capture
reEC = ')'            # end capture
reSpace = '(?:[ \t]+)'    # whitespace
reS = '(?:[ \t]*?)'   # optional whitespace


reIndet = reNL + '(?:[ \t]*)'
reIndetC = '(^[ \t]*)'
reIndetBlock = '(?<=\n)([ \t]*((?:if|for|else if)[^\n]*?\)|else)[ \t]*\n[ \t]+[^;\n]*;[^{};\n]*\n)'

# indntifier
reId = '(?:(?<!\w)[_a-zA-Z][_a-zA-Z0-9]{0,40}(?!\w))' 
reNum = '(?<![.a-z])[0-9]{0,30}[.]?[0-9]{1,30}(?![.a-z])'
reConst = '(?:(?<!\w)[_A-Z0-9]{0,40}(?!\w))'
reOp = '(?:[+-*/])'
reRel = '(?:==|!=|<|<=|>|>=)'

reP = '(?:\*?)'

rePreType = '(?:extern|static|const|signed|unsigned|volatile)'
rePreTypeList = '(?:'+rePreType+'\s+)*'
reType = '(?:int|float|struct|enum)'

reFS = '(?:[ \t]*--[^\n]*\n)'	# fill space, ignoring comments, one line
reFL = '(?:\s|--[^\n]*\n)*'	# fill space, ignoring comments, several lines
reE = '(?:(?<=[;:{}])|(?<=\A)|(?<=>>))(?:\s|--[^\n]*\n)*'		# previos command + whitespace


reDeclaration = reNL+reBC+reS+'('+rePreTypeList+reId+')'+reSpace+'('+reId+')'+reS+'(?:([ \t]*:?=[^;]*))?(;[ \t]*\r?\n|;'+reFS+'))'
# full regex: reDeclaration = '(?<=[\n])((?:[ \t]*)((?:(?:extern|static|const|signed|unsigned|volataile)\s+)*(?:(?<!\w)[_a-zA-Z][_a-zA-Z0-9]{0,40}(?!\w)))[ \t]+((?:(?<!\w)[_a-zA-Z][_a-zA-Z0-9]{0,40}??!\w)))[ \t]*?(?:([ \t]*=[^;]*))?(;[ \t]* \n|[ \t]*--[^ \n]* \n))'
reDecInline = '(?<=[(;,)])((?:[ \t]*)('+rePreTypeList+reId+')[ \t]+('+reId+')[ \t]*?()([,;)]))'
reDecArray = reNL+reBC+reS+'('+rePreTypeList+reId+')'+reSpace+'('+reId+')'+reS+'\['+reConst+'\]'

reFunction = reE+'(('+rePreTypeList+reId+'\*?'+')'+reFL+'('+'\*?'+reId+')'+'(?:\s*\(([^{;]*)\))'+reFL+'(\{[^{}]*\}|;))'
# full regex: reFunction = '(?:(?<=[;:{}])|(?<=\A))(?:\s|--[^\n ]*\n )*(((?:(?:extern|static|const|signed|unsigned|volataile)\s+)*(?:(?<!\w)[_a-zA-Z][_a-zA-Z0-9]{0,40}(?!\w))\*?)(?:\s|--[^ \n]* \n)*(\*?(?:(?<!\w)[_a-zA-Z][_a-zA-Z0-9]{0,40}(?!\w)))(?:\s*\([^{;]*\))(?:\s|--[^ \n]* \n)*(\{[^{}]*\}|;))'



# substitution Lists
# ===================================================
subStdTypes = [('(?<!\w)unsigned\s+int(?!\w)', 'Natural'), ('(?<!\w)int(?!\w)', 'Integer'), ('(?<!\w)float(?!\w)', 'Float'), ('(?<!\w)bool(?!\w)', 'Boolean'), ('(?<!\w)(?:unsigned\s+|signed\s+)?char(?!\w)', 'Character')]
subEmbeddedTypes = [('(?<!\w)uint8_t(?!\w)', 'Unsigned_8'), ('(?<!\w)uint16_t(?!\w)', 'Unsigned_16'), ('(?<!\w)uint32_t(?!\w)', 'Unsigned_32'), ('(?<!\w)int8_t(?!\w)', 'Integer_8'), ('(?<!\w)int16_t(?!\w)', 'Integer_16'), ('(?<!\w)int32_t(?!\w)', 'Integer_32')]
subTypes = subStdTypes + subEmbeddedTypes


subOperators = [('(?<=[^=!/<>:])(=)(?=[^=>])', ':='), ('==', '='), ('!=', '/='), ('!=', '/=')]
subBooleans = [('\s?&&\s?', ' and then '), ('\s?\|\|\s?', ' or else '), ('\s?&\s', ' and '), ('\s?\|\s?', ' or '), ('\s?\~\s?', ' not ')]


# helper Lists
# ===================================================
assignExtra = ['+', '-', '*', '/', '&', '|', '~']      # operators before assignment, e.g. "a += 1"


# global variables
adaNotCKeywords = ['abort', 'abs', 'elsif', 'not', 'reverse', 'abstract', 'accept', 'entry', 'access', 'exception' 
	'of', 'separate', 'aliased', 'exit', 'or', 'some', 'all', 'others', 'subtype', 'and', 'out', 'synchronized', 'array' 
	'function', 'overriding', 'at', 'tagged', 'generic', 'package', 'task', 'begin', 'pragma', 'terminate', 'body' 
	'procedure', 'type', 'in', 'constant', 'interface', 'until', 'is', 'raise', 'use', 'declare'	'range', 'delay' 
	'limited', 'record', 'when', 'delta', 'loop', 'rem', 'digits', 'renames', 'with', 'mod', 'requeue', 'xor' ]	


# global lists required for resolving names between several files
includeList = []
functionList = []


def readInputFile( fileName ):
	# Open file as file object and read to string
	sourceFile = open(fileName,'r')

	# Read file object to string
	sourceText = sourceFile.read()

	# Close file object
	sourceFile.close()

	# return
	return sourceText


def preProcess(text):
	log.debug("Preprocessing... (resolve #-statements)")
	includes = []
	matches = re.findall('(#include[ \t]*[<"]([\w,\s/-]+)\.h[">])', text)
	for match in matches:
		includes.append(match[1])
		newblock = 'with '+match[1]+';'
		newblock = re.sub('(?<=\w|\d)(/)(?=\w|\d)', '.', newblock)
		text = re.sub(re.escape(match[0]), newblock, text)

	matches = re.findall('(#define[ \t]*('+reId+')[ \t]*([^\r\n]*?))(?:\r?\n|--)', text)
	for match in matches:
		if match[2] != '':
			text = re.sub(re.escape(match[0]), match[1]+' : constant := '+match[2]+';', text)

	matches = re.findall('((#ifdef[ \t]*'+reId+')[^\n]*)\n', text)
	for match in matches:
		text = re.sub(re.escape(match[0]), match[1]+' -- C2ADA: use gnatprep for conditional compilation!', text)

	return (text, includes)


def removeHeaderGuards(text):
	guard = re.search('#ifndef[ \t]+([\w\d_]+_H_?) *\r?\n', text)
	if guard:
		log.debug("Removing Header Guards")
		guard = guard.group(1)
		text = re.sub('#ifndef[ \t]+'+guard+' *\r?\n', '', text)
		text = re.sub('#define[ \t]+'+guard+' *\r?\n', '', text)
		text = re.sub('#endif[ \t]+[^\r\n]*?'+guard+'[^\r\n]*?\r?\n', '', text)
	return text


def getIndent(string):
	whitespace = re.search(reIndetC, string)
	if whitespace:
		return whitespace.group(0)


def resolveIndentBlock(text):
	matches = re.findall(reIndetBlock, text)
	for match in reversed(matches):
		block = match[0]
		newblock = block
		indent = getIndent(block)
		newblock = re.sub(re.escape(match[1]), match[1]+'{', newblock)
		newblock = re.sub(';[^{};\n]*\n', ';\n'+indent+'}\n', newblock)
		text = re.sub(re.escape(block), newblock, text)
	return text


# Ada does not distinguish capital and lower case letters
def resolveCases(text):
	print "get all identifiers and compare for case"
	matches = re.findall(reId, text)
	for match in matches:
		identifier = match

		# find different cases
		for identifier2 in matches:
			if (identifier.lower() == identifier2.lower()) and (identifier != identifier2):
				newidentifier = identifier2.capitalize()
				text = re.sub('(?<!\w)'+identifier2+'(?!\w)', 'a'+newidentifier, text)

		identifier = re.sub('_{2,}|(?<![\w\d])_|_(?![\w\d])', '', identifier)	# remove wrong _
	return text


def resolveAdaKeywords(text):
	for keyword in adaNotCKeywords:
		text = re.sub('(?<![_\w\d])(?i)'+keyword+'(?![_\w\d])', 'a'+keyword, text, re.I)

	# remove "_" at the beginning or end of variables
	matches1 = re.findall('(?<!\w)(\_+(\w(?:\w|\d)*(?!\w)))', text)
	matches2 = re.findall('(?<!\w)((\w(?:\w|\d)*)\_+)(?!\w)', text)
	matches = matches1 + matches2
	for match in matches:
		text = re.sub(re.escape(match[0]), match[1], text)

	# replace equivalent keywords
	text = re.sub('(?<![\w\d_])break\s*;', 'exit;', text)

	return text




def replaceComments(text):
	matches = re.findall('(([ \t]*)/\*(?:.(?!/\*))*?\*/)([^\n]*\n)', text, re.DOTALL)


	for match in matches:
		block = match[0]
		indent = match[1]
		rest = match[2]
		newblock = re.sub('/\*','--', block)
		newblock = re.sub('\n\s*\*?(?!/)', '\n'+indent+'--', newblock)
		if re.search('[^\s]', rest):
			newblock = re.sub('\*/','\n', newblock)
		else:
			newblock = re.sub('\*/', '', newblock)
		newblock = re.sub('\\\\', '\\\\\\\\', newblock)	# escapeception

		text = re.sub(re.escape(block), newblock, text)
	return re.sub('//', '--', text)



def replaceNestedBlocks(text, counter=0):
	log.debug("Searching for non nested blocks")
	ifloopswitch = re.findall('((?<![\w\d_])(?:if|for|while|switch)\s*\([^{}]+?\)\s*?\{[^{}]*\})', text)
	#dos = re.findall('(do'+reFL+'\{[^{}]*\})', text)
	compounds = re.findall(reE+'\{[^\{\}]*\}', text)
	print "Nesting depth: "+str(counter)
	matches = ifloopswitch + compounds # +dos
	if not matches:
		return text
	for match in matches:
		if counter > C2ADA_MAX_NESTED_BLOCKS:
			log.error("Maximum number of nested Blocks reached, there might be a problem with unbalanced brackets: {...}")
			return text
		block = match
		newblock = replaceNonNestedBlock(match)
		text = re.sub(re.escape(block), newblock, text)

	counter += 1	
	text = replaceNestedBlocks(text, counter)
	return text	






def replaceNonNestedBlock(block):
	block = replaceIf(block)
	block = replaceLoops(block)
	block = replaceSwitch(block)
	block = replaceCompound(block)
	return block



def replaceDeclaration(text):
	matches1 = re.findall(reDeclaration, text)	# matches return var;
	#matches2 = re.findall(reDecInline, text)
	text = replaceInlineDeclaration(text)
	matches = matches1 #+ matches2

	for match in matches:
		declaration = match[0]
		indent = getIndent(declaration)
		atype = match[1]
		variable = match[2]
		assign = match[3]
		rest = match[4]
		if atype not in adaNotCKeywords+['return', 'if', 'for', 'while', 'end']:
			text = re.sub(re.escape(match[0]), indent+variable+' : '+atype+assign+rest, text)
	return text

def replaceInlineDeclaration(text):
	matches = re.findall(reDecInline, text)
	for match in matches:
		declaration = match[0]
		indent = getIndent(declaration)
		atype = match[1]
		atype = re.sub('(?<!\w)const(?!\w)', 'in', atype)
		variable = match[2]
		rest = match[4]
		if atype not in adaNotCKeywords+['return', 'if', 'for', 'while', 'end']:
			text = re.sub(re.escape(match[0]), indent+variable+' : '+atype+rest, text)
	return text

def findDeclarationLines(block):
	matches = re.findall(reDeclaration, block)
	declarations = []
	for match in matches:
		atype = match[1]
		if atype not in adaNotCKeywords+['return', 'if', 'for', 'while', 'end']:
			declarations.append(match[0])
	return declarations


def replaceAssignments(text):
	for assign in assignExtra:
		matches = re.findall(reE+'(('+reId+')'+reFL+re.escape(assign)+'= ?)', text)
		for match in reversed(matches):
			text = re.sub(re.escape(match[0]), match[1]+' = '+match[1]+' '+assign+' ', text)
	return text


def replaceTypes(text):
	for types in subTypes:
		text = re.sub(types[0], types[1], text)
	return text

def replaceCasts(text):
	for types in subTypes:
		text = re.sub('\( ?'+types[1]+' ?\)', types[1], text)
	return text	

def replaceIndDec(text):
	matches1 = re.findall('('+reId+' *(\+\+|\-\-))', text)
	matches2 = re.findall('((\+\+|\-\-) *'+reId+')', text)
	matches = matches1 + matches2
	for match in matches:
		newblock = match[0]+' = '+match[0]+' '+match[1][0]+' 1'
		newblock = re.sub('(\+\+|\-\-)', '', newblock);
		text = re.sub(re.escape(match[0]), newblock, text)
	return text

def replaceTypeDef(text):
	matches = re.findall(reIndet+'(typedef[ \t]+('+rePreTypeList+reId+')[ \t]+('+reId+');)', text)
	log.debug("Replacing typedef")
	for match in matches:
		block = match[0]
		newblock = 'type '+match[2]+' is new '+match[1]+';'
		text = re.sub(re.escape(block), newblock, text)
	return text


def replaceEnum(text):
	matches = re.findall('(?<![\w\d_])(enum\s*('+reId+')\s*\{([^\{\}]*)\})', text)
	for match in matches:
		block = match[0]
		newblock = 'type '+match[1]+' is ('+match[2]+')'
		text = re.sub(re.escape(block), newblock, text)
	return text


def replaceStructs(text):
	matches = re.findall('((?<![\w\d_])struct (\S*)\s*?\{[^\{\}]*\})', text)
	if not matches:
		return text

	for match in reversed(matches):
		structname = match[1]
		block = match[0]
		newblock = block
		newblock = re.sub('struct +'+structname, 'type '+structname, newblock)
		newblock = re.sub('\s*\{', ' is\nrecord', newblock)
		newblock = re.sub('\}', 'end record', newblock)
		text = re.sub(re.escape(block), newblock, text)

	# search recursive
	#text = replaceFunctions(text)
	return text 	


def replaceArrayDelaration(text):
	matches = re.findall(reNL+'('+reS+'('+rePreTypeList+reId+')'+reSpace+'('+reId+')'+reS+'\[('+reConst+')\])', text)
	for match in matches:
		block = match[0]
		newblock = 'type '+match[2]+'_Type is array (1 .. '+match[3]+') of '+match[1]+'\n'+match[2]+' : '+match[2]+'_Type'
		text = re.sub(re.escape(block), newblock, text)
	return text




def replaceElse(text):
	# replace else blocks:
	text = re.sub('(?<![\w\d_])else\s+if(?!=[\w\d_])', 'elsif', text)
	text = re.sub('}?\s*else\s*{?', 'else', text)
	matches = re.findall('(}\s*elsif\s*\([^{}]+?\)\s*?\{)', text)
	for match in matches:
		block = match
		newblock = block
		newblock = re.sub('\)\s*{', ' then ', newblock)		
		newblock = re.sub('}\s*elsif\s*\( *', 'elsif ', newblock)
		text = re.sub(re.escape(block), newblock, text)
	# FIX: comments in between
	return text

def replaceIf(text):
	# find if block
	matches = re.findall('((?<![\w\d_])if\s*\([^{}]+?\)\s*?\{[^{}]*\})', text)  # (?! elsif)

	for match in reversed(matches):
		block = match
		newblock = block
		newblock = re.sub('\)\s*{', ' then ', newblock)
		newblock = re.sub('if\s*\( *', 'if ', newblock)
		newblock = re.sub('}', 'end if;', newblock)
		text = re.sub(re.escape(block), newblock, text)
	
	return text


def replaceLoops(text):
	matches = re.findall('(?<![\w\d_])((?:while|for)\s*\([^{}]+?\)\s*?\{[^{}]*\})', text)
	for match in reversed(matches):
		block = match
		newblock = block
		if block[0] == 'f':
			newblock = re.sub('\s*\{', ' loop -- C2ADA: Think of better ada expression', newblock)			
		else:
			newblock = re.sub('while\s*\(', 'while ', newblock)
			newblock = re.sub('\)\s*\{', ' loop', newblock)
		newblock = re.sub('\}', 'end loop;', newblock)
		text = re.sub(re.escape(block), newblock, text)
	return text

def replaceSwitch(text):
	matches = re.findall('(?<![\w\d_])(switch\s*\([^{}]+?\)\s*?\{[^{}]*\})', text)
	for match in matches:
		block = match
		newblock = block
		cases = re.findall('(?<![\w\d_])(case\s+('+reId+'|'+reNum+')\s*:)', block)
		for case in cases:
			newblock = re.sub(re.escape(case[0]), 'when '+case[1]+' =>', newblock) 

		#newblock = re.sub('(?<![\w\d_])case\s+', 'when ', newblock)	
		newblock = re.sub('(?<![\w\d_])switch\s*\(', 'case ', newblock)
		newblock = re.sub('(?<![\w\d_])default\s*:', 'when others =>', newblock)
		newblock = re.sub('\)\s*{', ' is ', newblock)
		#newblock = re.sub('\s*:(?!\=)', ' =>', newblock)
		newblock = re.sub('\}', 'end case;', newblock)
		newblock = re.sub(reE+'exit;[^\n]*', '', newblock)
		text = re.sub(re.escape(block), newblock, text)
	return text

def replaceCompound(text):
	matches = re.findall('('+reE+'\{[^{}]*\})', text)
	for match in matches:
		block = match
		indent = getIndent(re.search('[ \t]*\{', block).group(0))
		newblock = block
		newblock = re.sub('[ \t]+\{', indent+'declare\n'+indent+'begin\n', newblock)
		newblock = re.sub('\{', ' declare\n'+indent+'begin\n', newblock)
		newblock = re.sub('\}', 'end;', newblock)
		text = re.sub(re.escape(block), newblock, text)
	return text

def replaceLabel(text):
	matches = re.findall('('+reE+'(('+reId+'):))', text)
	for match in matches:
		block = match[1]
		if match[2] != "default":
			log.debug("Replacing Label: "+match[2])
			text = re.sub('(?<!case )'+re.escape(match[1]), '<<'+match[2]+'>>', text)
	return text


def replaceArrayAccess(text):
	matches = re.findall('('+reId+'\[([^\[\]]+)\])', text)
	for match in matches:
		block = match[0]
		newblock = block
		newblock = re.sub('\[', '(', newblock)
		newblock = re.sub('\]', ')', newblock)
		text = re.sub(re.escape(block), newblock, text)
	return text



def findFunctions(text):
	matches = re.findall(reFunction, text)
	funList = []

	for match in matches:
		funname = match[2]
		funList.append(funname)
	return funList


def replaceFunctions(text):
	matches = re.findall(reFunction, text)
	# problem: reE ueberlappt mit letzter Funktion

	for match in matches:
		funname = match[2]
		log.debug("Replacing Function: "+funname)
		block = match[0]
		args = match[3]
		body = match[4]
		newblock = block
		indent = getIndent(block)
		declarations = findDeclarationLines(body)
		if (re.search('void +'+funname, block)):	
			newblock = re.sub('void +'+funname, 'procedure '+funname, newblock)
			newblock = re.sub('(?<=\))\s*\{', ' is\nbegin', newblock)
		else:
			rettype = match[1]
			newblock = re.sub(re.escape(rettype)+reFL+funname, 'function '+funname, newblock)
			if body == ';':
				newblock = re.sub('\)\s*;', ') return '+rettype+';', newblock)
			newblock = re.sub('(?<=\))'+reFL+'\{', ' return '+rettype+' is\nbegin', newblock)
		newblock = re.sub('\}', 'end '+funname+';', newblock)
		newargs = re.sub(',', ';', args)
		newblock = re.sub(re.escape(args), newargs, newblock)
		for declaration in reversed(declarations):
			if not re.search('.*begin.*'+declaration, block, re.DOTALL):
				newblock = re.sub(re.escape(declaration), '', newblock)
				declaration = replaceDeclaration(declaration)
				newblock = re.sub(' is\n', ' is\n'+indent+declaration, newblock)

		text = re.sub(re.escape(block), newblock, text)
	return (text) 
	

def replaceFunctionName(fileBaseName, text, includes):
	log.debug("Replacing function names...")
	for inc in includes:
		if(inc != fileBaseName):
			log.debug("File includes: "+inc)
			for funcFile in functionList:
				if inc == funcFile[0]:
					log.debug("Functionlist for "+inc+" :"+str(funcFile))
					for funName in funcFile[1]:
						text = re.sub('(?<![.\w])('+funName+')(?![;{=:+/.\w-])', inc+'.'+funName, text)
			
	return text


def replaceOperators(text):
	for op in subOperators:
		text = re.sub(op[0], op[1], text)
	for boolean in subBooleans:
		text = re.sub(boolean[0], boolean[1], text)

	return text

def replaceNumbers(text):
	matches = re.findall(reFL+'(?:[^\d.bx])((\d|\d{2}|\d{3})(\d{3})(\d{3})?(\d{3})?(\d{3})?(?=[^\d]))', text)
	for match in matches:
		newnumber = match[1]
		for group in match[2:]:
			if group != '':
				newnumber = newnumber+'_'+group
		text = re.sub(re.escape(match[0]), newnumber, text)

	matches = re.findall('(0[xX]([\da-fA-F]+)[ul]?)', text)
	for match in matches:
		text = re.sub(re.escape(match[0]), '16#'+match[1]+'#', text)

	matches = re.findall('(0b([01]+)[ul]?)', text) 
	for match in matches:
		text = re.sub(re.escape(match[0])+'(?=[^\d])', '2#'+match[1]+'#', text)

	return text

def replaceShifts(text):
	matches = re.findall('(('+reId+'|'+reNum+')\s+<<\s('+reId+'|'+reNum+'))', text)
	for match in matches:
		text = re.sub(re.escape(match[0]), 'Shift_Left('+match[1]+', '+match[2]+')', text)

	matches = re.findall('(('+reId+'|'+reNum+')\s+>>\s('+reId+'|'+reNum+'))', text)
	for match in matches:
		text = re.sub(re.escape(match[0]), 'Shift_Right('+match[1]+', '+match[2]+')', text)
	
	return text


def replaceVoid(text):
	return re.sub('\(s*void\s*\)', '', text)


def replaceExtra(text):
	for reExtra in reExtras:
		text = re.sub(reExtra[0], reExtra[1], text)
	return text




def insertPackageName(text, fileBaseName, isHeader):
	log.debug("Inserting Package Name")
	matches = re.findall('(?<=\n)[ \t]*(with [^\n]*;[^\n]*\n)', text)
	if isHeader:
		pkgStr = fileBaseName
	else:
		pkgStr = "body "+fileBaseName

	if matches:
		text = re.sub(re.escape(matches[-1]), matches[-1]+"\npackage "+pkgStr+" is\n\n", text)
		text = text+"\nend "+fileBaseName+";\n"
	else:
		text = "\npackage "+pkgStr+" is\n\n"+text+"\nend "+fileBaseName+";\n"
	return text

def postProcess(text):
	# remove ()
	text = re.sub('\(\)', '', text)

	return text


def translateFile( fileName ):
	# read file
	text = readInputFile(fileName)
	fileName = os.path.basename(fileName)
	fileBaseName, ext = os.path.splitext(fileName)

	# open output file
	if re.search('\.c(?:pp)?', ext):
		output = open(fileBaseName+".adb", 'w+')
		isHeader = False
	elif re.search('\.h(?:pp)?', ext):
		isHeader = True
		output = open(fileBaseName+".ads", 'w+')
	else:
		print "Unknown file format: "+ext
		sys.exit(1)


	log.info("Translating File "+fileName)

	# parseFile
	# 1. resolve problems
	text = replaceIndDec(text)		# before comments!
	text = replaceComments(text)
	text = replaceComments(text)	# double because of two comment blocks in one line
	text = resolveAdaKeywords(text)
	if isHeader:
		text = removeHeaderGuards(text)
	(text, includes) = preProcess(text)
	#text = resolveCases(text)
	text = resolveIndentBlock(text)	

	# 2. find non nested block
	text = replaceTypeDef(text)
	text = replaceEnum(text)
	text = replaceStructs(text)


	text = replaceLabel(text)
	text = replaceAssignments(text)
	text = replaceOperators(text)
	text = replaceShifts(text)

	# 3. process non nested block
	text = replaceElse(text)
	text = replaceNestedBlocks(text)

	if isHeader:
		funList = findFunctions(text)
		functionTupel = (fileBaseName, funList)
		functionList.append(functionTupel) 

	text = replaceFunctions(text)
	text = replaceFunctionName(fileBaseName, text, includes)

	# 3. replace rest
	text = replaceArrayDelaration(text)
	text = replaceDeclaration(text)
	text = replaceArrayAccess(text)
	text = replaceTypes(text)
	text = replaceCasts(text)
	text = replaceNumbers(text)
	text = replaceVoid(text)

	text = insertPackageName(text, fileBaseName, isHeader)
	text = postProcess(text)

	# write file
	log.info("Writing Output File: "+output.name)
	output.write(text)


# creates global log object
# @param name the name of the logger
# @param level level of logging e.g. logging.DEBUG
# @author Emanuel Regnath
def setupLogging(self, name, level):
    global log
    log = logging.getLogger(name)
    log.setLevel(level)

    formatter = logging.Formatter('[%(levelname)s] %(message)s')

    sh = logging.StreamHandler()
    sh.setLevel(level)
    sh.setFormatter(formatter)
    log.addHandler(sh)

    fh = logging.FileHandler(name + ".log")
    fh.setLevel(logging.DEBUG)
    fh.setFormatter(formatter)
    log.addHandler(fh)




# Main Program
#############################################
import logging
import fileinput
import sys
import os
import re



# check arguments
args = sys.argv[1:]
if len(args) == 0:
	print "Usage: python c2ada.py FILES"
	print "Will translate .c and .h files to .adb and .ads files.\n"
	print "In order to resolve include dependencies,\nall files must be specified in one call."
	print "Example: python c2ada.py main.c module.c module.h"
	sys.exit(0)


setupLogging(1, "c2ada", logging.INFO)
log.info("Starting Conversion")


for arg in args:
	if re.search('([^.]+[.][h](pp)?)', arg):
		fileName = arg
		log.info("Reading Header File: "+fileName)
		funTupel = translateFile(fileName)
	print "Found functions: "+str(functionList)

for arg in args:
	if re.search('([^.]+[.][c](pp)?)', arg):
		fileName = arg
		funTupel = translateFile(fileName)

