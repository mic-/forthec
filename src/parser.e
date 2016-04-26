-- ForthEC parser
-- /Mic, 2004/2009


include file.e
include get.e
include machine.e
include wildcard.e
include lexseq.e
include error.e
include symtab.e


global constant
	token_TAG = 1,
	token_VALUE = 2
	

global constant WORDS = 
{
"!",		-- (n adr --)
"!r",		-- (adr n --)
"!r+",		-- (adr n -- adr) 	store n in adr and increment adr by 4
"\"",
"\".",
"#align",	-- (-- n)		alignment granularity
"$\"",		-- 
"\'",
"(",		-- (--)			begin comment	
")",		-- (--)			end comment
"*",		-- (n1 n2 -- n3)
"+",		-- (n1 n2 -- n3)
"+!",		-- (n adr --) 		add n to the value stored att adr and store the new value
"+loop",
",",
"-",		-- (n1 n2 --)
"--",
"-rot",
".",		-- (n --)
".\"",
".(",
".s",
"/",		-- (n1 n2 -- n3)
"/c",		-- (-- n)		size of character (byte)
"/c*",
"/f",		-- (-- n)		size of floating point number in bytes
"/f*",
"/l",
"/l*",
"/mod",		-- (n1 n2 -- n3 n4)
"/n",
"/n*",
"/w",
"/w*",
"0<",
"0<=",
"0<>",
"0=",		-- (n1 n2 -- f)
"0>",
"0>=",
"1+",		-- (n1 -- n2)
"1-",		-- (n1 -- n2)
"16-bit",
"2*",		-- (n1 -- n2)
"2+",		-- (n1 -- n2)
"2/",		-- (n1 -- n2)
"2drop",
"2dup",
"2over",
"32-bit",
":",
--":i38",
--":i66",
":i",
":noname",
":w",
";",
";@",
";i",
";n",
";w",
"<",
"<<",
"<=",
"<>",
"<c->l",	-- (n1 -- n2)		byte to long (signed)
"<c->w",	-- (n1 -- n2)		byte to word (signed)
"<c@",		-- (adr -- signedbyte)	
"<w->l",	-- (n1 -- n2)		word to long (signed)
"<w@",		-- (adr -- signedword)
"=",		-- (n1 n2 -- cond)	true if n1=n2
">",
">=",
">>",
">>a",		-- (n1 -- n2)		arithmetic shift right
"?",
"?do",		-- (n1 n2 -- )		do one or more times
"?dup",		-- 			duplicate if non-zero
"?leave",	-- (cond -- ) 		leave if true
"?stack",
"@",		-- (adr -- n)		fetch 
"@+",		-- (adr1 -- adr2 n)	fetch and increase address
"\\",
"a!",		-- (n -- )		store in accumulator
"a@",		-- ( -- n)		fetch accumulator
"abort",
"abs",		-- (n1 -- n2)		absolute value
"again",	-- (--)			branch back to the nearest preceeding begin
"alias",
"align",
"aligned",
"allot",
"and",		-- (n1 n2 -- n3)
"ascii",	-- (n1 -- n2)
"begin",
"between",	-- (n min max -- flag)	min<=n<=max
"bl",
"bounds",
"branch",
"bye",
"c!",
"c!l",
"c!li",
"c,",
"c->l",		-- (n1 -- n2)		byte to long (unsigned)
"c->w",		-- (n1 -- n2)		byte to word (unsigned)
"c:es!",
"c:es@",
"c@",
"c@+",		-- (adr1 -- adr2 n)	
"c@]i",
"c@l",
"c@li",
"ca+",		
"call",
"calli",
"canonical",
"case",
"cell",
"cell+",
"cells",
"cmove",
"code",
"constant",
"convert",
"cr",		-- (-- char)
"cs@",		-- (adr -- signedbyte)	same as <c@
"decimal",	-- (--)
"default",
"defer",
"depth",
"digit",
"do",
"drop",		-- (n -- )		drop tos
"dup",		-- (n -- n n)		duplicate tos
"else",
"emit",
"emms",
"end-string-array",
"endcase",
"endof",
"entry",
"erase",
"es!",		-- (n -- )		store in ES register (dos16)
"exit",		-- (--)
"expect",	-- (adr n --)
"extern",
"f!",		-- (fp adr --)
"f!s",		-- (fp adr --)
"f*",
"f**",
"f+",		-- (fp1 fp2 -- fp3)
"f-",
"f.",		-- (fp --)		display fp
"f.s",
"f/",		-- (fp1 fp2 -- fp3)
"f0=",
"f0<>",		
"f<",
"f<>",
"f=",
"f>",
"f@",		-- (adr -- fp)
"f@s",
"fabs",
"falog",
"false",
"fatan",
"fclose",
"fconstant",
"fcos",
"fcr",
"fdepth",	-- (-- n)
"fdrop",
"fdup",
"fexit",
"ffills",
"fflush",
"fgetc",
"fgetline",
"fgets",
"fill",
"fix",		-- (fp -- l)		convert fp to integer (round towards nearest int)
"float",	-- (l -- fp)		convert integer to float
"flog",
"floor",
"fnegate",
"fopen",
"fover",
"fpop",		-- (fp -- l l)
"fpops",	-- (fp -- l)
"fpush",
"fpushs",	-- (l -- fp)		single precision version of fpush
"fputc",
"fputs",
"frot",
"fseek",
"fsin",
"fsincos",	-- (fp1 -- fp2 fp3)
"fsize",
"fsqrt",	-- (fp1 -- fp2)
"fswap",
"ftan",
"ftell",
"fvariable",
"here",		-- ( -- n)
"hex",		-- (--)
"i",		-- ( -- n)		loop index of innermost loop
"if",
"include",
"inp",		-- (n1 -- n2)		read a byte from port n1 (dos16)
"int",		-- (fp -- l)		round towards zero
"interrupt",	-- (n -- )		invoke an interrupt (dos16)
"j",		-- ( -- n)		loop index of secondary loop
"key",
"l+!",
"l.",
"l0=",
"l<",
"l=",
"l>",
"l@",		-- (adr -- n)		same as @
"la+",
"land",		-- (n1 n2 -- n3)	same as and
"leave",
"loop",		-- ( -- )
"max",		-- (n1 n2 -- n3)
"min",		-- (n1 n2 -- n3)
"mod",		-- (n1 n2 -- n3)
"move",
"mu/mod",
"n->w",
"na+",		-- (adr1 index -- adr2)	add index*sizeof(stackitem) to adr
"na1+",		-- (adr1 -- adr2)
"negate",	-- (n1 -- n2)	
"newline",	-- (-- char)
"nip",		-- (n1 n2 -- n2)
"noop",		-- (--)
"not",		-- (n1 -- n2)
"notouch",
"null",		-- (-- 0)
"number?",	-- (adr -- l flag)	
"of",		-- (selector test-value --)
"off",		-- (adr --)
"on",		-- (adr --)		stores true at adr
"or",		-- (n1 n2 -- n3)	logical or
"outp",
"over",		-- (n1 n2 -- n1 n2 n3)	n3 is a copy of n1
"p!",
"p+",
"p-",
"p@",
"p@a",
"pc+",
"pc-",
"pc2pw",
"pdrop",
"pdup",
"pf!",
"pf!a",
"pf@",
"pf@a",
"pick",
"printable?",
"public",
"pw*",
"pw+",
"pw-",
"pw2pc",
"pw2pcs",
"pw2pd",
"repeat",
"rot",		-- (n1 n2 n3 -- n2 n3 n1)
"s.",		-- (n --)
"set-interrupt-handler",
"sp!",
"sp0",
"sp@",
"space",	-- (--)
"spaces",	-- (n --)
"string-array",
"swap",		-- (n1 n2 -- n2 n1)
"then",
"to",
"touch",
"true",		-- ( -- true)
"tuck",		-- (n1 n2 -- n2 n1 n2)
"type",
"u.",
"u<",		-- (n1 n2 -- flag)	n1<n2 (unsigned)
"u<=",
"u>",
"u>=",
"ul*",
"um*",
"um/mod",
"until",	-- (cond -- )
"value",
"variable",
"w!",
"w!l",		-- (n adrhi adrlo -- )
"w,",
"w->l",		-- (n1 -- n2)	word to long (unsigned)
"w:es!",
"w@",		-- (adr -- n)
"w@+",		-- (adr1 -- adr2 n)
"wa+",		-- (adr1 index -- adr2)	adr2 = adr1 + index*sizeof(word)
"wa1+",		-- (adr1 -- adr2)	adr2 = adr1 + sizeof(word)
"while",
"within",	-- (n min max -- cond)	min<=n<max
"wl@",
"xor",		-- (n1 n2 -- n3)
"z\"",
"zpallot",
"zpvariable",
"{",
"}"
}


global sequence wordArgs
wordArgs = repeat(0,length(WORDS))


function keyword(sequence name,integer numArgs)
	integer p
	
	p = find(name,WORDS)
	if p<=0 then
		name = name[0]	-- just make it break to generate an ex.err file
	end if

	wordArgs[p] = numArgs
	return p
end function

	

global constant
	-- The "numArgs" value is not currently used for anything, so just specify
	-- zero for all of them.
	W_INCLUDE	= keyword("include",0),
	W_DEFER 	= keyword("defer",0),
	W_HERE		= keyword("here",0),
	W_PUBLIC	= keyword("public",0),
	
	W_CELL		= keyword("cell",0),
	W_CELLPLUS	= keyword("cell+",0),
	W_CELLS		= keyword("cells",0),
	
	W_DOT		= keyword(".",0),
	W_DOTS		= keyword(".s",0),
	W_DISPVAR	= keyword("?",0),
	W_DOTSTRING	= keyword(".\"",0),
	W_FDOT		= keyword("f.",0),
	W_EMIT		= keyword("emit",0),
	W_ASCII		= keyword("ascii",0),
	W_BL		= keyword("bl",0),
	
	W_PUTS		= keyword("\".",0),
	W_EXPECT	= keyword("expect",0),
	W_KEY		= keyword("key",0),
	
	W_CR		= keyword("cr",0),
	W_NEWLINE	= keyword("newline",0),
	W_SPACE		= keyword("space",0),
	W_SPACES	= keyword("spaces",0),
	
	W_COLON		= keyword(":",0),
	W_SEMICOLON	= keyword(";",0),
	W_SEMICOLONPOP	= keyword(";@",0),
	W_COLONIRQ	= keyword(":i",0),
	W_COLONWIN	= keyword(":w",0),
	W_SEMICOLONWIN	= keyword(";w",0),
	W_COLONNO	= keyword(":noname",0),
	--W_COLONI38	= keyword(":i38",0),
	--W_COLONI66	= keyword(":i66",0),
	W_SEMICOLONI	= keyword(";i",0),
	W_SEMICOLONN	= keyword(";n",0),

	W_SETINT	= keyword("set-interrupt-handler",0),
	
	W_QUOTE		= keyword("\'",0),
	W_COMMA		= keyword(",",0),
	W_CCOMMA	= keyword("c,",0),
	W_WCOMMA	= keyword("w,",0),

	W_BOUNDS	= keyword("bounds",0),
	
	W_BEGINCOMMENT	= keyword("(",0),
	W_ENDCOMMENT	= keyword(")",0),
	W_SINGLECOMMENT	= keyword("\\",0),

	W_PARAMSTART	= keyword("{",0),
	W_PARAMEND	= keyword("}",0),
	W_PARAMDELIM	= keyword("--",0),
	
	W_CONST		= keyword("constant",0),
	W_VAR		= keyword("variable",0),
	W_ZPVAR		= keyword("zpvariable", 0),
	W_FCONST	= keyword("fconstant",0),
	W_FVAR		= keyword("fvariable",0),
	W_EXTERN	= keyword("extern",0),
	
	W_NULL		= keyword("null",0),
	W_TRUE		= keyword("true",0),
	W_FALSE		= keyword("false",0),
	
	W_STORE		= keyword("!",0),
	W_CSTORE	= keyword("c!",0),
	W_CSTOREL	= keyword("c!l",0),
	W_CSTORELI	= keyword("c!li",0),
	W_FSTORE	= keyword("f!",0),
	W_FSTORES	= keyword("f!s",0),
	W_WSTORE	= keyword("w!",0),
	W_WSTOREL	= keyword("w!l",0),
	W_STORE_R	= keyword("!r",0),
	W_STORE_R_INC	= keyword("!r+",0),
	W_ERASE		= keyword("erase",0),
	W_FILL		= keyword("fill",0),
	W_FFILLS	= keyword("ffills",0),
	W_CMOVE		= keyword("cmove",0),
	W_FETCH		= keyword("@",0),
	W_FETCHADD	= keyword("@+",0),
	W_CFETCH	= keyword("c@",0),
	W_CFETCHADD	= keyword("c@+",0),
	W_CFETCHL	= keyword("c@l", 0),
	W_CFETCHLI	= keyword("c@li", 0),
	W_CFETCHILI	= keyword("c@]i", 0),
	W_SCFETCH	= keyword("<c@",0),
	W_CSFETCH	= keyword("cs@",0),
	W_WFETCH	= keyword("w@",0),
	W_WFETCHADD	= keyword("w@+",0),
	W_SWFETCH	= keyword("<w@",0),
	W_WLOFETCH	= keyword("wl@",0),
	W_FFETCH	= keyword("f@",0),
	W_FFETCHS	= keyword("f@s",0),
	W_ACCFETCH	= keyword("a@",0),

	W_C2W		= keyword("c->w",0),
	W_C2L		= keyword("c->l",0),
	W_CS2W		= keyword("<c->w",0),
	W_CS2L		= keyword("<c->l",0),
	W_W2L		= keyword("w->l",0),
	W_WS2L		= keyword("<w->l",0),
	
	W_ADRPLUSN	= keyword("na+",0),
	W_ADRPLUS1N	= keyword("na1+",0),
	W_ADRPLUSW	= keyword("wa+",0),
	W_ADRPLUS1W	= keyword("wa1+",0),
	
	-- Vector operations
	W_PSTORE	= keyword("p!",0),
	W_PFETCH	= keyword("p@",0),
	W_PWADD		= keyword("pw+",0),
	W_PWSUB		= keyword("pw-",0),
	W_PWMUL		= keyword("pw*",0),
	W_PCADD		= keyword("pc+",0),
	W_PCSUB		= keyword("pc-",0),
	W_PC2PW		= keyword("pc2pw",0),
	W_PW2PC		= keyword("pw2pc",0),
	W_PW2PCS	= keyword("pw2pcs",0),
	W_PDUP		= keyword("pdup",0),
	W_PDROP 	= keyword("pdrop",0),
	W_EMMS		= keyword("emms",0),
	
	W_ON		= keyword("on",0),
	W_OFF		= keyword("off",0),
	
	W_SPBOT		= keyword("sp0",0),
	W_SPTOP		= keyword("sp@",0),
	
	W_FPOP		= keyword("fpop",0),
	W_FPOPS		= keyword("fpops",0),
	
	W_CSIZE		= keyword("/c",0),
	W_FSIZE		= keyword("/f",0),
	W_NSIZE		= keyword("/n",0),
	
	W_DUP		= keyword("dup",0),
	W_DUPNZ		= keyword("?dup",0),
	W_OVER		= keyword("over",0),
	W_PICK		= keyword("pick",0),
	W_NIP		= keyword("nip",0),
	W_DROP		= keyword("drop",0),
	W_ROT		= keyword("rot",0),
	W_ROTINV	= keyword("-rot",0),
	W_SWAP		= keyword("swap",0),
	W_TUCK		= keyword("tuck",0),
	
	W_2DROP		= keyword("2drop",0),
	W_2DUP		= keyword("2dup",0),
	W_2OVER		= keyword("2over",0),

	W_FDUP		= keyword("fdup",0),
	W_FDROP		= keyword("fdrop",0),
	W_FOVER		= keyword("fover",0),
	W_FSWAP		= keyword("fswap",0),

	W_ADD		= keyword("+",0),
	W_SUB		= keyword("-",0),
	W_MUL		= keyword("*",0),
	W_DIV		= keyword("/",0),
	W_MOD		= keyword("mod",0),
	W_DIVMOD	= keyword("/mod",0),

	W_MIN		= keyword("min",0),
	W_MAX		= keyword("max",0),
	W_BETWEEN	= keyword("between",0),
	W_WITHIN	= keyword("within",0),
	
	W_FADD		= keyword("f+",0),
	W_FSUB		= keyword("f-",0),
	W_FMUL		= keyword("f*",0),
	W_FDIV		= keyword("f/",0),

	W_FSIN		= keyword("fsin",0),
	W_FCOS		= keyword("fcos",0),

	W_FLOAT		= keyword("float",0),
	W_FLOOR		= keyword("floor",0),
	
	W_MUL2		= keyword("2*",0),
	W_DIV2		= keyword("2/",0),
	W_DEC		= keyword("1-",0),
	W_INC		= keyword("1+",0),

	W_NEG		= keyword("negate",0),
	W_FNEG		= keyword("fnegate",0),
	
	W_SHL		= keyword("<<",0),
	W_SHR		= keyword(">>",0),
	W_SAR		= keyword(">>a",0),
	
	W_AND		= keyword("and",0),
	W_OR		= keyword("or",0),
	W_XOR		= keyword("xor",0),
	W_NOT		= keyword("not",0),

	W_EQ		= keyword("=",0),
	W_NE		= keyword("<>",0),
	W_GE		= keyword(">=",0),
	W_GT		= keyword(">",0),
	W_LE		= keyword("<=",0),
	W_LT		= keyword("<",0),
	W_UGE		= keyword("u>=",0),
	W_UGT		= keyword("u>",0),
	W_ULE		= keyword("u<=",0),
	W_ULT		= keyword("u<",0),
	W_EQ0		= keyword("0=",0),
	W_NE0		= keyword("0<>",0),
	W_GT0		= keyword("0>",0),
	W_LT0		= keyword("0<",0),

	W_FEQ		= keyword("f=",0),
	W_FNE		= keyword("f<>",0),
	W_FGT		= keyword("f>",0),
	W_FLT		= keyword("f<",0),
	
	W_BEGIN		= keyword("begin",0),
	W_WHILE		= keyword("while",0),
	W_REPEAT	= keyword("repeat",0),
	W_AGAIN		= keyword("again",0),
	W_DO		= keyword("do",0),
	W_DONZ		= keyword("?do",0),
	W_LOOP		= keyword("loop",0),
	W_INCLOOP	= keyword("+loop",0),
	W_LEAVE		= keyword("leave",0),
	W_LEAVETRUE	= keyword("?leave",0),
	W_UNTIL		= keyword("until",0),
	W_I		= keyword("i",0),
	W_J		= keyword("j",0),
	
	W_IF		= keyword("if",0),
	W_ELSE		= keyword("else",0),
	W_THEN		= keyword("then",0),
	
	W_CASE		= keyword("case",0),
	W_ENDCASE	= keyword("endcase",0),
	W_OF		= keyword("of",0),
	W_ENDOF		= keyword("endof",0),
	W_DEFAULT	= keyword("default",0),
	
	W_CALL		= keyword("call",0),
	W_CALLI		= keyword("calli",0),
	W_BYE		= keyword("bye",0),
	W_EXIT		= keyword("exit",0),
	
	W_ABS		= keyword("abs",0),

	W_ALLOT		= keyword("allot",0),
	W_ZPALLOT	= keyword("zpallot", 0),
	W_ZSTRING	= keyword("z\"",0),
	W_DOLLARSTRING	= keyword("$\"",0),

	W_INTERRUPT	= keyword("interrupt",0),
	W_INP		= keyword("inp",0),
	W_OUTP		= keyword("outp",0),
	W_ACCSTORE	= keyword("a!",0),
	W_CESFETCH	= keyword("c:es@",0),
	W_CESSTORE	= keyword("c:es!",0),
	W_WESSTORE	= keyword("w:es!",0),
	W_ESSTORE	= keyword("es!",0),
	
	W_TOUCH		= keyword("touch",0),
	W_NOTOUCH	= keyword("notouch",0)


--W_PW2PC

global constant
	ASCNUMBER = -6,
	HEXNUMBER = -5,
	NUMBER  = -4,
	FNUMBER = -3,
	NEWLINE = -2,
	UNKNOWN = -1,
	NOTHING = 0


global sequence
	constants,
	fconstants,
	tokens

global integer 
	verbose
	
	
integer
	isString


isString = 0
verbose = 0




-- Read a token from file fn
function get_token(integer fn,integer line)
	integer c,p,ttype,escaped
	sequence token,token2,val
	
	token = {}
	ttype = NUMBER
	
	-- Skip whitespace
	if not isString then
		c = ' '
		while (c=' ' or c='\t' or c=13) do
			c = getc(fn)
			if c = -1 then
				return {NOTHING}
			end if
		end while
	else
		c = getc(fn)
	end if
	
	escaped = 0
	
	while (isString and c!='\"') or
	      ((not isString) and (not (c=' ' or c='\t' or c=13))) do
		token &= c	-- Add the read character to the token

		-- Exit on linefeed
		if c=10 then	
			exit

		elsif c>='0' and c<='9' then

		-- A '.' means either a floating point number or an ID (UNKNOWN)
		elsif c='.' then
			if ttype=NUMBER and length(token)>1 then
				ttype = FNUMBER
			else
				ttype = UNKNOWN
			end if

		-- 'e'/'E' can occur in a hex number, a floating point number and
		-- an ID (UNKNOWN)
		elsif c='e' or c='E' then
			if ttype=HEXNUMBER then
				if c='e' then
					token[length(token)] -= ' '
				end if
			elsif ttype != FNUMBER and length(token)>1 then
				ttype = UNKNOWN
			end if
		
		-- 'x'/'X' can occur in a hex number and an ID (UNKNOWN)
		elsif c='x' or c='X' and length(token)=2 and ttype=NUMBER then
			if token[token_TAG]='0' then
				ttype = HEXNUMBER
			else
				ttype = UNKNOWN
			end if
		
		
		elsif c='\'' then
			ttype = ASCNUMBER
		
		
		elsif c='-' then
			if length(token)>1 then
				ttype = UNKNOWN
			end if
		
		elsif c='\\' then
			if isString then
				escaped = 1
			end if
			
		elsif (c=' ' or c='\t' or c=13) then
		else
			if ttype=HEXNUMBER and (c>='a' and c<='f') then
				token[length(token)] -= ' '
			elsif ttype=HEXNUMBER and (c>='A' and c<='F') then
			else
				ttype = UNKNOWN
			end if
		end if

		escaped = 0
		
		c = getc(fn)
		if c=-1 then exit end if
	end while
	
	if isString then
		token = token & "\""
		return {UNKNOWN,token,line}
	end if
	
	if not length(token) then
		return {NOTHING}
	
	elsif equal(token,{10}) then
		return {NEWLINE,token,line}
	
	else
		if token[length(token)]=10 then
			token = token[1..length(token)-1]
		end if
		token2 = lower(token)
		p = bsearch(token2,WORDS)
		if p then
			--if p = W_NOTOUCH then ? p end if
			return {p,token,line}
		end if
	end if

	if ttype = NUMBER then
		val = value(token)
		return {NUMBER,val[2],line}
	
	elsif ttype = FNUMBER then
		val = value(token)
		return {FNUMBER,val[2],line}
	
	elsif ttype=HEXNUMBER then
		val = value("#"&token[3..length(token)])
		return {NUMBER,val[2],line}
	elsif ttype=ASCNUMBER then
		c = 0 --value("#"&token[3..length(token)])
		--? token
		p = 1
		for i=length(token)-1 to 2 by -1 do
			c += token[i]*p
			p *= 256
		end for
		return {NUMBER,c,line}

	end if
	
	
	return {UNKNOWN,token,line}
end function



global function parse(sequence fname)
	integer fn,n,p,line,touch
	sequence s,t,token,asm
	
	fn = open(fname,"rb")
	if fn=-1 then
		ERROR("Unable to open file",{0,fname,1})
	end if
	
	if verbose then
		puts(1,"Parsing "&fname&"\n")
	end if
	
	line = 1
	touch = 1
	
	-- First stage: tokenize input stream	
	while 1 do
		token =	get_token(fn,line)
		
		if token[token_TAG]=NOTHING then
			exit
		elsif token[token_TAG]=NEWLINE then
			if not touch then
				tokens = append(tokens,token)
			end if
			line += 1
		elsif token[token_TAG] = W_BEGINCOMMENT then		-- start of comment
			while token[token_TAG]!=W_ENDCOMMENT and token[token_TAG]!=NOTHING do
				token = get_token(fn,line)
				if token[token_TAG]=NEWLINE then
					line += 1
				end if
			end while
		elsif token[token_TAG] = W_SINGLECOMMENT then
			while not (token[token_TAG]=NEWLINE or token[token_TAG]=NOTHING) do
				token = get_token(fn,line)
			end while
			line += 1
		elsif token[token_TAG]=W_ZSTRING or
		      token[token_TAG]=W_DOTSTRING or
		      token[token_TAG]=W_DOLLARSTRING then
			tokens = append(tokens,token)
			isString = 1
			tokens = append(tokens,get_token(fn,line))
			isString = 0
		elsif token[token_TAG]=W_TOUCH then
			tokens = append(tokens,token)
			touch = 1
		elsif token[token_TAG]=W_NOTOUCH then
			tokens = append(tokens,token)
			touch = 0
		elsif token[token_TAG]=W_INCLUDE and touch then
			token = get_token(fn,line)
			if parse(token[2]) then
			end if
		else
			if token[token_TAG] = UNKNOWN then
				install_symbol(token[token_VALUE],{SYM_UNDEF})
			end if
			tokens = append(tokens,token)
		end if
	end while
	
	close(fn)
	return 1
end function
