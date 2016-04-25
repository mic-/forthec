without warning

include parser.e


global constant
	NL = {13,10},		-- CR, LF
	MMX_REG = 1,
	XMM_REG = 2,
	powersof2 = {#100,#200,#400,#800,#1000,#2000,#4000,#8000,#10000,
	             #20000,#40000,#80000,#100000,#200000,#400000,#800000,
	             #1000000,#2000000,#4000000,#5000000,#6000000,#8000000},
	pow2_32 = {1,2,4,8,#10,#20,#40,#80,#100,#200,#400,#800,#1000,#2000,#4000,#8000,#10000,
	           #20000,#40000,#80000,#100000,#200000,#400000,#800000,#1000000,#2000000,
	           #4000000,#8000000,#10000000,#20000000,#40000000,#80000000}
	           

global constant COND = -2,
         NUMRANGE = -3,
         LESSBITSSET = -4,
         ISPOW2 = -5
         
         
global integer
	ifs,
	cases,
	loops,
	calls,
	outfile,
	usesI,
	usesConsole,
	usesCR, usesLF,
	usesLoops,
	usesFP,
	usesIP,
	usesDict,
	usesMiscS,
	usesCase,
	usesPrint,
	usesDiv,
	useRegs,
	allowInline,
	inline,
	strops,
	ignoreOutput,
	globLit,
	noMangle,
	noFold,
	p1,p2,
	dllentry,
	rangeIdx,

	fastfloat

global sequence
	code,
	maincode,
	inlinecode,
	inlines,
	deferred,
	referred,
	ifStack,
	literals,
	immliterals,
	includes,
	labelPrefix,
	publics,
	CMD,
	pendingWordDef,
	userWords,
	variables,
	regs,
	rstack,
	fastCmp

global integer
	linesRemoved,
	constRemoved,
	optLevel

global sequence
	patvars,
	constlist
	

global sequence cfgin,morefiles
global sequence outname,entry,fentry,nofpu
global integer cfgfile,dll,infpos
global atom t1,iwramStart,iwramEnd,ewramStart,ewramEnd



global procedure forthec_init()
	code = {}
	ifStack = {}
	maincode = {}
	deferred = {{},{}}
	publics = {{},{}}
	literals = {}
	immliterals = {{},{}}
	pendingWordDef = {}
	tokens = {}
	userWords = {{},{}}
	constants = {{},{}}
	fconstants = {{},{}}
	variables = {{},{}}
	inlinecode = {}
	inlines = {}
	
	inline = 0
	ignoreOutput = 0
end procedure


global function cut_filename(sequence fname)
	integer p
	
	p = length(fname)
	while p>=1 do
		if fname[p] = '.' then
			exit
		end if
		p -= 1
	end while

	if p then
		return {fname[1..p-1],fname[p+1..length(fname)]}
	end if
	
	return fname
end function



-- Insert sequence 'what' in sequence 'dest' of lexically ordered sequences
global function ordered_insert(sequence what,sequence dest)
	integer lb,ub,middle,res

	if not length(dest) then
		return {what}
	end if
	
	lb = 0
	ub = length(dest)-1
	
	while 1 do
		if ub<lb then
			return dest
		end if

		middle = floor((lb+ub)/2)
		
		res = compare(what,dest[middle+1])
		if res=0 then
			return dest
		elsif res<0 then
			if middle=0 then
				exit
			elsif compare(what,dest[middle])>0 then
				exit
			end if
			ub = middle-1
		else
			if middle=length(dest)-1 then
				if middle=0 then
					middle=1
				else
					middle += 1
				end if
				exit
			elsif compare(what,dest[middle+2])<0 then
				middle += 1
				exit
			end if
			lb = middle+1
		end if
	end while
	
	middle += 1
	if middle=1 then
		dest = {what}&dest
	elsif middle>length(dest) then
		dest = append(dest,what)
	else
		dest = dest[1..middle-1] & {what} & dest[middle..length(dest)]
	end if
	
	return dest
end function



		
-- Insert sequence 'what' in sequence 'dest' of lexically ordered sequences
global function assoc_insert(sequence name,object data,sequence dest)
	integer lb,ub,middle,res
	sequence names
	
	names = dest[1]
	
	if not length(names) then
		return {{name},{data}}
	end if
	
	lb = 0
	ub = length(names)-1
	
	while 1 do
		if ub<lb then
			return dest
		end if

		middle = floor((lb+ub)/2)
		
		res = compare(name,names[middle+1])
		if res=0 then
			return dest
		elsif res<0 then
			if middle=0 then
				exit
			elsif compare(name,names[middle])>0 then
				exit
			end if
			ub = middle-1
		else
			if middle=length(names)-1 then
				if middle=0 then
					middle=1
				else
					middle += 1
				end if
				exit
			elsif compare(name,names[middle+2])<0 then
				middle += 1
				exit
			end if
			lb = middle+1
		end if
	end while
	
	middle += 1
	if middle=1 then
		dest[1] = {name}&names
		dest[2] = {data}&dest[2]
	elsif middle>length(names) then
		dest[1] = append(names,name)
		dest[2] = append(dest[2],data)
	else
		dest[1] = names[1..middle-1] & {name} & names[middle..length(names)]
		dest[2] = dest[2][1..middle-1] & {data} & dest[2][middle..length(dest[2])]
	end if
	
	return dest
end function


	

global function make_label(sequence id)
	sequence s

	if noMangle then
		return id
	end if
	
	s = ""
	for i=1 to length(id) do
		s &= "%02x"
	end for
	
	return labelPrefix&"lbl_"&sprintf(s,id)
end function


-- Counts set bits
global function count_bits(atom a)
	atom msk
	integer cnt

	
	msk = 1

	cnt = 0
	for i=1 to 32 do
		if and_bits(a,msk) then
			cnt += 1
		end if
		msk += msk
	end for
	return cnt
end function


global procedure ADD_CODE(sequence instr,integer section)
	if not ignoreOutput then
		if length(pendingWordDef) then
			code &= instr
		else
			maincode &= instr
		end if
	elsif inline then
		inlinecode &= instr	
	end if
end procedure


global function get_code_length()
	if inline then
		return length(inlinecode)
	end if
	if length(pendingWordDef) then
		return length(code)
	end if
	return length(maincode)
end function


global procedure REMOVE_CODE(integer i1,integer i2)
	if not ignoreOutput then
		if length(pendingWordDef) then
			code = code[1..i1-1]&code[i2+1..length(code)]
		else
			maincode = maincode[1..i1-1]&maincode[i2+1..length(maincode)]
		end if
	elsif inline then
		inlinecode = inlinecode[1..i1-1]&inlinecode[i2+1..length(inlinecode)]
	end if
end procedure



global procedure count_refs()
	integer n

	referred = {{},{}}
	for i=1 to length(tokens) do
		if tokens[i][1] = UNKNOWN then
			n = bsearch(tokens[i][2],referred[1])
			if n>0 then
				referred[2][n][1] += 1
			else
				referred = assoc_insert(tokens[i][2],{1},referred)
			end if
		end if
	end for
end procedure



constant unary_ops = {
W_NOT,W_NEG,W_INC,W_DEC,W_MUL2,W_DIV2
}

constant binary_ops = {
W_ADD,W_SUB,W_MUL,W_DIV,W_MOD,
W_AND,W_OR,W_XOR
}

-- This procedure folds constant expressions before any code is generated
global procedure optimise_token_stream()
	integer p,q,r
	sequence immConst
	
	immConst = {{},{}}
	
	p = 1
	while p<=length(tokens) do
		r = tokens[p][1]
		
		if r = W_CONST then
			if p>1 and p<length(tokens) then
				if tokens[p-1][1] = NUMBER then
					immConst = assoc_insert(tokens[p+1][2],{tokens[p-1][2]},immConst)
				end if
				p += 1
			end if
		elsif r = UNKNOWN then
			q = bsearch(tokens[p][2],immConst[1])
			if q>0 then
				tokens[p][1] = NUMBER
				tokens[p][2] = immConst[2][q][1]
			end if
		else
			q = find(r,binary_ops)
			if q>0 and p>2 then
				if tokens[p-2][1]=NUMBER and tokens[p-1][1]=NUMBER then
					tokens[p][1]=NUMBER
					if r=W_ADD then
						tokens[p][2] = tokens[p-2][2] + tokens[p-1][2]
					elsif r=W_SUB then
						tokens[p][2] = tokens[p-2][2] - tokens[p-1][2]
					elsif r=W_MUL then
						tokens[p][2] = tokens[p-2][2] * tokens[p-1][2]
					elsif r=W_DIV then
						tokens[p][2] = floor(tokens[p-2][2] / tokens[p-1][2])
					elsif r=W_MOD then
						tokens[p][2] = remainder(tokens[p-2][2], tokens[p-1][2])
					elsif r=W_AND then
						tokens[p][2] = and_bits(tokens[p-2][2], tokens[p-1][2])
					elsif r=W_OR then
						tokens[p][2] = or_bits(tokens[p-2][2], tokens[p-1][2])
					elsif r=W_XOR then
						tokens[p][2] = xor_bits(tokens[p-2][2], tokens[p-1][2])
					end if
					
					if p=3 then
						tokens = tokens[p..length(tokens)]
					else
						tokens = tokens[1..p-3]&tokens[p..length(tokens)]
					end if
					p -= 2
				end if
			end if
			
			if q<=0 then
				q = find(r,unary_ops)
			else
				q = 0
			end if
			if q>0 and p>1 then
				if tokens[p-1][1]=NUMBER then
					tokens[p][1] = NUMBER

					if r=W_NEG then
						tokens[p][2] = -tokens[p-1][2]
					elsif r=W_NOT then
						tokens[p][2] = not_bits(tokens[p-1][2])
					elsif r=W_INC then
						tokens[p][2] = tokens[p-1][2] + 1
					elsif r=W_DEC then
						tokens[p][2] = tokens[p-1][2] - 1
					elsif r=W_MUL2 then
						tokens[p][2] = tokens[p-1][2] * 2
					elsif r=W_DIV2 then
						tokens[p][2] = floor(tokens[p-1][2] / 2)
					end if
				
					if p=2 then
						tokens = tokens[p..length(tokens)]
					else
						tokens = tokens[1..p-2]&tokens[p..length(tokens)]
					end if
					p -= 1					
				end if
			end if
		end if
		p += 1
	end while
end procedure

