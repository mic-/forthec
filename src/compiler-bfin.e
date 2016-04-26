-- ForthEC : A Forth compiler
-- /Mic, 2004/2005
--
-- Blackfin (ADSP-BF53x) code generator
--

without warning
include forthec.e
include optimiser-bfin.e


	
-- Register use:
----------------
-- r0  TOS
-- r1  Work register
-- r2  -"-
-- r3 
-- r4
-- r5  Case variable
-- r6  Loop counter
-- r7  Loop limit
-- p0  Dictionary pointer
-- p1  Parameter stack
-- p2  Return stack
-- p3  Loop stack
-- p4  Work register
-- p5  -"-
-- sp  Regular stack



-- Register allocation is currently handled in the optimiser rather than
-- the code generator.

sequence stack_regs
stack_regs = {}
constant opt_regs = {"r3","r4"}

function alloc_reg_bfin()
	if length(stack_regs)<length(opt_regs) then
		stack_regs = {opt_regs[length(stack_regs)+1]} & stack_regs
		return 1
	end if
	return 0
end function

procedure free_reg_bfin()
	if length(stack_regs)>1 then
		stack_regs = stack_regs[2..length(stack_regs)]
	elsif length(stack_regs)=1 then
		stack_regs = {}
	end if
end procedure



-- Push a value on the parameter stack
procedure PUSH_bfin(object a)
	if useRegs and alloc_reg_bfin() then
		ADD_CODE({stack_regs[1]&" = r0;"},0)

	else
		ADD_CODE({"p1 += -4;","[p1] = r0;"},0)
	end if
	
	if atom(a) then
	
		if a>=0 then
			if and_bits(a,#FFFF0000)=0 then
				if a<#40 then
					ADD_CODE({sprintf("r0 = %d;",a)},0)
				else
					ADD_CODE({sprintf("r0 = %d(z);",a)},0)
				end if
			else
				ADD_CODE({sprintf("r0.h = %d",and_bits(floor(a/#10000),#FFFF)),
					  sprintf("r0.l = %d",and_bits(a,#FFFF))},0)
			end if
		else
			if and_bits(a,#FFFF0000)=#FFFF0000 then
				ADD_CODE({sprintf("r0 = %d;",a)},0)
			else
				ADD_CODE({sprintf("r0.h = %d",and_bits(floor(a/#10000),#FFFF)),
					  sprintf("r0.l = %d",and_bits(a,#FFFF))},0)
			end if
		end if

	else
		ADD_CODE({"r0 = "&a&";"},0)
	end if
		
end procedure


function POP_bfin(sequence r)
	sequence s
	
	if useRegs and length(stack_regs) then
		if length(stack_regs) > 1 then
			ADD_CODE({stack_regs[2]&" = "&stack_regs[1]&";"},0)
		else
			ADD_CODE({"r0 = "&stack_regs[1]&";"},0)
		end if		
		s = stack_regs[1]
		free_reg_bfin()
	else
		s = "[p1++]"
	end if
	return r&" = "&s&";"
end function


function stackref_bfin(integer i)
	if i=0 then
		return "r0"
	elsif useRegs then
		if i <= length(stack_regs) then
			return stack_regs[i]
		else
			return sprintf("[p1 + %d]",(i-(length(stack_regs)+1))*4)
		end if
	end if
	return sprintf("[p1 + %d]",(i-1)*4)
end function


-- Handle comparison operations
procedure CMPI_bfin(sequence cond,integer p)
	integer n
	
	n = 0
	if p<length(tokens) and optLevel>=4 then
		n = tokens[p+1][1]

		-- Is the next token IF or WHILE ?
		if n=W_IF or n=W_WHILE then
			if equal(cond,">") then
				fastCmp = "cc"
				cond = "<="
			elsif equal(cond,">=") then
				fastCmp = "cc"
				cond = "<"
			else
				fastCmp = "!cc"
			end if
			ADD_CODE({"r1 = r0;",POP_bfin("r2"),POP_bfin("r0"),"cc = r2 "&cond&" r1;"},0)

		-- Is the next token ?LEAVE ?
		elsif n=W_LEAVETRUE then
			if equal(cond,">") then
				fastCmp = "!cc"
				cond = "<="
			elsif equal(cond,">=") then
				fastCmp = "!cc"
				cond = "<"
			else
				fastCmp = "cc"
			end if
			ADD_CODE({"r1 = r0;",POP_bfin("r2"),POP_bfin("r0"),"cc = r2 "&cond&" r1;"},0)
		
		else
			n = 0
		end if
	end if

	-- The general case
	if not n then
		ADD_CODE({"move.l (a2)+,d1","cmp.l d1,d0","s"&cond&" d0","ext.w d0","ext.l d0"},0)
	end if
end procedure



function compile_bfin()
	integer n,p,q,r,lastComp,case_,hasDefault,touch,optI
	sequence s,t,token,wordId,loopStack,loopStacks,params,caseLbl,asm,litBackup,context
	
	
	if verbose then
		puts(1,"Compiling\n")
	end if
	
	lastComp = 0
	case_ = -1
	loopStack = {}
	touch = 1
	fastCmp = ""
	optI = 0
	useRegs = 0
	inline = 0
	
	-- Prepare loop optimisation
	if optLevel>=4 then
		p = 1
		n = 0
		q = 0	
		while p<=length(tokens) do
			if tokens[p][1]=W_DO then
				tokens[p] &= q
				n = p
				q += 1
			elsif tokens[p][1]=W_LOOP or tokens[p][1]=W_INCLOOP then
				if n then
					tokens[n] &= 1
					n = 0
				end if
				q -= 1
				if q<0 then
					ERROR("DO...LOOP/+LOOP unbalanced",tokens[p])
				end if
			end if
			p += 1
		end while
	end if
	
	
	p = 1
	
	-- Second stage: translate
	while p<=length(tokens) do
		if tokens[p][1] = NUMBER then
			if p=length(tokens) or tokens[p+1][1]!=W_CONST then
				tokens[p] &= get_code_length()+1
				PUSH_bfin(tokens[p][2])
			end if
			
			
		-- NOTOUCH
		elsif tokens[p][1] = W_NOTOUCH then
			p += 1
			asm = {}
			while p<=length(tokens) do
				if tokens[p][1] = W_INCLUDE then
					if not find(tokens[p+1][2],includes) then
						includes = append(includes,tokens[p+1][2])
						p += 1
					end if
				elsif tokens[p][1] = W_TOUCH then
					if length(asm) then
						ADD_CODE({asm},0)
						asm = {}
					end if
					exit
				elsif tokens[p][1] = NEWLINE then
					if length(asm) then
						--if length(pendingWordDef) then
						--	code = append(code,asm)
						--else
						--	maincode = append(maincode,asm)
						--end if
						ADD_CODE({asm},0)
						asm = {}
					end if
				elsif tokens[p][1] = W_CALL then
					asm &= "call"
				elsif tokens[p][1] = UNKNOWN then
					if equal(lower(tokens[p][2]),"includelib") then
						if not find(tokens[p+1][2],includes) then
							includes = append(includes,tokens[p+1][2])
							p += 1
						end if
					else
						-- TODO: append to code/maincode
						if length(asm) then
							asm &= ' '
						end if
						asm &= tokens[p][2]
					end if
				elsif tokens[p][1] = NUMBER then
					if length(asm) then
						asm &= ' '
					end if
					asm &= sprintf("%d",tokens[p][2])						
				else
					if length(asm) then
						asm &= ' '
					end if
					asm &= tokens[p][2]				
				end if
				p += 1
			end while
			
		-- SP0
		--elsif tokens[p][1] = W_SPBOT then
		--	ADD_CODE({"push dword ptr [stackBase]"},0)
		
		-- SP@
		--elsif tokens[p][1] = W_SPTOP then
		--	ADD_CODE({"push esp"},0)
			
		-- !
		elsif tokens[p][1] = W_STORE then
			ADD_CODE({POP_bfin("r1"),"p5 = r0;","[p5] = r1;",POP_bfin("r0")},0)

		-- c!
		elsif tokens[p][1] = W_CSTORE then
			ADD_CODE({POP_bfin("r1"),"p5 = r0;","b[p5] = r1;",POP_bfin("r0")},0)

		-- w!
		elsif tokens[p][1] = W_WSTORE then
			ADD_CODE({POP_bfin("r1"),"p5 = r0;","w[p5] = r1;",POP_bfin("r0")},0)

		-- !r+
		elsif tokens[p][1] = W_STORE_R_INC then
--			ADD_CODE({"ldr r1,[r10],#4","str r0,[r1],#4","mov r0,r1"},0)

		-- ON
		elsif tokens[p][1] = W_ON then
		--	ADD_CODE({"pop eax","mov dword ptr [eax],-1"},0)
			ADD_CODE({"r1 = -1;","a5 = r0;","r0 = [p1++];","[a5] = r1;"},0)

		-- OFF
		elsif tokens[p][1] = W_OFF then
			ADD_CODE({"r1 = 0;","a5 = r0;","r0 = [p1++];","[a5] = r1;"},0)
			
		-- @
		elsif tokens[p][1] = W_FETCH then
			ADD_CODE({"p5 = r0;","r0 = [p5];"},0)
		-- c@
		elsif tokens[p][1] = W_CFETCH then
			ADD_CODE({"p5 = r0;","r0 = b[p5](z);"},0)
		-- cs@
		elsif tokens[p][1] = W_CSFETCH then
			ADD_CODE({"p5 = r0;","r0 = b[p5](x);"},0)
		-- w@
		elsif tokens[p][1] = W_WFETCH then
			ADD_CODE({"p5 = r0;","r0 = w[p5](z);"},0)
	
		-- a@
		--elsif tokens[p][1] = W_ACCFETCH then
		--	ADD_CODE({"push eax"},0)
			

		-- wa+
		elsif tokens[p][1] = W_ADRPLUSW then
			ADD_CODE({"r1 = [p1++];","r0 <<= 1;","r0 = r0 + r1;"},0)
			
		-- +
		elsif tokens[p][1] = W_ADD then
			ADD_CODE({POP_bfin("r1"),"r0 = r0 + r1;"},0)

		-- -
		elsif tokens[p][1] = W_SUB then
			ADD_CODE({POP_bfin("r1"),"r0 = r1 - r0;"},0)

		-- *
		elsif tokens[p][1] = W_MUL then
			ADD_CODE({POP_bfin("r1"),"r0 *= r1;"},0)
			
		-- /
		elsif tokens[p][1] = W_DIV then
			usesDiv = 1
			--ADD_CODE({"mov.l @r10+,r1","bsr __sdiv","nop"},0)
			--ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","divs d1,d0","ext.l d0"},0)
		-- MOD
		elsif tokens[p][1] = W_MOD then
			--ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","divs d1,d0","swap d0","ext.l d0"},0)
		-- /MOD
		elsif tokens[p][1] = W_DIVMOD then
			--ADD_CODE({"move.l d0,d1","move.l (a2),d0","divs d1,d0","move.l d0,d1","ext.l d0","swap d1","ext.l d1","move.l d1,(a2)"},0)

		
		-- /C
		elsif tokens[p][1] = W_CSIZE then
			PUSH_bfin(1)

		-- /F
		elsif tokens[p][1] = W_FSIZE then
			PUSH_bfin(8)

		-- /N
		elsif tokens[p][1] = W_NSIZE then
			PUSH_bfin(4)
			
		-- 1-
		elsif tokens[p][1] = W_DEC then
			ADD_CODE({"r0 += -1;"},0)
		-- 1+
		elsif tokens[p][1] = W_INC then
			ADD_CODE({"r0 += 1;"},0)
			
		-- 2*
		elsif tokens[p][1] = W_MUL2 then
			ADD_CODE({"r0 = r0 + r0;"},0)

		-- 2/
		elsif tokens[p][1] = W_DIV2 then
			ADD_CODE({"r0 >>>= 1;"},0)
		
		-- <<
		elsif tokens[p][1] = W_SHL then
			if p>1 then
				if tokens[p-1][1]=NUMBER then
					n = get_code_length()
					if length(tokens[p-1])=4 then
						if (n+1-tokens[p-1][4])=2 then
							free_reg_bfin()
						end if
						ADD_CODE({sprintf("r0 <<= %d;",tokens[p-1][2])},0)
						REMOVE_CODE(tokens[p-1][4],n)
					else
						ADD_CODE({POP_bfin("r0"),sprintf("r0 <<= %d;",tokens[p-1][2])},0)
					end if
				else
					ADD_CODE({"r1 = [p1++];"},0)
					ADD_CODE({"r1 <<= r0;","r0 = r1;"},0)				
				end if
			else
				ERROR("Unexpected token",tokens[p])
			end if

		-- >>
		elsif tokens[p][1] = W_SHR then
			if p>1 then
				if tokens[p-1][1]=NUMBER then
					n = get_code_length()
					if length(tokens[p-1])=4 then
						if (n+1-tokens[p-1][4])=2 then
							free_reg_bfin()
						end if
						REMOVE_CODE(tokens[p-1][4],n)
						ADD_CODE({sprintf("r0 >>= %d;",tokens[p-1][2])},0)
					else
						ADD_CODE({POP_bfin("r0"),sprintf("r0 >>= %d;",tokens[p-1][2])},0)
					end if
				else
					ADD_CODE({"r1 = [p1++];"},0)
					ADD_CODE({"r1 >>= r0;","r0 = r1;"},0)
				end if
			else
				ERROR("Unexpected token",tokens[p])
			end if
		
		-- BOUNDS
		elsif tokens[p][1] = W_BOUNDS then
			ERROR("Unimplemented word",tokens[p])
		
		-- DEFER
		elsif tokens[p][1] = W_DEFER then
			if p<length(tokens) then
				if tokens[p+1][1]=UNKNOWN then
					if not bsearch(tokens[p+1][2],userWords[1]) then
						deferred = assoc_insert(tokens[p+1][2],make_label(tokens[p+1][2]),deferred)
						p += 1
					else
						ERROR(ERROR_REDECLARED,tokens[p+1])
					end if
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
					
		-- :
		elsif tokens[p][1] = W_COLON then
			if p<length(tokens) then
				if tokens[p+1][1]=UNKNOWN then
					if not bsearch(tokens[p+1][2],userWords[1]) then  --WORDS) then
						if not length(pendingWordDef) then
							q = bsearch(tokens[p+1][2],publics[1])
							if (referred[2][bsearch(tokens[p+1][2],referred[1])][1]<2) and
							   (q<=0) and
							   (not noMangle) then
								ignoreOutput = 1
							end if

							inline = allowInline
							if allowInline then
								q = p+2
								n = 1
								while q < length(tokens) do
									if tokens[q][1] = W_REPEAT or tokens[q][1] = W_UNTIL or
									   tokens[q][1] = W_PARAMSTART or tokens[q][1] = W_CASE then
										inline = 0
										exit
									elsif tokens[q][1] = W_BEGIN or tokens[q][1] = W_DO then
										inline = 0
										exit
									elsif tokens[q][1] = W_SEMICOLON then
										exit
									elsif tokens[q][1] = UNKNOWN then
										if bsearch(tokens[q][2],userWords[1]) then
											inline = 0
											exit
										elsif bsearch(tokens[q][2],deferred[1]) then
											inline = 0
											exit
										end if
									end if
									q += 1
									if q-p >9 then
										inline = 0
										exit
									end if
								end while
							end if
							if inline then
								puts(1,tokens[p+1][2]&" candidate for inlining\n")
							end if
			
							pendingWordDef = tokens[p+1]
							params = {{},{}}
			
							q = bsearch(tokens[p+1][2],publics[1])
							if (referred[2][bsearch(tokens[p+1][2],referred[1])][1]<=4) and
							   (q<=0) and
							   (not noMangle) and
							   inline then
								ignoreOutput = 1
								wordId = tokens[p+1][2]
								--pendingWordDef &= 1
							end if
							
							if not inline then
								ADD_CODE({NL&"// "&tokens[p+1][2]},0)
							
								if q<=0 then
									wordId = make_label(tokens[p+1][2])
								else
									wordId = tokens[p+1][2]
								end if
								ADD_CODE({wordId&":"},0)
								ADD_CODE({"p2 += -4;","r0 = rets;","[p2] = r0;"},0)
		

								--loopStacks = loopStack
								context = {ifStack,loopStack}
								ifStack = {}
								loopStack = {}
							end if
							
							p += 1
						else
							ERROR(ERROR_WORD_INSIDE_WORD,tokens[p+1])
						end if
					else
						ERROR(ERROR_REDECLARED,tokens[p+1])
					end if
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

		
		-- {
		elsif tokens[p][1] = W_PARAMSTART then
			if length(pendingWordDef) then
				params = {{},{}}
				p += 1
				n = 0
				while 1 do
					if p>length(tokens) then
						ERROR(ERROR_EOF,tokens[p-1])
					elsif tokens[p][1] = UNKNOWN then
						params = assoc_insert(tokens[p][2],{n},params)
						n += 4
					elsif tokens[p][1] = W_PARAMDELIM then
						while tokens[p][1]!=W_PARAMEND do
							p += 1
						end while
						exit
					elsif tokens[p][1] = W_PARAMEND then
						exit
					else
						ERROR("Syntax error",tokens[p])
					end if
					p += 1
				end while
				if length(params[1]) and length(pendingWordDef)=3 then
					ADD_CODE({"mov.l r13,@-r11","mov.l r14,@-r11","mov r10,r13","mov r0,r14"},0)
				elsif length(params[1]) then
					ADD_CODE({"mov r10,r13","mov r0,r14"},0)
				end if
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if
		
		-- ;
		elsif tokens[p][1] = W_SEMICOLON then
			if length(pendingWordDef)=3 then
				if inline then
					inlines = append(inlines,inlinecode)
					userWords = assoc_insert(pendingWordDef[2],{wordId,inline,length(inlines)},userWords)
				else
					ADD_CODE({wordId&"_return:"},0)
					if length(params[1]) then
						--ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","push ecx","ret"},0)
						ADD_CODE({"mov.l @r11+,r14","mov.l @r11+,r13","lds.l @r11+,pr","rts","nop"},0)
					else
						ADD_CODE({"r1 = [p2++];","rets = r1;","rts;"},0)
					end if

					userWords = assoc_insert(pendingWordDef[2],{wordId,inline},userWords)
					if length(loopStack) then
						ERROR(sprintf("%d open do-loop(s) inside "&pendingWordDef[2],
						              length(loopStack)),tokens[p])
					end if
					ifStack = context[1]
					loopStack = context[2]
					context = {}
				end if
				inline = 0
				inlinecode = {}
				pendingWordDef = {}
				ignoreOutput = 0
				
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if

		-- ;@
		elsif tokens[p][1] = W_SEMICOLONPOP then
			ERROR("Unimplemented word",tokens[p])

			
		-- '
		elsif tokens[p][1] = W_QUOTE then
			n = bsearch(tokens[p+1][2],userWords[1])
			if n then
			--	ADD_CODE({"mov eax,offset "&userWords[2][n],"push eax"},0)
				p += 1
			end if
			
		-- =
		elsif tokens[p][1] = W_EQ then
			CMPI_bfin("==",p)
			
		-- <>
		elsif tokens[p][1] = W_NE then
			CMPI_bfin("!=",p)
		
		-- >
		elsif tokens[p][1] = W_GT then
			CMPI_bfin(">",p)
		-- >=
		elsif tokens[p][1] = W_GE then
			CMPI_bfin(">=",p)

		-- <
		elsif tokens[p][1] = W_LT then
			CMPI_bfin("<",p)
			--ADD_CODE({"pop eax","xor ecx,ecx","cmp [esp],eax","setl cl","neg ecx","mov [esp],ecx"},0)
		-- <=
		elsif tokens[p][1] = W_LE then
			CMPI_bfin("<=",p)

		-- 0=
		elsif tokens[p][1] = W_EQ0 then
			ADD_CODE({"r1 = -1;","r2 = 0;","cc = r0==0;","if cc r0 = r1;","if !cc r0 = r2;"},0)
		-- 0<>
		elsif tokens[p][1] = W_NE0 then
			ADD_CODE({"r1 = -1;","r2 = 0;","cc = r0==0;","if !cc r0 = r1;","if cc r0 = r2;"},0)
		-- 0>
		elsif tokens[p][1] = W_GT0 then
		--	ADD_CODE({"pop eax","xor ecx,ecx","cmp eax,0","setg cl","neg ecx","mov [esp],ecx"},0)
			ERROR("Unimplemented word",tokens[p])
		-- 0<
		elsif tokens[p][1] = W_LT0 then
		--	ADD_CODE({"pop eax","xor ecx,ecx","cmp eax,0","setl cl","neg ecx","mov [esp],ecx"},0)
			ERROR("Unimplemented word",tokens[p])


		-- NEGATE
		elsif tokens[p][1] = W_NEG then
			ADD_CODE({"r0 = - r0;"},0)
			

		-- ABS
		elsif tokens[p][1] = W_ABS then
			ADD_CODE({"r0 = abs r0;"},0)

		-- ALLOT
		elsif tokens[p][1] = W_ALLOT then
			ADD_CODE({"p5 = r0;","p0 = p0 + r5;"},0)
			
		-- AND
		elsif tokens[p][1] = W_AND then
			ADD_CODE({POP_bfin("r1"),"r0 = r0 & r1;"},0)

		-- BEGIN
		elsif tokens[p][1] = W_BEGIN then
			s = sprintf("__loop_%04x",loops)
			loops += 1
			loopStack = append(loopStack,s)
			ADD_CODE({s&":"},0)
			n = 0
			if optLevel >= 4 then
				q = p+1
				n = 1
				while q < length(tokens) do
					if tokens[q][1] = W_REPEAT or tokens[q][1] = W_UNTIL then
						exit
					elsif tokens[q][1] = W_BEGIN or tokens[q][1] = W_DO then
						n = 0
						exit
					elsif tokens[q][1] = UNKNOWN then
						if bsearch(tokens[q][2],userWords[1]) then
							n = 0
							exit
						elsif bsearch(tokens[q][2],deferred[1]) then
							n = 0
							exit
						end if
					end if
					q += 1
				end while
			end if
			
			--if n then
			--	useRegs = 1
			--end if
			
			--printf(1,"begin "&loopStack[length(loopStack)]&", line %d\n",tokens[p][3])
	
		-- DO
		elsif tokens[p][1] = W_DO then
			s = sprintf("__loop_%04x",loops)
			loops += 1
			loopStack = append(loopStack,s)
			n = 0

			-- Check if the loop variables can be placed in esi/edi
			if optLevel>=4 then
				-- Is this an innermost loop ?
				if length(tokens[p])=5 then
					q = p+1
					n = 1
					usesI = 0
					while tokens[q][1]!=W_LOOP and tokens[q][1]!=W_INCLOOP do
						if tokens[q][1]=UNKNOWN then
							if bsearch(tokens[q][2],userWords[1]) then
								n = 0
								exit
							elsif bsearch(tokens[q][2],deferred[1]) then
								n = 0
								exit
							end if
						elsif tokens[q][1] = W_I then
							usesI = 1
						elsif tokens[q][1]=W_DOT or tokens[q][1]=W_DOTSTRING or
						      tokens[q][1]=W_EXPECT or tokens[q][1]=W_KEY then
						      	n = 0
						      	exit
						end if
						q += 1
						if q>length(tokens) then
							ERROR("Expected LOOP/+LOOP, found end of file",tokens[p])
						end if
					end while
				end if
			end if
			
			if n then
				--puts(1,"Innermost loop: "&s&"\n")
				if optLevel>=4 then
					--useRegs = 1
					stack_regs = {}
				end if
				
				n = 0
				if (not usesI) and p>2 and tokens[q][1]=W_LOOP then
					if tokens[p-1][1]=NUMBER and tokens[p-2][1]=NUMBER then
						-- Use hardware loop
						n = 1
						q = get_code_length()
						ADD_CODE({sprintf("r1 = %d;",tokens[p-2][2]-tokens[p-1][2]),"lc1 = r1;","loop "&s&" lc1;","loop_begin "&s&";"},0)
						if length(tokens[p-2])=4 then
							REMOVE_CODE(tokens[p-2][4],q)
						else
							-- Shouldn't happen
							ERROR("Internal error : Bad token",tokens[p-2])
						end if
					end if
				end if
			end if
			if not n then
				ADD_CODE({"p3 += -8;","[p3] = r6;","[p3 + 4] = r7;","r6 = r0;",POP_bfin("r7"),POP_bfin("r0")},0)
				--ADD_CODE({"mov.l r7,@-r12","mov.l r6,@-r12","mov.l @r10+,r7","mov r0,r6","mov.l @r10+,r0"},0)
				usesI = 1
				ADD_CODE({s&":"},0)
			end if
			

		-- LOOP
		elsif tokens[p][1] = W_LOOP then
			if not usesI then
				ADD_CODE({"loop_end "&loopStack[length(loopStack)]&";"},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			else
				if optI then
					--ADD_CODE({"inc esi","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
					optI = 0
				else
					ADD_CODE({"r6 += 1;","cc = r6==r7;","if !cc jump "&loopStack[length(loopStack)]&";"},0)				
				end if
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				ADD_CODE({"r6 = [p3++];","r7 = [p3++];"},0)
				--ADD_CODE({"mov.l @r12+,r6","mov.l @r12+,r7"},0)
			end if
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if
			useRegs = 0
			stack_regs = {}
			
		-- +LOOP
		elsif tokens[p][1] = W_INCLOOP then
			if optI then
				--ADD_CODE({"pop eax","add esi,eax","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				--ADD_CODE({"mov eax,loopstackptr","pop ebx","sub eax,8","add [eax+4],ebx","mov ecx,[eax]","cmp [eax+4],ecx","jne "&loopStack[length(loopStack)]},0)
				ADD_CODE({"r6 = r6 + r0;",POP_bfin("r0"),"cc = r6==r7;","if !cc jump "&loopStack[length(loopStack)]&";"},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			ADD_CODE({"r6 = [p3++];","r7 = [p3++];"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if
			useRegs = 0
			stack_regs = {}
			
		-- I
		elsif tokens[p][1] = W_I then
			if optI then
				--ADD_CODE({"push esi"},0)
			else
				--ADD_CODE({"mov eax,loopstackptr","push dword ptr [eax-4]"},0)
				PUSH_bfin("r6")
				--ADD_CODE({"r0 = r6;"},0)
			end if
		-- J
		elsif tokens[p][1] = W_J then
			PUSH_bfin("[p3]")
			--ADD_CODE({"r0 = [p3];"},0)
			
		-- CASE
		elsif tokens[p][1] = W_CASE then
			if case_ = -1 then
				case_ = 0
				hasDefault = 0
				caseLbl = sprintf("__case_%06x",cases)
				ADD_CODE({"r5 = r0;",POP_bfin("r0")},0)
				
			else
				ERROR("CASE inside CASE",tokens[p])
			end if
		
		-- OF
		elsif tokens[p][1] = W_OF then
			if case_ >= 0 and not hasDefault then
				--ADD_CODE({"ldr r0,[sp],#4","cmp r0,r8","bne "&caseLbl&sprintf("_%04x_false",case_)},0)
				ADD_CODE({"mov.l @r10+,r0","cmp/eq r8,r0","bf "&caseLbl&sprintf("_%04x_false",case_)},0)
			elsif hasDefault then
				ERROR("OF found after DEFAULT",tokens[p])
			else
				ERROR("OF outside CASE",tokens[p])
			end if
		
		-- ENDOF
		elsif tokens[p][1] = W_ENDOF then
			if case_ >= 0 then
				ADD_CODE({"bra "&caseLbl&"_end","nop"},0)
				ADD_CODE({caseLbl&sprintf("_%04x_false:",case_)},0)
				case_ += 1
			else
				ERROR("ENDOF outside CASE",tokens[p])
			end if
		
		-- ENDCASE
		elsif tokens[p][1] = W_ENDCASE then
			if case_ >= 0 then
				ADD_CODE({caseLbl&"_end:"},0)
				case_ = -1
				cases += 1
			else
				ERROR("Unmatched ENDCASE",tokens[p])
			end if
		
		-- DEFAULT
		elsif tokens[p][1] = W_DEFAULT then
			if case_ >= 0 then
				hasDefault = 1
			else
				ERROR("DEFAULT outside CASE",tokens[p])
			end if

		-- EXTERN
		elsif tokens[p][1] = W_EXTERN then
			if tokens[p+1][1] = W_CONST then
				if length(tokens)>p+1 then
					if not length(pendingWordDef) then
						if tokens[p+2][1]=UNKNOWN and bsearch(tokens[p+2][2],constants[1])=0 then
							s = sprintf("const_%07x",length(constants[1]))
							n = length(maincode)
							constants = assoc_insert(tokens[p+2][2],{s,0,n+1},constants)
							ADD_CODE({"mov.l "&literals[r][1]&",r1","mov.l "&literals[q][1]&",r2","mov.l r1,@r2"},0)
							p += 2
						else
							ERROR(ERROR_REDECLARED,tokens[p+2])
							p += 2
						end if
					else
						ERROR("Found constant inside word definition",tokens[p])				
					end if
				else
					ERROR(ERROR_EOF,tokens[p])
				end if
			end if
			
		-- CONSTANT
		elsif tokens[p][1] = W_CONST then
			if length(tokens)>p then
				if not length(pendingWordDef) then
					if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],constants[1])=0 then
						s = sprintf("const_%07x",length(constants[1]))
						n = length(maincode)
						if tokens[p-1][1]=NUMBER then
							constants = assoc_insert(tokens[p+1][2],{s,0,n+1,tokens[p-1][2]},constants)
						else
							--q = add_literal(s)
							constants = assoc_insert(tokens[p+1][2],{s,0,n+1},constants)
							--ADD_CODE({"mov.l "&literals[q][1]&",r1","mov.l r0,@r1","mov.l @r10+,r0"},0)
							ADD_CODE({"p5.h = "&s&";","p5.l = "&s&";","[p5] = r0;",POP_bfin("r0")},0)
						end if
						p += 1
					else
						ERROR(ERROR_REDECLARED,tokens[p+1])
						p += 1
					end if
				else
					ERROR(ERROR_CONST_INSIDE_WORD,tokens[p])				
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

		
		-- VARIABLE
		elsif tokens[p][1] = W_VAR then
			if length(tokens)>p then
				if not length(pendingWordDef) then
					if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],variables[1])=0 then
						s = sprintf("var_%07x",length(variables[1]))
						variables = assoc_insert(tokens[p+1][2],{s,4},variables)
						ADD_CODE({"p5.h = "&s&";","p5.l = "&s&";","[p5] = p0;","p0 += 4;"},0)
						p += 1
					else
						ERROR(ERROR_REDECLARED,tokens[p+1])
						p += 1
					end if
				else
					ERROR(ERROR_VAR_INSIDE_WORD,tokens[p])				
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

		elsif tokens[p][1] = W_PUBLIC then
			if length(tokens)>p then
				if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],publics[1])=0 then
					publics = assoc_insert(tokens[p+1][2],{0},publics)
					p += 1
				else
					ERROR("Expected a unique name",tokens[p+1])
					p += 1
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		-- BYE
		elsif tokens[p][1] = W_BYE then
		

		-- HERE
		elsif tokens[p][1] = W_HERE then
			PUSH_bfin("r0")
			ADD_CODE({"r0 = p0;"},0)
		-- ,
		elsif tokens[p][1] = W_COMMA then
			ADD_CODE({"[p0++] = r0;",POP_bfin("r0")},0)
			
		-- c,
		elsif tokens[p][1] = W_CCOMMA then
			ADD_CODE({"b[p0++] = r0;",POP_bfin("r0")},0)

		-- w,
		elsif tokens[p][1] = W_WCOMMA then
			ADD_CODE({"w[p0++] = r0;",POP_bfin("r0")},0)
			
		
		-- CALL
		--elsif tokens[p][1] = W_CALL then
		--	ADD_CODE({"call "&tokens[p+1][2]},0)
		--	p += 1
			
		-- DROP
		elsif tokens[p][1] = W_DROP then
			ADD_CODE({POP_bfin("r0")},0)

		-- 2DROP
		elsif tokens[p][1] = W_2DROP then
			ADD_CODE({"p1 += 4;",POP_bfin("r0")},0)
			
		-- DUP
		elsif tokens[p][1] = W_DUP then
			PUSH_bfin("r0")
		-- 2DUP
		elsif tokens[p][1] = W_2DUP then
			--ADD_CODE({"mov r2,r0","ldr r1,[r10]","stmfd r10!,{r1,r2}"},0)
			--ADD_CODE({"r1 = [p1];","p1 += -8;","

		-- PICK
		elsif tokens[p][1] = W_PICK then
			if tokens[p-1][1] = NUMBER then
				ADD_CODE({"r0 = "&stackref_bfin(tokens[p-1][2])&";"},0)
			else
				ADD_CODE({"p5 = r0;","p5 = p1 + (p5 << 2);","r0 = [p5];"},0)
			end if
			
		-- NIP
		elsif tokens[p][1] = W_NIP then
			ADD_CODE({"p1 += 4;"},0)
			
		-- TUCK
		elsif tokens[p][1] = W_TUCK then
			ADD_CODE({"r1 = [p1];","p1 += -4;","[p1] = r1;","[p1+4] = r0;"},0)		

		-- OVER
		elsif tokens[p][1] = W_OVER then
			ADD_CODE({"r1 = [p1];","p1 += -4;","[p1] = r0;","r0 = r1;"},0)
			
		-- ELSE
		elsif tokens[p][1] = W_ELSE then
			if length(ifStack) and and_bits(length(ifStack),1)=0 then
				ADD_CODE({"jump "&ifStack[2]&";"},0)
				ADD_CODE({ifStack[1]&":"},0)
				ifStack = ifStack[2..length(ifStack)]
			else
				ERROR("Unmatched ELSE",tokens[p])
			end if
		
		-- EXIT
		elsif tokens[p][1] = W_EXIT then
			if length(pendingWordDef) then
				ADD_CODE({"jump "&wordId&"_return;"},0)
			else
				ERROR("Malplaced EXIT",tokens[p])
			end if
		
		-- CMOVE
		--elsif tokens[p][1] = W_CMOVE then
		--	if optI then
		--		ADD_CODE({"mov ebx,esi","mov edx,edi","pop ecx","pop edi","pop esi","rep movsb","mov edi,edx","mov esi,ebx"},0)
		--	else
		--		ADD_CODE({"pop ecx","pop edi","pop esi","rep movsb"},0)
		--	end if
			
		-- ERASE
		--elsif tokens[p][1] = W_ERASE then
		--	if optI then
		--		ADD_CODE({"mov ebx,edi","pop ecx","pop edi","xor eax,eax","rep stosb","mov edi,ebx"},0)
		--	else
		--		ADD_CODE({"pop ecx","pop edi","xor eax,eax","rep stosb"},0)
		--	end if
			
		-- FILL
		elsif tokens[p][1] = W_FILL then
			--if optI then
			--	ADD_CODE({"mov ebx,edi","pop eax","pop ecx","pop edi","rep stosb","mov edi,ebx"},0)
			--else
			--	ADD_CODE({"pop eax","pop ecx","pop edi","rep stosb"},0)
			--end if
			s = sprintf("__str_%06x",strops)
			--ADD_CODE({"ldr r1,[r10],#4","ldr r2,[r10],#4",s&':',"strb r0,[r2],#1","subs r1,r1,#1","bne "&s,"ldr r0,[r10],#4"},0)
			strops += 1			

		-- IF
		elsif tokens[p][1] = W_IF then
			s = sprintf("__if_%06x",ifs)
			ifStack = {s&"_false",s&"_true"}&ifStack
			ifs += 1
			if length(fastCmp) then
				ADD_CODE({"if "&fastCmp&" jump "&ifStack[1]&";"},0)
				fastCmp = ""
			else
				ADD_CODE({"cc = r0==-1;","r0 = [p1++];","if !cc jump "&ifStack[1]&";"},0)
			end if
			
		-- LEAVE
		elsif tokens[p][1] = W_LEAVE then
			ADD_CODE({"jump "&loopStack[length(loopStack)]&"_end"},0)
		-- ?LEAVE
		elsif tokens[p][1] = W_LEAVETRUE then
			if length(fastCmp) then
				ADD_CODE({"if "&fastCmp&" jump "&loopStack[length(loopStack)]&"_end;"},0)
				fastCmp = ""
			else
				ADD_CODE({"cc = r0==-1;","r0 = [p1++];","if cc jump "&loopStack[length(loopStack)]&"_end;"},0)
			end if
			
		-- NULL
		elsif tokens[p][1] = W_NULL or tokens[p][1]=W_FALSE then
			PUSH_bfin(0)
		-- TRUE
		elsif tokens[p][1] = W_TRUE then
			PUSH_bfin(-1)
		-- BL
		elsif tokens[p][1] = W_BL then
			PUSH_bfin(' ')
		
		-- OR
		elsif tokens[p][1] = W_OR then
			ADD_CODE({POP_bfin("r1"),"r0 = r0 | r1;"},0)

		-- REPEAT
		elsif tokens[p][1] = W_REPEAT then
			if length(loopStack) then
				ADD_CODE({"jump "&loopStack[length(loopStack)]&";"},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				if length(loopStack) = 1 then
					loopStack = {}
				else
					loopStack = loopStack[1..length(loopStack)-1]
				end if
			else
				ERROR("REPEAT with no matching BEGIN",tokens[p])
			end if
			useRegs = 0
			stack_regs = {}
			
		-- ROT
		elsif tokens[p][1] = W_ROT then
			ADD_CODE({"move.l (a2)+,d1","move.l (a2)+,d2","move.l d1,-(a2)","move.l d0,-(a2)","move.l d2,d0"},0)

		-- SWAP
		elsif tokens[p][1] = W_SWAP then
			ADD_CODE({"r1 = [p1];","[p1] = r0;","r0 = r1;"},0)
			
		-- THEN
		elsif tokens[p][1] = W_THEN then
			while 1 do
				ADD_CODE({ifStack[1]&":"},0)
				if length(ifStack)>1 then
					ifStack = ifStack[2..length(ifStack)]
				else
					ifStack = {}
				end if
				if not and_bits(length(ifStack),1) then
					exit
				end if
			end while

		-- AGAIN
		elsif tokens[p][1] = W_AGAIN then
			if not length(loopStack) then
				ERROR("Unmatched AGAIN",tokens[p])
			else
				ADD_CODE({"jump "&loopStack[length(loopStack)]&";"},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				if length(loopStack) = 1 then
					loopStack = {}
				else
					loopStack = loopStack[1..length(loopStack)-1]
				end if
			end if
			useRegs = 0
			stack_regs = {}
			
		-- UNTIL
		elsif tokens[p][1] = W_UNTIL then
			if not length(loopStack) then
				ERROR("Unmatched UNTIL",tokens[p])
			else
				ADD_CODE({"cc = r0==-1;",POP_bfin("r0"),"if !cc jump "&loopStack[length(loopStack)]&";"},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				if length(loopStack) = 1 then
					loopStack = {}
				else
					loopStack = loopStack[1..length(loopStack)-1]
				end if
			end if
			useRegs = 0
			stack_regs = {}
			
		-- WHILE
		elsif tokens[p][1] = W_WHILE then
			if length(fastCmp) then
				ADD_CODE({"if "&fastCmp&" jump "&loopStack[length(loopStack)]&"_end;"},0)
				fastCmp = ""
			else
				ADD_CODE({"cc = r0==-1;",POP_bfin("r0"),"if !cc jump "&loopStack[length(loopStack)]&"_end;"},0)
			end if
			
		-- XOR
		elsif tokens[p][1] = W_XOR then
			ADD_CODE({POP_bfin("r1"),"r0 = r0 ^ r1;"},0)
		
		-- Z"
		elsif tokens[p][1] = W_ZSTRING then
			if p<length(tokens) then
				p += 1
				s = ""
				while 1 do
					if p<=length(tokens) then
						s &= tokens[p][2]
						if s[length(s)]='\"' then
							if length(s)>1 then	
								s = s[1..length(s)-1]
							else
								s = ""
							end if
							exit
						end if
						p += 1
					else
						ERROR("Unexpected end of file",tokens[p-1])
					end if
				end while
				literals = append(literals,{sprintf("lit_%06x_%d",{length(literals)+globLit,length(pendingWordDef)}),".asciz \""&s&"\""})
				ADD_CODE({"p1 += -4;","[p1] = r0;","r0.h = "&literals[length(literals)][1]&";","r0.l = "&literals[length(literals)][1]&";"},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		elsif tokens[p][1] = UNKNOWN then

			-- Check user-defined words
			n = bsearch(tokens[p][2],userWords[1])
			if n then
				if userWords[2][n][2] then
					ADD_CODE(inlines[userWords[2][n][3]],0)
				else
					ADD_CODE({"call "&userWords[2][n][1]&";"},0)
				end if
			end if
			
			if not n then
				if length(pendingWordDef) then
					n = bsearch(tokens[p][2],params[1])
					if n then
						if params[2][n][1] = 0 then
							ADD_CODE({"mov.l r0,@-r10","mov r14,r0"},0)
						else
							ADD_CODE({"mov.l r0,@-r10",sprintf("mov.l @(%d,r13),r0",floor((params[2][n][1]-4)))},0)
						end if
					end if
				end if
			end if


			-- Check variables			
			if not n then
				n = bsearch(tokens[p][2],variables[1])
				if n then
					--q = add_literal(variables[2][n][1])
					--ADD_CODE({"mov.l r0,@-r10","mov.l "&literals[q][1]&",r0"},0) --&literals[variables[2][n][3]][1]&",r0"},0)
					ADD_CODE({"p1 += -4;","[p1] = r0;","r0.h = "&variables[2][n][1]&";","r0.l = "&variables[2][n][1]&";"},0)
				end if
			end if
			

			-- Check constants
			if not n then
				n = bsearch(tokens[p][2],constants[1])
				if n then
					if length(constants[2][n])=3 then
						--ADD_CODE({"mov.l r0,@-r10","mov.l #"&constants[2][n][1]&",r1","mov.l @r1,r0"},0)
						ADD_CODE({"p1 += -4;","[p1] = r0;","p5.h = "&constants[2][n][1]&";","p5.l = "&constants[2][n][1]&";","r0 = [p5];"},0)
						constants[2][n][2] = 1	-- Mark the constant as being used
					elsif length(constants[2][n])=4 then
						tokens[p] &= get_code_length()+1
						PUSH_bfin(constants[2][n][4])
					elsif length(constants[2][n])=5 then
						ADD_CODE({"mov.l r0,@-r10","mov.l "&literals[constants[2][n][5]][1]&",r1","mov.l @r1,r0"},0)
						constants[2][n][2] = 1	-- Mark the constant as being used
					elsif length(constants[2][n])=6 then
						--q = add_literal(tokens[p][2])
						ADD_CODE({"mov.l r0,@-r10","mov.l "&literals[q][1]&",r0"},0)
						constants[2][n][2] = 1	-- Mark the constant as being used
					end if
				end if
			end if

			-- Check deferred words		
			if not n then
				n = bsearch(tokens[p][2],deferred[1])
				if n then
					calls += 1
					ADD_CODE({"call "&deferred[2][n]&"; // deferred"},0)
				end if
			end if
			
			if not n then
				ERROR(ERROR_UNDECLARED,tokens[p])
			end if
		else
			ERROR("Unsupported word",tokens[p])
		end if

		p += 1
	end while
	
	

	if length(pendingWordDef) then
		ERROR(ERROR_OPEN_WORD,pendingWordDef)
	end if
	
	if length(loopStack) then
		ERROR(ERROR_OPEN_DO,tokens[p-1])
	end if
	
	return 0
end function





global procedure forthec_bfin()
	sequence dkarmdir,mcpu,gccend,march
	sequence asmOpts,linkOpts,crt0,lscript
	integer nofiles
	
	loops = 0
	calls = 0
	cases = 0
	ifs = 0
	strops = 0
	usesConsole = 0
	usesDiv = 0
	verbose = 0
	noMangle = 0
	noFold = 0
	globLit = 0
	optLevel = 5
	allowInline = 1
	dkarmdir = "c:\\bfin-elf-binutils"
	asmOpts = "" --"--register-prefix-optional --bitwise-or"	-- default
	linkOpts = "-T lnkscript"
	mcpu = "" 
	lscript = "lnkscript"
	march = ""
	crt0 = "crt0.o"
	--gccend = "-EL"
	entrypoint = "_start"
	fentry = "_main"
	nofpu = ""

	labelPrefix = "__"


	nofiles = 1
	if length(CMD)>=4 then
		if not (find("-t",CMD)>0 and length(CMD)=4) then
			nofiles = 0
		end if
	end if
	
	if nofiles then
		puts(1,"forthec -t bfin [options] <infile> <outfile>\n\n")
		puts(1,"Options:\n\n")
		puts(1,"-O<n>\t\tOptimisation level (n=0 min, n=6 max)\n")
		puts(1,"-ni\t\tDisable word inlining\n")
		puts(1,"-nm\t\tNo name mangling\n")
		--puts(1,"-cpu <name>\tSpecify target cpu (m68000)\n")
		--puts(1,"-arch <name>\tSpecify target architechture (armv4)\n")
		--puts(1,"-eb\t\tBig-endian\n")
		--puts(1,"-el\t\tLittle-endian (default)\n")
		--puts(1,"-nofpu\t\tDisable fpu code generation\n")
		--puts(1,"-entry <name>\tOverride default entrypoint symbol (_start)\n")
		puts(1,"-fentry <name>\tOverride default forth entrypoint symbol (_main)\n")
		puts(1,"-crt0 <name>\tOverride default crt0 filename (crt0.o)\n")
		puts(1,"-ls <name>\tOverride default linkscript filename (lnkscript)\n")
		puts(1,"-v\t\tVerbose compilation\n")
		puts(1,"\nPress any key to quit..")
		while get_key()=-1 do end while
		abort(0)
	end if

	-- Check options
	if find("-v",CMD) then
		verbose = 1
	end if

	if find("-ni",CMD) then
		allowInline = 0
	end if
	if find("-nm",CMD) then
		noMangle = 1
	end if
	if find("-nf",CMD) then
		noFold = 1
	end if

	if find("-eb",CMD) then
		gccend = "-EB"
	end if
	if find("-el",CMD) then
		gccend = "-EL"
	end if
	--if find("-nofpu",CMD) then
	--	nofpu = "-mno-fpu"
	--end if

	if find("-crt0",CMD) then
		crt0 = CMD[find("-crt0",CMD)+1]
	end if
	if find("-ls",CMD) then
		lscript = CMD[find("-ls",CMD)+1]
	end if
	--if find("-cpu",CMD) then
	--	mcpu = "-"&CMD[find("-cpu",CMD)+1]
	--end if
	--if find("-arch",CMD) then
	--	march = "-march="&CMD[find("-arch",CMD)+1]
	--	mcpu = ""
	--end if
	if find("-entry",CMD) then
		entrypoint = CMD[find("-entry",CMD)+1]
	end if
	if find("-fentry",CMD) then
		fentry = CMD[find("-fentry",CMD)+1]
	end if



	if find("-O0",CMD) then
		optLevel = 0
	elsif find("-O1",CMD) then
		optLevel = 1
	elsif find("-O2",CMD) then
		optLevel = 2
	elsif find("-O3",CMD) then
		optLevel = 3
	elsif find("-O4",CMD) then
		optLevel = 4
	elsif find("-O5",CMD) then
		optLevel = 5
	end if
	if optLevel then
		optLevel = 5
	end if

	infpos = 5
	while CMD[infpos][1]='-' do
		if equal(CMD[infpos],"-cpu") then
			infpos += 1
		elsif equal(CMD[infpos],"-arch") then
			infpos += 1
		elsif equal(CMD[infpos],"-ls") then
			infpos += 1
		elsif equal(CMD[infpos],"-crt0") then
			infpos += 1
		elsif equal(CMD[infpos],"-entry") then
			infpos += 1
		elsif equal(CMD[infpos],"-fentry") then
			infpos += 1
		end if
		infpos += 1
	end while
	morefiles = ""
	if length(CMD)>infpos+1 then
		for i=infpos+2 to length(CMD) do
			morefiles &= ' '&CMD[i]
		end for
	end if

	-- Read cfg
	cfgfile = open("forthec.cfg","r")
	if cfgfile != -1 then
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		if cfgin[1]=GET_SUCCESS then
			if equal(cfgin[2][1],"BFGCC") then
				dkarmdir = cfgin[2][2]
			end if
		end if
		--? cfgin
		close(cfgfile)
	end if


	outname = CMD[infpos+1]
	outname = cut_filename(outname)
	outname[2] = lower(outname[2])


	forthec_init()
	
	iwramStart = #03000000
	iwramEnd   = #03007FFF
	ewramStart = #02000000
	ewramEnd   = #0203FFFF
	

	t1 = time()
	if parse(CMD[infpos]) then end if

	if optLevel>0 and not noFold then
		optimise_token_stream()
	end if
	
	count_refs()
	
	if compile_bfin() then end if

	if errorCount then
		printf(1,"Aborting with %d errors encountered\n",errorCount)
		puts(1,"Press any key to abort..")
		while get_key()=-1 do end while
		abort(0)
	end if

	outfile = open(outname[1]&".asm","wb")
	
	if optLevel>0 then
		if verbose then
			puts(1,"Optimising\n")
		end if
		maincode = optimise_bfin(maincode,1)
		code = optimise_bfin(code,0)
	end if
	


	puts(outfile,"// Generated by ForthEC"&NL&NL)
	puts(outfile,NL&"//#############################################################################"&NL&NL)


	-- Check for unused constants and remove lines such as "mov const,imm"
	for j=1 to length(constlist) do
		for i=1 to length(constants[2]) do
			if equal(constlist[j][1][12..24],constants[2][i][1]) then
				if constlist[j][4]=0 then
					constants[2][i][2] = 0
					for k=1 to length(maincode) do
						if length(maincode[k])>29 then
					--		if equal("mov "&constlist[j][1],maincode[k][1..29]) then
					--			maincode[k] = {}
					--			linesRemoved += 1
					--		end if
						end if
					end for
				end if
				exit
			end if
		end for
	end for


	puts(outfile,".bss"&NL)
	puts(outfile,".align 2"&NL)
	for i=1 to length(constants[2]) do
		if constants[2][i][2] or optLevel=0 then
			puts(outfile,"\t"&constants[2][i][1]&":"&NL)
			puts(outfile,"\t\t.long 0"&NL)
		end if
	end for


	for i=1 to length(variables[2]) do
		--if variables[2][i][2] = 4 then
			--puts(outfile,"\t"&variables[2][i][1]&" dd ?"&NL)
			puts(outfile,"\t"&variables[2][i][1]&":"&NL)
			puts(outfile,"\t\t.long 0"&NL)
		--else
		--	puts(outfile,"\t"&variables[2][i][1]&" dq ?"&NL)
		--end if
	end for

	puts(outfile,NL&"//#############################################################################"&NL&NL)

	-- Write code section
	--puts(outfile,".code"&NL&"align 4"&NL&NL)
	puts(outfile,".text"&NL&".globl "&fentry&NL&".align 2"&NL)

	--if usesDiv then
	--	for i=1 to length(sdiv_code) do
	--		puts(outfile,sdiv_code[i]&NL)
	--	end for
	--	puts(outfile,NL&"//#############################################################################"&NL&NL)
	--end if
	
	for i=1 to length(code) do
		puts(outfile,code[i]&NL)
	end for

	puts(outfile,NL&"//#############################################################################"&NL&NL)

	puts(outfile,fentry&":"&NL)
	puts(outfile,	"p1 = sp;"&NL&		-- parameter stack
			"p5 = 0x400;"&NL&
			"p2 = p1; p2 -= p5;"&NL&	-- return stack
			"p3 = p2; p3 -= p5;"&NL&	-- loop stack
			sprintf("p0.h = %d;"&NL&"p0.l = %d;"&NL,
			        {and_bits(floor(ewramStart/#10000),#FFFF),
			         and_bits(ewramStart,#FFFF)}) ) -- "dictionary"
	

	for i=1 to length(maincode) do
		if length(maincode[i]) then
			puts(outfile,maincode[i]&NL)
		end if
	end for


	for i=1 to length(literals) do
		puts(outfile,".align 2"&NL&""&literals[i][1]&": "&literals[i][2]&NL)
	end for
	

	close(outfile)

	if verbose then
		printf(1,"Done\nRemoved %d unused constants and %d redundant instructions\nTotal compilation time: %1.2f seconds\n\n",{constRemoved,linesRemoved,time()-t1})
	end if


	if equal(outname[2],"bin") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system(dkarmdir&"\\bin\\bfin-elf-as "&mcpu&" "&asmOpts&" -o"&outname[1]&".o "&outname[1]&".asm",2)
		system(dkarmdir&"\\bin\\bfin-elf-ld -T "&lscript&" "&crt0&" "&outname[1]&".o"&morefiles,2)
		system(dkarmdir&"\\bin\\bfin-elf-objcopy -O binary a.out "&outname[1]&".bin",2)

		system("del "&outname[1]&".asm",2)
		system("del "&outname[1]&".o",2)
		system("del a.out",2)

	elsif equal(outname[2],"o") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system(dkarmdir&"\\bin\\bfin-elf-as "&mcpu&" "&asmOpts&" -o"&outname[1]&".o "&outname[1]&".asm",2)
	end if



	if verbose then
		puts(1,"\nPress any key..")
		while get_key() = -1 do end while
	end if
end procedure




