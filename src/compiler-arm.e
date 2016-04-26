-- ForthEC : A Forth compiler
-- /Mic, 2004/2008
--
-- ARM code generator
--


without warning
include forthec.e
include parser.e
include optimiser-arm.e


	
-- Register use:
----------------
-- r0	TOS
-- r1	Work register
-- r2	-"-
-- r3	Used for loop optimisations
-- r4	-"-
-- r5	-"-
-- r6	Loop counter
-- r7	Loop limit
-- r8	Case variable
-- r9	Dictionary pointer
-- r10	Parameter stack
-- r11	Return stack
-- r12	Loop stack
-- r13
-- r14
-- r15
	



-- Push a value on the parameter stack
procedure PUSH_arm(atom a)
	atom mask
	integer hi,lo,ones
	
	ADD_CODE({"str r0,[r10,#-4]!"},0)
	if a>=0 and a<256 then
		ADD_CODE({sprintf("mov r0,#%d",a)},0)
	elsif find(a,powersof2)>0 then
		ADD_CODE({sprintf("mov r0,#%d",a)},0)
	else
		mask = 1
		lo = -1
		hi = -1
		for i=0 to 31 do
			if and_bits(a,mask) then
				lo = i
				exit
			end if
			mask += mask
		end for
		if lo>=24 then
			ADD_CODE({sprintf("mov r0,#%d",a)},0)
		else
			mask += mask
			ones = 1
			for i=lo+1 to 31 do
				if and_bits(a,mask) then
					ones += 1
					hi = i
					if ones>8 or (hi-lo)>7 then
						exit
					end if
				end if
				mask += mask
			end for
			if ones<=8 and (hi-lo)<8 then
				ADD_CODE({sprintf("mov r0,#%d",a)},0)
			else
				ADD_CODE({sprintf("ldr r0,=%d",a)},0)
			end if
		end if
	end if
end procedure


procedure CMPI_arm(sequence cond,integer p)
	integer n
	
	n = 0
	if p<length(tokens) and optLevel>=4 then
		n = tokens[p+1][1]
		if n=W_IF or n=W_WHILE then
			fastCmp = "bn"&cond
			if equal(fastCmp,"bnne") then
				fastCmp = "beq"
			elsif equal(fastCmp,"bneq") then
				fastCmp = "bne"
			end if
			ADD_CODE({"ldr r1,[r10],#4","cmp r1,r0","ldr r0,[r10],#4"},0)
		elsif n=W_LEAVETRUE then
			fastCmp = "b"&cond
			ADD_CODE({"ldr r1,[r10],#4","cmp r1,r0","ldr r0,[r10],#4"},0)
		else
			n = 0
		end if
	end if
	if not n then
		ADD_CODE({"ldr r1,[r10],#4","mov r0,#0","cmp r0,r1","mov"&cond&" r0,#-1"},0)
	end if
end procedure



function compile_arm()
	integer n,p,q,lastComp,case_,hasDefault,touch,optI
	sequence s,t,token,wordId,loopStack,params,caseLbl,asm
	
	
	if verbose then
		puts(1,"Compiling\n")
	end if
	
	lastComp = 0
	case_ = -1
	loopStack = {}
	touch = 1
	fastCmp = ""
	optI = 0

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
				PUSH_arm(tokens[p][2])
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
			--ADD_CODE({"mov eax,[esp]","mov ecx,[esp+4]","mov [eax],ecx","add esp,8"},0)
			ADD_CODE({"ldr r1,[r10],#4","str r1,[r0]","ldr r0,[r10],#4"},0)

		-- c!
		elsif tokens[p][1] = W_CSTORE then
			ADD_CODE({"ldr r1,[r10],#4","strb r1,[r0]","ldr r0,[r10],#4"},0)
		-- w!
		elsif tokens[p][1] = W_WSTORE then
			ADD_CODE({"ldr r1,[r10],#4","strh r1,[r0]","ldr r0,[r10],#4"},0)

		-- !r+
		elsif tokens[p][1] = W_STORE_R_INC then
			ADD_CODE({"ldr r1,[r10],#4","str r0,[r1],#4","mov r0,r1"},0)

		-- ON
		--elsif tokens[p][1] = W_ON then
		--	ADD_CODE({"pop eax","mov dword ptr [eax],-1"},0)
		
		-- OFF
		--elsif tokens[p][1] = W_OFF then
		--	ADD_CODE({"pop eax","mov dword ptr [eax],0"},0)
			
		-- @
		elsif tokens[p][1] = W_FETCH then
			ADD_CODE({"ldr r0,[r0]"},0)
		-- c@
		elsif tokens[p][1] = W_CFETCH then
			ADD_CODE({"ldrb r0,[r0]"},0)
		-- cs@
		elsif tokens[p][1] = W_CSFETCH then
			ADD_CODE({"ldrsb r0,[r0]"},0)
		-- w@
		elsif tokens[p][1] = W_WFETCH then
			ADD_CODE({"ldrh r0,[r0]"},0)
		-- wl@
		elsif tokens[p][1] = W_WLOFETCH then
			ADD_CODE({"ldrh r0,[r0]","and r0,r0,#255"},0)
	
		-- a@
		--elsif tokens[p][1] = W_ACCFETCH then
		--	ADD_CODE({"push eax"},0)
			

		-- +
		elsif tokens[p][1] = W_ADD then
			ADD_CODE({"ldr r1,[r10],#4","add r0,r0,r1"},0)

		-- -
		elsif tokens[p][1] = W_SUB then
			ADD_CODE({"ldr r1,[r10],#4","rsb r0,r0,r1"},0)

		-- *
		elsif tokens[p][1] = W_MUL then
			ADD_CODE({"ldr r1,[r10],#4","mul r0,r1,r0"},0)
		-- /
		--elsif tokens[p][1] = W_DIV then
			--ADD_CODE({"pop ebx","pop eax","cdq","idiv ebx","push eax"},0)

		-- MOD
		--elsif tokens[p][1] = W_MOD then
			--ADD_CODE({"pop ebx","pop eax","cdq","idiv ebx","push edx"},0)

		-- /MOD
		--elsif tokens[p][1] = W_DIVMOD then
			--ADD_CODE({"pop ebx","pop eax","cdq","idiv ebx","push edx","push eax"},0)

		
		-- /C
		elsif tokens[p][1] = W_CSIZE then
			PUSH_arm(1)

		-- /F
		elsif tokens[p][1] = W_FSIZE then
			PUSH_arm(8)

		-- /N
		elsif tokens[p][1] = W_NSIZE then
			PUSH_arm(4)
			
		-- 1-
		elsif tokens[p][1] = W_DEC then
			ADD_CODE({"sub r0,r0,#1"},0)
		-- 1+
		elsif tokens[p][1] = W_INC then
			ADD_CODE({"add r0,r0,#1"},0)
			
		-- 2*
		elsif tokens[p][1] = W_MUL2 then
			ADD_CODE({"add r0,r0,r0"},0)

		-- 2/
		elsif tokens[p][1] = W_DIV2 then
			ADD_CODE({"mov r0,r0,asr#1"},0)
		
		-- <<
		elsif tokens[p][1] = W_SHL then
			ADD_CODE({"ldr r1,[r10],#4","mov r0,r1,lsl r0"},0)
		-- >>
		elsif tokens[p][1] = W_SHR then
			ADD_CODE({"ldr r1,[r10],#4","mov r0,r1,lsr r0"},0)
		
		-- BOUNDS
		--elsif tokens[p][1] = W_BOUNDS then
			--ADD_CODE({"pop ecx","pop eax","lea ecx,[eax+ecx-1]","push ecx","push eax"},0)
		
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
							pendingWordDef = tokens[p+1]
							params = {{},{}}
							ADD_CODE({NL&"@ "&tokens[p+1][2]},0)
							wordId = make_label(tokens[p+1][2]) 
							ADD_CODE({wordId&":"},0)
							--ADD_CODE({"sub ipstackptr,4","pop ecx","mov eax,ipstackptr","mov [eax],ecx"},0)
							ADD_CODE({"str r14,[r11,#-4]!"},0)
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
					ADD_CODE({"sub ipstackptr,4","mov eax,ipstackptr","mov [eax],ebp","mov ebp,esp"},0)
				elsif length(params[1]) then
					ADD_CODE({"mov ebp,esp"},0)
				end if
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if
		
		-- ;
		elsif tokens[p][1] = W_SEMICOLON then
			if length(pendingWordDef)=3 then
				--ADD_CODE({wordId&"_return:","sub ipstackptr,2","mov eax,ipstackptr","mov ecx,[ipstack+eax*4]","inc eax","mov ebp,[ipstack+eax*4]","push ecx","ret"},0)			
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
					ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","push ecx","ret"},0)
				else
					ADD_CODE({"ldr r15,[r11],#4"},0)
				end if
				ADD_CODE({".pool"},0)
				userWords = assoc_insert(pendingWordDef[2],wordId,userWords)
				pendingWordDef = {}
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if

		-- ;@
		elsif tokens[p][1] = W_SEMICOLONPOP then
			if length(pendingWordDef)=3 then
				--ADD_CODE({wordId&"_return:","sub ipstackptr,2","mov eax,ipstackptr","mov ecx,[ipstack+eax*4]","inc eax","mov ebp,[ipstack+eax*4]","pop eax","push ecx","ret"},0)			
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
					ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","pop eax","push ecx","ret"},0)
				else
					ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax]","add ipstackptr,4","pop eax","push ecx","ret"},0)
				end if
				userWords = assoc_insert(pendingWordDef[2],wordId,userWords)
				pendingWordDef = {}
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if

			
		-- '
		elsif tokens[p][1] = W_QUOTE then
			n = bsearch(tokens[p+1][2],userWords[1])
			if n then
				--ADD_CODE({"mov eax,offset "&userWords[2][n],"push eax"},0)
				p += 1
			end if
			
		-- =
		elsif tokens[p][1] = W_EQ then
			CMPI_arm("eq",p)
			
		-- <>
		elsif tokens[p][1] = W_NE then
			CMPI_arm("ne",p)
		
		-- >
		elsif tokens[p][1] = W_GT then
			CMPI_arm("gt",p)
		-- >=
		elsif tokens[p][1] = W_GE then
			CMPI_arm("ge",p)

		-- <
		elsif tokens[p][1] = W_LT then
			CMPI_arm("lt",p)
			--ADD_CODE({"pop eax","xor ecx,ecx","cmp [esp],eax","setl cl","neg ecx","mov [esp],ecx"},0)
		-- <=
		elsif tokens[p][1] = W_LE then
			CMPI_arm("le",p)

		-- 0=
		elsif tokens[p][1] = W_EQ0 then
			ADD_CODE({"cmp r0,#0","moveq r0,#-1","movne r0,#0"},0)
		-- 0<>
		elsif tokens[p][1] = W_NE0 then
			ADD_CODE({"cmp r0,#0","moveq r0,#0","movne r0,#-1"},0)
		-- 0>
		--elsif tokens[p][1] = W_GT0 then
		--	ADD_CODE({"pop eax","xor ecx,ecx","cmp eax,0","setg cl","neg ecx","mov [esp],ecx"},0)
		
		-- 0<
		--elsif tokens[p][1] = W_LT0 then
		--	ADD_CODE({"pop eax","xor ecx,ecx","cmp eax,0","setl cl","neg ecx","mov [esp],ecx"},0)


		-- NEGATE
		elsif tokens[p][1] = W_NEG then
			ADD_CODE({"rsb r0,r0,#0"},0)
			

		-- ABS
		--elsif tokens[p][1] = W_ABS then

		-- ALLOT
		elsif tokens[p][1] = W_ALLOT then
			ADD_CODE({"add r9,r9,r0","ldr r0,[r10],#4"},0)
			
		-- AND
		elsif tokens[p][1] = W_AND then
			ADD_CODE({"ldr r1,[r10],#4","and r0,r0,r1"},0)

		-- BEGIN
		elsif tokens[p][1] = W_BEGIN then
			s = sprintf("__loop_%04x",loops)
			loops += 1
			loopStack = append(loopStack,s)
			ADD_CODE({s&":"},0)
	
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
					while tokens[q][1]!=W_LOOP and tokens[q][1]!=W_INCLOOP do
						if tokens[q][1]=UNKNOWN then
							if bsearch(tokens[q][2],userWords[1]) then
								n = 0
								exit
							elsif bsearch(tokens[q][2],deferred[1]) then
								n = 0
								exit
							end if
						elsif tokens[q][1]=W_DOT or tokens[q][1]=W_DOTSTRING or
						      tokens[q][1]=W_EXPECT or tokens[q][1]=W_KEY then
						      	n = 0
						      	exit
						end if
						q += 1
					end while
				end if
			end if
			
			--if n then
			--	optI = 1
			--	ADD_CODE({"pop esi","add loopstackptr,8","pop edi"},0)
			--else
				----ADD_CODE({"mov eax,loopstackptr","pop ecx","pop edx","add loopstackptr,8","mov [eax],edx","mov [eax+4],ecx"},0)
				--ADD_CODE({"ldr r1,[r10],#4","str r1,[r12,#-4]!","str r0,[r12,#-4]!","ldr r0,[r10],#4"},0)
			ADD_CODE({"str r7,[r12,#-4]!","str r6,[r12,#-4]!","ldr r7,[r10],#4","mov r6,r0","ldr r0,[r10],#4"},0)
			--end if
			ADD_CODE({s&":"},0)

		-- LOOP
		elsif tokens[p][1] = W_LOOP then
			if optI then
				ADD_CODE({"inc esi","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				ADD_CODE({"add r6,r6,#1","cmp r6,r7","bne "&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			ADD_CODE({"ldr r6,[r12],#4","ldr r7,[r12],#4"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if

		-- +LOOP
		elsif tokens[p][1] = W_INCLOOP then
			if optI then
				ADD_CODE({"pop eax","add esi,eax","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				ADD_CODE({"add r6,r6,r0","ldr r0,[r10],#4","cmp r6,r7","bne "&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			ADD_CODE({"ldr r6,[r12],#4","ldr r7,[r12],#4"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if

		-- I
		elsif tokens[p][1] = W_I then
			if optI then
				ADD_CODE({"push esi"},0)
			else
				--ADD_CODE({"mov eax,loopstackptr","push dword ptr [eax-4]"},0)
				ADD_CODE({"str r0,[r10,#-4]!","mov r0,r6"},0)
			end if
		-- J
		elsif tokens[p][1] = W_J then
			ADD_CODE({"str r0,[r10,#-4]!","ldr r0,[r12]"},0)
			
		-- CASE
		elsif tokens[p][1] = W_CASE then
			if case_ = -1 then
				case_ = 0
				hasDefault = 0
				caseLbl = sprintf("__case_%06x",cases)
				ADD_CODE({"ldr r8,[r10],#4"},0)
			else
				ERROR("CASE inside CASE",tokens[p])
			end if
		
		-- OF
		elsif tokens[p][1] = W_OF then
			if case_>=0 and not hasDefault then
				ADD_CODE({"ldr r0,[r10],#4","cmp r0,r8","bne "&caseLbl&sprintf("_%04x_false",case_)},0)
			elsif hasDefault then
				ERROR("OF found after DEFAULT",tokens[p])
			else
				ERROR("OF outside CASE",tokens[p])
			end if
		
		-- ENDOF
		elsif tokens[p][1] = W_ENDOF then
			if case_>=0 then
				ADD_CODE({"b "&caseLbl&"_end"},0)
				ADD_CODE({caseLbl&sprintf("_%04x_false:",case_)},0)
				case_ += 1
			else
				ERROR("ENDOF outside CASE",tokens[p])
			end if
		
		-- ENDCASE
		elsif tokens[p][1] = W_ENDCASE then
			if case_>=0 then
				ADD_CODE({caseLbl&"_end:"},0)
				case_ = -1
				cases += 1
			else
				ERROR("Unmatched ENDCASE",tokens[p])
			end if
		
		-- DEFAULT
		elsif tokens[p][1] = W_DEFAULT then
			if case_>=0 then
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
							--if tokens[p-1][1]=NUMBER then
							--	constants = assoc_insert(tokens[p+1][2],{s,0,n+1,tokens[p-1][2]},constants)
							--else
							constants = assoc_insert(tokens[p+2][2],{s,0,n+1},constants)
							ADD_CODE({"ldr r2,="&tokens[p+2][2],"ldr r1,="&s,"str r2,[r1]"},0)
							--end if
							--ADD_CODE({"pop eax","mov dword ptr ["&s&"],eax"},0)
							p += 2
						else
							ERROR(ERROR_REDECLARED,tokens[p+2])
							p += 2
						end if
					else
						ERROR(ERROR_CONST_INSIDE_WORD,tokens[p])				
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
							constants = assoc_insert(tokens[p+1][2],{s,0,n+1},constants)
							ADD_CODE({"ldr r1,="&s,"str r0,[r1]","ldr r0,[r10],#4"},0)
						end if
						--ADD_CODE({"pop eax","mov dword ptr ["&s&"],eax"},0)
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
				if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],variables[1])=0 then
					s = sprintf("var_%07x",length(variables[1]))
					variables = assoc_insert(tokens[p+1][2],{s,4},variables)
					ADD_CODE({"ldr r1,="&s,"str r9,[r1]","add r9,r9,#4"},0)
					p += 1
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
					p += 1
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

			
		-- BYE
		elsif tokens[p][1] = W_BYE then
			--ADD_CODE({"invoke ExitProcess,0"},0)
		

		-- HERE
		--elsif tokens[p][1] = W_HERE then
		--	ADD_CODE({"push dword ptr [dictptr]"},0)
		
		-- ,
		--elsif tokens[p][1] = W_COMMA then
		--	ADD_CODE({"mov ebx,dictptr","pop eax","add dictptr,4","mov [ebx],eax"},0)
		
		-- c,
		--elsif tokens[p][1] = W_CCOMMA then
		--	ADD_CODE({"mov ebx,dictptr","pop eax","inc dword ptr dictptr","mov [ebx],al"},0)
			
		
		-- CALL
		--elsif tokens[p][1] = W_CALL then
		--	ADD_CODE({"call "&tokens[p+1][2]},0)
		--	p += 1
			
		-- DROP
		elsif tokens[p][1] = W_DROP then
			ADD_CODE({"ldr r0,[r10],#4"},0)

		-- 2DROP
		elsif tokens[p][1] = W_2DROP then
			ADD_CODE({"add r10,r10,#4","ldr r0,[r10],#4"},0)
			
		-- DUP
		elsif tokens[p][1] = W_DUP then
			ADD_CODE({"str r0,[r10,#-4]!"},0)
		-- 2DUP
		elsif tokens[p][1] = W_2DUP then
			ADD_CODE({"mov r2,r0","ldr r1,[r10]","stmfd r10!,{r1,r2}"},0)

		-- PICK
		elsif tokens[p][1] = W_PICK then
			--ADD_CODE({"pop eax","push dword ptr [esp+eax*4]"},0)
			ADD_CODE({"ldr r0,[r10,r0,lsl#2]"},0)
			
		-- NIP
		elsif tokens[p][1] = W_NIP then
			--ADD_CODE({"pop eax","pop ebx","push eax"},0)
			ADD_CODE({"add r10,r10,#4"},0)
			
		-- TUCK
		--elsif tokens[p][1] = W_TUCK then
		--	ADD_CODE({"pop eax","pop ebx","push eax","push ebx","push eax"},0)
		

		-- OVER
		elsif tokens[p][1] = W_OVER then
			ADD_CODE({"ldr r1,[r10]","str r0,[r10,#-4]!","mov r0,r1"},0)
			
		-- ELSE
		elsif tokens[p][1] = W_ELSE then
			if length(ifStack) and and_bits(length(ifStack),1)=0 then
				ADD_CODE({"b "&ifStack[2]},0)
				ADD_CODE({ifStack[1]&":"},0)
				ifStack = ifStack[2..length(ifStack)]
			else
				ERROR("Unmatched ELSE",tokens[p])
			end if
		
		-- EXIT
		elsif tokens[p][1] = W_EXIT then
			if length(pendingWordDef) then
				ADD_CODE({"b "&wordId&"_return"},0)
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
			ADD_CODE({"ldr r1,[r10],#4","ldr r2,[r10],#4",s&':',"strb r0,[r2],#1","subs r1,r1,#1","bne "&s,"ldr r0,[r10],#4"},0)
			strops += 1			

		-- IF
		elsif tokens[p][1] = W_IF then
			s = sprintf("__if_%06x",ifs)
			ifStack = {s&"_false",s&"_true"}&ifStack
			ifs += 1
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&ifStack[1]},0)
				fastCmp = ""
			else
				ADD_CODE({"cmp r0,#-1","ldr r0,[r10],#4","bne "&ifStack[1]},0)
			end if
			
		-- LEAVE
		elsif tokens[p][1] = W_LEAVE then
			ADD_CODE({"b "&loopStack[length(loopStack)]&"_end"},0)
		-- ?LEAVE
		elsif tokens[p][1] = W_LEAVETRUE then
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&loopStack[length(loopStack)]&"_end"},0)
				fastCmp = ""
			else
				ADD_CODE({"ldr r1,[r10],#4","cmp r1,#-1","beq "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- NULL
		elsif tokens[p][1] = W_NULL or tokens[p][1]=W_FALSE then
			PUSH_arm(0)
		-- TRUE
		elsif tokens[p][1] = W_TRUE then
			PUSH_arm(-1)
		-- BL
		elsif tokens[p][1] = W_BL then
			PUSH_arm(' ')
		
		-- OR
		elsif tokens[p][1] = W_OR then
			ADD_CODE({"ldr r1,[r10],#4","orr r0,r0,r1"},0)

		-- REPEAT
		elsif tokens[p][1] = W_REPEAT then
			if length(loopStack) then
				ADD_CODE({"b "&loopStack[length(loopStack)]},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				if length(loopStack) = 1 then
					loopStack = {}
				else
					loopStack = loopStack[1..length(loopStack)-1]
				end if
			else
				ERROR(ERROR_NO_BEGIN,tokens[p])
			end if
		-- ROT
		--elsif tokens[p][1] = W_ROT then
		--	ADD_CODE({"pop eax","pop ecx","pop edx","push ecx","push edx","push eax"},0)

		-- SWAP
		elsif tokens[p][1] = W_SWAP then
			ADD_CODE({"ldr r1,[r10]","str r0,[r10]","mov r0,r1"},0)
			
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
			
		-- UNTIL
		elsif tokens[p][1] = W_UNTIL then
			if not length(loopStack) then
				ERROR("Unmatched UNTIL",tokens[p])
			else
				ADD_CODE({"cmp r0,#-1","ldr r0,[r10],#4","bne "&loopStack[length(loopStack)]},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				if length(loopStack) = 1 then
					loopStack = {}
				else
					loopStack = loopStack[1..length(loopStack)-1]
				end if
			end if
			
		-- WHILE
		elsif tokens[p][1] = W_WHILE then
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&loopStack[length(loopStack)]&"_end"},0)
				fastCmp = ""
			else
				ADD_CODE({"cmp r0,#-1","ldr r0,[r10],#4","bne "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- XOR
		elsif tokens[p][1] = W_XOR then
			ADD_CODE({"ldr r1,[r10],#4","eor r0,r0,r1"},0)
		
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
				--literals = append(literals,{sprintf("lit_%06x",length(literals)),"db \""&s&"\",0"})
				--ADD_CODE({"push offset "&literals[length(literals)][1]},0)
				literals = append(literals,{sprintf("lit_%06x",length(literals)),".asciz \""&s&"\""})
				ADD_CODE({"str r0,[r10,#-4]!","ldr r0,="&literals[length(literals)][1]},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		elsif tokens[p][1] = UNKNOWN then
			n = bsearch(tokens[p][2],userWords[1])
			if n then
				ADD_CODE({"bl "&userWords[2][n]},0)
			end if
			
			if not n then
				if length(pendingWordDef) then
					--t = tokens[p][2]
					n = bsearch(tokens[p][2],params[1])
					if n then
						ADD_CODE({sprintf("push dword ptr [ebp+%d]",params[2][n][1])},0)
					end if
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],variables[1])
				if n then
					--ADD_CODE({"push dword ptr ["&variables[2][n][1]&"]"},0)
					ADD_CODE({"str r0,[r10,#-4]!","ldr r0,="&variables[2][n][1]},0)
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],constants[1])
				if n then
					--ADD_CODE({"push dword ptr ["&constants[2][n][1]&"]"},0)
					if length(constants[2][n])=3 then
						ADD_CODE({"str r0,[r10,#-4]!","ldr r1,="&constants[2][n][1],"ldr r0,[r1]"},0)
						constants[2][n][2] = 1	-- Mark the constant as being used
					else
						--ADD_CODE({"str r0,[r10,#-4]!","ldr r0,="&sprintf("%d",constants[2][n][4])},0)
						PUSH_arm(constants[2][n][4])
					end if
				end if
			end if

			--if not n then
			--	n = bsearch(tokens[p][2],fconstants[1])
			--	if n then
			--		ADD_CODE({"fld qword ptr ["&fconstants[2][n]&"]"},0)
			--		ADD_CODE({"mov eax,fpstackptr","fstp qword ptr [eax]","add fpstackptr,8"},0)
			--	end if
			--end if
		
			if not n then
				n = bsearch(tokens[p][2],deferred[1])
				if n then
					ADD_CODE({"bl "&deferred[2][n]&" @ deferred"},0)
					--printf(1,tokens[p][2]&" on line %d\n",tokens[p][3])
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





global procedure forthec_arm()
	sequence dkarmdir,mcpu,gccend,march
	sequence asmOpts,linkOpts,crt0,lscript
	integer nofiles
	
	loops = 0
	cases = 0
	ifs = 0
	strops = 0
	usesConsole = 0
	verbose = 0
	noMangle = 0
	noFold = 0
	optLevel = 5
	dkarmdir = "c:\\devkitarm"
	asmOpts = "-mcpu=arm7tdmi -EL"	-- default
	linkOpts = "-T lnkscript -e _start -EL"
	mcpu = "-mcpu=arm7tdmi"
	lscript = "lnkscript"
	march = ""
	crt0 = "crt0.o"
	gccend = "-EL"
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
		puts(1,"forthec -t arm [options] <infile> <outfile>\n\n")
		puts(1,"Options:\n\n")
		puts(1,"-O<n>\t\tOptimisation level (n=0 min, n=6 max)\n")
		puts(1,"-nf\t\tNo constant expression folding\n")
		puts(1,"-nm\t\tNo name mangling\n")
		puts(1,"-cpu <name>\tSpecify target cpu (arm7tdmi)\n")
		puts(1,"-arch <name>\tSpecify target architechture (armv4)\n")
		puts(1,"-eb\t\tBig-endian\n")
		puts(1,"-el\t\tLittle-endian (default)\n")
		puts(1,"-nofpu\t\tDisable fpu code generation\n")
		puts(1,"-entry <name>\tOverride default entrypoint symbol (_start)\n")
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
	if find("-nofpu",CMD) then
		nofpu = "-mno-fpu"
	end if

	if find("-crt0",CMD) then
		crt0 = CMD[find("-crt0",CMD)+1]
	end if
	if find("-ls",CMD) then
		lscript = CMD[find("-ls",CMD)+1]
	end if
	if find("-cpu",CMD) then
		mcpu = "-mcpu="&CMD[find("-cpu",CMD)+1]
	end if
	if find("-arch",CMD) then
		march = "-march="&CMD[find("-arch",CMD)+1]
		mcpu = ""
	end if
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
		if cfgin[1]=GET_SUCCESS then
			if equal(cfgin[2][1],"ARMGCC") then
				dkarmdir = cfgin[2][2]
			end if
		end if
		close(cfgfile)
	end if


	outname = CMD[infpos+1]
	outname = cut_filename(outname)
	outname[2] = lower(outname[2])

	outfile = open(outname[1]&".asm","wb")


	code = {}
	ifStack = {}
	maincode = {}
	deferred = {{},{}}
	literals = {}
--	includes = {"windows.inc","kernel32.inc","user32.inc","kernel32.lib","user32.lib"}
	pendingWordDef = {}
	tokens = {}
	userWords = {{},{}}
	constants = {{},{}}
	fconstants = {{},{}}
	variables = {{},{}}

	forthec_init()

	t1 = time()
	if parse(CMD[infpos]) then end if

	if optLevel>0 and not noFold then
		optimise_token_stream()
	end if

	count_refs()
	
	if compile_arm() then end if

	if errorCount then
		printf(1,"Aborting with %d errors encountered\n",errorCount)
		puts(1,"Press any key to abort..")
		while get_key()=-1 do end while
		abort(0)
	end if
	
	if optLevel>0 then
		if verbose then
			puts(1,"Optimising\n")
		end if
		maincode = optimise_arm(maincode,1)
		code = optimise_arm(code,0)
	end if


	puts(outfile,"@ Generated by ForthEC"&NL&NL)
	puts(outfile,NL&";##############################################################################"&NL&NL)


	-- Check for unused constants and remove lines such as "mov const,imm"
	for j=1 to length(constlist) do
		for i=1 to length(constants[2]) do
			if equal(constlist[j][1][12..24],constants[2][i][1]) then
				if constlist[j][4]=0 then
					constants[2][i][2] = 0
					for k=1 to length(maincode) do
						if length(maincode[k])>29 then
							if equal("mov "&constlist[j][1],maincode[k][1..29]) then
								maincode[k] = {}
								linesRemoved += 1
							end if
						end if
					end for
				end if
				exit
			end if
		end for
	end for

	puts(outfile,".data"&NL)

	for i=1 to length(literals) do
		puts(outfile,".align 2"&NL&"\t"&literals[i][1]&": "&literals[i][2]&NL)
	end for
	puts(outfile,".align 2"&NL)

	for i=1 to length(constants[2]) do
		if constants[2][i][2] or optLevel=0 then
			----puts(outfile,"\t"&constants[2][i][1]&" dd ?"&NL)
			--puts(outfile,"\t.global "&constants[2][i][1]&NL)
			--puts(outfile,"\t.size "&constants[2][i][1]&",4"&NL)
			puts(outfile,"\t"&constants[2][i][1]&":"&NL)
			puts(outfile,"\t\t.word 0"&NL)
		end if
	end for

	--for i=1 to length(fconstants[2]) do
		--puts(outfile,"\t"&fconstants[2][i]&" dq ?"&NL)
	--end for

	for i=1 to length(variables[2]) do
		--if variables[2][i][2] = 4 then
			--puts(outfile,"\t"&variables[2][i][1]&" dd ?"&NL)
			puts(outfile,"\t"&variables[2][i][1]&":"&NL)
			puts(outfile,"\t\t.word 0"&NL)
		--else
		--	puts(outfile,"\t"&variables[2][i][1]&" dq ?"&NL)
		--end if
	end for

	puts(outfile,NL&";##############################################################################"&NL&NL)

	-- Write code section
	--puts(outfile,".code"&NL&"align 4"&NL&NL)
	puts(outfile,".text"&NL&".global "&fentry&NL&".align 2"&NL)

	for i=1 to length(code) do
		puts(outfile,code[i]&NL)
	end for

	puts(outfile,NL&";##############################################################################"&NL&NL)

	puts(outfile,fentry&":"&NL)
	puts(outfile,	"mov r10,r13"&NL&		-- parameter stack
			"sub r11,r10,#0x400"&NL&	-- return stack
			"sub r12,r11,#0x400"&NL&	-- loop stack
			"ldr r9,=0x2000000"&NL)		-- "dictionary"


	for i=1 to length(maincode) do
		if length(maincode[i]) then
			puts(outfile,maincode[i]&NL)
		end if
	end for

	puts(outfile,NL&".pool"&NL&".end "&fentry&NL)

	close(outfile)

	if verbose then
		printf(1,"Done\nRemoved %d unused constants and %d redundant instructions\nTotal compilation time: %1.2f seconds\n\n",{constRemoved,linesRemoved,time()-t1})
	end if


	if equal(outname[2],"bin") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system(dkarmdir&"\\bin\\arm-elf-as "&mcpu&" "&march&" "&gccend&" "&nofpu&" -o"&outname[1]&".o "&outname[1]&".asm",2)
		system(dkarmdir&"\\bin\\arm-elf-ld -T "&lscript&" "&gccend&" -e "&entrypoint&" "&crt0&" "&outname[1]&".o"&morefiles,2)
		system(dkarmdir&"\\bin\\arm-elf-objcopy -O binary a.out "&outname[1]&".bin",2)

		system("del "&outname[1]&".asm",2)
		system("del "&outname[1]&".o",2)
		system("del a.out",2)

	elsif equal(outname[2],"o") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system(dkarmdir&"\\bin\\arm-elf-as "&mcpu&" "&march&" "&gccend&" "&nofpu&" -o"&outname[1]&".o "&outname[1]&".asm",2)
	end if



	if verbose then
		puts(1,"\nPress any key..")
		while get_key() = -1 do end while
	end if
end procedure




