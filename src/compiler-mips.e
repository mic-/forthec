-- ForthEC : A Forth compiler
-- /Mic, 2004/2005
--
-- MIPS code generator
--

without warning
include forthec.e
include parser.e
include optimiser-sh.e

	
-- Register use:
----------------
-- r1  Assembler temporary
-- r8  Work register
-- r9  -"-
-- r10 
-- r11
-- r12
-- r13 Loop counter
-- r14 Loop limit
-- r15 Dictionary pointer
-- r16 Parameter stack
-- r17 Return stack
-- r18 Loop stack
-- r19 Named parameter base
-- r20 Case variable
-- r21 Work register
-- r22 -"-
-- r23 -"-
-- r25 TOS
-- r31 Link register
-- r29 Regular stack





procedure PUSH_mips(atom a)
	atom mask
	integer hi,lo,ones,p
	
	ADD_CODE({"addi $16,$16,-4","sw $25,($16)"},0)
	if a>=0 and a<#10000 then
		ADD_CODE({sprintf("ori $25,$0,%d",a)},0)
	else
		ADD_CODE({sprintf("li $25,%d",a)},0)
	end if
end procedure



procedure CMPI_mips(sequence cond,integer p)
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
			elsif equal(fastCmp,"bnlt") then
				fastCmp = "bge"
			elsif equal(fastCmp,"bnle") then
				fastCmp = "bgt"
			elsif equal(fastCmp,"bngt") then
				fastCmp = "ble"
			elsif equal(fastCmp,"bnge") then
				fastCmp = "blt"
			end if
			ADD_CODE({"move.l d0,d1","move.l (a2)+,d2","move.l (a2)+,d0","cmp.l d1,d2"},0)
		elsif n=W_LEAVETRUE then
			fastCmp = "b"&cond
			ADD_CODE({"move.l d0,d1","move.l (a2)+,d2","move.l (a2)+,d0","cmp.l d1,d2"},0)
		else
			n = 0
		end if
	end if
	if not n then
		ADD_CODE({"move.l (a2)+,d1","cmp.l d1,d0","s"&cond&" d0","ext.w d0","ext.l d0"},0)
	end if
end procedure



function compile_mips()
	integer n,p,q,r,lastComp,case_,hasDefault,touch,optI
	sequence s,t,token,wordId,loopStack,params,caseLbl,asm,context
	
	
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
				PUSH_mips(tokens[p][2])
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
					exit
				elsif tokens[p][1] = NEWLINE then
					if length(asm) then
						if length(pendingWordDef) then
							code = append(code,asm)
						else
							maincode = append(maincode,asm)
						end if
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
			ADD_CODE({"mov.l @r10+,r1","mov.l r1,@r0","mov.l @r10+,r0"},0)

		-- c!
		elsif tokens[p][1] = W_CSTORE then
			ADD_CODE({"mov.l @r10+,r1","mov.b r1,@r0","mov.l @r10+,r0"},0)

		-- w!
		elsif tokens[p][1] = W_WSTORE then
			ADD_CODE({"mov.l @r10+,r1","mov.w r1,@r0","mov.l @r10+,r0"},0)

		-- !r+
		elsif tokens[p][1] = W_STORE_R_INC then
--			ADD_CODE({"ldr r1,[r10],#4","str r0,[r1],#4","mov r0,r1"},0)

		-- ON
		--elsif tokens[p][1] = W_ON then
		--	ADD_CODE({"pop eax","mov dword ptr [eax],-1"},0)
		
		-- OFF
		--elsif tokens[p][1] = W_OFF then
		--	ADD_CODE({"pop eax","mov dword ptr [eax],0"},0)
			
		-- @
		elsif tokens[p][1] = W_FETCH then
			ADD_CODE({"mov.l @r0,r0"},0)
		-- c@
		elsif tokens[p][1] = W_CFETCH then
			ADD_CODE({"mov.b @r0,r0","extu.b"},0)
		-- cs@
		elsif tokens[p][1] = W_CSFETCH then
			ADD_CODE({"mov.b @r0,r0"},0)
		-- w@
		elsif tokens[p][1] = W_WFETCH then
			ADD_CODE({"mov.w @r0,r0","extu.w r0,r0"},0)
	
		-- a@
		--elsif tokens[p][1] = W_ACCFETCH then
		--	ADD_CODE({"push eax"},0)
			

		-- +
		elsif tokens[p][1] = W_ADD then
			ADD_CODE({"lw $8,($16)","addiu $16,$16,4","add $25,$8,$25"},0)

		-- -
		elsif tokens[p][1] = W_SUB then
			ADD_CODE({"lw $8,($16)","addiu $16,$16,4","sub $25,$8,$25"},0)

		-- *
		elsif tokens[p][1] = W_MUL then
			--ADD_CODE({"move.l (a2)+,d1","muls d1,d0"},0)
			
		-- /
		elsif tokens[p][1] = W_DIV then
			--ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","divs d1,d0","ext.l d0"},0)
		-- MOD
		elsif tokens[p][1] = W_MOD then
			--ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","divs d1,d0","swap d0","ext.l d0"},0)
		

		-- /MOD
		elsif tokens[p][1] = W_DIVMOD then
			--ADD_CODE({"move.l d0,d1","move.l (a2),d0","divs d1,d0","move.l d0,d1","ext.l d0","swap d1","ext.l d1","move.l d1,(a2)"},0)

		
		-- /C
		elsif tokens[p][1] = W_CSIZE then
			PUSH_mips(1)

		-- /F
		elsif tokens[p][1] = W_FSIZE then
			PUSH_mips(8)

		-- /N
		elsif tokens[p][1] = W_NSIZE then
			PUSH_mips(4)
			
		-- 1-
		elsif tokens[p][1] = W_DEC then
			ADD_CODE({"addi $25,$25,-1"},0)
		-- 1+
		elsif tokens[p][1] = W_INC then
			ADD_CODE({"addiu $25,$25,1"},0)
			
		-- 2*
		elsif tokens[p][1] = W_MUL2 then
			ADD_CODE({"add $25,$25,$25"},0)

		-- 2/
		elsif tokens[p][1] = W_DIV2 then
			ADD_CODE({"sra $25,$25,1"},0)
		
		-- <<
		elsif tokens[p][1] = W_SHL then
			ADD_CODE({"lw $8,($16)","addiu $16,$16,4","sllv $25,$8,$25"},0)

		-- >>
		elsif tokens[p][1] = W_SHR then
			ADD_CODE({"lw $8,($16)","addiu $16,$16,4","srlv $25,$8,$25"},0)
		
		-- BOUNDS
		elsif tokens[p][1] = W_BOUNDS then
			--ADD_CODE({"pop ecx","pop eax","lea ecx,[eax+ecx-1]","push ecx","push eax"},0)
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
					n = lookup_symbol(tokens[p+1][token_VALUE])
					if symtab[lex_DATA][n][symbol_TAG] = SYM_UNDEF then
						symtab[lex_DATA][n][symbol_TAG] = SYM_WORD

						if not length(pendingWordDef) then
							pendingWordDef = tokens[p+1]
							params = {{},{}}
							ADD_CODE({NL&"# "&tokens[p+1][2]},0)
							wordId = make_label(tokens[p+1][2]) 
							ADD_CODE({wordId&":"},0)
							ADD_CODE({"addi $17,$17,-4","sw $31,($17)"},0)
							context = {ifStack,loopStack}
							ifStack = {}
							loopStack = {}
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
						ERROR("Unexpected end of file",tokens[p-1])
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
					ADD_CODE({"addi $17,$17,-4","sw $19,($17)","or $19,$0,$16"},0)
				elsif length(params[1]) then
					ADD_CODE({"or $19,$0,$16"},0)
				end if
			else
				ERROR("Use of { outside word definition",tokens[p])
			end if
		
		-- ;
		elsif tokens[p][1] = W_SEMICOLON then
			if length(pendingWordDef)=3 then
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
					--ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","push ecx","ret"},0)
					ADD_CODE({"lw $19,($17)","lw $31,4($17)","addiu $17,$17,8","jr $31","nop"},0)
				else
					--ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax]","add ipstackptr,4","push ecx","ret"},0)
					ADD_CODE({"lw $31,($17)","addiu $17,$17,4","jr $31","nop"},0)
				end if
				userWords = assoc_insert(pendingWordDef[2],wordId,userWords)
				pendingWordDef = {}
				ifStack = context[1]
				loopStack = context[2]
				context = {}
			else
				ERROR("Use of ; with no corresponding :",tokens[p])
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
			CMPI_mips("eq",p)
			
		-- <>
		elsif tokens[p][1] = W_NE then
			CMPI_mips("ne",p)
		
		-- >
		elsif tokens[p][1] = W_GT then
			CMPI_mips("gt",p)
		-- >=
		elsif tokens[p][1] = W_GE then
			CMPI_mips("ge",p)

		-- <
		elsif tokens[p][1] = W_LT then
			CMPI_mips("lt",p)
			--ADD_CODE({"pop eax","xor ecx,ecx","cmp [esp],eax","setl cl","neg ecx","mov [esp],ecx"},0)
		-- <=
		elsif tokens[p][1] = W_LE then
			CMPI_mips("le",p)

		-- 0=
		elsif tokens[p][1] = W_EQ0 then
			ADD_CODE({"mov #0,r1","cmp/eq #0,r0","negc r1,r0"},0)
		-- 0<>
		elsif tokens[p][1] = W_NE0 then
			ADD_CODE({"mov #0,r1","cmp/eq #0,r0","negc r1,r0","not r0,r0"},0)
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
			ADD_CODE({"neg r0,r0"},0)
			

		-- ABS
		elsif tokens[p][1] = W_ABS then
			--ADD_CODE({"cmp.l #0,d0","slt d1","ext.w d1","ext.l d1","move.l d1,d2","eor.l d1,d0","and.l #1,d2","add.l d2,d0"},0)

		-- ALLOT
		elsif tokens[p][1] = W_ALLOT then
			ADD_CODE({"add r0,r9","mov.l @r10+,r0"},0)
			
		-- AND
		elsif tokens[p][1] = W_AND then
			ADD_CODE({"mov.l @r10+,r1","and r1,r0"},0)

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
			
			ADD_CODE({"mov.l r7,@-r12","mov.l r6,@-r12","mov.l @r10+,r7","mov r0,r6","mov.l @r10+,r0"},0)
			ADD_CODE({s&":"},0)

		-- LOOP
		elsif tokens[p][1] = W_LOOP then
			if optI then
				--ADD_CODE({"inc esi","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				--ADD_CODE({"add r6,r6,#1","cmp r6,r7","bne "&loopStack[length(loopStack)]},0)
				ADD_CODE({"add #1,r6","cmp/eq r7,r6","bf "&loopStack[length(loopStack)]},0)				
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			ADD_CODE({"mov.l @r12+,r6","mov.l @r12+,r7"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if

		-- +LOOP
		elsif tokens[p][1] = W_INCLOOP then
			if optI then
				--ADD_CODE({"pop eax","add esi,eax","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				--ADD_CODE({"mov eax,loopstackptr","pop ebx","sub eax,8","add [eax+4],ebx","mov ecx,[eax]","cmp [eax+4],ecx","jne "&loopStack[length(loopStack)]},0)
				ADD_CODE({"add.l d0,d6","move.l (a2)+,d0","cmp d7,d6","bne "&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			ADD_CODE({"move.l (a4)+,d6","move.l (a4)+,d7"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if

		-- I
		elsif tokens[p][1] = W_I then
			if optI then
				--ADD_CODE({"push esi"},0)
			else
				--ADD_CODE({"mov eax,loopstackptr","push dword ptr [eax-4]"},0)
				ADD_CODE({"mov.l r0,@-r10","mov r6,r0"},0)
			end if
		-- J
		elsif tokens[p][1] = W_J then
			ADD_CODE({"mov.l r0,@-r10","mov.l @r12,r0"},0)
			
		-- CASE
		elsif tokens[p][1] = W_CASE then
			if case_ = -1 then
				case_ = 0
				hasDefault = 0
				caseLbl = sprintf("__case_%06x",cases)
				ADD_CODE({"mov r0,r8","mov.l @r10+,r0"},0)
				
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
							--if tokens[p-1][1]=NUMBER then
							--	constants = assoc_insert(tokens[p+1][2],{s,0,n+1,tokens[p-1][2]},constants)
							--else
							--q = add_literal(s)
							--r = add_literal(tokens[p+2][2])
							constants = assoc_insert(tokens[p+2][2],{s,0,n+1,0,q},constants)
							ADD_CODE({"mov.l "&literals[r][1]&",r1","mov.l "&literals[q][1]&",r2","mov.l r1,@r2"},0)
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
							--q = add_literal(s)
							constants = assoc_insert(tokens[p+1][2],{s,0,n+1},constants)
							ADD_CODE({"la $8,"&s,"sw $25,($8)","lw $25,($16)","addiu $16,$16,4"},0)
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
				if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],variables[1])=0 then
					s = sprintf("var_%07x",length(variables[1]))
					--q = add_literal(s)
					variables = assoc_insert(tokens[p+1][2],{s,4},variables)
					--ADD_CODE({"ldr r1,="&s,"str r9,[r1]","add r9,r9,#4"},0)
					ADD_CODE({"la $8,"&s,"sw $15,($8)","addiu $15,$15,4"},0)
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
		

		-- HERE
		elsif tokens[p][1] = W_HERE then
			ADD_CODE({"mov.l r0,@-r10","mov r9,r0"},0)
		-- ,
		elsif tokens[p][1] = W_COMMA then
			ADD_CODE({"mov.l r0,@r9","add #4,r9","mov.l @r10+,r0"},0)
			
		-- c,
		elsif tokens[p][1] = W_CCOMMA then
			--ADD_CODE({"move.b d0,(a1)+","move.l (a2)+,d0"},0)

		-- w,
		elsif tokens[p][1] = W_WCOMMA then
			--ADD_CODE({"move.w d0,(a1)+","move.l (a2)+,d0"},0)
			
		
		-- CALL
		--elsif tokens[p][1] = W_CALL then
		--	ADD_CODE({"call "&tokens[p+1][2]},0)
		--	p += 1
			
		-- DROP
		elsif tokens[p][1] = W_DROP then
			ADD_CODE({"lw $25,($16)","addiu $16,$16,4"},0)

		-- 2DROP
		elsif tokens[p][1] = W_2DROP then
			ADD_CODE({"add #4,r10","mov.l @r10+,r0"},0)
			
		-- DUP
		elsif tokens[p][1] = W_DUP then
			ADD_CODE({"mov.l r0,@-r10"},0)
		-- 2DUP
		elsif tokens[p][1] = W_2DUP then
			--ADD_CODE({"mov r2,r0","ldr r1,[r10]","stmfd r10!,{r1,r2}"},0)

		-- PICK
		elsif tokens[p][1] = W_PICK then
			--ADD_CODE({"ldr r0,[r10,r0,lsl#2]"},0)
			
		-- NIP
		elsif tokens[p][1] = W_NIP then
			--ADD_CODE({"pop eax","pop ebx","push eax"},0)
			ADD_CODE({"add #4,r10"},0)
			
		-- TUCK
		--elsif tokens[p][1] = W_TUCK then
		--	ADD_CODE({"pop eax","pop ebx","push eax","push ebx","push eax"},0)
		

		-- OVER
		elsif tokens[p][1] = W_OVER then
			ADD_CODE({"mov.l @r10,r1","mov.l r0,@-r10","mov r1,r0"},0)
			
		-- ELSE
		elsif tokens[p][1] = W_ELSE then
			if length(ifStack) and and_bits(length(ifStack),1)=0 then
				ADD_CODE({"bra "&ifStack[2],"nop"},0)
				ADD_CODE({ifStack[1]&":"},0)
				ifStack = ifStack[2..length(ifStack)]
			else
				ERROR("Unmatched ELSE",tokens[p])
			end if
		
		-- EXIT
		elsif tokens[p][1] = W_EXIT then
			if length(pendingWordDef) then
				ADD_CODE({"bra "&wordId&"_return","nop"},0)
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
				ADD_CODE({fastCmp&" "&ifStack[1]},0)
				fastCmp = ""
			else
				--ADD_CODE({"cmp r0,#-1","ldr r0,[r10],#4","bne "&ifStack[1]},0)
				ADD_CODE({"mov r0,r1","mov.l @r10+,r0","cmp/pz r1","bt "&ifStack[1]},0)
			end if
			
		-- LEAVE
		elsif tokens[p][1] = W_LEAVE then
			ADD_CODE({"bra "&loopStack[length(loopStack)]&"_end","nop"},0)
		-- ?LEAVE
		elsif tokens[p][1] = W_LEAVETRUE then
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&loopStack[length(loopStack)]&"_end"},0)
				fastCmp = ""
			else
				ADD_CODE({"mov r0,r1","mov.l @r10+,r0","cmp/pz r1","bf "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- NULL
		elsif tokens[p][1] = W_NULL or tokens[p][1]=W_FALSE then
			PUSH_mips(0)
		-- TRUE
		elsif tokens[p][1] = W_TRUE then
			PUSH_mips(-1)
		-- BL
		elsif tokens[p][1] = W_BL then
			PUSH_mips(' ')
		
		-- OR
		elsif tokens[p][1] = W_OR then
			ADD_CODE({"lw $8,($16)","addiu $16,$16,4","or $25,$25,$8"},0)

		-- REPEAT
		elsif tokens[p][1] = W_REPEAT then
			if length(loopStack) then
				ADD_CODE({"bra "&loopStack[length(loopStack)],"nop"},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				if length(loopStack) = 1 then
					loopStack = {}
				else
					loopStack = loopStack[1..length(loopStack)-1]
				end if
			else
				ERROR("REPEAT with no matching BEGIN",tokens[p])
			end if
		-- ROT
		elsif tokens[p][1] = W_ROT then
		--	ADD_CODE({"pop eax","pop ecx","pop edx","push ecx","push edx","push eax"},0)
			ADD_CODE({"move.l (a2)+,d1","move.l (a2)+,d2","move.l d1,-(a2)","move.l d0,-(a2)","move.l d2,d0"},0)

		-- SWAP
		elsif tokens[p][1] = W_SWAP then
			ADD_CODE({"lw $8,($16)","sw $25,($16)","or $25,$0,$8"},0)
			
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
				ERROR(ERROR_NO_BEGIN,tokens[p])
			else
				ADD_CODE({"or $8,$0,$25","lw $25,($16)","addiu $16,$16,4","li $9,-1","bne $8,$9,"&loopStack[length(loopStack)],"nop"},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			end if
			
		-- WHILE
		elsif tokens[p][1] = W_WHILE then
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&loopStack[length(loopStack)]&"_end"},0)
				fastCmp = ""
			else
				ADD_CODE({"or $8,$0,$25","lw $25,($16)","addiu $16,$16,4","li $9,-1","bne $8,$9,"&loopStack[length(loopStack)]&"_end","nop"},0)
			end if
			
		-- XOR
		elsif tokens[p][1] = W_XOR then
			ADD_CODE({"lw $8,($16)","addiu $16,$16,4","xor $25,$25,$8"},0)
		
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
						ERROR(ERROR_EOF,tokens[p-1])
					end if
				end while
				--literals = append(literals,{sprintf("lit_%06x",length(literals)),"db \""&s&"\",0"})
				--ADD_CODE({"push offset "&literals[length(literals)][1]},0)
				literals = append(literals,{sprintf("lit_%06x",length(literals)),".asciz \""&s&"\""})
				ADD_CODE({"addi $16,$16,-4","sw $25,($16)","la $25,"&literals[length(literals)][1]},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		elsif tokens[p][1] = UNKNOWN then
			n = bsearch(tokens[p][2],userWords[1])
			if n then
				--s = sprintf("__ret_%04x",calls)
				--calls += 1
				--ADD_CODE({"lea "&s&",a6","bra "&userWords[2][n],s&":"},0)
				ADD_CODE({"jal "&userWords[2][n],"nop"},0)
			end if
			
			if not n then
				if length(pendingWordDef) then
					n = bsearch(tokens[p][2],params[1])
					if n then
					--	ADD_CODE({sprintf("push dword ptr [ebp+%d]",params[2][n][1])},0)
					--ADD_CODE({sprintf("mov.l r0,@-r10","mov #%d,r0",params[2][n][1]),"mov.l @(r0,r10),r0"},0)
					ADD_CODE({"addi $16,$16,-4","sw $25,($16)",sprintf("lw $25, %d($19)",params[2][n][1])},0)
					end if
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],variables[1])
				if n then
					ADD_CODE({"addi $16,$16,-4","sw $25,($16)","la $8,"&variables[2][n][1],"lw $25,($8)"},0)
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],constants[1])
				if n then
					if length(constants[2][n])=3 then
						ADD_CODE({"addi $16,$16,-4","sw $25,($16)","la $8,"&constants[2][n][1],"lw $25,($8)"},0)
						constants[2][n][2] = 1	-- Mark the constant as being used
					elsif length(constants[2][n])=4 then
						PUSH_mips(constants[2][n][4])
					--elsif length(constants[2][n])=5 then
					--	ADD_CODE({"mov.l r0,@-r10","mov.l "&literals[constants[2][n][5]][1]&",r1","mov.l @r1,r0"},0)
					--	constants[2][n][2] = 1	-- Mark the constant as being used
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
					s = sprintf("__ret_%04x",calls)
					calls += 1
					ADD_CODE({"jal "&deferred[2][n]&" # deferred",s&":"},0)
				end if
			end if
			
			if not n then
				ERROR("Undefined word",tokens[p])
			end if
		else
			ERROR("Unsupported word",tokens[p])
		end if

		p += 1
	end while
	
	return 0
end function





global procedure forthec_mips()
	sequence dkarmdir,mcpu,gccend,march
	sequence asmOpts,linkOpts,crt0,lscript
	integer nofiles
	
	loops = 0
	calls = 0
	cases = 0
	ifs = 0
	strops = 0
	usesConsole = 0
	verbose = 0
	noMangle = 0
	optLevel = 5
	dkarmdir = "c:\\m68k-coff-gcc"
	asmOpts = "--register-prefix-optional --bitwise-or"	-- default
	linkOpts = "-T lnkscript"
	mcpu = "-m68000"
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
		puts(1,"forthec -t arm [options] <infile> <outfile>\n\n")
		puts(1,"Options:\n\n")
		puts(1,"-O<n>\t\tOptimisation level (n=0 min, n=6 max)\n")
		puts(1,"-nm\t\tNo name mangling\n")
		--puts(1,"-cpu <name>\tSpecify target cpu (m68000)\n")
		--puts(1,"-arch <name>\tSpecify target architechture (armv4)\n")
		--puts(1,"-eb\t\tBig-endian\n")
		--puts(1,"-el\t\tLittle-endian (default)\n")
		--puts(1,"-nofpu\t\tDisable fpu code generation\n")
		--puts(1,"-entry <name>\tOverride default entrypoint symbol (_start)\n")
		puts(1,"-fentry <name>\tOverride default forth entrypoint symbol (_main)\n")
		puts(1,"-crt0 <name>\tOverride default crt0 filename (crt0.o)\n")
		puts(1,"-ls <name>\tOverride default linkscript filename (md.ld)\n")
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
		mcpu = "-"&CMD[find("-cpu",CMD)+1]
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
--		cfgin = get(cfgfile)
--		if cfgin[1]=GET_SUCCESS then
--			if equal(cfgin[2][1],"MASMDIR") then
--				masmdir = cfgin[2][2]
--			end if
--		end if
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
	immliterals = {{},{}}
	pendingWordDef = {}
	tokens = {}
	userWords = {{},{}}
	constants = {{},{}}
	fconstants = {{},{}}
	variables = {{},{}}

	iwramStart = #06004000
	iwramEnd   = #060FFFFF
	ewramStart = #00200000
	ewramEnd   = #002FFFFF
	

	t1 = time()
	if parse(CMD[infpos]) then end if
	if compile_mips() then end if
	if optLevel>0 then
		if verbose then
			puts(1,"Optimising\n")
		end if
		--maincode = optimise_mips(maincode,1)
		--code = optimise_mips(code,0)
	end if


	puts(outfile,"# Generated by ForthEC"&NL&NL)
	puts(outfile,NL&"##############################################################################"&NL&NL)


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

	--puts(outfile,".data"&NL)

	puts(outfile,".bss"&NL)
	puts(outfile,".align 2"&NL)

	for i=1 to length(constants[2]) do
		if constants[2][i][2] or optLevel=0 then
			----puts(outfile,"\t"&constants[2][i][1]&" dd ?"&NL)
			--puts(outfile,"\t.global "&constants[2][i][1]&NL)
			--puts(outfile,"\t.size "&constants[2][i][1]&",4"&NL)
			puts(outfile,"\t"&constants[2][i][1]&":"&NL)
			puts(outfile,"\t\t.long 0"&NL)
		end if
	end for

	--for i=1 to length(fconstants[2]) do
		--puts(outfile,"\t"&fconstants[2][i]&" dq ?"&NL)
	--end for

	for i=1 to length(variables[2]) do
		--if variables[2][i][2] = 4 then
			--puts(outfile,"\t"&variables[2][i][1]&" dd ?"&NL)
			puts(outfile,"\t"&variables[2][i][1]&":"&NL)
			puts(outfile,"\t\t.long 0"&NL)
		--else
		--	puts(outfile,"\t"&variables[2][i][1]&" dq ?"&NL)
		--end if
	end for

	puts(outfile,NL&"##############################################################################"&NL&NL)

	-- Write code section
	--puts(outfile,".code"&NL&"align 4"&NL&NL)
	puts(outfile,".text"&NL&".globl "&fentry&NL&".align 2"&NL)

	for i=1 to length(code) do
		puts(outfile,code[i]&NL)
	end for

	puts(outfile,NL&"##############################################################################"&NL&NL)

	puts(outfile,fentry&":"&NL)
	--puts(outfile,   "mov.l _parm_stack_,r10"&NL&	-- parameter stack
	--                "mov.l _ret_stack_,r11"&NL&	-- return stack
	--                "mov.l _loop_stack_,r12"&NL&	-- loop stack
	--                "mov.l _dictionary_,r9"&NL	-- "dictionary"
	--                )


	for i=1 to length(maincode) do
		if length(maincode[i]) then
			puts(outfile,maincode[i]&NL)
		end if
	end for

	--puts(outfile, sprintf(".align 2"&{13,10}&"_parm_stack_: .long 0x%08x",iwramEnd+1)&NL)
	--puts(outfile, sprintf("_ret_stack_:  .long 0x%08x",iwramEnd+1-#1000)&NL)
	--puts(outfile, sprintf("_loop_stack_: .long 0x%08x",iwramEnd+1-#3000)&NL)
	--puts(outfile, sprintf("_dictionary_: .long 0x%08x",iwramEnd+1-#10000)&NL)


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
		system(dkarmdir&"\\bin\\m68k-coff-as "&mcpu&" "&asmOpts&" -o"&outname[1]&".o "&outname[1]&".asm",2)
		system(dkarmdir&"\\bin\\m68k-coff-ld -T "&lscript&" "&crt0&" "&outname[1]&".o"&morefiles,2)
		system(dkarmdir&"\\bin\\m68k-coff-objcopy -O binary a.out "&outname[1]&".bin",2)

		system("del "&outname[1]&".asm",2)
		system("del "&outname[1]&".o",2)
		system("del a.out",2)

	elsif equal(outname[2],"o") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system(dkarmdir&"\\bin\\arm-elf-as "&mcpu&" "&asmOpts&" -o"&outname[1]&".o "&outname[1]&".asm",2)
	end if



	if verbose then
		puts(1,"\nPress any key..")
		while get_key() = -1 do end while
	end if
end procedure




