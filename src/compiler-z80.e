-- ForthEC : A Forth compiler
-- /Mic, 2004/2009
--
-- Z80 code generator
--


without warning
include forthec.e
include optimiser-z80.e


	
-- Register use:
----------------
-- d0  TOS
-- d1  Work register
-- d2  -"-
-- d3 
-- d4
-- d5
-- d6  Loop counter
-- d7  Loop limit
-- a0  Zero
-- a1  Dictionary pointer
-- a2  Parameter stack
-- a3  Return stack
-- a4  Loop stack
-- a5
-- a6  Link register
-- a7  -

-- Old layout
-------------
-- a	Work register
-- bc	TOS
-- de	Dictionary pointer
-- hl	Work register
-- sp	Parameter stack
-- ix	Return stack
-- iy	Loop stack

-- New layout
-------------
-- a	Work register
-- bc	TOS
-- de	Return stack
-- hl	Work register
-- sp	Parameter stack
-- bc'	Loop stack
-- de'	Loop counter
-- hl'	Loop limit
-- ix	Link register
-- iy	Dictionary pointer

--###########################################################################################





-- Push a value on the parameter stack
procedure PUSH_z80(atom a)
	atom mask
	integer hi,lo,ones
	
	ADD_CODE({"push bc"},0)
	ADD_CODE({sprintf("ld bc,%d",a)},0)
end procedure


-- Handle comparison operations
procedure CMPI_z80(sequence cond,integer p)
	integer n
	
	n = 0
	if p<length(tokens) and optLevel>=4 then
		n = tokens[p+1][1]

		-- Is the next token IF or WHILE ?
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

		-- Is the next token ?LEAVE ?
		elsif n=W_LEAVETRUE then
			fastCmp = "b"&cond
			ADD_CODE({"move.l d0,d1","move.l (a2)+,d2","move.l (a2)+,d0","cmp.l d1,d2"},0)
		
		else
			n = 0
		end if
	end if
	
	-- The general case
	if not n then
		ADD_CODE({"move.l (a2)+,d1","cmp.l d1,d0","s"&cond&" d0","ext.w d0","ext.l d0"},0)
	end if
end procedure



function compile_z80()
	integer n,p,q,lastComp,case_,hasDefault,touch,optI
	sequence s,t,token,wordId,loopStack,loopStacks,params,caseLbl,asm,context
	
	
	if verbose then
		puts(1,"Compiling\n")
	end if
	
	lastComp = 0
	case_ = -1
	loopStack = {}
	loopStacks = {}
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
				PUSH_z80(tokens[p][2])
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
			ADD_CODE({"move.l (a2)+,(a0,d0)","move.l (a2)+,d0"},0)

		-- c!
		elsif tokens[p][1] = W_CSTORE then
			--ADD_CODE({"move.b (a2)+,(a0,d0)","addq.l #3,a2","move.l (a2)+,d0"},0)
			ADD_CODE({"pop hl","ld a,l","ld (bc),a","pop bc"},0)

		-- w!
		elsif tokens[p][1] = W_WSTORE then
			--ADD_CODE({"move.w (a2)+,(a0,d0)","addq.l #2,a2","move.l (a2)+,d0"},0)
			ADD_CODE({"pop hl","ld a,l","ld (bc),a","ld a,h","inc bc","ld (bc),a","pop bc"},0)

		-- !r+
		elsif tokens[p][1] = W_STORE_R_INC then
			ADD_CODE({"move.l (a2)+,a5","move.l d0,(a5)+","move.l a5,d0"},0)

		-- ON
		elsif tokens[p][1] = W_ON then
			--ADD_CODE({"move.l #-1,(a0,d0)","move.l (a2)+,d0"},0)
			ADD_CODE({"ld a,255","ld (bc),a","inc bc","ld (bc),a","pop bc"},0)
		
		-- OFF
		elsif tokens[p][1] = W_OFF then
			ADD_CODE({"ld a,0","ld (bc),a","inc bc","ld (bc),a","pop bc"},0)
			
		-- @
		elsif tokens[p][1] = W_FETCH then
			ADD_CODE({"move.l (a0,d0),d0"},0)
		-- @+
		elsif tokens[p][1] = W_FETCHADD then
			ADD_CODE({"move.l d0,d1","move.l (a0,d1),d0","addq.l #4,d1","move.l d1,-(a2)"},0)

		-- c@
		elsif tokens[p][1] = W_CFETCH then
			--ADD_CODE({"move.b (a0,d0),d0","and.l #255,d0"},0)
			ADD_CODE({"ld a,(bc)","ld c,a","ld b,0"},0)

		-- c@+
		elsif tokens[p][1] = W_CFETCHADD then
			ADD_CODE({"ld a,(bc)","inc bc","push bc","ld c,a","ld b,0"},0)

		-- cs@
		elsif tokens[p][1] = W_CSFETCH or
		      tokens[p][1] = W_SCFETCH then
			ADD_CODE({"move.b (a0,d0),d0","ext.w d0","ext.l d0"},0)
		-- w@
		elsif tokens[p][1] = W_WFETCH then
			ADD_CODE({"move.w (a0,d0),d0","and.l #65535,d0"},0)

		-- w@+
		elsif tokens[p][1] = W_WFETCHADD then
			ADD_CODE({"move.l d0,d1","move.w (a0,d1),d0","addq.l #2,d1","move.l d1,-(a2)","and.l #65535,d0"},0)
	
		-- a@
		--elsif tokens[p][1] = W_ACCFETCH then
		--	ADD_CODE({"push eax"},0)
			

		-- +
		elsif tokens[p][1] = W_ADD then
			ADD_CODE({"pop hl","add hl,bc","ld bc,hl"},0)

		-- -
		elsif tokens[p][1] = W_SUB then
			--ADD_CODE({"move.l (a2)+,d1","sub.l d0,d1","move.l d1,d0"},0)
			ADD_CODE({"pop hl","and a","sbc hl,bc","ld c,l","ld b,h"},0)

		-- *
		elsif tokens[p][1] = W_MUL then
			--ADD_CODE({"move.l (a2)+,d1","muls d1,d0"},0)
			
		-- /
		elsif tokens[p][1] = W_DIV then
		--	ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","divs d1,d0","ext.l d0"},0)
		-- MOD
		elsif tokens[p][1] = W_MOD then
		--	ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","divs d1,d0","swap d0","ext.l d0"},0)
		-- /MOD
		elsif tokens[p][1] = W_DIVMOD then
		--	ADD_CODE({"move.l d0,d1","move.l (a2),d0","divs d1,d0","move.l d0,d1","ext.l d0","swap d1","ext.l d1","move.l d1,(a2)"},0)

		
		-- /C
		elsif tokens[p][1] = W_CSIZE then
			PUSH_z80(1)

		-- /F
		elsif tokens[p][1] = W_FSIZE then
			PUSH_z80(8)

		-- /N
		elsif tokens[p][1] = W_NSIZE then
			PUSH_z80(4)
			
		-- 1-
		elsif tokens[p][1] = W_DEC then
			ADD_CODE({"dec bc"},0)
		-- 1+
		elsif tokens[p][1] = W_INC then
			ADD_CODE({"inc bc"},0)
			
		-- 2*
		elsif tokens[p][1] = W_MUL2 then
			ADD_CODE({"ld hl,bc","add hl,hl","ld bc,hl"},0)

		-- 2/
		elsif tokens[p][1] = W_DIV2 then
			--ADD_CODE({"asr.l #1,d0"},0)
			ADD_CODE({"srl b","rr c"},0)
		
		-- <<
		elsif tokens[p][1] = W_SHL then
			--ADD_CODE({"move.l (a2)+,d1","lsl.l d0,d1","move.l d1,d0"},0)
			ADD_CODE({"ld a,c","pop bc","-:","cp 0","jr z,+","sla c","rl b","dec a","jr -"},0)
			
		-- >>
		elsif tokens[p][1] = W_SHR then
			--ADD_CODE({"move.l (a2)+,d1","lsr.l d0,d1","move.l d1,d0"},0)
			ADD_CODE({"ld a,c","pop bc","-:","cp 0","jr z,+","srl b","rr c","dec a","jr -"},0)
		
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
		elsif tokens[p][1] = W_COLON or tokens[p][1] = W_COLONIRQ then 
			if p<length(tokens) then
				if tokens[p+1][1]=UNKNOWN then
					if not bsearch(tokens[p+1][2],userWords[1]) then 
						if not length(pendingWordDef) then
--							pendingWordDef = tokens[p+1]
--							params = {{},{}}
--							ADD_CODE({NL&"; "&tokens[p+1][2]},0)
--							wordId = make_label(tokens[p+1][2]) 
--							ADD_CODE({wordId&":"},0)
--							p += 1
--							loopStacks = loopStack
--							loopStack = {}

							if (referred[2][bsearch(tokens[p+1][2],referred[1])][1]<2) and
							   (q<=0) and
							   (not noMangle) then
								ignoreOutput = 1
							end if

							inline = (allowInline) and (tokens[p][1] = W_COLON)
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
							
							if not ignoreOutput then
								inline = 0
							end if
							if not inline then
								ADD_CODE({NL&"; "&tokens[p+1][2]},0)
							
								if q<=0 then
									wordId = make_label(tokens[p+1][2])
								else
									wordId = tokens[p+1][2]
								end if
								ADD_CODE({wordId&":"},0)
								--ADD_CODE({"p2 += -4;","r0 = rets;","[p2] = r0;"},0)
		

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
					ERROR(ERROR_EXPECTED_ID,tokens[p+1])
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
				--	ADD_CODE({"sub ipstackptr,4","mov eax,ipstackptr","mov [eax],ebp","mov ebp,esp"},0)
				elsif length(params[1]) then
				--	ADD_CODE({"mov ebp,esp"},0)
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
					--	ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","push ecx","ret"},0)
					else
						--ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax]","add ipstackptr,4","push ecx","ret"},0)
						--ADD_CODE({"move.l (a3)+,a6","jmp (a6)"},0)
						--ADD_CODE({"ld l,(ix+0)","ld h,(ix+1)","inc ix","inc ix","jp (hl)"},0)
						ADD_CODE({"ld a,(de)","ld l,a","inc de","ld a,(de)","ld h,a","inc de","jp (hl)"},0)
					end if
					--ADD_CODE({".pool"},0)
					userWords = assoc_insert(pendingWordDef[2],{wordId,0},userWords)
					if length(loopStack) then
						ERROR(ERROR_OPEN_DO,tokens[p])
					end if
					--loopStack = loopStacks
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

		-- ;i
		elsif tokens[p][1] = W_SEMICOLONI then
			if length(pendingWordDef)=3 then
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
				--	ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","push ecx","ret"},0)
				else
					--ADD_CODE({"move.l (a3)+,a6","jmp (a6)"},0)
					--ADD_CODE({"ld l,(ix+0)","ld h,(ix+1)","inc ix","inc ix","push hl","reti"},0)
					ADD_CODE({"ld a,(de)","ld l,a","inc de","ld a,(de)","ld h,a","inc de","push hl","reti"},0)
				end if
				--ADD_CODE({".pool"},0)
				userWords = assoc_insert(pendingWordDef[2],{wordId,0},userWords)
				pendingWordDef = {}
				if length(loopStack) then
					ERROR(ERROR_OPEN_DO,tokens[p])
				end if
				loopStack = loopStacks
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if

		-- ;n
		elsif tokens[p][1] = W_SEMICOLONN then
			if length(pendingWordDef)=3 then
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
				--	ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","push ecx","ret"},0)
				else
					--ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax]","add ipstackptr,4","push ecx","ret"},0)
					--ADD_CODE({"move.l (a3)+,a6","jmp (a6)"},0)
					--ADD_CODE({"ld l,(ix+0)","ld h,(ix+1)","inc ix","inc ix","push hl","retn"},0)
					ADD_CODE({"ld a,(de)","ld l,a","inc de","ld a,(de)","ld h,a","inc de","push hl","retn"},0)
				end if
				--ADD_CODE({".pool"},0)
				userWords = assoc_insert(pendingWordDef[2],{wordId,0},userWords)
				pendingWordDef = {}
				if length(loopStack) then
					ERROR(ERROR_OPEN_DO,tokens[p])
				end if
				loopStack = loopStacks
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if
			
		-- ;@
		elsif tokens[p][1] = W_SEMICOLONPOP then
			if length(pendingWordDef)=3 then
				--ADD_CODE({wordId&"_return:","sub ipstackptr,2","mov eax,ipstackptr","mov ecx,[ipstack+eax*4]","inc eax","mov ebp,[ipstack+eax*4]","pop eax","push ecx","ret"},0)			
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
				--	ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax+4]","add ipstackptr,8","mov ebp,[eax]","pop eax","push ecx","ret"},0)
				else
				--	ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax]","add ipstackptr,4","pop eax","push ecx","ret"},0)
				end if
				userWords = assoc_insert(pendingWordDef[2],{wordId,0},userWords)
				pendingWordDef = {}
			else
				ERROR("Use of ;@ with no corresponding :",tokens[p])
			end if

		elsif tokens[p][1] = W_SETINT then
			ADD_CODE({"; set interrupt handler"},0)
			
		-- '
		elsif tokens[p][1] = W_QUOTE then
			n = bsearch(tokens[p+1][2],userWords[1])
			if n then
				ADD_CODE({"move.l d0,-(a2)","lea "&userWords[2][n][1]&",a5","move.l a5,d0"},0)
			--	ADD_CODE({"mov eax,offset "&userWords[2][n],"push eax"},0)
				p += 1
			end if
			
		-- =
		elsif tokens[p][1] = W_EQ then
			CMPI_z80("eq",p)
			
		-- <>
		elsif tokens[p][1] = W_NE then
			CMPI_z80("ne",p)
		
		-- >
		elsif tokens[p][1] = W_GT then
			CMPI_z80("gt",p)
		-- >=
		elsif tokens[p][1] = W_GE then
			CMPI_z80("ge",p)

		-- <
		elsif tokens[p][1] = W_LT then
			CMPI_z80("lt",p)
		-- <=
		elsif tokens[p][1] = W_LE then
			CMPI_z80("le",p)

		-- 0=
		elsif tokens[p][1] = W_EQ0 then
			ADD_CODE({"cmp.l #0,d0","seq d0","ext.w d0","ext.l d0"},0)
		-- 0<>
		elsif tokens[p][1] = W_NE0 then
			ADD_CODE({"cmp.l #0,d0","sne d0","ext.w d0","ext.l d0"},0)
		-- 0>
		elsif tokens[p][1] = W_GT0 then
			ERROR("Unimplemented word",tokens[p])
		-- 0<
		elsif tokens[p][1] = W_LT0 then
			ERROR("Unimplemented word",tokens[p])


		-- NEGATE
		elsif tokens[p][1] = W_NEG then
			ADD_CODE({"neg.l d0"},0)
			

		-- ABS
		elsif tokens[p][1] = W_ABS then
			ADD_CODE({"cmp.l #0,d0","slt d1","ext.w d1","ext.l d1","move.l d1,d2","eor.l d1,d0","and.l #1,d2","add.l d2,d0"},0)

		-- ALLOT
		elsif tokens[p][1] = W_ALLOT then
			ADD_CODE({"add.l d0,a1","move.l (a2)+,d0"},0)
			
		-- AND
		elsif tokens[p][1] = W_AND then
			--ADD_CODE({"move.l (a2)+,d1","and.l d1,d0"},0)
			--ADD_CODE({"pop hl","add hl,bc","ld bc,hl"},0)
			ADD_CODE({"pop hl","ld a,b","and h","ld b,a","ld a,c","and l","ld c,a"},0)
			
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
			
			--ADD_CODE({"move.l d7,-(a4)","move.l d6,-(a4)","move.l (a2)+,d7","move.l d0,d6","move.l (a2)+,d0"},0)
			--ADD_CODE({"push de","ld de,65532","add iy,de","pop de","pop hl","ld (iy+0),l","ld (iy+1),h","ld (iy+2),c","ld (iy+3),b","pop bc"},0)
			ADD_CODE({"ld hl,(__forthec_loop_stack)","dec hl","ld a,ixh","ld (hl),a","dec hl","ld a,ixl","ld (hl),a",
				  "dec hl","ld a,iyh","ld (hl),a","dec hl","ld a,iyl","ld (hl),a","ld (__forthec_loop_stack),hl","push bc","pop ix","pop iy","pop bc"},0)
			
			ADD_CODE({s&":"},0)

		-- LOOP
		elsif tokens[p][1] = W_LOOP then
			if optI then
				--ADD_CODE({"inc esi","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
			--	ADD_CODE({"addq.l #1,d6","cmp.l d7,d6","bne "&loopStack[length(loopStack)]},0)
				--ADD_CODE({"push de","ld l,(iy+2)","ld h,(iy+3)","ld e,(iy+0)","ld d,(iy+1)","inc hl","ld (iy+2),l","ld (iy+3),h","and a","sbc hl,de","pop de","jp nz,"&loopStack[length(loopStack)]},0)
			ADD_CODE({"inc ix","ld a,ixl","xor iyl","ld h,a","ld a,ixh","xor iyh","or h","jp nz,"&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			--ADD_CODE({"move.l (a4)+,d6","move.l (a4)+,d7"},0)
			--ADD_CODE({"ex de,hl","ld de,4","add iy,de","ex de,hl"},0)
			ADD_CODE({"ld hl,(__forthec_loop_stack)","ld a,(hl)","ld iyl,a","inc hl","ld a,(hl)","ld iyh,a",
				  "inc hl","ld a,(hl)","ld ixl,a","inc hl","ld a,(hl)","ld ixh,a","inc hl","ld (__forthec_loop_stack),hl"},0)
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
				ADD_CODE({"move.l d0,-(a2)","move.l d6,d0"},0)
			end if
		-- J
		elsif tokens[p][1] = W_J then
			ADD_CODE({"move.l d0,-(a2)","move.l (a4),d0"},0)
			
		-- CASE
		elsif tokens[p][1] = W_CASE then
			if case_ = -1 then
				case_ = 0
				hasDefault = 0
				caseLbl = sprintf("__case_%06x",cases)
				ADD_CODE({"move.l d0,d5","move.l (a2)+,d0"},0)
				
			else
				ERROR("CASE inside CASE",tokens[p])
			end if
		
		-- OF
		elsif tokens[p][1] = W_OF then
			if case_>=0 and not hasDefault then
				ADD_CODE({"move.l (a2)+,d0","cmp.l d5,d0","bne "&caseLbl&sprintf("_%04x_false",case_)},0)
			elsif hasDefault then
				ERROR("OF found after DEFAULT",tokens[p])
			else
				ERROR("OF outside CASE",tokens[p])
			end if
		
		-- ENDOF
		elsif tokens[p][1] = W_ENDOF then
			if case_ >= 0 then
				ADD_CODE({"bra "&caseLbl&"_end"},0)
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
							ADD_CODE({"move.l #"&tokens[p+2][2]&","&s},0)
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
							--ADD_CODE({"ldr r1,="&s,"str r0,[r1]","ldr r0,[r10],#4"},0)
							ADD_CODE({"move.l #"&s&",a5","move.l d0,(a5)","move.l (a2)+,d0"},0)
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
					--ADD_CODE({"ldr r1,="&s,"str r9,[r1]","add r9,r9,#4"},0)
					ADD_CODE({"move.l #"&s&",a5","move.l a1,(a5)","addq.l #4,a1"},0)
					p += 1
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
					p += 1
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

		-- INP
		elsif tokens[p][1] = W_INP then
			--ADD_CODE({"pop dx","in al,dx"},0)
			ADD_CODE({"in c,(c)","ld b,0"},0)
		-- OUTP
		elsif tokens[p][1] = W_OUTP then
			ADD_CODE({"pop hl","out (c),l","pop bc"},0)
			--ADD_CODE({"pop dx","pop ax","out dx,al"},0)
			
		-- BYE
		elsif tokens[p][1] = W_BYE then
		

		-- HERE
		elsif tokens[p][1] = W_HERE then
			ADD_CODE({"move.l d0,-(a2)","move.l a1,d0"},0)
		-- ,
		elsif tokens[p][1] = W_COMMA then
			ADD_CODE({"move.l d0,(a1)+","move.l (a2)+,d0"},0)
			
		-- c,
		elsif tokens[p][1] = W_CCOMMA then
			ADD_CODE({"move.b d0,(a1)+","move.l (a2)+,d0"},0)

		-- w,
		elsif tokens[p][1] = W_WCOMMA then
			ADD_CODE({"move.w d0,(a1)+","move.l (a2)+,d0"},0)
			
		
		-- CALL
		--elsif tokens[p][1] = W_CALL then
		--	ADD_CODE({"call "&tokens[p+1][2]},0)
		--	p += 1

		-- CALLI
		elsif tokens[p][1] = W_CALLI then
			--ADD_CODE({"pop ax","call near ax"},0)
			ADD_CODE({"ld hl,bc","pop bc","call __forthec_call_hl"},0)

			
		-- DROP
		elsif tokens[p][1] = W_DROP then
			ADD_CODE({"pop bc"},0)

		-- 2DROP
		elsif tokens[p][1] = W_2DROP then
			ADD_CODE({"pop bc","pop bc"},0)
			
		-- DUP
		elsif tokens[p][1] = W_DUP then
			ADD_CODE({"push bc"},0)
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
			ADD_CODE({"addq.l #4,a2"},0)
			
		-- TUCK
		--elsif tokens[p][1] = W_TUCK then
		--	ADD_CODE({"pop eax","pop ebx","push eax","push ebx","push eax"},0)
		

		-- OVER
		elsif tokens[p][1] = W_OVER then
			ADD_CODE({"move.l (a2),d1","move.l d0,-(a2)","move.l d1,d0"},0)
			
		-- ELSE
		elsif tokens[p][1] = W_ELSE then
			if length(ifStack) and and_bits(length(ifStack),1)=0 then
				ADD_CODE({"bra "&ifStack[2]},0)
				ADD_CODE({ifStack[1]&":"},0)
				ifStack = ifStack[2..length(ifStack)]
			else
				ERROR("Unmatched ELSE",tokens[p])
			end if
		
		-- EXIT
		elsif tokens[p][1] = W_EXIT then
			if length(pendingWordDef) then
				ADD_CODE({"bra "&wordId&"_return"},0)
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
				ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","cmp.l #-1,d1","bne "&ifStack[1]},0)
			end if
			
		-- LEAVE
		elsif tokens[p][1] = W_LEAVE then
			ADD_CODE({"jp "&loopStack[length(loopStack)]&"_end"},0)
		-- ?LEAVE
		elsif tokens[p][1] = W_LEAVETRUE then
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&loopStack[length(loopStack)]&"_end"},0)
				fastCmp = ""
			else
				ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","cmp.l #-1,d1","beq "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- NULL
		elsif tokens[p][1] = W_NULL or tokens[p][1]=W_FALSE then
			PUSH_z80(0)
		-- TRUE
		elsif tokens[p][1] = W_TRUE then
			PUSH_z80(-1)
		-- BL
		elsif tokens[p][1] = W_BL then
			PUSH_z80(' ')
		
		-- OR
		elsif tokens[p][1] = W_OR then
			ADD_CODE({"pop hl","ld a,c","or l","ld c,a","ld a,b","or h","ld b,a"},0)

		-- REPEAT
		elsif tokens[p][1] = W_REPEAT then
			if length(loopStack) then
				ADD_CODE({"bra "&loopStack[length(loopStack)]},0)
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
			ADD_CODE({"move.l (a2)+,d1","move.l (a2)+,d2","move.l d1,-(a2)","move.l d0,-(a2)","move.l d2,d0"},0)

		-- SWAP
		elsif tokens[p][1] = W_SWAP then
			--ADD_CODE({"move.l (a2),d1","move.l d0,(a2)","move.l d1,d0"},0)
			ADD_CODE({"pop hl","push bc","ld c,l","ld b,h"},0)
			
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
				ADD_CODE({"bra "&loopStack[length(loopStack)],"nop"},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
				if length(loopStack) = 1 then
					loopStack = {}
				else
					loopStack = loopStack[1..length(loopStack)-1]
				end if
			end if
			
		-- UNTIL
		elsif tokens[p][1] = W_UNTIL then
			if not length(loopStack) then
				ERROR("Unmatched UNTIL",tokens[p])
			else
				ADD_CODE({"ld hl,1","add hl,bc","pop bc","jp nz,"&loopStack[length(loopStack)]},0)
				--ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","cmp.l #-1,d1","bne "&loopStack[length(loopStack)]},0)
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
				ADD_CODE({"move.l d0,d1","move.l (a2)+,d0","cmp.l #-1,d1","bne "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- XOR
		elsif tokens[p][1] = W_XOR then
			--ADD_CODE({"move.l (a2)+,d1","eor.l d1,d0"},0)
			ADD_CODE({"pop hl","ld a,l","xor c","ld c,a","ld a,h","xor b","ld b,a"},0)
		
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
				literals = append(literals,{sprintf("lit_%06x",length(literals)),".asciz \""&s&"\""})
				ADD_CODE({"move.l d0,-(a2)","move.l #"&literals[length(literals)][1]&",d0"},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		elsif tokens[p][1] = UNKNOWN then
			n = bsearch(tokens[p][2],userWords[1])
			if n then
				if userWords[2][n][2] then
					ADD_CODE({"; Inlined word "&tokens[p][2]},0)
					ADD_CODE(inlines[userWords[2][n][3]],0)
				else
					ADD_CODE({"call "&userWords[2][n][1]&";"},0)
					s = sprintf("__ret_%04x",calls)
					calls += 1
					--ADD_CODE({"lea "&s&",a6","bra "&userWords[2][n],s&":"},0)
					--ADD_CODE({"ld hl,"&s,"ld (ix+0),l","ld (ix+1),h","inc ix","inc ix","jp "&userWords[2][n],s&":"},0)
					--ADD_CODE({"dec ix","dec ix","ld (ix+0),"&s&"%256","ld (ix+1),"&s&"/256","jp "&userWords[2][n],s&":"},0)
					ADD_CODE({"ex de,hl","dec hl","ld (hl),>"&s,"dec hl","ld (hl),<"&s,"ex de,hl","jp "&userWords[2][n][1],s&":"},0)
				end if				
			end if
			
			if not n then
				if length(pendingWordDef) then
					n = bsearch(tokens[p][2],params[1])
					if n then
					--	ADD_CODE({sprintf("push dword ptr [ebp+%d]",params[2][n][1])},0)
					end if
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],variables[1])
				if n then
					ADD_CODE({"move.l d0,-(a2)","move.l #"&variables[2][n][1]&",d0"},0)
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],constants[1])
				if n then
					if length(constants[2][n])=3 then
						ADD_CODE({"move.l d0,-(a2)","move.l #"&constants[2][n][1]&",a5","move.l (a5),d0"},0)
						constants[2][n][2] = 1	-- Mark the constant as being used
					else
						--ADD_CODE({"str r0,[r10,#-4]!","ldr r0,="&sprintf("%d",constants[2][n][4])},0)
						PUSH_z80(constants[2][n][4])
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
					--ADD_CODE({"lea "&s&",a6","bra "&deferred[2][n]&" /* deferred */",s&":"},0)
					--ADD_CODE({"dec ix","dec ix","ld (ix+0),"&s&"%256","ld (ix+1),"&s&"/256","jp "&deferred[2][n]&" ; deferred",s&":"},0)
					ADD_CODE({"ex de,hl","dec hl","ld (hl),"&s&"/256","dec hl","ld (hl),"&s&"%256","ex de,hl","jp "&deferred[2][n]&" ; deferred",s&":"},0)
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





global procedure forthec_z80()
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
	noFold = 0
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
	fentry = "main"
	nofpu = ""

	labelPrefix = "__"


	nofiles = 1
	if length(CMD)>=4 then
		if not (find("-t",CMD)>0 and length(CMD)=4) then
			nofiles = 0
		end if
	end if
	
	if nofiles then
		puts(1,"forthec -t z80 [options] <infile> <outfile>\n\n")
		puts(1,"Options:\n\n")
		puts(1,"-O<n>\t\tOptimisation level (n=0 min, n=6 max)\n")
		puts(1,"-nm\t\tNo name mangling\n")
		--puts(1,"-cpu <name>\tSpecify target cpu (m68000)\n")
		--puts(1,"-entry <name>\tOverride default entrypoint symbol (_start)\n")
		puts(1,"-fentry <name>\tOverride default forth entrypoint symbol (main)\n")
		--puts(1,"-crt0 <name>\tOverride default crt0 filename (crt0.o)\n")
		--puts(1,"-ls <name>\tOverride default linkscript filename (md.ld)\n")
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
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		cfgin = get(cfgfile)
		if cfgin[1]=GET_SUCCESS then
			if equal(cfgin[2][1],"68KGCC") then
				dkarmdir = cfgin[2][2]
			end if
		end if
		close(cfgfile)
	end if


	outname = CMD[infpos+1]
	outname = cut_filename(outname)
	outname[2] = lower(outname[2])


	forthec_init()


	t1 = time()
	if parse(CMD[infpos]) then end if
	
	
	if optLevel>0 and not noFold then
		optimise_token_stream()
	end if
	
	count_refs()
	
	if compile_z80() then end if
	
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
		maincode = optimise_z80(maincode, 1)
		code = optimise_z80(code, 0)
	end if


	puts(outfile,"; Generated by ForthEC"&NL&NL)
	puts(outfile,NL&";##############################################################################"&NL&NL)

	puts(outfile,".orga $0000"&NL&"di"&NL&"jp "&fentry&NL)
	
	-- Write data
	puts(outfile,".data"&NL)
	for i=1 to length(literals) do
		puts(outfile,".even"&NL&"\t"&literals[i][1]&": "&literals[i][2]&NL)
	end for

	-- Write constants
	--puts(outfile,".bss"&NL)
	--puts(outfile,".even"&NL)
	for i=1 to length(constants[2]) do
		if constants[2][i][2] or optLevel=0 then
			puts(outfile,"\t"&constants[2][i][1]&":"&NL)
			puts(outfile,"\t\tdc.l 0"&NL)
		end if
	end for


	-- Write variables
	for i=1 to length(variables[2]) do
		--if variables[2][i][2] = 4 then
			--puts(outfile,"\t"&variables[2][i][1]&" dd ?"&NL)
			puts(outfile,"\t"&variables[2][i][1]&":"&NL)
			puts(outfile,"\t\tdc.l 0"&NL)
		--else
		--	puts(outfile,"\t"&variables[2][i][1]&" dq ?"&NL)
		--end if
	end for

	puts(outfile,NL&";##############################################################################"&NL&NL)

	-- Write code section
	--puts(outfile,".text"&NL&".globl "&fentry&NL&".even"&NL)
	for i=1 to length(code) do
		puts(outfile,code[i]&NL)
	end for

	puts(outfile,NL&";##############################################################################"&NL&NL)

	-- Set up address registers
	puts(outfile,fentry&":"&NL)
	--puts(outfile,   "move.l #0xffffff,a7"&NL&	-- regular stack (interrupts etc)
	--                "move.l #0xfffe00,a2"&NL&	-- parameter stack
	--                "move.l #0xfff000,a3"&NL&	-- return stack
	--                "move.l #0xffec00,a4"&NL&	-- loop stack
	--                "move.l #0xff8000,a1"&NL&	-- "dictionary"
	--                "move.l #0,a0"&NL
	--                )
	puts(outfile,"ld sp,$dff0"&NL&	-- parameter stack		
	             "ld de,$d200"&NL&	-- return stack
	             "ld iy,$c200"&NL&	-- "dictionary"
	             "exx"&NL&
	             "ld bc,$d100"&NL&	-- loop stack
	             "exx"&NL
	             )


	for i=1 to length(maincode) do
		if length(maincode[i]) then
			puts(outfile,maincode[i]&NL)
		end if
	end for

	puts(outfile,NL&".end "&NL)

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
		system(dkarmdir&"\\bin\\m68k-coff-as "&mcpu&" "&asmOpts&" -o"&outname[1]&".o "&outname[1]&".asm",2)
	end if



	if verbose then
		puts(1,"\nPress any key..")
		while get_key() = -1 do end while
	end if
end procedure




