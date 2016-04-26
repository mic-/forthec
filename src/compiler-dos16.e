-- ForthEC : A Forth compiler
-- /Mic, 2004/2008
--
-- x86 16-bit code generator
--


without warning
include forthec.e
include parser.e
include optimiser-dos16.e


	



-- Push a value on the parameter stack
procedure PUSH_dos16(atom a)
	if (a>=0 and a<128) then
		ADD_CODE({sprintf("push byte %d",a)},0)
	else
		ADD_CODE({sprintf("push word %d",a)},0)
	end if
end procedure


-- Handle comparison operations
procedure CMPI_dos16(sequence cond,integer p)
	integer n
	
	n = 0
	if p<length(tokens) and optLevel>=4 then
		n = tokens[p+1][1]
		if n=W_IF or n=W_WHILE then
			fastCmp = "jn"&cond
			if equal(fastCmp,"jnnz") then
				fastCmp = "jz"
			end if
			ADD_CODE({"pop ax","pop cx","cmp cx,ax"},0)
		elsif n=W_LEAVETRUE then
			fastCmp = "j"&cond
			ADD_CODE({"pop ax","pop cx","cmp cx,ax"},0)
		else
			n = 0
		end if
	end if
	if not n then
		ADD_CODE({"pop ax","mov bp,sp","xor cx,cx","cmp [bp],ax","set"&cond&" cl","neg cx","mov [bp],cx"},0)
	end if
end procedure


function alloc_reg(integer type_)
	integer n

	n = find(0,regs[type_])
	if n<1 then
		ERROR("No free registers",{0,0,0})
	end if
	regs[type_][n] = 1
	return n-1
end function


procedure free_reg(integer type_,integer n)
	regs[type_][n+1] = 0
end procedure



function compile_dos16()
	integer n,p,q,lastComp,case_,hasDefault,touch,optI,uses_ebp,ebpCodePos,hasLoops
	sequence s,t,token,wordId,loopStack,params,caseLbl,asm,lsp
	
	
	if verbose then
		puts(1,"Compiling\n")
	end if
	
	lastComp = 0
	case_ = -1
	loopStack = {}
	touch = 1
	fastCmp = ""
	optI = 0
	uses_ebp = 0
	ebpCodePos = 0
	lsp = "loopstackptr"
	hasLoops = 0
	
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
			PUSH_dos16(tokens[p][2])
		
		elsif tokens[p][1] = FNUMBER then
			t = sprintf("dq %f",tokens[p][2])
			if equal(t,"dq 1.000000") then
				n = -2
			elsif equal(t,"dq 0.000000") then
				n = -1
			else
				n = 0
				if optLevel>0 then
					for i=1 to length(literals) do
						if equal(t,literals[i][2]) then
							n = i
							exit
						end if
					end for
				end if
			end if
			if n then
				if fastfloat then
					if n=-1 then
						ADD_CODE({"fldz"},0)
					elsif n=-2 then
						ADD_CODE({"fld1"},0)
					else
						ADD_CODE({"fld qword ["&literals[n][1]&"]"},0)
					end if
				else
					if n=-1 then
						ADD_CODE({"mov eax,fpstackptr","fldz","add fpstackptr,8","fstp qword ptr [eax]"},0)
					elsif n=-2 then
						ADD_CODE({"mov eax,fpstackptr","fld1","add fpstackptr,8","fstp qword ptr [eax]"},0)
					else
						ADD_CODE({"mov eax,fpstackptr","fld qword ptr ["&literals[n][1]&"]","add fpstackptr,8","fstp qword ptr [eax]"},0)
					end if
				end if
			else
				if fastfloat then
					if n=-1 then
						ADD_CODE({"fldz"},0)
					elsif n=-2 then
						ADD_CODE({"fld1"},0)
					else
						ADD_CODE({"fld qword ["&sprintf("lit_%06x",length(literals))&"]"},0)
					end if
				else
					if n=-1 then
						ADD_CODE({"mov eax,fpstackptr","fldz","add fpstackptr,8","fstp qword ptr [eax]"},0)
					elsif n=-2 then
						ADD_CODE({"mov eax,fpstackptr","fld1","add fpstackptr,8","fstp qword ptr [eax]"},0)
					else
						ADD_CODE({"mov eax,fpstackptr","fld qword ptr ["&sprintf("lit_%06x",length(literals))&"]","add fpstackptr,8","fstp qword ptr [eax]"},0)
					end if
				end if
				--if n>0 then
					literals = append(literals,{sprintf("lit_%06x",length(literals)),t})
				--end if
			end if
			
		-- NOTOUCH
		elsif tokens[p][1] = W_NOTOUCH then
			--abort(0)
			--puts(1,"NOTOUCH\n")
			p += 1
			asm = {}
			while p<=length(tokens) do
				if tokens[p][1] = W_INCLUDE then
					if not find(tokens[p+1][2],includes) then
						includes = append(includes,tokens[p+1][2])
						p += 1
					end if
				elsif tokens[p][1] = W_TOUCH then
					--puts(1,"TOUCH\n")
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
		elsif tokens[p][1] = W_SPBOT then
			--ADD_CODE({"push dword ptr [stackBase]"},0)
		
		-- SP@
		elsif tokens[p][1] = W_SPTOP then
			ADD_CODE({"push sp"},0)
			
		-- !
		elsif tokens[p][1] = W_STORE then
			ADD_CODE({"pop bx","pop ax","mov [bx],ax"},0)

		-- a!
		elsif tokens[p][1] = W_ACCSTORE then
			ADD_CODE({"pop ax"},0)
		-- c!
		elsif tokens[p][1] = W_CSTORE then
			ADD_CODE({"pop bx","pop ax","mov [bx],al"},0)
		-- c:es!
		elsif tokens[p][1] = W_CESSTORE then
			ADD_CODE({"pop bx","pop ax","mov [es:bx],al"},0)
		-- w:es!
		elsif tokens[p][1] = W_WESSTORE then
			ADD_CODE({"pop bx","pop ax","mov [es:bx],ax"},0)
		-- es!
		elsif tokens[p][1] = W_ESSTORE then
			ADD_CODE({"pop es"},0)

		-- w!
		elsif tokens[p][1] = W_WSTORE then
			ADD_CODE({"pop bx","pop ax","mov [bx],ax"},0)

		-- f!
		elsif tokens[p][1] = W_FSTORE then
			if fastfloat then
				ADD_CODE({"pop bx","fstp qword [bx]"},0)
			else
				ADD_CODE({"mov bx,fpstackptr","pop bp","fld qword [bx-8]","sub fpstackptr,8","fstp qword [bp]"},0)
			end if
			
		-- f!s
		elsif tokens[p][1] = W_FSTORES then
			if fastfloat then
				ADD_CODE({"pop bx","fstp dword [bx]"},0)
			else
				ADD_CODE({"pop bp","mov bx,fpstackptr","fld qword ptr [bx-8]","sub fpstackptr,8","fstp dword ptr [bp]"},0)
			end if
			
		-- !r+
		elsif tokens[p][1] = W_STORE_R_INC then
			ADD_CODE({"pop ax","mov bp,sp","mov bx,[bp]","mov [bx],ax","add word [bp],2"},0)

		-- ON
		elsif tokens[p][1] = W_ON then
			ADD_CODE({"pop eax","mov dword ptr [eax],-1"},0)
		-- OFF
		elsif tokens[p][1] = W_OFF then
			ADD_CODE({"pop eax","mov dword ptr [eax],0"},0)
			
		-- @
		elsif tokens[p][1] = W_FETCH then
			ADD_CODE({"pop bx","push word [bx]"},0)
		-- c@
		elsif tokens[p][1] = W_CFETCH then
			ADD_CODE({"pop bx","movzx ax,byte [bx]","push ax"},0)
		-- c:es@
		elsif tokens[p][1] = W_CESFETCH then
			ADD_CODE({"pop bx","movzx ax,byte [es:bx]","push ax"},0)
		-- cs@
		elsif tokens[p][1] = W_CSFETCH then
			ADD_CODE({"pop bx","movsx ax,byte [bx]","push ax"},0)
		-- w@
		elsif tokens[p][1] = W_WFETCH then
			ADD_CODE({"pop bx","push word [bx]"},0)
	
		-- a@
		elsif tokens[p][1] = W_ACCFETCH then
			ADD_CODE({"push ax"},0)
			
		-- f@
		elsif tokens[p][1] = W_FFETCH then
			if fastfloat then
				ADD_CODE({"pop bx","fld qword [bx]"},0)
			else
				ADD_CODE({"pop bp","mov ebx,fpstackptr","fld qword [bp]","add fpstackptr,8","fstp qword [bx]"},0)
			end if		
		-- f@s
		elsif tokens[p][1] = W_FFETCHS then
			if fastfloat then
				ADD_CODE({"pop bx","fld dword [bx]"},0)
			else
				ADD_CODE({"pop bp","mov ebx,fpstackptr","fld dword [bp]","add fpstackptr,8","fstp qword [bx]"},0)
			end if		
		-- +
		elsif tokens[p][1] = W_ADD then
			ADD_CODE({"pop ax","mov bp,sp","add [bp],ax"},0)

		-- -
		elsif tokens[p][1] = W_SUB then
			ADD_CODE({"pop ax","mov bp,sp","sub [bp],ax"},0)

		-- *
		elsif tokens[p][1] = W_MUL then
			ADD_CODE({"pop ax","mov bp,sp","imul word [bp]","mov [bp],ax"},0)
		-- /
		elsif tokens[p][1] = W_DIV then
			ADD_CODE({"pop bx","pop ax","cwd","idiv bx","push ax"},0)

		-- MOD
		elsif tokens[p][1] = W_MOD then
			ADD_CODE({"pop bx","pop ax","cwd","idiv bx","push dx"},0)

		-- /MOD
		elsif tokens[p][1] = W_DIVMOD then
			ADD_CODE({"pop bx","pop ax","cwd","idiv bx","push dx","push ax"},0)

		-- f+
		elsif tokens[p][1] = W_FADD then
			if fastfloat then
				ADD_CODE({"faddp st1,st0"},0)
			else
				ADD_CODE({"mov bx,fpstackptr","fld qword [bx-8]","fadd qword [bx-16]","sub fpstackptr,8","fstp qword [bx-16]"},0)
			end if
		-- f-
		elsif tokens[p][1] = W_FSUB then
			if fastfloat then
				ADD_CODE({"fsubp st1,st0"},0)
			else
				ADD_CODE({"mov bx,fpstackptr","fld qword [bx-8]","fsub qword [bx-16]","sub fpstackptr,8","fstp qword [bx-16]"},0)
			end if
		-- f*
		elsif tokens[p][1] = W_FMUL then
			if fastfloat then
				ADD_CODE({"fmulp st1,st0"},0)
			else
				ADD_CODE({"mov bx,fpstackptr","fld qword [bx-8]","fmul qword [bx-16]","sub fpstackptr,8","fstp qword [bx-16]"},0)
			end if
		-- f/
		elsif tokens[p][1] = W_FDIV then
			if fastfloat then
				ADD_CODE({"fdivp st1,st0"},0)
			else
				ADD_CODE({"mov bx,fpstackptr","fld qword [bx-16]","fdiv qword [bx-8]","sub fpstackptr,8","fstp qword [bx-16]"},0)
			end if
			
		
		-- /C
		elsif tokens[p][1] = W_CSIZE then
			PUSH_dos16(1)

		-- /F
		elsif tokens[p][1] = W_FSIZE then
			PUSH_dos16(8)

		-- /N
		elsif tokens[p][1] = W_NSIZE then
			PUSH_dos16(4)
			
		-- 1-
		elsif tokens[p][1] = W_DEC then
			ADD_CODE({"mov bp,sp","dec word [bp]"},0)
		-- 1+
		elsif tokens[p][1] = W_INC then
			ADD_CODE({"mov bp,sp","inc word [bp]"},0)
			
		-- 2*
		elsif tokens[p][1] = W_MUL2 then
			ADD_CODE({"mov bp,sp","sal word [bp],1"},0)

		-- 2/
		elsif tokens[p][1] = W_DIV2 then
			ADD_CODE({"mov bp,sp","sar word [bp],1"},0)
		
		-- <<
		elsif tokens[p][1] = W_SHL then
			ADD_CODE({"pop cx","mov bp,sp","shl word [bp],cl"},0)
		-- >>
		elsif tokens[p][1] = W_SHR then
			ADD_CODE({"pop cx","mov bp,sp","shr word [bp],cl"},0)
		
		-- BOUNDS
		--elsif tokens[p][1] = W_BOUNDS then
		--	ADD_CODE({"pop ecx","pop eax","lea ecx,[eax+ecx-1]","push ecx","push eax"},0)
		
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
							--puts(1,tokens[p+1][2]&"\n")
							params = {{},{}}
							ADD_CODE({"; "&tokens[p+1][2]},0)
							wordId = make_label(tokens[p+1][2]) 
							ADD_CODE({wordId&":"},0)
							ADD_CODE({"sub word [ipstackptr],2","pop cx","mov bx,[ipstackptr]","mov [bx],cx"},0)
							p += 1
							uses_ebp = 2 -- -1
							lsp = "loopstackptr" --"ebp"
							--ebpCodePos = length(code)+1
							--ADD_CODE({"add miscstackptr,4","mov bx,miscstackptr","mov [bx-4],bp","mov bp,loopstackptr"},0)
							--usesMiscS += 1
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

		
	
		-- ;
		elsif tokens[p][1] = W_SEMICOLON then
			if length(pendingWordDef)=3 then
				usesIP = 1
				--ADD_CODE({wordId&"_return:","sub ipstackptr,2","mov eax,ipstackptr","mov ecx,[ipstack+eax*4]","inc eax","mov ebp,[ipstack+eax*4]","push ecx","ret"},0)			
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
					ADD_CODE({"mov bx,[ipstackptr]","mov cx,[bx+2]","add word [ipstackptr],4","mov bp,[bx]","push cx","ret"},0)
				else
					if uses_ebp = -1 and hasLoops then
						ADD_CODE({"sub miscstackptr,4","mov loopstackptr,ebp","mov esi,miscstackptr","mov ebp,[esi]"},0)
						lsp = "loopstackptr"
					elsif ebpCodePos>0 then
						REMOVE_CODE(ebpCodePos,ebpCodePos+3)
						usesMiscS -= 1
					end if
					ADD_CODE({"mov bx,[ipstackptr]","push word [bx]","add word [ipstackptr],2","retn"},0)
				end if
				userWords = assoc_insert(pendingWordDef[2],wordId,userWords)
				pendingWordDef = {}
				uses_ebp = 0
				ebpCodePos = 0
				hasLoops = 0
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
					if uses_ebp = -1 and hasLoops then
						ADD_CODE({"sub miscstackptr,4","mov loopstackptr,ebp","mov esi,miscstackptr","mov ebp,[esi]"},0)
						lsp = "loopstackptr"
					elsif ebpCodePos>0 then
						REMOVE_CODE(ebpCodePos,ebpCodePos+3)
					end if				
					ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax]","add ipstackptr,4","pop eax","push ecx","ret"},0)
				end if
				userWords = assoc_insert(pendingWordDef[2],wordId,userWords)
				pendingWordDef = {}
				uses_ebp = 0
				ebpCodePos = 0
				hasLoops = 0
			else
				ERROR(ERROR_NO_COLON,tokens[p])
			end if

		-- '
		elsif tokens[p][1] = W_QUOTE then
			n = bsearch(tokens[p+1][2],userWords[1])
			if n then
				ADD_CODE({"push word "&userWords[2][n]},0)
				p += 1
			end if
			
		-- =
		elsif tokens[p][1] = W_EQ then
			CMPI_dos16("z",p)
			
		-- <>
		elsif tokens[p][1] = W_NE then
			CMPI_dos16("nz",p)
		
		-- >
		elsif tokens[p][1] = W_GT then
			CMPI_dos16("g",p)
		-- >=
		elsif tokens[p][1] = W_GE then
			CMPI_dos16("ge",p)

		-- <
		elsif tokens[p][1] = W_LT then
			CMPI_dos16("l",p)
		-- <=
		elsif tokens[p][1] = W_LE then
			--ADD_CODE({"pop eax","xor ecx,ecx","cmp [esp],eax","setle cl","neg ecx","mov [esp],ecx"},0)
			CMPI_dos16("le",p)
			
		-- 0=
		elsif tokens[p][1] = W_EQ0 then
			ADD_CODE({"pop ax","xor cx,cx","sub sp,2","cmp ax,0","setz cl","mov bx,sp","neg cx","mov [bx],cx"},0)
		-- 0>
		elsif tokens[p][1] = W_GT0 then
			ADD_CODE({"pop ax","xor cx,cx","sub sp,2","cmp ax,0","setg cl","mov bx,sp","neg cx","mov [bx],cx"},0)
		-- 0<
		elsif tokens[p][1] = W_LT0 then
			ADD_CODE({"pop ax","xor cx,cx","sub sp,2","cmp ax,0","setl cl","mov bx,sp","neg cx","mov [bx],cx"},0)

		-- f=
		elsif tokens[p][1] = W_FEQ then
			if fastfloat then
				ADD_CODE({"xor cx,cx","fcomip st0,st1",
				          "ffree st0","fincstp","setz cl","neg cx","push cx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-8]","fld qword ptr [eax-16]","sub esp,4","sub fpstackptr,16","fcomip st(0),st(1)",
				          "fstp dword ptr [scratch2]","setz cl","neg ecx","mov [esp],ecx"},0)
			end if
		-- f<>
		elsif tokens[p][1] = W_FNE then
			if fastfloat then
				ADD_CODE({"xor cx,cx","fcomip st0,st1",
				          "ffree st0","setnz cl","fincstp","neg cx","push cx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-8]","fld qword ptr [eax-16]","sub esp,4","sub fpstackptr,16","fcomip st(0),st(1)",
				          "ffree st(0)","setnz cl","fincstp","neg ecx","mov [esp],ecx"},0)
			end if
		-- f>
		elsif tokens[p][1] = W_FGT then
			if fastfloat then
				ADD_CODE({"xor cx,cx","fcomip st0,st1",
				          "ffree st0","setle cl","fincstp","neg ecx","push cx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-16]","sub esp,4","fcomip qword ptr [eax-8]","sub fpstackptr,8","setg cl","neg ecx","mov [esp],ecx"},0)
			end if		
		-- f<
		elsif tokens[p][1] = W_FLT then
			if fastfloat then
				ADD_CODE({"xor cx,cx","fcomip st0,st1",
				          "ffree st0","setg cl","fincstp","neg ecx","push cx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-16]","sub esp,4","fcomip qword ptr [eax-8]","sub fpstackptr,8","setl cl","neg ecx","mov [esp],ecx"},0)
			end if
			
		-- NEGATE
		elsif tokens[p][1] = W_NEG then
			ADD_CODE({"mov bx,sp","neg word [bx]"},0)
			
		-- FNEGATE
		elsif tokens[p][1] = W_FNEG then
			if fastfloat then
				ADD_CODE({"fchs"},0)
			else
				ADD_CODE({"mov bx,fpstackptr","fld qword [bx-8]","fchs","fstp qword [bx-8]"},0)
			end if
			
		-- FLOOR
		elsif tokens[p][1] = W_FLOOR then
			if fastfloat then
				ADD_CODE({"push ax","mov bx,sp","fistp word [bx]","fwait"},0)
			else
				ADD_CODE({"mov bx,fpstackptr","fld qword [bx-8]","mov bp,sp","sub word [fpstackptr,8]","fistp word [bp]"},0)
			end if
			
		-- FLOAT
		elsif tokens[p][1] = W_FLOAT then
			if fastfloat then
				ADD_CODE({"mov bx,sp","fild word [bx]","fwait","add sp,2"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fild dword ptr [esp]","fwait","add fpstackptr,8","add esp,4","fstp qword ptr [eax]"},0)
			end if
			
		-- FSIN
		elsif tokens[p][1] = W_FSIN then
			if fastfloat then
				ADD_CODE({"fsin"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","fsin","fstp qword ptr [eax-8]"},0)
			end if
			
		-- FCOS
		elsif tokens[p][1] = W_FCOS then
			if fastfloat then
				ADD_CODE({"fcos"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","fcos","fstp qword ptr [eax-8]"},0)
			end if
			
		-- ABS
		elsif tokens[p][1] = W_ABS then

		-- ALLOT
		elsif tokens[p][1] = W_ALLOT then
			ADD_CODE({"pop ax","add word [dictptr],ax"},0)
			
		-- AND
		elsif tokens[p][1] = W_AND then
			ADD_CODE({"pop ax","mov bx,sp","and [bx],ax"},0)

		-- BEGIN
		elsif tokens[p][1] = W_BEGIN then
			s = sprintf("@@loop_%04x",loops)
--	puts(1,s&"*\n")
			loops += 1
			loopStack = append(loopStack,s)
			ADD_CODE({s&":"},0)
	
		-- DO
		elsif tokens[p][1] = W_DO then
			s = sprintf("@@loop_%04x",loops)
--	puts(1,s&"\n")
			loops += 1
			loopStack = append(loopStack,s)
			n = 0
			hasLoops = 1
			usesLoops += 1
			
			-- Check if the loop variables can be placed in si/di
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
			
			if n then
				optI = 1
				ADD_CODE({"pop si","add word ["&lsp&"],4","pop di"},0)
			else
				ADD_CODE({"mov bx,["&lsp&"]","pop cx","pop dx","add word ["&lsp&"],4","mov [bx],dx","mov [bx+2],cx"},0)
			end if
			ADD_CODE({s&":"},0)

		-- LOOP
		elsif tokens[p][1] = W_LOOP then
			if optI then
				ADD_CODE({"inc si","cmp si,di","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				ADD_CODE({"mov bx,["&lsp&"]","sub bx,4","inc word [bx+2]","mov cx,[bx]","cmp [bx+2],cx","jne "&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
--	puts(1,loopStack[length(loopStack)]&"_end:\n")
			ADD_CODE({"sub word ["&lsp&"],4"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if

		-- +LOOP
		elsif tokens[p][1] = W_INCLOOP then
			if optI then
				ADD_CODE({"pop ax","add si,ax","cmp si,di","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				ADD_CODE({"mov bx,["&lsp&"]","pop ax","sub bx,4","add [bx+2],ax","mov cx,[bx]","cmp [bx+4],cx","jne "&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
--	puts(1,loopStack[length(loopStack)]&"_end:+\n")
			ADD_CODE({"sub word ["&lsp&"],4"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if

		-- I
		elsif tokens[p][1] = W_I then
			if optI then
				ADD_CODE({"push si"},0)
			else
				ADD_CODE({"mov bx,["&lsp&"]","push word [bx-2]"},0)
			end if
		-- J
		elsif tokens[p][1] = W_J then
			ADD_CODE({"mov bx,"&lsp,"push word [bx-6]"},0)
			
		-- CASE
		elsif tokens[p][1] = W_CASE then
			if case_=-1 then
				case_ = 0
				hasDefault = 0
				caseLbl = sprintf("@@case_%06x",cases)
				ADD_CODE({caseLbl&':'},0)
				ADD_CODE({"pop word [caseVar]"},0)
			else
				ERROR("CASE inside CASE",tokens[p])
			end if
		
		-- OF
		elsif tokens[p][1] = W_OF then
			if case_>=0 and not hasDefault then
				ADD_CODE({"pop ax","cmp ax,caseVar","jne "&caseLbl&sprintf("_%04x_false",case_)},0)
			elsif hasDefault then
				ERROR("OF found after DEFAULT",tokens[p])
			else
				ERROR("OF outside CASE",tokens[p])
			end if
		
		-- ENDOF
		elsif tokens[p][1] = W_ENDOF then
			if case_>=0 then
				ADD_CODE({"jmp "&caseLbl&"_end"},0)
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
			
		-- CONSTANT
		elsif tokens[p][1] = W_CONST then
			if length(tokens)>p then
				if not length(pendingWordDef) then
					if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],constants[1])=0 then
						s = sprintf("const_%07x",length(constants[1]))
						n = length(maincode)
						--constants = assoc_insert(tokens[p+1][2],{s,0,n+1},constants)
						if tokens[p-1][1]=NUMBER then
							if n then
								maincode = maincode[1..n-1]
							else
								maincode = {}
							end if
							n -= 1
							constants = assoc_insert(tokens[p+1][2],{s,0,n+1,tokens[p-1][2]},constants)
						else
							constants = assoc_insert(tokens[p+1][2],{s,0,n+1},constants)
							ADD_CODE({"pop ax","mov word ["&s&"],ax"},0)
						end if
						p += 1
					else
						--puts(1,"\n")
						--for i=1 to length(constants[1]) do
						--	puts(1,constants[1][i]&", ")
						--end for
						ERROR(ERROR_REDECLARED,tokens[p+1])
						p += 1
					end if
				else
					ERROR(ERROR_CONST_INSIDE_WORD,tokens[p])				
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

		-- FCONSTANT
		elsif tokens[p][1] = W_FCONST then
			if length(tokens)>p then
				if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],fconstants[1])=0 then
					usesFP = 1
					s = sprintf("fconst_%07x",length(fconstants[1]))
					fconstants = assoc_insert(tokens[p+1][2],s,fconstants)
					if fastfloat then
						ADD_CODE({"fstp qword ptr ["&s&"]"},0)
					else
						ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","add fpstackptr,8","fstp qword ptr ["&s&"]"},0)
					end if
					p += 1
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
					p += 1
				end if
			else
				ERROR("Unexpected end of file",tokens[p])
			end if
		
		-- VARIABLE
		elsif tokens[p][1] = W_VAR then
			if length(tokens)>p then
				if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],variables[1])=0 then
					usesDict = 1
					s = sprintf("var_%07x",length(variables[1]))
					variables = assoc_insert(tokens[p+1][2],{s,4},variables)
					ADD_CODE({"mov ax,[dictptr]","mov ["&s&"],ax","add word [dictptr],2"},0)
					p += 1
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
					p += 1
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

		-- FVARIABLE
		elsif tokens[p][1] = W_FVAR then
			if length(tokens)>p then
				if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],variables[1])=0 then
					usesDict = 1
					s = sprintf("var_%07x",length(variables[1]))
					variables = assoc_insert(tokens[p+1][2],{s,8},variables)
					ADD_CODE({"mov eax,dictptr","mov "&s&",eax","add dictptr,8"},0)
					p += 1
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
					p += 1
				end if
			else
				ERROR("Unexpected end of file",tokens[p])
			end if

		-- INTERRUPT
		elsif tokens[p][1] = W_INTERRUPT then
			if tokens[p-1][1] = NUMBER then
				if length(pendingWordDef) then
					if length(code)>1 then
						code = code[1..length(code)-1]
					else
						code = {}
					end if
				else
					if length(maincode)>1 then
						maincode = maincode[1..length(maincode)-1]
					else
						maincode = {}
					end if
				end if
				ADD_CODE({sprintf("int 0x%x",tokens[p-1][2])},0)
			else
				ERROR("You suck",tokens[p])
			end if

		-- INP
		elsif tokens[p][1] = W_INP then
			ADD_CODE({"pop dx","in al,dx"},0)
		-- OUTP
		elsif tokens[p][1] = W_OUTP then
			ADD_CODE({"pop dx","pop ax","out dx,al"},0)
			
		-- .
		elsif tokens[p][1] = W_DOT then
			usesConsole = 1
			usesPrint = 1
			ADD_CODE({"pop ax","call near __forthec_print"},0)
			--ADD_CODE({"pop eax",
			-- 	  "invoke wsprintf,ADDR szBuffer,ADDR szFormatInt,eax",
			--	  "mov edi,offset szBuffer",
			--	  "mov al,0",
			--	  "repne scasb",
			--	  "sub edi,offset szBuffer",
			--	  "dec edi",
			--	  "invoke WriteConsole,hOutput,ADDR szBuffer,edi,ADDR lpCharsWritten,NULL"},0)
			  

		-- EMIT
		elsif tokens[p][1] = W_EMIT then
			usesConsole = 1
			ADD_CODE({"pop dx","mov ah,2","int 21h"},0)
			--ADD_CODE({"pop eax",
			-- 	  "invoke wsprintf,ADDR szBuffer,ADDR szFormatChar,eax",
			--	  "invoke WriteConsole,hOutput,ADDR szBuffer,1,ADDR lpCharsWritten,NULL"},0)

		-- ."
		elsif tokens[p][1] = W_DOTSTRING then
			if p<length(tokens) then
				usesConsole = 1
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
				literals = append(literals,{sprintf("lit_%06x",length(literals)),"db \""&s&"$\""})
				ADD_CODE({"mov dx,"&literals[length(literals)][1],"mov ah,9","int 21h"},0)
				--ADD_CODE({sprintf("invoke WriteConsole,hOutput,ADDR "&literals[length(literals)][1]&",%d,ADDR lpCharsWritten,NULL",length(s))},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
		
		-- ".
		elsif tokens[p][1] = W_PUTS then
			usesConsole = 1
			ADD_CODE({"pop dx","mov ah,9","int 0x21"},0)

		-- EXPECT
		elsif tokens[p][1] = W_EXPECT then
			usesConsole = 1
			--ADD_CODE({"pop ecx","pop ebx"},0)
			--ADD_CODE({"invoke ReadConsole,hInput,ebx,ecx,ADDR lpCharsWritten,NULL"},0)

		-- KEY
		elsif tokens[p][1] = W_KEY then
			usesConsole = 1
			ADD_CODE({"mov ah,7","int 0x21"},0)
			
		-- BYE
		elsif tokens[p][1] = W_BYE then
			ADD_CODE({"mov ax,0x4C00","int 0x21"},0)
		
		-- CR
		elsif tokens[p][1] = W_CR then
			usesConsole = 1
			usesCR = 1
			ADD_CODE({"mov dx,sCR","mov ah,9","int 0x21"},0)
		-- NEWLINE
		elsif tokens[p][1] = W_NEWLINE then
			usesConsole = 1
			usesLF = 1
			ADD_CODE({"mov dx,sLF","mov ah,9","int 0x21"},0)

		-- SPACE
		elsif tokens[p][1] = W_SPACE then
			usesConsole = 1
			ADD_CODE({"mov dl,' '","mov ah,2","int 0x21"},0)

		-- SPACES
		elsif tokens[p][1] = W_SPACES then
			usesConsole = 1
			--ADD_CODE({"mov edi,offset szBuffer",
			--	  "pop ecx",
			--	  "mov eax,32",
			--	  "rep stosb",
			--          "invoke WriteConsole,hOutput,ADDR szBuffer,ecx,ADDR lpCharsWritten,NULL"},0)
		
		-- HERE
		--elsif tokens[p][1] = W_HERE then
		--	ADD_CODE({"push dword ptr [dictptr]"},0)
		
		-- ,
		elsif tokens[p][1] = W_COMMA then
			ADD_CODE({"mov ebx,dictptr","pop eax","add dictptr,4","mov [ebx],eax"},0)
		-- c,
		elsif tokens[p][1] = W_CCOMMA then
			ADD_CODE({"mov ebx,dictptr","pop eax","inc dword ptr dictptr","mov [ebx],al"},0)
			
		
		-- CALL
		--elsif tokens[p][1] = W_CALL then
		--	ADD_CODE({"call "&tokens[p+1][2]},0)
		--	p += 1
		
		-- CALLI
		elsif tokens[p][1] = W_CALLI then
			ADD_CODE({"pop ax","call near ax"},0)
			
		-- DROP
		elsif tokens[p][1] = W_DROP then
			ADD_CODE({"pop ax"},0)
			
		-- DUP
		elsif tokens[p][1] = W_DUP then
			ADD_CODE({"mov bx,sp","push word [bx]"},0)

		-- PICK
		elsif tokens[p][1] = W_PICK then
			ADD_CODE({"pop ax","mov bx,sp","lea bx,[bx+ax*2]","push word [bx]"},0)
			
		-- NIP
		elsif tokens[p][1] = W_NIP then
			ADD_CODE({"pop ax","pop bx","push ax"},0)
			
		-- TUCK
		elsif tokens[p][1] = W_TUCK then
			ADD_CODE({"pop ax","pop bx","push ax","push bx","push ax"},0)
		
		-- FDUP
		elsif tokens[p][1] = W_FDUP then
			if fastfloat then
				ADD_CODE({"fld st(0)"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","add fpstackptr,8","fstp qword ptr [eax]"},0)
			end if
			
		-- FDROP
		elsif tokens[p][1] = W_FDROP then
			ADD_CODE({"ffree st0","fincstp"},0)
			if not fastfloat then
				ADD_CODE({"sub fpstackptr,8"},0)
			end if
			
		-- FSWAP
		elsif tokens[p][1] = W_FSWAP then
			if fastfloat then
				ADD_CODE({"fxch"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]",
				          "fld qword ptr [eax-16]","fstp qword ptr [eax-8]","fstp qword ptr [eax-16]"},0)
			end if

		-- FOVER
		elsif tokens[p][1] = W_FOVER then
			if fastfloat then
				ADD_CODE({"fld st1"},0)
			else
			--	ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]",
			--	          "fld qword ptr [eax-16]","fstp qword ptr [eax-8]","fstp qword ptr [eax-16]"},0)
			end if
			
		-- OVER
		elsif tokens[p][1] = W_OVER then
			ADD_CODE({"mov bx,sp","push word [bx+2]"},0)
			
		-- ELSE
		elsif tokens[p][1] = W_ELSE then
			if length(ifStack) and and_bits(length(ifStack),1)=0 then
				ADD_CODE({"jmp "&ifStack[2]},0)
				ADD_CODE({ifStack[1]&":"},0)
				ifStack = ifStack[2..length(ifStack)]
			else
				ERROR("Unmatched ELSE",tokens[p])
			end if
		
		-- EXIT
		elsif tokens[p][1] = W_EXIT then
			if length(pendingWordDef) then
				ADD_CODE({"jmp "&wordId&"_return"},0)
			else
				ERROR("Malplaced EXIT",tokens[p])
			end if
		
		-- CMOVE
		elsif tokens[p][1] = W_CMOVE then
			if optI then
				ADD_CODE({"mov ebx,esi","mov edx,edi","pop ecx","pop edi","pop esi","rep movsb","mov edi,edx","mov esi,ebx"},0)
			else
				ADD_CODE({"pop ecx","pop edi","pop esi","rep movsb"},0)
			end if
			
		-- ERASE
		elsif tokens[p][1] = W_ERASE then
			if optI then
				ADD_CODE({"mov ebx,edi","pop ecx","pop edi","xor eax,eax","rep stosb","mov edi,ebx"},0)
			else
				ADD_CODE({"pop ecx","pop edi","xor eax,eax","rep stosb"},0)
			end if
			
		-- FILL
		elsif tokens[p][1] = W_FILL then
			if optI then
				ADD_CODE({"mov bx,di","pop ax","pop cx","pop di","push es","push ds","pop es","rep stosb","pop es","mov di,bx"},0)
			else
				ADD_CODE({"pop ax","pop cx","pop di","mov bx,es","push ds","pop es","rep stosb","mov es,bx"},0)
			end if

		-- FFILLS
		elsif tokens[p][1] = W_FFILLS then
			if optI then
				ADD_CODE({"mov ebx,edi","pop eax","pop ecx","pop edi","rep stosb","mov edi,ebx"},0)
			else
				if fastfloat then
					ADD_CODE({"fstp dword ptr [scratch1]","pop ecx","pop edi","mov eax,dword ptr [scratch1]","rep stosd"},0)
				else
					ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","sub fpstackptr,8","fstp dword ptr [scratch1]","pop ecx","pop edi","mov eax,dword ptr [scratch1]","rep stosd"},0)
				end if
			end if
			
		-- FPOP
		elsif tokens[p][1] = W_FPOP then
			if fastfloat then
				ADD_CODE({"sub esp,8","fstp qword ptr [esp]"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","sub esp,8","fld qword ptr [eax-8]","sub fpstackptr,8","fstp qword ptr [esp]"},0)
			end if
			
		-- FPOPS
		elsif tokens[p][1] = W_FPOPS then
			if fastfloat then
				ADD_CODE({"sub esp,4","fstp dword ptr [esp]"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","sub esp,4","fld qword ptr [eax-8]","sub fpstackptr,8","fstp dword ptr [esp]"},0)
			end if
			
		-- IF
		elsif tokens[p][1] = W_IF then
			s = sprintf("@@if_%06x",ifs)
			ifStack = {s&"_false",s&"_true"}&ifStack
			ifs += 1
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&ifStack[1]},0)
				fastCmp = ""
			else
				ADD_CODE({"pop ax","cmp ax,-1","jne "&ifStack[1]},0)
			end if
			
		-- LEAVE
		elsif tokens[p][1] = W_LEAVE then
			ADD_CODE({"jmp "&loopStack[length(loopStack)]&"_end"},0)
		-- ?LEAVE
		elsif tokens[p][1] = W_LEAVETRUE then
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&loopStack[length(loopStack)]&"_end"},0)
				fastCmp = ""
			else
				ADD_CODE({"pop ax","cmp ax,-1","jz "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- NULL
		elsif tokens[p][1] = W_NULL or tokens[p][1]=W_FALSE then
			PUSH_dos16(0)
		-- TRUE
		elsif tokens[p][1] = W_TRUE then
			PUSH_dos16(-1)
		-- BL
		elsif tokens[p][1] = W_BL then
			PUSH_dos16(' ')
		
		-- OR
		elsif tokens[p][1] = W_OR then
			ADD_CODE({"pop ax","mov bx,sp","or [bx],ax"},0)

		elsif tokens[p][1] = W_EMMS then
			ADD_CODE({"emms"},0)
			for i=1 to length(rstack) do
				free_reg(MMX_REG,rstack[i])
			end for
			rstack = {}
			
		elsif tokens[p][1] = W_PDUP then
			n = alloc_reg(MMX_REG)
			ADD_CODE({sprintf("movq mm(%d),mm(%d)",{n,rstack[1]})},0)
			rstack = n&rstack

		elsif tokens[p][1] = W_PDROP then
			n = rstack[1]
			free_reg(MMX_REG,n)
			if length(rstack)=1 then
				rstack = {}
			else
				rstack = rstack[2..length(rstack)]
			end if
		
		elsif tokens[p][1] = W_PFETCH then
			n = alloc_reg(MMX_REG)
			rstack = n&rstack
			ADD_CODE({"pop eax",sprintf("movd mm(%d),dword ptr [eax]",n)},0)

		elsif tokens[p][1] = W_PSTORE then
			n = rstack[1]
			ADD_CODE({"pop eax",sprintf("movd dword ptr [eax],mm(%d)",n)},0)
			free_reg(MMX_REG,n)
			if length(rstack)=1 then
				rstack = {}
			else
				rstack = rstack[2..length(rstack)]
			end if

		elsif tokens[p][1] = W_PWMUL then
			ADD_CODE({sprintf("pmullw mm(%d),mm(%d)",{rstack[2],rstack[1]})},0)
			free_reg(MMX_REG,rstack[1])
			if length(rstack)=1 then
				rstack = {}
			else
				rstack = rstack[2..length(rstack)]
			end if
			
		elsif tokens[p][1] = W_PCADD then
			ADD_CODE({sprintf("paddb mm(%d),mm(%d)",{rstack[2],rstack[1]})},0)
			free_reg(MMX_REG,rstack[1])
			if length(rstack)=1 then
				rstack = {}
			else
				rstack = rstack[2..length(rstack)]
			end if
		elsif tokens[p][1] = W_PWADD then
			ADD_CODE({sprintf("paddw mm(%d),mm(%d)",{rstack[2],rstack[1]})},0)
			free_reg(MMX_REG,rstack[1])
			if length(rstack)=1 then
				rstack = {}
			else
				rstack = rstack[2..length(rstack)]
			end if
			
		elsif tokens[p][1] = W_PC2PW then
			n = alloc_reg(MMX_REG)
			ADD_CODE({sprintf("pxor mm(%d),mm(%d)",n),sprintf("punpcklbw mm(%d),mm(%d)",{rstack[1],n})},0)
			free_reg(MMX_REG,n)
			
		elsif tokens[p][1] = W_PW2PC then
			n = alloc_reg(MMX_REG)
			ADD_CODE({"mov eax,-1",sprintf("packuswb mm(%d),mm(%d)",{rstack[1],rstack[1]}),sprintf("movd mm(%d),eax",n),
			          sprintf("pand mm(%d),mm(%d)",{rstack[1],n})},0)
			free_reg(MMX_REG,n)
			
		-- REPEAT
		elsif tokens[p][1] = W_REPEAT then
			ADD_CODE({"jmp "&loopStack[length(loopStack)]},0)
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			if length(loopStack) = 1 then
				loopStack = {}
			else
				loopStack = loopStack[1..length(loopStack)-1]
			end if
		
		-- ROT
		elsif tokens[p][1] = W_ROT then
			ADD_CODE({"pop ax","pop cx","pop dx","push cx","push dx","push ax"},0)

		-- SWAP
		elsif tokens[p][1] = W_SWAP then
			ADD_CODE({"pop ax","pop cx","push ax","push cx"},0)
			
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
				ADD_CODE({"pop ax","cmp ax,-1","jnz "&loopStack[length(loopStack)]},0)
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
				ADD_CODE({"pop ax","cmp ax,-1","jnz "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- XOR
		elsif tokens[p][1] = W_XOR then
			ADD_CODE({"pop ax","mov bx,sp","xor word [bx],ax"},0)
		
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
				literals = append(literals,{sprintf("lit_%06x",length(literals)),"db \""&s&"\",0"})
				ADD_CODE({"push word "&literals[length(literals)][1]},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if

		-- $"
		elsif tokens[p][1] = W_DOLLARSTRING then
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
				literals = append(literals,{sprintf("lit_%06x",length(literals)),"db \""&s&"$\""})
				ADD_CODE({"push word "&literals[length(literals)][1]},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		elsif tokens[p][1] = UNKNOWN then
			n = bsearch(tokens[p][2],userWords[1])
			if n then
				ADD_CODE({"call near "&userWords[2][n]},0)
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
					ADD_CODE({"push word ["&variables[2][n][1]&"]"},0)
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],constants[1])
				if n then
					if length(constants[2][n])=3 then
						ADD_CODE({"push word ["&constants[2][n][1]&"]"},0)
						constants[2][n][2] = 1	-- Mark the constant as being used
					else
						PUSH_dos16(constants[2][n][4])
					end if
				end if
			end if

			if not n then
				n = bsearch(tokens[p][2],fconstants[1])
				if n then
					ADD_CODE({"fld qword ["&fconstants[2][n]&"]"},0)
					if not fastfloat then
						ADD_CODE({"mov eax,fpstackptr","fstp qword ptr [eax]","add fpstackptr,8"},0)
					end if
				end if
			end if
		
			if not n then
				n = bsearch(tokens[p][2],deferred[1])
				if n then
					ADD_CODE({"call near "&deferred[2][n]&" ; deferred"},0)
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
	
	--usesLoops += hasLoops
	usesCase += cases

	if length(pendingWordDef) then
		ERROR(ERROR_OPEN_WORD,pendingWordDef)
	end if
	
	if length(loopStack) then
		--? loopStack
		ERROR(ERROR_OPEN_DO,tokens[p-1])
	end if
	
	return 0
end function





global procedure forthec_dos16()
	sequence nasmdir
	integer nofiles
	
	loops = 0
	cases = 0
	ifs = 0
	usesConsole = 0
	usesCR = 0
	usesLF = 0
	usesLoops = 0
	usesFP = 0
	usesIP = 0
	usesDict = 0
	usesMiscS = 0
	usesCase = 0
	usesPrint = 0
	fastfloat = 1
	verbose = 0
	noMangle = 0
	optLevel = 6
	nasmdir = "c:\\nasm"
	labelPrefix = "@@"


	nofiles = 1
	if length(CMD)>=4 then
		if not (find("-t",CMD)>0 and length(CMD)=4) then
			nofiles = 0
		end if
	end if
	
	if nofiles then
		puts(1,"forthec -t dos16 [options] <infile> <outfile>\n\n")
		puts(1,"Options:\n\n")
		puts(1,"-O0\tTurn optimisations off\n")
		puts(1,"-O1\tRemove unused constants\n")
		puts(1,"-O2\tO1 + remove redundant integer instructions\n")
		puts(1,"-O3\tO2 + remove redundant fpu instructions\n")
		puts(1,"-O4\tO3 + optimise loops and branches\n")
		puts(1,"-O5\tO4 + additional integer optimisations\n")
		puts(1,"-f\tUse alternative floating point code\n")
		puts(1,"-dll\tInsert DLL initialisation code when target is .asm\n")
		puts(1,"-nm\tNo name mangling (default when target is .dll)\n")	
		puts(1,"-res\tUse a resource script\n")
		puts(1,"-v\tVerbose compilation\n")
		puts(1,"\nPress any key to quit..")
		while get_key()=-1 do end while
		abort(0)
	end if

	-- Check options
	if find("-v",CMD) then
		verbose = 1
	end if
	if find("-f",CMD) then
		fastfloat = 0
	end if
	if find("-nm",CMD) then
		noMangle = 1
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
	elsif find("-O6",CMD) then
		optLevel = 6
	end if


	-- Read cfg
	cfgfile = open("forthec.cfg","r")
	if cfgfile != -1 then
		cfgin = get(cfgfile)
		if cfgin[1]=GET_SUCCESS then
			if equal(cfgin[2][1],"NASMDIR") then
				nasmdir = cfgin[2][2]
			end if
		end if
		close(cfgfile)
	end if


	outname = CMD[length(CMD)]
	outname = cut_filename(outname)
	outname[2] = lower(outname[2])

	outfile = open(outname[1]&".asm","wb")

	code = {}
	ifStack = {}
	maincode = {}
	deferred = {{},{}}
	literals = {}
	includes = {} --{"windows.inc","kernel32.inc","user32.inc","kernel32.lib","user32.lib"}
	pendingWordDef = {}
	tokens = {}
	userWords = {{},{}}
	constants = {{},{}}
	fconstants = {{},{}}
	variables = {{},{}}
	regs = repeat({0,0,0,0,0,0,0,0},2)
	rstack = {}

	forthec_init()
	
	t1 = time()
	if parse(CMD[length(CMD)-1]) then end if

	if optLevel>0 and not noFold then
		optimise_token_stream()
	end if
	
	count_refs()
	
	if compile_dos16() then end if

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
		maincode = optimise_dos16(maincode,1)
		code = optimise_dos16(code,0)
	end if


	puts(outfile,"; Generated by ForthEC"&NL&NL)
	-- Write header
	puts(outfile,"[org 0x100]"&NL&NL)
	for i=1 to length(includes) do
		if equal(lower(includes[i][length(includes[i])-2..length(includes[i])]),"lib") then
			puts(outfile,"includelib "&includes[i]&NL)
		else
			puts(outfile,"include "&includes[i]&NL)
		end if
	end for


	puts(outfile,NL&";##############################################################################"&NL&NL)

	puts(outfile,"__start:"&NL)

	if usesFP then
		puts(outfile,"mov word [fpstackptr],fpstack"&NL)
	end if
	if usesIP then
		puts(outfile,"mov word [ipstackptr],fpstack+0x200+0x400"&NL)
	end if
	if usesMiscS>0 then
		puts(outfile,"mov word [miscstackptr],fpstack+0x200+0x400"&NL)
	end if
	if usesLoops then
		puts(outfile,"mov word [loopstackptr],fpstack+0x200+0x400+0x100"&NL)
	end if
	if usesDict then
		puts(outfile,"mov word [dictptr],fpstack+0x800"&NL&NL)
	end if

	for i=1 to length(maincode) do
		if length(maincode[i]) then
			puts(outfile,maincode[i]&NL)
		end if
	end for

	puts(outfile,";##############################################################################"&NL&NL)

	-- Write code section
	for i=1 to length(code) do
		puts(outfile,code[i]&NL)
	end for

	--puts(outfile,NL&"END __start"&NL)

	if usesPrint then
	puts(outfile,"__forthec_print:"&NL&
		"mov bx,printBuf+16"&NL&
		"push ax"&NL&
		"test ax,ax"&NL&
		"jns __fp_us"&NL&
		"neg ax"&NL&
		"__fp_us:"&NL&
		"mov cx,10"&NL&
		"__fp_loop:"&NL&
		"\tcwd"&NL&
		"\tdec bx"&NL&	
		"\tdiv cx"&NL&
		"\tadd dl,'0'"&NL&
		"\tmov [bx],dl"&NL&
		"\ttest ax,ax"&NL&
		"\tjnz __fp_loop"&NL&
		"pop ax"&NL&
		"test ax,ax"&NL&
		"jns __fp_us2"&NL&
		"dec bx"&NL&
		"mov byte [bx],'-'"&NL&
		"__fp_us2:"&NL&
		"mov dx,bx"&NL&
		"mov ah,9"&NL&
		"int 21h"&NL&
		"retn"&NL)
	end if

	puts(outfile,NL&";##############################################################################"&NL&NL)


	-- Write data section
	--puts(outfile,".data"&NL&"\tszFormatInt db \"%d \",0"&NL&"\tszFormatChar db \"%hc\",0"&NL&"\tszBuffer db 256 dup(0)"&NL)
	if usesCR or usesLF then
		puts(outfile,"\tsCR db 13,'$'"&NL&"\tsLF db 10,'$'"&NL)
	end if
	for i=1 to length(literals) do
		puts(outfile,"\t"&literals[i][1]&" "&literals[i][2]&NL)
	end for

	-- Write data? section
	--puts(outfile,NL&"\tstackBase dw 0"&NL&"\tdictptr dw 0"&NL)
	if usesDict then
		puts(outfile,"\tdictptr dw 0"&NL)
	end if
	if usesIP then
		puts(outfile,"\tipstackptr dw 0"&NL)
	end if
	if usesFP then
		puts(outfile,"\tfpstackptr dw 0"&NL)
	end if
	if usesLoops then
		puts(outfile,"\tloopstackptr dw 0"&NL)
	end if
	if usesMiscS>0 then
		puts(outfile,"\tmiscstackptr dw 0"&NL)
	end if
	if usesCase then
		puts(outfile,"\tcaseVar dw 0"&NL)
	end if
	if usesPrint then
		puts(outfile,"\tprintBuf times 16 db 0"&NL&"\tdb \" $\""&NL)
	end if
	--puts(outfile,"\tscratch1 dw 0"&NL&"\tscratch2 dw 0"&NL&"\treturnaddress dw 0"&NL)

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

	for i=1 to length(constants[2]) do
		if constants[2][i][2] or optLevel=0 then
			puts(outfile,"\t"&constants[2][i][1]&" dw 0"&NL)
		--elsif not constants[2][i][2] then
		--	constRemoved += 1
		end if
	end for

	for i=1 to length(fconstants[2]) do
		puts(outfile,"\t"&fconstants[2][i]&" dq 0"&NL)
	end for

	for i=1 to length(variables[2]) do
		--if variables[2][i][2] = 4 then
			puts(outfile,"\t"&variables[2][i][1]&" dw 0"&NL)
		--else
		--	puts(outfile,"\t"&variables[2][i][1]&" dq ?"&NL)
		--end if
	end for
	puts(outfile,"fpstack:"&NL)

	close(outfile)

	if verbose then
		printf(1,"Done\nRemoved %d unused constants and %d redundant instructions\nTotal compilation time: %1.2f seconds\n\n",{constRemoved,linesRemoved,time()-t1})
	end if


	if equal(outname[2],"com") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system(nasmdir&"\\nasm -o "&outname[1]&".com "&outname[1]&".asm",2)
		system("del "&outname[1]&".asm",2)
	--elsif equal(outname[2],"obj") then
	--	if verbose then
	--		puts(1,"\nAssembling\n")
	--	end if
	--	system("ml /c /coff /I"&masmdir&"\\include /Fo"&outname[1]&".obj "&outname[1]&".asm",2)
	end if




	if verbose then
		puts(1,"\nPress any key..")
		while get_key() = -1 do end while
		
	end if
end procedure




