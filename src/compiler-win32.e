-- ForthEC : A Forth compiler
-- /Mic, 2004/2005
--
-- 2005-04-10
-- * Better support for DLL output target
--
-- 2004-12-15
-- * Added some SIMD constructs.
-- * Optimised the FPU code (limits FPU stack depth to 8 levels).
-- * Added additional loop optimisations.
-- * Fixed some bugs (FPU compares i.a)
--
-- 2004-12-02
-- * Added more optimisation patterns.
-- * Most integer constants are now completly removed and replaced by immediate values.
-- * FPU code optimisations are now aborted after the first pass if no optimisations are
--   found (eg. if no FPU code is used).
--
-- 2004-11-29
-- * Added additional optimisation based on pattern matching.
--
-- 2004-04-18
-- * Added loop and conditional branch optimisation.
--
-- 2004-04-17
-- * Fixed a bug in loop and +loop.
-- * Implemented more words.
--
-- 2004-04-16
-- * Now removes duplicate floating point literals.
-- * Added floating point instruction optimisation.
-- * Split the compiler into modules.
--
-- 2004-04-15
-- * Added a simple code optimiser.
-- * Implemented more words.
--
-- 2004-04-14
-- * Added code to check if the file handle is valid in compile().
-- * Fixed a bug with nested while loops.


without warning
include forthec.e
include parser.e
include optimiser-win32.e



-- Push a value on the parameter stack
procedure PUSH_win32(atom a)
	ADD_CODE({"sub esp,4",sprintf("mov dword ptr [esp],%d",a)},0)
end procedure


-- Handle comparison operations
procedure CMPI_win32(sequence cond,integer p)
	integer n
	
	n = 0
	if p<length(tokens) and optLevel>=4 then
		n = tokens[p+1][1]
		if n=W_IF or n=W_WHILE then
			fastCmp = "jn"&cond
			if equal(fastCmp,"jnnz") then
				fastCmp = "jz"
			end if
			ADD_CODE({"pop eax","pop ecx","cmp ecx,eax"},0)
		elsif n=W_LEAVETRUE then
			fastCmp = "j"&cond
			ADD_CODE({"pop eax","pop ecx","cmp ecx,eax"},0)
		else
			n = 0
		end if
	end if
	if not n then
		ADD_CODE({"pop eax","xor ecx,ecx","cmp [esp],eax","set"&cond&" cl","neg ecx","mov [esp],ecx"},0)
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



function compile_win32()
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
			PUSH_win32(tokens[p][2])
		
		elsif tokens[p][1] = FNUMBER then
			t = sprintf("real8 %f",tokens[p][2])
			if equal(t,"real8 1.000000") then
				n = -2
			elsif equal(t,"real8 0.000000") then
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
						ADD_CODE({"fld qword ptr ["&literals[n][1]&"]"},0)
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
						ADD_CODE({"fld qword ptr ["&sprintf("lit_%06x",length(literals))&"]"},0)
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
			ADD_CODE({"push dword ptr [stackBase]"},0)
		
		-- SP@
		elsif tokens[p][1] = W_SPTOP then
			ADD_CODE({"push esp"},0)
			
		-- !
		elsif tokens[p][1] = W_STORE then
			ADD_CODE({"mov eax,[esp]","mov ecx,[esp+4]","mov [eax],ecx","add esp,8"},0)

		-- c!
		elsif tokens[p][1] = W_CSTORE then
			ADD_CODE({"mov eax,[esp]","mov ecx,[esp+4]","mov [eax],cl","add esp,8"},0)
			--ADD_CODE({"pop eax","pop ecx","mov [eax],cl"},0)
		-- w!
		elsif tokens[p][1] = W_WSTORE then
			ADD_CODE({"pop ecx","pop eax","mov [ecx],ax"},0)

		-- f!
		elsif tokens[p][1] = W_FSTORE then
			if fastfloat then
				ADD_CODE({"pop eax","fstp qword ptr [eax]"},0)
			else
				ADD_CODE({"mov ebx,fpstackptr","pop eax","fld qword ptr [ebx-8]","sub fpstackptr,8","fstp qword ptr [eax]"},0)
			end if
			
		-- f!s
		elsif tokens[p][1] = W_FSTORES then
			if fastfloat then
				ADD_CODE({"pop eax","fstp dword ptr [eax]"},0)
			else
				ADD_CODE({"pop eax","mov ebx,fpstackptr","fld qword ptr [ebx-8]","sub fpstackptr,8","fstp dword ptr [eax]"},0)
			end if
			
		-- !r+
		elsif tokens[p][1] = W_STORE_R_INC then
			ADD_CODE({"pop eax","mov ecx,[esp]","mov [ecx],eax","add dword ptr [esp],4"},0)

		-- ON
		elsif tokens[p][1] = W_ON then
			ADD_CODE({"pop eax","mov dword ptr [eax],-1"},0)
		-- OFF
		elsif tokens[p][1] = W_OFF then
			ADD_CODE({"pop eax","mov dword ptr [eax],0"},0)
			
		-- @
		elsif tokens[p][1] = W_FETCH then
			ADD_CODE({"pop eax","mov ebx,[eax]","push ebx"},0)
		-- c@
		elsif tokens[p][1] = W_CFETCH then
			ADD_CODE({"pop eax","movzx ebx,byte ptr [eax]","push ebx"},0)
		-- cs@
		elsif tokens[p][1] = W_CSFETCH then
			ADD_CODE({"pop eax","movsx ebx,byte ptr [eax]","push ebx"},0)
		-- w@
		elsif tokens[p][1] = W_WFETCH then
			ADD_CODE({"pop eax","movzx ebx,word ptr [eax]","push ebx"},0)
	
		-- a@
		elsif tokens[p][1] = W_ACCFETCH then
			ADD_CODE({"push eax"},0)
			
		-- f@
		elsif tokens[p][1] = W_FFETCH then
			if fastfloat then
				ADD_CODE({"pop eax","fld qword ptr [eax]"},0)
			else
				ADD_CODE({"pop eax","mov ebx,fpstackptr","fld qword ptr [eax]","add fpstackptr,8","fstp qword ptr [ebx]"},0)
			end if		
		-- f@s
		elsif tokens[p][1] = W_FFETCHS then
			if fastfloat then
				ADD_CODE({"pop eax","fld dword ptr [eax]"},0)
			else
				ADD_CODE({"pop eax","mov ebx,fpstackptr","fld dword ptr [eax]","add fpstackptr,8","fstp qword ptr [ebx]"},0)
			end if		
		-- +
		elsif tokens[p][1] = W_ADD then
			ADD_CODE({"pop eax","add [esp],eax"},0)

		-- -
		elsif tokens[p][1] = W_SUB then
			ADD_CODE({"pop eax","sub [esp],eax"},0)

		-- *
		elsif tokens[p][1] = W_MUL then
			ADD_CODE({"pop eax","imul dword ptr [esp]","mov [esp],eax"},0)
		-- /
		elsif tokens[p][1] = W_DIV then
			ADD_CODE({"pop ebx","pop eax","cdq","idiv ebx","push eax"},0)

		-- MOD
		elsif tokens[p][1] = W_MOD then
			ADD_CODE({"pop ebx","pop eax","cdq","idiv ebx","push edx"},0)

		-- /MOD
		elsif tokens[p][1] = W_DIVMOD then
			ADD_CODE({"pop ebx","pop eax","cdq","idiv ebx","push edx","push eax"},0)

		-- f+
		elsif tokens[p][1] = W_FADD then
			if fastfloat then
				ADD_CODE({"faddp st(1),st(0)"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","fadd qword ptr [eax-16]","sub fpstackptr,8","fstp qword ptr [eax-16]"},0)
			end if
		-- f-
		elsif tokens[p][1] = W_FSUB then
			if fastfloat then
				ADD_CODE({"fsubp st(1),st(0)"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","fsub qword ptr [eax-16]","sub fpstackptr,8","fstp qword ptr [eax-16]"},0)
			end if
		-- f*
		elsif tokens[p][1] = W_FMUL then
			if fastfloat then
				ADD_CODE({"fmulp st(1),st(0)"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","fmul qword ptr [eax-16]","sub fpstackptr,8","fstp qword ptr [eax-16]"},0)
			end if
		-- f/
		elsif tokens[p][1] = W_FDIV then
			if fastfloat then
				ADD_CODE({"fdivp st(1),st(0)"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-16]","fdiv qword ptr [eax-8]","sub fpstackptr,8","fstp qword ptr [eax-16]"},0)
			end if
			
		
		-- /C
		elsif tokens[p][1] = W_CSIZE then
			PUSH_win32(1)

		-- /F
		elsif tokens[p][1] = W_FSIZE then
			PUSH_win32(8)

		-- /N
		elsif tokens[p][1] = W_NSIZE then
			PUSH_win32(4)
			
		-- 1-
		elsif tokens[p][1] = W_DEC then
			ADD_CODE({"dec dword ptr [esp]"},0)
		-- 1+
		elsif tokens[p][1] = W_INC then
			ADD_CODE({"inc dword ptr [esp]"},0)
			
		-- 2*
		elsif tokens[p][1] = W_MUL2 then
			ADD_CODE({"sal dword ptr [esp],1"},0)

		-- 2/
		elsif tokens[p][1] = W_DIV2 then
			ADD_CODE({"sar dword ptr [esp],1"},0)
		
		-- <<
		elsif tokens[p][1] = W_SHL then
			ADD_CODE({"pop ecx","shl dword ptr [esp],cl"},0)
		-- >>
		elsif tokens[p][1] = W_SHR then
			ADD_CODE({"pop ecx","shr dword ptr [esp],cl"},0)
		
		-- BOUNDS
		elsif tokens[p][1] = W_BOUNDS then
			ADD_CODE({"pop ecx","pop eax","lea ecx,[eax+ecx-1]","push ecx","push eax"},0)
		
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
							ADD_CODE({"; "&tokens[p+1][2]},0)
							wordId = make_label(tokens[p+1][2]) 
							ADD_CODE({wordId&":"},0)
							ADD_CODE({"sub ipstackptr,4","pop ecx","mov eax,ipstackptr","mov [eax],ecx"},0)
							p += 1
							uses_ebp = -1
							lsp = "ebp"
							ebpCodePos = length(code)+1
							ADD_CODE({"add miscstackptr,4","mov eax,miscstackptr","mov [eax-4],ebp","mov ebp,loopstackptr"},0)
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

		-- :w
		elsif tokens[p][1] = W_COLONWIN then
			if p<length(tokens) then
				if tokens[p+1][1]=UNKNOWN then
					if not bsearch(tokens[p+1][2],userWords[1]) then  --WORDS) then
						if not length(pendingWordDef) then
							pendingWordDef = tokens[p+1]&0	-- append 0 to distinguise from a normal :
							params = {{},{}}
							wordId = make_label(tokens[p+1][2]) 
							ADD_CODE({wordId&":"},0)
							--ADD_CODE({"mov eax,ipstackptr","mov [eax],ecx","mov [eax+4],ebx","mov [eax+8],esi","pop ebx","add ipstackptr,20","mov [eax+12],edi","mov [eax+16],ebx"},0)
							ADD_CODE({"mov eax,ipstackptr",
							          "xchg eax,esp",		-- eax=esp(old)(real), esp=ipstackptr
							          "push dword ptr [eax]",	-- esp=ipstackptr-4, [esp]=eip(old)
							          "pusha",			
							          "xchg eax,esp",		
							          "mov ipstackptr,eax",
							          "pop eax"			-- pop eip off the stack
							          },0)
							p += 1
							uses_ebp = 1
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
						--printf(1,"%d\n",tokens[p][1])
						--puts(1,tokens[p][2]&"\n")
						ERROR("Syntax error",tokens[p])
					end if
					p += 1
				end while
				if uses_ebp=-1 then
					REMOVE_CODE(ebpCodePos,ebpCodePos+3)
				end if
				uses_ebp = 1
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
					if uses_ebp = -1 and hasLoops then
						ADD_CODE({"sub miscstackptr,4","mov loopstackptr,ebp","mov esi,miscstackptr","mov ebp,[esi]"},0)
						lsp = "loopstackptr"
					elsif ebpCodePos>0 then
						REMOVE_CODE(ebpCodePos,ebpCodePos+3)
					end if
					ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax]","add ipstackptr,4","push ecx","ret"},0)
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
				ERROR("Use of ;@ with no corresponding :",tokens[p])
			end if

		-- ;w
		elsif tokens[p][1] = W_SEMICOLONWIN then
			if length(pendingWordDef)=4 then
				ADD_CODE({wordId&"_return:"},0)
				if length(params[1]) then
					--ADD_CODE({"mov ecx,ipstackptr","sub ipstackptr,24","mov edi,[ecx-12]","mov ebp,[ecx-4]","mov esi,[ecx-16]","mov ebx,[ecx-20]","pop eax","push dword ptr [ecx-8]","mov ecx,[ecx-24]"},0)
					ADD_CODE({"mov eax,ipstackptr",
						  "xchg eax,esp",	-- eax=esp(real)(curr), esp=ipstackptr
						  "add ipstackptr,36",
						  "popa",		-- eax=esp(real)(old), esp=ipstackptr(old)
						  "mov esp,[esp]",	-- esp=eip(old)
						  "xchg eax,esp",	-- eax=eip(old), esp=esp(real)(old)
						  "xchg eax,[esp]"	-- eax=retval, [esp]=eip(old)
						  },0)
				else
					ADD_CODE({"mov eax,ipstackptr","mov ecx,[eax-4]","sub ipstackptr,20","mov edi,[eax-8]","mov esi,[eax-12]","mov ebx,[eax-16]","pop eax","push ecx"},0)
				end if
				ADD_CODE({sprintf("ret %d",length(params[1])*4)},0)
				userWords = assoc_insert(pendingWordDef[2],wordId,userWords)
				pendingWordDef = {}
				uses_ebp = 0
				ebpCodePos = 0
			else
				ERROR("Use of ;w with no corresponding :w",tokens[p])
			end if
			
		-- '
		elsif tokens[p][1] = W_QUOTE then
			n = bsearch(tokens[p+1][2],userWords[1])
			if n then
				ADD_CODE({"mov eax,offset "&userWords[2][n],"push eax"},0)
				p += 1
			end if
			
		-- =
		elsif tokens[p][1] = W_EQ then
			CMPI_win32("z",p)
			
		-- <>
		elsif tokens[p][1] = W_NE then
			CMPI_win32("nz",p)
		
		-- >
		elsif tokens[p][1] = W_GT then
			CMPI_win32("g",p)
		-- >=
		elsif tokens[p][1] = W_GE then
			CMPI_win32("ge",p)

		-- <
		elsif tokens[p][1] = W_LT then
			CMPI_win32("l",p)
		-- <=
		elsif tokens[p][1] = W_LE then
			--ADD_CODE({"pop eax","xor ecx,ecx","cmp [esp],eax","setle cl","neg ecx","mov [esp],ecx"},0)
			CMPI_win32("le",p)
			
		-- 0=
		elsif tokens[p][1] = W_EQ0 then
			ADD_CODE({"pop eax","xor ecx,ecx","sub esp,4","cmp eax,0","setz cl","neg ecx","mov [esp],ecx"},0)
		-- 0>
		elsif tokens[p][1] = W_GT0 then
			ADD_CODE({"pop eax","xor ecx,ecx","sub esp,4","cmp eax,0","setg cl","neg ecx","mov [esp],ecx"},0)
		-- 0<
		elsif tokens[p][1] = W_LT0 then
			ADD_CODE({"pop eax","xor ecx,ecx","sub esp,4","cmp eax,0","setl cl","neg ecx","mov [esp],ecx"},0)

		-- f=
		elsif tokens[p][1] = W_FEQ then
			if fastfloat then
				ADD_CODE({"xor ecx,ecx","fcomip st(0),st(1)",
				          "ffree st(0)","fincstp","setz cl","sub esp,4","neg ecx","mov [esp],ecx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-8]","fld qword ptr [eax-16]","sub esp,4","sub fpstackptr,16","fcomip st(0),st(1)",
				          "fstp dword ptr [scratch2]","setz cl","neg ecx","mov [esp],ecx"},0)
			end if
		-- f<>
		elsif tokens[p][1] = W_FNE then
			if fastfloat then
				ADD_CODE({"xor ecx,ecx","sub esp,4","fcomip st(0),st(1)",
				          "ffree st(0)","setnz cl","fincstp","neg ecx","mov [esp],ecx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-8]","fld qword ptr [eax-16]","sub esp,4","sub fpstackptr,16","fcomip st(0),st(1)",
				          "ffree st(0)","setnz cl","fincstp","neg ecx","mov [esp],ecx"},0)
			end if
		-- f>
		elsif tokens[p][1] = W_FGT then
			if fastfloat then
				ADD_CODE({"xor ecx,ecx","fcomip st(0),st(1)",
				          "ffree st(0)","setle cl","fincstp","sub esp,4","neg ecx","mov [esp],ecx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-16]","sub esp,4","fcomip qword ptr [eax-8]","sub fpstackptr,8","setg cl","neg ecx","mov [esp],ecx"},0)
			end if		
		-- f<
		elsif tokens[p][1] = W_FLT then
			if fastfloat then
				ADD_CODE({"xor ecx,ecx","fcomip st(0),st(1)",
				          "ffree st(0)","setg cl","fincstp","sub esp,4","neg ecx","mov [esp],ecx"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","xor ecx,ecx","fld qword ptr [eax-16]","sub esp,4","fcomip qword ptr [eax-8]","sub fpstackptr,8","setl cl","neg ecx","mov [esp],ecx"},0)
			end if
			
		-- NEGATE
		elsif tokens[p][1] = W_NEG then
			ADD_CODE({"neg dword ptr [esp]"},0)
			
		-- FNEGATE
		elsif tokens[p][1] = W_FNEG then
			if fastfloat then
				ADD_CODE({"fchs"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","fchs","fstp qword ptr [eax-8]"},0)
			end if
			
		-- FLOOR
		elsif tokens[p][1] = W_FLOOR then
			if fastfloat then
				ADD_CODE({"sub esp,4","fistp dword ptr [esp]","fwait"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","sub esp,4","fld qword ptr [eax-8]","sub fpstackptr,8","fistp dword ptr [esp]"},0)
			end if
			
		-- FLOAT
		elsif tokens[p][1] = W_FLOAT then
			if fastfloat then
				ADD_CODE({"fild dword ptr [esp]","fwait","add esp,4"},0)
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
			ADD_CODE({"pop eax","add dictptr,eax"},0)
			
		-- AND
		elsif tokens[p][1] = W_AND then
			ADD_CODE({"pop eax","and [esp],eax"},0)

		-- BEGIN
		elsif tokens[p][1] = W_BEGIN then
			s = sprintf("@@loop_%04x",loops)
			loops += 1
			loopStack = append(loopStack,s)
			ADD_CODE({s&":"},0)
	
		-- DO
		elsif tokens[p][1] = W_DO then
			s = sprintf("@@loop_%04x",loops)
			loops += 1
			loopStack = append(loopStack,s)
			n = 0
			hasLoops = 1
			
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
			
			if n then
				optI = 1
				ADD_CODE({"pop esi","add "&lsp&",8","pop edi"},0)
			else
				ADD_CODE({"mov eax,"&lsp,"pop ecx","pop edx","add "&lsp&",8","mov [eax],edx","mov [eax+4],ecx"},0)
			end if
			ADD_CODE({s&":"},0)

		-- LOOP
		elsif tokens[p][1] = W_LOOP then
			if optI then
				ADD_CODE({"inc esi","cmp esi,edi","jne "&loopStack[length(loopStack)]},0)
				optI = 0
			else
				ADD_CODE({"mov eax,"&lsp,"sub eax,8","inc dword ptr [eax+4]","mov ecx,[eax]","cmp [eax+4],ecx","jne "&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			ADD_CODE({"sub "&lsp&",8"},0)
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
				ADD_CODE({"mov eax,"&lsp,"pop ebx","sub eax,8","add [eax+4],ebx","mov ecx,[eax]","cmp [eax+4],ecx","jne "&loopStack[length(loopStack)]},0)
			end if
			ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			ADD_CODE({"sub "&lsp&",8"},0)
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
				ADD_CODE({"mov eax,"&lsp,"push dword ptr [eax-4]"},0)
			end if
		-- J
		elsif tokens[p][1] = W_J then
			ADD_CODE({"mov eax,"&lsp,"push dword ptr [eax-12]"},0)
			
		-- CASE
		elsif tokens[p][1] = W_CASE then
			if case_=-1 then
				case_ = 0
				hasDefault = 0
				caseLbl = sprintf("@@case_%06x",cases)
				ADD_CODE({caseLbl&':'},0)
				ADD_CODE({"pop dword ptr [caseVar]"},0)
			else
				ERROR("CASE inside CASE",tokens[p])
			end if
		
		-- OF
		elsif tokens[p][1] = W_OF then
			if case_>=0 and not hasDefault then
				ADD_CODE({"pop eax","cmp eax,caseVar","jne "&caseLbl&sprintf("_%04x_false",case_)},0)
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
						constants = assoc_insert(tokens[p+1][2],{s,0,n+1},constants)
						ADD_CODE({"pop eax","mov dword ptr ["&s&"],eax"},0)
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
				ERROR(ERROR_EOF,tokens[p])
			end if
		
		-- VARIABLE
		elsif tokens[p][1] = W_VAR then
			if length(tokens)>p then
				if tokens[p+1][1]=UNKNOWN and bsearch(tokens[p+1][2],variables[1])=0 then
					s = sprintf("var_%07x",length(variables[1]))
					variables = assoc_insert(tokens[p+1][2],{s,4},variables)
					ADD_CODE({"mov eax,dictptr","mov "&s&",eax","add dictptr,4"},0)
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
					s = sprintf("var_%07x",length(variables[1]))
					variables = assoc_insert(tokens[p+1][2],{s,8},variables)
					ADD_CODE({"mov eax,dictptr","mov "&s&",eax","add dictptr,8"},0)
					p += 1
				else
					ERROR(ERROR_REDECLARED,tokens[p+1])
					p += 1
				end if
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		-- .
		elsif tokens[p][1] = W_DOT then
			usesConsole = 1
			ADD_CODE({"pop eax",
			 	  "invoke wsprintf,ADDR szBuffer,ADDR szFormatInt,eax",
				  "mov edi,offset szBuffer",
				  "mov al,0",
				  "repne scasb",
				  "sub edi,offset szBuffer",
				  "dec edi",
				  "invoke WriteConsole,hOutput,ADDR szBuffer,edi,ADDR lpCharsWritten,NULL"},0)
		-- f.
		--elsif tokens[p][1] = W_FDOT then
		--	usesConsole = 1
		--	ADD_CODE({"pop eax",
		--		  "add al,'0'",
		--		  "mov [szFormatFloat+3],al
		--		  "invoke wsprintf,ADDR szBuffer,ADDR szFormatInt,eax",
		--		  "mov edi,offset szBuffer",
		--		  "mov al,0",
		--		  "repne scasb",
		--		  "sub edi,offset szBuffer",
		--  		  "invoke WriteConsole,hOutput,ADDR szBuffer,edi,ADDR lpCharsWritten,NULL"},0)
				  

		-- EMIT
		elsif tokens[p][1] = W_EMIT then
			usesConsole = 1
			ADD_CODE({"pop eax",
			 	  "invoke wsprintf,ADDR szBuffer,ADDR szFormatChar,eax",
				  "invoke WriteConsole,hOutput,ADDR szBuffer,1,ADDR lpCharsWritten,NULL"},0)

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
						ERROR("Unexpected end of file",tokens[p-1])
					end if
				end while
				literals = append(literals,{sprintf("lit_%06x",length(literals)),"db \""&s&"\",0"})
				ADD_CODE({sprintf("invoke WriteConsole,hOutput,ADDR "&literals[length(literals)][1]&",%d,ADDR lpCharsWritten,NULL",length(s))},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
		
		-- ".
		elsif tokens[p][1] = W_PUTS then
			usesConsole = 1
			ADD_CODE({"pop ebx"},0)
			ADD_CODE({"mov edi,ebx",
				  "mov al,0",
				  "repne scasb",
				  "sub edi,ebx "},0)
			ADD_CODE({"invoke WriteConsole,hOutput,ebx,edi,ADDR lpCharsWritten,NULL"},0)

		-- EXPECT
		elsif tokens[p][1] = W_EXPECT then
			usesConsole = 1
			ADD_CODE({"pop ecx","pop ebx"},0)
			ADD_CODE({"invoke ReadConsole,hInput,ebx,ecx,ADDR lpCharsWritten,NULL"},0)

		-- KEY
		elsif tokens[p][1] = W_KEY then
			usesConsole = 1
			ADD_CODE({"invoke GetConsoleMode,hInput,ADDR scratch1"},0)
			ADD_CODE({"mov ebx,scratch1","and ebx,0FFFFFFF9h"},0)
			ADD_CODE({"invoke SetConsoleMode,hInput,ebx"},0)
			ADD_CODE({"invoke ReadConsole,hInput,ADDR scratch2,1,ADDR lpCharsWritten,NULL","mov eax,scratch2","and eax,255","push eax"},0)
			ADD_CODE({"invoke SetConsoleMode,hInput,scratch1"},0)

		-- BYE
		elsif tokens[p][1] = W_BYE then
			ADD_CODE({"invoke ExitProcess,0"},0)
		
		-- CR
		elsif tokens[p][1] = W_CR then
			usesConsole = 1
			ADD_CODE({"invoke WriteConsole,hOutput,ADDR szCR,1,ADDR lpCharsWritten,NULL"},0)
		-- NEWLINE
		elsif tokens[p][1] = W_NEWLINE then
			usesConsole = 1
			ADD_CODE({"invoke WriteConsole,hOutput,ADDR szLF,1,ADDR lpCharsWritten,NULL"},0)

		-- SPACE
		elsif tokens[p][1] = W_SPACE then
			usesConsole = 1
			ADD_CODE({"mov byte ptr [szBuffer],32",
			          "invoke WriteConsole,hOutput,ADDR szBuffer,1,ADDR lpCharsWritten,NULL"},0)
		-- SPACES
		elsif tokens[p][1] = W_SPACES then
			usesConsole = 1
			ADD_CODE({"mov edi,offset szBuffer",
				  "pop ecx",
				  "mov eax,32",
				  "rep stosb",
			          "invoke WriteConsole,hOutput,ADDR szBuffer,ecx,ADDR lpCharsWritten,NULL"},0)
		
		-- HERE
		elsif tokens[p][1] = W_HERE then
			ADD_CODE({"push dword ptr [dictptr]"},0)
		
		-- ,
		elsif tokens[p][1] = W_COMMA then
			ADD_CODE({"mov ebx,dictptr","pop eax","add dictptr,4","mov [ebx],eax"},0)
		-- c,
		elsif tokens[p][1] = W_CCOMMA then
			ADD_CODE({"mov ebx,dictptr","pop eax","inc dword ptr dictptr","mov [ebx],al"},0)
			
		
		-- CALL
		elsif tokens[p][1] = W_CALL then
			ADD_CODE({"call "&tokens[p+1][2]},0)
			p += 1
		-- CALLI
		elsif tokens[p][1] = W_CALLI then
			ADD_CODE({"pop eax","call eax"},0)
			
		-- DROP
		elsif tokens[p][1] = W_DROP then
			ADD_CODE({"lea esp,[esp+4]"},0)
			
		-- DUP
		elsif tokens[p][1] = W_DUP then
			ADD_CODE({"mov eax,[esp]","push eax"},0)

		-- PICK
		elsif tokens[p][1] = W_PICK then
			ADD_CODE({"pop eax","push dword ptr [esp+eax*4]"},0)
			
		-- NIP
		elsif tokens[p][1] = W_NIP then
			ADD_CODE({"pop eax","pop ebx","push eax"},0)
			
		-- TUCK
		elsif tokens[p][1] = W_TUCK then
			ADD_CODE({"pop eax","pop ebx","push eax","push ebx","push eax"},0)
		
		-- FDUP
		elsif tokens[p][1] = W_FDUP then
			if fastfloat then
				ADD_CODE({"fld st(0)"},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]","add fpstackptr,8","fstp qword ptr [eax]"},0)
			end if
			
		-- FDROP
		elsif tokens[p][1] = W_FDROP then
			ADD_CODE({"ffree st(0)","fincstp"},0)
			if not fastfloat then
				ADD_CODE({"sub fpstackptr,8"},0)
			end if
			
		-- FSWAP
		elsif tokens[p][1] = W_FSWAP then
			if fastfloat then
--				ADD_CODE({},0)
			else
				ADD_CODE({"mov eax,fpstackptr","fld qword ptr [eax-8]",
				          "fld qword ptr [eax-16]","fstp qword ptr [eax-8]","fstp qword ptr [eax-16]"},0)
			end if
			
		-- OVER
		elsif tokens[p][1] = W_OVER then
			ADD_CODE({"mov eax,[esp+4]","push eax"},0)
			
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
				ADD_CODE({"mov ebx,edi","pop eax","pop ecx","pop edi","rep stosb","mov edi,ebx"},0)
			else
				ADD_CODE({"pop eax","pop ecx","pop edi","rep stosb"},0)
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
				ADD_CODE({"pop eax","cmp eax,-1","jne "&ifStack[1]},0)
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
				ADD_CODE({"pop eax","cmp eax,-1","jz "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- NULL
		elsif tokens[p][1] = W_NULL or tokens[p][1]=W_FALSE then
			PUSH_win32(0)
		-- TRUE
		elsif tokens[p][1] = W_TRUE then
			PUSH_win32(-1)
		-- BL
		elsif tokens[p][1] = W_BL then
			PUSH_win32(' ')
		
		-- OR
		elsif tokens[p][1] = W_OR then
			ADD_CODE({"pop eax","or [esp],eax"},0)

		elsif tokens[p][1] = W_EMMS then
			ADD_CODE({"emms"},0)
			for i=1 to length(rstack) do
				free_reg(MMX_REG,rstack[i])
			end for
			rstack = {}
		
		-- PDUP
		elsif tokens[p][1] = W_PDUP then
			n = alloc_reg(MMX_REG)
			ADD_CODE({sprintf("movq mm(%d),mm(%d)",{n,rstack[1]})},0)
			rstack = n&rstack

		-- PDROP
		elsif tokens[p][1] = W_PDROP then
			n = rstack[1]
			free_reg(MMX_REG,n)
			if length(rstack)=1 then
				rstack = {}
			else
				rstack = rstack[2..length(rstack)]
			end if

		-- P@		
		elsif tokens[p][1] = W_PFETCH then
			n = alloc_reg(MMX_REG)
			rstack = n&rstack
			ADD_CODE({"pop eax",sprintf("movd mm(%d),dword ptr [eax]",n)},0)

		-- P!
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
			ADD_CODE({"pop eax","pop ecx","pop edx","push ecx","push edx","push eax"},0)

		-- SWAP
		elsif tokens[p][1] = W_SWAP then
			ADD_CODE({"pop eax","pop ecx","push eax","push ecx"},0)
			
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
				ADD_CODE({"pop eax","cmp eax,-1","jnz "&loopStack[length(loopStack)]},0)
				ADD_CODE({loopStack[length(loopStack)]&"_end:"},0)
			end if
			
		-- WHILE
		elsif tokens[p][1] = W_WHILE then
			if length(fastCmp) then
				ADD_CODE({fastCmp&" "&loopStack[length(loopStack)]&"_end"},0)
				fastCmp = ""
			else
				ADD_CODE({"pop eax","cmp eax,-1","jnz "&loopStack[length(loopStack)]&"_end"},0)
			end if
			
		-- XOR
		elsif tokens[p][1] = W_XOR then
			ADD_CODE({"pop eax","xor [esp],eax"},0)
		
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
				ADD_CODE({"push offset "&literals[length(literals)][1]},0)
			else
				ERROR(ERROR_EOF,tokens[p])
			end if
			
		elsif tokens[p][1] = UNKNOWN then
			n = bsearch(tokens[p][2],userWords[1])
			if n then
				ADD_CODE({"call "&userWords[2][n]},0)
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
					ADD_CODE({"push dword ptr ["&variables[2][n][1]&"]"},0)
				end if
			end if
			
			if not n then
				n = bsearch(tokens[p][2],constants[1])
				if n then
					ADD_CODE({"push dword ptr ["&constants[2][n][1]&"]"},0)
					constants[2][n][2] = 1	-- Mark the constant as being used
				end if
			end if

			if not n then
				n = bsearch(tokens[p][2],fconstants[1])
				if n then
					ADD_CODE({"fld qword ptr ["&fconstants[2][n]&"]"},0)
					if not fastfloat then
						ADD_CODE({"mov eax,fpstackptr","fstp qword ptr [eax]","add fpstackptr,8"},0)
					end if
				end if
			end if
		
			if not n then
				n = bsearch(tokens[p][2],deferred[1])
				if n then
					ADD_CODE({"call "&deferred[2][n]&" ; deferred"},0)
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




global procedure forthec_win32()
	sequence masmdir,subsystem
	integer nofiles
	
	loops = 0
	cases = 0
	ifs = 0
	usesConsole = 0
	noFold = 0
	fastfloat = 1
	verbose = 0
	noMangle = 0
	optLevel = 6
	masmdir = "c:\\masm32"
	labelPrefix = "@@"


	nofiles = 1
	if length(CMD)>=4 then
		if not (find("-t",CMD)>0 and length(CMD)=4) then
			nofiles = 0
		end if
	end if

	
	if nofiles then
		puts(1,"forthec -t win32 [options] <infile> <outfile>\n\n")
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
	if find("-nf",CMD) then
		noFold = 1
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
			if equal(cfgin[2][1],"MASMDIR") then
				masmdir = cfgin[2][2]
			end if
		end if
		close(cfgfile)
	end if


	outname = CMD[length(CMD)]
	outname = cut_filename(outname)
	outname[2] = lower(outname[2])

	dll = equal(outname[2],"dll") or (find("-dll",CMD) and (not equal(outname[2],"exe")))
	if dll then
		noMangle = 1
	end if

	code = {}
	ifStack = {}
	maincode = {}
	deferred = {{},{}}
	literals = {}
	includes = {"windows.inc","kernel32.inc","user32.inc","kernel32.lib","user32.lib"}
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
	
	if compile_win32() then end if

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
		maincode = optimise_win32(maincode,1)
		code = optimise_win32(code,0)
	end if

	if usesConsole then
		subsystem = "CONSOLE"
	else
		subsystem = "WINDOWS"
	end if


	puts(outfile,"; Generated by ForthEC"&NL&NL)
	-- Write header
	puts(outfile,".686p"&NL&".mmx"&NL&".model flat,stdcall"&NL&"option casemap:none"&NL&NL)
	for i=1 to length(includes) do
		if equal(lower(includes[i][length(includes[i])-2..length(includes[i])]),"lib") then
			puts(outfile,"includelib "&includes[i]&NL)
		else
			puts(outfile,"include "&includes[i]&NL)
		end if
	end for

	puts(outfile,NL&";##############################################################################"&NL&NL)
	puts(outfile,"pushdw macro pval"&NL)
	puts(outfile,"\tdb 68h"&NL)
	puts(outfile,"\tdd pval"&NL)
	puts(outfile,"endm"&NL)
	puts(outfile,NL&";##############################################################################"&NL&NL)

	-- Write data section
	puts(outfile,".data"&NL&"\tszFormatInt db \"%d \",0"&NL&"\tszFormatChar db \"%hc\",0"&NL&"\tszBuffer db 256 dup(0)"&NL)
	puts(outfile,"\tszCR db 13,0"&NL&"\tszLF db 10,0"&NL)
	for i=1 to length(literals) do
		puts(outfile,"\t"&literals[i][1]&" "&literals[i][2]&NL)
	end for

	-- Write data? section
	puts(outfile,NL&".data?"&NL&"\tstackBase dd ?"&NL&"\tdictptr dd ?"&NL&"\tdictionary dd 32768 dup(?)"&NL)
	puts(outfile,"\tipstackptr dd ?"&NL&"\tipstack dd 256 dup(?)"&NL)
	puts(outfile,"\tfpstackptr dd ?"&NL&"\tfpstack dq 128 dup(?)"&NL)
	puts(outfile,"\tloopstackptr dd ?"&NL&"\tloopstack dd 64 dup(?)"&NL)
	puts(outfile,"\tmiscstackptr dd ?"&NL&"\tmiscstack dd 64 dup(?)"&NL)
	puts(outfile,"\thOutput dd ?"&NL&"\thInput dd ?"&NL&"\tcaseVar dd ?"&NL&"\tlpCharsWritten dd ?"&NL)
	puts(outfile,"\tscratch1 dd ?"&NL&"\tscratch2 dd ?"&NL&"\treturnaddress dd ?"&NL)

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
			puts(outfile,"\t"&constants[2][i][1]&" dd ?"&NL)
		end if
	end for

	for i=1 to length(fconstants[2]) do
		puts(outfile,"\t"&fconstants[2][i]&" dq ?"&NL)
	end for

	for i=1 to length(variables[2]) do
		--if variables[2][i][2] = 4 then
			puts(outfile,"\t"&variables[2][i][1]&" dd ?"&NL)
		--else
		--	puts(outfile,"\t"&variables[2][i][1]&" dq ?"&NL)
		--end if
	end for

	puts(outfile,NL&";##############################################################################"&NL&NL)

	-- Write code section
	puts(outfile,".code"&NL&"align 4"&NL&NL)
	if not dll then
		for i=1 to length(code) do
			puts(outfile,code[i]&NL)
		end for
	else
		for i=1 to length(userWords[2]) do
			puts(outfile,"PUBLIC "&userWords[2][i]&NL)
		end for
		puts(outfile,NL)
	end if

	puts(outfile,NL&";##############################################################################"&NL&NL)

	if dll then
		puts(outfile,"__ForthecDllMain PROC hInstDLL:DWORD, reason:DWORD, unused:DWORD"&NL)
		puts(outfile,"cmp reason,1"&NL&"jne @@dllmain_not_attach"&NL)
	else
		puts(outfile,"__start:"&NL)
	end if

	if usesConsole then
		puts(outfile,"invoke GetStdHandle,STD_OUTPUT_HANDLE"&NL&"mov hOutput,eax"&NL)
		puts(outfile,"invoke GetStdHandle,STD_INPUT_HANDLE"&NL&"mov hInput,eax"&NL)
	end if
	puts(outfile,"mov fpstackptr,offset fpstack"&NL&"mov ipstackptr,offset ipstack"&NL&"add ipstackptr,1024"&NL&"mov dictptr,offset dictionary"&NL)
	puts(outfile,"mov miscstackptr,offset miscstack"&NL)
	puts(outfile,"mov loopstackptr,offset loopstack"&NL&NL)

	for i=1 to length(maincode) do
		if length(maincode[i]) then
			puts(outfile,maincode[i]&NL)
		end if
	end for

	if dll then
		dllentry = find("LibMain",userWords[1])
		if dllentry then
			puts(outfile,"@@dllmain_not_attach:"&NL)
			puts(outfile,"push unused"&NL&"push reason"&NL&"push hInstDLL"&NL&"call "&userWords[2][dllentry]&NL&"ret"&NL)
		else
			puts(outfile,"mov eax,1"&NL&"ret"&NL)
			puts(outfile,"@@dllmain_not_attach:"&NL)
			puts(outfile,"mov eax,0"&NL&"ret"&NL)
		end if		
		puts(outfile,"__ForthecDllMain ENDP"&NL&NL)
		for i=1 to length(code) do
			puts(outfile,code[i]&NL)
		end for
		puts(outfile,NL&"END __ForthecDllMain"&NL)
	else
		puts(outfile,NL&"END __start"&NL)
	end if

	close(outfile)

	if verbose then
		printf(1,"Done\nRemoved %d unused constants and %d redundant instructions\nTotal compilation time: %1.2f seconds\n\n",{constRemoved,linesRemoved,time()-t1})
	end if


	if equal(outname[2],"exe") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system("ml /c /coff /I"&masmdir&"\\include /Fo"&outname[1]&".obj "&outname[1]&".asm",2)
		if find("-res",CMD) then
			system("rc "&outname[1]&".rc",2)
			--system("cvtres "&outname[1]&".res",2)
			system("link /SUBSYSTEM:"&subsystem&" /OPT:NOREF /LIBPATH:"&masmdir&"\\lib /OUT:"&outname[1]&".exe "&outname[1]&".obj "&outname[1]&".res",2)
			system("del "&outname[1]&".res",2)
		else
			system("link /SUBSYSTEM:"&subsystem&" /OPT:NOREF /LIBPATH:"&masmdir&"\\lib /OUT:"&outname[1]&".exe "&outname[1]&".obj",2)
		end if
		system("del "&outname[1]&".asm",2)
		system("del "&outname[1]&".obj",2)
	elsif equal(outname[2],"dll") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system("ml /c /coff /I"&masmdir&"\\include /Fo"&outname[1]&".obj "&outname[1]&".asm",2)
		system("link /SUBSYSTEM:"&subsystem&" /DLL /DEF:"&outname[1]&".def /OPT:NOREF /LIBPATH:"&masmdir&"\\lib /OUT:"&outname[1]&".dll "&outname[1]&".obj",2)
		system("del "&outname[1]&".asm",2)
		system("del "&outname[1]&".obj",2)
	elsif equal(outname[2],"obj") then
		if verbose then
			puts(1,"\nAssembling\n")
		end if
		system("ml /c /coff /I"&masmdir&"\\include /Fo"&outname[1]&".obj "&outname[1]&".asm",2)
	end if



	if verbose then
		puts(1,"\nPress any key..")
		while get_key() = -1 do end while
	end if
end procedure




