-- ForthEC code optimiser
-- /Mic, 2004


include parser.e


-- x86 registers
constant regs32 = {"eax","ebx","ecx","edx","esi","edi","ebp","esp"}
constant regs16 = {"ax","bx","cx","dx","si","di","bp","sp"}
constant regs8 = {"al","ah","bl","bh","cl","ch","dl","dh"}

-- x86 condition codes ("nz" is left out because of how these are used in the patterns below)
constant conds = {"g","ge","l","le","z"}



-- Integer instruction patterns to scan for
--
-- Examples of rules:
--
-- {
--	{
--	"mov /reg32/,/reg32/",
--	"add [esp],$1"
--	},
--	{{"add [esp],$2"}}
-- }
--
-- This would match e.g.
-- 	
--	mov ecx,eax
--	add [esp],ecx
--
-- and replace it with
--
--	add [esp],eax
--
constant intpatterns = {
{
	{
	"push /reg32/",
	"pop $1"
	},
	{}
},
{
	{
	"mov /reg32/,/reg32/",
	"add [esp],$1"
	},
	{{"add [esp],$2"}}
},
{
	{
	"mov /reg32/,[/reg32/]",
	"push $1"
	},
	{{"push dword ptr [$2]"}}
},
{
	{
	"push /reg32/",
	{"mov /reg32/,/const/","mov /reg32/,/var/","mov /reg32/,/imm/"},
	"pop /reg32/",
	"cmp $4,$2"
	},
	{{"cmp $1,$3"}}
},
{
	{
	"sub esp,4",
	"mov dword ptr [esp],/imm/",
	"pop eax",
	"mov /const/,eax"
	},
	{{"mov $2,$1"}}
},
{
	{
	"mov eax,/imm/",
	"mov /const/,eax"
	},
	{{"mov $2,$1"}}
},
{
	{
	"push /const/",
	"add esp,4"
	},
	{}
},
{
	{
	"mov eax,ipstackptr",
	"mov ecx,[eax]",
	"add ipstackptr,4",
	"push ecx",
	"ret"
	},
	{1,{"add ipstackptr,4"},{"jmp dword ptr [eax]"}}
},
{
	{
	"push dword ptr [esp]",
	"mov eax,/imm/",
	"pop /reg32/",
	"cmp $2,eax"
	},
	{{"cmp dword ptr [esp],$1"}}
},
{
	{
	"mov eax,[esp]",
	"add /reg32/,eax"
	},
	{{"add $1,[esp]"}}
},
{
	{
	"push eax",
	"mov ebx,dictptr",
	"pop eax",
	"add dictptr,4",
	"mov [ebx],eax"
	},
	{2,4,5}
},
{
	{
	"sub esp,4",
	"mov dword ptr [esp],/imm/",
	"mov ebx,dictptr",
	"pop eax",
	"add dictptr,4",
	"mov [ebx],eax"
	},
	{3,5,{"mov dword ptr [ebx],$1"}}
},
{
	{
	"sub esp,/imm/",
	"mov dword ptr [esp],/imm/",
	"sub esp,/imm/",
	"mov dword ptr [esp],/imm/"
	},
	{{"mov dword ptr [esp-$1],$2"},{"mov dword ptr [esp-%($1+$3)],$4"},{"sub esp,%($1+$3)"}}
},
{
	{
	"mov dword ptr [esp-/imm/],/imm/",
	"mov dword ptr [esp-/imm/],/imm/",
	"sub esp,/imm/",
	"mov dword ptr [esp-/imm/],/imm/",
	"mov dword ptr [esp-/imm/],/imm/",
	"sub esp,/imm/"
	},
	{1,2,{"mov dword ptr [esp-%($5+$6)],$7"},{"mov dword ptr [esp-%($5+$8)],$9"},{"sub esp,%($5+$10)"}}
},
{
	{
	"lea esp,[esp+/imm/]",
	"lea esp,[esp+/imm/]"
	},
	{{"lea esp,[esp+%($1+$2)]"}}
},
{
	{
	"mov eax,[esp+/imm/]",
	"push eax"
	},
	{{"push dword ptr [esp+$1]"}}
},
{
	{
	"set/cond/ cl",
	"neg ecx",
	"mov [esp],ecx",
	"pop eax",
	"cmp eax,-1",
	"jnz /label/"
	},
	{4,{"jn$1 $2"}}
},
{
	{
	"add [esp],eax",
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	{"mov [eax],/reg8/"}
	},
	{{"mov ecx,[esp]"},3,{"mov [eax+ecx],$1"}}
},
{
	{
	"add [esp],/reg32d/",
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	{"mov [eax],/reg8/"}
	},
	{2,3,{"mov [eax+$1],$2"}}
},
{
	{
--	"push /reg32/",
	{"push /reg32/","push /var/","push /const/","push /ind/"},
	{"push /reg32/","push /var/","push /const/","push /ind/"},
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	{"mov [eax],/reg32/","mov [eax],/reg16/","mov [eax],/reg8/"},
	"add esp,8"
	},
	{{"mov ecx,$1"},{"mov eax,$2"},{"mov [eax],$3"}}
},
{
	{
	"sub esp,4",
	"mov dword ptr [esp],/imm/",
	{"push /reg32/","push /var/","push /const/"},
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	{"mov [eax+/reg32d/],/reg32d/", "mov [eax+/reg32d/],/reg16d/", "mov [eax+/reg32d/],/reg8d/"},
	"add esp,8"
	},
	{{"mov ecx,$1"},{"mov eax,$2"},{"mov [eax+$3],$4"}}
},
{
	{
	"sub esp,4",
	"mov dword ptr [esp],/imm/",
	{"push /reg32/","push /var/","push /const/"},
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	{"mov [eax],/reg32/", "mov [eax],/reg16/", "mov [eax],/reg8/"},
	"add esp,8"
	},
	{{"mov ecx,$1"},{"mov eax,$2"},{"mov [eax],$3"}}
},
{
	{
	"mov eax,/var/",
	"push dword ptr [eax]",
	"mov eax,/var/",
	"push dword ptr [eax]",
	"mov eax,/const/",
	"add [esp],eax",
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	"mov [eax],ecx",
	"add esp,8"
	},
	{{"mov ecx,$2"},1,{"mov ecx,[ecx]"},{"mov eax,[eax]"},{"add ecx,$3"},{"mov [ecx],eax"}}
},
{
	{
	"push dword ptr [esp]",
	"pop /reg32/"
	},
	{{"mov $1,[esp]"}}
},
{
	{
	"mov /reg32/,[esp]",
	"add /reg32/,$1"
	},
	{{"add $2,[esp]"}}
},
{
	{
	"mov ecx,eax",
	"mov eax,/var/",
	"mov [eax],ecx"
	},
	{{"mov ecx,$1"},{"mov [ecx],eax"}}
},
{
	{
	"mov eax,/imm/",
	"mov ecx,[esp]",
	"mov [ecx],eax",
	"add dword ptr [esp],4"
	},
	{{"mov eax,[esp]"},{"mov dword ptr [eax],$1"},4}
},
{	
	{
	"push dword ptr [eax]",
	{"mov eax,/const/","mov eax,/var/"},
	"add [esp],eax",
	"pop eax"
	},
	{{"mov eax,[eax]"},{"add eax,$1"}}
},
{	
	{
	"sub esp,/imm/",
	"sub esp,/imm/"
	},
	{{"sub esp,%($1+$2)"}}
},
{
	{
	"mov eax,/imm/",
	"add [/reg32/],eax"
	},
	{{"add dword ptr [$2],$1"}}
},
{
	{
	"sub ipstackptr,4",
	"pop ecx",
	"mov eax,ipstackptr",
	"mov [eax],ecx"
	},
	{1,3,{"pop dword ptr [eax]"}}
},
{	
	{
	"sub ipstackptr,4",
	"mov eax,ipstackptr",
	"pop dword ptr [eax]",
	"sub ipstackptr,4",
	"mov eax,ipstackptr",
	"mov [eax],ebp",
	"mov ebp,esp"
	},
	{2,{"pop dword ptr [eax-4]"},{"sub ipstackptr,8"},{"mov [eax-8],ebp"},{"mov ebp,esp"}}
},
{	
	{
	{"push /var/","push /ind/"},
	"mov eax,loopstackptr",
	"mov eax,dword ptr [eax-4]",
	"add [esp],eax",
	"pop /reg32/"
	},
	{{"mov $2,$1"},{"mov edx,loopstackptr"},{"add $2,dword ptr [edx-4]"}}
},
{
	{
	"fld /qlit/",
	"sub esp,4",
	"fstp dword ptr [esp]",
	"fld /qlit/",
	"sub esp,4",
	"fstp dword ptr [esp]"
	},
	{1,4,{"sub esp,8"},{"fstp dword ptr [esp]"},{"fstp dword ptr [esp+4]"}}
},
{
	{
	"mov ecx,/imm/",
	"mov eax,/var/",
	"mov [eax],ecx"
	},
	{2,{"mov dword ptr [eax],$1"}}
},
{
	{
	"mov ecx,/imm/",
	"mov eax,/var/",
	"mov [eax],cl"
	},
	{2,{"mov byte ptr [eax],$1"}}
},
{
	{
	"mov ecx,/imm/",
	"mov eax,/var/",
	"mov [eax+/reg32d/],cl"
	},
	{2,{"mov byte ptr [eax+$3],$1"}}
},
{
	{
	"mov eax,[esp]",
	"mov dword ptr [eax],/imm/",
	"add dword ptr [esp],4",
	"mov eax,[esp]",
	"mov dword ptr [eax],/imm/",
	"add dword ptr [esp],4"
	},
	{1,2,{"mov dword ptr [eax+4],$2"},{"add dword ptr [esp],8"}}
},
{
	{
	"mov /reg32/,/imm/",
	"cmp $1,$2",
	"jnz /label/"
	},
	{}
},
{
	{
	"push /ind/",
	"pop /reg32/"
	},
	{{"mov $2,$1"}}
},
{
	{
	"mov /reg32d/,eax",
	"mov eax,$1"
	},
	{{"xchg eax,$1"}}
},
{
	{
	"mov eax,/var/",
	"push dword ptr [eax]",
	"inc dword ptr [esp]",
	"push $1",
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	"mov [eax],ecx",
	"add esp,8"
	},
	{1,{"inc dword ptr [eax]"}}
},
{
	{
	"mov eax,/var/",
	"add [esp],eax",
	"pop eax"
	},
	{{"pop eax"},{"add eax,$1"}}
},
{
	{
	{"add [esp],eax","sub [esp],eax","and [esp],eax"},
	"push /var/",
	"mov eax,[esp]",
	"mov ecx,[esp+4]",
	"mov [eax],ecx",
	"add esp,8"
	},
	{{1,1,3," eax,[esp]"},{"mov ecx,$1"},{"add esp,4"},{"mov [ecx],eax"}}
},
{
	{
	"push ebx",
	"inc dword ptr [esp]"
	},
	{{"inc ebx"},{"push ebx"}}
},
{
	{
	"mov /reg32/,$1"
	},
	{}
}
}


-- Floating point instruction patterns to scan for
constant fppatterns = {
{
	{
	"mov eax,fpstackptr",		-- The pattern
	"fld qword ptr [lit_",
	"add fpstackptr,8",
	"fstp qword ptr [eax]",
	"mov eax,fpstackptr",
	"sub esp,4",
	"fld qword ptr [eax-8]",
	"sub fpstackptr,8",
	"fstp dword ptr [esp]"
	},
	{1,2,6,9}			-- Which instructions to keep	
},
{
	{
	"mov eax,fpstackptr",
	"fld qword ptr [eax-8]",
	{"fadd qword ptr [eax-16]","fsub qword ptr [eax-16]","fmul qword ptr [eax-16]","fdiv qword ptr [eax-16]"},
	"sub fpstackptr,8",
	"fstp qword ptr [eax-16]",
	"mov eax,fpstackptr",
	"fld qword ptr [eax-8]",
	{"fadd qword ptr [eax-16]","fsub qword ptr [eax-16]","fmul qword ptr [eax-16]","fdiv qword ptr [eax-16]"},
	"sub fpstackptr,8",
	"fstp qword ptr [eax-16]"
	},
	{1,2,3,{"sub eax,8"},{"sub fpstackptr,16"},8,10}
},
{
	{
	"mov eax,dword ptr [var_",
	"mov ebx,fpstackptr",
	"fld qword ptr [eax]",
	"add fpstackptr,8",
	"fstp qword ptr [ebx]",
	"mov eax,dword ptr [var_",
	"mov ebx,fpstackptr",
	"fld qword ptr [eax]",
	"add fpstackptr,8",
	"fstp qword ptr [ebx]",
	"mov eax,fpstackptr",
	"fld qword ptr [eax-8]",
	{"fadd qword ptr [eax-16]","fsub qword ptr [eax-16]","fmul qword ptr [eax-16]","fdiv qword ptr [eax-16]"},
	"sub fpstackptr,8",
	"fstp qword ptr [eax-16]"
	},
	{6,2,3,1,8,{"add fpstackptr,8"},{13,1,4," st(0),st(1)"},{"fstp qword ptr [ebx]"}}
},
{
	{
	"mov eax,fpstackptr",
	"fld qword ptr [lit",
	"sub esp,4",
	"fstp dword ptr [esp]"
	},
	{2,3,4}
}
}



	
linesRemoved = 0
constRemoved = 0
constlist = {}


-- Is this a pop instruction ?
function is_pop(sequence s)
	if length(s)>3 then
		if equal(s[1..3],"pop") then
			return 1
		end if
	end if
	return 0
end function


-- Is this a push instruction ?
function is_push(sequence s)
	if length(s)>4 then
		if equal(s[1..4],"push") then
			return 1
		elsif length(s)>19 then
			if equal(s[1..19],"mov dword ptr [esp]") then
				return 2
			end if
		end if
	end if
	return 0
end function


function get_reg(sequence s)
	integer p
	
	p = find(',',s)
	if p then
		return s[p+1..length(s)]
	else
		return s[find(' ',s)+1..length(s)]
	end if
end function


function get_reg1(sequence s)
	integer p
	
	p = find(',',s)
	if p then
		return s[find(' ',s)+1..p-1]
	else
		return s[find(' ',s)+1..length(s)]
	end if
end function


function get_imm(sequence s)
	return s[find(',',s)+1..length(s)]
end function


-- safe_compare(code,pattern)
function safe_compare(sequence s1,sequence s2)
	if length(s1)>=length(s2) then
		return equal(s1[1..length(s2)],s2)
	end if
	return 0
end function


function compare_patterns(sequence s1,sequence s2)
	integer eql,p1,p2,m,n,o
	sequence s3,s4
	
	eql = 1
	p1 = 1
	p2 = 1
	m = length(patvars)
	o = 0
	
	
	while eql do
		if p1>length(s1) or
		   p2>length(s2) then
			exit
		end if
		if s2[p2] = '/' then
			s3 = {}
			p2 += 1
			while s2[p2] != '/' do
				s3 &= s2[p2]
				p2 += 1
			end while
			p2 += 1
			
			s4 = {}
			while p1<=length(s1) do
				if s1[p1]=',' or
				   s1[p1]=']' then
					exit
				end if
				s4 &= s1[p1]
				p1 += 1
			end while
			
			if equal(s3,"reg32") then
				if find(s4,regs32) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			elsif equal(s3,"reg16") then
				if find(s4,regs16) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			elsif equal(s3,"reg8") then
				if find(s4,regs8) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
				o = 1
			elsif equal(s3,"reg32d") then
				if find(s4,regs32[2..length(regs32)]) then
					--puts(1,s4&"\n")
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			elsif equal(s3,"reg16d") then
				if find(s4,regs16[2..length(regs16)]) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			elsif equal(s3,"reg8d") then
				if find(s4,regs8[2..length(regs8)]) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
				o = 1
			elsif equal(s3,"const") then
				s4 &= ']'
				eql = 0
				if find('_',s4) and length(s4)>=17 then
					if equal("dword ptr [const_",s4[1..17]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"var") then
				s4 &= ']'
				eql = 0
				if find('_',s4) then
					if equal("dword ptr [var_",s4[1..15]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"ind") then
				s4 &= ']'
				eql = 0
				if length(s4)>12 then
					if equal("dword ptr [e",s4[1..12]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"qlit") then
				s4 &= ']'
				eql = 0
				if find('_',s4) then
					if equal("qword ptr [lit_",s4[1..15]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"imm") then
				o = 0
				if length(s4)>6 then
					if equal(s4[1..6],"offset") then
						patvars = append(patvars,s4)
						o = 1
					end if
				end if
				if o=0 then
					s3 = value(s4)
					if s3[1]=GET_SUCCESS and integer(s3[2]) then
						patvars = append(patvars,s4)
					else
						eql = 0
					end if
				end if
			elsif equal(s3,"cond") then
				if find(' ',s4) then
					p1 -= length(s4)+1-find(' ',s4)
					--puts(1,"s1[p1] = "&s1[p1]&"\n")
					s4 = s4[1..find(' ',s4)-1]
				end if
				if find(s4,conds) then
					patvars = append(patvars,s4)
					--puts(1,s4&".\n")
				else
					eql = 0
				end if
			elsif equal(s3,"label") then
				if s4[1]='@' then
					patvars = append(patvars,s4)
				else
					eql = 0
				end if
			elsif equal(s3,"mem32") then
				s4 &= ']'
				eql = 0
				if length(s4)>11 then
					if equal("dword ptr [",s4[1..11]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"mem") then
				s4 &= ']'
				eql = 0
				if length(s4)>11 then
					if equal("dword ptr [",s4[1..11]) or
					   equal("qword ptr [",s4[1..11]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			end if
			
		elsif s2[p2]='$' then
			n = 0
			p2 += 1
			while p2<=length(s2) do
				if s2[p2]>='0' and s2[p2]<='9' then
					n = n*10 + s2[p2]-'0'
				else
					exit
				end if
				p2 += 1
			end while
			s3 = patvars[n]

			s4 = {}
			while p1<=length(s1) do
				if s1[p1]=',' or
				   s1[p1]=']' then
				   	if s1[p1]=']' then p1+=1 end if
					exit
				end if
				s4 &= s1[p1]
				p1 += 1
			end while
			if length(s4)>11 then
				if equal(s4[7..9],"ptr") then
					s4 &= ']'
				end if
			end if
			
			if not equal(s3,s4) then
				eql = 0
			end if
				
		else
			if s1[p1]!=s2[p2] then
				eql = 0
			end if
			p1 += 1
			p2 += 1
		end if
	end while
	
	if p1<length(s1) or p2<length(s2) then
		eql = 0
	end if
	
	if not eql then
		if m=0 then
			patvars = {}
		else
			patvars = patvars[1..m]
		end if
	end if
	
	return eql
end function



function pattern_append(sequence s1,sequence s2)
	integer p2,m,n,oper,expval
	sequence s3,s4,ops

	p2 = 1
	s3 = {}
	while 1 do
		if p2>length(s2) then
			exit
		end if
		
		if s2[p2]='$' then
			n = 0
			p2 += 1
			while p2<=length(s2) do
				if s2[p2]>='0' and s2[p2]<='9' then
					n = n*10 + s2[p2]-'0'
				else
					exit
				end if
				p2 += 1
			end while
			s3 &= patvars[n]
		elsif s2[p2]='%' then
			p2 += 2
			expval = 0
			ops = {}
			oper = 0
			while s2[p2] != ')' do
				if s2[p2] = '$' or
				   s2[p2] = '#' then
				   	m = s2[p2]
					n = 0
					p2 += 1
					while p2<=length(s2) do
						if s2[p2]>='0' and s2[p2]<='9' then
							n = n*10 + s2[p2]-'0'
						else
							exit
						end if
						p2 += 1
					end while
					if m='$' then
						s4 = value(patvars[n])
					else
						s4 = {GET_SUCCESS,n}
					end if
					if oper=0 then
						expval = s4[2]
					elsif oper='+' then
						expval += s4[2]
					elsif oper='-' then
						expval -= s4[2]
					elsif oper='*' then
						expval *= s4[2]
					end if

				elsif s2[p2] = '+' then
					oper = s2[p2]
					p2 += 1
				elsif s2[p2] = '*' then
					oper = s2[p2]
					p2 += 1
				elsif s2[p2] = '-' then
					oper = s2[p2]
					p2 += 1
				else
					p2 += 1
				end if
			end while
			p2 += 1
			s3 &= sprintf("%d",expval)
					
		else
			s3 &= s2[p2]
			p2 += 1
		end if
	end while
	
	return append(s1,s3)
end function




global function optimise_win32(sequence subject,integer remConst)
	integer i1,i2,i3,i4,p,q,n,m,o,clean,improvement,times,reachable
	sequence s,r,t,u,pat

	if remConst then
		-- Remove unused constants
		for i=1 to length(constants[2]) do
			if constants[2][i][2]=0 then
				subject = subject[1..constants[2][i][3]-1] & {"add esp,4"} & subject[constants[2][i][3]+2..length(subject)]
				constRemoved += 1
				for j=i+1 to length(constants[2]) do
					if constants[2][j][3]>constants[2][i][3] then
						constants[2][j][3] -= 1
					end if
				end for
			end if
		end for
	end if

	
	if optLevel>=2 then
	
	-- Collapse combinations like push reg/pop reg	
	p = 2
	while p<=length(subject) do
		if is_pop(subject[p]) then
			n = is_push(subject[p-1])
			if n=1 then
				r = get_reg(subject[p])
				if find(r,regs32) then
					if not equal(r,get_reg(subject[p-1])) then
						s = "mov "&r&","&get_reg(subject[p-1])
						if p-2>0 then
							if p+1<=length(subject) then
								subject = subject[1..p-2] & {s} & subject[p+1..length(subject)]
							else
								subject = subject[1..p-2] & {s}
							end if
							linesRemoved += 1
						end if
					end if
				end if
			elsif n=2 then
				r = get_reg(subject[p])
				if find(r,regs32) then
					s = "mov "&get_reg(subject[p])&","&get_imm(subject[p-1])
					--puts(1,"Collapsing "&subject[p-2]&" and "&subject[p-1]&" and "&subject[p]&" into "&s&"\n")
					if p-3>0 then
						if p+1<=length(subject) then
							subject = subject[1..p-3] & {s} & subject[p+1..length(subject)]
						else
							subject = subject[1..p-3] & {s}
						end if
						linesRemoved += 2
					end if
				end if
			end if
		end if
		p += 1
	end while
	
	-- Remove combinations like sub esp,4/mov dword ptr [esp],x/add esp,4
	clean = 0
	while not clean do	-- Repeat until the entire code sequence was scanned without finding anything to remove
		p = 1
		clean = 1
		while p<=length(subject)-2 do
			if length(subject[p])=9 then
				if equal(subject[p],"sub esp,4") then
					if length(subject[p+2])=9 then
						if equal(subject[p+2],"add esp,4") then
							if length(subject[p+1])>19 then
								if equal(subject[p+1][1..19],"mov dword ptr [esp]") then
									if p+3<=length(subject) then
										subject = subject[1..p-1] & subject[p+3..length(subject)]
										linesRemoved += 3
										clean = 0
									end if
								end if
							end if
						end if
					end if
				end if
			end if
			p += 1
		end while
	end while

	-- Collapse combination like mov eax,imm/mov const_x,eax
	p = 2
	while p<=length(subject) do
		if length(subject[p])>21 then
			if equal(subject[p][1..21],"mov dword ptr [const_") then
				if equal(get_reg(subject[p]),"eax") then
					if length(subject[p-1])>7 then
						if equal(subject[p-1][1..7],"mov eax") then
							s = get_imm(subject[p-1])
							r = value(s)
							if r[1] = GET_SUCCESS then	-- Make sure it's a number
								if p>2 and p<length(subject) then
									subject = subject[1..p-2] &
									          --{"mov dword ptr ["&subject[p][5..find(',',subject[p])-1]&"],"&s} &
									          {"mov "&subject[p][5..find(',',subject[p])-1]&","&s} &
									          subject[p+1..length(subject)]
									linesRemoved += 1
								end if
							end if
						end if
					end if
				end if
			end if
		end if
		p += 1
	end while
	
	-- Remove combinations like add esp,4/sub esp,4
	p = 1
	while p<length(subject) do
		if equal(subject[p],"add esp,4") then
			if equal(subject[p+1],"sub esp,4") then
				if p>1 and p+1<length(subject) then
					subject = subject[1..p-1] & subject[p+2..length(subject)]
					linesRemoved += 2
				end if
			end if
		end if
		p += 1
	end while
	
	end if
	

	if optLevel>=3 then
	
		-- Replace common floating point instruction patterns by more compact forms
		for pass=1 to 2 do
			improvement = linesRemoved
			
			p = 1
			while p<=length(subject) do
				for i=1 to length(fppatterns) do
					pat = fppatterns[i]
					if p+length(pat[1])<=length(subject)+1 then
						if safe_compare(subject[p],pat[1][1]) then
							n = 1
							for j=2 to length(pat[1]) do
								if sequence(pat[1][j][1]) then
									m = 0
									for k=1 to length(pat[1][j]) do
										if safe_compare(subject[p+j-1],pat[1][j][k]) then
											m = k
											exit
										end if
									end for
									if not m then
										n = 0
										exit
									end if
								else
									if not safe_compare(subject[p+j-1],pat[1][j]) then
										n = 0
										exit
									end if
								end if
							end for
							if n then
								s = {}
								for j=1 to length(pat[2]) do
									if sequence(pat[2][j]) then
										if sequence(pat[2][j][1]) then
											s = append(s,pat[2][j][1])
										else
											s = append(s,subject[p+pat[2][j][1]-1][pat[2][j][2]..pat[2][j][3]]&pat[2][j][4])
										end if
									else
										s = append(s,subject[p+pat[2][j]-1])
									end if
								end for
								subject = subject[1..p-1] & s & subject[p+length(pat[1])..length(subject)]
								linesRemoved += length(pat[1])-length(pat[2])
								exit
							end if
						end if
					end if
				end for
				p += 1
			end while
			
			improvement -= linesRemoved
			if not improvement then
				exit
			end if
		end for
	end if

	if optLevel >= 5 then
		-- Run up to 10 passes
		times = 10
		while times>0 do
			improvement = linesRemoved
			
			-- Replace common integer instruction patterns by more compact forms
			p = 1
			while p<=length(subject) do
				for i=1 to length(intpatterns) do
					pat = intpatterns[i]
					if p+length(pat[1])<=length(subject)+1 then
						patvars = {}
						--if compare_patterns(subject[p],pat[1][1]) then
							n = 1
							for j=1 to length(pat[1]) do

								if sequence(pat[1][j][1]) then
									m = 0
									for k=1 to length(pat[1][j]) do
										if compare_patterns(subject[p+j-1],pat[1][j][k]) then
											m = k
											exit
										end if
									end for
									if not m then
										n = 0
										exit
									end if
								else
									if not compare_patterns(subject[p+j-1],pat[1][j]) then
										n = 0
										exit
									end if
								end if
							end for

							-- Was a matching pattern found ?
							if n then
								s = {}
								for j=1 to length(pat[2]) do
									if sequence(pat[2][j]) then
										if sequence(pat[2][j][1]) then
											s = pattern_append(s,pat[2][j][1])
										else
											s = pattern_append(s,subject[p+pat[2][j][1]-1][pat[2][j][2]..pat[2][j][3]]&pat[2][j][4])
										end if
									else
										s = pattern_append(s,subject[p+pat[2][j]-1])
									end if
								end for
								subject = subject[1..p-1] & s & subject[p+length(pat[1])..length(subject)]
								linesRemoved += length(pat[1])-length(pat[2])
								exit
							end if
						--end if
					end if
				end for
				p += 1
			end while

			
			-- Stop when there's nothing more to optimise
			improvement -= linesRemoved
			if not improvement then
				exit
			end if
			times -= 1
		end while
	end if
	
	if optLevel >= 5 then
		if remConst then
			constlist = {}
			for i=1 to length(subject) do
				patvars = {}
				if compare_patterns(subject[i],"mov /const/,/imm/") then
					constlist = append(constlist,patvars&i&1)
				end if
			end for
		end if

		patvars = {}
		for i=1 to length(constlist) do
			patvars = constlist[i][1..2]
			for j=1 to length(subject) do
				if equal(subject[j],"push "&patvars[1]) then
					subject[j] = "pushdw "&patvars[2]
					constlist[i][4] = 0
				elsif compare_patterns(subject[j][4..length(subject[j])]," /reg32/,$1") then
					subject[j] = subject[j][1..find(',',subject[j])-1]&","&patvars[2]
					patvars = patvars[1..2]
					constlist[i][4] = 0
				end if
			end for
		end for

		p=2
		while 1 do
			if p+2>length(subject) then
				exit
			end if
			if equal(subject[p],"sub esp,4") then
				patvars = {}
				if compare_patterns(subject[p+1],"mov dword ptr [esp],/imm/") then
					subject = subject[1..p-1] & {"pushdw "&patvars[1]} & subject[p+2..length(subject)]
					linesRemoved += 1
				end if
			end if
			p += 1
		end while
		

		if 1 then
		for j=1 to 51 do
			if j=1 then
				pat = {
				"mov eax,/var/",
				"push dword ptr [eax]",
				"add dword ptr [esp],/imm/",
				"push $1",
				"mov eax,[esp]",
				"mov ecx,[esp+4]",
				"mov [eax],ecx",
				"add esp,8"}
				r = {
				"mov eax,$1",
				"add dword ptr [eax],$2"
				}
			elsif j=2 then
				pat = {
				"mov eax,/var/",
				"push dword ptr [eax]",
				"mov eax,/imm/",
				"pop ecx",
				"cmp ecx,eax"}
				r = {
				"mov eax,$1",
				"cmp dword ptr [eax],$2"}
			elsif j=3 then
				pat = {
				"mov eax,/imm/",
				"xor ecx,ecx",
				"cmp [esp],eax",
				"set/cond/ cl",
				"neg ecx",
				"mov [esp],ecx",
				"pop eax",
				"cmp eax,-1",
				"jz /label/"}
				r = {
				"cmp dword ptr [esp],$1",
				"lea esp,[esp+4]",
				"j$2 $3"}
			elsif j=4 then
				pat = {
				"push /ind/",
				"mov eax,/imm/",
				"sub [esp],eax",
				"pop eax"}			
				r = {
				"mov eax,$1",
				"sub eax,$2"}
			elsif j=5 then
				pat= {
				"mov [esp],eax",
				"pop eax"}
				r = {"lea esp,[esp+4]"}
			elsif j=6 then
				pat = {
				"push /reg32/",
				"mov eax,/imm/",
				"sub [esp],eax",
				"pop /reg32/"}
				r = {
				"lea $3,[$1-$2]"}
			elsif j=7 then
				pat = {
				"push /ind/",
				"cmp dword ptr [esp],/imm/",
				"lea esp,[esp+4]"}
				r = {
				"cmp $1,$2"}
			elsif j=8 then
				pat = {
				"push eax",
				"lea eax,[ecx-/imm/]",
				"pop ecx"}
				r = {
				"mov edx,eax",
				"lea eax,[ecx-$1]",
				"mov ecx,edx"}
			elsif j=9 then
				pat = {
				"push /ind/",
				"mov eax,/imm/",
				"and [esp],eax",
				"pop eax"}
				r = {
				"mov eax,$1",
				"and eax,$2"}
			elsif j=10 then
				pat = {
				"push /var/",
				"mov eax,[esp]",
				"mov ecx,[esp+4]",
				"mov [eax],ecx",
				"add esp,8"}
				r = {
				"mov eax,$1",
				"pop ecx",
				"mov [eax],ecx"}
			elsif j=11 then
				pat = {
				"push /reg32/",
				"pop /reg32/"}
				r = {
				"mov $2,$1"}
			elsif j=12 then
				pat = {
				"mov eax,loopstackptr",
				"pop ecx",
				"pop edx",
				"add loopstackptr,8",
				"mov [eax],edx",
				"mov [eax+4],ecx"}
				r = {
				"mov eax,loopstackptr",
				"pop dword ptr [eax+4]",
				"pop dword ptr [eax]",
				"add loopstackptr,8"}
			elsif j=13 then
				pat = {
				"pushdw /imm/",
				"pushdw /imm/",
				"mov eax,loopstackptr",
				"pop dword ptr [eax+4]",
				"pop dword ptr [eax]"}
				r = {
				"mov eax,loopstackptr",
				"mov dword ptr [eax],$1",
				"mov dword ptr [eax+4],$2"}
			elsif j=14 then
				pat = {
				"neg ecx",
				"mov [esp],ecx",
				"pop eax",
				"cmp eax,-1"}
				r = {
				"add esp,4",
				"cmp ecx,1"}
			elsif j=15 then
				pat = {
				"pop ecx",
				"mov ebx,eax",
				"lea eax,[ecx-/imm/]",
				"mov ecx,ebx",
				"push eax",
				"push ecx"}
				r = {
				"pop ecx",
				"sub ecx,$1",
				"push ecx",
				"push eax"}
			elsif j=16 then
				pat = {
				"pushdw /imm/",
				"push /var/",
				"add dword ptr [esp],/imm/",
				"mov eax,[esp]",
				"mov ecx,[esp+4]",
				"mov [eax],ecx",
				"add esp,8"}
				r = {
				"mov eax,$2",
				"add eax,$3",
				"mov dword ptr [eax],$1"}
			elsif j=18 then
				pat = {
				"mov eax,/imm/",
				"push dword ptr [esp+eax*4]"}
				r = {
				"push dword ptr [esp+%(#4*$1)]"}
			elsif j=17 then
				pat = {
				"mov ecx,/imm/",
				"shl dword ptr [esp],cl"}
				r = {
				"shl dword ptr [esp],$1"}
			elsif j=19 then
				pat = {
				"push /ind/",
				"mov eax,/imm/",
				"mov ecx,dword ptr [esp+eax*4]",
				"pop eax",
				"mov [ecx],ax"}
				r = {
				"mov eax,dword ptr [esp+%($2*#4-#4)]",
				"mov ecx,$1",
				"mov [eax],cx"}
			elsif j=20 then
				pat = {
				"push ecx",
				"add dword ptr [esp],/imm/",
				"pop eax"}
				r = {
				"lea eax,[ecx+$1]"}
			elsif j=21 then
				pat = {
				"pop eax",
				"pop ecx",
				"xchg eax,ecx"}
				r = {
				"pop ecx",
				"pop eax"}
			elsif j=22 then
				pat = {
				"mov eax,/ind/",
				"pop ecx",
				"xchg eax,ecx"}
				r = {
				"mov ecx,$1",
				"pop eax"}
			elsif j=23 then
				pat = {
				"push /ind/",
				"add dword ptr [esp],/imm/",
				"pop /reg32/"}
				r = {
				"mov $3,$1",
				"add $3,$2"}
			elsif j=24 then
				pat = {
				"pop /reg32/",
				"sub $1,/imm/",
				"push $1"}
				r = {
				"sub dword ptr [esp],$2"}
			elsif j=24 then
				pat = {
				"pop /reg32/",
				"add $1,/imm/",
				"push $1"}
				r = {
				"add dword ptr [esp],$2"}
			elsif j=25 then
				pat = {
				"push /reg32d/",
				"mov eax,/var/",
				"add [esp],eax",
				"mov eax,[esp]"}
				r = {
				"mov eax,$2",
				"add eax,$1",
				"push eax"}
			elsif j=26 then
				pat = {
				"push /reg32/",
				"push dword ptr [esp]"}
				r = {
				"push $1",
				"push $1"}
			elsif j=27 then
				pat = {
				"push /reg32/",
				"mov /reg32/,dword ptr [esp"}
				r = {
				"push $1",
				"mov $2,$1"}
			elsif j=28 then
				pat = {
				"mov eax,/var/",
				"push dword ptr [eax]",
				"mov eax,dword ptr [esp]",
				"and eax,/imm/",
				"add eax,/var/",
				"movsx ebx,byte ptr [eax]",
				"inc ebx",
				"mov eax,ebx",
				"add eax,[esp]",
				"mov ecx,$1",
				"add esp,4",
				"mov [ecx],eax"}
				r = {
				"mov eax,$1",
				"mov ebx,$2",
				"mov ecx,dword ptr [eax]",
				"and ebx,ecx",
				"add ebx,$3",
				"inc ecx",
				"movsx edx,byte ptr [ebx]",
				"add ecx,edx",
				"mov dword ptr [eax],ecx"}
			elsif j=29 then
				pat = {
				"mov ecx,[esp]",
				"mov ecx,[esp+4]",
				"mov [eax+ecx],cl"}
				r = {
				"mov ecx,[esp]",
				"mov edx,[esp+4]",
				"mov [eax+ecx],dl"}
			elsif j=30 then
				pat = {
				"push ebx",
				"mov eax,/var/",
				"mov ecx,[esp]",
				"mov edx,[esp+4]",
				"mov [eax+ecx],dl",
				"add esp,8"}
				r = {
				"mov eax,$1",
				"mov ecx,dword ptr [esp]",
				"mov [eax+ebx],cl",
				"add esp,4"}
			elsif j=31 then
				pat = {
				"push ebx",
				"mov ecx,ebx",
				"mov eax,/var/",
				"mov [eax],ecx",
				"mov eax,/var/",
				"pop ecx"}
				r = {
				"mov edx,$1",
				"mov ecx,ebx",
				"mov eax,$2",
				"mov dword ptr [edx],ebx"}
			elsif j=32 then
				pat = {
				"lea esp,[esp+4]",
				"sub dword ptr [esp],/imm/",
				"push /reg32/"}
				r = {
				"mov dword ptr [esp],$2",
				"sub dword ptr [esp+4],$1"}
			elsif j=33 then
				pat = {
				"mov eax,fpstackptr",
				"fld /qlit/",
				"add fpstackptr,8",
				"fstp qword ptr [eax]",
				"mov eax,fpstackptr",
				"fld qword ptr [eax-8]",
				"sub fpstackptr,8",
				"fstp /mem32/"}
				r = {
				"fld $1",
				"fstp $2"}
			elsif j=34 then
				pat = {
				"push /ind/",
				"shl dword ptr [esp],/imm/",
				"pop eax"}
				r = {
				"mov eax,$1",
				"shl eax,$2"}
			elsif j=35 then
				pat = {
				"add [esp],eax",
				"pop eax"}
				r = {
				"add eax,[esp]",
				"add esp,4"}
			elsif j=36 then
				pat = {
				"push /reg32d/",
				"shl dword ptr [esp],/imm/",
				"pop eax"}
				r = {
				"mov eax,$1",
				"shl eax,$2"}
			elsif j=37 then
				pat = {
				"mov eax,ebp",
				"sub eax,/imm/"}
				r = {
				"lea eax,[ebp-$1]"}
			elsif j=38 then
				pat = {
				"mov eax,ebp",
				"mov eax,dword ptr [eax-/imm/]"}
				r = {
				"mov eax,dword ptr [ebp-$1]"}
			elsif j=39 then
				pat = {
				"push /var/",
				"mov eax,/ind/",
				"add eax,[esp]",
				"add esp,4"}
				r = {
				"mov eax,$1",
				"add eax,$2"}
			elsif j=40 then
				pat = {
				"push /ind/",
				"mov eax,/ind/",
				"add [esp],eax",
				"pop /reg32/"}
				r = {
				"mov $3,$1",
				"add $3,$2"}
			elsif j=41 then
				pat = {
				"mov /reg32/,fpstackptr",
				"fld /mem/",
				"add fpstackptr,8",
				"fstp qword ptr [$1]",
				"mov /reg32/,fpstackptr",
				"fld /mem/",
				"add fpstackptr,8",
				"fstp qword ptr [$3]",
				"mov eax,fpstackptr",
				"xor ecx,ecx",
				"fld qword ptr [eax-8]",
				"fld qword ptr [eax-16]",
				"sub esp,4",
				"sub fpstackptr,16",
				"fcomip st(0),st(1)"}
				r = {
				"fld $4",
				"fld $2",
				"xor ecx,ecx",
				"sub esp,4",
				"fcomip st(0),st(1)"}
			elsif j=42 then
				pat = {
				"push /reg32/",
				"mov eax,/imm/",
				"and [esp],eax",
				"pop eax"}
				r = {
				"mov eax,$1",
				"and eax,$2"}
			elsif j=43 then
				pat = {
				"sub esp,/imm/",
				"add esp,$1"}
				r = {}
			elsif j=44 then
				pat = {
				"pushdw /imm/",
				"pushdw /imm/",
				"mov eax,ebp",
				"pop ecx",
				"pop edx",
				"add ebp,8",
				"mov [eax],edx",
				"mov [eax+4],ecx"}
				r = {
				"mov dword ptr [ebp],$1",
				"mov dword ptr [ebp+4],$2",
				"add ebp,8"}
			elsif j=45 then
				pat = {
				"push /var/",
				"mov eax,/ind/",
				"shl eax,2",
				"add eax,[esp]",
				"add esp,4",
				"fld dword ptr [eax]"}
				r = {
				"mov ecx,$1",
				"mov eax,$2",
				"fld dword ptr [ecx+eax*4]"}
			elsif j=46 then
				pat = {
				"fld /mem/",
				"fldz",
				"xor ecx,ecx",
				"sub esp,4",
				"fcomip st(0),st(1)",
				"ffree st(0)",
				"set/cond/ cl",
				"fincstp",
				"add esp,4",
				"cmp ecx,1",
				"jne /label/"}
				r = {
				"fld $1",
				"fldz",
				"fcomip st(0),st(1)",
				"ffree st(0)",
				"fincstp",
				"jn$2 $3"}
			elsif j=47 then
				pat = {
				"fld /mem/",
				"fldz",
				"xor ecx,ecx",
				"sub esp,4",
				"fcomip st(0),st(1)",
				"ffree st(0)",
				"setn/cond/ cl",
				"fincstp",
				"add esp,4",
				"cmp ecx,1",
				"jne /label/"}
				r = {
				"fld $1",
				"fldz",
				"fcomip st(0),st(1)",
				"ffree st(0)",
				"fincstp",
				"j$2 $3"}
			elsif j=48 then
				pat = {
				"lea esp,[esp+4]",
				"pop ecx",
				"mov edx,eax",
				"lea eax,[ecx-/imm/]",
				"mov ecx,edx",
				"push eax",
				"push ecx"}
				r = {
				"mov dword ptr [esp],eax",
				"sub dword ptr [esp+4],$1"}
			elsif j=49 then
				pat = {
				"push /reg32d/",
				"mov eax,2",
				"imul dword ptr [esp]",
				"lea esp,[esp+4]"}
				r = {
				"lea eax,[$1*2]"
				}
			elsif j=50 then
				pat = {
				"push /reg32d/",
				"mov eax,4",
				"imul dword ptr [esp]",
				"lea esp,[esp+4]"}
				r = {
				"lea eax,[$1*4]"
				}
			elsif j=51 then
				pat = {
				"push /reg32d/",
				"mov eax,8",
				"imul dword ptr [esp]",
				"lea esp,[esp+4]"}
				r = {
				"lea eax,[$1*8]"
				}

			elsif j=55 then
				pat = {
				"pop /reg32d/",
				"push eax"}
				r = {
				"mov $1,dword ptr [esp]",
				"mov dword ptr [esp],eax"}

			elsif j=111 then
				pat = {
				"push /reg32d/",
				"mov eax,/imm/",
				"and [esp],eax",
				"pop eax"}
				r = {
				"mov eax,$2",
				"and eax,$1"}
			elsif j=112 then
				pat = {
				"pop ebx",
				"push eax",
				"mov ecx,ebx",
				"xchg eax,ecx"}
				r = {
				"mov ecx,eax",
				"xchg eax,[esp]"}
			elsif j=999 then
				pat = {
				"pushdw /imm/",
				"inc dword ptr [esp]"}
				r = {
				"pushdw %($1+#1)"}
			end if
			
			p = 1
			while 1 do
				if p+7>length(subject) then
					exit
				end if
				n = 0
				patvars = {}
				for i=1 to length(pat) do
					if not compare_patterns(subject[p+i-1],pat[i]) then
						n = i
						exit
					end if
				end for
				if not n then
					s = {}
					for i=1 to length(r) do
						s = pattern_append(s,r[i])
					end for
					if p>1 and p+length(pat)<=length(subject) then
						subject = subject[1..p-1] & s & subject[p+length(pat)..length(subject)]
						linesRemoved += length(pat)-length(r)
					end if
				end if
				p += 1
			end while
		end for
		end if
		
		if optLevel>=6 then
		
		--? {0,1,0}
		p = length(subject)
		while p<length(subject) do
			if length(subject[p])>6 then
				if equal(subject[p][1..6],"@@lbl_") and not find('r',subject[p]) then
					n = 1
					for i=p+1 to length(subject) do
						if length(subject[i])>5 then
							if equal(subject[i][1..5],"call ") then
								n = 0
								exit
							end if
							if length(subject[i])>6 then
								if equal(subject[i][1..6],"@@lbl_") and find('r',subject[i]) then
									if equal(subject[p],subject[i][1..find('r',subject[i])-2]&':') then
										n = 2
										m = i
										exit
									end if
								end if
							end if
						end if
					end for
					if n=2 and 0 then
						if equal(subject[p+1],"sub ipstackptr,4") then
							if equal(subject[m+2],"add ipstackptr,4") then
								subject = subject[1..m] & {"mov eax,returnaddress","jmp eax"} & subject[m+4..length(subject)]
								subject = subject[1..p] & {"pop dword ptr [returnaddress]"} & subject[p+4..length(subject)]
								linesRemoved += 3
								p = m-3
							else
								subject = subject[1..m] & {"mov ecx,returnaddress","pop eax","jmp ecx"} & subject[m+7..length(subject)]
								subject = subject[1..p] & {"pop dword ptr [returnaddress]"} & subject[p+4..length(subject)]
								linesRemoved += 5
								p = m-5
							end if							
						end if
					end if
				end if
			end if
			p += 1
		end while

		
		-- This removes redundant loop counter checks in innermost loops
		p = 1
		m = 0
		while p<length(subject) do
			if length(subject[p])>6 then
				if equal(subject[p][1..7],"@@loop_") and not find('e',subject[p]) then
					m = p
				elsif equal(subject[p][1..7],"@@loop_") and m then
					n = p
					q = m
					o = 0
					reachable = 1
					
					while m<n do

						if length(subject[m])>6 then
							if equal(subject[m][1..7],"cmp esi") then
								if subject[m+1][1] = 'j' then
									if reachable>0 then
										if m=q+1 then
											o = 1
											exit
										end if
									end if
								end if
							elsif equal(subject[m][1..4],"@@if") then
								if find('t',subject[m]) then
									reachable += 1
								else
									reachable -= 1
								end if
							elsif equal(subject[m][1..4],"@@ca") then
								if find('n',subject[m]) then
									reachable += 1
								else
									reachable -= 1
								end if
							end if
						end if
						m += 1
					end while
					
					if o then
						if equal(subject[n-2],"cmp esi,edi") then
							subject = subject[1..n-3] & {"jmp "&subject[n][1..11]} & subject[n..length(subject)]
							linesRemoved += 1
							p = n
						end if
					end if
					m = 0
				end if
			end if
			p += 1
		end while


		-- This code tries to put stack variables in registers in innermost loops
		p = 1
		m = 0
		while p<length(subject) do
			if length(subject[p])>6 then
				if equal(subject[p][1..7],"@@loop_") and not find('e',subject[p]) then
					m = p
				elsif equal(subject[p][1..7],"@@loop_") and m then
					n = p
					q = m
					o = 0
					clean = 1
					r = {0,0,0,0,0,0,0,0}
					
					while m<n do
						if length(subject[m])>6 then
							if equal(subject[m][1..7],"add esp") then
								clean = 0
								exit 
							elsif equal(subject[m][1..7],"sub esp") then
								clean = 0
								exit
							elsif equal(subject[m][1..7],"lea esp") then
								clean = 0
								exit
							elsif subject[m][1] = 'f' or subject[m][1] = 'p' then
								clean = 0
								exit
							end if
						end if
						i1 = find('x',subject[m])
						if i1 then
							i2 = find(subject[m][i1-2..i1],regs32)
							if i2 then
								r[i2] = 1
							end if
						end if
						m += 1
					end while
					
					if clean then
						m = q
						o = 0
						while o!=-4 and m>=2 do
							m -= 1
							if length(subject[m])>6 then
								if equal(subject[m][1..3],"pop") then
									o += 4
								elsif equal(subject[m][1..4],"push") then
									o -= 4
								elsif equal(subject[m][1..7],"add esp") then
									s = value(subject[m][9..length(subject[m])])
									o += s[2]
								elsif equal(subject[m][1..7],"sub esp") then
									s = value(subject[m][9..length(subject[m])])
									o -= s[2]
								elsif equal(subject[m][1..7],"lea esp") then
									s = value(subject[m][9..length(subject[m])])
									if find('+',subject[m]) then
										o += s[2]
									else
										o -= s[2]
									end if
								end if
							end if
						end while
						if o=-4 then
							--puts(1,subject[m]&"\n")
							i1 = find(0,r)
							o = 0
							if i1<4 then
								--puts(1,regs32[i1]&" is free\n")
								if equal(subject[m][1..4],"push") then
									if subject[m][5]='d' then
										subject[m] = "mov "&regs32[i1]&","&subject[m][8..length(subject[m])]									
									else
										subject[m] = "mov "&regs32[i1]&","&subject[m][5..length(subject[m])]
									end if
									for i=m+1 to n-1 do
										if length(subject[i])>3 then
											if equal(subject[i][1..4],"push") then
												o -= 4
											elsif equal(subject[i][1..3],"pop") then
												o += 4
											end if
										end if
										--if o<=0 then
										if o=0 then
											i2 = find('[',subject[i]) 
											if i2 then
												if equal(subject[i][i2..i2+4],"[esp]") then
													if equal(subject[i][i2-4..i2-2],"ptr") then
														i3 = i2-10
														i4 = i2+4
													else
														i3 = i2
														i4 = i2+4
													end if
													if i4<length(subject[i]) then
														t = subject[i][i4+1..length(subject[i])]
													else
														t = {}
													end if
													subject[i] = subject[i][1..i3-1] & regs32[i1] & t
												elsif equal(subject[i][i2..i2+4],"[esp+") then
													if equal(subject[i][i2-4..i2-2],"ptr") then
														i3 = i2-10
													else
														i3 = i2
													end if
													i4 = find(']',subject[i])
													if i4<length(subject[i]) then
														t = subject[i][i4+1..length(subject[i])]
													else
														t = {}
													end if
													u = value(subject[i][i2+5..i4-1])
													if -u[2]=o then
														subject[i] = subject[i][1..i3-1] & regs32[i1] & t
													else
														if u[2]-4 = 0 then
															subject[i] = subject[i][1..i2+3] & subject[i][i4..length(subject[i])]
														else
															subject[i] = subject[i][1..i2+4] & sprintf("%d",u[2]-4) & subject[i][i4..length(subject[i])]
														end if
													end if	
												end if
											end if
										elsif o<0 then
											i2 = find('[',subject[i]) 
											if i2 then
												if equal(subject[i][i2..i2+4],"[esp+") then
													if equal(subject[i][i2-4..i2-2],"ptr") then
														i3 = i2-10
													else
														i3 = i2
													end if
													i4 = find(']',subject[i])
													if i4<length(subject[i]) then
														t = subject[i][i4+1..length(subject[i])]
													else
														t = {}
													end if
													u = value(subject[i][i2+5..i4-1])
													if -u[2]=o then
														subject[i] = subject[i][1..i3-1] & regs32[i1] & t
													else
														if u[2]+o = 0 then
															subject[i] = subject[i][1..i2+3] & subject[i][i4..length(subject[i])]
														else
															subject[i] = subject[i][1..i2+4] & sprintf("%d",u[2]+o) & subject[i][i4..length(subject[i])]
														end if
													end if
												end if
											end if
										else	
											exit
										end if
									end for
									
									for i=n+1 to length(subject) do
										if length(subject[i])>6 then
											if equal(subject[i][1..3],"pop") then
												subject[i] = "mov "&subject[i][5..length(subject[i])]&","&regs32[i1]
												exit
											--elsif equal(subject[m][1..4],"push") then
											--	o -= 4
											elsif equal(subject[i][1..7],"add esp") then
												subject = subject[1..i-1]&subject[i+1..length(subject)]
												linesRemoved += 1
												exit
											--elsif equal(subject[m][1..7],"sub esp") then
											--	s = value(subject[m][9..length(subject[m])])
											elsif equal(subject[i][1..7],"lea esp") then
												subject = subject[1..i-1]&subject[i+1..length(subject)]
												linesRemoved += 1
												exit
											end if
										end if
									end for
						
								end if			
							end if
						end if
					end if
					m = 0
				end if
			end if
			p += 1
		end while

		p = 1
		while p<length(subject) do
			if equal(subject[p],"imul ebx") then
				if equal(subject[p+1],"mov ebx,eax") then
					subject = subject[1..p-1] & {"imul ebx,eax"} & subject[p+2..length(subject)]
					linesRemoved += 1
				end if
			end if
			p += 1
		end while
		
		end if
		
	end if
	
	return subject
end function


