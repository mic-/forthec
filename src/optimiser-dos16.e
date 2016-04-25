-- ForthEC x86-16 code optimiser 
-- /Mic, 2005


include parser.e


-- x86 registers
constant regs32 = {"eax","ebx","ecx","edx","esi","edi","ebp","esp"}
constant regs16 = {"ax","bx","cx","dx","si","di","bp","sp"}
constant regs8 = {"al","ah","bl","bh","cl","ch","dl","dh"}

-- x86 condition codes ("nz" is left out because of how these are used in the patterns below)
constant conds = {"g","ge","l","le","z"}

constant COND = -2,
         NUMRANGE = -3
         
         

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
	{"push byte /imm/","push word /imm/"},
	"pop /reg16/"
	},
	{
	{"mov $2,$1"}
	}
},
{
	{
	"push word [bx+/imm/]",
	"pop /reg16/"
	},
	{
	{"mov $2,[bx+$1]"}
	}
},
{
	{
	"push /reg16/",
	"pop $1"
	},
	{
	}
},
{
	{
	"push /reg16/",
	"pop /reg16/"
	},
	{
	{"mov $2,$1"}
	}
},
-- 5
{
	{
	"mov ax,/imm/",
	"mov bp,sp",
	{"add [bp],ax","sub [bp],ax"}
	},
	{
	2,
	{3,1,3," word [bp],$1"}
	}
},
{
	{
	"mov bx,sp",
	"mov bx,[bx+2]",
	"pop ax",
	"mov [bx],ax",
	"mov bp,sp",
	"add word [bp],/imm/"
	},
	{
	1,
	3,
	{"pop bp"},
	{"mov [bp],ax"},
	{"add bp,$1"},
	{"mov [bx],bp"}
	}
},
{
	{
	"push word /imm/",
	"mov si,/imm/",
	"add word [loopstackptr],4",
	"pop di"
	},
	{
	2,
	3,
	{"mov di,$1"}
	}
},
-- 8
{
	{
	"push word /var/",
	"pop /reg16/"
	},
	{
	{"mov $2,$1"}
	}
},
{
	{
	{"push byte /imm/","push word /imm/"},
	{"push byte /imm/","push word /imm/"},
	"mov bx,[loopstackptr]",
	"pop cx",
	"pop dx",
	"add word [loopstackptr],4",
	"mov [bx],dx",
	"mov [bx+2],cx"
	},
	{
	3,
	6,
	{"mov word [bx],$1"},
	{"mov word [bx+2],$2"}
	}
},
{
	{
	"push ax",
	"mov /reg16d/,/var/",
	"pop ax"
	},
	{
	2
	}
},
{
	{
	"mov ax,/imm/",
	"mov word /const/,ax"
	},
	{
	{"mov word $2,$1"}
	}
},
{
	{
	"mov ax,[dictptr]",
	"mov /var/,ax",
	"add ax,2",
	"mov /var/,ax",
	"add word [dictptr],/imm/",
	"mov ax,[dictptr]",
	"mov /var/,ax",
	"add word [dictptr],2"
	},
	{
	1,
	2,
	3,
	4,
	{"add word [dictptr],%($3 + #2)"},
	{"add ax,2"},
	7
	}
},
{
	{
	"mov ax,[dictptr]",
	"mov /var/,ax",
	"add word [dictptr],/imm/",
	"mov ax,[dictptr]",
	"mov /var/,ax",
	"add word [dictptr],2"
	},
	{
	1,
	2,
	{"add ax,2"},
	5,
	{"add word [dictptr],%($2 + #2)"}
	}
},

-- 12
{
	{
	"xor cx,cx",
	"sub sp,2",	
	"cmp ax,/imm/",
	"setz cl",
	"mov bx,sp",
	"neg cx",
	"mov [bx],cx",
	"pop ax",
	"cmp ax,-1",
	"jnz /label/"
	},
	{
	3,
	10
	}
},
{
	{
	"mov bx,/var/",
	"movzx ax,byte [bx]",
	"push ax",
	"mov bp,sp",
	{"inc word [bp]","dec word [bp]"},
	"mov bx,$1",
	"pop ax",
	"mov [bx],al"
	},
	{
	1,
	{5,1,3," byte [bx]"}
	}
},
{
	{
	"mov bx,/var/",
	"movzx ax,byte [bx]",
	"push ax",
	"mov bp,sp",
	{"add word [bp],/imm/","sub word [bp],/imm/"},
	"mov bx,$1",
	"pop ax",
	"mov [bx],al"
	},
	{
	1,
	{5,1,3," byte [bx],$2"}
	}
},
{
	{
	"push word /var/",
	"mov bx,sp",
	"push word [bx]"
	},
	{
	{"mov ax,$1"},
	{"push ax"},
	{"push ax"}
	}
},
{
	{
	"push ax",
	"mov bx,sp",
	"push word [bx]"
	},
	{
	1,
	1
	}
},
{
	{
	"mov ax,/imm/",
	"mov bx,sp",
	{"and [bx],ax","or [bx],ax","xor [bx],ax"}
	},
	{
	2,
	{3,1,3," word [bx],$1"}
	}
},
{
	{
	"push ax",
	"mov bx,sp",
	{"and word [bx],/imm/","or word [bx],/imm/","xor word [bx],/imm/"},
	"mov bp,sp",
	{"dec word [bp]","inc word [bp]"},
	"pop ax"
	},
	{
	{3,1,3," ax,$1"},
	{5,1,3," ax"}
	}
},
{
	{
	"mov bx,/var/",
	"movzx ax,byte [bx]",
	"mov bp,sp",
	"add [bp],ax",
	"pop bx"
	},
	{
	{"mov bp,$1"},
	{"pop bx"},
	{"movzx ax,[bp]"},
	{"add bx,ax"}
	}
},
{
	{
	"mov bx,/var/",
	"movzx ax,byte [bx]",
	"push ax",
	"mov bx,/var/",
	"movzx ax,byte [bx]",
	"mov bp,sp",
	"add [bp],ax",
	"pop ax"
	},
	{
	1,
	{"mov bp,$2"},
	2,
	{"movzx cx,byte [bp]"},
	{"add ax,cx"}
	}
},
{
	{
	"sub word [loopstackptr],4",
	"mov si,0",
	"add word [loopstackptr],4"
	},
	{
	2
	}
},
{
	{
	"push /reg16/",
	"mov dx,/imm/",
	"pop ax",
	"out dx,al"
	},
	{
	{"mov ax,$1"},
	2,
	4
	}
},
{
	{
	"push /reg16/",
	"mov ax,/imm/",
	"pop cx",
	"cmp cx,ax"
	},
	{
	{"cmp $1,$2"}
	}
},
{
	{
	"mov cx,/imm/",
	"mov bp,sp",
	{"shr word [bp],cl","shl word [bp],cl"}
	},
	{
	2,
	{3,1,3," word [bp],$1"}
	}
},
{
	{
	"push byte /imm/",
	"mov bx,/var/",
	"pop ax",
	"mov [bx],ax"
	},
	{
	2,
	{"mov word [bx],$1"}
	}
},
{
	{
	"mov bx,/var/",
	"push word [bx]",
	"mov bx,/var/",
	"pop ax",
	"mov [bx],al"
	},
	{
	1,
	{"mov bp,$2"},
	{"mov ax,[bx]"},
	{"mov [bp],al"}
	}
},
{
	{
	"mov bp,sp",
	{"add [bp],ax",	"sub [bp],ax"},
	"pop /reg16d/"
	},
	{
	3,
	{2,1,3," $1,ax"}
	}
},
{
	{
	"mov bp,sp",
	{"add [bp],ax",	"sub [bp],ax"},
	"pop ax"
	},
	{
	{"pop bx"},
	{2,1,3," ax,bx"}
	}
},
{
	{
	"push cx",
	"mov bp,/var/",
	"pop bx"
	},
	{
	{"mov bx,cx"},
	2
	}
},
{
	{
	"push word /const/",
	"pop /reg16/"
	},
	{
	{"mov $2,$1"}
	}
},
{
	{
	"push /reg16d/",
	"mov ax,/const/",
	"pop cx",
	"cmp cx,ax"
	},
	{
	{"cmp $1,$2"}
	}
},
{
	{
	"push word /var/",
	"mov ax,si",
	"pop bx",
	"add bx,ax"
	},
	{
	{"mov bx,$1"},
	{"add bx,si"}
	}
},
{
	{
	"add bx,si",
	"pop ax",
	"mov [bx],al"
	},
	{
	2,
	{"mov [bx+si],al"}
	}
},
{
	{
	"push byte /imm/",
	"mov /reg16d/,/var/",
	"pop ax"
	},
	{
	2,
	{"mov ax,$1"}
	}
},
{
	{
	"push word [bx]",
	"pop /reg16/"
	},
	{
	{"mov $1,[bx]"}
	}
},
{
	{
	"push word /ind/",
	"pop /reg16/"
	},
	{
	{"mov $2,$1"}
	}
},
{
	{
	"mov ax,/imm/",
	"mov /ind/,al"
	},
	{
	{"mov byte $2,$1"}
	}
}
	

}


--? length(intpatterns)

	
linesRemoved = 0
constRemoved = 0
constlist = {}

integer showCmp


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


function is_armpush(sequence s)
	integer p

	if length(s)>=4 then
		if equal(s[1..4],"str ") then
			p = find('!',s) 
			if p > 0 then
				if equal(s[p-8..p-1],"r10,#-4]") then
					return 1
				end if
			end if
		end if
	end if
	return 0
end function


function is_armpop(sequence s)
	integer p
	
	if (length(s)>=4) then
		if equal(s[1..4],"ldr ") then
			p = find(']',s)
			if p > 0 and length(s)=p+3 then
			 	if equal(s[p-4..p+3],"[r10],#4") then
					return 1
				end if
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
			elsif equal(s3,"reg16d") then
				if find(s4,regs16[2..length(regs16)]) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			elsif equal(s3,"reg32d") then
				if find(s4,regs32[2..length(regs32)]) then
					--puts(1,s4&"\n")
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			elsif equal(s3,"const") then
				s4 &= ']'
				eql = 0
				if find('_',s4) and length(s4)>=6 then
					if equal("const_",s4[2..7]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"var") then
				s4 &= ']'
				--puts(1,s4&"\n")
				eql = 0
				if find('_',s4) then
					if equal("var_",s4[2..5]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"ind") then
				s4 &= ']'
				eql = 0
				if length(s4)>4 then
					if equal("[b",s4[1..2]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"qlit") then
				s4 &= ']'
				eql = 0
				if find('_',s4) then
					if equal("qword [lit_",s4[1..15]) then
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
			elsif equal(s3,"if") then
				if s4[1]='_' then
					patvars = append(patvars,s4)
				else
					eql = 0
				end if				
			elsif equal(s3,"mem32") then
				s4 &= ']'
				eql = 0
				if length(s4)>11 then
					if equal("dword [",s4[1..11]) then
						eql = 1
						p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"mem") then
				s4 &= ']'
				eql = 0
				if length(s4)>11 then
					if equal("dword [",s4[1..11]) or
					   equal("qword [",s4[1..11]) then
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
			--if length(s4)>11 then
				--if equal(s4[7..9],"ptr") then
				--	s4 &= ']'
				--end if
				if find('[',s4)>0 and find(']',s4)<=0 then
					s4 &= ']'
				end if
			--end if

			--if showCmp then
			--	puts(1,s3&", "&s4&", ")
			--end if
			
			if not equal(s3,s4) then
				if s3[1]='_' then
					if not equal(s3&':',s4) then
						eql = 0
					end if
				else
					eql = 0
				end if
			end if
				
		else
			if s1[p1]!=s2[p2] then
				eql = 0
			end if
			p1 += 1
			p2 += 1
		end if
	end while
	
	if showCmp then
		puts(1,s1&" == "&s2&" ?\n")
	end if
	
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




function pattern_optimise(sequence subject,sequence patterns,integer maxIterations)
	integer i1,i2,i3,i4,p,q,n,m,o,clean,improvement,times,reachable
	sequence s,r,t,u,pat

	while maxIterations>0 do
		improvement = linesRemoved

		-- Replace common instruction patterns by more compact forms
		p = 1
		while p<=length(subject) do
			for i=1 to length(patterns) do
				pat = patterns[i]
				if p+length(pat[1])<=length(subject)+1 then
					patvars = {}
					n = 1
					for j=1 to length(pat[1]) do
						showCmp = 0
						--if i=length(patterns) then
						--	--if j>1 then ? j end if
						--	if j=6 then
						--		--puts(1,patvars[1]&"\n")
						--		--puts(1,pat[1][j]&"\n")
						--		--puts(1,subject[p+j-1]&"\n")
						--		showCmp = 1
						--	end if
						--end if
						
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
						--if i=5 or i=6 then
						--	if equal(patvars[1],patvars[2]) then
						--		linesRemoved += 5
						--		subject = subject[1..p-1] & subject[p+5..length(subject)]
						--	else
						--		linesRemoved += 4
						--		subject = subject[1..p-1] & {"b "&patvars[3]} & subject[p+5..length(subject)]
						--	end if
						--else
							s = {}
							for j=1 to length(pat[2]) do
								if sequence(pat[2][j]) then
									if sequence(pat[2][j][1]) then
										s = pattern_append(s,pat[2][j][1])
									else
										if pat[2][j][1] = COND then
											if pat[2][j][2] = NUMRANGE then
												t = value(patvars[pat[2][j][3]])
												if t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5] and pat[2][j][6] then
													s = pattern_append(s,pat[2][j][7])
												elsif (not (t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5])) and pat[2][j][6]=0 then
													s = pattern_append(s,pat[2][j][7])
												end if
											end if

										else
											s = pattern_append(s,subject[p+pat[2][j][1]-1][pat[2][j][2]..pat[2][j][3]]&pat[2][j][4])
										end if
									end if
								else
									s = pattern_append(s,subject[p+pat[2][j]-1])
								end if
							end for
							subject = subject[1..p-1] & s & subject[p+length(pat[1])..length(subject)]
							linesRemoved += length(pat[1])-length(pat[2])
						--end if
						exit
					end if
				end if
			end for
			p += 1
		end while


		-- Stop when there's nothing more to optimise
		improvement -= linesRemoved
		if not improvement then
			exit
		end if
		maxIterations -= 1
	end while
		
	--? maxIterations
	
	return subject
end function



global function optimise_dos16(sequence subject,integer remConst)
	integer i1,i2,i3,i4,p,q,n,m,o,clean,improvement,times,reachable
	sequence s,r,t,u,pat
	integer regsAreFree,isInnermost

	if remConst and 0 then
		-- Remove unused constants
		for i=1 to length(constants[2]) do
			if constants[2][i][2]=0 then
				--subject = subject[1..constants[2][i][3]-1] & {"add sp,2"} & subject[constants[2][i][3]+2..length(subject)]
				--constRemoved += 1
				--for j=i+1 to length(constants[2]) do
				--	if constants[2][j][3]>constants[2][i][3] then
				--		constants[2][j][3] -= 1
				--	end if
				--end for
			end if
		end for
	end if


	
	
	if optLevel >= 5 then
		-- Run up to 10 passes
		subject = pattern_optimise(subject,intpatterns,10)

	end if
	
	
	return subject
end function


