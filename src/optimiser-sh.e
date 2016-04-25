-- ForthEC SuperH code optimiser 
-- /Mic, 2004/2005


include parser.e
include forthec.e


-- SH data registers registers
constant regssh = {"r0","r1","r2,","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12","r13","r14","r15"}

-- Condition codes ("nz" is left out because of how these are used in the patterns below)
--constant conds_68k = {"gt","ge","lt","le","eq"}

        

-- Integer instruction patterns to scan for
--
constant intpatterns_sh = {
{
	{
	"mov.l r0,@-r10",
	"mov #/imm/,r0",
	"mov.l @r10+,r0"
	},
	{
	}
},
{
	{
	"extu.w r0,r0",
	"and #/imm/,r0"
	},
	{
	2
	}
},
{
	{
	"mov.l @r10,r1",
	"mov.l r0,@-r10",
	"mov r1,r0",
	"mov r0,r1",
	"add r0,r0",
	"add #/imm/,r0",
	"mov.l @r10+,r1",
	"mov.w r1,@r0"
	},
	{
	{"mov r0,r1"},
	{"mov.l @r10,r0"},
	5,
	6,
	8
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov #/imm/,r0",
	"mov.l @r10+,r1",
	"add r1,r0",
	"add r1,r0"
	},
	{
	{"mov r0,r1"},
	{"add r0,r0"},
	{"add #$1,r0"}
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov #255,r0",
	"mov r0,r1",
	"mov.l @r10+,r0",
	"cmp//pz r1",
	"bt /label/"
	},
	{
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov.l /literal/,r0",
	"mov.l @r10+,r1",
	"mov.w r1,@r0",
	"mov.l @r10+,r0"
	},
	{
	{"mov.l $1,r1"},
	{"mov.w r0,@r1"},
	5
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov r6,r0",
	"add r0,r0",
	"mov.l @r10+,r1"
	},
	{
	{"mov r0,r1"},
	2,
	3
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov.l /literal/,r0",
	"mov.l @r10+,r1"
	},
	{
	{"mov r0,r1"},
	2
	}
},


{
	{
	"cmp//eq #/imm/,r0",
	"negc r1,r0",
	"mov r0,r1",
	"mov.l @r10+,r0",
	"cmp//pz r1",
	"bf /label/"
	},
	{
	1,
	4,
	{"bt $2"}
	}
},

{
	{
	"mov #0,r1",
	"cmp//eq #/imm/,r0",
	"negc r1,r0",
	"not r0,r0",
	"mov r0,r1",
	"mov.l @r10+,r0",
	"cmp//pz r1",
	"bt /label/"
	},
	{
	2,
	6,
	{"bt $2"}
	}
},
{
	{
	"mov #0,r1",
	"cmp//eq #/imm/,r0",
	"negc r1,r0",
	"mov r0,r1",
	"mov.l @r10+,r0",
	"cmp//pz r1",
	"bt /label/"
	},
	{
	2,
	5,
	{"bf $2"}
	}
},
{
	{
	"mov.l r0,@-r10",	--addr
	"mov #/imm/,r0",
	"mov.l @r10,r1",	--addr
	"mov.l r0,@-r10",
	"mov r1,r0",		--addr
	"mov.l r0,@-r10",
	"mov #/imm/,r0",
	"mov.l @r10+,r1",	--addr
	"add r1,r0",		--addr+imm2
	"mov.l @r10+,r1",
	"mov.w r1,@r0",		--*addr+imm2=imm1
	"mov.l @r10+,r0"
	},
	{
	{"mov #$2,r1"},
	{"mov #$1,r2"},

	{"add r0,r1"},
	{"mov.w r2,@r1"}
	}
},
{
	{
	"mov.l @r10,r1",
	"mov r0,r2",
	"mov r1,r0",
	"add #/imm/,r0",
	"mov r2,r1"
	},
	{
	{"mov r0,r1"},
	{"mov.l @r10,r0"},
	4
	}
},

{
	{
	"mov.l r0,@-r10",
	"mov r1,r0",
	"mov.l r0,@-r10",
	"mov #/imm/,r0",
	"mov.l @r10+,r1",
	"add r1,r0",
	"mov.l @r10+,r1"
	},
	{
	{"mov r0,r2"},
	{"mov r1,r0"},
	{"add #$1,r0"},
	{"mov r2,r1"}
	}
},

{
	{
	"cmp//eq #/imm/,r0",
	"negc r1,r0",
	"not r0,r0",
	"mov r0,r1",
	"mov.l @r10+,r0",
	"cmp//pz r1",
	"bf /label/"
	},
	{
	1,
	5,
	{"bf $2"}
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov.l /literal/,r0",
	"mov.l /literal/,r1",
	"mov.w r0,@r1",
	"mov.l @r10+,r0"
	},
	{
	{"mov.l $1,r2"},
	3,
	{"mov.w r2,@r1"}
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov #/imm/,r0",
	"mov.l /literal/,r1",
	"mov.w r0,@r1",
	"mov.l @r10+,r0"
	},
	{
	{"mov #$1,r2"},
	3,
	{"mov.w r2,@r1"}
	}
},
{
	{
	"mov.l r0,@-r10",
	"mov #0,r0",
	"mov r0,r1",
	"mov.l @r10+,r0",
	"cmp//pz r1",
	"bt /label/"
	},
	{
	{"bra $1"},
	{"nop"}
	}
},

{
	{
	"mov.l r0,@-r10",
	"mov.l /literal/,r0",
	"mov.l r0,@-r10",
	"mov #/imm/,r0",
	"mov.l r7,@-r12",
	"mov.l r6,@-r12",
	"mov.l @r10+,r7",
	"mov r0,r6",
	"mov.l @r10+,r0"
	},
	{
	{"mov #$2,r6"},
	{"mov.l $1,r7"}
	}
},

{
	{
	"mov.l r0,@-r10",
	"mov #/imm/,r0",
	"mov.l @r10+,r1",
	"and r1,r0"
	},
	{
	{"and #$1,r0"}
	}
}

}



constant intpatterns2_sh = {
{
	{
	"mov.l @r10+,r0",
	{"bt /label/","bf /label/"}
	},
	{
	{2,1,2,"/s $1"},
	1
	}
},
{
	{
	"mov.l @r10+,r0",
	"add #1,r6",
	"cmp//eq r7,r6",
	{"bt /label/","bf /label/"}
	},
	{
	2,
	3,
	{4,1,2,"/s $1"},
	1
	}
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
--function is_push(sequence s)
--	if length(s)>4 then
--		if equal(s[1..4],"push") then
--			return 1
--		elsif length(s)>19 then
--			if equal(s[1..19],"mov dword ptr [esp]") then
--				return 2
--			end if
--		end if
--	end if
--	return 0
--end function


function is_shpush(sequence s)
--	integer p
--
--	if length(s)>=4 then
--		if equal(s[1..4],"str ") then
--			p = find('!',s) 
--			if p > 0 then
--				if equal(s[p-8..p-1],"r10,#-4]") then
--					return 1
--				end if
--			end if
--		end if
--	end if
	return 0
end function


function is_shpop(sequence s)
--	integer p
--	
--	if (length(s)>=4) then
--		if equal(s[1..4],"ldr ") then
--			p = find(']',s)
--			if p > 0 and length(s)=p+3 then
--			 	if equal(s[p-4..p+3],"[r10],#4") then
--					return 1
--				end if
--			end if
--		end if
--	end if
	return 0
end function

									

function get_reg2_sh(sequence s)
	integer p,q
	
	p = find(',',s)
	q = find(')',s)
	if p>q then
		return s[p+1..length(s)]
	else
		return s[find(',',s[q+1..length(s)])+1..length(s)]
	end if
end function


function get_reg1_sh(sequence s)
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



function compare_patterns_sh(sequence s1,sequence s2)
	integer eql,p1,p2,p3,m,n,o
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
			p3 = p2	
			while s2[p2] != '/' do
				s3 &= s2[p2]
				p2 += 1
			end while
			if p3=p2 then
				s3 &= '/'
				if p1<=length(s1) then
					if s1[p1]!='/' then
						eql = 0
						exit
					else
						p1 += 1
					end if
				else
					eql = 0
					exit
				end if
			end if
			p2 += 1
			
			if p2 != p3+1 then
			
			s4 = {}
			while p1<=length(s1) do
				if s1[p1]=',' or
				   s1[p1]=']' then
					exit
				end if
				s4 &= s1[p1]
				p1 += 1
			end while
			
			--if equal(s3,"dreg") then
			--	if find(s4,dregs) then
			--		patvars = append(patvars,s4)
			--	else
			--		eql = 0
			--		exit
			--	end if
			if equal(s3,"reg") then
				if find(s4,regssh) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if

			elsif equal(s3,"const") then
				--s4 &= ']'
				eql = 0
				if find('_',s4) and length(s4)>=6 then
					if equal("const_",s4[1..6]) then
						eql = 1
						--p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			elsif equal(s3,"var") then
				--s4 &= ']'
				eql = 0
				if find('_',s4) then
					if equal("var_",s4[1..4]) then
						eql = 1
						--p1 += 1
						patvars = append(patvars,s4)
					end if
				end if
			--elsif equal(s3,"ind") then
			--	s4 &= ']'
			--	eql = 0
			--	if length(s4)>12 then
			--		if equal("dword ptr [e",s4[1..12]) then
			--			eql = 1
			--			p1 += 1
			--			patvars = append(patvars,s4)
			--		end if
			--	end if
			--elsif equal(s3,"qlit") then
			--	s4 &= ']'
			--	eql = 0
			--	if find('_',s4) then
			--		if equal("qword ptr [lit_",s4[1..15]) then
			--			eql = 1
			--			p1 += 1
			--			patvars = append(patvars,s4)
			--		end if
			--	end if
			elsif equal(s3,"imm") then
				o = 0
				--if length(s4)>6 then
				--	if equal(s4[1..6],"offset") then
				--		patvars = append(patvars,s4)
				--		o = 1
				--	end if
				--end if
				if o=0 then
					s3 = value(s4)
					if s3[1]=GET_SUCCESS and integer(s3[2]) then
						patvars = append(patvars,s4)
					else
						eql = 0
					end if
				end if
			--elsif equal(s3,"cond") then
			--	if find(' ',s4) then
			--		p1 -= length(s4)+1-find(' ',s4)
			--		--puts(1,"s1[p1] = "&s1[p1]&"\n")
			--		s4 = s4[1..find(' ',s4)-1]
			--	end if
			--	if find(s4,conds_68k) then
			--		patvars = append(patvars,s4)
			--		--puts(1,s4&"*\n")
			--	else
			--		eql = 0
			--	end if
			elsif equal(s3,"label") then
				if s4[1]='_' then
					patvars = append(patvars,s4)
				else
					eql = 0
				end if
			elsif equal(s3,"literal") then
				if length(s4)>3 then
					if equal(s4[1..3],"lit") then
						patvars = append(patvars,s4)
					else
						eql = 0
					end if
				else
					eql = 0
				end if
			elsif equal(s3,"if") then
				if s4[1]='_' then
					patvars = append(patvars,s4)
				else
					eql = 0
				end if				
			--elsif equal(s3,"mem32") then
			--	s4 &= ']'
			--	eql = 0
			--	if length(s4)>11 then
			--		if equal("dword ptr [",s4[1..11]) then
			--			eql = 1
			--			p1 += 1
			--			patvars = append(patvars,s4)
			--		end if
			--	end if
			--elsif equal(s3,"mem") then
			--	s4 &= ']'
			--	eql = 0
			--	if length(s4)>11 then
			--		if equal("dword ptr [",s4[1..11]) or
			--		   equal("qword ptr [",s4[1..11]) then
			--			eql = 1
			--			p1 += 1
			--			patvars = append(patvars,s4)
			--		end if
			--	end if
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



function pattern_append_sh(sequence s1,sequence s2)
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
					elsif oper='&' then
						expval = and_bits(expval,s4[2])
					elsif oper='|' then
						expval = or_bits(expval,s4[2])
					elsif oper='^' then
						expval = xor_bits(expval,s4[2])
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
				elsif s2[p2] = '&' then
					oper = s2[p2]
					p2 += 1
				elsif s2[p2] = '|' then
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




function pattern_optimise_sh(sequence subject,sequence patterns,integer maxIterations)
	integer i1,i2,i3,i4,p,q,n,m,o,clean,improvement,times,reachable,newlen
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

						if sequence(pat[1][j][1]) then
							m = 0
							for k=1 to length(pat[1][j]) do
								if compare_patterns_sh(subject[p+j-1],pat[1][j][k]) then
									m = k
									exit
								end if
							end for
							if not m then
								n = 0
								exit
							end if
						else
							if not compare_patterns_sh(subject[p+j-1],pat[1][j]) then
								n = 0
								exit
							end if
						end if
					end for

					-- Was a matching pattern found ?
					if n then
						if i=9999 then --i=5 or i=6 then
							if equal(patvars[1],patvars[2]) then
								linesRemoved += 5
								subject = subject[1..p-1] & subject[p+5..length(subject)]
							else
								linesRemoved += 4
								subject = subject[1..p-1] & {"b "&patvars[3]} & subject[p+5..length(subject)]
							end if
						else
							s = {}
							newlen = 0
							for j=1 to length(pat[2]) do
								if sequence(pat[2][j]) then
									if sequence(pat[2][j][1]) then
										s = pattern_append_sh(s,pat[2][j][1])
										newlen += 1
									else
										if pat[2][j][1] = COND then
											if pat[2][j][2] = NUMRANGE then
												t = value(patvars[pat[2][j][3]])
												if t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5] and pat[2][j][6] then
													s = pattern_append_sh(s,pat[2][j][7])
													newlen += 1
												elsif (not (t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5])) and pat[2][j][6]=0 then
													s = pattern_append_sh(s,pat[2][j][7])
													newlen += 1
												end if
											elsif pat[2][j][2] = LESSBITSSET then
												t = value(patvars[pat[2][j][3]])
												u = value(patvars[pat[2][j][4]])
												if count_bits(t[2])<count_bits(u[2]) and pat[2][j][6] then
													s = pattern_append_sh(s,pat[2][j][7])
													newlen += 1
												elsif count_bits(t[2])>=count_bits(u[2]) and pat[2][j][6]=0 then
													s = pattern_append_sh(s,pat[2][j][7])
													newlen += 1
												end if
											end if

										else
											s = pattern_append_sh(s,subject[p+pat[2][j][1]-1][pat[2][j][2]..pat[2][j][3]]&pat[2][j][4])
											newlen += 1
										end if
									end if
								else
									s = pattern_append_sh(s,subject[p+pat[2][j]-1])
									newlen += 1
								end if
							end for
							subject = subject[1..p-1] & s & subject[p+length(pat[1])..length(subject)]
							linesRemoved += length(pat[1])-newlen --length(pat[2])
						end if
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
		

	return subject
end function



global function optimise_sh(sequence subject,integer remConst)
	integer i1,i2,i3,i4,p,q,n,m,o,clean,improvement,times,reachable
	sequence s,r,t,u,pat
	integer regsAreFree,isInnermost

	i1 = 0
	i2 = 0
	i3 = 0
	m = 0
	n = 0
	o = 0
	p = 0
	q = 0
	improvement = 0
	
	if remConst and 0 then
		-- Remove unused constants
		for i=1 to length(constants[2]) do
			if constants[2][i][2]=0 then
				--subject = subject[1..constants[2][i][3]-1] & {"add esp,4"} & subject[constants[2][i][3]+2..length(subject)]
				--constRemoved += 1
				--for j=i+1 to length(constants[2]) do
				--	if constants[2][j][3]>constants[2][i][3] then
				--		constants[2][j][3] -= 1
				--	end if
				--end for
			end if
		end for
	end if


	if optLevel >= 5 and 0 then
		if remConst then
			constlist = {}
			for i=1 to length(subject) do
				patvars = {}
				--if compare_patterns_sh(subject[i],"ldr r1,/const/") then
				--	if equal(subject[i+1],"str r0,[r1]") then
				--		constlist = append(constlist,patvars&{i,1,1})
				--	elsif equal(subject[i+1],"str r2,[r1]") then
				--		constlist = append(constlist,patvars&{i,2,1})
				--	end if
				--end if
			end for
		end if

		--patvars = {}
		--for i=1 to length(constlist) do
		--	patvars = constlist[i][1]
		--	for j=1 to length(subject) do
		--		if equal(subject[j],"push "&patvars[1]) then
		--			subject[j] = "pushdw "&patvars[2]
		--			constlist[i][4] = 0
		--		elsif compare_patterns_sh(subject[j][4..length(subject[j])]," /reg32/,$1") then
		--			subject[j] = subject[j][1..find(',',subject[j])-1]&","&patvars[2]
		--			patvars = patvars[1..2]
		--			constlist[i][4] = 0
		--		end if
		--	end for
		--end for

		--p=2
		--while 1 do
		--	if p+2>length(subject) then
		--		exit
		--	end if
		--	if equal(subject[p],"sub esp,4") then
		--		patvars = {}
		--		if compare_patterns_sh(subject[p+1],"mov dword ptr [esp],/imm/") then
		--			subject = subject[1..p-1] & {"pushdw "&patvars[1]} & subject[p+2..length(subject)]
		--			linesRemoved += 1
		--		end if
		--	end if
		--	p += 1
		--end while
	end if		
	
	
	if optLevel >= 5 then
		-- Run up to 10 passes
		subject = pattern_optimise_sh(subject,intpatterns_sh,10)

		-- This code removes unnecessary return stack operations
		p = 1
		while p<length(subject) do
			if equal(subject[p],"sts.l pr,@-r11") then
				m = p
				q = p
				clean = 1
				while q<=length(subject) do
					if equal(subject[q],"lds.l @r11+,pr") then
						exit
					elsif compare_patterns_sh(subject[q],"bsr /label/") then
						clean = 0
						exit
					end if
					q += 1
				end while
				if clean then
					subject = subject[1..m-1]&subject[m+1..q-1]&subject[q+1..length(subject)]
					p = q
					linesRemoved += 2
				end if
			end if
			p += 1
		end while
					

		-- This code tries to move constant assignments out of innermost loops
		times = 2
		while times>0 do
			improvement = 0 
			p = 1
			m = 0

			while p<length(subject) do
				if length(subject[p])>6 then
					if equal(subject[p][1..7],"__loop_") and not find('e',subject[p]) then
						m = p

					elsif equal(subject[p][1..7],"__loop_") and m then
						-- Do the label numbers match?
						t = subject[p]
						if equal(subject[p][8..11],subject[m][8..11]) then
							n = p
							q = m
							o = 0
							clean = 1

							-- Mark all registers as unused
							r = repeat(-1,16) 	-- Unused / constant / variable
							u = repeat({},16)	-- Offsets
							t = r			-- Values
							
							r[5] = 0
							r[6] = 0
							o = 0

							while m<n do
								if length(subject[m])>6 then
									patvars = {}
									if compare_patterns_sh(subject[m],"mov.l /literal/,/reg/") then
										o = find(0,r)
										if o>0 then
											--r[o] = 1
											t[o] = patvars[2]
											u[o] = m
											--subject[m] = sprintf("mov r%d,"&patvars[2],o-1)
										else
											--exit
										end if
									--elsif compare_patterns_sh(subject[m],"mov
									end if
									
									patvars = {}
									if compare_patterns_sh(subject[m],"bsr /label/") then
										r = repeat(-1,16)
										exit
									end if
								end if
								m += 1
							end while
							
							--puts(1,subject[q]&"\n")
							for i=1 to 16 do
								if r[i]=1 then
							--		printf(1,"%s has constant value %s\n",{regs68k[i],t[i]})
									subject = subject[1..q-1] & {"mov.l "&t[i]&","&regssh[i]} & subject[q..length(subject)]
									q += 1
									u += 1
									subject[u[i]] = sprintf("mov r%d,"&t[i],i-1)
									improvement = 1
									--linesRemoved += length(u[i])-1
									--? u[i]
								end if
							end for
							for i=1 to 16 do
								if r[i]=1 then
									--for j=1 to length(u[i]) do
										--subject = subject[1..u[i][j]-1] & subject[u[i][j]+1..length(subject)]
										-- Adjust subsequent offsets
										--for k=i+1 to 16 do
										--	if r[k]=1 then
										--		for l=1 to length(u[k]) do
										--			if u[k][l]>u[i][j] then
										--				u[k][l] -= 1
										--			end if
										--		end for
										--	end if
										--end for
									--end for
								end if
							end for
						end if
						m = 0
					end if
					--m = 0
				end if
				p += 1
			end while
			
			-- Stop when there's nothing more to optimise
			--improvement -= linesRemoved
			if not improvement then
				exit
			end if
			--if find(0,r)<=0 then
			--	exit
			--end if
			times -= 1			
		end while
		
		
		subject = pattern_optimise_sh(subject,intpatterns2_sh,4)
	end if
	
	
	return subject
end function


