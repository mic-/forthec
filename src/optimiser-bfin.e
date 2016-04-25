-- ForthEC Blackfin code optimiser 
-- /Mic, 2004/2005


include parser.e
include forthec.e


-- Blackfin data registers
constant dregs_bfin = {"r0","r1","r2","r3","r4","r5","r6","r7"}

constant pregs_bfin = {"p0","p1","p2,","p3","p4","p5"}

constant allregs_bfin = dregs_bfin & pregs_bfin

-- Condition codes ("nz" is left out because of how these are used in the patterns below)
--constant conds_68k = {"gt","ge","lt","le","eq"}

        

-- Integer instruction patterns to scan for
--
--	dreg:	r0-r7
--	preg:	p0-p5
--	label:	_.*:?
--	imm:	-?[0-9]+
--	ext:	\(x\)|\(z\)|<nothing>
--	const:	const_[0-9A-F]+
--	var:	var_[0-9A-F]+
--
constant intpatterns_bfin = {
{
	{
	"r0 = /imm/;",
	"r0 = [p1 + /imm/];"
	},
	{
	2
	}
},
{
	{
	"r1 = [p1];",
	"[p1] = r0;",
	"r0 = r1;",
	"r0 <<= /imm/;",
	"r1 = [p1++];",
	"r0 = r0 | r1;"
	},
	{
	5,
	{"r1 <<= $1;"},
	6
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = 0;",
	"cc = r0==-1;",
	"r0 = [p1++];",
	"if !cc jump /label/"
	},
	{
	{"jump $1;"}
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"r6 = r6 + r0;",
	"r0 = [p1++];"
	},
	{
	{COND,NUMRANGE,1,0,63,1,"r6 += $1;"},
	{COND,NUMRANGE,1,0,63,0,"r1 = $1$2;"},
	{COND,NUMRANGE,1,0,63,0,"r6 = r6 + r1;"}	
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"r1 = [p1++];",
	"r0 = r0 + r1;"
	},
	{
	{COND,NUMRANGE,1,-64,63,1,"r0 += $1;"},
	{COND,NUMRANGE,1,-64,63,0,"r1 = $1$2;"},
	{COND,NUMRANGE,1,-64,63,0,"r0 = r0 + r1;"}	
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"r1 = [p1++];",
	"r0 = r1 - r0;"
	},
	{
	{COND,NUMRANGE,1,0,64,1,"r0 += -$1;"},
	{COND,NUMRANGE,1,-64,63,0,"r1 = $1$2;"},
	{COND,NUMRANGE,1,-64,63,0,"r0 = r0 - r1;"}
	}
},
{
	{
	"if !cc jump /label/;",
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"jump /label/;",
	"/label/:",
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"/label/:"
	},
	{
	2,
	3,
	{"r1 = $2$3;"},
	{"r2 = $6$7;"},
	{"if cc r0 = r1;"},
	{"if !cc r0 = r2;"}
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"r1 = [p1++];",
	{"r0 = r0 & r1;","r0 = r0 | r1;","r0 = r0 ^ r1;"}
	},
	{
	{"r1 = $1$2;"},
	5
	}
},
{
	{
	"r1 = -1;",
	"r2 = 0;",
	"cc = r0==0;",
	"if cc r0 = r1;",
	"if !cc r0 = r2;",
	"cc = r0==-1;",
	{"r0 = /mem/;","r0 = /streg/;"},
	"if !cc jump /label/;"
	},
	{
	3,
	7,
	{"if !cc jump $2;"}
	}
},
{
	{
	"r1 = -1;",
	"r2 = 0;",
	"cc = r0==0;",
	"if !cc r0 = r1;",
	"if cc r0 = r2;",
	"cc = r0==-1;",
	{"r0 = /mem/;","r0 = /streg/;"},
	"if !cc jump /label/;"
	},
	{
	3,
	7,
	{"if cc jump $2;"}
	}
},
{
	{
	"r0 = /imm//ext/;",
	"p5 = r0;"
	},
	{
	{"p5 = $1$2;"}
	}
},
{
	{
	"/dreg/ = /size/[p5]/ext/;",
	"/dreg/ = /imm//ext/;",
	"r0 = r0 & /dreg/;",
	"cc = r0==0;",
	{"r0 = /mem/;","r0 = /streg/;"},
	"if cc jump /label/;"
	},
	{
	1,
	{COND,ISPOW2,5,0,31,1,"cc = bittst($1,$i);"},
	{COND,ISPOW2,5,0,31,0,"r1 = $5$6;"},
	{COND,ISPOW2,5,0,31,0,"cc = r0==0;"},
	5,
	{"if !cc jump $9"}
	}
},
{
	{
	"/dreg/ = /size/[p5]/ext/;",
	"/dreg/ = /imm//ext/;",
	"r0 = r0 & /dreg/;",
	"cc = r0==0;",
	{"r0 = /mem/;","r0 = /streg/;"},
	"if !cc jump /label/;"
	},
	{
	1,
	{COND,ISPOW2,5,0,31,1,"cc = bittst($1,$i);"},
	{COND,ISPOW2,5,0,31,0,"r0 = $5$6;"},
	{COND,ISPOW2,5,0,31,0,"cc = r0==0;"},
	5,
	{"if cc jump $9"}
	}
},
{
	{
	"r0.h = /var/;",
	"r0.l = $1;",
	"p5 = r0;",
	"r0 = [p5];"
	},
	{
	{"p5.h = $1;"},
	{"p5.l = $1;"},
	4
	}
},
{
	{
	"r0.h = /var/;",
	"r0.l = $1;",
	"r1 = [p1++];",
	"p5 = r0;",
	"[p5] = r1;",
	"r0 = [p1++];"
	},
	{
	{"p5.h = $1;"},
	{"p5.l = $1;"},
	3,
	5,
	6
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r1 = [p1++];"
	},
	{
	{"r1 = r0;"}
	}
},
{
	{
	"r1 = [p1];",
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = r1;",
	"r1 = [p1];",
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = r1;"
	},
	{
	1,

	{"p1 += -8"},
	{"[p1] = r1;"},
	{"[p1+4] = r0;"}
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"p3 += -8;",
	"[p3] = r6;",
	"[p3 + 4] = r7;",
	"r6 = r0;",
	"r7 = [p1++];",
	"r0 = [p1++];"
	},
	{
	7,
	8,
	9,
	{"r6 = $3$4;"},
	{"r7 = $1$2;"}
	}
},
{
	{
	"r1 = [p1];",
	"[p1] = r0;",
	"r0 = r1;",
	"r0 += /imm/;",
	"p5 = r0;",
	"r0 = /size/[p5]/ext/;"
	},
	{
	{"p5 = [p1];"},
	2,
	{"r0 = $2[p5 + $1]$3;"}
	}
},
{
	{
	"r0 = r1;",
	"r1 = /imm//ext/;",
	"r0 = r0 + r1;",
	"p5 = r0;",
	"r0 = /size/[p5]/ext/;"
	},
	{
	{"p5 = r1;"},
	{"p4 = $1$2;"},
	{"p5 = p5 + p4;"},
	5
	}
},
{
	{
	"r0 = [p1 + /imm/];",
	"r1 = [p1++];",
	"p5 = r0;",
	"/size/[p5] = r1;",
	"r0 = [p1++];"
	},
	{
	{"p5 = [p1 + $1];"},
	2,
	4,
	5
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"cc = r0==0;",
	"r0 = [p1++];"
	},
	{
	3
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 <<= /imm/;",
	"r1 = [p1++];",
	"r0 = r0 | r1;"
	},
	{
	{"r1 = r0;"},
	3,
	5
	}
},
--{
--	{
--	"r0 = /dreg/;",
--	"r0 <<= /imm/;"
--	},
--	{
--	{"r0 = $1 << $2;"}
--	}
--},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"p1 += -4;",
	"[p1] = r0;"
	},
	{
	{"p1 += -8;"},
	{"[p1] = r0;"},
	{"[p1+4] = r0;"}
	}
},
{
	{
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"r1 = [p1];",
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = r1;",
	"r1 = [p1++];",
	"p5 = r0;",
	"/size/[p5] = r1;",
	"r0 = [p1++];"
	},
	{
	{"r1 = $1$2;"},
	{"p5 = r0;"},
	{"$3[p5] = r1;"}
	}
}
}

constant bfin_patterns_3 = {
{
	{
	"r0 = /size/[p5]/ext/;",
	"/streg/ = r0;",
	"r0 = /imm//ext/;"
	},
	{
	{"$3 = $1[p5]$2;"},
	3
	}
},
{
	{
	"/dreg/ = /dreg/;",
	"$1 = /dreg/;"
	},
	{
	2
	}
},
{
	{
	"r1 = /streg/;",
	{"r0 = r0 & r1;","r0 = r0 | r1;","r0 = r0 ^ r1;"}
	},
	{
	{2,1,10,"$1;"}
	}
},
{
	{
	"/streg/ = r0;",
	"r0 = /imm//ext/;",
	"r1 = $1;",
	"/streg/ = r0;",
	"r0 = r1;",
	"r1 = $4;",
	"p5 = r0;",
	"/size/[p5] = r1;",
	"r0 = r3;"
	},
	{
	{"r1 = $2$3;"},
	7,
	8
	}
},
{
	{
	"/streg/ = r0;",
	"r0 = /imm//ext/;",
	"r1 = $1;",
	"r0 = r0 + r1;"
	},
	{
	{COND,NUMRANGE,1,0,63,1,"r0 += $2;"},
	{COND,NUMRANGE,1,0,63,0,"r1 = $2$3;"},
	{COND,NUMRANGE,1,0,63,0,"r0 = r0 + r1;"}		
	}
},
{
	{
	"r1 = r3;",
	"r4 = r0;",
	"r0 = r1;",
	"p1 += -4;",
	"[p1] = r0;",
	"r0 = /imm//ext/;",
	"r1 = r4;",
	"r0 = r0 + r1;",
	"r1 = r3;",
	"p5 = r0;",
	"/size/[p5] = r1;",
	"r0 = [p1++];"
	},
	{
	1,
	2,
	{"r0 = r3;"},
	{"r2 = $1$2;"},
	{"r2 = r2 + r4;"},
	{"p5 = r2;"},
	11
	}
}
	
}



constant intpatterns2_bfin = {
{
	{
	"/reg/ = /reg/;",
	"$2 = $1;"
	},
	{
	1
	}
},
{
	{
	"r1 = r2;",
	"r3 = r0;",
	"r0 = r1;",
	"r1 = /imm//ext/;"
	},
	{
	2,
	{"r0 = r2;"},
	4
	}
},
{
	{
	"/reg/ = /reg/;",
	"$1 = /reg/;"
	},
	{
	2
	}
},
{
	{
	"r0 = [p1++];",
	"p1 += -4;",
	"[p1] = r0;"
	},
	{
	}
},
{
	{
	"/reg/ = $1;"
	},
	{
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


								

--function get_reg2_bfin(sequence s)
--	integer p,q
--	
--	p = find(',',s)
--	q = find(')',s)
--	if p>q then
--		return s[p+1..length(s)]
--	else
--		return s[find(',',s[q+1..length(s)])+1..length(s)]
--	end if
--end function


--function get_reg1_bfin(sequence s)
--	integer p
--	
--	p = find(',',s)
--	if p then
--		return s[find(' ',s)+1..p-1]
--	else
--		return s[find(' ',s)+1..length(s)]
--	end if
--end function


--function get_imm(sequence s)
--	return s[find(',',s)+1..length(s)]
--end function




constant pattern_types = {
"dreg",
"preg",
"const",
"var",
"imm",
"label",
"literal",
"ext",
"streg",
"mem",
"size",
"reg",
"if"
}

function compare_patterns_bfin(sequence s1,sequence s2)
	integer eql,p1,p2,p3,p4,m,n,o,c
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
			p2 += 1
			
			if p2 != p3+1 then

---- New pattern matcher
			s4 = {}
			p4 = 0
			n = find(s3,pattern_types)
			eql = 0
			while p1 <= length(s1) do
				c = s1[p1]
				s4 &= c
				p4 += 1
				
				if n <= 0 then
					puts(1,"Internal error : Bad pattern: "&s3&"\n")
					exit

				elsif n = 1 then	-- dreg
					o = 0
					for i=1 to length(dregs_bfin) do
						if equal(s4,dregs_bfin[i]) then
							--patvars = append(patvars,s4)
							eql = 1
							exit
						-- Are there any strings left that could
						-- potentially match if we keep reading
						-- more characters?
						elsif p4 < length(dregs_bfin[i]) then
							o = 1
						end if
					end for
					if eql then exit end if
					if not o then exit end if
					
				elsif n = 2 then	-- preg
					o = 0
					for i=1 to length(pregs_bfin) do
						if equal(s4,pregs_bfin[i]) then
							--patvars = append(patvars,s4)
							eql = 1
							exit
						-- Are there any strings left that could
						-- potentially match if we keep reading
						-- more characters?
						elsif p4 < length(pregs_bfin[i]) then
							o = 1
						end if
					end for
					if eql then exit end if
					if not o then exit end if

				elsif n = 3 then	-- const
					if p4 = 6 then
						if not equal(s4,"const_") then
							exit
						else
							eql = 1
						end if
					elsif p4 > 6 then
						if not ( (c>='0' and c<='9') or (c>='A' and c<='F') ) then
							s4 = s4[1..p4-1]
							exit
						end if
					end if

				elsif n = 4 then	-- var
					if p4 = 4 then
						if not equal(s4,"var_") then
							exit
						else
							eql = 1
						end if
					elsif p4 > 4 then
						if not ( (c>='0' and c<='9') or (c>='A' and c<='F') ) then
							s4 = s4[1..p4-1]
							exit
						end if
					end if

				elsif n = 5 then	-- imm
					if not (c>='0' and c<='9') then
						if p4=1 and c='-' then
							eql = 1
						else
							if p4>1 then s4 = s4[1..p4-1] end if
							if eql then p1-=1 end if
							exit
						end if
					else
						eql = 1
					end if
					
				elsif n = 6 then	-- label
					if p4 = 1 then
						if c != '_' then
							exit
						else
							eql = 1
						end if
					else
						if c = ';' or
						   c = ',' or
						   c = ' ' then
							s4 = s4[1..p4-1]
							exit
						end if
					end if
					
				elsif n = 7 then	-- literal
				
				elsif n = 8 then	-- ext
					if p4 = 1 then
						if c != '(' then
							eql = 1
							s4 = ""
							p1 -= 1
							exit
						end if
					elsif p4=2 then
						if not (c = 'x' or c = 'z') then
							eql = 0
							exit
						end if
					else
						if c = ')' then
							eql = 1
							--puts(1,"Matched ext against "&s4&" ("&s2&")\n")
						end if
						exit
					end if
				
				elsif n = 9 then	-- streg
				--	puts(1,"Trying to match an streg\n")
					o = 0
					if p4 = 2 then
						if find(s4,{"r3","r4"}) then
							eql = 1
						end if
						exit
					end if
				
				elsif n = 10 then	-- mem
					if p4 = 1 then
						if c != '[' then
							exit
						end if
					elsif c = ']' then
						eql = 1
						exit
					end if
					
				elsif n = 11 then	-- size
					if c = 'b' or c = 'w' then
						eql = 1
						--puts(1,"Matched size against "&s4&" ("&s2&")\n")
						exit
					else
						s4 = ""
						p1 -= 1
						eql = 1
						exit
					end if
				
				elsif n = 12 then	-- reg
					o = 0
					for i=1 to length(allregs_bfin) do
						if equal(s4,allregs_bfin[i]) then
							eql = 1
							exit
						-- Are there any strings left that could
						-- potentially match if we keep reading
						-- more characters?
						elsif p4 < length(allregs_bfin[i]) then
							o = 1
						end if
					end for
					if eql then exit end if
					if not o then exit end if
					
				end if
				
				p1 += 1
			end while
			
			--if eql then
			--	puts(1,"matched "&s4&" against "&s3&"\n")
			--end if
			
			if eql then
				p1 += 1
				patvars = append(patvars,s4)
			end if
			
---- /New pattern matcher				
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
				   s1[p1]=';' or
				   s1[p1]=']' then
				   	if s1[p1]=']' then p1+=1 end if
					exit
				end if
				s4 &= s1[p1]
				p1 += 1
				if length(s4)=length(s3) then exit end if
			end while
			--if length(s4)>11 then
			--	if equal(s4[7..9],"ptr") then
			--		s4 &= ']'
			--	end if
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



function pattern_append_bfin(sequence s1,sequence s2)
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
				elsif s2[p2]='i' then
					n = -rangeIdx
					p2 += 1
					exit
				else
					exit
				end if
				p2 += 1
			end while
			if n>=0 then
				s3 &= patvars[n]
			else
				s3 &= sprintf("%d",-n)
			end if
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




function pattern_optimise_bfin(sequence subject,sequence patterns,integer maxIterations)
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
								if compare_patterns_bfin(subject[p+j-1],pat[1][j][k]) then
									m = k
									exit
								end if
							end for
							if not m then
								n = 0
								exit
							end if
						else
							if not compare_patterns_bfin(subject[p+j-1],pat[1][j]) then
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
										s = pattern_append_bfin(s,pat[2][j][1])
										newlen += 1
									else
										if pat[2][j][1] = COND then
											if pat[2][j][2] = NUMRANGE then
												t = value(patvars[pat[2][j][3]])
												if t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5] and pat[2][j][6] then
													s = pattern_append_bfin(s,pat[2][j][7])
													newlen += 1
												elsif (not (t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5])) and pat[2][j][6]=0 then
													s = pattern_append_bfin(s,pat[2][j][7])
													newlen += 1
												end if
											elsif pat[2][j][2] = LESSBITSSET then
												t = value(patvars[pat[2][j][3]])
												u = value(patvars[pat[2][j][4]])
												if count_bits(t[2])<count_bits(u[2]) and pat[2][j][6] then
													s = pattern_append_bfin(s,pat[2][j][7])
													newlen += 1
												elsif count_bits(t[2])>=count_bits(u[2]) and pat[2][j][6]=0 then
													s = pattern_append_bfin(s,pat[2][j][7])
													newlen += 1
												end if
											elsif pat[2][j][2] = ISPOW2 then
												t = value(patvars[pat[2][j][3]])
												--u = value(patvars[pat[2][j][4]])
												if find(t[2],pow2_32)>0 and pat[2][j][6] then
													rangeIdx = find(t[2],pow2_32)-1
													s = pattern_append_bfin(s,pat[2][j][7])
													newlen += 1
												elsif find(t[2],pow2_32)<=0 and pat[2][j][6]=0 then
													s = pattern_append_bfin(s,pat[2][j][7])
													newlen += 1
												end if
												
											end if

										else
											s = pattern_append_bfin(s,subject[p+pat[2][j][1]-1][pat[2][j][2]..pat[2][j][3]]&pat[2][j][4])
											newlen += 1
										end if
									end if
								else
									s = pattern_append_bfin(s,subject[p+pat[2][j]-1])
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



-- Responsible for removing unnecessary return stack operations
function optimise_return_stack_bfin(sequence subject)
	integer m,n,p,q
	integer clean
	
	p = 1
	while p<length(subject) do
		if equal(subject[p],"p2 += -4;") then
			m = p
			q = p
			clean = 1
			while q<=length(subject) do
				if equal(subject[q],"rets = r1;") then
					exit
				elsif compare_patterns_bfin(subject[q],"call /label/;") then
					clean = 0
					exit
				end if
				q += 1
			end while
			if clean then
				subject = subject[1..m-1]&subject[m+3..q-2]&subject[q+1..length(subject)]
				p = q-4
				linesRemoved += 5
			end if
		end if
		p += 1
	end while
	
	return subject
end function



-- Responsible for moving constant assignments out of innermost loops
function optimise_constant_assignments_bfin(sequence subject)
	integer l,m,n,o,p,q
	integer clean,times,improvement
	sequence r,s,t,u
	
	times = 3
	while times>0 do
		improvement = 0 
		p = 1
		m = 0

		while p<length(subject) do
			if length(subject[p])>7 then
				if (equal(subject[p][1..7],"__loop_") and not find('e',subject[p])) or
				   (equal(subject[p][1..5],"loop ")) then
					m = p

				elsif (equal(subject[p][1..7],"__loop_") or
				       equal(subject[p][1..8],"loop_end")) and m then
					-- Do the label numbers match?
					t = subject[p]
					if (length(subject[p])=21 and equal(subject[p][17..20],subject[m][13..16])) or
					   equal(subject[p][8..11],subject[m][8..11]) then
						n = p
						q = m
						o = 0
						clean = 1

						-- Mark all registers as unused
						r = repeat(-1,length(allregs_bfin)) 	-- Unused / constant / variable
						u = repeat({},length(r))		-- Offsets
						t = r					-- Values

						r[2] = 0
						r[3] = 0
						r[14] = 0
						--r[14] = 0

						o = 0

						while m<n do
							if length(subject[m])>6 then
								patvars = {}
								--if find(subject[m][length(subject[m])-1..length(subject[m])],regs68k) then
								--	s = {0,find(subject[m][length(subject[m])-1..length(subject[m])],regs68k)}
									patvars = {}
									if compare_patterns_bfin(subject[m],"/reg/ = /imm//ext/;") and
									   r[find(patvars[1],allregs_bfin)] != -1 then
									   	l = find(patvars[1],allregs_bfin)
									   	
										if r[l]=0 then
											t[l] = patvars[2]&patvars[3]
											u[l] = {m}
											r[l] = 1
										elsif equal(patvars[2]&patvars[3],t) then
											u[l] &= m
										else
											r[l] = -1
										end if
									else
									--patvars = {}
									--if compare_patterns_bfin(subject[m],"move.l #/const/,/reg/") and r[s[2]]!=-1 then
									--	if r[s[2]]=0 then
									--		t[s[2]] = patvars[1]
									--		u[s[2]] = {m}
									--		r[s[2]] = 1
									--	elsif equal(patvars[1],t) then
									--		u[s[2]] &= m
									--	else
									--		r[s[2]] = -1
									--	end if
									--else
									--	r[s[2]] = -1
									--end if
									end if
								--end if
								patvars = {}
								if compare_patterns_bfin(subject[m],"call /label/;") then
									r = repeat(-1,length(r))
									exit
								end if
							end if
							m += 1
						end while

						--puts(1,subject[q]&"\n")
						for i=1 to length(r) do
							if r[i]=1 then
								--printf(1,"%s has constant value %s\n",{regs68k[i],t[i]})
								subject = subject[1..q-1] & {sprintf(allregs_bfin[i]&" = ",i-1)&t[i]} & subject[q..length(subject)]
								q += 1
								u += 1
								improvement = 1
								linesRemoved += length(u[i])-1
								--? u[i]
							end if
						end for
						for i=1 to length(r) do
							if r[i]=1 then
								for j=1 to length(u[i]) do
									subject = subject[1..u[i][j]-1] & subject[u[i][j]+1..length(subject)]
									-- Adjust subsequent offsets
									for k=i+1 to length(r) do
										if r[k]=1 then
											for ll=1 to length(u[k]) do
												if u[k][ll] > u[i][j] then
													u[k][ll] -= 1
												end if
											end for
										end if
									end for
								end for
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
		
	return subject
end function



function optimise_register_usage_bfin(sequence subject)
	integer m,n,o,p,q,d
	integer loopStart,loopEnd,maxDepth
	integer clean,times,improvement
	sequence r,s,t,u,depth

	times = 3
	while times>0 do
		improvement = 0 
		p = 1
		loopStart = 0

		while p<length(subject) do
			if length(subject[p])>6 then
				if equal(subject[p][1..7],"__loop_") and not find('e',subject[p]) then
					loopStart = p

				elsif equal(subject[p][1..7],"__loop_") and loopStart then
					-- Do the label numbers match?
					t = subject[p]
					if equal(subject[p][8..11],subject[loopStart][8..11]) then
						loopEnd = p
						m = loopStart
						q = m
						o = 0
						clean = 1
						d = 0
						depth = {}
						maxDepth = 0
						
						-- Mark all registers as used
						r = repeat(1,length(allregs_bfin))
						
						r[3..5] = {0,0,0}
						
						o = 0

						while m<loopEnd do
							--if find('[',subject[m])>0 then
							patvars = {}
							depth &= d
							if compare_patterns_bfin(subject[m],"p1 += /imm/;") then
								s = value(patvars[1])
								if s[2]=-4 then
									d -= s[2]
									if d > maxDepth then
										maxDepth = d
									end if
								else
									printf(1,subject[loopStart]&": %d\n",s[2])
									clean = 0
									exit
								end if

							-- Don't allow LEAVE/?LEAVE
							elsif subject[m][1]='i' and
							      m<loopEnd-3 and
							      (equal(subject[m],"if cc jump "&subject[loopEnd][1..length(subject[loopEnd])-1]&";") or
							       equal(subject[m],"if !cc jump "&subject[loopEnd][1..length(subject[loopEnd])-1]&";")) then
								clean = 0
								exit
							elsif subject[m][1]='j' and
							      m<loopEnd-3 and
							      (equal(subject[m],"jump "&subject[loopEnd][1..length(subject[loopEnd])-1]&";")) then
								clean = 0
								exit

							elsif compare_patterns_bfin(subject[m],"call /label/;") then
								clean = 0
								exit
							elsif compare_patterns_bfin(subject[m],"/reg/ = [p1++];") then
								d -= 4
								r[find(patvars[length(patvars)],allregs_bfin)] = 1
							elsif compare_patterns_bfin(subject[m],"/reg/ = /reg/;") then
								r[find(patvars[length(patvars)-1],allregs_bfin)] = 1
							elsif compare_patterns_bfin(subject[m],"/reg/ = /imm//ext/;") then
								r[find(patvars[length(patvars)-2],allregs_bfin)] = 1
							end if
							
							m += 1
						end while

						--printf(1,subject[loopStart]&" (d=%d, max=%d)\n\t",{d,maxDepth})
						--? r
						
						if clean and d=0 and find(0,r)>0 then
							--puts(1,"\t")
							--? depth
							depth = floor(depth/4)+1
							s = {}
							for i=1 to length(r) do
								if r[i] = 0 then
									s &= i
								end if
							end for
							r = s
							
							m = loopStart
							n = 1
							while m<loopEnd do
								patvars = {}
								if compare_patterns_bfin(subject[m],"p1 += /imm/;") then
									s = value(patvars[1])
									--d -= floor(floor(s[2])/4)
									--if d > maxDepth then
									--	maxDepth = d
									--end if
									--if s[2] = -4 then
									--	if compare_patterns_bfin(subject[m+1],"[p1];")
									if s[2]=-4 and depth[n]<=length(r) then
										subject = subject[1..m-1]&subject[m+1..length(subject)]
										m -= 1
										linesRemoved += 1
										p -= 1
									end if
								elsif compare_patterns_bfin(subject[m],"[p1] = /reg/;") then
									if depth[n]>1 and depth[n]<=length(r)+1 then
										subject[m] = allregs_bfin[r[depth[n]-1]]&" = "&patvars[length(patvars)]&";"
									end if
								elsif compare_patterns_bfin(subject[m],"/reg/ = [p1];") then
									if depth[n]>1 and depth[n]<=length(r)+1 then
										subject[m] = patvars[length(patvars)]&" = "&allregs_bfin[r[depth[n]-1]]&";"
									end if
								
								elsif compare_patterns_bfin(subject[m],"/reg/ = [p1 + /imm/];") then
								elsif compare_patterns_bfin(subject[m],"[p1 + /imm/] = /reg/;") then
								elsif compare_patterns_bfin(subject[m],"/reg/ = [p1++];") then
									if depth[n]>1 and depth[n]<=length(r)+1 then
										subject[m] = patvars[length(patvars)]&" = "&allregs_bfin[r[depth[n]-1]]&";"
									end if
									--d -= 4
									--r[find(patvars[length(patvars)],allregs_bfin)] = 1
--								elsif compare_patterns_bfin(subject[m],"/reg/ = /reg/;") then
--									r[find(patvars[length(patvars)-1],allregs_bfin)] = 1
--								elsif compare_patterns_bfin(subject[m],"/reg/ = /imm//ext/;") then
--									r[find(patvars[length(patvars)-2],allregs_bfin)] = 1
								end if

								m += 1
								n += 1
							end while
								
						end if
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
		
	return subject
end function



global function optimise_bfin(sequence subject,integer remConst)
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
				--if compare_patterns_bfin(subject[i],"ldr r1,/const/") then
				--	if equal(subject[i+1],"str r0,[r1]") then
				--		constlist = append(constlist,patvars&{i,1,1})
				--	elsif equal(subject[i+1],"str r2,[r1]") then
				--		constlist = append(constlist,patvars&{i,2,1})
				--	end if
				--end if
			end for
		end if

	end if		
	
	
	if optLevel >= 5 then
		subject = pattern_optimise_bfin(subject,intpatterns_bfin,10)

		subject = optimise_return_stack_bfin(subject)

		subject = optimise_register_usage_bfin(subject)
		
		subject = optimise_constant_assignments_bfin(subject)
		
		subject = pattern_optimise_bfin(subject,intpatterns2_bfin,4)
	end if
	
	
	return subject
end function


