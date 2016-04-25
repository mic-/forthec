-- ForthEC M68k code optimiser 
-- /Mic, 2004/2005


include parser.e
include forthec.e


-- M68k data registers registers
constant dregs = {"d0","d1","d2","d3","d4","d5","d6","d7","d8"}
constant regs68k = {"d0","d1","d2","d3","d4","d5","d6","d7","a0","a1","a2","a3","a4","a5","a6","a7"}

-- M68k condition codes ("nz" is left out because of how these are used in the patterns below)
constant conds_68k = {"gt","ge","lt","le","eq"}

        

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
constant intpatterns_68k = {
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l (a2)+,d1",
	{"and.l d1,d0","eor.l d1,d0","add.l d1,d0"}
	},
	{
	{4,1,5," #$1,d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l d0,d1",
	"move.l (a2)+,d0",
	"cmp.l #-1,d1",
	"bne /label/"
	},
	{
	2,
	{"cmp.l #-1,d0"},
	6
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l d1,d0",
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l d0,d1",
	"move.l (a2)+,d2",
	"move.l (a2)+,d0",
	"cmp.l d1,d2",
	"b/cond/ /label/"
	},
	{
	{"cmp.l #$1,d1"},
	{"b$2 $3"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l (a2)+,d1",
	"sub.l d0,d1",
	"move.l d1,d0"
	},
	{
	{COND,NUMRANGE,1,1,8,1,"subq.l #$1,d0"},
	{COND,NUMRANGE,1,1,8,0,"sub.l #$1,d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"add.l d0,a1",
	"move.l (a2)+,d0"
	},
	{
	{"add.l #$1,a1"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l d1,d0",
	"move.l (a2)+,d1",
	"muls d1,d0"
	},
	{
	4
	}
},
{
	{
	"move.l (a2),d1",
	"move.l d0,(a2)",
	"move.l d1,d0",
	{"addq.l #/imm/,d0","subq.l #/imm/,d0"},
	"move.l (a2),d1",
	"move.l d0,(a2)",
	"move.l d1,d0"
	},
	{
	{4,1,6," #$1,(a2)"}
	}
},

{
	{
	"and.l #/imm/,d0",
	"and.l #/imm/,d0"
	},
	{
	{COND,LESSBITSSET,1,2,0,1,"and.l #$1,d0"},
	{COND,LESSBITSSET,1,2,0,0,"and.l #$2,d0"}
	}
},
{
	{
	"move.l #/imm/,d0",
	"cmp.l #$1,d0",
	"beq /label/"
	},
	{
	{"bra $2"}
	}
},
{
	{
	"move.l #/imm/,d0",
	"cmp.l #$1,d0",
	"bne /label/"
	},
	{
	}
},
{
	{
	"cmp.l #/imm/,d0",
	"sne d0",
	"ext.w d0",
	"ext.l d0",
	"move.l d0,d1",
	"move.l (a2)+,d0",
	"cmp.l #-1,d1",
	"bne /label/"
	},
	{
	{"move.l d0,d1"},
	6,
	{"cmp.l #$1,d1"},
	{"beq $2"}
	}
},
{
	{
	"cmp.l #/imm/,d0",
	"seq d0",
	"ext.w d0",
	"ext.l d0",
	"move.l d0,d1",
	"move.l (a2)+,d0",
	"cmp.l #-1,d1",
	"bne /label/"
	},
	{
	{"move.l d0,d1"},
	6,
	{"cmp.l #$1,d1"},
	{"bne $2"}
	}
},
{
	{
	"cmp.l #/imm/,d0",
	"seq d0",
	"ext.w d0",
	"ext.l d0",
	"move.l d0,d1",
	"move.l (a2)+,d0",
	"cmp.l #-1,d1",
	"beq /label/"
	},
	{
	{"move.l d0,d1"},
	6,
	{"cmp.l #$1,d1"},
	{"beq $2"}
	}
},
{
	{
	"move.l #/imm/,d0",
	"move.l (a2)+,d1",
	"lsl.l d0,d1",
	"move.l d1,d0"
	},
	{
	{COND,NUMRANGE,1,1,8,1,"move.l (a2)+,d0"},
	{COND,NUMRANGE,1,1,8,1,"lsl.l #$1,d0"},
	{COND,NUMRANGE,1,1,8,0,"move.l #$1,d1"},	
	{COND,NUMRANGE,1,1,8,0,"move.l (a2)+,d0"},	
	{COND,NUMRANGE,1,1,8,0,"lsl.l d1,d0"}	
	}
},
{
	{
	"move.l #/imm/,d0",
	"move.l (a2)+,d1",
	"lsr.l d0,d1",
	"move.l d1,d0"
	},
	{
	{COND,NUMRANGE,1,1,8,1,"move.l (a2)+,d0"},
	{COND,NUMRANGE,1,1,8,1,"lsr.l #$1,d0"},
	{COND,NUMRANGE,1,1,8,0,"move.l #$1,d1"},	
	{COND,NUMRANGE,1,1,8,0,"move.l (a2)+,d0"},	
	{COND,NUMRANGE,1,1,8,0,"lsr.l d1,d0"}	
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d1",
	"move.l (a2)+,d0",
	{"lsl.l d1,d0","lsr.l d1,d0"}
	},
	{
	2,
	4
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l (a2)+,d1",
	"or.l d1,d0"
	},
	{
	{"or.l #$1,d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.w (a2)+,(a0,d0)",
	"addq.l #/imm/,a2",
	"move.l (a2)+,d0"
	},
	{
	{"move.l #$1,a5"},
	{"move.w d0,(a5)"},
	--{"addq.l #$2,a2"},
	{"move.l (a2)+,d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l (a2)+,(a0,d0)",
	"move.l (a2)+,d0"
	},
	{
	{"move.l #$1,a5"},
	{"move.l d0,(a5)"},
	4
	}
},
{
	{
	"cmp.l #0,d0",
	"seq d0",
	"ext.w d0",
	"ext.l d0",
	"move.l (a2)+,d1",
	"cmp.l #-1,d1",
	"beq /label/"
	},
	{
	5,
	1,
	7
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l #/imm/,a5",
	{"move.l d0,(a5)","move.w d0,(a5)"},
	"move.l (a2)+,d0"
	},
	{
	{"move.l #$2,a5"},
	{4,1,6," #$1,(a5)"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"or.l #/imm/,d0",
	"move.l (a2)+,d1",
	"or.l d1,d0"
	},
	{
	{"or.l #%($1|$2),d0"}
	}
},
{
	{
	"move.l #/imm/,d0",
	{"move.b d0,(a1)+","move.w d0,(a1)+","move.l d0,(a1)+"}
	},
	{
	{2,1,6," #$1,(a1)+"}
	}
},
{	
	{
	"move.l d0,-(a2)",
	"move.l #/var/,d0",
	"move.l (a2)+,(a0,d0)",
	"move.l (a2)+,d0"
	},
	{
	{"move.l #$1,a5"},
	{"move.l d0,(a5)"},
	4
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l d1,d0",
	"move.w (a2)+,(a0,d0)",
	"addq.l #2,a2",
	"move.l (a2)+,d0"
	},
	{
	{"move.w d0,(a0,d1)"},
	--4,
	5
	}
},
{
	{
	"move.l #/var/,d0",
	"move.l (a0,d0),d0"
	},
	{
	{"move.l #$1,a5"},
	{"move.l (a5),d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l d6,d0",
	"lsl.l #/imm/,d0",
	"move.l (a2)+,d1",
	"add.l d1,d0"
	},
	{
	{"move.l d6,d1"},
	{"lsl.l #$1,d1"},
	{"add.l d1,d0"}
	}
},
{
	{
	"move.w (a2)+,(a0,d0)",
	"addq.l #2,a2"
	},
	{
	{"move.l (a2)+,d1"},
	{"move.w d1,(a0,d0)"}
	}
},
{
	{
	"move.b (a2)+,(a0,d0)",
	"addq.l #3,a2"
	},
	{
	{"move.l (a2)+,d1"},
	{"move.b d1,(a0,d0)"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l #/imm/,a5",
	"move.w #/imm/,(a5)",
	"move.l #/imm/,a5",
	"move.w d0,(a5)",
	"move.l (a2)+,d0"
	},
	{
	3,
	4,
	5,
	{"move.w #$1,(a5)"}
	}
},
{
	{
	"move.l d0,-(a2)",
	{"move.b #/imm/,(a1)+","move.w #/imm/,(a1)+","move.l #/imm/,(a1)+"},
	"move.l (a2)+,d0"
	},
	{
	2
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l d7,-(a4)",
	"move.l d6,-(a4)",
	"move.l (a2)+,d7",
	"move.l d0,d6",
	"move.l (a2)+,d0"
	},
	{
	5,
	6,
	{"move.l #$2,d6"},
	{"move.l #$1,d7"}
	}
},
{
	{
	"move.l d0,-(a2)",
	{"move.l (a4),d0"},
	"move.l (a2)+,d1",
	{"add.l d1,d0","and.l d1,d0","eor.l d1,d0"}
	},
	{
	{"move.l (a4),d1"},
	{4,1,5," d1,d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	{"move.l d6,d0"},
	"move.l (a2)+,d1",
	{"add.l d1,d0","and.l d1,d0","eor.l d1,d0"}
	},
	{
	{4,1,5," d6,d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #var_0000001,a5",
	"move.l d0,(a5)",
	"move.l (a2)+,d0"
	},
	{
	2,
	3
	}
},
{
	{
	"move.l (a2),d1",
	"move.l d0,(a2)",
	"move.l d1,d0",
	"move.l #/imm/,d1",
	{"lsl.l d1,d0","lsr.l d1,d0"},
	"move.l (a2)+,d1"
	},
	{
	{"move.l d0,d1"},
	{"move.l #$1,d2"},
	{"move.l (a2)+,d0"},
	{5,1,5," d2,d0"}
	}
},
{
	{
	"move.l (a2)+,d0",
	"move.l #/imm/,a5",
	"move.w d0,(a5)",
	"move.l (a2)+,d0",
	"move.l #$1,a5",
	"move.w d0,(a5)"
	},
	{
	2,
	1,
	3,
	4,
	6
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l #/imm/,a5",
	"move.w #/imm/,(a5)",
	"move.l #$2,a5",
	"move.w d0,(a5)",
	"move.l #$2,a5",
	"move.l (a2)+,d0"
	},
	{
	3,
	4,
	{"move.w #$1,(a5)"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l d0,d1",
	"move.l (a2)+,d2",
	"move.l (a2)+,d0",
	"cmp.l d1,d2",
	"b/cond/ /label/"
	},
	{
	{"cmp.l #$1,d0"},
	8
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l (a2),d1",
	"move.l d0,(a2)",
	"move.l d1,d0"
	},
	{
	{"move.l #$1,-(a2)"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l d1,d0",
	"move.l (a2)+,d1",
	"move.b d1,(a0,d0)",
	"move.l (a2)+,d0"
	},
	{
	{"move.b d0,(a0,d1)"},
	5
	}
},
{
	{
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l d0,d1",
	"move.l (a2)+,d0"
	},
	{
	{"move.l #$1,d1"}
	}
},
{
	{
	"move.l (a2),d1",
	"move.b d0,(a0,d1)",
	"move.l (a2)+,d0"
	},
	{
	{"move.l (a2)+,d1"},
	2,
	{"move.l d1,d0"}
	}
},
{
	{
	"move.l d0,-(a2)",
	"cmp.l #/imm/,d0",
	"seq d0",
	"ext.w d0",
	"ext.l d0",
	"move.l d0,d1",
	"move.l (a2)+,d0",
	"cmp.l #-1,d1",
	"beq /label/"
	},
	{
	2,
	9
	}
},
{
	{
	"move.l d1,d0",
	"move.l d0,d1"
	},
	{
	1
	}
},
{
	{
	"move.l d1,d0",
	"move.l (a2)+,d0"
	},
	{
	2
	}
},
{
	{
	"move.l d0,(a2)",
	"move.l (a2)+,d0"
	},
	{
	{"addq.l #4,a2"}
	}
},
{
	{
	"and.l #/imm/,d0",
	"cmp.l #0,d0"
	},
	{
	1
	}
},
	


{
	{
	"move.l d0,-(a2)",
	"move.l d0,-(a2)",
	"moveq.l #/imm/,d0",
	"move.l d0,d1",
	"move.l (a2)+,d2",
	"move.l (a2)+,d0",
	"cmp.l d1,d2",
	"b/cond/ /label/"
	},
	{
	{"cmp.l #$1,d0"},
	8
	}
},

{
	{
	"move.l d0,-(a2)",
	"move.l (a0,d0),d0",
	"move.l d0,-(a2)",
	"move.l #/imm/,d0",
	"move.l (a2)+,(a0,d0)",
	"move.l (a2)+,d0"
	},
	{
	{"move.l (a0,d0),d1"},
	{"move.l #$1,a5"},
	{"move.l d1,(a5)"}
	}
}

}



constant intpatterns2_68k = {
{
	{
	"move.l d0,-(a2)",
	"move.l (a2)+,d0"
	},
	{
	}
},
{	{
	"move.l (a4)+,d6",
	"move.l (a4)+,d7",
	"move.l d7,-(a4)",
	"move.l d6,-(a4)"
	},
	{
	}
},
{
	{
	"move.l #/imm/,/dreg/"
	},
	{
	{COND,NUMRANGE,1,-128,127,1,"moveq.l #$1,$2"},
	{COND,NUMRANGE,1,-128,127,0,"move.l #$1,$2"}
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


function is_68kpush(sequence s)
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


function is_68kpop(sequence s)
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




function compare_patterns_68k(sequence s1,sequence s2)
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
			
			if equal(s3,"dreg") then
				if find(s4,dregs) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			elsif equal(s3,"reg") then
				if find(s4,regs68k) then
					patvars = append(patvars,s4)
				else
					eql = 0
					exit
				end if
			--elsif equal(s3,"reg32d") then
			--	if find(s4,regs32[2..length(regs32)]) then
			--		--puts(1,s4&"\n")
			--		patvars = append(patvars,s4)
			--	else
			--		eql = 0
			--		exit
			--	end if
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
			elsif equal(s3,"cond") then
				if find(' ',s4) then
					p1 -= length(s4)+1-find(' ',s4)
					--puts(1,"s1[p1] = "&s1[p1]&"\n")
					s4 = s4[1..find(' ',s4)-1]
				end if
				if find(s4,conds_68k) then
					patvars = append(patvars,s4)
					--puts(1,s4&"*\n")
				else
					eql = 0
				end if
			elsif equal(s3,"label") then
				if s4[1]='_' then
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



function pattern_append_68k(sequence s1,sequence s2)
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




function pattern_optimise_68k(sequence subject,sequence patterns,integer maxIterations)
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
								if compare_patterns_68k(subject[p+j-1],pat[1][j][k]) then
									m = k
									exit
								end if
							end for
							if not m then
								n = 0
								exit
							end if
						else
							if not compare_patterns_68k(subject[p+j-1],pat[1][j]) then
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
										s = pattern_append_68k(s,pat[2][j][1])
										newlen += 1
									else
										if pat[2][j][1] = COND then
											if pat[2][j][2] = NUMRANGE then
												t = value(patvars[pat[2][j][3]])
												if t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5] and pat[2][j][6] then
													s = pattern_append_68k(s,pat[2][j][7])
													newlen += 1
												elsif (not (t[2]>=pat[2][j][4] and t[2]<=pat[2][j][5])) and pat[2][j][6]=0 then
													s = pattern_append_68k(s,pat[2][j][7])
													newlen += 1
												end if
											elsif pat[2][j][2] = LESSBITSSET then
												t = value(patvars[pat[2][j][3]])
												u = value(patvars[pat[2][j][4]])
												if count_bits(t[2])<count_bits(u[2]) and pat[2][j][6] then
													s = pattern_append_68k(s,pat[2][j][7])
													newlen += 1
												elsif count_bits(t[2])>=count_bits(u[2]) and pat[2][j][6]=0 then
													s = pattern_append_68k(s,pat[2][j][7])
													newlen += 1
												end if
											end if

										else
											s = pattern_append_68k(s,subject[p+pat[2][j][1]-1][pat[2][j][2]..pat[2][j][3]]&pat[2][j][4])
											newlen += 1
										end if
									end if
								else
									s = pattern_append_68k(s,subject[p+pat[2][j]-1])
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



function optimise_register_usage_68k(sequence subject)
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
						r = repeat(1,length(dregs))
						
						r[3..5] = {0,0,0}
						
						o = 0

						while m<loopEnd do
							--if find('[',subject[m])>0 then
							patvars = {}
							depth &= d
							if compare_patterns_68k(subject[m],"move.l /dreg/,-(a2)") then
								--s = value(patvars[1])
								--if s[2]=-4 then
									d += 4
									if d > maxDepth then
										maxDepth = d
									end if
								--else
								--	printf(1,subject[loopStart]&": %d\n",s[2])
								--	clean = 0
								--	exit
								--end if
							
							-- Don't allow LEAVE/?LEAVE
							elsif subject[m][1]='b' and
							      m<loopEnd-3 and
							      (equal(subject[m],"beq "&subject[loopEnd][1..length(subject[loopEnd])-1]) or
							       equal(subject[m],"bne "&subject[loopEnd][1..length(subject[loopEnd])-1]) or
							       equal(subject[m],"bra "&subject[loopEnd][1..length(subject[loopEnd])-1])) then
								clean = 0
								exit
							
							elsif compare_patterns_68k(subject[m],"lea /label/,a6") then
								clean = 0
								exit
							elsif compare_patterns_68k(subject[m],"move.l (a2)+,/dreg/") then
								d -= 4
								r[find(patvars[length(patvars)],dregs)] = 1
							elsif compare_patterns_68k(subject[m],"move.l /dreg/,/dreg/") then
								r[find(patvars[length(patvars)],dregs)] = 1
							elsif compare_patterns_68k(subject[m],"move.l #/imm/,/dreg/") then
								r[find(patvars[length(patvars)],dregs)] = 1
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
								if compare_patterns_68k(subject[m],"move.l /dreg/,-(a2)") then
									--s = value(patvars[1])
									--d -= floor(floor(s[2])/4)
									--if d > maxDepth then
									--	maxDepth = d
									--end if
									--if s[2] = -4 then
									--	if compare_patterns_68k(subject[m+1],"[p1];")
									if depth[n]<=length(r) then
										--subject = subject[1..m-1]&subject[m+1..length(subject)]
										subject[m] = "move.l "&patvars[length(patvars)]&","&dregs[r[depth[n]]]
										--m -= 1
										--linesRemoved += 1
										--p -= 1
									end if
								elsif compare_patterns_68k(subject[m],"move.l (a2),/dreg/") then
									if depth[n]>1 and depth[n]<=length(r)+1 then
										subject[m] = "move.l "&dregs[r[depth[n]-1]]&","&patvars[length(patvars)]
									end if
								elsif compare_patterns_68k(subject[m],"move.l /dreg/,(a2)") then
									if depth[n]>1 and depth[n]<=length(r)+1 then
										subject[m] = "move.l "&patvars[length(patvars)]&","&dregs[r[depth[n]-1]]
									end if
								
								--elsif compare_patterns_68k(subject[m],"/reg/ = [p1 + /imm/];") then
								--elsif compare_patterns_68k(subject[m],"[p1 + /imm/] = /reg/;") then
								elsif compare_patterns_68k(subject[m],"move.l (a2)+,/dreg/") then
									if depth[n]>1 and depth[n]<=length(r)+1 then
										subject[m] = "move.l "&dregs[r[depth[n]-1]]&","&patvars[length(patvars)]
									end if
									--d -= 4
									--r[find(patvars[length(patvars)],allregs_bfin)] = 1
--								elsif compare_patterns_68k(subject[m],"/reg/ = /reg/;") then
--									r[find(patvars[length(patvars)-1],allregs_bfin)] = 1
--								elsif compare_patterns_68k(subject[m],"/reg/ = /imm//ext/;") then
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



global function optimise_m68k(sequence subject,integer remConst)
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
				--if compare_patterns_68k(subject[i],"ldr r1,/const/") then
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
		--		elsif compare_patterns_68k(subject[j][4..length(subject[j])]," /reg32/,$1") then
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
		--		if compare_patterns_68k(subject[p+1],"mov dword ptr [esp],/imm/") then
		--			subject = subject[1..p-1] & {"pushdw "&patvars[1]} & subject[p+2..length(subject)]
		--			linesRemoved += 1
		--		end if
		--	end if
		--	p += 1
		--end while
	end if		
	
	
	if optLevel >= 5 then
		-- Run up to 10 passes
		subject = pattern_optimise_68k(subject,intpatterns_68k,10)


		-- This code removes unnecessary return stack operations
		p = 1
		while p<length(subject) do
			if equal(subject[p],"move.l a6,-(a3)") then
				m = p
				q = p
				clean = 1
				while q<=length(subject) do
					if equal(subject[q],"move.l (a3)+,a6") then
						exit
					elsif compare_patterns_68k(subject[q],"lea /label/,a6") then
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
		
		subject = optimise_register_usage_68k(subject)

		-- This code tries to move constant assignments out of innermost loops
		times = 3
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
							r = repeat(0,16) 	-- Unused / constant / variable
							u = repeat({},16)	-- Offsets
							t = r			-- Values
							
							o = 0
							-- Mark a3 and a6 as unusable
							r[9+3] = -1
							r[9+6] = -1

							while m<n do
								if length(subject[m])>6 then
									patvars = {}
									if find(subject[m][length(subject[m])-1..length(subject[m])],regs68k) then
										s = {0,find(subject[m][length(subject[m])-1..length(subject[m])],regs68k)}
										patvars = {}
										if compare_patterns_68k(subject[m],"move.l #/imm/,/reg/") and r[s[2]]!=-1 then
											if r[s[2]]=0 then
												t[s[2]] = patvars[1]
												u[s[2]] = {m}
												r[s[2]] = 1
											elsif equal(patvars[1],t) then
												u[s[2]] &= m
											else
												r[s[2]] = -1
											end if
										else
										patvars = {}
										if compare_patterns_68k(subject[m],"move.l #/const/,/reg/") and r[s[2]]!=-1 then
											if r[s[2]]=0 then
												t[s[2]] = patvars[1]
												u[s[2]] = {m}
												r[s[2]] = 1
											elsif equal(patvars[1],t) then
												u[s[2]] &= m
											else
												r[s[2]] = -1
											end if
										else
											r[s[2]] = -1
										end if
										end if
									end if
									patvars = {}
									if compare_patterns_68k(subject[m],"lea /label/,a6") then
										r = repeat(-1,16)
										exit
									end if
								end if
								m += 1
							end while
							
							--puts(1,subject[q]&"\n")
							for i=1 to 16 do
								if r[i]=1 then
									--printf(1,"%s has constant value %s\n",{regs68k[i],t[i]})
									subject = subject[1..q-1] & {"move.l #"&t[i]&","&regs68k[i]} & subject[q..length(subject)]
									q += 1
									u += 1
									improvement = 1
									linesRemoved += length(u[i])-1
									--? u[i]
								end if
							end for
							for i=1 to 16 do
								if r[i]=1 then
									for j=1 to length(u[i]) do
										subject = subject[1..u[i][j]-1] & subject[u[i][j]+1..length(subject)]
										-- Adjust subsequent offsets
										for k=i+1 to 16 do
											if r[k]=1 then
												for l=1 to length(u[k]) do
													if u[k][l]>u[i][j] then
														u[k][l] -= 1
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
		
		
		subject = pattern_optimise_68k(subject,intpatterns2_68k,4)
	end if
	
	
	return subject
end function


