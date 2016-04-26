-- ForthEC ARM code optimiser 
-- /Mic, 2004/2005


include parser.e


-- ARM registers
constant regs32 = {"r0","r1","r2","r3","r4","r5","r6","r7","r8"}

-- ARM condition codes ("nz" is left out because of how these are used in the patterns below)
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
	"ldr r1,[r10]",
	"str r0,[r10,#-4]!",
	"mov r0,r1",
	"ldr r1,[r10],#4"
	},
	{
	{"ldr r2,[r10]"},
	{"mov r1,r0"},
	{"mov r0,r2"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"ldr r1,[r10],#4",
	{"add r0,r0,r1","and r0,r0,r1","orr r0,r0,r1","eor r0,r0,r1"}
	},
	{
	{4,1,3," r0,r0,#$1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"ldr r1,[r10],#4",
	"rsb r0,r0,r1"
	},
	{
	{"sub r0,r0,#$1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"ldr r1,[r10],#4",
	"cmp r1,r0"
	},
	{
	--{"mov r1,#$1"},
	{"cmp r0,#$1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"cmp r0,#/imm/",
	"ldr r0,[r10],#4",
	"bne /label/"
	},
	{
	{"ldr r1,=$1"},
	{"cmp r1,#$2"},
	5
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"cmp r0,#/imm/",
	"ldr r0,[r10],#4",
	"bne /label/"
	},
	{
	{"ldr r1,=$1"},
	{"cmp r1,#$2"},
	5
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,=/const/",
	"str r0,[r1]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r2,=$1"},
	3,
	{"str r2,[r1]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,r1",
	"cmp r0,#/imm/",
	"ldr r0,[r10],#4"
	},
	{
	{"cmp r1,#$1"}
	}
},
{
	{
	"ldr r1,[r10]",
	"str r0,[r10]",
	"mov r0,r1",
	{"add r0,r0,#/imm/","sub r0,r0,#/imm/","and r0,r0,#/imm/","orr r0,r0,#/imm/"},
	"ldr r1,[r10]",
	"str r0,[r10]",
	"mov r0,r1"
	},
	{
	1,
	{4,1,3," r1,r1,#$1"},
	{"str r1,[r10]"}
	}
},
{
	{
	"ldr r2,[r10]",
	"mov r1,r0",
	"mov r0,r2",
	{"mul r0,r1,r0","add r0,r0,r1","and r0,r0,r1"}
	},
	{
	{"ldr r1,[r10]"},
	4
	}
},
{
	{
	{"and r0,r0,#/imm/","add r0,r0,#/imm/","orr r0,r0,#/imm/","eor r0,r0,#/imm/"},
	"cmp r0,#0"
	},
	{
	{1,1,3,"s r0,r0,#$1"}
	}
},
{
	{
	"moveq r0,#0",
	"movne r0,#-1",
	"cmp r0,#-1",
	"ldr r0,[r10],#4",
	"bne /label/"
	},
	{
	4,
	{"beq $1"}
	}
},
{
	{
	"moveq r0,#-1",
	"movne r0,#0",
	"cmp r0,#-1",
	"ldr r0,[r10],#4",
	"bne /label/"
	},
	{
	4,
	{"bne $1"}
	}
},
{
	{
	"beq /label/",
	"b /label/",
	"/label/"
	},
	{
	{"bne $2"},
	3
	}
},
{
	{
	"bne /label/",
	"b /label/",
	"/label/"
	},
	{
	{"beq $2"},
	3
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"str r7,[r12,#-4]!",
	"str r6,[r12,#-4]!",
	"ldr r7,[r10],#4",
	"mov r6,r0",
	"ldr r0,[r10],#4"
	},
	{
	5,
	6,
	{"ldr r7,=$1"},
	{"mov r6,#$2"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	{"ldrh r0,[r0]","ldrb r0,[r0]"},
	{"ands r0,r0,#/imm/","orrs r0,r0,#/imm/","eors r0,r0,#/imm/"},
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r2,=$1"},
	{3,1,4," r1,[r2]"},
	{4,1,4," r1,r1,#$2"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	"str r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$1"},
	{"ldr r2,=$2"},
	{"str r1,[r2]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	{"strb r1,[r0]","strh r1,[r0]"},
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$1"},
	{"ldr r2,=$2"},
	{6,1,4," r1,[r2]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	"str r0,[r1],#4",
	"mov r0,r1"
	},
	{
	{"ldr r1,=$1"},
	{"str r1,[r0],#4"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"ldr r1,[r10],#4",
	"mov r0,r1,lsl r0"
	},
	{
	{"mov r0,r0,lsl#$1"}
	}
},
{
	{
	"mov r0,/reg32/",
	"mov r0,r0,lsl#/imm/"
	},
	{
	{"mov r0,$1,lsl#$2"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"str r7,[r12,#-4]!",
	"str r6,[r12,#-4]!",
	"ldr r7,[r10],#4",
	"mov r6,r0",
	"ldr r0,[r10],#4"
	},
	{
	5,
	6,
	{"mov r6,#$2"},
	{"mov r7,#$1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r2,[r10]",
	"mov r1,r0",
	"mov r0,r2",
	"strh r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$1"},
	{"strh r1,[r0]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"ldr r1,[r10],#4",
	"mov r0,r1,lsr r0"
	},
	{
	{"mov r0,r0,lsr#$1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	"add r0,r0,r1"
	},
	{
	{"ldr r1,=$1"},
	{"add r0,r0,r1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"cmp r0,#/imm/",
	"ldr r0,[r10],#4"
	},
	{2}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"ldr r0,[r10,r0,lsl#2]",
	"ldr r1,[r10],#4"
	},
	{
	{"mov r1,r0"},
	{"ldr r0,[r10,#%($1-#1*#4)]"}
	}
},
{
	{
	"mov r0,#/imm/",
	"ldr r0,[r10,r0,lsl#2]"
	},
	{
	{"ldr r0,[r10,#%($1*#4)]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,r0,lsl#/imm/",
	"ldr r1,[r10],#4",
	"orr r0,r0,r1"
	},
	{
	{"orr r0,r0,r0,lsl#$1"}
	}
},
{
	{
	"add r0,r0,#/imm/",
	{"ldrh r0,[r0]","ldrb r0,[r0]"}
	},
	{
	{2,1,4," r0,[r0,#$1]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r1,r0",
	"ldr r0,[r10,#/imm/]",
	"strh r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,[r10,#%($1-#4)]"},
	{"strh r0,[r1]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,[r10,#/imm/]",
	"add r0,r0,#/imm/",
	"ldr r1,[r10],#4",
	"strh r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,[r10,#%($1-#4)]"},
	{"strh r0,[r1,#$2]"},
	{"ldr r0,[r10],#4"}
	}
},
{
	{
	"ldr r1,[r10]",
	"str r0,[r10]",
	"mov r0,r1",
	"ldr r1,=/imm/",
	"add r0,r0,r1",
	"ldr r1,[r10]",
	"str r0,[r10]",
	"mov r0,r1",
	"ldr r1,=$1",
	"add r0,r0,r1"
	},
	{
	4,
	{"ldr r2,[r10]"},
	{"add r0,r0,r1"},
	{"add r2,r2,r1"},
	{"str r2,[r10]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"add r6,r6,r0",
	"ldr r0,[r10],#4"
	},
	{
	{"add r6,r6,#$1"}
	}
},
{
	{
	"ldr r0,[r10],#4",
	"ldr r0,[r10],#4"
	},
	{
	{"add r10,r10,#4"},
	2
	}
},
{
	{
	"ldr r1,[r10]",
	"str r0,[r10,#-4]!",
	"mov r0,r1",
	"add r0,r0,#/imm/",
	"ldr r1,[r10],#4",
	"strh r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,[r10],#4"},
	{"strh r0,[r1,#$1]"},
	{"mov r0,r1"}
	}
},

{
	{
	"beq /label/",
	{"sub r0,r0,#/imm/","add r0,r0,#/imm/"},
	"$1"
	},
	{
	{2,1,3,"ne r0,r0,#$2"}
	}
},
{
	{
	"ldr r0,=/imm/",
	"ldr r1,=/imm/",
	"add r0,r0,r1"
	},
	{
	{"ldr r0,=%($1+$2)"}
	}
},
{
	{
	"mov r0,r1",
	"ldrh r0,[r0,#/imm/]"
	},
	{
	{COND,NUMRANGE,1,0,255,1,"ldrh r0,[r1,#$1]"},
	{COND,NUMRANGE,1,0,255,0,"mov r5,#$1"},
	{COND,NUMRANGE,1,0,255,0,"ldrh r0,[r1,r5]"}
	}
},
{
	{
	"ldr r1,[r10]",
	"str r0,[r10]",
	{"ldrh r0,[r1,#/imm/]","ldrb r0,[r1,#/imm/]"},
	{"and r0,r0,#/imm/","add r0,r0,#/imm/","orr r0,r0,#/imm/"},
	"ldr r1,[r10],#4",
	{"add r0,r0,r1","and r0,r0,r1","orr r0,r0,r1"}
	},
	{
	{"ldr r1,[r10],#4"},
	{3,1,4," r2,[r1,#$1]"},
	{4,1,3," r2,r2,#$2"},
	{6,1,3," r0,r2,r0"}
	}
},
{
	{
	"mov r0,r1",
	"ldr r1,=/imm/",
	"add r0,r0,r1",
	{"ldrh r0,[r0]","ldrb r0,[r0]"}
	},
	{
	{"ldr r2,=$1"},
	{4,1,4," r0,[r1,r2]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"ldr r1,[r10],#4",
	"str r0,[r1],#4",
	"mov r0,r1"
	},
	{
	{"mov r2,#$1"},
	{"str r2,[r0],#4"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	"str r0,[r1],#4",
	"mov r0,r1"
	},
	{
	{"ldr r2,=$1"},
	{"str r2,[r0],#4"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/var/",
	"ldr r0,[r0]",
	{"add r0,r0,#/imm/","sub r0,r0,#/imm/","eor r0,r0,#/imm/"},
	"str r0,[r10,#-4]!",
	"ldr r0,=$1",
	"ldr r1,[r10],#4",
	"str r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$1"},
	{"ldr r2,[r1]"},
	{4,1,3," r2,r2,#$2"},
	{"str r2,[r1]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,r6,lsl#/imm/",
	{"add r0,r0,#/imm/","and r0,r0,#/imm/","orr r0,r0,#/imm/"},
	"ldr r1,[r10],#4",
	"strh r0,[r1,#/imm/]",
	"mov r0,r1"
	},
	{
	{"mov r1,r6,lsl#$1"},
	{3,1,3," r1,r1,#$2"},
	{"strh r1,[r0,#$3]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,r6,lsl#/imm/",
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	{"add r0,r0,r1","orr r0,r0,r1"},
	"ldr r1,[r10],#4",
	"strh r0,[r1,#/imm/]",
	"mov r0,r1"
	},
	{
	{"ldr r1,=$2"},
	{"mov r2,r6,lsl#$1"},
	{6,1,3," r1,r1,r2"},
	{"strh r1,[r0,#$3]"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"str r0,[r10,#-4]!",
	"ldr r0,=/var/",
	"ldr r1,[r10],#4",
	"str r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$2"},
	{"mov r2,#$1"},
	{"str r2,[r1]"}
	}
},
{
	{
	"bne /label/",
	"ldr r1,=/var/",
	"ldr r2,[r1]",
	{"add r2,r2,#/imm/","sub r2,r2,#/imm/","and r2,r2,#/imm/","orr r2,r2,#/imm/"},
	"str r2,[r1]",
	"b /label/",
	"/label/",
	"ldr r1,=$2",
	"ldr r2,[r1]",
	{"add r2,r2,#/imm/","sub r2,r2,#/imm/","and r2,r2,#/imm/","orr r2,r2,#/imm/"},
	"str r2,[r1]",
	"/label/"
	},
	{
	2,
	3,
	{4,1,3,"eq r2,r2,#$3"},
	{10,1,3,"ne r2,r2,#$6"},
	11
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/var/",
	"ldr r0,[r0]",
	"cmp r0,#/imm/",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$1"},
	{"ldr r1,[r1]"},
	{"cmp r1,#$2"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	"strh r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$1"},
	{"strh r0,[r1]"},
	{"ldr r0,[r10],#4"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"str r7,[r12,#-4]!",
	"str r6,[r12,#-4]!",
	"ldr r7,[r10],#4",
	"mov r6,r0",
	"ldr r0,[r10],#4"
	},
	{
	3,
	4,
	5,
	{"mov r6,#$1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,r6",
	"str r0,[r10,#-4]!",
	"ldr r0,=/imm/",
	"ldr r1,[r10],#4",
	"cmp r1,r0",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$1"},
	{"cmp r6,r1"}
	}
},
{
	{
	"str r0,[r10,#-4]!",
	"mov r0,#/imm/",
	"str r0,[r10,#-4]!",
	"ldr r0,=/var/",
	"str r0,[r10,#-4]!",
	"mov r0,r6",
	"ldr r1,[r10],#4",
	"add r0,r0,r1",
	"ldr r1,[r10],#4",
	"strb r1,[r0]",
	"ldr r0,[r10],#4"
	},
	{
	{"ldr r1,=$2"},
	{"mov r2,#$1"},
	{"strb r2,[r1,r6]"}
	}
}

}


constant intpatterns2 = {
{
	{
	"mov r1,/reg32/",
	{"orr r0,r0,r1","and r0,r0,r1","add r0,r0,r1","rsb r0,r0,r1"}
	},
	{
	{2,1,3," r0,r0,$1"}
	}
},
{
	{
	"mov r0,/reg32d/",
	"str r0,[r10,#-4]!"
	},
	{
	{"str $1,[r10,#-4]!"}
	}
},
{	
	{
	"mov r2,r3",
	"mov r1,r0",
	"mov r0,r2",
	{"strh r1,[r0]","strb r1,[r0]"},
	"mov r0,r3"
	},
	{
	{4,1,4," r0,[r3]"},
	5
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


function compare_patterns_arm(sequence s1,sequence s2)
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
			elsif equal(s3,"reg32d") then
				if find(s4,regs32[2..length(regs32)]) then
					--puts(1,s4&"\n")
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

						if sequence(pat[1][j][1]) then
							m = 0
							for k=1 to length(pat[1][j]) do
								if compare_patterns_arm(subject[p+j-1],pat[1][j][k]) then
									m = k
									exit
								end if
							end for
							if not m then
								n = 0
								exit
							end if
						else
							if not compare_patterns_arm(subject[p+j-1],pat[1][j]) then
								n = 0
								exit
							end if
						end if
					end for

					-- Was a matching pattern found ?
					if n then
						if i=5 or i=6 then
							if equal(patvars[1],patvars[2]) then
								linesRemoved += 5
								subject = subject[1..p-1] & subject[p+5..length(subject)]
							else
								linesRemoved += 4
								subject = subject[1..p-1] & {"b "&patvars[3]} & subject[p+5..length(subject)]
							end if
						else
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
function optimise_return_stack_arm(sequence subject)
    integer m,n,p,q
    integer clean

    p = 1
    while p<length(subject) do
        if equal(subject[p],"str r14,[r11,#-4]!") then
            m = p   -- m is the line where we save the link register on the stack
            q = p   -- q is the line where we pop the link register off the stack (into the pc)
            clean = 1
            while q <= length(subject) do
                if equal(subject[q], "ldr r15,[r11],#4") then
                    exit
                elsif compare_patterns_arm(subject[q],"bl /label/") then
                    -- The code modifies LR, so it's not safe to optimise away saving and restoring LR
                    clean = 0
                    exit
                end if
                q += 1
            end while

            if clean then
                subject[q] = "mov r15,r14"
                subject = subject[1..m-1]&subject[m+1..length(subject)]
                p = q
                linesRemoved += 1
            end if
        end if
        p += 1
    end while

    return subject
end function



-- Responsible for moving constant assignments out of innermost loops
function optimise_constant_assignments_arm(sequence subject)
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
				if (equal(subject[p][1..7],"__loop_") and not find('e',subject[p])) then
					m = p

				elsif equal(subject[p][1..7],"__loop_") and m then    -- Do the label numbers match?
					t = subject[p]
					if --(length(subject[p])=21 and equal(subject[p][17..20],subject[m][13..16])) or
					   equal(subject[p][8..11],subject[m][8..11]) then
						n = p
						q = m
						o = 0
						clean = 1

						-- Mark all registers as unused
						r = repeat(-1,length(regs32))  -- Unused / constant / variable
						u = repeat({},length(r))       -- Offsets
						t = r                          -- Values

						r[2] = 0
						r[3] = 0
						r[4] = 0

						o = 0

						while m<n do
							if length(subject[m])>6 then
								patvars = {}
								--if find(subject[m][length(subject[m])-1..length(subject[m])],regs68k) then
								--	s = {0,find(subject[m][length(subject[m])-1..length(subject[m])],regs68k)}
									patvars = {}
									if compare_patterns_arm(subject[m],"/reg/ = /imm/") and
									   r[find(patvars[1],regs32)] != -1 then
									   	l = find(patvars[1],regs32)

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
									end if
								--end if
								patvars = {}
								if compare_patterns_arm(subject[m],"bl /label/") then
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
								subject = subject[1..q-1] & {sprintf(regs32[i]&" = ",i-1)&t[i]} & subject[q..length(subject)]
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
		if not improvement then
			exit
		end if
		times -= 1
	end while

	return subject
end function




global function optimise_arm(sequence subject,integer remConst)
	integer i1,i2,i3,i4,p,q,n,m,o,clean,improvement,times,reachable
	sequence s,r,t,u,pat
	integer regsAreFree,isInnermost

	if remConst and 0 then
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


	if optLevel >= 5 and 0 then
		if remConst then
			constlist = {}
			for i=1 to length(subject) do
				patvars = {}
				if compare_patterns_arm(subject[i],"ldr r1,/const/") then
					if equal(subject[i+1],"str r0,[r1]") then
						constlist = append(constlist,patvars&{i,1,1})
					elsif equal(subject[i+1],"str r2,[r1]") then
						constlist = append(constlist,patvars&{i,2,1})
					end if
				end if
			end for
		end if

		--patvars = {}
		--for i=1 to length(constlist) do
		--	patvars = constlist[i][1]
		--	for j=1 to length(subject) do
		--		if equal(subject[j],"push "&patvars[1]) then
		--			subject[j] = "pushdw "&patvars[2]
		--			constlist[i][4] = 0
		--		elsif compare_patterns_arm(subject[j][4..length(subject[j])]," /reg32/,$1") then
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
		--		if compare_patterns_arm(subject[p+1],"mov dword ptr [esp],/imm/") then
		--			subject = subject[1..p-1] & {"pushdw "&patvars[1]} & subject[p+2..length(subject)]
		--			linesRemoved += 1
		--		end if
		--	end if
		--	p += 1
		--end while
	end if		
	
	
	if optLevel >= 5 then
		-- Run up to 10 passes
		subject = pattern_optimise(subject,intpatterns,10)

		subject = optimise_return_stack_arm(subject)

		-- This code tries to put stack variables in registers in innermost loops
		times = 3
		while times>0 do
			improvement = 0 --linesRemoved
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

							-- Mark all registers as allocated
							r = repeat(1,16) 

							-- Mark r3..r5 as free
							r[4] = 0
							r[5] = 0
							r[6] = 0

							o = 0

							while m<n do
								if length(subject[m])>6 then
									if is_armpush(subject[m]) then
										o -= 4
									elsif is_armpop(subject[m]) then
										o += 4
									elsif equal(subject[m][1..7],"add r10") then
									--	clean = 0
										i3 = find('#',subject[m])
										s = value(subject[m][i3..length(subject[m])])
										o += s[2]
										--exit 
									elsif equal(subject[m][1..7],"sub r10") then
									--	clean = 0
										i3 = find('#',subject[m])
										s = value(subject[m][i3..length(subject[m])])
										o -= s[2]
										--exit
									elsif equal(subject[m][1..3],"bl ") then
										clean = 0
										exit
									end if
								end if

								i1 = find('r',subject[m])
								if i1 then
									if length(subject[m]) > i1 then
										i2 = find(subject[m][i1..i1+1],regs32)
										if i2 then
											r[i2] = 1
										end if
									end if
								end if
								m += 1
							end while

							regsAreFree = (find(0,r) > 0)

							if clean and regsAreFree and o=0 then
								--puts(1,subject[q]&"\n")
								--? r
								m = q+1
								while m<n do
									if is_armpush(subject[m]) then
										exit
									end if
									m += 1
								end while
								if m<n then
									i1 = find(0,r)
									r[i1] = 1
									improvement = 1
									subject[m] = "mov "&regs32[i1]&","&subject[m][5..find(',',subject[m])-1]
									m += 1
									o = -4
									q = 0
									while m<n do
										if is_armpush(subject[m]) then
											o -= 4
										elsif is_armpop(subject[m]) then
											o += 4
											if o=0 and q=0 then
												q = 1
												subject[m] = "mov "&subject[m][5..find(',',subject[m])-1]&","&regs32[i1]
											end if
										elsif equal(subject[m][1..7],"add r10") then
											i3 = find('#',subject[m])
											s = value(subject[m][i3..length(subject[m])])
											o += s[2]
										elsif equal(subject[m][1..7],"sub r10") then
											i3 = find('#',subject[m])
											s = value(subject[m][i3..length(subject[m])])
											o -= s[2]
										else
											i3 = find('[',subject[m]) 
											if i3 > 0 then
												if equal(subject[m][i3+1..i3+3],"r10") then
													-- ldr rd,[r10]
													if subject[m][i3+4] = ']' then
														if o = -4 then
															if equal(subject[m][1..4],"ldr ") then
																subject[m] = "mov "&subject[m][5..find(',',subject[m])-1]&","&regs32[i1]
															elsif equal(subject[m][1..4],"str ") then
																subject[m] = "mov "&regs32[i1]&","&subject[m][5..find(',',subject[m])-1]
															end if
														end if
													elsif subject[m][i3+4] = ',' then
														i2 = find(']',subject[m])
														s = value(subject[m][i3+6..i2-1])
														if s[1]=GET_SUCCESS then
															s[2] -= 4
															if s[2]=0 then
																subject[m] = "ldr "&subject[m][5..find(',',subject[m])-1]&",[r10]"
															else
																subject[m] = "ldr "&subject[m][5..find(',',subject[m])-1]&sprintf(",[r10,#%d]",s[2])
															end if
														else
															puts(1,"ERROR: "&subject[m]&"\n")
														end if
													end if
												end if
											end if
						
										end if
										m += 1
									end while
														
								end if
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
			if find(0,r)<=0 then
				exit
			end if
			times -= 1			
		end while
		
		subject = pattern_optimise(subject,intpatterns2,4)
	end if
	
	
	return subject
end function


