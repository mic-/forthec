-- Symbol table management
-- /Mic, 2005


include lexseq.e


global constant
	SYM_UNDEF = 0,
	SYM_WORD = 1,
	SYM_VAR = 2,
	SYM_FVAR = 3,
	SYM_CONST = 4,
	SYM_FCONST = 5
	

global constant
	symbol_TAG = 1


global sequence symtab


symtab = {{},{}}


global function lookup_symbol(sequence sname)
	return bsearch(sname,symtab[lex_NAMES])
end function


global function get_symbol_tag(sequence sname)
	integer p
	
	p = lookup_symbol(sname)
	if p > 0 then
		return symtab[lex_DATA][p][symbol_TAG]	
	end if
	return -1
end function


global procedure set_symbol_tag(sequence sname,integer t)
	integer p
	
	p = lookup_symbol(sname)
	if p > 0 then
		symtab[lex_DATA][p][symbol_TAG] = t	
	end if
end procedure


global procedure install_symbol(sequence sname,sequence data)
	integer p
	
	p = lookup_symbol(sname)
	if p <= 0 then
		symtab = assoc_insert(sname,data,symtab)
	end if
end procedure


