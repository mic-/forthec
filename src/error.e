-- Error handling routines
-- /Mic, 2004/2013

include lexseq.e


-- Error enumerators
global constant ERROR_UNDECLARED	= 1,
		ERROR_OPEN_WORD		= 2,
		ERROR_OPEN_DO		= 3,
		ERROR_OPEN_BEGIN	= 4,
		ERROR_OPEN_IF		= 5,
		ERROR_REDECLARED	= 6,
		ERROR_NO_DO		= 7,
		ERROR_NO_BEGIN		= 8,
		ERROR_NO_IF		= 9,
		ERROR_EXPECTED_ID	= 10,
		ERROR_NO_COLON		= 11,
		ERROR_VAR_INSIDE_WORD	= 12,
		ERROR_CONST_INSIDE_WORD	= 13,
		ERROR_WORD_INSIDE_WORD	= 14,
		ERROR_EOF		= 15,
		ERROR_IRQ_PARAMS = 16

-- Messages corresponding to the above enumerators
constant ERROR_STRINGS = {
	"Undeclared",
	"Open word definition",
	"Open DO-loop",
	"Open BEGIN-loop",
	"Open IF-construct",
	"Non-unique identifier",
	"No matching DO",
	"No matching BEGIN",
	"No matching IF",
	"Expected an identifier",
	"No matching : (colon)",
	"Found variable declaration inside word",
	"Found constant declaration insode word",
	"Nested word declaration",
	"Unexpected end of file",
	"Found {}-form parameter list for :I (interrupt handler)"
}


global sequence errors
global integer errorCount


global procedure ERROR(object msg,sequence token)
	integer report,p

	
	report = 1	

	if sequence(msg) then
		puts(1,"ERROR: "&msg)
		if sequence(token[2]) then
			puts(1,": "&token[2])
		end if
		errorCount += 1	
		printf(1," (line %d)\nPress any key to abort..",token[3])
		while get_key()=-1 do end while
		abort(0)
	else
		-- Only report the first occurance of a specific error
		if token[1] = -1 then		-- -1 == UNKNOWN (Identifier)
			p = bsearch(token[2],errors[1])
			if p > 0 then
				if find(msg,errors[2][p]) > 0 then
					report = 0
				end if
			else

				errors = assoc_insert(token[2],{msg},errors)
			end if

		else
			p = 0
		end if
		
		if report then		
			puts(1,"ERROR: "&ERROR_STRINGS[msg])
			if sequence(token[2]) then
				printf(1,": "&token[2]&" (line %d)\n",token[3])
			end if
			if p > 0 then errors[2][p] &= msg end if
		end if
		errorCount += 1
	end if
end procedure
