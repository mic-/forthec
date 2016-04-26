-- Functions for lexically ordered sequences and associative arrays
-- /Mic, 2004/2005

include get.e
include wildcard.e


global constant
	lex_NAMES = 1,
	lex_DATA = 2
	
	
-- Do a binary search for 'look_for' in 'look_in'.
-- 'look_in' is assumed to be lexically ordered.
global function bsearch(sequence look_for,sequence look_in)
	integer lb,ub,middle,res
	
	lb = 0
	ub = length(look_in)-1
	
	while 1 do
		if ub<lb then
			return 0
		end if

		middle = floor((lb+ub)/2)
		
		res = compare(look_for,look_in[middle+1])
		if res=0 then
			return middle+1
		elsif res<0 then
			ub = middle-1
		else
			lb = middle+1
		end if
	end while
	return 0
end function



-- Insert sequence 'what' in sequence 'dest' of lexically ordered sequences
global function ordered_insert(sequence what,sequence dest)
	integer lb,ub,middle,res

	-- Is the destination empty?
	if not length(dest) then
		return {what}
	end if
	
	-- Initialise lower and upper bounds
	lb = 0
	ub = length(dest)-1
	
	while 1 do
		if ub<lb then
			return dest
		end if

		middle = floor((lb+ub)/2)
		
		res = compare(what,dest[middle+1])
		if res=0 then
			return dest
		elsif res<0 then
			if middle=0 then
				exit
			elsif compare(what,dest[middle])>0 then
				exit
			end if
			ub = middle-1
		else
			if middle=length(dest)-1 then
				if middle=0 then
					middle=1
				else
					middle += 1
				end if
				exit
			elsif compare(what,dest[middle+2])<0 then
				middle += 1
				exit
			end if
			lb = middle+1
		end if
	end while
	
	middle += 1
	if middle=1 then
		dest = {what}&dest
	elsif middle>length(dest) then
		dest = append(dest,what)
	else
		dest = dest[1..middle-1] & {what} & dest[middle..length(dest)]
	end if
	
	return dest
end function



		
-- Insert sequence 'what' in sequence 'dest' of lexically ordered sequences
--
-- Used for associative arrays. 'what' is inserted in 'dest[1]' while
-- 'data' is inserted at the corresponding position of 'dest[2]'
global function assoc_insert(sequence name,object data,sequence dest)
	integer lb,ub,middle,res
	sequence names
	
	names = dest[lex_NAMES]
	
	if not length(names) then
		return {{name},{data}}
	end if
	
	lb = 0
	ub = length(names)-1
	
	while 1 do
		if ub<lb then
			return dest
		end if

		middle = floor((lb+ub)/2)
		
		res = compare(name,names[middle+1])
		if res=0 then
			return dest
		elsif res<0 then
			if middle=0 then
				exit
			elsif compare(name,names[middle])>0 then
				exit
			end if
			ub = middle-1
		else
			if middle=length(names)-1 then
				if middle=0 then
					middle=1
				else
					middle += 1
				end if
				exit
			elsif compare(name,names[middle+2])<0 then
				middle += 1
				exit
			end if
			lb = middle+1
		end if
	end while
	
	middle += 1
	if middle=1 then
		dest[lex_NAMES] = {name}&names
		dest[lex_DATA] = {data}&dest[lex_DATA]
	elsif middle>length(names) then
		dest[lex_NAMES] = append(names,name)
		dest[lex_DATA] = append(dest[lex_DATA],data)
	else
		dest[lex_NAMES] = names[1..middle-1] & {name} & names[middle..length(names)]
		dest[lex_DATA] = dest[lex_DATA][1..middle-1] & {data} & dest[lex_DATA][middle..length(dest[lex_DATA])]
	end if
	
	return dest
end function

