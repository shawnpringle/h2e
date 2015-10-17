-- set.e



-- simple cryptographic looking bijective map
constant id_base = rand(power(2,29))
constant id_step = rand(11)
function to_index_from_id(integer id)
	return (id - id_base) / id_step + 1	
end function 
function to_id_from_index(integer id)
	return (id-1)* id_step + id_base
end function

-- the squence containing all set data.
sequence data = {}

-- the set object.
public type set(object x)
	x = to_index_from_id(x)
	if compare(0,x)<0 and compare(x,length(data))<=0 then
		return 1
	end if
	return 0
end type

public function new()
	data = append(data,{})
	return to_id_from_index(length(data))
end function

public function add(integer sp, object x)
	integer s = to_index_from_id(sp)
	data[s] = append(data[s],x)
	return sp
end function

public function has(integer sp, object x)
	integer s = to_index_from_id(sp)
	return find(x,data[s])!=0
end function


