name = "Conlyapa"
description = "See Model refs in user's guide"
type = "C"
parameters = {"k"}
variables = {"x", "y"}

function f (k, x, y)

	x1 =  - x - y^2 
	y1 = k * x * y 

	return x1, y1

end

function Jf(k, x, y)

	return   
		
	-1,		 - 2 * y,
	k * y,	 k * x	 

end
