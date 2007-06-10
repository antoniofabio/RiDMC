name = "Conlyapb"
description = " See Model refs in user's guide)"
type = "C"
parameters = {"k"}
variables = {"x", "y"}

function f (k, x, y)

	x1 =  y + k * x * (x^2 + y^2)
	
	y1 = - x 

	return x1, y1

end

function Jf(k, x, y)

	return   
		
	3 * k * x^2	+ k * y^2,	 2 * k * x * y
	- 1,				 0	 

end
