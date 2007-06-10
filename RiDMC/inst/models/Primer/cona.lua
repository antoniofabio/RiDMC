name = "Cona"
description = " See Model refs in user's guide"
type = "C"
parameters = {"a"}
variables = {"x", "y"}

function f (a, x, y)	
	
	x1 =  y - x^2 + 2
	y1 = 2 * a * (x^2 - y^2)

	return x1, y1

end

function Jf(a, x, y)

	return 
  
	-2 * x	, 1,
	4 * a * x	, -4 * a * y

end
