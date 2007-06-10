name = "Conb"
description = " See Model refs in user's guide"
type = "C"
parameters = {"a", "b"}
variables = {"x", "y"}

function f (a, b, x, y)

	x1 =  x^2 + a * x + x * y ^2
	
	y1 =   b * y^(3/2) - y

   	return x1, y1

end

function Jf(a, b, x, y)

	return 

	a + 2 * x + y^2,	 2 * x * y,
	0,			 - 1 + 3/2 * b * math.sqrt(y)	 

end
