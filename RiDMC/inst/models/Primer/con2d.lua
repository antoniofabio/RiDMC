name = "Con2d"
description = " See Model refs in user's guide"
type = "C"
parameters = {"a11", "a12", "a21", "a22"}

variables = {"x", "y"}

function f(a11, a12, a21, a22, x, y)

	x1 = a11 * x + a12 * y
	y1 = a21 * x + a22 * y

	return x1, y1

end

function Jf(a11, a12, a21, a22, x, y)

	return  
 
	a11,	 a12,
	a21,	 a22

end
