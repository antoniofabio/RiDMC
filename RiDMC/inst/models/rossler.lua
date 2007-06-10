name = "Rossler"
description = "See Model refs in user's guide"
type = "C"
parameters = {"a", "b", "c"}
variables = {"x", "y", "z"}

function f(a, b, c, x, y, z)

	x1 = - (y + z)
	y1 = x + (a * y)
	z1 = b + z * (x - c)

	return x1, y1, z1

end

function Jf(a, b, c, x, y, z)

	return
   
	0,	-1,	1,
	1,	a,	0,
	z,	0,	x - c

end
