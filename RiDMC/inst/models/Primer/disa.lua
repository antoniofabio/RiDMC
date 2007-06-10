name = "Conc"
description = "See Model refs in user's guide."
type = "D"
parameters = {"a", "b"}
variables = {"x", "y"}

function f(a, b, x, y)

	x1= 2 * b * x + 10
	y1= 2 * a * y^2

	return x1, y1

end


function Jf(a, b, x, y)

return

2 * b,	0,
0,	4 * a * y

end
