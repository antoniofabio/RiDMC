name = "Tinkerbell Map"
description = "See Model refs in user's guide"
type = "D"
parameters = {"c1", "c2", "c3", "c4"}
variables = {"x", "y"}

function f(c1, c2, c3, c4, x, y)

	x1 = x^2 - y^2 + c1 * x + c2 * y	
	y1 = 2 * x * y + c3 * x + c4 * y

	return x1, y1

end

function Jf(c1, c2, c3, c4, x, y)

	return

	2 * x + c1,		-2 * y + c2,
      2 * y + c3,		2 * x + c4

end

