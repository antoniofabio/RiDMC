name = "Silnikov"
description = "See Model refs in user's guide"
type = "D"
parameters = {"a", "b", "c", "d", "e"}
variables = {"x", "y", "z"}

function f(a, b, c, d, e, x, y, z)

	x1 = a * x - b * (y - z)
	y1 = b * x + a * (y - z) 
	z1 = c * x - d * x^3 + e * z

	return x1, y1, z1

end

