name = "Quasi"
description = "See Model refs in user's guide"
type = "C"

parameters = {"a", "b", "c", "d"}

variables = {"x", "y", "z"}

function f(a, b, c, d, x, y, z)

	x1 = (a - b) * x - c * y + x * z + x * d * (1.0 - z^2)
	y1 = c * x + (a - b) * y + y * z + y * d * (1.0 - z^2)
	z1 = a * z - x^2 - y^2 - z^2

    return x1, y1, z1

end


