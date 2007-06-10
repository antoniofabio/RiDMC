name = "Rotor"
description = "See Model refs in user's guide"
type = "D"
parameters = {"c", "rho"}
variables = {"x", "y"}

function f(c1, rho, x, y)

	x1 = math.mod((x + y), 2 * math.pi)
   	y1 = c * y + rho * math.sin(x + y)

	return x1, y1

end
