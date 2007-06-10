name = "Standard"
description = "See Model refs in user's guide"
type = "D"
parameters = {"a"}
variables = {"x", "y"}

function f(a, x, y)

	two_pi = 2 * math.pi    
	x1 = math.mod(x + y + a * (1/two_pi) * math.sin(two_pi * x), 1)
	y1 = y + a * (1/two_pi) * math.sin(two_pi * x)

	return x1, y1

end
