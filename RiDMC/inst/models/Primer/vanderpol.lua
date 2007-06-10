name = "VanderPol"
description = "See Model refs in user's guide"
type = "C"
parameters = {"k", "mu", "b"}
variables = {"x", "y"}

function f(k, mu, b, x, y)

	x1= k * y + mu * x * (b - y^2)
	y1= - x + mu

	return x1, y1

end


function Jf(k, mu, b, x, y)

return

mu * (b - y^2),	k - 2 * mu * x * y,
-1,			0

end
