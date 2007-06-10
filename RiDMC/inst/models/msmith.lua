name = "MS"
description = "See Model refs in user's guide"
type = "D"
parameters = {"epsilon", "mu"}
variables = {"x", "y"}

function f (epsilon, mu, x, y)

	x1 = epsilon * x + mu - y^2
	y1 = x

return x1, y1

end

function Jf (epsilon, mu, x, y)

	return  

	epsilon,	-2 * y,
 	1,		 0

end



