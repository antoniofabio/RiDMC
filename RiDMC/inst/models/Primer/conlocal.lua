name = "Conlocal"
description = "See Model refs in user's guide"
type = "C"

parameters = {"a"}

variables = {"x", "y"}


function f(a, x, y)

	x1 = y^2 - 3*x + a
	y2 = x^2 -y^2

	return x1, y2

end

