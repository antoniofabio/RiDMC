name = "Gingerman"
description = "See Model refs in user's guide"
type = "D"

parameters = {"a"}

variables = {"x", "y",}

function f(a, x, y)

	x1 = 1 - y + math.abs(x)
	y1 = x

	return x1, y1

end
