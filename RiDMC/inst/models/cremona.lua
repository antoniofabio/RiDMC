name = "Cremona"
description = "See Model refs in user's guide"
type = "D"
parameters = {"a"}
variables = {"x", "y"}

function f(a, x, y)

	x1 = x * math.cos(a) - (y - x^2) * math.sin (a)
	y1 = x * math.sin(a) + (y - x^2) * math.cos(a)

	return x1, y1

end

