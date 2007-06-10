name = "Tent"
description = "See Model refs in user's guide"
type = "D"
parameters = {"c1", "c2"}
variables = {"x"}

function f(c1, c2, x)

	if (x <= c1) then 
		y = (c2 / c1) * x
	end

	if (x > c1) then
		y = (c2 / (1 - c1)) * (1 - x)
	end

	return y

end
