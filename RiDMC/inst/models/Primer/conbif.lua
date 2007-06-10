name = "Conbif"
description = "See Model refs in user's guide"
type = "C"

parameters = {"mu"}

variables = {"x"}


function f(mu, x)

	x1 = x^3 + x^2 - (2+mu)*x + mu

	return x1

end

