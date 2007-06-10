name = "Flip"
type = "D"
description = "See Model refs in user's guide"
parameters = {"mu"}
variables = {"x"}

function f (mu, x)

	x1 = mu - x^2
 
	return x1

end

function Jf (mu, x)

	return -2 * x
end

