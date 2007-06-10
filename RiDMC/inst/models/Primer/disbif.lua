name = "disbif"
description = "See Model refs in user's guide"
type = "D"
parameters = {"mu"}
variables = {"x"}


function f(mu, x)

	x1 = mu + x - x^2
    
	return x1

end

function Jf(mu, x)

	return 1 - 2 * x

end
