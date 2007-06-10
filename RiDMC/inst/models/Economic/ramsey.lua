name = "ramsey" 
description = "See Model refs in user's guide"
type = "C"
parameters = {"alpha", "theta", "rho", "g", "n"}
variables = {"c", "k"}


function f(alpha, theta, rho, g, n, c, k)

	eq1 = (alpha/(theta*k^(1-alpha)) - rho/theta - g)*c
	eq2 = k^(alpha) -c - (n+g)*k

	return eq1, eq2

end

