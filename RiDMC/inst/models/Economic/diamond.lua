name = "diamond"
description = "See Model refs in user's guide"
type = "D"
parameters = {"alpha", "n", "g", "rho"}
variables = {"k"}

function f (alpha, n, g, rho, k)

	k1 = (k^(alpha))*(1 - alpha)/((1 + n)*(1+g)*(2 + rho)) --2.61

	return k1
end


function Jf(alpha, n, g, rho, k)

return ((1 - alpha)/((1 + n) * (1 + g) * (2 + rho))) * alpha * k^(1 - alpha)

end
