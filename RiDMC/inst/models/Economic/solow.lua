name = "Solow"
description = "See Model refs in user's guide"
type = "C"
parameters = {"s", "alpha", "n", "g", "delta"}
variables = {"k"}

function f(s, alpha, n, g, delta, k)

	k1 = s * k^alpha - (n + g + delta) * k

	return k1

end

