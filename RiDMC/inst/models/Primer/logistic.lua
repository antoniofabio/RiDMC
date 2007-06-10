name = "logistic map"
description = " See Model refs in user's guide"
type = "D"
parameters = {"mu"}
variables = {"x"}

function f(mu, x)

	y = mu * x * (1 - x)

	return y

end

function Jf(mu, x)

	return mu - 2 * mu * x;
end

