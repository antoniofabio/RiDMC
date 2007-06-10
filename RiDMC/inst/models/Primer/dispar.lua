name = "Dispar"
description = "See Model refs in user's guide"
type = "D"
parameters = {"a", "b", "m", "s"}
variables = {"p"}

function f(a, b, m, s, p)

	y = a + m + (1 - b - s) * p

	return y

end

function Jf(a, b, m, s, p)

	return 1 - b - s
end


