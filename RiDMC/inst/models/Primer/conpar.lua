name = "Conpar"
description = "See Model refs in user's guide"
type = "C"
parameters = {"a", "m", "b", "s"}
variables = {"p"}

function f(a, m, b, s, p)

	p1 = a + m - (b + s) * p
	
	return p1
end

function Jf(a, m, b, s, p)

return - b - s

end
