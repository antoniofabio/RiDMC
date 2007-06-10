name = "Henon"
description = "See Model refs in user's guide"
type = "D"
parameters = {"a", "b"}
variables = {"x", "y"}


function f(a, b, x, y)

    x1 = a - x^2 + b * y
    y1 = x

    return x1, y1

end


function Jf(a, b, x, y)

	return 

	-2 * x,	b,
	1,		0

end

-- inverse 
function g(a, b, x, y)

    x1 = y
    y1 = (x - a + y^2) / b

    return x1, y1

end




