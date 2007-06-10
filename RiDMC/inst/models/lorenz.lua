name = "Lorenz"
description = "See Model refs in user's guide"
type = "C"
parameters = {"sigma", "r", "b"}
variables = {"x", "y", "z"}

function f(sigma, r, b, x, y, z)

    x1 = - sigma * (x - y)
    y1 = x * (r - z) - y
    z1 = x * y - b * z

    return x1, y1, z1

end

function Jf(sigma, r, b, x, y, z)

	return   

	-sigma,	sigma,	0,
	r - z,	-1,		-x,
	y,		x,		-b

end

