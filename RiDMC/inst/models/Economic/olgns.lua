name = "Olgns"
description = "See Model refs in user's guide"
type = "D"
parameters = {"mu", "b"}
variables = {"c", "l"}

function f (mu, b, c, l)

	c1 = l^mu
	l1 = b * ( l - c )
 
	return c1, l1

end

function Jf (mu, b, c, l)

	return

	0,	mu * l^(mu - 1),
	-b,	b

end



