name = "Olg1"
description = "See Model refs in user's guide"
type = "D"
parameters = {"r","gamma", "b"}
variables = {"l", "c"}

function f (r, gamma, b, l, c)

	l1 = (r*c*math.exp(-c))^gamma
	c1 = (r*c*math.exp(-c))^gamma-(1/b)*l
 
	return l1, c1

end

function Jf (r, gamma, b, l, c)

	return

	0,		(gamma*(r*c*math.exp(-c))^(gamma-1))*r*math.exp(-c)*(1-c),
	-(1/b),	(gamma*(r*c*math.exp(-c))^(gamma-1))*r*math.exp(-c)*(1-c) 

end

