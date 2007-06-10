name = "BH"
description = "See Model refs in user's guide"
type = "D"
parameters = {"g_1", "b_1", "g_2", "b_2", "beta", "w", "R", "C"}
variables = {"x1", "x2", "x3", "n_1", "n_2","U1","U2"}

function f( g_1, b_1, g_2, b_2, beta, w, R, C, x1, x2, x3, n_1, n_2, U1, U2)

	e1 = b_1 + g_1 * x1
	e2 = b_2 + g_2 * x1

	U1 = (x1 - R * x2) * (g_1 * x3 + b_1 - R * x2) + w * U1 - C    
	U2 = (x1 - R * x2) * (g_2 * x3 + b_2 - R * x2) + w * U2

	dU = U2 - U1	

	n1 = 1 / ( 1 + math.exp( beta * dU ) )
    	n2 = 1 - n1

	x0 = ( n1 * e1 + n2 * e2 ) / R

    	return x0, x1, x2, n1, n2, U1, U2

end
