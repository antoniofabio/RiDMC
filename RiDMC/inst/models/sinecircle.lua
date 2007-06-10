name = "sine circle"
description = "See Model refs in user's guide"
type = "D"
parameters = {"omega", "k"}
variables = {"theta"}

function f(omega, k, theta)

two_pi = 2 * math.pi

	y1 = theta + omega + (k/two_pi)*math.sin(two_pi*theta) 
         
        y2 = math.mod(y1, 1)

    	return y2

end

function Jf(omega, k, theta)

	return 1 + k*cos(two_pi*theta)
end

