name = "Quasiperiodicity"
description = "See Model refs in user's guide"
type = "D"

parameters = {"c1", "c2", "rho"}

variables = {"x", "y"}


two_pi = 2 * math.pi

a = {}
a[1] = -0.2681366365
a[2] = -0.910675594
a[3] =  0.3117202638
a[4] = -0.0400397884

k = {}
k[1] =  0.985460843
k[2] =  0.5044604561
k[3] =  0.9470747252
k[4] =  0.233501055

b= {}
b[1] =  0.0881861167
b[2] = -0.5650288998
b[3] =  0.1629954873
b[4] = -0.8039888198

j = {}
j[1] =  0.9903072286
j[2] =  0.3363069701
j[3] =  0.2980492123
j[4] =  0.1550646728



function f(c1, c2, rho, x, y)

    x1 = x + c1 + rho * p1(x, y) / two_pi, 1
    y1 = y + c2 + rho * p2(x, y) / two_pi, 1

    x2 = math.mod(x1, 1)
    y2 = math.mod(y1, 1)

    return x2, y2

end


function p1(x, y)
    result = a[1] * math.sin(two_pi * (x + k[1])) + 
	     a[2] * math.sin(two_pi * (y + k[2])) + 
	     a[3] * math.sin(two_pi * (x + y + k[3])) + 
	     a[4] * math.sin(two_pi * (x - y + k[4]))

    return result

end

function p2(x, y)

     return b[1] * math.sin(two_pi * (x + j[1])) + 
	    b[2] * math.sin(two_pi * (y + j[2])) + 
	    b[3] * math.sin(two_pi * (x + y + j[3])) + 
	    b[4] * math.sin(two_pi * (x - y + j[4]))
end

