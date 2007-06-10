name = "Adaptive Cournot Oligopoly"
description = "See Model refs in user's guide"
type = "D"
parameters = {"a", "b","c"}
variables ={"x", "y"}

function f(a, b, c, x, y)

	if (a*y <= 1) then 

 	x1= (1-c)*x+c*(math.sqrt(y/a)-y)

	else
	x1=(1-c)*x
	end

	if (b*x <= 1) then 

	y1= (1-c)*y+c*(math.sqrt(x/b)-x)
	
	else
	y1=(1-c)*y
	end

return x1, y1

end

function Jf(a,b,c,x,y)

dxdx = 1-c
dydy = dxdx

	if (a*y <= 1) then

	dxdy = c*(1/2*math.sqrt(1/a/y)-1)

	else 
	
	dxdy = 0

	end

	if (b*x <= 1) then

	
	dydx = c*(1/2*math.sqrt(1/b/x)-1)

	else

	dydx = 0

	end


return 

	dxdx,	dxdy,
	dydx,	dydy

end










