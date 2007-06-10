name = "Cournot Olygopoly"
description = "See Model refs in user's guide"
type = "D"
parameters = {"a", "b"}
variables ={"x", "y"}

function f(a, b,  x, y)

	if (a*y <= 1) then 

 	x1= math.sqrt(y/a)-y

	else
 	
	x1=0

	end

	if (b*x <= 1) then 

	y1= math.sqrt(x/b)-x
	
	else

	y1=0

	end


return x1, y1

end



function Jf(a,b,x,y)


	if (a*y <= 1) then

	dxdy =math.sqrt(1/(4*a*y))-1

	else 
	
	dxdy = 0

	end


	if (b*x <= 1) then
	
	dydx = math.sqrt(1/(4*b*x))-1	

	else

	dydx = 0

	end


return 	0,	dxdy,
		dydx,	0

end










