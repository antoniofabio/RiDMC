name = "Disparlag"
description = "See Model refs in user's guide"
type = "D"
parameters = {"b", "s"}
variables = {"tildez", "tildep"}

function f(b, s, tildez, tildep)

	tildez1 = tildep
	tildep1 = -s * tildez + (1-b) * tildep

   	return tildez1, tildep1

end

function Jf(b, s)

	return	0,	1,
			-s,	1-b

end




