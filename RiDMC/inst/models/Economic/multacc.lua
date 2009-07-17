name = "multiplier accelerator model"
description = "Samuelson model with heterogeneous expectations"
type = "D"
parameters = {"i", "b", "gamma", "mu1", "mu2", "k"}
variables = {"Y", "Z"}

-- gamma stands for gamma^2 in the original model
-- to conform with the original model replace gamma by gamma^2 everywhere below

function f(i, b, gamma, mu1, mu2, k, Y, Z)
	eq = i/(1-b)

	devY = (Y - eq)/eq
	devZ = (Z - eq)/eq

	e1Y = Y + mu1 * (Y - eq)
	e2Y = Y + mu2 * (eq - Y)

	e1Z = Z + mu1 * (Z - eq)
	e2Z = Z + mu2 * (eq - Z)

	wY = 1 / (1 + (gamma * devY)^2)
	wZ = 1 / (1 + (gamma * devZ)^2)

	Y1 = i + b*(1+k) * (wY * e1Y + (1-wY) * e2Y) - k*b*(wZ * e1Z + (1-wZ) * e2Z)
	Z1 = Y

	return  Y1, Z1
end
