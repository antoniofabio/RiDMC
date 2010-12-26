name = "multiplier accelerator"
description = "Samuelson model with het. expectations"
type = "D"
parameters = {"i", "b", "gamma", "mu1", "mu2", "k"}
variables = {"Y", "Z"}

function f(i, b, gamma, mu1, mu2, k, Y, Z)
  eq = i/(1-b)
  devY = (Y - eq)/eq
  devZ = (Z - eq)/eq
  wY = 1 / (1 + gamma * devY^2)
  wZ = 1 / (1 + gamma * devZ^2)
  e1Y = Y + mu1 * (Y - eq)
  e2Y = Y + mu2 * (eq - Y)
  e1Z = Z + mu1 * (Z - eq)
  e2Z = Z + mu2 * (eq - Z)
  eY = wY * e1Y + (1-wY) * e2Y
  eZ = wZ * e1Z + (1-wZ) * e2Z
  Y1 = i + b * (1+k) * eY - k * b * eZ
  Z1 = Y
  return  Y1, Z1
end

function Jf(i, b, gamma, mu1, mu2, k, Y, Z)
  eq = i/(1-b)
  devY = (Y - eq)/eq
  devZ = (Z - eq)/eq
  wY = 1 / (1 + gamma * devY^2)
  wZ = 1 / (1 + gamma * devZ^2)
  J11 = b * (1 + k) * wY *
      (gamma * devY^2 * (-2*mu1 -3*mu2 + 1) +
      (1 + mu1))
  J12 = -b * k * wZ *
      (gamma * devZ^2 * (-2*mu1 -3*mu2 + 1) +
      (1 + mu1))
  return J11, J12,
           1,   0
end
