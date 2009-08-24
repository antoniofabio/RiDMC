name = "Ol3G_noise"
description = "Overlapping 3-Generation Model with noise"
type = "D"
parameters = {"wY", "wM",  "r", "b", "kM",  "kO", "eps", "anoise", "bnoise"}
variables = {"cY", "cM"}

function f (wY, wM,  r, b, kM, kO, eps, anoise, bnoise, cY, cM)
  omega1 = 1
  omega2 = (rbeta(anoise,bnoise) - 0.5) * eps + 1
  u = r*b/math.exp(b*cY)
  v = (kO/omega2)+kM+u*(cY - wY)
  cY1 = wY + wM-(kM*wM/v)-(kO*wM*cM*u/(kM*omega1*v))
  cM1 = kM*wM/v
  return cY1, cM1
end
