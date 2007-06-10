name = "Hopf"
description = "See Model refs in user's guide"
type = "C"
parameters = {"mu", "k"}
variables = {"x", "y"}

function f(mu, k, x, y)

    x1 = y + k*x * (x^2 + y^2)
    x2 = -x + mu*y

    return x1, x2

end

