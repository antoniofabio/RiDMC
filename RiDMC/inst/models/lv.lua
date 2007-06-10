name = "Lotka - Volterra"
description = "See Model refs in user's guide1"
parameters = {"alpha", "beta", "gamma", "delta"}
variables = {"x", "y"}
type = "C"

function f(alpha, beta, gamma, delta, x, y)

    x1 = alpha * x - beta * x * y
    y2 = - gamma * y + delta * x * y

    return x1, y2

end

