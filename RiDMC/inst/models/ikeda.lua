name = "Ikeda"
description = "See Model refs in user's guide"
type = "D"

parameters = {"a", "b", "c", "d"}

variables = {"x", "y"}


function f(a, b, c, d, x, y)

    r = b - d / (1.0 + x^2 + y^2)

    x1 = a + c * (x * math.cos(r) - y * math.sin(r))
    y1 = c * (x * math.sin(r) + y * math.cos(r))

    return x1, y1

end
