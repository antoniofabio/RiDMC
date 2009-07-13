library(RiDMC)

m <- Model(text='
     name = "SCED"
     description = "none"
     type = "D"
     parameters = {"phi0", "phi1"}
     variables = {"d"}

     function f(phi0, phi1, d)
            a=(1-phi0)
            b=(1-phi1)
            if (d <= phi0) then
                    d1 = (2/phi0) * d * (1-(1/(2*phi0))*d)
            end
            if (d > phi0) then
                    d1 = (1/(phi0*a))*d*(1-phi0^2*b-(1-phi0*b)*d)
            end
            return d1
     end
     ')

##Should raise an error
try({
  ly <- LyapunovExponentsMap(m, var=0.8, time=10,
                             par.x = "phi1",
                             par.y = "miao",
                             par.x.range=c(0.01, 0.99), par.x.howMany=10,
                             par.y.range=c(0.01, 0.99), par.y.howMany=10)
})

##Swap par.x with par.y
ly <- LyapunovExponentsMap(m, var=0.8, time=10,
                           par.x = "phi1",
                           par.y = "phi0",
                           par.x.range=c(0.01, 0.99), par.x.howMany=10,
                           par.y.range=c(0.01, 0.99), par.y.howMany=10)

##Use default par.? values
ly <- LyapunovExponentsMap(m, var=0.8, time=10,
                           par.x.range=c(0.01, 0.99), par.x.howMany=10,
                           par.y.range=c(0.01, 0.99), par.y.howMany=10)
