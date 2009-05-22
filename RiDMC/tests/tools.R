library(RiDMC)

uniqueSet(list(c(3, 10),
               c(3, 21),
               c(2, 11),
               c(4, 9),
               c(2, -7)),
          function(a, b) a[1] == b[1])
