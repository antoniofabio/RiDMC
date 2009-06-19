library(RiDMC)

uniqueSet(list(c(3, 10),
               c(3, 21),
               c(2, 11),
               c(4, 9),
               c(2, -7)),
          function(a, b) a[1] == b[1])

##Check .sanitizeNamedVector
stopifnot(identical(.sanitizeNamedVector(1:3, letters[1:3]),
                    structure(1:3, .Names = c("a", "b", "c"))))
stopifnot(identical(.sanitizeNamedVector(c(b=-7), letters[1:3]),
                    structure(c(NA, -7, NA), .Names = c("a", "b", "c"))))
stopifnot(identical(.sanitizeNamedVector(c(b=-7, a=31), letters[1:3]),
                    structure(c(31, -7, NA), .Names = c("a", "b", "c"))))
stopifnot(identical(.sanitizeNamedVector(, letters[1:3]),
                    structure(c(NA_real_, NA_real_, NA_real_),
                              .Names = c("a", "b", "c"))))
