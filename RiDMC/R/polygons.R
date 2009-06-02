##
##Utility functions to deal with arbitrary polygons
##

sideYCrossesPtY <- function(segY, ptY) {
  segY <- as.matrix(segY, ncol=2)
  return(apply(segY - ptY, 1, prod) < 0)
}

polySidesY <- function(poly) {
  polyY <- rep(poly[,2], each=2)
  return(matrix(c(polyY[-1], polyY[1]),
                ncol=2, byrow=TRUE))
}
segmentVerticalIntersection <- function(x1, y1, x2, y2, y0) {
  slope <- (y2 - y1) / (x2 - x1)
  intercept <- y1 - slope * x1
  ans <- (y0 - intercept) / slope
  nFiniteSlope <- !is.finite(slope)
  ans[nFiniteSlope] <- x1[nFiniteSlope]
  return(ans)
}

odd <- function(x) {
  return(x %% 2 == 1)
}

polyContainsPt <- function(poly, pt) {
  ptY <- pt[2]
  crosses <- sideYCrossesPtY(polySidesY(poly), ptY)
  if(sum(crosses) < 2)
    return(FALSE)
  wC <- which(crosses)
  poly2 <- rbind(poly, poly[1,,drop=FALSE])
  poly3 <- poly2[rep(wC, each=2) + rep(0:1, length(wC)),,drop=FALSE]
  nSeg <- nrow(poly3) / 2
  poly3_1 <- poly3[seq_len(nSeg) * 2 - 1,,drop=FALSE]
  poly3_2 <- poly3[seq_len(nSeg) * 2,,drop=FALSE]
  xes <- segmentVerticalIntersection(poly3_1[,1],
                                     poly3_1[,2],
                                     poly3_2[,1],
                                     poly3_2[,2],
                                     ptY)
  ptX <- pt[1]
  return(odd(sum(xes <= ptX)) && odd(sum(xes >= ptX)))
}
