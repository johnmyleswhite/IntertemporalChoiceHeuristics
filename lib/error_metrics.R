mad <- function(p, y) {return(mean(abs(p - y), na.rm=TRUE))}

rmse <- function(p, y) {return(sqrt(mean((p - y)^2, na.rm=TRUE)))}

zero.one <- function(p, y) {
  y.pred <- ifelse(p > 0.5, 1, 0)
  return(mean(y.pred == y, na.rm=TRUE))
}

log.like <- function(p, y) {
  like <- ifelse(y == 1, p, 1 - p)
  return(sum(log(like), na.rm=TRUE))
}
