#' @export
var.pop <- function(x) {
     N <- length(x)
     var.out <- var(x)*(N-1)/N
     return(var.out)
}

#' @export
sd.pop <- function(x) {
     sqrt(var.pop(x))
}

