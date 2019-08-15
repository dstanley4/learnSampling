#' Create simulated data
#' @param n Population d-value
#' @param mu Population d-value
#' @param sd Population d-value
#' @param cormatrix Population d-value
#' @param empirical Population d-value
#' @return Data frame with desired properties
#' @export
mvrnorm_cor <- function(n = 1, mu, sd, cormatrix, empirical = FALSE) {

     sdmatrix <- sd %o% sd

     if (!all(dim(sdmatrix) == dim(cormatrix))) {
          print("Error: sd list and cormatrix not compatible")
          return(-1)
     }

     Sigma <- cormatrix * sdmatrix

     dataout <- MASS::mvrnorm(n = n,
                              mu = mu,
                              Sigma = Sigma,
                              empirical = empirical)

 return(dataout)
}
