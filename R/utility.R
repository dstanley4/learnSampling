#' Create simulated correlatio data
#' @param n Sample size
#' @param mu Sample mean values
#' @param sd Sample sd values
#' @param cormatrix Sample correlation matrix
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


#' Create simulated data
#' @param rho Population correlation
#' @param n Population size
#' @param mu Population mean values
#' @param sd Population sd values
#' @return Data frame with desired properties
#' @export
make_pop_cor <- function(n = 1000000, rho = .30, mu = c(0,0), sd = c(1,1)) {
        cormatrix <- diag(2)
        cormatrix[1,2] <- rho
        cormatrix[2,1] <- rho

        my_data <- mvrnorm_cor(n = n,
                               mu = mu,
                               sd = sd,
                               cormatrix = cormatrix,
                               empirical = TRUE)


        my_data <- as.data.frame(my_data)
        names(my_data) <- c("x", "y")
        return(my_data)

}
