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


#' Create simulated correlation population
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



#' Create simulated d-value population
#' @param d Population d-value
#' @param n Population size (size for each of the two populations)
#' @param mu Population mean values (for both populations)
#' @param sd Population sd values (for both populations)
#' @return Data frame with desired properties
#' @export
make_pop_d <- function(n = 1000000, d = .30, mu = c(.8,0), sd = c(1,1)) {

        x <- as.numeric(scale(rnorm(n))) * sd[1] + mu[1]
        pop1 <- data.frame(x)

        x <- as.numeric(scale(rnorm(n))) * sd[2] + mu[2]
        pop2 <- data.frame(x)

        output <- list(pop1 = pop1,
                       pop2 = pop2)

        return(output)

}


calc_pop_d <- function(pop1, pop2) {
        pop1var <- var(pop1[,1])
        pop2var <- var(pop2[,1])
        n1 <- length(pop1[,1])
        n2 <- length(pop2[,1])

        pooled_var <- (pop1var*(n1-1) + pop2var*(n2-1))/(n1+n2-2)

        dout <- (mean(pop1[,1]) - mean(pop2[,1]))/ sqrt(pooled_var)


}
