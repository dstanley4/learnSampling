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


#' Create simulated cmeasurement error
#' @param df data frame with simulation results
#' @param rxx vector with range of rxx values
#' @param ryy vector with range of ryy values
#' @return Data frame with desired properties
#' @export
simulate_measurement_error <- function(df, rxx, ryy) {

    rxx_max <- max(rxx)
    rxx_min <- min(rxx)
    rxx_range <- rxx_max - rxx_min
    rxx_sd <- rxx_range / 4
    rxx_mean <- mean(rxx)

    ryy_max <- max(ryy)
    ryy_min <- min(ryy)
    ryy_range <- ryy_max - ryy_min
    ryy_sd <- ryy_range / 4
    ryy_mean <- mean(ryy)

    num_r <- length(df$r)

    rxx_values <- round(rnorm(n = num_r,
                              mean = rxx_mean,
                              sd = rxx_sd),2)
    rxx_values[rxx_values>1] <- 1
    rxx_values[rxx_values<.1] <- .1


    ryy_values <- round(rnorm(n = num_r,
                              mean = ryy_mean,
                              sd = ryy_sd),2)
    ryy_values[ryy_values>1] <- 1
    ryy_values[ryy_values<.1] <- .1


    r_true <- df$r
    n <- df$n
    r_obs <- r_true * sqrt(rxx_values * ryy_values)

    ci_obs_LL <- ci.LL(r_obs, n)
    ci_obs_UL <- ci.UL(r_obs, n)

    ci_cor_LL <- ci_obs_LL/sqrt(rxx_values*ryy_values)
    ci_cor_UL <- ci_obs_UL/sqrt(rxx_values*ryy_values)


    in_interval_obs <- rep(NA, length(n))
    in_interval_cor <- rep(NA, length(n))
    ci_cor <- rep("", length(n))
    ci_obs <- rep("", length(n))
    number.of.samples <- length(n)
    pop.r <- df$pop.r[1]
    for (i in 1:number.of.samples) {
        ci_cor[i] <- sprintf("[%1.2f, %1.2f]", ci_cor_LL[i], ci_cor_UL[i])
        ci_obs[i] <- sprintf("[%1.2f, %1.2f]", ci_obs_LL[i], ci_obs_UL[i])

        in_interval_cor[i] <- is_value_in_interval(pop.r, c(ci_cor_LL[i], ci_cor_UL[i]))
        in_interval_obs[i] <- is_value_in_interval(pop.r, c(ci_obs_LL[i], ci_obs_UL[i]))
    }


    addon_cols <- data.frame(r.obs = round(r_obs,2),
                             rxx = rxx_values,
                             ryy = ryy_values,
                             ci.obs = ci_obs,
                             ci.obs.cap.pop.r = in_interval_obs,
                             ci.cor = ci_cor,
                             ci.cor.cap.pop.r = in_interval_cor)

    df <- cbind(df, addon_cols)

    df <- dplyr::select(df,
                        sample.number,
                        pop.r,
                        n,
                        r.obs,
                        rxx,
                        ryy,
                        ci.obs,
                        ci.obs.cap.pop.r,
                        ci.cor,
                        ci.cor.cap.pop.r)

    return(df)
}



r_to_z <- function(r) {
    zvalue <- atanh(r)
    return(zvalue)
}

z_to_r <- function(z) {
    rvalue <- tanh(z)
    return(rvalue)
}

r_to_z_se <- function(N) {
    se_out <-  1 / sqrt(N-3)
    return(se_out)
}


ci.LL <- function(r, n, level = .95) {
    alpha_level_half = (1 - level)/2
    LLz <- r_to_z(r) - qnorm(1 -alpha_level_half) * r_to_z_se(n)
    LL <- z_to_r(LLz)
}

ci.UL <- function(r, n, level = .95) {
    alpha_level_half = (1 - level)/2
    ULz <- r_to_z(r) + qnorm(1 -alpha_level_half) * r_to_z_se(n)
    UL <- z_to_r(ULz)
}

