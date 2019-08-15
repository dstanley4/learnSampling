#' Calculate a number of sample correlations based on a specified population correlation
#' @param data Population data - first two columns used for population correlation rho.
#' @param n Sample size for all samples If you use n, do not use n.min or n.max.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @return Data frame with sample correlations
#' @export
get_r_samples_from_population_data <- function(data = NA, n, number.of.samples = 10, number.of.decimals = 2) {
     rs <- rep(NA,number.of.samples)
     dfs <- rep(NA,number.of.samples)
     ts <- rep(NA,number.of.samples)
     in_interval <- rep(NA, number.of.samples)
     ps <- rep(NA,number.of.samples)
     LLs <- rep(NA,number.of.samples)
     ULs <- rep(NA,number.of.samples)

     pop_data <- data[,1:2]
     names(pop_data) <- c("x","y")
     pop.r <- round(cor(pop_data$x, pop_data$y), number.of.decimals)

     if (length(n) > 1) {
          n_min <- n[1]
          n_max <- n[2]
          n_range <- n_max - n_min
          cur_ns <- round(runif(number.of.samples)*n_range + n_min)
     } else {
          cur_ns <- rep(n, number.of.samples)
     }

     for (i in 1:number.of.samples) {
          cur_sample_n <- cur_ns[i]
          cur_sample <- dplyr::sample_n(pop_data, cur_sample_n)
          x <- cur_sample$x
          y <- cur_sample$y
          r_info <- stats::cor.test(x,y)
          rs[i] <- round(r_info$estimate,number.of.decimals)
          LLs[i] <- round(r_info$conf.int[1],number.of.decimals)
          ULs[i] <- round(r_info$conf.int[2],number.of.decimals)
          in_interval[i] <- is_value_in_interval(pop.r, c(r_info$conf.int[1], r_info$conf.int[2]))
          ts[i] <- round(r_info$statistic,number.of.decimals)
          dfs[i] <- round(r_info$parameter,number.of.decimals)
          ps[i] <- round(r_info$p.value,5)
     }
     xx<-1:number.of.samples
     sample.number <- xx
     data.out <- data.frame(sample.number, pop.r = pop.r, n = cur_ns, r =  rs, ci.LL = LLs, ci.UL = ULs, ci.captured.pop.r = in_interval, t = ts, df = dfs, p = ps)
     rownames(data.out) <- NULL

     return(data.out)
}



#' Calculate a number of sample correlations based on a specified population correlation
#' @param pop.r Population correlation. Do not use pop.data is you provide this value.
#' @param n Sample size for all samples If you use n, do not use n.min or n.max.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @return Data frame with sample correlations
#' @examples
#' get_r_samples(pop.r = .35,n=100)
#' @export
get_r_samples <- function(pop.r = NA, n, number.of.samples = 10, number.of.decimals = 3) {

     Sigma <- diag(2)
     Sigma[1,2] <- pop.r
     Sigma[2,1] <- pop.r
     mu <- c(0,0)
     K <- number.of.samples
     data_samples <- get_true_scores_with_sampling_error_cov(Sigma, mu, n, K)

     rs <- rep(NA,number.of.samples)
     dfs <- rep(NA,number.of.samples)
     ts <- rep(NA,number.of.samples)
     in_interval <- rep(NA, number.of.samples)
     ps <- rep(NA,number.of.samples)
     LLs <- rep(NA,number.of.samples)
     ULs <- rep(NA,number.of.samples)
     pi_LLs <- rep(NA,number.of.samples)
     pi_ULs <- rep(NA,number.of.samples)
     pi_in_interval <- rep(NA, number.of.samples)
     ci_as_pi_in_interval <- rep(NA, number.of.samples)
     for (i in 1:number.of.samples) {
          x <- data_samples$x_true[,i]
          y <- data_samples$y_true[,i]
          r_info <- stats::cor.test(x,y)
          rs[i] <- round(r_info$estimate,number.of.decimals)
          LLs[i] <- round(r_info$conf.int[1],number.of.decimals)
          ULs[i] <- round(r_info$conf.int[2],number.of.decimals)
          in_interval[i] <- is_value_in_interval(pop.r, c(r_info$conf.int[1], r_info$conf.int[2]))
          ts[i] <- round(r_info$statistic,number.of.decimals)
          dfs[i] <- round(r_info$parameter,number.of.decimals)
          ps[i] <- round(r_info$p.value,5)
          pi_info <- predictionInterval::pi.r(r = rs[i], n = n, rep.n = n)
          pi_LLs[i] <- round(pi_info$lower_prediction_interval, number.of.decimals)
          pi_ULs[i] <- round(pi_info$upper_prediction_interval, number.of.decimals)
          if (i > 1) {
               pi_in_interval[i-1] <- is_value_in_interval(rs[i], c(pi_LLs[i-1], pi_ULs[i-1]))
               ci_as_pi_in_interval[i-1]  <- is_value_in_interval(rs[i], c(LLs[i-1], ULs[i-1]))
          }
     }
     xx<-1:number.of.samples
     sample.number <- xx
     data.out <- data.frame(sample.number, pop.r = pop.r, n = n, r =  rs, ci.LL = LLs, ci.UL = ULs, ci.captures.pop.r = in_interval, pi.LL = pi_LLs, pi.UL = pi_ULs, pi.captures.next.r = pi_in_interval, ci.captures.next.r = ci_as_pi_in_interval)
     rownames(data.out) <- NULL


     pi_in_interval[i] <- is_value_in_interval(pop.r, c(r_info$conf.int[1], r_info$conf.int[2]))



     return(data.out)
}



get_true_scores_with_sampling_error_cov <- function(Sigma, mu, n, K) {
     # This is the MASS::mvrnorm routine structured to eliminate
     # redundant calculations when repeated K times

     #Matrix approach for fast bivariate simulations

     p <- length(mu)
     eS <- eigen(Sigma, symmetric = TRUE)
     ev <- eS$values

     Xmulti <-  eS$vectors %*% diag(sqrt(pmax(ev, 0)), p)

     xs <- matrix(NA,n,K)
     ys <- matrix(NA,n,K)
     for (i in 1:K) {
          X <- matrix(rnorm(p * n), n)
          X2 <- Xmulti %*% t(X)
          X2 <- t(X2)
          xs[,i] <- X2[,1]
          ys[,i] <- X2[,2]
     }

     output <- list()
     output$x_true <- xs
     output$y_true <- ys
     return(output)
}


is_value_in_interval <- function(value, interval) {
        is_in_interval <- FALSE
        check_interval<-findInterval(value,sort(interval),rightmost.closed = TRUE)
        if (check_interval==1) {
                is_in_interval <- TRUE
        }
        return(is_in_interval)
}

#' Calculate the percentage of rows that are true in a column
#' @param x the column to be examined
#' @return The percent equal to TRUE
#'@export
percent_true <- function(x) {
     sum_TRUE <- sum(x, na.rm = TRUE)
     sum_length <- sum(!is.na(x))
     return(sum_TRUE/sum_length*100)
}


