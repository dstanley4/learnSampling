#' Calculate a number of sample d-values (unbiased) based on a specified (infinite) population correlation.
#' @param pop.M Population d-value
#' @param pop.SD Cell size for both cells for all samples. If you use n, do not use n.min or n.max.
#' @param n Cell size for both cells for all samples. If you use n, do not use n.min or n.max.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @return Data frame with sample d-values
#' @examples
#' get_M_samples(pop.d = .35, cell.n = 100)
#' @export
get_M_samples <- function(pop.M = NA, pop.SD = NA, n, number.of.samples = 10, number.of.decimals = 2) {

     Ms <- rep(NA, number.of.samples)
     SDs <- rep(NA, number.of.samples)
     SDsN <- rep(NA, number.of.samples)
     VARs <- rep(NA, number.of.samples)
     VARsN <- rep(NA, number.of.samples)
     SEs <- rep(NA, number.of.samples)
     LLs <- rep(NA,number.of.samples)
     ULs <- rep(NA,number.of.samples)
     in_interval <- rep(NA, number.of.samples)

     for (i in 1:number.of.samples) {
          group.data <- rnorm(n, mean = pop.M, sd = pop.SD)

          Ms[i]  <- round(mean(group.data), number.of.decimals)
          SDs[i] <- round(sd(group.data), number.of.decimals)
          VARs[i] <- round(var(group.data), number.of.decimals)
          VARsN[i] <- VARs[i]*(n-1)/n
          SDsN[i] <- sqrt(VARsN[i])
          SEs[i] <- round(SDs[i]/sqrt(n), number.of.decimals)
          tout <- t.test(group.data)
          LLs[i] <- round(tout$conf.int[1],number.of.decimals)
          ULs[i] <- round(tout$conf.int[2],number.of.decimals)
          in_interval[i] <- is_value_in_interval(pop.M, c(LLs[i], ULs[i]))


     }
     xx<-1:number.of.samples
     sample.number <- xx
     data.out <- data.frame(sample.number, pop.M = pop.M, pop.SD = pop.SD, n = n, M = Ms, LL = LLs, UL = ULs, ci.captured.pop.M = in_interval, SD_n = SDsN, SD_n_1 = SDs, Var_n = VARsN, Var_n_1 = VARs, est.SE = SEs)
     rownames(data.out) <- NULL
     return(data.out)
}
