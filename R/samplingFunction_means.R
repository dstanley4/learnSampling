#' Calculate a number of sample d-values (unbiased) based on a specified (infinite) population correlation.
#' @param pop.data Data to use for popuilation (optional)
#' @param data.column.name Column name from data to use for sampling (optional)
#' @param pop.M Population mean
#' @param pop.SD Population SD
#' @param n Cell size for both cells for all samples. If you use n, do not use n.min or n.max.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @return Data frame with sample d-values
#' @examples
#' get_M_samples(pop.M = 100, pop.SD = 15, n = 100)
#' @export
get_M_samples <- function(pop.data = NULL, data.column.name = NULL, pop.M = NA, pop.SD = NA, n, number.of.samples = 10, number.of.decimals = 2) {

     Ms <- rep(NA, number.of.samples)
     SDs <- rep(NA, number.of.samples)
     SDsN <- rep(NA, number.of.samples)
     VARs <- rep(NA, number.of.samples)
     VARsN <- rep(NA, number.of.samples)
     SEs <- rep(NA, number.of.samples)
     LLs <- rep(NA,number.of.samples)
     ULs <- rep(NA,number.of.samples)
     in_interval <- rep(NA, number.of.samples)

     if (!is.null(pop.data)) {
             dv_sub <- substitute(data.column.name)
             dv_name <- deparse(dv_sub)
             dv <- pop.data[,dv_name]
             pop_data <- data.frame(dv)
             names(pop_data) <- "x"
             pop.M <- round(mean(pop_data$x), number.of.decimals)
             pop.SD <- round(sd(pop_data$x), number.of.decimals)
             pop.VAR <- round(var(pop_data$x), number.of.decimals)
     } else {
             pop.VAR <- pop.SD^2
     }


     for (i in 1:number.of.samples) {
          if (!is.null(pop.data)) {
                  group.data <- dplyr::sample_n(pop_data, n)
                  group.data <- dplyr::pull(group.data, x)
          } else {
                  group.data <- rnorm(n, mean = pop.M, sd = pop.SD)
          }

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
     data.out <- data.frame(sample.number, pop.M = pop.M, n = n, sample.M = Ms, LL = LLs, UL = ULs, ci.captured.pop.M = in_interval, pop.var = pop.VAR, sample.var.n = VARsN, sample.var.n_1 = VARs, est.SE = SEs)
     rownames(data.out) <- NULL
     return(data.out)
}
