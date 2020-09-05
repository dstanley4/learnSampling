#' Calculate a number of sample d-values (unbiased) based on a specified (infinite) population correlation.
#' @param pop.d Population d-value
#' @param cell.n Cell size for both cells for all samples. If you use two values (e.g., c(20, 40), these represent -3/+3 SD for variable sample sizes
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @param var.equal TRUE/FALSE indicate type of t-test to use
#' @param alternative indicates type of alternative hypothesis (e.g., "two.sided") for t.test
#' @return Data frame with sample d-values
#' @examples
#' get_d_samples(pop.d = .35, cell.n = 100)
#' @export
get_d_samples <- function(pop.d = NA, cell.n = NA, number.of.samples = 10, number.of.decimals = 3, var.equal = TRUE, alternative = "two.sided") {

     set.seed(1)

     if (is.na(pop.d)) {return()}


     if (length(cell.n) == 1) {
          ns.for.cell1 = rep(cell.n, number.of.samples)
          ns.for.cell2 = rep(cell.n, number.of.samples)
     } else {
          cell.n <- sort(cell.n)
          cell.min <- cell.n[1]
          cell.max <- cell.n[2]
          cell.sd <- (cell.max  - cell.min) / 6
          cell.mean <- mean(cell.min, cell.max)
          ns.for.cell1 = abs(round(rnorm(mean = cell.mean, sd = cell.sd, n = number.of.samples)))
          ns.for.cell2 = ns.for.cell1
     }


     dfs <- rep(NA,number.of.samples)
     ts <- rep(NA,number.of.samples)
     ps <- rep(NA,number.of.samples)
     ds <- rep(NA,number.of.samples)
     d2s <- rep(NA,number.of.samples)
     LLs <- rep(NA,number.of.samples)
     ULs <- rep(NA,number.of.samples)
     in_interval <- rep(NA,number.of.samples)

     for (i in 1:number.of.samples) {
          cell1.n <- ns.for.cell1[i]
          cell2.n <- ns.for.cell2[i]

          group1.data <- rnorm(cell1.n) + pop.d
          group2.data <- rnorm(cell2.n)
          tout <- t.test(group1.data, group2.data, var.equal = var.equal, alternative = alternative)

          dfs[i] <- round(tout$parameter, number.of.decimals)
          ts[i] <- round(tout$statistic, number.of.decimals)
          ps[i] <- round(tout$p.value, number.of.decimals)

          ciinfo <- MBESS::ci.smd(ncp = ts[i], n.1 = cell1.n, n.2 = cell2.n)
          ds[i] <- round(ciinfo$smd, number.of.decimals)

          in_interval[i] <- is_value_in_interval(pop.d, c(ciinfo$Lower.Conf.Limit.smd, ciinfo$Upper.Conf.Limit.smd))

          LLs[i] <- round(ciinfo$Lower.Conf.Limit.smd, number.of.decimals)
          ULs[i] <- round(ciinfo$Upper.Conf.Limit.smd, number.of.decimals)
     }
     xx<-1:number.of.samples
     sample.number <- xx
     data.out <- data.frame(sample.number, pop.d = pop.d, cell1.n = ns.for.cell1, cell2.n = ns.for.cell2, d = ds, LL = LLs, UL = ULs, ci.captured.pop.d = in_interval, t = ts, df = dfs, p = ps)
     rownames(data.out) <- NULL

     return(data.out)
}



#' Calculate a number of sample correlations based on a specified population correlation
#' @param pop1 Population data using first column of data frame
#' @param pop2 Population data using first column of data frame
#' @param cell.n Sample size for all samples (range of sample size if two specified)
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @return Data frame with sample correlations
#' @export
get_d_samples_from_population_data <- function(pop1 = NULL, pop2 = NULL, cell.n, number.of.samples = 10, number.of.decimals = 2) {

        set.seed(2)
        if (is.null(pop1)) {return()}

        if (length(cell.n) == 1) {
                ns.for.cell1 = rep(cell.n, number.of.samples)
                ns.for.cell2 = rep(cell.n, number.of.samples)
        }


        m1s <- rep(NA,number.of.samples)
        m2s <- rep(NA,number.of.samples)
        v1s <- rep(NA,number.of.samples)
        v2s <- rep(NA,number.of.samples)
        diffs <- rep(NA,number.of.samples)
        sps <- rep(NA,number.of.samples)
        ds <- rep(NA,number.of.samples)
        ds_unbiased <- rep(NA,number.of.samples)

        for (i in 1:number.of.samples) {
                cell1.n <- ns.for.cell1[i]
                cell2.n <- ns.for.cell2[i]

                group1.data <- sample(pop1, size = cell1.n)
                group2.data <- sample(pop2, size = cell2.n)
                #group1.data <- rnorm(cell1.n) + pop.d
                #group2.data <- rnorm(cell2.n)

                n1 <- cell1.n
                n2 <- cell2.n
                m1 <- mean(group1.data)
                m2 <- mean(group2.data)
                diff <- m1 - m2
                v1 <- var(group1.data)
                v2 <- var(group2.data)
                vp <- ((n1-1)*v1 + (n2-1)*v2) / (n1 + n2 - 2)
                sp <- sqrt(vp)
                d <- (m1 - m2)/sp
                cf <- 1 - 3 / (4 * (n1 + n2) -9)
                m1s[i] <- m1
                m2s[i] <- m2
                v1s[i] <- v1
                v2s[i] <- v2
                diffs[i] <- diff
                sps[i] <- sp

                ds[i] <-d
                ds_unbiased[i] <- d*cf
        }
        xx<-1:number.of.samples
        sample.number <- xx

        data.out <- data.frame(study = sample.number,
                               group_n = ns.for.cell1,
                               mean1 = m1s,
                               var1_n_1= v1s,
                               mean2 = m2s,
                               var2_n_1 = v2s,
                               d = ds,
                               d_unbiased = ds_unbiased)

        data.out <- round(data.out,2)
        rownames(data.out) <- NULL

        data.out <- tibble::as_tibble(data.out)
        return(data.out)
}


# get_d_samples_from_population_data <- function(pop1 = NULL, pop2 = NULL, cell.n, number.of.samples = 10, number.of.decimals = 2, var.equal = TRUE, alternative = "two.sided") {
#         if (is.null(pop1)) {return()}
#
#         pop.d <- calc_pop_d(pop1,pop2)
#
#         if (length(cell.n) == 1) {
#                 ns.for.cell1 = rep(cell.n, number.of.samples)
#                 ns.for.cell2 = rep(cell.n, number.of.samples)
#         } else {
#                 cell.n <- sort(cell.n)
#                 cell.min <- cell.n[1]
#                 cell.max <- cell.n[2]
#                 cell.sd <- (cell.max  - cell.min) / 6
#                 cell.mean <- mean(cell.min, cell.max)
#                 ns.for.cell1 = abs(round(rnorm(mean = cell.mean, sd = cell.sd, n = number.of.samples)))
#                 ns.for.cell2 = ns.for.cell1
#         }
#
#
#         dfs <- rep(NA,number.of.samples)
#         ts <- rep(NA,number.of.samples)
#         ps <- rep(NA,number.of.samples)
#         ds <- rep(NA,number.of.samples)
#         d2s <- rep(NA,number.of.samples)
#         LLs <- rep(NA,number.of.samples)
#         ULs <- rep(NA,number.of.samples)
#         in_interval <- rep(NA,number.of.samples)
#
#         for (i in 1:number.of.samples) {
#                 cell1.n <- ns.for.cell1[i]
#                 cell2.n <- ns.for.cell2[i]
#
#                 group1.data <- dplyr::sample_n(pop1, size = cell1.n)[,1]
#                 group2.data <- dplyr::sample_n(pop2, size = cell2.n)[,1]
#                 #group1.data <- rnorm(cell1.n) + pop.d
#                 #group2.data <- rnorm(cell2.n)
#
#                 tout <- t.test(group1.data, group2.data, var.equal = var.equal, alternative = alternative)
#
#                 dfs[i] <- round(tout$parameter, number.of.decimals)
#                 ts[i] <- round(tout$statistic, number.of.decimals)
#                 ps[i] <- round(tout$p.value, number.of.decimals)
#
#                 ciinfo <- MBESS::ci.smd(ncp = ts[i], n.1 = cell1.n, n.2 = cell2.n)
#                 ds[i] <- round(ciinfo$smd, number.of.decimals)
#
#                 in_interval[i] <- is_value_in_interval(pop.d, c(ciinfo$Lower.Conf.Limit.smd, ciinfo$Upper.Conf.Limit.smd))
#
#                 LLs[i] <- round(ciinfo$Lower.Conf.Limit.smd, number.of.decimals)
#                 ULs[i] <- round(ciinfo$Upper.Conf.Limit.smd, number.of.decimals)
#         }
#         xx<-1:number.of.samples
#         sample.number <- xx
#         # data.out <- data.frame(sample.number,
#         #                        pop.d = pop.d,
#         #                        cell1.n = ns.for.cell1,
#         #                        cell2.n = ns.for.cell2,
#         #                        d = ds,
#         #                        LL = LLs,
#         #                        UL = ULs,
#         #                        ci.captured.pop.d = in_interval,
#         #                        t = ts,
#         #                        df = dfs,
#         #                        p = ps)
#         data.out <- data.frame(study = sample.number,
#                                pop.d = pop.d,
#                                cell1.n = ns.for.cell1,
#                                cell2.n = ns.for.cell2,
#                                d = ds)
#         rownames(data.out) <- NULL
#
#         return(data.out)
# }


