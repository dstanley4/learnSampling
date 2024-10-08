#' Calculate a number of sample d-values (unbiased) based on a specified (infinite) population correlation.
#' @param pop.data Data to use for population (optional)
#' @param pop.column.name Column name from data to use for sampling (optional)
#' @param pop.M Population mean
#' @param pop.SD Population SD
#' @param n Cell size for both cells for all samples. If you use n, do not use n.min or n.max.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @param seed.value random number seed value
#' @return Data frame with desired properties
#' @examples
#' get_M_samples(pop.M = 100, pop.SD = 15, n = 100)
#' @export
get_M_samples <- function(pop.data = NULL, pop.column.name = NULL, pop.M = NA, pop.SD = NA, n = 10, number.of.samples = 10, number.of.decimals = 2, expanded.output = FALSE, seed.value = 1) {

     set.seed(seed.value)

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
             dv_sub <- substitute(pop.column.name)
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


     dv_col <- pull(dv[,1])
     N <- length(dv_col)
     pop_id_vector <- seq(1:N)



     for (i in 1:number.of.samples) {
          if (!is.null(pop.data)) {
                  sample_id <- sample(pop_id_vector, n)

                  Ms[i]  <- mean(dv_col[sample_id])
                  SDs[i] <- round(sd(dv_col[sample_id]), number.of.decimals)
                  VARs[i] <- round(var(dv_col[sample_id]), number.of.decimals)
                  VARsN[i] <- VARs[i]*(n-1)/n
                  SDsN[i] <- sqrt(VARsN[i])
                  SEs[i] <- round(SDs[i]/sqrt(n), number.of.decimals)
                  tout <- t.test(dv_col[sample_id])
                  LLs[i] <- round(tout$conf.int[1],number.of.decimals)
                  ULs[i] <- round(tout$conf.int[2],number.of.decimals)
                  in_interval[i] <- is_value_in_interval(pop.M, c(LLs[i], ULs[i]))
          } else {
                  group.data <- rnorm(n, mean = pop.M, sd = pop.SD)
                  Ms[i]  <- mean(group.data)
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



     }
     xx<-1:number.of.samples
     sample.number <- xx
     if (expanded.output == TRUE) {
             data.out <- data.frame(n = n,
                                       pop_mean = pop.M,
                                       sample_mean = Ms,
                                       LL = LLs,
                                       UL = ULs,
                                       ci_captured_pop_M = in_interval,
                                       pop_var = pop.VAR,
                                       sample_var_n = VARsN,
                                       sample_var_n_1 = VARs,
                                       est_SE = SEs)
     } else {
             data.out <- data.frame(study = sample.number,
                                        n = n,
                                       sample_mean = Ms,
                                       sample_var_n = VARsN,
                                       sample_var_n_1 = VARs)
     }
     rownames(data.out) <- NULL
     data.out <- tibble::as_tibble(data.out)
     return(data.out)
}

# get_M_samples <- function(pop.data = NULL, pop.column.name = NULL, pop.M = NA, pop.SD = NA, n = 10, number.of.samples = 10, number.of.decimals = 2, expanded.output = FALSE) {
#
#         set.seed(1)
#
#         Ms <- rep(NA, number.of.samples)
#         SDs <- rep(NA, number.of.samples)
#         SDsN <- rep(NA, number.of.samples)
#         VARs <- rep(NA, number.of.samples)
#         VARsN <- rep(NA, number.of.samples)
#         SEs <- rep(NA, number.of.samples)
#         LLs <- rep(NA,number.of.samples)
#         ULs <- rep(NA,number.of.samples)
#         in_interval <- rep(NA, number.of.samples)
#
#         if (!is.null(pop.data)) {
#                 dv_sub <- substitute(pop.column.name)
#                 dv_name <- deparse(dv_sub)
#                 dv <- pop.data[,dv_name]
#                 pop_data <- data.frame(dv)
#                 names(pop_data) <- "x"
#                 pop.M <- round(mean(pop_data$x), number.of.decimals)
#                 pop.SD <- round(sd(pop_data$x), number.of.decimals)
#                 pop.VAR <- round(var(pop_data$x), number.of.decimals)
#         } else {
#                 pop.VAR <- pop.SD^2
#         }
#
#
#         for (i in 1:number.of.samples) {
#                 if (!is.null(pop.data)) {
#                         group.data <- dplyr::sample_n(pop_data, n)
#                         group.data <- dplyr::pull(group.data, x)
#                 } else {
#                         group.data <- rnorm(n, mean = pop.M, sd = pop.SD)
#                 }
#
#                 Ms[i]  <- mean(group.data)
#                 SDs[i] <- round(sd(group.data), number.of.decimals)
#                 VARs[i] <- round(var(group.data), number.of.decimals)
#                 VARsN[i] <- VARs[i]*(n-1)/n
#                 SDsN[i] <- sqrt(VARsN[i])
#                 SEs[i] <- round(SDs[i]/sqrt(n), number.of.decimals)
#                 tout <- t.test(group.data)
#                 LLs[i] <- round(tout$conf.int[1],number.of.decimals)
#                 ULs[i] <- round(tout$conf.int[2],number.of.decimals)
#                 in_interval[i] <- is_value_in_interval(pop.M, c(LLs[i], ULs[i]))
#
#
#         }
#         xx<-1:number.of.samples
#         sample.number <- xx
#         if (expanded.output == TRUE) {
#                 data.out <- data.frame(n = n,
#                                        pop_mean = pop.M,
#                                        sample_mean = Ms,
#                                        LL = LLs,
#                                        UL = ULs,
#                                        ci_captured_pop_M = in_interval,
#                                        pop_var = pop.VAR,
#                                        sample_var_n = VARsN,
#                                        sample_var_n_1 = VARs,
#                                        est_SE = SEs)
#         } else {
#                 data.out <- data.frame(study = sample.number,
#                                        n = n,
#                                        sample_mean = Ms,
#                                        sample_var_n = VARsN,
#                                        sample_var_n_1 = VARs)
#         }
#         rownames(data.out) <- NULL
#         data.out <- tibble::as_tibble(data.out)
#         return(data.out)
# }





#' @export
get_male_heights <- function(N = 50000, seed_value = 1, mean = 180, std = 7.5, variance) {
        if (!is.null(variance)) {
                std = sqrt(variance)
        }

        set.seed(seed_value)
        id <- 1:N
        sex = rep("male", N)
        height = round(as.numeric(scale(rnorm(n = N, mean = mean, sd = std))*std+mean))
        data_out <- tibble::tibble(id, sex, height)
        return(data_out)
}

#' @export
get_female_heights <- function(N = 50000, seed_value = 1, mean = 165, std = 7.5, variance) {
        if (!is.null(variance)) {
                std = sqrt(variance)
        }

        set.seed(seed_value)
        id <- 1:N
        sex = rep("female", N)
        height = round(as.numeric(scale(rnorm(n = N, mean = mean, sd = std))*std+mean))
        data_out <- tibble::tibble(id, sex, height)
        return(data_out)
}


#' @export
get_height_population <- function(N = 100000, seed_value = 1, mdiff = 15,  std = 10, variance = NULL) {
        if (is.null(variance)) {
                if (!is.null(std)) {
                        variance = std ^2
                }
        }

        females <- get_female_heights(mean = 165, variance = variance)
        male_mean <- 165 + mdiff
        males <- get_male_heights(mean = male_mean, variance = variance)
        nsize <- dim(males)[1]
        females$id <- females$id + nsize
        data_out <- dplyr::bind_rows(males, females)
        N <- dim(data_out)[1]
        new_order <- sample(1:N,N)
        data_out <- data_out[new_order,]
        data_out$id <- 1:N
        return(data_out)
}


#' @export
make_population <- function(N = 50000, seed_value = 1, mean = 165, std, variance = 56) {
        if (!is.null(variance)) {
                std = sqrt(variance)
        }

        set.seed(seed_value)
        id <- 1:N
        height = round(as.numeric(scale(rnorm(n = N, mean = mean, sd = std))*std+mean))
        data_out <- tibble::tibble(id, height)
        return(data_out)
}


#' @export
get_mean_samples_ratio <-function(..., n = 5, a = 1, number.of.trials = 10) {
        population_list <- list(...)
        number_pops = length(population_list)
        K <- number.of.trials

        if (number_pops>1) {
                a = number_pops
        }
        message(sprintf("Number of populations: %g", number_pops))
        message(sprintf("Using a = %g", a))


        k_vec <- seq(1:K)
        method1_var <- rep(NA, K)
        method2_var <- rep(NA, K)
        var_ratio <- rep(NA,K)

        for (cur_K in k_vec) {
                if (number_pops > 1) {
                        a_vec <- seq(1, a)
                } else {
                        a_vec <- rep(1, a)
                }

                a_mean_vec <- rep(NA, a)
                a_var_vec <- rep(NA, a)
                a_count <- 0
                for (cur_a in a_vec) {
                        a_count <- a_count + 1
                        cur_population <- population_list[[cur_a]]
                        cur_sample <- dplyr::slice_sample(cur_population, n = n)
                        a_mean_vec[a_count] <- mean(cur_sample$height)
                        a_var_vec[a_count] <- var(cur_sample$height)
                }

                method1_var[cur_K] <- var(a_mean_vec)
                mse <- mean(a_var_vec)
                method2_var[cur_K] <- mse/n
                var_ratio[cur_K] <- method1_var[cur_K]/method2_var[cur_K]
        }

        data.out <- data.frame(trial = k_vec,
                               method_1_obs_var_est = method1_var,
                               method_2_exp_var_est = method2_var,
                               var_ratio = var_ratio)
        return(data.out)
}

#' @export
get_mean_samples <-function(..., n = 5, a = 1, number.of.trials = 10) {
        population_list <- list(...)
        number_pops = length(population_list)
        K <- round(number.of.trials/a)

        if (number_pops>1) {
                a = number_pops
        }
        message(sprintf("Number of populations: %g", number_pops))
        message(sprintf("Using a = %g", a))
        trial_vec <- seq(1:number.of.trials)
        trial_mean <- rep(NA, number.of.trials)
        trial_pop  <- rep(NA, number.of.trials)
        k_vec <- seq(1:K)

        cur_trial <- 1
        for (cur_K in k_vec) {
                if (number_pops > 1) {
                        a_vec <- seq(1, a)
                } else {
                        a_vec <- rep(1, a)
                }

                a_mean_vec <- rep(NA, a)
                a_var_vec <- rep(NA, a)
                a_count <- 0
                for (cur_a in a_vec) {
                        a_count <- a_count + 1
                        cur_population <- population_list[[cur_a]]
                        cur_sample <- dplyr::slice_sample(cur_population, n = n)
                        trial_mean[cur_trial] <- mean(cur_sample$height)
                        trial_pop[cur_trial] <- cur_a
                        cur_trial <- cur_trial + 1
                        #a_mean_vec[a_count] <- mean(cur_sample$height)
                        #a_var_vec[a_count] <- var(cur_sample$height)
                }


        }

        data.out <- data.frame(trial = trial_vec,
                               source_population = trial_pop,
                               sample_mean = trial_mean)
        return(data.out)
}



