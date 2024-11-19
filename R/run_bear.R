#' Function to run the bear model
#' @param lowest the lower 95% CI of previous census
#' @param highest the higher 95% CI of previous census
#' @param years_since the time in years since the last census
#' @param years_to_forecast the time in years to forecast in to the future
#' @param female_harvest the number of female bears to remove
#' @param removals a dataframe of removals between the past census and this year
#' @param nsim the number of simulations to run (set at 5000)

run_bear <- function(lowest, highest,
                     years_since,
                     years_to_forecast,
                     female_harvest,
                     removals,
                     nsim = 5000) {
  #### Load data
  mort_fem <- readRDS(here::here("data/mort_feml.rds"))
  mort_coy <- readRDS(here::here("data/mort_COY.rds"))
  litter_size <- readRDS(here::here("data/LitterSize.rds"))
  prob_coy <- readRDS(here::here("data/prob_COY.rds"))
  stabil_fordelig <- readRDS(here::here("data/Stabil_fordelig.rds"))
  harvest_age <- readRDS(here::here("data/harvest_age.rds"))

  ## Add the previous monitoring data to the future projection
  ## Loop through all the iterations
  ## Return the whole n_bear/N_Bear matrices for plotting

  nyear <- sum(years_since + years_to_forecast)



  ### Create matrix
  A <- matrix(0, ncol = 20, nrow = 20)
  lam <- matrix(ncol = nsim, nrow = nyear)
  S <- matrix(0, nrow = nyear, ncol = 20)
  LS <- matrix(0, nrow = nyear, ncol = 20)
  P_COY <- matrix(0, nrow = nyear, ncol = 20)
  R <- matrix(0, nrow = nyear, ncol = 20)

  n_litter <- array(0, dim = c(20, nyear, nsim))
  n_COY <- array(0, dim = c(20, nyear, nsim))

  N_bear <- matrix(ncol = nyear + 1, nrow = nsim)
  N_litter <- matrix(ncol = nyear + 1, nrow = nsim)
  n_bear <- array(0, dim = c(20, nyear + 1, nsim))
  alder1 <- tidyr::tibble(alder = seq(1, 20))


  ### Starting population range
  n1_low <- lowest
  n1_high <- highest

  removals_f <- stats::rmultinom(
    years_to_forecast,
    female_harvest,
    prob = harvest_age$Andel)

  colnames(removals_f) <- c()



  ###############################################
  #### removals_tot
  removals_tot <- cbind(removals, removals_f)

  ###############################################
  ##### simulate x j=nsim x i=nyear
  for (j in 1:nsim) {
    N_start <- as.integer(runif(
      1, n1_low,
      n1_high
    ))

    age_start <- sample(seq(1:20), size = N_start, prob = stabil_fordelig$Andel,
                        replace = TRUE)

    n_start <- data.frame(antall = table(age_start)) %>%
      rename(alder = antall.age_start, antall = antall.Freq) %>%
      mutate(alder = as.numeric(as.character(alder))) %>%
      right_join(., alder1) %>%
      mutate(antall = replace_na(antall, 0)) %>%
      arrange(., alder)


    n_bear[, 1, j] <- as.matrix(n_start$antall)

    N_bear[j, 1] <- sum(n_bear[, 1, j])

    for (i in 1:nyear) {
      # H can be removed here
      # Age-transitions;
      S[i, 1] <- A[2, 1] <- 1 - boot::inv.logit(sample(mort_coy$COY_mort, 1))
      S[i, 2] <- A[3, 2] <- (1 - boot::inv.logit(sample(mort_fem$Y1, 1)))
      S[i, 3] <- A[4, 3] <- (1 - boot::inv.logit(sample(mort_fem$Y2, 1)))
      S[i, 4] <- A[5, 4] <- (1 - boot::inv.logit(sample(mort_fem$Y3, 1)))
      S[i, 5] <- A[6, 5] <- (1 - boot::inv.logit(sample(mort_fem$Y4, 1)))
      S[i, 6] <- A[7, 6] <- (1 - boot::inv.logit(sample(mort_fem$Y5, 1)))
      S[i, seq(7, 11)] <- A[8, 7] <- A[9, 8] <- A[10, 9] <- A[11, 10] <- A[12, 11] <- (1 - boot::inv.logit(sample(mort_fem$Y6_10, 1)))
      S[i, seq(12, 16)] <- A[13, 12] <- A[14, 13] <- A[15, 14] <- A[16, 15] <- A[17, 16] <- (1 - boot::inv.logit(sample(mort_fem$Y11_15, 1)))
      S[i, seq(17, 20)] <- A[18, 17] <- A[19, 18] <- A[20, 19] <- (1 - boot::inv.logit(sample(mort_fem$Y_16, 1)))


      # Probability to emerge with COY;
      P_COY[i, 4] <- boot::inv.logit(sample(prob_coy$Y4, 1))
      P_COY[i, 5] <- boot::inv.logit(sample(prob_coy$Y5, 1))
      P_COY[i, seq(6, 10)] <- boot::inv.logit(sample(prob_coy$Y6_10, 1))
      P_COY[i, seq(11, 15)] <- boot::inv.logit(sample(prob_coy$Y11_15, 1))
      P_COY[i, seq(16, 20)] <- boot::inv.logit(sample(prob_coy$Y_16, 1))

      # Litter size
      LS[i, 4] <- exp(sample(litter_size$Y4, 1)) / 2
      LS[i, 5] <- exp(sample(litter_size$Y5, 1)) / 2
      LS[i, seq(6, 10)] <- exp(sample(litter_size$Y6_10, 1)) / 2
      LS[i, seq(11, 15)] <- exp(sample(litter_size$Y11_15, 1)) / 2
      LS[i, seq(16, 20)] <- exp(sample(litter_size$Y_16, 1)) / 2

      ## Recruitment function
      R[i, 4] <- S[i, 4] * P_COY[i, 4] * LS[i, 4]
      R[i, 5] <- S[i, 5] * P_COY[i, 5] * LS[i, 5]
      R[i, 6] <- S[i, 6] * P_COY[i, 6] * LS[i, 6]
      R[i, seq(7, 11)] <- S[i, 7] * P_COY[i, 6] * LS[i, 6]
      R[i, seq(12, 16)] <- S[i, 12] * P_COY[i, 11] * LS[i, 11]
      R[i, seq(17, 20)] <- S[i, 17] * P_COY[i, 16] * LS[i, 16]
      A[1, ] <- R[i, ]

      ## Transitions;
      lam[i, j] <- popbio::eigen.analysis(A)$lam

      ################
      ## Survival;

      for (a in 1:19) {
        n_bear[a + 1, i + 1, j] <- stats::rbinom(1, n_bear[a, i, j], S[i, a])
      }


      ################
      ## Harvest - both
      n_bear[, i + 1, j] <- n_bear[, i + 1, j] - removals_tot[, i]

      for (a in 1:20) {
        n_bear[a, i + 1, j] <- if_else(n_bear[a, i + 1, j] < 0, 0, n_bear[a, i + 1, j])
      }
      ## realized(P_COY)

      for (a in 5:20) {
        n_litter[a, i, j] <- rbinom(1, n_bear[a, i + 1, j], P_COY[i, a])
      }
      N_litter[j, i + 1] <- sum(n_litter[, i, j])

      ################
      ## Litters;

      for (a in 5:20) {
        n_COY[a, i, j] <- n_litter[a, i, j] * min(rpois(1, LS[i, a]), 3)
      }


      ## Gather it all in to N_bear and n_bear

      n_bear[1, i + 1, j] <- sum(n_COY[, i, j])
      N_bear[j, i + 1] <- sum(n_bear[, i + 1, j])

      # year-loop
    }
  } # loop closed


  return(N_bear)
}
