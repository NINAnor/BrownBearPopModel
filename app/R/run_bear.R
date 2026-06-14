#' Function to run the bear model
#' @param lowest the lower 95% CI of previous census
#' @param highest the higher 95% CI of previous census
#' @param years_since the time in years since the last census
#' @param years_to_forecast the time in years to forecast in to the future
#' @param female_harvest the number of female bears to remove each year (this value can be different for each year up to 5 years)
#' @param removals a dataframe of removals between the past census and this year
#' @param nsim the number of simulations to run (set at 50 for testing)
#' @param terminalAgeClass logical. Whether reaching the last age class is "terminal" (TRUE) or not (FALSE, default). 
#' If the age class is terminal, individuals in that age class disappear from the model after one year (= die). Otherwise, they may remain in the last age class if they survive.
#' @param data which parameters should the model use ("old" is pre 2026 update based on Bischoff et al. 2018 or "new" )

run_bear <- function(
    lowest,
    highest,
    years_since,
    years_to_forecast,
    female_harvest,
    removals,
    nsim = 50,
    terminalAgeClass = FALSE,
    data_version = "model_old"
  ) {
    
    library(popbio)
    library(dplyr)
    library(tibble)
    library(tidyr)
    
    data_path <- file.path("data", data_version)
    
    # ----------------------------
    # Load data
    # ----------------------------
    
    mort_fem <- readRDS(file.path(data_path, "mort_feml.rds"))
    mort_COY <- readRDS(file.path(data_path, "mort_COY.rds"))
    LitterSize <- readRDS(file.path(data_path, "LitterSize.rds"))
    prob_COY <- readRDS(file.path(data_path, "prob_COY.rds"))
    Stabil_fordelig <- readRDS(file.path(data_path, "Stabil_fordelig.rds"))
    harvest_age <- readRDS(file.path(data_path, "harvest_age.rds"))
    
    # ----------------------------
    # Standardise parameter scales
    # ----------------------------
    # Old data: logit/log scale
    # New data: probability/natural scale
    
    to_prob <- function(x) {
      if (all(x >= 0 & x <= 1, na.rm = TRUE)) {
        x
      } else {
        plogis(x)
      }
    }
    
    to_litter <- function(x) {
      if (all(x > 0 & x < 10, na.rm = TRUE)) {
        x
      } else {
        exp(x)
      }
    }
    
    mort_fem <- mort_fem %>%
      mutate(across(-any_of("Iteration"), to_prob))
    
    mort_COY <- mort_COY %>%
      mutate(across(-any_of("Iteration"), to_prob))
    
    prob_COY <- prob_COY %>%
      mutate(across(-any_of("Iteration"), to_prob))
    
    LitterSize <- LitterSize %>%
      mutate(across(-any_of("Iteration"), to_litter))
    
    # Safety checks
    if (anyNA(mort_fem) || anyNA(mort_COY) || anyNA(prob_COY) || anyNA(LitterSize)) {
      stop("One or more demographic input files contain NA values.")
    }
    
    if (any(mort_fem[-1] < 0 | mort_fem[-1] > 1, na.rm = TRUE)) {
      stop("mort_fem contains invalid mortality probabilities.")
    }
    
    if (any(prob_COY[-1] < 0 | prob_COY[-1] > 1, na.rm = TRUE)) {
      stop("prob_COY contains invalid reproduction probabilities.")
    }
    
    nyear <- years_since + years_to_forecast
    
    # ----------------------------
    # Initialise objects
    # ----------------------------
    
    A <- matrix(0, ncol = 20, nrow = 20)
    lam <- matrix(ncol = nsim, nrow = nyear)
    S <- matrix(0, nrow = nyear, ncol = 20)
    LS <- matrix(0, nrow = nyear, ncol = 20)
    P_COY <- matrix(0, nrow = nyear, ncol = 20)
    R <- matrix(0, nrow = nyear, ncol = 20)
    
    n_litter <- array(0, dim = c(20, nyear, nsim))
    n_COY <- array(0, dim = c(20, nyear, nsim))
    N_bear_pre <- matrix(ncol = nyear + 1, nrow = nsim)
    N_bear <- matrix(ncol = nyear + 1, nrow = nsim)
    N_litter <- matrix(ncol = nyear + 1, nrow = nsim)
    n_bear <- array(0, dim = c(20, nyear + 1, nsim))
    
    alder1 <- tibble(alder = 1:20)
    
    # ----------------------------
    # Future harvest
    # ----------------------------
    
    if (length(female_harvest) == 1) {
      female_harvest <- rep(female_harvest, min(5, years_to_forecast))
    } else {
      female_harvest <- c(
        female_harvest,
        rep(0, max(0, min(5, years_to_forecast) - length(female_harvest)))
      )
    }
    
    removals_f <- do.call(
      cbind,
      lapply(female_harvest, function(fh) {
        stats::rmultinom(1, fh, prob = harvest_age$Andel)
      })
    )
    
    if (ncol(removals_f) < years_to_forecast) {
      removals_f <- cbind(
        removals_f,
        matrix(0, nrow = nrow(removals_f), ncol = years_to_forecast - ncol(removals_f))
      )
    }
    
    colnames(removals_f) <- NULL
    
    # ----------------------------
    # Past + future removals
    # ----------------------------
    
    removals <- as.matrix(removals)
    
    if (nrow(removals) != 20) {
      stop("removals must have 20 rows, one for each age class.")
    }
    
    if (ncol(removals) == years_since) {
      removals <- cbind(matrix(0, nrow = 20, ncol = 1), removals)
    }
    
    if (ncol(removals) != years_since + 1) {
      stop("removals must have either years_since columns, or years_since + 1 columns including the census year.")
    }
    
    if (ncol(removals_f) != years_to_forecast) {
      stop("removals_f must have one column per forecast year.")
    }
    
    removals_tot <- cbind(removals, removals_f)
    removals_tot[is.na(removals_tot)] <- 0
    removals_tot[, 1] <- 0
    
    if (ncol(removals_tot) != nyear + 1) {
      stop("removals_tot has the wrong number of columns.")
    }
    
    # ----------------------------
    # Main simulation loop
    # ----------------------------
    
    for (j in 1:nsim) {
      
      N_start <- as.integer(runif(1, lowest, highest))
      
      age_start <- sample(
        1:20,
        size = N_start,
        prob = Stabil_fordelig$Andel,
        replace = TRUE
      )
      
      n_start <- data.frame(antall = table(age_start)) %>%
        dplyr::rename(alder = antall.age_start, antall = antall.Freq) %>%
        dplyr::mutate(alder = as.numeric(as.character(alder))) %>%
        dplyr::right_join(alder1, by = "alder") %>%
        dplyr::mutate(antall = replace_na(antall, 0)) %>%
        dplyr::arrange(alder)
      
      n_bear[, 1, j] <- as.matrix(n_start$antall)
      n_bear[, 1, j] <- pmax(n_bear[, 1, j] - removals_tot[, 1], 0)
      
      N_bear[j, 1] <- sum(n_bear[, 1, j])
      N_bear_pre[j, 1] <- N_bear[j, 1]
      
      for (i in 1:nyear) {
        
        # ----------------------------
        # Survival
        # mort_* is mortality probability, so survival = 1 - mortality
        # ----------------------------
        
        S[i, 1] <- A[2, 1] <- 1 - sample(mort_COY$COY_mort, 1)
        S[i, 2] <- A[3, 2] <- 1 - sample(mort_fem$Y1, 1)
        S[i, 3] <- A[4, 3] <- 1 - sample(mort_fem$Y2, 1)
        S[i, 4] <- A[5, 4] <- 1 - sample(mort_fem$Y3, 1)
        S[i, 5] <- A[6, 5] <- 1 - sample(mort_fem$Y4, 1)
        S[i, 6] <- A[7, 6] <- 1 - sample(mort_fem$Y5, 1)
        
        S[i, 7:11] <- A[8, 7] <- A[9, 8] <- A[10, 9] <-
          A[11, 10] <- A[12, 11] <- 1 - sample(mort_fem$Y6_10, 1)
        
        S[i, 12:16] <- A[13, 12] <- A[14, 13] <- A[15, 14] <-
          A[16, 15] <- A[17, 16] <- 1 - sample(mort_fem$Y11_15, 1)
        
        S[i, 17:20] <- A[18, 17] <- A[19, 18] <- A[20, 19] <-
          1 - sample(mort_fem$Y_16, 1)
        
        if (!terminalAgeClass) {
          A[20, 20] <- S[i, 20]
        }
        
        # ----------------------------
        # Reproduction and litter size
        # ----------------------------
        
        P_COY[i, 4] <- sample(prob_COY$Y4, 1)
        P_COY[i, 5] <- sample(prob_COY$Y5, 1)
        P_COY[i, 6:10] <- sample(prob_COY$Y6_10, 1)
        P_COY[i, 11:15] <- sample(prob_COY$Y11_15, 1)
        P_COY[i, 16:20] <- sample(prob_COY$Y_16, 1)
        
        LS[i, 4] <- sample(LitterSize$Y4, 1) / 2
        LS[i, 5] <- sample(LitterSize$Y5, 1) / 2
        LS[i, 6:10] <- sample(LitterSize$Y6_10, 1) / 2
        LS[i, 11:15] <- sample(LitterSize$Y11_15, 1) / 2
        LS[i, 16:20] <- sample(LitterSize$Y_16, 1) / 2
        
        # Checks before stochastic draws
        if (anyNA(S[i, ]) || any(S[i, ] < 0) || any(S[i, ] > 1)) {
          stop("Invalid survival probability in year ", i, ": ",
               paste(round(S[i, ], 3), collapse = ", "))
        }
        
        if (anyNA(P_COY[i, ]) || any(P_COY[i, ] < 0) || any(P_COY[i, ] > 1)) {
          stop("Invalid reproduction probability in year ", i, ": ",
               paste(round(P_COY[i, ], 3), collapse = ", "))
        }
        
        if (anyNA(LS[i, ]) || any(LS[i, ] < 0)) {
          stop("Invalid litter size in year ", i, ": ",
               paste(round(LS[i, ], 3), collapse = ", "))
        }
        
        # ----------------------------
        # Recruitment matrix
        # ----------------------------
        
        R[i, 4] <- S[i, 4] * P_COY[i, 4] * LS[i, 4]
        R[i, 5] <- S[i, 5] * P_COY[i, 5] * LS[i, 5]
        R[i, 6] <- S[i, 6] * P_COY[i, 6] * LS[i, 6]
        R[i, 7:11] <- S[i, 7] * P_COY[i, 6] * LS[i, 6]
        R[i, 12:16] <- S[i, 12] * P_COY[i, 11] * LS[i, 11]
        R[i, 17:20] <- S[i, 17] * P_COY[i, 16] * LS[i, 16]
        
        A[1, ] <- R[i, ]
        lam[i, j] <- popbio::eigen.analysis(A)$lam
        
        # ----------------------------
        # Age transitions
        # ----------------------------
        
        for (a in 1:19) {
          n_bear[a + 1, i + 1, j] <- rbinom(
            1,
            n_bear[a, i, j],
            S[i, a]
          )
        }
        
        if (!terminalAgeClass) {
          surv_lastAge <- rbinom(1, n_bear[20, i, j], S[i, 20])
          n_bear[20, i + 1, j] <- n_bear[20, i + 1, j] + surv_lastAge
        }
        
        # ----------------------------
        # Newborn cubs
        # ----------------------------
        
        for (a in 5:20) {
          n_litter[a, i, j] <- rbinom(
            1,
            n_bear[a, i + 1, j],
            P_COY[i, a]
          )
          
          n_COY[a, i, j] <- n_litter[a, i, j] *
            min(rpois(1, LS[i, a]), 3)
        }
        
        n_bear[1, i + 1, j] <- sum(n_COY[, i, j])
        N_litter[j, i + 1] <- sum(n_litter[, i, j])
        
        # Save pre-harvest total
        N_bear_pre[j, i + 1] <- sum(n_bear[, i + 1, j])
        
        # Apply removals
        n_bear[, i + 1, j] <- pmax(
          n_bear[, i + 1, j] - removals_tot[, i + 1],
          0
        )
        
        N_bear[j, i + 1] <- sum(n_bear[, i + 1, j])
      }
    }
    
    list(
      pre = N_bear_pre,
      post = N_bear,
      removals = removals_tot,
      lambda = lam
    )
  }