# verify if variable is binary (numeric or text)
check_binary <- function(column) {
  if (length(unique(na.omit(column))) == 2) {
    return(T)
  }
  else return(F)
}

# verify if variable is binary numeric (assuming variable is binary)
check_binary_numeric <- function(column) {
  if (all(sort(unique(na.omit(column))) == c(0, 1))) {
    return(T)
  }
  else return(F)
}

# verify if variable is continuous (numeric, binary inclusive)
check_continuous <- function(column) {
  if (inherits(column, c("numeric", "integer", "complex", "double", "logical"))) {
    return(T)
  }
  else return(F)
}

# verify if variable is continuous (binary exclusive)
check_decimal <- function(column) {
  unique_values <- unique(na.omit(column))
  any_decimal <- sapply(unique_values, function(x) {x == round(x)})
  return(!all(any_decimal))
}

# traditional quantile values
trad_quantiles <- c(
  "Median: 0, 50, 100" = "0, 50, 100",
  "Tertile: 0, 33.33, 66.66, 100" = "0, 33.33, 66.66, 100",
  "Quartile: 0, 25, 50, 75, 100" = "0, 25, 50, 75, 100",
  "Quintile: 0, 20, 40, 60, 80, 100" = "0, 20, 40, 60, 80, 100",
  "Decile: 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100" = "0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100"
)


# calculate population sd/bootstrap estimate sd
bse <- function(x) sd(x) * sqrt((length(x) - 1) / length(x))

# cochran's Q test for heterogeneity
cochrans_q_het <- function(ate, cate0, cate100, se0, se100, estimand) {
  if (estimand == "rr" | estimand == "or") {
    cate0_diff <- log(cate0) - log(ate)
    cate100_diff <- log(cate100) - log(ate)
  } else if (estimand == "rd") {
    cate0_diff <- cate0 - ate
    cate100_diff <- cate100 - ate
  } else {}
  
  Q <- (cate0_diff/se0)^2 + (cate100_diff/se100)^2
  p <- pchisq(Q, 1, lower.tail = F)
  
  return(c(Q, p))
}

# calculate IP weights (unstabilized or stabilized, full or subset)
ipweights <- function(dataset, covariates, treatment, stabilize=F) {
  testform <- reformulate(names(covariates), treatment)
  w.out <- weightit(testform, data=dataset, method="glm", estimand="ATE", stabilize=stabilize)
  return(w.out$weights)
}

# estimate the doubly robust exposure effect (pass full sample level propensity score if estimating the CATE)
dr_estimate <- function(data, indices, y, z, X, estimand, cate = F, ps) {
    boot_data <- data[indices, ]
    boot_y <- y[indices]
    boot_z <- z[indices]
    boot_X <- X[indices, ]
    
    if (cate == F) propensity <- ipweights(boot_data, boot_X, boot_z)
    else propensity <- ps
    
    mu_1 <- predict(lm(boot_y ~ as.matrix(boot_X), subset = (boot_z == 1)), boot_X)
    mu_0 <- predict(lm(boot_y ~ as.matrix(boot_X), subset = (boot_z == 0)), boot_X)
    exp_y1 <- mean((boot_z * (boot_y - mu_1) / propensity) + mu_1)
    exp_y0 <- mean(((1 - boot_z) * (boot_y - mu_0) / (1 - propensity)) + mu_0)
    
    if (estimand == "rd") return(exp_y1 - exp_y0)
    else if (estimand == "rr") return(exp_y1/exp_y0)
    else if (estimand == "or") return( (exp_y1*(1-exp_y0)) / (exp_y0*(1-exp_y1)) )
}

# augumented iptw estimator with standard error estimates (doubly robust) from Lunceford and Davidian
aiptw <- function(data, subset, y, z, X, estimand, cate = F, ps) {
  subset_y <- y[subset]
  subset_z <- z[subset]
  subset_adj_X <- X[subset, ]
  subset_data <- data[subset, ]
  
  if (cate == F) propensity <- 1 / ipweights(subset_data, subset_adj_X, subset_z)
  else propensity <- 1 / ps[subset]
  
  m_1 <- predict(lm(subset_y ~ subset_z + as.matrix(subset_adj_X), subset = (subset_z == 1)), subset_adj_X)
  m_0 <- predict(lm(subset_y ~ subset_z + as.matrix(subset_adj_X), subset = (subset_z == 0)), subset_adj_X)
  mu_1 <- ((subset_z * subset_y) - (subset_z - propensity) * m_1) / propensity
  mu_0 <- (((1 - subset_z) * subset_y) + (subset_z - propensity) * m_0) / (1 - propensity)
  dr <- mean(mu_1) - mean(mu_0)
  
  # standard error
  dr_se <- sqrt(mean((mu_1 - mu_0 - dr)^2) / length(mu_1))
  
  return(c(dr, dr_se))
}

# estimate ATE using specified method
estimate_ate <- function(data, y, z, X, estimand, family, estimation_method, propensity) {
    
  if (estimation_method == "lr" | estimation_method == "iptw") {
      model <- glm(y ~ z + as.matrix(X), family=family, weights=propensity)
      
      if (estimand == "or") {
          ate <- exp(model$coefficients["z"])
          ate_confint <- exp(confint(model, parm = "z"))
      } else if (estimand == "rd") {
          ate <- model$coefficients["z"]
          ate_confint <- coefci(model, parm = "z", vcov = sandwich)
      } else if (estimand == "rr") {
          ate <- exp(model$coefficients["z"])
          ate_confint <- exp(coefci(model, parm = "z", vcov = sandwich))
      } else {}
  }
    
  if (estimation_method == "dr") {
      boot_ate <- boot(data=data, statistic=dr_estimate, R=100, y=y, z=z, X=X, estimand=estimand, cate=F)
      ate <- boot_ate$t0
      ate_confint <- boot.ci(boot_ate, conf=0.95, type="perc")$percent[4:5]
  }
    
  return(c(ate, ate_confint))
}

# estimate CATE using specified method
estimate_cate <- function(data, y, z, adj_X, subset, estimand, family, estimation_method, propensity) {
    
  if (estimation_method == "lr" | estimation_method == "iptw") {
    model <- glm(y ~ z + as.matrix(adj_X), family=family, subset=subset, weights=propensity)
    cate <- model$coefficients["z"]
    
    if (estimand == "or") {
        cate_se <- summary(model)$coefficients["z", "Std. Error"]
    } else if (estimand == "rd" | estimand == "rr") {
        cate_se <- as.matrix(coeftest(model, vcov. = sandwich))["z", "Std. Error"]
    } else {}
  }
    
  if (estimation_method == "dr") {
    aiptw_estimate <- aiptw(data, subset, y, z, adj_X, estimand, cate = T, propensity) # new function needed to obtain se
    cate <- aiptw_estimate[1]
    cate_se <- aiptw_estimate[2] # figure out how to estimate this later, using Lunceford and Davidian formula for now which assumes correct exposure and outcome model specification
  }
    
  return(c(cate, cate_se))
} # not finished

# create matrix of covariates that doesn't include specified candidate effect modifier (local function for conditional bootstrap method)
generate_adj_X <- function(em, X, P) {
  curr_em <- str_split(em, ":")[[1]]
  ind_rep_em <- which(c(paste0(names(P), "_1"), paste0(names(P), "_0"), names(P)) %in% curr_em) %% ncol(P)
  ind_rep_em <- ifelse(ind_rep_em == 0, ind_rep_em + ncol(P), ind_rep_em)
  
  adj_X <- X %>%
    dplyr::select(-any_of(names(P)[ind_rep_em]))
  
  return(adj_X)
}

# parallel processing
option3a <- function() {
  cl <- parallel::makeCluster(3, type = "FORK")
  plan(cluster, workers = cl)
  p <- progressor(along = names(ems))
  packages_required <- c("stringr", "dplyr", "tidyverse", "WeightIt", "data.table", "shiny")
  global_required <- c("data", "y", "z", "X", "P", "ems", "family", "quantiles", "B", "estimation_method", "test")
  
  results_list <- foreach(em = names(ems), .packages = packages_required, .export = global_required) %dopar% {
    source("helper-functions.R")
    adj_X <- generate_adj_X(em, X, P)
    
    results_item_bh_cates <- tibble(quantile=numeric(),
                                    em=character(),
                                    o_cate=numeric(),
                                    o_cate_se=numeric())
    
    results_item_em_rejectT <- if (test == "PQ") character(0) else NULL
    
    # estimate observed CATEs if conducting bootstrap hypothesis test or prior Cochran's Q test
    if (test == "BH" | test == "PQ") {
      mod_cate0 <- glm(y ~ z + as.matrix(adj_X), family=family, subset=which(get(em, ems) == 0))
      mod_cate100 <- glm(y ~ z + as.matrix(adj_X), family=family, subset=which(get(em, ems) == 1))
      
      if (test == "BH") {
        
        results_item_bh_cates <- tibble(
          quantile = c(0,100),
          em = em,
          o_cate = c(mod_cate0$coefficients["z"], mod_cate100$coefficients["z"]),
          o_cate_se = c(summary(mod_cate0)$coefficients["z", "Std. Error"],
                        summary(mod_cate100)$coefficients["z", "Std. Error"]))
        
      } else if (test == "PQ") {
        
        PQ_rejectT <- cochrans_q_het(ate = ate,
                                     cate0 = mod_cate0$coefficients["z"],
                                     cate100 = mod_cate100$coefficients["z"],
                                     se0 = summary(mod_cate0)$coefficients["z", "Std. Error"],
                                     se100 = summary(mod_cate100)$coefficients["z", "Std. Error"],
                                     estimand = estimand)[2] < alpha
        
        if (PQ_rejectT) {
          results_item_em_rejectT <- c(results_item_em_rejectT, em)
        } else {
          next
        }
      }
    }
    
    boot1 <- lapply(quantiles, function(q) {
      boot2 <- lapply(rep(1, B), function(b) {
        # bootstrap CATE data according to quantiles
        inem <- which(get(em, ems) == 1)
        outem <- which(get(em, ems) == 0)
        ind_inem <- sample(inem, size = round(nrow(P) * q/100), replace = T)
        ind_outem <- sample(outem, size = round(nrow(P) * (100-q)/100), replace = T)
        cate_ind <- c(ind_inem, ind_outem)
        
        # create outcome model and add estimated CATE (and SE) to bootstrap storage structure
        cate_estimate <- estimate_cate(data, y, z, adj_X, cate_ind, estimand, family, estimation_method, propensity)
        cate <- cate_estimate[1]
        cate_se <- cate_estimate[2]
        
        return(tibble(
          quantile = q,
          em = em,
          cate = cate,
          cate_se = cate_se))
      })
      return(rbindlist(boot2))
    })
    
    # cancel
    interruptor$execInterrupts()
    
    p(sprintf("em=%s", em))
    progress$inc(1/length(names(ems)), detail = sprintf("em = %s", em))
    
    return(list(boots = rbindlist(boot1), 
                bh_cates = results_item_bh_cates, 
                em_rejectT = results_item_em_rejectT))
  } # end foreach
  
  stopCluster(cl = cl)
  
  return(results_list)
}