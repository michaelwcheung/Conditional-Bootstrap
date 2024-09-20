# first attempt at parallelization using future lapply + progressr
# for - future_lapply - for

boot_fflf <- function(fut_globals) {
  handlers(global = TRUE)
  handlers("cli")
  plan(multisession, workers = 2)
  registerDoFuture()
  
  # instantiate bootstrap storage structure
  boots <- tibble(quantile = numeric(),
                  em = character(),
                  cate = numeric(),
                  cate_se = numeric())
  
  # if conducting bootstrap hypothesis test, instantiate storage structure for observed CATEs
  if (test == "BH") {
    bh_cates <- tibble(quantile = numeric(),
                       em = character(),
                       o_cate = numeric(),
                       o_cate_se = numeric())
  }
  
  # compute ate
  ate_estimate <- estimate_ate(data, y, z, X, estimand, family, estimation_method, propensity)
  ate <- ate_estimate[1]
  ate_confint <- ate_estimate[2:3]
  
  # main computation
  # initialize counters and time storage vectors
  init <- numeric()
  end <- numeric()
  em_counter <- 0
  b_counter <- 0
  n_ems <- ncol(ems)
  n_quantiles <- length(quantiles)
  n_iter <- n_ems * n_quantiles * B
  
  p <- progressr::progressor(along = 1:n_iter)
  
  if (test == "PQ") em_rejectT <- character(0) 
  
  for (em in names(ems)) {
    em_counter <- em_counter + 1
    q_counter <- 0
    
    adj_X <- generate_adj_X(em, X, P)
    
    # estimate observed CATEs if conducting bootstrap hypothesis test or prior Cochran's Q test
    if (test == "BH" | test == "PQ") {
      mod_cate0 <- glm(y ~ z + as.matrix(adj_X), family=family, subset=which(get(em, ems) == 0))
      mod_cate100 <- glm(y ~ z + as.matrix(adj_X), family=family, subset=which(get(em, ems) == 1))
      
      if (test == "BH") {
        
        bh_cates <- bh_cates %>% add_row(
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
          em_rejectT <- c(em_rejectT, em)
        } else {
          next
        }
      }
    }
    
    
    
    flf <- future_lapply(quantiles, function(q) {
      q_counter <- q_counter + 1
      
      boots1 <- tibble(quantile = numeric(),
                      em = character(),
                      cate = numeric(),
                      cate_se = numeric())
      
      for (b in 1:B) {
        b_counter <- b_counter + 1
        init <- c(init, Sys.time())
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
        
        end <- c(end, Sys.time())
        
        time <- round(seconds_to_period(sum(end - init)), 0)
        est <- n_iter*(mean(end[end != 0] - init[init != 0])) - time
        remaining <- round(seconds_to_period(est), 0)
        
        percent <- b_counter/n_iter * 100
        
        cat(paste0(sprintf('\r[%-50s] %d%%',
                           paste(rep('=', percent / 2), collapse = ''),
                           floor(percent)),
                   " | Execution Time: ", time,
                   " | Estimated Time Remaining: ", remaining,
                   " | Effect Modifier:", em_counter, "/", n_ems,
                   " | Quantile:", q_counter, "/", n_quantiles,
                   " | Bootstrap: ", b, "/", B, "     "))
        
        p(message = sprintf("em: %s | quantile: %d | bootstrap : %d", em, q, b))
        
        boots1 <- rbind(boots1, tibble(
          quantile = q,
          em = em,
          cate = cate,
          cate_se = cate_se))
      }
      return(list(boots = boots1))
    }, future.chunk.size = 1, future.seed = TRUE)
    
    boots <- rbind(boots, rbindlist(lapply(flf, function(x) x$boots)))
  }
  
  # lll_result <- lll()
  # boots <- rbindlist(lapply(lll_result, function(x) x$boots))
  # bh_cates <- rbindlist(lapply(lll_result, function(x) x$bh_cates))
  # em_rejectT <- lapply(lll_result, function(x) x$em_rejectT)
  
  # calculate means/medians, bootstrap confidence intervals
  if (estimand == "or" | estimand == "rr") {
    statistics <- boots %>%
      group_by(quantile, em) %>%
      summarise(mean_cate = mean(exp(cate)),
                median_cate = median(exp(cate)),
                cate_lp_se = bse(cate),
                cate_ci_lwr = quantile(exp(cate), probs = 0.025),
                cate_ci_upr = quantile(exp(cate), probs = 0.975))
  } else if (estimand == "rd") {
    statistics <- boots %>%
      group_by(quantile, em) %>%
      summarise(mean_cate = mean(cate),
                median_cate = median(cate),
                cate_lp_se = bse(cate),
                cate_ci_lwr = quantile(cate, probs = 0.025),
                cate_ci_upr = quantile(cate, probs = 0.975))
  } else {}
  
  # make CATEs solid colors on plot if they are outside the ATE 95% CI
  statistics <- statistics %>%
    mutate(cate_visible = case_when(median_cate > ate_confint[2] ~ T,
                                    median_cate < ate_confint[1] ~ T,
                                    T ~ F))
  
  # conduct heterogeneity test
  ## check for overlapping of CIs
  if (test == "CI") {
    
    ### candidates reject null hypothesis of homogeneity if at least one of the 0 or 100 quantile CIs do not overlap with the ATE CI
    em_rejectT <- statistics %>%
      filter(quantile == 0 | quantile == 100) %>%
      mutate(cate_ci_rejectT = case_when(cate_ci_lwr > ate_confint[2] ~ T,
                                         cate_ci_upr < ate_confint[1] ~ T,
                                         T ~ F)) %>%
      filter(cate_ci_rejectT == T) %>%
      pull(em) %>%
      unique()
    
    # conduct Cochran's Q test
  } else if (test == "Q") {
    
    em_rejectT <- statistics %>%
      group_by(em) %>%
      filter(quantile == min(quantiles) | quantile == max(quantiles)) %>%
      arrange(quantile) %>%
      summarise(Q_rejectT = cochrans_q_het(ate = ate,
                                           cate0 = first(mean_cate),
                                           cate100 = last(mean_cate),
                                           se0 = first(cate_lp_se),
                                           se100 = last(cate_lp_se),
                                           estimand = estimand)[2] < alpha) %>%
      filter(Q_rejectT == T) %>%
      pull(em)
    
    # conduct bootstrap hypothesis one-sample test
  } else if (test == "BH") {
    
    bh_cates <- bh_cates %>%
      mutate(t = case_when(family == "gaussian" ~ (o_cate-ate)/o_cate_se,
                           family == "binomial" ~ (o_cate-log(ate))/o_cate_se))
    
    em_rejectT <- boots %>%
      left_join(., bh_cates) %>%
      filter(quantile == 0 | quantile == 100) %>%
      mutate(boot_t = (cate-o_cate)/cate_se) %>%
      group_by(quantile, em) %>%
      summarise(bh_reject_pval = mean(abs(boot_t) > abs(t))) %>%
      mutate(bh_rejectT = bh_reject_pval < alpha) %>%
      filter(bh_rejectT == T) %>%
      pull(em) %>%
      unique()
    
  } else {}
  
  result <- list(
    ate = ate,
    ate_confint = ate_confint,
    statistics = statistics,
    em_rejectT = em_rejectT
  )
  
  return(result)
}