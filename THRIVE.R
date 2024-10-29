# THRIVE (Transparent High-dimensional Reproducible Inference through Bootstrap Validation and Exploration)

# Load libraries and helper functions
library(tidyverse) # data manipulation, ggplot
library(WeightIt) # ipw package
library(sandwich) # robust standard errors for ATE CI
library(lmtest)
library(pals) # color palette for plot
library(geomtextpath) # label for ATE on plot
library(lubridate) # time for progress bar
source("helper-functions.R")

#   Plot displays median cate values
#   Currently only works for categorical effect modifiers 
#   CATEs for quantiles 0 and 100 must be estimated
#   Also assuming if any variables from P are in matrix of confounders, they are named the same as in P
## y: Outcome vector
## z: Exposure vector
## X: Matrix of confounders
## P: Matrix of binary candidate effect modifiers
## data: Full data sample
## quantiles: Quantiles of EMs to bootstrap and test; numeric vector (0 and 100 must be included)
## B: Number of bootstrap samples
## estimand: Either multiplicative ("or" for odds ratio, "rr" for risk ratio) or additive ("rd" for risk difference) scale
## estimation method: "lr" for adjusted linear regression, "iptw" for inverse probability of treatment weighting, "dr" for doubly robust estimation
## test: Test to assess heterogeneity; "Q" for Cochran's Q (default), "PQ" for Cochran's Q test prior to bootstrapping, "BH" for bootstrap hypothesis test, "CI" for checking overlap of confidence intervals with ATE
## alpha: type-1 error level with which heterogeneity test p-value is compared (applicable for Q and BH)

# Removed for now:
## show_bootd: Option to overlay bootstrap CATE densities on plot; "one" for example overlay of one EM, "all" for all EMs, "none" for no EMs
## show_bootci: Option to overlay bootstrap CATE confidence intervals on plot; "one" for example overlay of one EM, "all" for all EMs, "none" for no EMs

thrive <- function(y, z, X, P, data,
                   quantiles = c(0,25,50,75,100), 
                   B = 1000, 
                   estimand = "rd",
                   estimation_method = "lr",
                   test = "Q",
                   alpha = 0.05) {
                   # show_bootd = "none", 
                   # show_bootci = "none")

    # check that quantiles includes 0 and 100, if it doesn't throw error
    if (any(!(c(0,100) %in% quantiles))) stop("0 and/or 100 are not included in vector of quantiles")
    
    # if candidate effect modifiers are unnamed, assign names
    if (sum(is.na(colnames(P))) != 0){
        colnames(P)[which(is.na(colnames(P)))] <- paste0("em", 1:sum(is.na(colnames(P))))
    } 
    
    # Get levels of candidate effect modifiers
    P_levels <- P %>%
        map(unique) %>%
        map(na.omit)
    
    P <- as_tibble(P) %>%
        mutate_all(as_factor)
    
    # get names of multilevel categorical variables
    mlP <- names(which(lengths(P_levels) > 2))
    
    # get names of binary character variables
    cP <- P_levels %>%
        map(as.numeric) %>%
        map(is.na) %>%
        map(all) %>%
        as_vector()
    bcP <- names(which(cP[!(names(cP) %in% all_of(mlP))]))
    
    # create dummy indicators for multilevel categorical variables and binary character variables
    for (p in c(mlP, bcP)) {
        mlP_form <- as.formula(paste0("~ -1 + ", p))
        P <- P %>%
            select(-all_of(p)) %>%
            bind_cols(as.data.frame(model.matrix(mlP_form, model.frame(~ ., P, na.action=na.pass))))
    }
    
    duplicate_bcP <- P_levels[bcP] %>% 
        map_chr(c(2)) %>%
        paste0(bcP, .)
    
    P <- P %>% 
        select(-all_of(duplicate_bcP)) %>%
        rename_with(~ gsub(" ", "_", .x))
    
    # create data frame of EM indicators for all 1 and 2-way interactions
    P <- P %>%
        mutate_if(is.factor, ~ as.numeric(.)-1)
    
    counter_P <- P %>%
        mutate_all(~ case_when(.x == 1 ~ 0,
                               .x == 0 ~ 1))
    names(counter_P) <- paste0(names(counter_P), "_0")
    
    full_P <- P %>%
        rename_with(~ paste0(.x, "_1")) %>%
        cbind(., counter_P)
    
    ems <- as.data.frame(model.matrix(~ .^2 - 1, data = full_P))
    
    remove <- c(paste0(names(P), "_0"), paste0(names(P), "_1"), 
                paste0(names(P), "_1:", names(P), "_0"),
                paste0(names(P), "_0:", names(P), "_1"))
    
    if (length(mlP) != 0) {
        remove <- c(remove, names(full_P)[str_detect(names(full_P), paste(mlP, collapse="|"))])
    }
    
    ems <- ems %>%
        dplyr::select(-any_of(remove)) %>%
        cbind(P, .)
    
    # check positivity: print prevalence of observations for each effect modifier interaction (and size)
    #   throw warning if less than 5% or more than 95% prevalence
    cat("Checking positivity...\n")
    tab1 <- tibble("Effect Modifier (Interaction)" = names(ems),
                   "Prevalence" = paste0(round(apply(ems, 2, mean, na.rm=T)*100, 2), "%"),
                   "In-Count" = apply(ems, 2, sum, na.rm=T),
                   "Out-Count" = apply(ems, 2, function (x) sum(x == 0, na.rm = T)))
    
    colnames(tab1)[2] <- paste0("Prevalence, N=", nrow(ems), "(%)")
    
    if (any(tab1$`In-Count`/nrow(ems) < 0.05) | any(tab1$`Out-Count`/nrow(ems) < 0.05)) {
        prob_em <- tab1 %>%
            filter(`In-Count`/nrow(ems) < 0.05 | `Out-Count`/nrow(ems) < 0.05) %>%
            pull(`Effect Modifier (Interaction)`)
        warning("The following interactions have less than 5% or more than 95% prevalence. Bootstrap results may be biased:")
        print(prob_em)
    }
    
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
    
    # set regression model family based on desired estimand
    if (estimand == "or") {
        family <- "binomial"
    } else if (estimand == "rd") {
        family <- "gaussian"
    } else if (estimand == "rr") {
        family <- "poisson"
    } else {}
    
    # estimate propensity score if estimation method is IPTW or DR
    if (estimation_method == "iptw" | estimation_method == "dr") {
        propensity <- ipweights(data, X, z)
    } else if (estimation_method == "lr") {
        propensity <- rep(1, length(z))
    } else {}
    
    # compute full data ATE
    ate_estimate <- estimate_ate(data, y, z, X, estimand, family, estimation_method, propensity)
    ate <- ate_estimate[1]
    ate_confint <- ate_estimate[2:3]
    
    # initialize counters and time storage vectors
    init <- numeric()
    end <- numeric()
    em_counter <- 0
    b_counter <- 0
    n_ems <- ncol(ems)
    n_quantiles <- length(quantiles)
    n_iter <- n_ems*n_quantiles*B
    
    # if performing Cochran's Q test before bootstrapping, create list of effect modifiers
    if (test == "PQ") em_rejectT <- character(0)
    
    # conditional bootstrap
    cat("\n\nPerforming conditional boostrap:\n")
    
    for (em in names(ems)) {
        
        # counter for em
        em_counter <- em_counter + 1
        
        # set quantile counter 
        q_counter <- 0 
        
        # create matrix of confounders and effect modifiers that don't belong to current em
        adj_X <- generate_adj_X(em, X, P)
        
        # estimate observed CATEs if conducting bootstrap hypothesis test or prior Cochran's Q test
        if (test == "BH" | test == "PQ") {
            
            mod_cate0 <- glm(y ~ z + as.matrix(adj_X), family = family, subset = which(get(em, ems) == 0))
            mod_cate100 <- glm(y ~ z + as.matrix(adj_X), family = family, subset = which(get(em, ems) == 1))
            
            if (test == "BH") {
                
                bh_cates <- bh_cates %>% 
                    add_row(quantile = c(0,100),
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
        
        for (q in quantiles) {
            
            # counter for quantile
            q_counter <- q_counter + 1
            
            for (b in 1:B) {
                
                # counter for bootstrap
                b_counter <- b_counter + 1
                
                # get time before bootstrap
                init <- c(init, Sys.time())
                
                # bootstrap CATE data according to quantiles
                inem <- which(get(em, ems) == 1)
                outem <- which(get(em, ems) == 0)
                ind_inem <- sample(inem, size=round(nrow(P)*q/100), replace=T)
                ind_outem <- sample(outem, size=round(nrow(P)*(100-q)/100), replace=T)
                cate_ind <- c(ind_inem, ind_outem)
                
                # create outcome model and add estimated CATE (and SE) to bootstrap storage structure
                cate_estimate <- estimate_cate(data, y, z, adj_X, cate_ind, estimand, family, estimation_method, propensity)
                
                boots <- boots %>% add_row(quantile = q,
                                           em = em, 
                                           cate = cate_estimate[1],
                                           cate_se = cate_estimate[2])
                
                # get time after bootstrap
                end <- c(end, Sys.time())
                
                # estimated remaining time (average time it took to run other iterations)
                time <- round(seconds_to_period(sum(end - init)), 0)
                est <- n_iter*(mean(end[end != 0] - init[init != 0])) - time
                remaining <- round(seconds_to_period(est), 0)
                
                percent <- b_counter/n_iter * 100
                
                # print progress
                cat(paste0(sprintf('\r[%-50s] %d%%',
                                   paste(rep('=', percent / 2), collapse = ''),
                                   floor(percent)),
                           " | Execution time:", time,
                           " | Estimated time remaining:", remaining,
                           " | Effect Modifier:", em_counter, "/", n_ems,
                           " | Quantile:", q_counter, "/", n_quantiles,
                           " | Bootstrap: ", b, "/", B, "     "))
            }
        }
    }
    
    cat("\n\n Conditional bootstrap complete.")
    
    # calculate means/medians, bootstrap confidence intervals
    if (estimand == "or" | estimand == "rr") {
        statistics <- boots %>%
            group_by(quantile, em) %>%
            summarise(mean_cate = mean(exp(cate)),
                      median_cate = median(exp(cate)),
                      cate_LP_se = bse(cate),
                      cate_ci_lwr = quantile(exp(cate), probs = 0.025),
                      cate_ci_upr = quantile(exp(cate), probs = 0.975))
    } else if (estimand == "rd") {
        statistics <- boots %>%
            group_by(quantile, em) %>%
            summarise(mean_cate = mean(cate),
                      median_cate = median(cate),
                      cate_LP_se = bse(cate),
                      cate_ci_lwr = quantile(cate, probs = 0.025),
                      cate_ci_upr = quantile(cate, probs = 0.975))
    } else {}
    
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
            filter(quantile == 0 | quantile == 100) %>%
            group_by(em) %>%
            arrange(quantile) %>%
            summarise(Q_rejectT = cochrans_q_het(ate = ate,
                                                 cate0 = first(mean_cate),
                                                 cate100 = last(mean_cate),
                                                 se0 = first(cate_LP_se),
                                                 se100 = last(cate_LP_se),
                                                 estimand = estimand)[2] < alpha) %>%
            filter(Q_rejectT == T) %>%
            pull(em)
        
    # conduct bootstrap hypothesis one-sample test
    } else if (test == "BH") {
        
        bh_cates <- bh_cates %>%
            mutate(t = case_when(family == "gaussian" ~ (o_cate-ate)/o_cate_se,
                                 family == "binomial" | family == "poisson" ~ (o_cate-log(ate))/o_cate_se))
        
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
    
    plot_statistics <- statistics %>%
        filter(em %in% em_rejectT) %>%
        mutate(color = em,
               alpha = ifelse(cate_visible == F, 0.3, 1.0))
    
    em_colors <- tibble(em = unique(statistics$em),
                        color = unname(glasbey(length(unique(statistics$em)))))
    
    cate_edges <- plot_statistics %>%
        group_by(em) %>%
        arrange(quantile) %>%
        reframe(x1 = lag(quantile),
                x2 = quantile,
                y1 = lag(median_cate),
                y2 = median_cate,
                color = em,
                alpha = case_when(lag(cate_visible) + cate_visible == 2 ~ 1.0,
                                  lag(cate_visible) + cate_visible == 1 ~ 0.5,
                                  T ~ 0.3),
                linetype = case_when(lag(cate_visible) + cate_visible == 2 ~ "solid",
                                     lag(cate_visible) + cate_visible == 1 ~ "dashed",
                                     T ~ "dotted")) %>%
        filter(!is.na(x1))
    
    # plot bootstraped CATEs
    plot <- ggplot() +
        geom_labelhline(yintercept=ate, label="ATE", 
                        hjust=0.95, color="#A6A6A6", linewidth=1.5,
                        textcolour="black") +
        geom_hline(yintercept=ate_confint, linewidth=0.5, alpha=0.3, linetype="dotdash") +
        geom_point(data=plot_statistics, 
                   aes(x=quantile, y=median_cate, group=em, 
                       color=color, alpha=I(alpha))) +
        scale_color_manual(breaks = em_colors$em,
                           values = em_colors$color) +
        scale_x_continuous(breaks = quantiles) + 
        geom_segment(data=cate_edges,
                     aes(x=x1, xend=x2, y=y1, yend=y2,
                         color=color, alpha=alpha,
                         linetype=linetype)) + 
        scale_alpha(guide = 'none') +
        scale_linetype_identity(guide = "legend",
                                labels = c("Significant","Between Significance Threshold","Insignificant"),
                                breaks = c("solid","dashed","dotted")) +
        labs(linetype="Heterogeneity",
             x = "Quantile",
             color = "Effect Modifier",
             title = "Bootstrapped CATE Estimates by Quantile") 
    
    if (estimand == "or") {
        plot <- plot +
            ylab("Median CATE (odds ratio)")
    } else if (estimand == "rd") {
        plot <- plot +
            ylab("Median CATE (risk difference)")
    } else if (estimand == "rr") {
        plot <- plot +
            ylab("Median CATE (risk ratio)")
    }
    
    # return table of EM counts, bootstrap results (full and summarized), and plot
    return(list(table1 = tab1,
                boots = boots,
                stats = statistics,
                plot = plot))
}

