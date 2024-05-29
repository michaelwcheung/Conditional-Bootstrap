# Server

library(shiny) # shiny app
library(DT) # data tables in shiny app
library(tidyverse) # data manipulation, ggplot
library(pals) # color palette for plot
library(geomtextpath) # label for ATE on plot
library(lubridate) # time for progress bar

server <- function(input, output) {
    
    #--------------------------------load data and set parameters (incorporate into app later)----------------------------------------- 
    
    # load data and set outcome and exposure vectors, covariate and effect modifier matrices
    data <- read_csv("~/Documents/emm_local/data/data_droughts_malnutrition_101822.csv")
    # data <- data[sample(nrow(data), 10000, replace=F),]
    y <- pull(data, stunted)
    z <- pull(data, drought)
    X <- select(data, -c(stunted, drought))
    P <- select(X, education_none, mass_media, rural_residence)
    quantiles = c(0,25,50,75,100)
    B = 200
    estimand = "rd" 
    test = "BH"
    alpha = 0.05
    # show_bootd = show_bootci = "none"

    #---------------------------------helper functions--------------------------------------------------------------------------------
    
    # calculate population sd/bootstrap estimate sd
    bse <- function(x) sd(x)*sqrt((length(x)-1)/length(x))
    
    # cochran's Q test for heterogeneity
    cochrans_q_het <- function(ate, cate0, cate100, se0, se100, estimand) {
        
        if (estimand == "rr" | estimand == "or") {
            cate0_diff <- log(cate0) - log(ate)
            cate100_diff <- log(cate100) - log(ate)
            se0 <- log(se0)
            se100 <- log(se100)
        } else if (estimand == "rd") {
            cate0_diff <- cate0 - ate
            cate100_diff <- cate100 - ate
        } else {}
        
        Q <- (cate0_diff/se0)^2 + (cate100_diff/se100)^2
        p <- pchisq(Q, 1, lower.tail = F)
        
        return(c(Q, p))
    }
    
    #----------------------------------------------------------------------------------------------------------------------------------
    
    
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
    
    # check positivity: print proportions of observations for each effect modifier interaction (and size)
    #   throw warning if less than 5 observations in or not in
    cat("Checking positivity...\n")
    tab1 <- tibble("Effect Modifier (Interaction)" = names(ems),
                   "Proportion in Data" = paste0(round(apply(ems, 2, mean, na.rm=T)*100, 2), "%"),
                   "In-Count" = apply(ems, 2, sum, na.rm=T),
                   "Out-Count" = apply(ems, 2, function (x) sum(x == 0, na.rm = T)))
    
    colnames(tab1)[2] <- paste0("Proportion in Data (N=", nrow(ems), ")")
    
    #------------------------Implement throw as popup later------------------------------------------------------------------------
    if (any(tab1$`In-Count`<=5) | any(tab1$`Out-Count`<=5)) {
        prob_em <- tab1 %>%
            filter(`In-Count`<=5 | `Out-Count`<=5) %>%
            pull(`Effect Modifier (Interaction)`)
        warning("The following interactions have less than 5 in- or out-count observations. Bootstrap results may be biased:")
        print(prob_em)
    }
    #----------------------------------------------------------------------------------------------------------------------------------
    
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
    
    # compute full data ATE 
    if (estimand == "or") {
        ate_mod <- glm(y ~ z + as.matrix(X), family = binomial)
        ate <- exp(ate_mod$coefficients["z"])
        ate_confint <- exp(confint(ate_mod)["z",1:2])
    } else if (estimand == "rd") {
        ate_mod <- lm(y ~ z + as.matrix(X))
        ate <- ate_mod$coefficients["z"]
        ate_confint <- confint(ate_mod)["z",1:2]
    } else {}
    
    #--------------------------------------------------------Implement progress bar as popup later---------------------------- 
    # initialize counters and time storage vectors
    init <- numeric()
    end <- numeric()
    em_counter <- 0
    b_counter <- 0
    n_ems <- ncol(ems)
    n_quantiles <- length(quantiles)
    n_iter <- n_ems*n_quantiles*B
    #-------------------------------------------------------------------------------------------------------------------------- 
    
    # set regression model family based on desired estimand
    if (estimand == "or") {
        family <- "binomial"
    } else if (estimand == "rd") {
        family <- "gaussian"
    } else {}
    
    # conditional bootstrap
    cat("\n\nPerforming conditional boostrap:\n")
    
    for (em in names(ems)) {
        
        # counter for em
        em_counter <- em_counter + 1
        
        # set quantile counter 
        q_counter <- 0 
        
        # create matrix of confounders and effect modifiers that don't belong to current em
        curr_em <- str_split(em,":")[[1]]
        ind_rep_em <- which(c(paste0(names(P), "_1"), paste0(names(P), "_0")) %in% curr_em) %% ncol(P)
        ind_rep_em <- ifelse(ind_rep_em == 0, ind_rep_em + ncol(P), ind_rep_em)
        
        adj_X <- X %>%
            dplyr::select(-any_of(names(P)[ind_rep_em]))
        
        # if conducting bootstrap hypothesis test, estimate observed CATEs
        if (test == "BH") {
            
            mod_cate0 <- glm(y ~ z + as.matrix(adj_X), family = family, subset = which(get(em, ems) == 0))
            mod_cate100 <- glm(y ~ z + as.matrix(adj_X), family = family, subset = which(get(em, ems) == 1))
            
            bh_cates <- bh_cates %>% 
                add_row(quantile = c(0,100),
                        em = em,
                        o_cate = c(mod_cate0$coefficients["z"], mod_cate100$coefficients["z"]),
                        o_cate_se = c(summary(mod_cate0)$coefficients["z", "Std. Error"],
                                      summary(mod_cate100)$coefficients["z", "Std. Error"]))
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
                mod <- glm(y ~ z + as.matrix(adj_X), family = family, subset = cate_ind)
                
                boots <- boots %>% add_row(quantile = q,
                                           em = em,
                                           cate = mod$coefficients["z"],
                                           cate_se = summary(mod)$coefficients["z", "Std. Error"])
                
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
    if (estimand == "or") {
        statistics <- boots %>%
            group_by(quantile, em) %>%
            summarise(mean_cate = mean(exp(cate)),
                      median_cate = median(exp(cate)),
                      cate_se = bse(exp(cate)),
                      cate_ci_lwr = quantile(exp(cate), probs = 0.025),
                      cate_ci_upr = quantile(exp(cate), probs = 0.975))
    } else if (estimand == "rd") {
        statistics <- boots %>%
            group_by(quantile, em) %>%
            summarise(mean_cate = mean(cate),
                      median_cate = median(cate),
                      cate_se = bse(cate),
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
                                                 se0 = first(cate_se),
                                                 se100 = last(cate_se),
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
            pull(em)
        
    } else {}
    
    # filter out effect modifiers that failed to reject null of homogeneity
    plot_statistics <- statistics %>%
        filter(em %in% em_rejectT) %>%
        mutate(color = em,
               alpha = ifelse(cate_visible == F, 0.3, 1.0))
    
    # assign colors to effect modifiers
    em_colors <- tibble(em = unique(statistics$em),
                        color = unname(glasbey(length(unique(statistics$em)))))
    
    # create line segments on plot
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
    
    # plot ATE
    plot <- ggplot() +
        scale_x_continuous(breaks = quantiles, limits = c(min(quantiles), max(quantiles))) +
        scale_y_continuous(limits = c(min(plot_statistics$median_cate),
                                      max(plot_statistics$median_cate))) +
        geom_labelhline(yintercept=ate, label="ATE", 
                        hjust=0.95, color="#A6A6A6", linewidth=1.5,
                        textcolour="black") +
        geom_hline(yintercept=ate_confint, linewidth=0.5, alpha=0.3, linetype="dotdash") +
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
    } else {}
    
    # read selected effect modifiers from checkboxes
    selected_lines <- reactiveVal(em_rejectT)
    
    output$checkboxes <- renderUI({
        checkboxGroupInput("lines", label = "Effect Modifier",
                           choices = setNames(em_rejectT, em_rejectT),
                           selected = selected_lines())
    })
    
    observeEvent(input$lines, {
        selected_lines(input$lines)
        output$Plot <- renderPlot({
            
            # filter data based on selected lines
            plot_statistics_selected <- plot_statistics %>%
                filter(em %in% input$lines)
            cate_edges_selected <- cate_edges %>%
                filter(em %in% input$lines)
            
            # plot CATEs and line segments
            plot + geom_point(data=plot_statistics_selected,
                              aes(x=quantile, y=median_cate, group=em,
                                  color=color, alpha=I(alpha))) +
                scale_color_manual(breaks = em_colors$em,
                                   values = em_colors$color) +
                geom_segment(data=cate_edges_selected,
                             aes(x=x1, xend=x2, y=y1, yend=y2,
                                 color=color, alpha=alpha,
                                 linetype=linetype)) +
                scale_alpha(guide = 'none') +
                scale_linetype_identity(guide = "legend",
                                        labels = c("Significant","Between Significance Threshold","Insignificant"),
                                        breaks = c("solid","dashed","dotted"))
        })
    })
    
    # ranking of CATEs by effect modifier and quantile
    tab2 <- statistics %>%
        filter(em %in% em_rejectT) %>%
        select(quantile, em,  median_cate, mean_cate, cate_ci_lwr, cate_ci_upr) %>%
        arrange(desc(abs(median_cate))) %>%
        mutate(across(c(median_cate, mean_cate, cate_ci_lwr, cate_ci_upr), \(x) round(x, 3))) %>%
        dplyr::rename("Quantile" = quantile,
                      "Effect Modifier" = em,
                      "Median CATE" = median_cate,
                      "Mean CATE" = mean_cate,
                      "Lower 95% CI" = cate_ci_lwr,
                      "Upper 95% CI" = cate_ci_upr)
    
    # table of regression coefficients
    if (length(em_rejectT) != 0) {
        tab3 <- statistics %>%
            filter(em %in% em_rejectT) %>%
            group_by(em) %>%
            group_modify(~ broom::tidy(lm(median_cate ~ quantile, data = .x))) %>%
            filter(term != "(Intercept)") %>%
            select(em, estimate, p.value) %>%
            arrange(desc(abs(estimate))) %>%
            mutate(estimate = round(estimate, 5),
                   p.value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))) %>%
            dplyr::rename("Effect Modifier" = em,
                          "Coefficient Estimate" = estimate,
                          "P-value" = p.value)
    } else {
        tab3 <- tibble("Effect Modifier" = character(0),
                       "Coefficient Estimate" = numeric(0),
                       "P-value" = numeric(0))
    }

    output$tab1 <- DT::renderDataTable(DT::datatable(tab1, rownames = F))
    output$tab2 <- DT::renderDataTable(DT::datatable(tab2, rownames = F))
    output$tab3 <- DT::renderDataTable(DT::datatable(tab3, rownames = F))
    
    #--------------------------------------------------------Implement CATE CIs and density options later----------------------------
    # # add colors to bootstrap color structure
    # cboots <- boots %>%
    #     mutate(color = ifelse(em %in% em_rejectT, em, "."))
    # 
    # # choose EMs with most number of 'significant' median cates across quantiles to display densities or cis
    # em_show <- statistics %>%
    #     group_by(em) %>%
    #     reframe(significant_cates = sum(cate_visible),
    #             largest_cate = max(abs(median_cate))) %>%
    #     arrange(desc(significant_cates), desc(largest_cate)) %>%
    #     dplyr::slice(1:2) %>%
    #     pull(em)
    # 
    # # EM confidence intervals by quantile
    # if (show_bootci == "one") {
    #     
    #     # show confidence intervals for one EM
    #     plot <- plot +
    #         geom_errorbar(data=filter(plot_statistics, em==em_show[1]),
    #                       aes(x=quantile,
    #                           ymin=cate_ci_lwr, 
    #                           ymax=cate_ci_upr,
    #                           group=em,
    #                           color=em),
    #                       alpha=0.6,
    #                       width=30/length(quantiles))
    #     
    # } else if (show_bootci == "all") {
    #     
    #     # show confidence intervals for all EMs
    #     plot <- plot +
    #         geom_errorbar(data=plot_statistics,
    #                       aes(x=quantile,
    #                           ymin=cate_ci_lwr, 
    #                           ymax=cate_ci_upr,
    #                           group=em,
    #                           color=em),
    #                       alpha=0.6,
    #                       width=30/length(quantiles))
    #     
    # } else if (show_bootci == "none") {
    # } else {}
    # 
    # # EM densities by quantile
    # emd <- cboots %>%
    #     mutate(emq = paste(em, quantile, sep='.')) %>%
    #     group_by(emq, color) %>%
    #     reframe(x = density(cate)$x,
    #             y = density(cate)$y/(5*length(quantiles)) + unique(quantile))
    # 
    # if (show_bootd == "one") {
    #     
    #     one_emd <- emd %>%
    #         filter(startsWith(emq, em_show[2]))
    #     
    #     # show one EM bootstrap estimates and densities at each quantile
    #     plot <- plot + 
    #         geom_point(data=filter(cboots, em == em_show[2]), 
    #                    aes(x=quantile, y=cate, color=color),
    #                    alpha=0.03) + 
    #         geom_path(data=one_emd, 
    #                   aes(x=y, y=x, group=emq, color=color), 
    #                   alpha=0.4)
    #     
    # } else if (show_bootd == "all") {
    #     
    #     # show all EM bootstrap densities at each quantile
    #     plot <- plot + 
    #         geom_point(data=cboots, 
    #                    aes(x=quantile, y=cate, color=color),
    #                    alpha=0.03) + 
    #         geom_path(data=emd, 
    #                   aes(x=y, y=x, group=emq, color=color), 
    #                   alpha=0.4)
    #     
    # } else if (show_bootd == "none") {
    # } else {}
    #--------------------------------------------------------------------------------------------------------------------------
}
