# Server

library(shiny) # Shiny app
library(DT) # data tables in Shiny app
library(tidyverse) # data manipulation, ggplot
library(pals) # color palette for plot
library(geomtextpath) # label for ATE on plot
# library(lubridate) # time for progress bar

server <- function(input, output) {
    
    #--------------------------------load data and set parameters (incorporate into app later)----------------------------------------- 
    data <- read_csv("~/Documents/emm_local/data/data_droughts_malnutrition_101822.csv")
    data <- data[sample(nrow(data), 10000, replace=F),]
    y <- pull(data, stunted)
    z <- pull(data, drought)
    X <- select(data, -c(stunted, drought))
    ems <- select(X, education_none, mass_media, rural_residence)
    quantiles = c(0,20,40,60,80,100) 
    B = 100
    estimand = "rd"
    boot_ate = F
    #----------------------------------------------------------------------------------------------------------------------------------
    
    # if EMs are unnamed, assign names
    if (sum(is.na(colnames(ems))) != 0){
        colnames(ems)[which(is.na(colnames(ems)))] <- paste0("em", 1:sum(is.na(colnames(ems))))
    } 
    
    ems <- as_data_frame(ems)
    
    # create dummy indicators for multilevel categorical variables
    mlems <- names(which(apply(ems, 2, function(x) length(unique(na.omit(x)))) > 2))
    for (em in mlems) {
        mlem_form <- as.formula(paste0("~ -1 +", em))
        ems <- ems %>%
            select(-all_of(em)) %>%
            bind_cols(as_data_frame(model.matrix(mlem_form, data=ems)))
    }
    
    # create data frame of EM indicators for all 1 and 2-way interactions
    ems <- ems %>%
        mutate_all(as.numeric)
    
    counter_ems <- ems %>%
        mutate_all(~ case_when(.x == 1 ~ 0,
                               .x == 0 ~ 1))
    names(counter_ems) <- paste0(names(counter_ems), "_no")
    
    full_ems <- ems %>%
        rename_with(~ paste0(.x, "_yes")) %>%
        cbind(., counter_ems)
    
    subgroups <- as_data_frame(model.matrix(~ .^2 - 1, data = full_ems))
    
    remove <- c(paste0(names(ems), "_no"), paste0(names(ems), "_yes"), 
                paste0(names(ems), "_yes:", names(ems), "_no"),
                paste0(names(ems), "_no:", names(ems), "_yes"))
    
    subgroups <- subgroups %>%
        dplyr::select(-any_of(remove)) %>%
        cbind(ems, .)
    
    # check positivity: print proportions of observations in each subgroup (and size)
    #   throw warning if less than 5 observations in subgroup or not in subgroup
    # cat("Checking subgroup positivity...\n")
    tab1 <- tibble(Subgroup = names(subgroups),
                   "Proportion in Data" = round(apply(subgroups, 2, mean, na.rm=T)*100, 2),
                   "In-Subgroup Count" = apply(subgroups, 2, sum, na.rm=T),
                   "Out-of-Subgroup Count" = apply(subgroups, 2, function (x) sum(x == 0, na.rm = T)))
    
    colnames(tab1)[2] <- paste0("Proportion in Data (N=", nrow(subgroups), ")")
    
    #--------------------------------Implement throw as popup later--------------------------------------------------------------------
    # if (any(tab1$`In-Subgroup Count`<=5) | any(tab1$`Out-of-Subgroup Count`<=5)) {
    #     prob_sg <- tab1 %>%
    #         filter(`In-Subgroup Count`<=5 | `Out-of-Subgroup Count`<=5) %>%
    #         pull(Subgroup)
    #     warning("The following subgroups have less than 5 in-subgroup or out-of-subgroup observations. Bootstrap results may be biased:")
    #     print(prob_sg)
    # }
    #----------------------------------------------------------------------------------------------------------------------------------
    
    # instantiate bootstrap storage structure
    ## hets = CATE - ATE, H_0: hets == 0, H_a: hets =/= 0
    ## hets_bate = CATE - bootstrapped ATE
    boots <- tibble(quantile = numeric(),
                    subgroup = character(),
                    cate = numeric(),
                    hets = numeric(),
                    hets_bate = numeric())
    
    if (boot_ate == F) {
        boots <- dplyr::select(boots, -hets_bate)
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
    # # initialize counters and time storage vectors
    # init <- numeric()
    # end <- numeric()
    # sg_counter <- 0
    # b_counter <- 0
    # n_subgroups <- ncol(subgroups)
    # n_quantiles <- length(quantiles)
    # n_iter <- n_subgroups*n_quantiles*B
    
    # conditional bootstrap
    # cat("\n\nPerforming conditional boostrap:\n")
    #--------------------------------------------------------------------------------------------------------------------------
    
    for (subgroup in names(subgroups)) {
        
        # # counter for subgroup
        # sg_counter <- sg_counter + 1
        # 
        # # set quantile counter 
        # q_counter <- 0 
        
        # create matrix of confounders and effect modifiers that don't belong to subgroup
        curr_sg_em <- str_split(subgroup,":")[[1]]
        ind_rep_em <- which(c(paste0(names(ems), "_yes"), paste0(names(ems), "_no")) %in% curr_sg_em) %% ncol(ems)
        ind_rep_em <- ifelse(ind_rep_em == 0, ind_rep_em + ncol(ems), ind_rep_em)
        
        adj_X <- X %>%
            dplyr::select(-any_of(names(ems)[ind_rep_em]))
        
        for (q in quantiles) {
            
            # # counter for quantile
            # q_counter <- q_counter + 1
            
            for (b in 1:B) {
                
                # # counter for bootstrap
                # b_counter <- b_counter + 1
                # 
                # # get time before bootstrap
                # init <- c(init, Sys.time())
                
                # bootstrap CATE data according to quantiles
                insg <- which(get(subgroup, subgroups) == 1)
                outsg <- which(get(subgroup, subgroups) == 0)
                ind_insg <- sample(insg, size=round(nrow(ems)*q/100), replace=T)
                ind_outsg <- sample(outsg, size=round(nrow(ems)*(100-q)/100), replace=T)
                cate_ind <- c(ind_insg, ind_outsg)
                
                # if bootstrapping ATE, create second bootstrap sample within each iteration from full data to estimate ATE
                if (boot_ate == T) {
                    
                    bate_ind <- sample(1:length(y), replace=T)
                    
                    if (estimand == "or") {
                        bate_mod <- glm(y ~ z + as.matrix(X), family = binomial, subset = bate_ind)
                        bate <- exp(bate_mod$coefficients["z"])
                    } else if (estimand == "rd") {
                        bate_mod <- lm(y ~ z + as.matrix(X), subset = bate_ind)
                        bate <- bate_mod$coefficients["z"]
                    } else {}
                }
                
                # create outcome model based on desired estimand and add to bootstrap storage structure
                if (estimand == "or") {
                    mod <- glm(y ~ z + as.matrix(adj_X), family = binomial, subset = cate_ind)
                    if (boot_ate == T) {
                        boots <- boots %>% add_row(quantile = q,
                                                   subgroup = subgroup,
                                                   cate = exp(mod$coefficients["z"]),
                                                   hets = exp(mod$coefficients["z"])/ate,
                                                   hets_bate = exp(mod$coefficients["z"])/bate)
                    } else {
                        boots <- boots %>% add_row(quantile = q,
                                                   subgroup = subgroup,
                                                   cate = exp(mod$coefficients["z"]),
                                                   hets = exp(mod$coefficients["z"])/ate)
                    }
                    
                } else if (estimand == "rd") {
                    mod <- lm(y ~ z + as.matrix(adj_X), subset = cate_ind)
                    if (boot_ate == T) {
                        boots <- boots %>% add_row(quantile = q,
                                                   subgroup = subgroup,
                                                   cate = mod$coefficients["z"],
                                                   hets = mod$coefficients["z"] - ate,
                                                   hets_bate = mod$coefficients["z"] - bate)
                    } else {
                        boots <- boots %>% add_row(quantile = q,
                                                   subgroup = subgroup,
                                                   cate = mod$coefficients["z"],
                                                   hets = mod$coefficients["z"] - ate)
                    }
                } else {}
                
                # # get time after bootstrap
                # end <- c(end, Sys.time())
                # 
                # # estimated remaining time (average time it took to run other iterations)
                # time <- round(seconds_to_period(sum(end - init)), 0)
                # est <- n_iter*(mean(end[end != 0] - init[init != 0])) - time
                # remaining <- round(seconds_to_period(est), 0)
                # 
                # percent <- b_counter/n_iter * 100
                # 
                # # print progress
                # cat(paste0(sprintf('\r[%-50s] %d%%',
                #                    paste(rep('=', percent / 2), collapse = ''),
                #                    floor(percent)),
                #            " | Execution time:", time,
                #            " | Estimated time remaining:", remaining,
                #            " | Subgroup:", sg_counter, "/", n_subgroups,
                #            " | Quantile:", q_counter, "/", n_quantiles,
                #            " | Bootstrap: ", b, "/", B, "     "))
            }
        }
    }
    
    # cat("\n\n Conditional bootstrap complete.")
    
    # calculate means/medians, bootstrap confidence intervals
    if (boot_ate == F) {
        statistics <- boots %>%
            group_by(quantile, subgroup) %>%
            summarise(mean_cate = mean(cate),
                      median_cate = median(cate),
                      cate_ci_lwr = quantile(cate, probs = 0.025),
                      cate_ci_upr = quantile(cate, probs = 0.975),
                      mean_hets = mean(hets),
                      median_hets = median(hets),
                      hets_ci_lwr = quantile(hets, probs = 0.025),
                      hets_ci_upr = quantile(hets, probs = 0.975)) %>%
            mutate(cate_ci_rejectT = case_when(cate_ci_lwr > ate_confint[2] ~ T,
                                               cate_ci_upr < ate_confint[1] ~ T,
                                               T ~ F),
                   hets_ci_rejectT = case_when(estimand == "or" & hets_ci_lwr < 1 & hets_ci_upr < 1 ~ T,
                                               estimand == "or" & hets_ci_lwr > 1 & hets_ci_upr > 1 ~ T,
                                               estimand == "rd" & hets_ci_lwr < 0 & hets_ci_upr < 0 ~ T,
                                               estimand == "rd" & hets_ci_lwr > 0 & hets_ci_upr > 0 ~ T,
                                               T ~ F))
    } else {
        statistics <- boots %>%
            group_by(quantile, subgroup) %>%
            summarise(mean_cate = mean(cate),
                      median_cate = median(cate),
                      cate_ci_lwr = quantile(cate, probs = 0.025),
                      cate_ci_upr = quantile(cate, probs = 0.975),
                      mean_hets = mean(hets),
                      median_hets = median(hets),
                      hets_ci_lwr = quantile(hets, probs = 0.025),
                      hets_ci_upr = quantile(hets, probs = 0.975),
                      mean_hets_bate = mean(hets_bate),
                      median_hets_bate = median(hets_bate),
                      hets_bate_ci_lwr = quantile(hets_bate, probs = 0.025),
                      hets_bate_ci_upr = quantile(hets_bate, probs = 0.975)) %>%
            mutate(cate_ci_rejectT = case_when(cate_ci_lwr > ate_confint[2] ~ T,
                                               cate_ci_upr < ate_confint[1] ~ T,
                                               T ~ F),
                   hets_ci_rejectT = case_when(estimand == "or" & hets_ci_lwr < 1 & hets_ci_upr < 1 ~ T,
                                               estimand == "or" & hets_ci_lwr > 1 & hets_ci_upr > 1 ~ T,
                                               estimand == "rd" & hets_ci_lwr < 0 & hets_ci_upr < 0 ~ T,
                                               estimand == "rd" & hets_ci_lwr > 0 & hets_ci_upr > 0 ~ T,
                                               T ~ F),
                   hets_bate_ci_rejectT = case_when(estimand == "or" & hets_bate_ci_lwr < 1 & hets_bate_ci_upr < 1 ~ T,
                                                    estimand == "or" & hets_bate_ci_lwr > 1 & hets_bate_ci_upr > 1 ~ T,
                                                    estimand == "rd" & hets_bate_ci_lwr < 0 & hets_bate_ci_upr < 0 ~ T,
                                                    estimand == "rd" & hets_bate_ci_lwr > 0 & hets_bate_ci_upr > 0 ~ T,
                                                    T ~ F))
    }
    
    # only display subgroups with CATEs that have at least one non-overlapping CI (across quantiles) with ATE 
    sg_rejectT <- statistics %>%
        group_by(subgroup) %>%
        reframe(any_reject = any(cate_ci_rejectT)) %>%
        filter(any_reject == T) %>%
        pull(subgroup)
    
    plot_statistics <- statistics %>%
        filter(subgroup %in% sg_rejectT) %>%
        mutate(color = ifelse(cate_ci_rejectT == F, ".", subgroup),
               alpha = ifelse(color == ".", 0.2, 1.0))
    
    sg_colors <- tibble(subgroup = unique(statistics$subgroup),
                        color = unname(glasbey(length(unique(statistics$subgroup)))))
    
    cate_edges <- plot_statistics %>%
        group_by(subgroup) %>%
        arrange(quantile) %>%
        reframe(x1 = lag(quantile),
                x2 = quantile,
                y1 = lag(median_cate),
                y2 = median_cate,
                color = case_when(lag(cate_ci_rejectT) + cate_ci_rejectT == 0 ~ ".",
                                  T ~ subgroup),
                alpha = case_when(lag(cate_ci_rejectT) + cate_ci_rejectT == 2 ~ 1.0,
                                  lag(cate_ci_rejectT) + cate_ci_rejectT == 1 ~ 0.6,
                                  T ~ 0.05),
                linetype = case_when(lag(cate_ci_rejectT) + cate_ci_rejectT == 2 ~ "1",
                                     lag(cate_ci_rejectT) + cate_ci_rejectT == 1 ~ "2",
                                     T ~ "3")) %>%
        filter(!is.na(x1))
    
    # plot distributions
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
             color = "Subgroup") 
    
    if (estimand == "or") {
        plot <- plot +
            ylab("Median CATE (odds ratio)")
    } else if (estimand == "rd") {
        plot <- plot +
            ylab("Median CATE (risk difference)")
    } else {}
    
    selected_lines <- reactiveVal(sg_rejectT)
    
    output$checkboxes <- renderUI({
        checkboxGroupInput("lines", label = "Subgroup",
                           choices = setNames(sg_rejectT, sg_rejectT),
                           selected = selected_lines())
    })
    
    observeEvent(input$lines, {
        selected_lines(input$lines)
        output$Plot <- renderPlot({
            
            # Filter data based on selected lines
            plot_statistics2 <- plot_statistics %>%
                filter(subgroup %in% input$lines)
            cate_edges2 <- cate_edges %>%
                filter(subgroup %in% input$lines)
            
            # Plot
            plot + geom_point(data=plot_statistics2,
                              aes(x=quantile, y=median_cate, group=subgroup,
                                  color=color, alpha=alpha)) +
                scale_color_manual(breaks = sg_colors$subgroup,
                                   values = sg_colors$color) +
                geom_segment(data=cate_edges2,
                             aes(x=x1, xend=x2, y=y1, yend=y2,
                                 color=color, alpha=alpha,
                                 linetype=linetype)) +
                scale_alpha(guide = 'none') +
                scale_linetype_manual(values = c("solid","dashed","dotted"),
                                      labels = c("Significant","Between Significance Threshold","Insignificant"))
        })
    })

    tab2 <- statistics %>%
        filter(cate_ci_rejectT == T) %>%
        select(quantile, subgroup,  median_cate, mean_cate, cate_ci_lwr, cate_ci_upr) %>%
        arrange(desc(abs(median_cate))) %>%
        mutate(across(c(median_cate, mean_cate, cate_ci_lwr, cate_ci_upr), \(x) round(x, 3))) %>%
        dplyr::rename("Quantile" = quantile,
                      "Subgroup" = subgroup,
                      "Median CATE" = median_cate,
                      "Mean CATE" = mean_cate,
                      "Lower 95% CI" = cate_ci_lwr,
                      "Upper 95% CI" = cate_ci_upr)
    
    output$tab1 <- DT::renderDataTable(DT::datatable(tab1, rownames = F))
    output$tab2 <- DT::renderDataTable(DT::datatable(tab2, rownames = F))
    
    #--------------------------------------------------------Implement CATE CIs and density options later----------------------------
    # # add colors to bootstrap color structure
    # cboots <- boots %>%
    #     mutate(color = ifelse(subgroup %in% sg_rejectT, subgroup, "."))
    # 
    # # choose subgroups with most number of 'significant' median cates across quantiles to display densities or cis
    # sg_show <- statistics %>%
    #     group_by(subgroup) %>%
    #     reframe(significant_cates = sum(cate_ci_rejectT),
    #             largest_cate = max(abs(median_cate))) %>%
    #     arrange(desc(significant_cates), desc(largest_cate)) %>%
    #     dplyr::slice(1:2) %>%
    #     pull(subgroup)
    # 
    # # subgroup confidence intervals by quantile
    # if (show_bootci == "one") {
    #     
    #     # show confidence intervals for one subgroup
    #     plot <- plot +
    #         geom_errorbar(data=filter(plot_statistics, subgroup==sg_show[1]),
    #                       aes(x=quantile,
    #                           ymin=cate_ci_lwr, 
    #                           ymax=cate_ci_upr,
    #                           group=subgroup,
    #                           color=subgroup),
    #                       alpha=0.6,
    #                       width=30/length(quantiles))
    #     
    # } else if (show_bootci == "all") {
    #     
    #     # show confidence intervals for all subgroups
    #     plot <- plot +
    #         geom_errorbar(data=plot_statistics,
    #                       aes(x=quantile,
    #                           ymin=cate_ci_lwr, 
    #                           ymax=cate_ci_upr,
    #                           group=subgroup,
    #                           color=subgroup),
    #                       alpha=0.6,
    #                       width=30/length(quantiles))
    #     
    # } else if (show_bootci == "none") {
    # } else {}
    # 
    # # subgroup densities by quantile
    # sgd <- cboots %>%
    #     mutate(sgq = paste(subgroup, quantile, sep='.')) %>%
    #     group_by(sgq, color) %>%
    #     reframe(x = density(cate)$x,
    #             y = density(cate)$y/(5*length(quantiles)) + unique(quantile))
    # 
    # if (show_bootd == "one") {
    #     
    #     one_sgd <- sgd %>%
    #         filter(startsWith(sgq, sg_show[2]))
    #     
    #     # show one subgroup bootstrap estimates and densities at each quantile
    #     plot <- plot + 
    #         geom_point(data=filter(cboots, subgroup == sg_show[2]), 
    #                    aes(x=quantile, y=cate, color=color),
    #                    alpha=0.03) + 
    #         geom_path(data=one_sgd, 
    #                   aes(x=y, y=x, group=sgq, color=color), 
    #                   alpha=0.4)
    #     
    # } else if (show_bootd == "all") {
    #     
    #     # show all subgroup bootstrap densities at each quantile
    #     plot <- plot + 
    #         geom_point(data=cboots, 
    #                    aes(x=quantile, y=cate, color=color),
    #                    alpha=0.03) + 
    #         geom_path(data=sgd, 
    #                   aes(x=y, y=x, group=sgq, color=color), 
    #                   alpha=0.4)
    #     
    # } else if (show_bootd == "none") {
    # } else {}
    #--------------------------------------------------------------------------------------------------------------------------------------------
    
}
