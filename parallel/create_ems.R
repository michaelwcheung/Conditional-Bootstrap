create_ems <- function(P) {
    # check that candidate effect modifiers are named
    if (sum(is.na(colnames(P))) != 0) {
    colnames(P)[which(is.na(colnames(P)))] <- paste0("em", 1:sum(is.na(colnames(P))))
    }

    # get levels of candidate effect modifiers
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

    # create dummy indicators for mlP and cP
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

    ems <- as.data.frame(model.matrix(~ .^2 - 1, data=full_P))

    remove <- c(paste0(names(P), "_0"), paste0(names(P), "_1"),
                paste0(names(P), "_1:", names(P), "_0"),
                paste0(names(P), "_0:", names(P), "_1"))

    if (length(mlP) != 0) {
    remove <- c(remove, names(full_P)[str_detect(names(full_P), paste(mlP, collapse="|"))])
    }

    ems <- ems %>%
    dplyr::select(-any_of(remove)) %>%
    cbind(P, .)

    # check positivity for each effect modifier interaction
    cat("Checking positivity...\n")
    tab1 <- tibble("Effect Modifier (Interaction)" = names(ems),
                "Proportion in Data" = paste0(round(apply(ems, 2, mean, na.rm=T)*100, 2), "%"),
                "In-Count" = apply(ems, 2, sum, na.rm=T),
                "Out-Count" = apply(ems, 2, function(x) sum(x==0, na.rm=T)))

    colnames(tab1)[2] <- paste0("Proportion in Data (N=", nrow(ems), ")")

    # throw warning if less than or equal to 5 observations in `In-Count` or `Out-Count`
    if (any(tab1$`In-Count`<=5) | any(tab1$`Out-Count`<=5)) {
    prob_em <- tab1 %>% 
        filter(`In-Count`<=5 | `Out-Count`<=5) %>%
        pull(`Effect Modifier (Interaction)`)
    warning("The following interactions have less than 5 in- or out-count observations. Bootstrap results may be biased:")
    print(prob_em)
    }
    return(list("P"=P, "ems"=ems))
}