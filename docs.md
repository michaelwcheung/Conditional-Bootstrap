Documentation & Notes for the THRIVE Shiny App (In Progress)

# How to use the app
1. Open the `app/global.R` file and click 'Run App', or run `runApp()`.

## 'Upload Data' tab
1. In the 'Upload Data' tab, upload a csv file of your data or choose 'Load sample data'.
    * Expected output: a data table of your uploaded data; a 'Next: Variable Selection' button.
    * Purpose: double check that the uploaded dataset is correct.
2. Click 'Next: Variable Selection'.
    * Expected output: four selection boxes with dropdown menus for selecting the outcome variable, treatment variable, covariate(s), and candidate variable(s). A slider for the dimension of cross-classifications to consider. 
    * Purpose: restrict user access. 
3. Select the variables for outcome, treatment, covariate(s), and candidate variable(s). 
    * Expected output: success feedback (green) for outcome variable and treatment variable selections; a 'Next: Covariate Data Type Verification' button.
    * Purpose: define causal model for analysis.
    * Note: for the sake of computation and plotting, please only select a few candidate effect modifier(s), e.g. $\leq$ 4, at the moment.
4. Click 'Next: Covariate Data Type Verification'.
    * Expected output: a data table of the role, data type, and percent NA for each variable in your dataset; a 'Next: Confirm Potential Effect Modifier Prevalence' button.
    * Purpose: double check that each variable is coded correctly by role, data type, and missingness. 
    * Note: continuous variables may be classified as categorical variables in 'Data Type' and vice versa. We plan to make this column adjustable in the future.
5. Click 'Next: Confirm Potential Effect Modifier Prevalence'.
    * Expected output: a data table of the prevalence of each effect modifier in the dataset; a 'Next: Set Hyper Parameters' button.
    * Purpose: double check positivity and that there are sufficient (at least 5) instances of each effect modifier for an unbiased bootstrap. 
6. Click 'Next: Set Hyper Parameters' button.
    * Expected output: tab switches to the 'Set Hyper Parameters' tab.
    * Purpose: notify user that 'Upload Data' is complete; easy access to switch tabs (avoid scrolling to the top). 
  
## 'Set Hyper Parameters' tab
1. Select hyper parameters, which include the quantiles, type of estimand, estimation method, and number of resamples. Click 'Start Resampling!' button.
    * Expected output: a progress bar with some information including time to completion; a 'Next: See Results' button upon completion
    * Purpose: perform the resample and provide user with some expectation of runtime
2. Click 'Next: See Results' button.
    * Expected output: tab switches to the 'Results' tab.
    * Purpose: notify user that resampling is complete; easy access to switch tabs (avoid scrolling to the top).
  
## 'Results' tab
1. If no significant effect modifiers are identified, the only things on this page should be a plot of the ATE and its 95% CI across the quantiles, and a message regarding the lack of significant effect modifiers. 
2. Otherwise, one should see the following three items when there are identified effect modifiers:
    * 'Resampled CATE Estimates by Quantile plot': an interactive plotly plot of the effect modifiers, their median CATEs and the ATE for comparison. Select/deselect legend items to edit the plot and hover over points to see the value of the corresponding CATE.
    * 'Resampled CATEs by Quantile and Subgroup' table: a table of each quantile and effect modifier combination present on the plot, their median/mean CATEs and 95% CI.
    * 'Linear Regression Coefficients of CATEs on Quantile by Effect Modifier' table: a table of each effect modifier and their linear regression coefficient estimate (include exact linear model formula). Hover over each row to see an interpretation of the coefficient. 
3. All results can be downloaded for your convenience. 


# Global Store Dictionary
- **store$data**: data frame of uploaded dataset
- **store$y_outcome**: text input of column name from 'Select outcome variable:'
- **store$z_treatment**: text input of column name from 'Select treatment variable:'
- **store$X_covariates**: list of text inputs of column name(s) from 'Select covariates:'
- **store$P_candidates**: list of text inputs of column name(s) from 'Select candidate effect modifiers:'
- **store$variable_verification**: tibble of each variable in the dataset, their role, data type, and percent null 
- **store$y**: vector of the outcome variable
- **store$z**: vector of the treatment variable
- **store$X**: matrix of the covariates
- **store$P**: matrix of the candidate effect modifiers
- **store$emprevalence_table**: tibble of effect modifier prevalence
- **store$ems**: data frame of all effect modifiers (including cross-classifications)
- **store$quantiles**: vector of the radio button selectino of quantiles from 'Select quantile:'
- **store$B**: number of bootstrap samples from 'Number of bootstrap samples:'
- **store$estimand**: corresponding shorthand of the radio button selection of estimand type from 'Select the type of estimand:'
- **store$estimation_method**: corresponding shorthand of the radio button selection of estimation method from 'Select the estimation method for causal effect:'
- **store$test**: "BH" (or "PQ" or "CI" or "Q")
- **store$boots**: tibble of resamples, where each row corresponds to one resample (the quantile, the effect modifier, CATE, and the CATE's standard error)
- **store$bh_cates**: tibble of 
- **store$statistics**: tibble of median, mean CATEs, standard error, 95% CIs of each quantile and effect modifier combination
- **store$em_rejectT**: list of significant effect modifiers 
- **store$plot_statistics**: tibble of significant effect modifiers filtered from store\$statistics 
- **store$em_colors**: tibble of unique colors for each effect modifier
- **store$cate_edges**: tibble of data for line segments on plot (x1, x2, y1, y2, color, alpha, linetype)
- **store$em_quantile_table**: tibble of significant effect modifiers sorted by their median CATEs filtered from store\$statistics
- **store$regression_coefficients_table**: tibble of significant effect modifiers sorted by their linear regression coefficient estimates from the model (include exact model formula) when filtered from store\$statistics
- **store$plot**: ggplot of resampled CATE estimates by quantile


# Notes, Unresolved Bugs
- Please select only a few candidate effect modifiers at the moment (computation-wise, color-options-on-the-plot-wise)
- Please avoid clicking buttons multiple times as this adds runs. 
- Please ensure that the custom quantile includes 0 and 100, and each number is separated by a comma. 
- The plots downloaded from plotly's interactive plot leave legend items cutoff.
