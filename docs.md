Documentation/Notes for Conditional Bootstrap Shiny App (In Progress)

# How to use the app
1. Open the `global.R` file and click 'Run App', or run `runApp()`.

## 'Upload Data' tab
1. In the 'Upload Data' tab, upload a csv file of your data.
  1. Expected output: a data table of your uploaded data; a 'Next: Variable Selection' button.
  2. Purpose: double check that the uploaded dataset is correct.
2. Click 'Next: Variable Selection'.
  1. Expected output: four selection boxes with dropdown menus for selecting the outcome variable, treatment variable, covariate(s), and candidate effect modifier(s).
  2. Purpose: restrict user access. 
3. Select the variables for outcome, treatment, covariate(s), and candidate effect modifier(s). 
  1. Expected output: success feedback (green) for outcome variable and treatment variable selections; a 'Next: Covariate Data Type Verification' button.
  2. Purpose: define causal model for analysis.
  3. Note: for the sake of computation and plotting, please only select a few candidate effect modifier(s), e.g. \leq 4, at the moment.
4. Click 'Next: Covariate Data Type Verification'.
  1. Expected output: a data table of the role, data type, and percent NA for each variable in your dataset; a 'Next: Confirm Effect Modifier Prevalence' button.
  2. Purpose: double check that each variable is coded correctly by role, data type, and missingness. 
  3. Note: continuous variables may be classified as categorical variables in 'Data Type' and vice versa. We plan to make this column adjustable in the future.
5. Click 'Next: Confirm Effect Modifier Prevalence'.
  1. Expected output: a data table of the prevalence of each effect modifier in the dataset; a 'Next: Set Bootstrap Parameters' button.
  2. Purpose: double check positivity and that there are sufficient (at least 5) instances of each effect modifier for an unbiased bootstrap. 
6. Click 'Next: Set Bootstrap Parameters' button.
  1. Expected output: tab switches to the 'Set Bootstrap Parameters' tab.
  2. Purpose: notify user that 'Upload Data' is complete; easy access to switch tabs (avoid scrolling to the top). 
  
## 'Set Bootstrap Parameters' tab
1. Select bootstrap parameters, which include the quantiles, type of estimand, estimation method, and number of bootstrap samples. Click 'Start Bootstrap!' button.
  1. Expected output: a progress bar with some information; a 'Next: See Results' button upon completion
  2. Purpose: perform the bootstrap and provide user with some expectation of runtime
2. Click 'Next: See Results' button.
  1. Expected output: tab switches to the 'Bootstrap Results' tab.
  2. Purpose: notify user that bootstrap is complete; easy access to switch tabs (avoid scrolling to the top).
  
## 'Bootstrap Results' tab
1. If no significant effect modifiers are observed, the only things on this page should be a plot of the ATE and its 95% CI across the quantiles, and a message regarding the lack of significant efect modifiers. 
2. Otherwise, one should see the following three items when there are significant effect modifiers:
  - 'Bootstrapped CATE Estimates by Quantile plot': an interactive plotly plot of the effect modifiers, their median CATEs and the ATE for comparison. Select/deselect legend items to edit the plot and hover over points to see the value of the corresponding CATE.
  - 'Bootstrapped CATEs by Quantile and Subgroup' table: a table of each quantile and effect modifier combination present on the plot, their median/mean CATEs and 95% CI.
  - 'Linear Regression Coefficients of CATEs on Quantile by Effect Modifier' table: a table of each effect modifier and their linear regression coefficient estimate (include exact linear model formula). Hover over each row to see an interpretation of the coefficient. 
3. All results are downloadable for your convenience. 


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
- **store$ems**: data frame of all effect modifiers (including interactions)
- **store$quantiles**: vector of the radio button selectino of quantiles from 'Select quantile:'
- **store$B**: number of bootstrap samples from 'Number of bootstrap samples:'
- **store$estimand**: corresponding shorthand of the radio button selection of estimand type from 'Select the type of estimand:'
- **store$estimation_method**: corresponding shorthand of the radio button selection of estimation method from 'Select the estimation method for causal effect:'
- **store$test**: "BH" (or "PQ" or "CI" or "Q")
- **store$boots**: tibble of bootstraps, where each row corresponds to one bootstrap sample (the quantile, the effect modifier, CATE, and the CATE's standard error)
- **store$bh_cates**: tibble of 
- **store$statistics**: tibble of median, mean CATEs, standard error, 95% CIs of each quantile and effect modifier combination
- **store$em_rejectT**: list of significant effect modifiers 
- **store$plot_statistics**: tibble of significant effect modifiers filtered from store\$statistics 
- **store$em_colors**: tibble of unique colors for each effect modifier
- **store$cate_edges**: tibble of data for line segments on plot (x1, x2, y1, y2, color, alpha, linetype)
- **store$em_quantile_table**: tibble of significant effect modifiers sorted by their median CATEs filtered from store\$statistics
- **store$regression_coefficients_table**: tibble of significant effect modifiers sorted by their linear regression coefficient estimates from the model (include exact model formula) when filtered from store\$statistics
- **store$plot**: ggplot of bootstrapped CATE estimates by quantile


# Example
- Dataset: data_droughts_malnutrition_.csv (345,499 rows x 12 columns)
- Note: due to the size of the dataset, some buttons have delayed outputs, max 5 seconds; please be patient!
- Modification: One can uncomment line 59 in the `mod-data.R` file to use a sampled dataset of size 10,000 to reduce runtime for testing purposes. This may affect effect modifier prevalence and the resulting bootstraps.

## Parameters
- outcome: stunted (binary)
- treatement: drought (binary)
- covariates: all (binary)
- candidate effect modifiers: education_none, mass_media, rural_residence (binary; total of 15 with interactions)
- quantile: median: 0, 50, 100
- estimand: risk difference
- estimation method: linear regression
- number of bootstraps (and their approximate runtimes): 10 (8 min), 50 (40 min :cry:), 100 (1 hour 17 min :sob:); by extrapolation, 1000 bootstraps would take 13 hours 20 min :skull: and therefore was not attempted. 
- plots, tables: see attached folder `results` for all 3 bootstrap results

# Notes, Unresolved Bugs
- Please select only a few candidate effect modifiers at the moment (computation-wise, color-options-on-the-plot-wise)
- Please avoid clicking buttons multiple times as this adds runs. 
- Please ensure that the custom quantile includes 0 and 100, and each number is separated by a comma. 
- To stop (terminate) the bootstrap runs, please terminate in console as this functionality has not been implemented within the app.
- For the estimation method 'IP weighting', propensity scores are calculated for each bootstrapped sample (adds a factor of n complexity). We may instead just use the propensity scores calculated on the entire dataset per our previous conversation (removes the factor of n complexity). 
- The progress bar does not seem to accurately reflect the percentage (of bootstraps completed).
- You may re-upload a different dataset and restart the process, but some of the UI may not appear/disappear as expected (e.g. leftover tables from previous dataset). The app will still function, but if you would like a fresh start I would recommend terminating the app and running it again at the moment.
- The plots downloaded from plotly's interactive plot leave legend items cutoff.
- Make the legend look prettier (currently using an annotation for linetype). 
- Please contact c6shi@ucsd.edu or use the form available at the top of the app for any clarifications, comments, questions, etc. Thank you! :smile:
