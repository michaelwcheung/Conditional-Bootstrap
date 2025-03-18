# THRIVE

The Transparent High-Dimensional Reproducible Inference through Validation and Exploration (THRIVE) method is a model-agnostic semi-automated heterogeneous subgroup identification and estimation procedure that uses a resampling framework to generate pseudo-populations of specified distributions of potential effect modifiers to estimate their CATEs. 
 
## Usage

THRIVE can be run via Shiny app or as a function call in R. Please choose the option that is best suited for your needs.

* Shiny app: ideal for demonstration purposes, includes a toy dataset
    * Download the latest repository and run the bash script (R must be installed):
    ```
    git clone https://github.com/michaelwcheung/Conditional-Bootstrap.git
    cd Conditional-Bootstrap
    sh run.sh
    ```
* function call, `thrive()`: ideal for analysis on full dataset, includes parallelization options for faster computation (located in `THRIVE.R`)
    * See `test/case-study.Rmd` for an example

## Notes
* For developers, to help decipher the Shiny code, please see `docs.md` for a detailed description of variables. 
* For a more user-friendly walkthrough of the Shiny app, please see `test/walkthrough.pdf`. 