# THRIVE
 
## Usage

The Transparent High-Dimensional Reproducible Inference through Validation and Exploration (THRIVE) method can be run via Shiny app or as a function call in R. Please choose the option that is best suited for your needs.

- Shiny app: ideal for demonstration purposes, includes a toy dataset
  - Download the latest repository and run the bash script (R must be installed):
  ```
  git clone https://github.com/michaelwcheung/Conditional-Bootstrap.git
  cd Conditional-Bootstrap
  sh run.sh
  ```
- function call, `thrive()`: ideal for analysis on full dataset, includes parallelization options for faster computation (located in `THRIVE.R`)