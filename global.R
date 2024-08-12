# load libraries
library(shiny)
library(DT)
library(tidyverse)
library(WeightIt) # ipw package
library(sandwich) # robust standard errors for ATE CI
library(lmtest)
library(boot) # bootstrap DR ATE CI
library(plotly)
library(shinythemes)
library(bslib) # use bootstrap themes
library(shinyWidgets)
library(shinyFeedback)
library(r2r) # instead of hashmaps, let's use store
library(pals) # color palette for plot
library(thematic)

# source modules
source("mod-about.R")
source("mod-data.R")
source("mod-bootstrap.R")
source("mod-results.R")
source("helper-functions.R")

# allow for larger files to be uploaded
options(shiny.maxRequestSize=30*1024^2)

# view dependency tree
options(shiny.reactlog = TRUE)