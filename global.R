# load libraries
library(shiny)
library(DT)
library(tidyverse)
library(WeightIt) # ipw package
library(sandwich) # robust standard errors for ATE CI
library(lmtest)
library(boot) # bootstrap DR ATE CI
library(plotly)
library(bslib) # use bootstrap themes
library(shinyWidgets)
library(shinyFeedback)
library(thematic)
library(RColorBrewer)
library(promises)
library(future)
library(ipc)
plan(multisession)
# library(foreach) # parallel computation 
# library(doParallel) # detect cores
# library(doSNOW) # parallel computation

# source modules
source("mod-about.R")
source("mod-data.R")
source("mod-bootstrap.R")
source("mod-results.R")
source("helper-functions.R")

# options
options(shiny.maxRequestSize = 30*1024^2) # allow for larger files to be uploaded
options(shiny.reactlog = TRUE) # view dependency tree
options(future.globals.maxSize = 850*1024^2) # future env needs larger memory
options(shiny.fullstacktrace = TRUE)