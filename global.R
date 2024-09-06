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
library(foreach) # parallel computation 
# library(doParallel) # detect cores; parallel computation
# library(doSNOW) # parallel computation
library(doFuture) # parallel computation
library(progressr) # progress bar
library(data.table)
library(parallel)
library(future.apply)
plan(multisession, workers = 2) # try multicore and specify number of workers (does this not matter once we hit plan(cluster) in the bootstrap?)
registerDoFuture()
handlers(global = TRUE)
handlers("cli")

# source modules
source("mod-about.R")
source("mod-data.R")
source("mod-bootstrap.R")
source("mod-results.R")
source("helper-functions.R")

# options
options(shiny.maxRequestSize = 30*1024^2) # allow for larger files to be uploaded
options(shiny.reactlog = TRUE) # view dependency tree
options(future.globals.maxSize = 8000*1024^2) # future env needs larger memory
options(shiny.fullstacktrace = TRUE) # debug
options(future.debug = FALSE) # debug future
options(progressr.enable = TRUE) # enable interactive session