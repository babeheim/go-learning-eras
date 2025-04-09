
library(rethinking)  # github.com/rmcelreath/rethinking
library(kaya)        # github.com/babeheim/kaya
library(tictoc)
library(digest)
library(XML)
library(tikzDevice)
library(yaml)
library(igraph)
library(viridis)
library(dplyr)
library(glue)
library(pander)
library(loo)
library(testthat)
library(longformer)
library(stringdist)
library(philentropy) # for JSD, Jensen-Shannon Divergence

stopifnot(capabilities("png"))
stopifnot(capabilities("cairo"))

machine_name <- Sys.info()[["nodename"]]
project_seed <- 2025

init_project <- FALSE     # wipes figures/, rds/, data/

set.seed(project_seed)
options(warnPartialMatchDollar=TRUE)

files <- list.files("R_functions", full.names = TRUE)
for (i in 1:length(files)) source(files[i])

# load color palettes

blues <- trimws(c(
  " #EBF4FB",
  " #CCDEF1",
  " #9BC4DF",
  " #4B90C4",
  " #2163A9",
  " #0C367D"
))

freckles <- trimws(c(
   " #FFAF56",
   " #FF6200",
   " #FF3800",
   " #D91F11",
   " #B2001F"
))

github <- trimws(c( 
  " #d6e6a5", 
  " #8cc665", 
  " #44a340", 
  " #1e6823"
))

rose <- trimws(c(
  " #FFA8A0", 
  " #FF7489", 
  " #FF5072", 
  " #FF254D", 
  " #F10033"
))

magenta_red <- trimws(c(
  " #D543D2",
  " #C83FC6",
  " #CD3BA0",
  " #D23777",
  " #D83431"
))

magenta <- trimws(c(
  " #f1c6d0",
  " #e9aabd",
  " #df90b0",
  " #cc6d9e",
  " #a94689",
  " #7a2c72"
))

teal_yellow_mauve <- trimws(c(
  " #409192",
  " #5eae87",
  " #a5c98c",
  " #e7e1a3",
  " #e5b881",
  " #db8876",
  " #c05f7e"
))
