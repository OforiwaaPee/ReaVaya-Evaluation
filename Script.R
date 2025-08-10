# ---- 0) Setup ----
pkgs <- c(
  "tidyverse","data.table","readxl","janitor",
  "sf","lwgeom","units",
  "fixest","modelsummary",
  "did",           # for Callaway & Sant'Anna (alt.)
  "survey","srvyr"
)
invisible(lapply(setdiff(pkgs, rownames(installed.packages())), install.packages))
invisible(lapply(pkgs, library, character.only=TRUE))

options(scipen=999, dplyr.width=Inf)
set.seed(42)
