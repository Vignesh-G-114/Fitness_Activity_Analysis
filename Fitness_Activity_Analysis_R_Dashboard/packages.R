required <- c("shiny","bslib","dplyr","ggplot2","DT","scales","readr","plotly","bsicons","lubridate")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) install.packages(missing)
