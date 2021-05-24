# This is a main script that sets up the global settings - load packages and functions
trypackage<-function(name){
  if(!require(name,character.only = TRUE)){
    install.packages(name,character.only = TRUE)
  }
  library(name,character.only = TRUE)
}

packlist<-c("shiny","mclust","DT","plotly","tidyr","plyr","dplyr","GGally","grid","purrr","ggalt","markdown","shinyBS")
for(pack in packlist){ trypackage(pack) }

source("R/plotFunctions.R",local=TRUE)
source("R/tableFunctions.R",local=TRUE)