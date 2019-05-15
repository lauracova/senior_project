library(tictoc)
tic();nameslist2<-read.csv("data/nameslist.csv");toc();
tic();laura2<-read.csv("data/laura.csv");toc();

library(tidyverse)
laura <- nameslist %>% filter(Name == "Laura")
write.csv(laura, "data/laura.csv", row.names=FALSE)


file_list <- list.files("names/")
#full list of everystate
my_list <-do.call("rbind",map(file_list,function(x) 
  read_table(here::here(paste0("Case_Study_12/analysis/names/",x)), col_names = FALSE) %>% 
    separate("X1", into=c("State", "Gender", "Year", "Name", "Count"), remove=TRUE)
))

#split data set up by name save as csv

splitnames <- split(nameslist, as.factor(nameslist$Names))
