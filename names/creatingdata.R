library(tictoc)
tic();nameslist2<-read.csv("data/nameslist.csv");toc();
tic();laura2<-read.csv("data/laura.csv");toc();

library(tidyverse)
laura <- nameslist %>% filter(Name == "Laura")
write.csv(laura, "data/laura.csv", row.names=FALSE)


file_list <- list.files("data/names/")
#full list of everystate
my_list <-do.call("rbind",map(file_list,function(x) 
  read_table(here::here(paste0("data/names/",x)), col_names = FALSE) %>% 
    separate("X1", into=c("State", "Gender", "Year", "Name", "Count"), remove=TRUE)
))
write.csv(my_list, "data/nameslist.csv", row.names=FALSE)

#split data set up by name save as csv
library(stringr)

nameslist <-read.csv("data/nameslist.csv")
path <- 'data/splitnames'
if(str_detect(name,"A") == "A"){
  path <- 'data/splitnames/a/'
}

for (name in levels(nameslist$Name)){

  tmp=subset(nameslist,Name==name)
  fn=paste('data/splitnames/',name,sep='','.csv')
  write.csv(tmp,fn,row.names=FALSE)

  }
  
Alabama <-read.csv("data/splitnames/Alabama.csv")

tic();Laura <-read.csv("data/splitnames/Laura.csv");toc();

tic();Garrett <-read.csv("data/splitnames/Garrett.csv");toc();

tic();Laura <-read_csv("data/splitnames/Laura.csv");toc();
tic();Laura <-read.csv("data/splitnames/Laura.csv");toc();


user <- "Anna"

paste('data/splitnames/',name,sep='','.csv')

name <- read.csv(paste('data/splitnames/',user,sep='','.csv'))





celebrity <- read_csv("data/celebrity_deaths_4.csv") %>% filter(nationality=="American") %>% 
  separate(name, into=c("Name"), sep=" ", remove=FALSE) %>% 
  mutate(info = paste(name," ","(",birth_year,"-", death_year,")",sep='')) %>% 
  select(Name, info) %>% 
  group_by(Name) %>% 
  mutate(info = paste0(info, collapse = ",")) %>% slice(1)

################ maybe i should join by first and put actors with same name info in string

write.csv(celebrity, "data/celebrity.csv", row.names=FALSE)

celebrity <- read_csv("data/celebrity.csv")
  







