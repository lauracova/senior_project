########################
#spliting data by name
########################

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


# loop to split into datasets by name
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




####################################################################
# Celebrity Names
####################################################################


celebrity <- read.csv("data/celebrity_deaths_4.csv") %>% filter(nationality=="American") %>% 
  separate(name, into=c("Name"), sep=" ", remove=FALSE) %>% 
  mutate(info = paste(name," ","(",birth_year,"-", death_year,")",sep='')) %>% 
  select(Name, info) %>% 
  group_by(Name) %>% 
  mutate(info = paste0(info, collapse = ",")) %>% slice(1)

################ maybe i should join by first and put actors with same name info in string

write.csv(celebrity, "data/celebrity.csv", row.names=FALSE)

celebrity <- read_csv("data/celebrity.csv")
  




########################################################################
# population
########################################################################

nameslist <-read.csv("data/nameslist.csv")

population_yearF <- nameslist %>% 
  filter(Gender=="F") %>% 
  group_by(Year) %>% 
  summarise(Population = sum(as.integer(Count)))

write.csv(population_yearF, "data/populationYearF.csv", row.names=FALSE)

population_yearM <- nameslist %>% 
  filter(Gender=="M") %>% 
  group_by(Year) %>% 
  summarise(Population = sum(as.integer(Count)))
write.csv(population_yearM, "data/populationYearM.csv", row.names=FALSE)



population_state <- nameslist %>% 
  group_by(State) %>% 
  summarise(Count = sum(as.integer(Count)))

population_gender <- nameslist %>% 
  group_by(Gender) %>% 
  summarise(Count = sum(as.integer(Count)))


population_year_state <- nameslist %>% 
  group_by(Year, State, Gender) %>% 
  summarise(Population=sum(as.integer(Count)))
write.csv(population_year_state, "data/population_year_state.csv", row.names=FALSE)



#######################################################################
# rank
#######################################################################

nameslist <-read.csv("data/nameslist.csv")

population_year <- nameslist %>% 
  group_by(Year, Gender) %>% 
  summarise(YearPopulation = sum(as.integer(Count)))


nameslist <- nameslist %>% 
  group_by(Name,Year,Gender) %>% 
  summarise(Count=sum(Count)) %>% 
  ungroup() %>% 
  group_by(Year,Gender) %>% 
  mutate(Rank=as.integer(rank(desc(Count), ties.method = "min")))

nameslist<- left_join(nameslist, population_year, b=c("Gender", "Year")) %>% 
  mutate(Percent=(Count/Population)/0.01)


for (name in levels(nameslist$Name)){
  tmp=subset(nameslist,Name==name)
  fn=paste('data/splitnames_withrank/',name,sep='','.csv')
  write.csv(tmp,fn,row.names=FALSE)
}

#---------------------
#doesnt work because data now has many copies of the same CountNamebyYearGender
#so there are even more ties that affect the rank

population_state <- nameslist %>% 
  group_by(State, Gender) %>% 
  summarise(Population = sum(as.integer(Count)))
write.csv(population_state, "data/population_state.csv", row.names = FALSE)

nlist <- nameslist %>% 
  group_by(Name,Year,Gender) %>% 
  mutate(CountNamebyYearGender=sum(Count)) %>% 
  ungroup() %>% 
  group_by(Year,Gender) %>% 
  mutate(Rank=as.integer(rank(desc(CountNamebyYearGender), ties.method = "min")))

nlist2 <- left_join(nlist, population_year, by=c("Gender", "Year")) %>% 
  mutate(Percent=(CountNamebyYearGender/YearPopulation)/0.01)

nlist3 <- left_join(nlist2, population_state, by=c("Gender", "State")) %>% 
  mutate(PercentTotal=(Count/StatePopulation)/0.01)

for (name in levels(nlist3$Name)){
  tmp=subset(nlist3,Name==name)
  fn=paste('data/splitnames_complete/',name,sep='','.csv')
  write.csv(tmp,fn,row.names=FALSE)
}



#####################################################################3
# popular names rank
##################################################################3333

nameslist <-read.csv("data/nameslist.csv")


#when nothing is selected
topingender <- nameslist %>%
  group_by(Name, Gender) %>%
  summarise(Count=sum(Count)) %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(Rank=as.integer(rank(desc(Count), ties.method = "min"))) %>%
  filter(Rank <= 200) %>% 
  arrange(Rank) %>% 
  ungroup()
write.csv(topingender, "data/topingender.csv", row.names=FALSE)

topM <- topingender %>% filter(Gender=="M") %>% 
  select(Rank, Name, Count) %>% 
  rename(`Male_Names`=Name, CountM = Count)

topF <- topingender %>% filter(Gender=="F") %>% 
  select(Name,Count) %>% 
  rename(`Female_Names`=Name, CountF=Count)

topMF <- cbind(topM,topF)


write.csv(topMF, "data/topingender.csv", row.names=FALSE)



#when state is selected
topinstate <- nameslist %>%
  group_by(Name, State) %>%
  summarise(Count=sum(Count)) %>% 
  ungroup() %>% 
  group_by(State) %>% 
  mutate(Rank=as.integer(rank(desc(Count), ties.method = "first"))) %>%
  filter(Rank <= 100) %>% 
  arrange(Rank) %>% 
  ungroup()

stateM <- topinstate %>% filter(Gender=="M") %>% arrange(State, Rank) %>% 
  select(Rank, State, Name, Count) %>% 
  rename(`Male_Names`=Name, CountM = Count)


stateF <- topinstate %>% filter(Gender=="F") %>% arrange(State, Rank) %>% 
  select(Name,Count) %>% 
  rename(`Female_Names`=Name, CountF=Count)

stateMF <- cbind(stateM,stateF)

write.csv(topinstate, "data/topinstate.csv", row.names=FALSE)


#when year is selected
topinyear <- nameslist %>%
  group_by(Name, Year) %>% 
  summarise(Count=sum(Count)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(Rank=as.integer(rank(desc(Count), ties.method = "first"))) %>%
  filter(Rank <= 100) %>% 
  arrange(Rank) %>% 
  ungroup()

yearM <- topinyear %>% filter(Gender=="M") %>% arrange(Year, Rank) %>% 
  select(Rank, Year, Name, Count) %>% 
  rename(`Male_Names`=Name, CountM = Count)

yearF <- topinyear %>% filter(Gender=="F") %>% arrange(Year, Rank) %>% 
  select(Name,Count) %>% 
  rename(`Female_Names`=Name, CountF=Count)

yearMF <- cbind(yearM,yearF)

write.csv(topinyear, "data/topinyear.csv", row.names=FALSE)


#when year and state are selected
top <- nameslist %>%
  group_by(Name, Year, State, Gender) %>% 
  summarise(Count=sum(Count)) %>% 
  ungroup() %>% 
  group_by(Year, State, Gender) %>% 
  mutate(Rank=as.integer(rank(desc(Count), ties.method = "first"))) %>%
  filter(Rank <= 100) %>% 
  arrange(Rank) %>% 
  ungroup()

topiM <- top %>% filter(Gender=="M") %>% arrange(State, Year, Rank) %>% 
  select(Rank, State, Year, Name, Count) %>% 
  rename(`Male_Names`=Name, CountM = Count)

topiF <- top %>% filter(Gender=="F") %>% arrange(State, Year, Rank) %>% 
  select(Name,Count) %>% 
  rename(`Female_Names`=Name, CountF=Count)

topiMF <- cbind(topiM,topiF)

write.csv(top, "data/topYearStateGender.csv", row.names=FALSE)

write.csv(top, "data/topYearState.csv", row.names=FALSE)







################################################################################
# list of names to compare similar spellings and names
################################################################################
uname <- data.frame()
uname <- unique(nameslist$Name)
write.csv(uname, "data/justNames.csv", row.names=FALSE)
justName <- read_csv("data/justNames.csv")
View(justName)


#---------------------------------------------

if(input$gender=="M"){
  ggplot()+
    geom_sf(fill=NA, data=states)+
    geom_sf(aes(fill=Count/Population), data=stat)+
    theme_minimal()+
    scale_fill_gradient(low="lightblue", high="darkblue")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    labs(fill="Count per 100,000", title=paste("Count of name",input$name,"over the United States"))
} else if(input$gender=="F") {
  ggplot()+
    geom_sf(fill=NA, data=states)+
    geom_sf(aes(fill=CountPerThousand), data=stat)+
    theme_minimal()+
    scale_fill_gradient(low="lightpink", high="deeppink4")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    labs(fill="Count per 100,000", title=paste("Count of name",input$name,"over the United States"))

  
  
  
  
  

##########################################################
  # code for box of famous people with name
  box(
    textOutput(outputId = "famousTitle"),
    tableOutput(outputId="info"),
    tags$head(tags$style("#famousTitle{
                         font-size: 20px;
}" ))
          ),
  
  output$famousTitle <- renderText({
    paste("Famous People with the Name",input$name)
  })
  
  output$info <- renderTable({
    
    req(input$name)
    name <- name()
    celebrity <- read.csv("../data/celebrity.csv")
    join <- left_join(name, celebrity, by="Name")
    pp <- unique(join$info)
    ppp <- str_split(pp, ",")[[1]]
    as.data.frame(ppp)
    
  },colnames=FALSE, hover=TRUE)
