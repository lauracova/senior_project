
nameslist <-read.csv("data/nameslist.csv")


taylor <- filter(nameslist, Name=="Taylor" & Year >= 1970)

matthew <- filter(nameslist, Name=="Matthew")

laura <- filter(nameslist, Name=="Laura")

#use first letter of name to predict as well

t <- taylor %>% group_by(Gender) %>% summarise(Count=sum(Count))
a <- filter(t, Gender=="F")
b <- filter(t, Gender=="M")
#if(a/(a+b)>0.6 | b/(a+b)>0.6 | a/(a+b)<0.4 | b/(a+b)<0.4){
  
#}else{
  if(a$Count>b$Count){
    gender <- "F"
  }else{
    gender <- "M"
  }
#}


names <- filter(nameslist, Year >= 2010) %>% 
  group_by(Name, Gender) %>% 
  summarise(Count=sum(Count)) %>% 
  mutate(First=str_sub(Name, end = 1)) 
  

lm(Gender~Name, data=names)






#have names gotten more diverse over time- show number of different names based on the number of people for the year

n <- nameslist %>% 
  group_by(Name, Gender, Year) %>% 
  mutate(Unique = 1) %>% 
  ungroup() %>% 
  group_by(Gender, Year) %>% 
  summarise(Unique=sum(Unique), Population = sum(as.integer(Count)))
n %>% ggplot()+
  geom_line(aes(x=Year, y=Unique/Population, color=Gender), size=1)+
  theme_minimal()
#maybe show percent of people that have the same name ?




#--------------------------------------------------------------------
# 


pred <- filter(nameslist, Year >= 1960) %>% 
  group_by(Name, Gender) %>% 
  summarise(Count=sum(Count)) %>% 
  spread(key=Gender, value=Count) %>% 
  rename(Female = `F`, Male=`M`) %>% 
  replace_na(list(Female=0, Male=0)) %>% # filter(Male==Female) %>% View()
  mutate(PercentFemale=Female/(Female+Male), PercentMale=Male/(Female+Male),
         Gender= case_when(Female > Male ~ "F",
                           Male > Female ~ "M",
                           Female == Male ~ "F")) %>% 
  select(-c(Female, Male))
 # filter(PercentFemale >= 0.5 & PercentFemale < 1) %>% View()


write.csv(pred, "data/genderpred.csv", row.names=FALSE)
pred <- read_csv("data/genderpred.csv")


pred %>% ggplot()+
  geom_histogram(aes(x=PercentFemale))+
  theme_minimal()+
  labs(title="Distribution of the Percent of a Name that is Female")


