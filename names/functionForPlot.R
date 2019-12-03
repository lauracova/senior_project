#didn't get anything to work here


#setting variables
name <- read.csv(paste('data/splitnames_withrank/',"Laura",sep='','.csv'))%>% 
                   filter(Gender == "F") %>% 
                   group_by(Name, Gender, Year) %>% 
                   summarise(Count=sum(Count))
if("" != ""){
  name2 <- read.csv(paste('data/splitnames_withrank/',"Sarah",sep='','.csv'))%>% 
    filter(Gender == "F") %>% 
    group_by(Name, Gender, Year) %>% 
    summarise(Count=sum(Count))
  name3 <- rbind(name, name2)
} else{name3 <- ""}

button <- "Count"


#Function
myplot <- function(n1, n2, bn){
  
  if(n2 == ""){
    if(n1$Gender=="F"){
      p <- n1 %>% ggplot(aes(x=Year, y=))+
        geom_line(size=1.5, color="lightpink2")+
        theme_minimal()+
        labs(title="Trend in Name Over Time in U.S.")+
        coord_cartesian(xlim=c(1910,2017))+
        theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
    }
  return(p)
  }
}

  #call to function
p <- myplot(name, name3, button)
ggplotly(p)






