

name <- read.csv(paste('data/splitnames/',"Laura",sep='','.csv')) %>% 
  filter(Gender == "F") %>% 
  group_by(Name, Gender, Year) %>% 
  summarise(Count=sum(Count))

p <- name %>% ggplot(aes(x=Year, y=Count))+
  geom_line(size=2, color="lightpink2")+
  geom_line(data=dat, aes(x=Year, y=slope))+
  theme_minimal()+
  labs(title="Trend in Name Over Time")+
  coord_cartesian(xlim=c(1910,2017))

ggplotly(p)


#########################################################
# working with the slope

y<-name$Count
x<-name$Year 
n<- nrow(name)

y_n <- y[1:(n-1)]-y[2:n]
y_n<-append(y_n, NA, after = length(y_n))
x_n <- x[1:(n-1)]-x[2:n]
x_n <- append(x_n, NA, after = length(x_n))

#y_n2 <- y[1:(n-5)]-y[6:n]
#y_n2<-append(y_n, NA, after = length(y_n))
#x_n2 <- x[1:(n-5)]-x[6:n]
#x_n2 <- append(x_n, NA, after = length(x_n))

dat <- data.frame(x, y, x_n, y_n, x_n2, y_n2)

dat <- dat %>% mutate(slope = y_n/x_n) %>% rename(Year=x, Count=y, year_n=x_n, count_n=y_n)




dat %>% filter(Count == max(dat$Count)) # gives the highest count the name reached





