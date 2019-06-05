


nameslist <-read.csv("data/nameslist.csv")


CountByNameGender<- nameslist %>% group_by(Gender, Name) %>% summarise(Count=sum(Count)) %>% arrange(desc(Count))
write.csv(CountByNameGender, "data/CountByNameGender", row.names = FALSE)



if(input$popularstate != "All"){
  
}
if(input$popularyear != "All"){
  
}
if(input$populargender != "All" & input$popularstate != "All"){
  
}
if(input$populargender != "All" & input$popularyear != "All"){
  
}
if(input$popularyear != "All" & input$popularstate != "All"){
  
}


