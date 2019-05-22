#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(plotly)



ui <- dashboardPage( skin="black",
  
  dashboardHeader(title="Names in America"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Name Search", tabName = "names"),
      menuItem("Popularity", tabName = "popular")
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "names",
        fluidRow(
          box(width = 3,
            textInput(inputId = "name", label = "Enter a first name:"),
            selectInput(inputId = "gender", label = "Enter gender:", choices = list("Choose one","M", "F")),
            submitButton(text = "Update")
          ),
          box(width = 3,
              textInput(inputId = "name2", label = "Enter another first name:"),
              selectInput(inputId = "gender2", label = "Enter gender:", choices = list("Choose one","M", "F")),
              submitButton(text = "Update")
          ),
          box(
            plotlyOutput(outputId = "plot")
            ),
          box(
            textOutput(outputId="info")
          )
        )
      ),
      #second tab content
      tabItem(tabName = "popular",
              h2("Rank of Names")
      )
    )
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    name <- read.csv(paste('../data/splitnames/',input$name,sep='','.csv'))
      
    p <- name %>% 
      filter(Gender == input$gender) %>% 
      group_by(Name, Gender, Year) %>% 
      summarise(Count=sum(Count))
    if(input$gender=="F"){
      p <- p %>% 
        ggplot(aes(x=Year, y=Count))+
        geom_line(size=2, color="lightpink2")+
        theme_minimal()+
        labs(title="Trend in Name Over Time")+
        coord_cartesian(xlim=c(1910,2017))
    } else
      p <- p %>% 
        ggplot(aes(x=Year, y=Count))+
        geom_line(size=2, color="steelblue1")+
        theme_minimal()+
        labs(title="Trend in Name Over Time")+
        coord_cartesian(xlim=c(1910,2017))
      ggplotly(p)
  })
  
  output$info <- renderText({  #renderTable or Code
    
    #paste("Celebrities with the first name ", input$name, ":", sep="")?????????????
    
    name <- read.csv(paste('../data/splitnames/',input$name,sep='','.csv'))
    celebrity <- read_csv("../data/celebrity.csv")
    join <- left_join(name, celebrity, by="Name")
    unique(join$info)
    #info1 <- unique(join$info)
    
      #if(input$name == celebrity$first){
      #  print(celebrity$info)
      #}

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

