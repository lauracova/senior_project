#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)




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
          box(
            textInput(inputId = "name", label = "Enter a first name:"),
            actionButton(inputId = "update", label = "Update")
          ), 
          box(
            plotOutput(outputId = "plot", hover = "plot_hover"),
            verbatimTextOutput("info")
            #change hover to click to get x and y click on-click instead
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
  
  output$plot <- renderPlot({
    input$update
    
    nameslist <- read.csv("../data/nameslist.csv") %>% select(-X) %>% 
      filter(Name == input$name) %>% 
      group_by(Name, Gender, Year) %>% 
      summarise(Count=sum(as.numeric(Count)))
    
      nameslist %>% 
        ggplot(aes(x=as.integer(Year), y=Count, color=Gender))+
        geom_line(size=2)+
        #geom_point(color="black", alpha=0.3)+
        scale_color_manual(values=c("lightpink2", "steelblue1"))+
        theme_minimal()+
        labs(x="Year", title="Trend in Name Over Time")
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_hover$x, "\ny=", input$plot_hover$y)
  })
  #change hover to click to get x and y click on-click instead
  
}

# Run the application 
shinyApp(ui = ui, server = server)

