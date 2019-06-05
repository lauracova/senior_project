#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(plotly)
library(ggrepel)
library(stringr)
library(rvest)

ui <- dashboardPage( skin="black",
  
  dashboardHeader(title="Names in America"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Name Search", tabName = "names"),
      menuItem("Popular Name Rank", tabName = "popular")
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "names", h2("Search a Name"),
        fluidRow(
          box(width = 3,
            textInput(inputId = "name", label = "Enter a first name:"),
            selectInput(inputId = "gender", label = "Enter gender:", choices = list("Choose one","M", "F")),
            submitButton(text = "Update")
          ),
          box(
            plotlyOutput(outputId = "plot"),
            radioButtons(inputId = "button", label= NULL, choices = c("Count", "Percent"), inline=TRUE)
            ),
          box(width = 3,
              textInput(inputId = "name2", label = "Enter another first name to compare:"),
              selectInput(inputId = "gender2", label = "Enter gender:", choices = list("Choose one","M", "F")),
              submitButton(text = "Update")
          ),
          box(
            textOutput(outputId = "famousTitle"),
            tableOutput(outputId="info"),
            tags$head(tags$style("#famousTitle{
                                 font-size: 20px;
                                 }" ))
          ),
          box(
            textOutput(outputId = "meaningTitle"),
            uiOutput(outputId="meaning"), br(),
            "This information came from www.behindthename.com.",
            tags$head(tags$style("#meaningTitle{
                                 font-size: 20px;
                                 }" ))
          )
        )
      ),
      #second tab content
      tabItem(tabName = "popular", h2("Rank of Names"),h4("Select to display rank of names:"),

          fluidRow(
            column(4, selectInput(inputId = "populargender", "Gender:", c("All", "M", "F"))),
            column(4, selectInput(inputId= "popularstate", "State:", c("All", state.abb))),
            column(4, uiOutput(outputId="popularyear"))
          ),
          tableOutput("populartable")
          
      )
    )
  )
)

nameslist <-read.csv("../data/nameslist.csv")


server <- function(input, output) {
  
  output$popularyear <- renderUI({
    selectInput("popyear", "Year:", choices=c("All",unique(as.character(nameslist$Year))))
  })
  
  name <- reactive(read.csv(paste('../data/splitnames/',input$name,sep='','.csv'))%>% 
    filter(Gender == input$gender) %>% 
    group_by(Name, Gender, Year) %>% 
    summarise(Count=sum(Count)))
  
  output$plot <- renderPlotly({
    
    req(input$name)
    name <- name()
    
    if(input$button=="Count"){

      if(input$name2 != ""){
        name2 <- read.csv(paste('../data/splitnames/',input$name2,sep='','.csv'))%>% 
          filter(Gender == input$gender2) %>% 
          group_by(Name, Gender, Year) %>% 
          summarise(Count=sum(Count))
        name3 <- rbind(name, name2)
      }
  
      dat <- name %>% filter(Count == max(Count))
      
      if(input$name2 == ""){
        if(input$gender=="F"){
          p <- name %>% ggplot(aes(x=Year, y=Count))+
            geom_line(size=1.5, color="lightpink2")+
            geom_point(data=dat)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
        } else {
          p <- name %>% 
            ggplot(aes(x=Year, y=Count))+
            geom_line(size=1.5, color="steelblue1")+
            geom_point(data=dat)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
      }} else if (input$name2 != ""){
        if(input$gender=="F" & input$gender2=="F"){
          p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
            geom_line(size=1.5)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            scale_color_manual(values=c("lightpink2", "pink"))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
        } else if(input$gender=="F" & input$gender2=="M"){
          p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
            geom_line(size=1.5)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            scale_color_manual(values=c("lightpink2", "steelblue1"))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
        }else if(input$gender=="M" & input$gender2=="F"){
          p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
            geom_line(size=1.5)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            scale_color_manual(values=c("steelblue1", "lightpink2"))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
        }else if(input$gender=="M" & input$gender2=="M"){
          p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
            geom_line(size=1.5)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            scale_color_manual(values=c("steelblue1", "steelblue3"))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
          
        }
      }
      
    }else if(input$button=="Percent"){
      
      #wrangling data
      if(input$gender=="F"){
        population_yearF<-read_csv("../data/populationYearF.csv")
        name <- inner_join(name, population_yearF, by="Year") %>% 
          mutate(Percent=(Count/Population)/0.01)
      } else if (input$gender=="M"){
        population_yearM<-read_csv("../data/populationYearM.csv")
        name <- inner_join(name, population_yearM, by="Year") %>% 
          mutate(Percent=(Count/Population)/0.01)
      }
      
      if(input$name2 != ""){
        name2 <- read.csv(paste('../data/splitnames/',input$name2,sep='','.csv'))%>% 
          filter(Gender == input$gender2) %>% 
          group_by(Name, Gender, Year) %>% 
          summarise(Count=sum(Count))
        if(input$gender2=="F"){
          population_yearF<-read_csv("../data/populationYearF.csv")
          name2 <- inner_join(name2, population_yearF, by="Year") %>% 
            mutate(Percent=(Count/Population)/0.01)
        } else if (input$gender2=="M"){
          population_yearM<-read_csv("../data/populationYearM.csv")
          name2 <- inner_join(name2, population_yearM, by="Year") %>% 
            mutate(Percent=(Count/Population)/0.01)
        }
        name3 <- rbind(name, name2)
      }

      #plots that show the percent
      if(input$name2 == ""){
        if(input$gender=="F"){
          p <- name %>% ggplot(aes(x=Year, y=Percent))+
            geom_line(size=1.5, color="lightpink2")+
            theme_minimal()+
            labs(title=paste("Percent of Females named",input$name,"in U.S."))+
            coord_cartesian(xlim=c(1910,2017))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
        } else {
          p <- name %>% 
            ggplot(aes(x=Year, y=Percent))+
            geom_line(size=1.5, color="steelblue1")+
            theme_minimal()+
            labs(title=paste("Percent of Males named",input$name,"in U.S."))+
            coord_cartesian(xlim=c(1910,2017))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
        }} else if (input$name2 != ""){
          if(input$gender=="F" & input$gender2=="F"){
            p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
              geom_line(size=1.5)+
              theme_minimal()+
              labs(title=paste("Percent of Females named",input$name,"vs",input$name2))+
              coord_cartesian(xlim=c(1910,2017))+
              scale_color_manual(values=c("lightpink2", "pink"))+
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
          } else if(input$gender=="F" & input$gender2=="M"){
            p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
              geom_line(size=1.5)+
              theme_minimal()+
              labs(title=paste("Percent of Females named",input$name,"vs Males named",input$name2))+
              coord_cartesian(xlim=c(1910,2017))+
              scale_color_manual(values=c("lightpink2", "steelblue1"))+
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
          }else if(input$gender=="M" & input$gender2=="F"){
            p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
              geom_line(size=1.5)+
              theme_minimal()+
              labs(title=paste("Percent of Males named",input$name,"vs Females named",input$name2))+
              coord_cartesian(xlim=c(1910,2017))+
              scale_color_manual(values=c("steelblue1", "lightpink2"))+
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
          }else if(input$gender=="M" & input$gender2=="M"){
            p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
              geom_line(size=1.5)+
              theme_minimal()+
              labs(title=paste("Percent of Males named",input$name,"vs",input$name2))+
              coord_cartesian(xlim=c(1910,2017))+
              scale_color_manual(values=c("steelblue1", "steelblue3"))+
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
          }
        }
      
    }
    
    #draws the plot
    ggplotly(p)
    
    })
  
  output$famousTitle <- renderText({
    paste("Famous People with the Name",input$name)
  })
  
  output$meaningTitle <- renderText({
    paste("Meaning and History of the Name",input$name)
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
  
  
  output$meaning <- renderUI({  
    
    req(input$name)
    name <- str_to_lower(input$name)
    address <- paste("https://www.behindthename.com/name/",name, sep="")
    my.html <- read_html(address)
    mysection <- html_nodes(my.html, "section")
    mystring <- html_text(mysection)[1]
    meaning <- str_split(mystring, "\n")[[1]][3]
    enc2utf8(meaning)
    
  })
  
  output$populartable <- renderTable({

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

