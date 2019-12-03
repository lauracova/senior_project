#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)

pacman::p_load(tidyverse, USAboundaries, sf, geofacet, ggrepel, scales, readr, plotly, stringr, rvest, leaflet)

topYearState<- read_csv("../data/topYearState.csv") %>% mutate(Year=as.integer(Year), Count=as.integer(Count), Rank=as.integer(Rank))
topYearStateGender<- read_csv("../data/topYearStateGender.csv") %>% mutate(Year=as.integer(Year), Count=as.integer(Count), Rank=as.integer(Rank))

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
      tabItem(tabName = "names", h2("Search a First Name"),
        fluidRow(
          box(width = 3,
            textInput(inputId = "name", label = "Enter a first name:", placeholder = "Name"),
            actionButton(inputId = "go", label = "Update"),
            tags$head(tags$style("#go{margin-bottom: 30px;}")),
            selectInput(inputId = "gender", label = "Enter gender:", choices = list("Choose one","M", "F"))
            ),
          box(
            plotlyOutput(outputId = "plot"),
            column(10, radioButtons(inputId = "button", label= NULL, choices = c("Count", "Percent", "Rank"), inline=TRUE))
            ),
          box(width = 3,
              textInput(inputId = "name2", label = "Enter another first name to compare:", placeholder = "Name"),
              actionButton(inputId = "go2", label = "Update"),
              tags$head(tags$style("#go2{margin-bottom: 30px;}")),
              selectInput(inputId = "gender2", label = "Enter gender:", choices = list("Choose one","M", "F"))
          ),
          box(
            leafletOutput("map"),
            absolutePanel(top=20, right=25,
              selectInput(inputId = "mapyear", label="Enter a year:", choices = c("All", 2017:1910))
            )
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
      tabItem(tabName = "popular", h2("Rank of Popular Names"),h4("Select to display ranking of the top 100 names:"),

          fluidRow(
            column(4, selectInput(inputId="popgender", "Gender:", choices=c("All",c("M", "F")))),
            column(4, selectInput("popyear", "Year:", choices=c("All", 2017:1910))),
            column(4, selectInput(inputId= "popstate", "State:", c("All", state.abb)))

          ),
          tableOutput(outputId = "poptable")
          
      )
    )
  )
)



server <- function(input, output, session) {
  
  
  
  name <- eventReactive(input$go, {
    tmpfilename <- paste0(str_to_title(input$name), ".csv");
    tmpfilepath <- paste('../data/splitnames_withrank/',tmpfilename,sep='');
    if(tmpfilename %in% dir("../data/splitnames_withrank/")){
                            read_csv(tmpfilepath, 
                            col_types = list(col_character(), col_integer(), col_character(), 
                                             col_integer(), col_integer(), col_integer(), col_double()))
    }else{
      data.frame(Name=input$name, Year=1910:2017, Gender=input$gender,Count=0, Rank=0, Population=1000000, Percent=0)
    }
    })
  
#  name <- eventReactive(input$go, {read_csv(paste('../data/splitnames_withrank/',input$name,sep='','.csv'),
#                                              col_types = list(col_character(), col_integer(), col_character(),
#                                                               col_integer(), col_integer(), col_integer(), col_double()))
#  })
  
  name2 <- eventReactive(input$go2, {
    tmpfilename <- paste0(str_to_title(input$name2), ".csv");
    tmpfilepath <- paste('../data/splitnames_withrank/',tmpfilename,sep='');
    if(tmpfilename %in% dir("../data/splitnames_withrank/")){
      read_csv(tmpfilepath, 
               col_types = list(col_character(), col_integer(), col_character(), 
                                col_integer(), col_integer(), col_integer(), col_double()))
    }else{
      data.frame(Name=input$name2, Year=1910:2017, Gender=input$gender2,Count=0, Rank=0, Population=1000000, Percent=0)
    }
  })
  
  
  observe({
    pred <- read_csv('../data/genderpred.csv')
    g <- filter(pred, Name==str_to_title(input$name))[[4]]
    updateSelectInput(session, "gender",label = "Enter gender:", choices = list("Choose one","M", "F"), selected = g)
  })
  
  observe({
    pred <- read_csv('../data/genderpred.csv')
    g <- filter(pred, Name==str_to_title(input$name2))[[4]]
    updateSelectInput(session, "gender2",label = "Enter gender:", choices = list("Choose one","M", "F"), selected = g)
  })
  
  output$plot <- renderPlotly({
    
    req(input$name)
    req(input$gender)

    name <- name() %>% filter(Gender == input$gender)

    if(input$name2!=""){
      name2 <- name2() %>% filter(Gender == input$gender2)
      name3 <- rbind(name, name2)
    }
  
    
    if(input$button=="Count"){

      if(input$name2 == ""){
        if(input$gender=="F"){
          p <- name %>% ggplot(aes(x=Year, y=Count))+
            geom_line(size=1.5, color="lightpink2")+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
            scale_y_continuous(labels = comma)
        } else {
          p <- name %>% 
            ggplot(aes(x=Year, y=Count))+
            geom_line(size=1.5, color="steelblue1")+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
            scale_y_continuous(labels = comma)
          
        }} else if (input$name2 != ""){
        if(input$gender=="F" & input$gender2=="F"){
          p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
            geom_line(size=1.5)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            scale_color_manual("",values=c("lightpink2", "pink"))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
            scale_y_continuous(labels = comma)
        } else if(input$gender=="F" & input$gender2=="M"){
          if(input$name == input$name2){
            p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Gender))+
              geom_line(size=1.5)+
              theme_minimal()+
              labs(title="Trend in Name Over Time in U.S.")+
              coord_cartesian(xlim=c(1910,2017))+
              scale_color_manual(values=c("lightpink2", "steelblue1"))+
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
              scale_y_continuous(labels = comma)
          }else{
            if(input$name < input$name2){
              p <- name3 %>%  ggplot(aes(x=Year, y=Count, color=Name))+
                geom_line(size=1.5)+
                theme_minimal()+
                labs(title="Trend in Name Over Time in U.S.")+
                coord_cartesian(xlim=c(1910,2017))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                scale_y_continuous(labels = comma)+
                scale_color_manual("",values=c("lightpink2", "steelblue1"))
            } else if(input$name > input$name2){
              p <- name3 %>%  ggplot(aes(x=Year, y=Count, color=Name))+
                geom_line(size=1.5)+
                theme_minimal()+
                labs(title="Trend in Name Over Time in U.S.")+
                coord_cartesian(xlim=c(1910,2017))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                scale_y_continuous(labels = comma)+
                scale_color_manual("",values=c("steelblue1", "lightpink2"))
            }
          }
        }else if(input$gender=="M" & input$gender2=="F"){
          if(input$name == input$name2){
            p <- name3 %>% 
              ggplot(aes(x=Year, y=Count, color=Gender))+
              geom_line(size=1.5)+
              theme_minimal()+
              labs(title="Trend in Name Over Time in U.S.")+
              coord_cartesian(xlim=c(1910,2017))+
              scale_color_manual(values=c("lightpink2", "steelblue1"))
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
              #scale_y_continuous(labels = comma)
          } else{
            if(input$name < input$name2){
              p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
                geom_line(size=1.5)+
                theme_minimal()+
                labs(title="Trend in Name Over Time in U.S.")+
                coord_cartesian(xlim=c(1910,2017))+
                scale_color_manual("",values=c("steelblue1","lightpink2"))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                scale_y_continuous(labels = comma)
            } else if(input$name > input$name2){
              p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
                geom_line(size=1.5)+
                theme_minimal()+
                labs(title="Trend in Name Over Time in U.S.")+
                coord_cartesian(xlim=c(1910,2017))+
                scale_color_manual("",values=c("lightpink2", "steelblue1"))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                scale_y_continuous(labels = comma)
            }
          }
        }else if(input$gender=="M" & input$gender2=="M"){
          p <- name3 %>% ggplot(aes(x=Year, y=Count, color=Name))+
            geom_line(size=1.5)+
            theme_minimal()+
            labs(title="Trend in Name Over Time in U.S.")+
            coord_cartesian(xlim=c(1910,2017))+
            scale_color_manual("",values=c("steelblue1", "steelblue3"))+
            theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
            scale_y_continuous(labels = comma)
          
        }
      }
      
    }else if(input$button=="Percent"){

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
              scale_color_manual("",values=c("lightpink2", "pink"))+
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
          } else if(input$gender=="F" & input$gender2=="M"){
            if(input$name == input$name2){
              p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Gender))+
                geom_line(size=1.5)+
                theme_minimal()+
                labs(title=paste("Percent of Females named",input$name,"vs Males named",input$name2))+
                coord_cartesian(xlim=c(1910,2017))+
                scale_color_manual(values=c("lightpink2", "steelblue1"))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
            } else {
              if(input$name < input$name2){
                p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
                  geom_line(size=1.5)+
                  theme_minimal()+
                  labs(title=paste("Percent of Females named",input$name,"vs Males named",input$name2))+
                  coord_cartesian(xlim=c(1910,2017))+
                  scale_color_manual("",values=c("lightpink2", "steelblue1"))+
                  theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
              } else if(input$name > input$name2){
                p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
                  geom_line(size=1.5)+
                  theme_minimal()+
                  labs(title=paste("Percent of Females named",input$name,"vs Males named",input$name2))+
                  coord_cartesian(xlim=c(1910,2017))+
                  scale_color_manual("",values=c("steelblue1", "lightpink2"))+
                  theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
              }
            }
          }else if(input$gender=="M" & input$gender2=="F"){
            if(input$name == input$name2){
              p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Gender))+
                geom_line(size=1.5)+
                theme_minimal()+
                labs(title=paste("Percent of Males named",input$name,"vs Females named",input$name2))+
                coord_cartesian(xlim=c(1910,2017))+
                scale_color_manual(values=c("lightpink2","steelblue1"))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
            } else{
              if(input$name < input$name2){
                p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
                  geom_line(size=1.5)+
                  theme_minimal()+
                  labs(title=paste("Percent of Males named",input$name,"vs Females named",input$name2))+
                  coord_cartesian(xlim=c(1910,2017))+
                  scale_color_manual("",values=c("steelblue1", "lightpink2"))+
                  theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
              }else if(input$name > input$name2){
                p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
                  geom_line(size=1.5)+
                  theme_minimal()+
                  labs(title=paste("Percent of Males named",input$name,"vs Females named",input$name2))+
                  coord_cartesian(xlim=c(1910,2017))+
                  scale_color_manual("",values=c("lightpink2", "steelblue1"))+
                  theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
              }
            }
          }else if(input$gender=="M" & input$gender2=="M"){
            p <- name3 %>% ggplot(aes(x=Year, y=Percent, color=Name))+
              geom_line(size=1.5)+
              theme_minimal()+
              labs(title=paste("Percent of Males named",input$name,"vs",input$name2))+
              coord_cartesian(xlim=c(1910,2017))+
              scale_color_manual("",values=c("steelblue1", "steelblue3"))+
              theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))
          }
        }
        }else if(input$button=="Rank"){
          
          #plots that show the rank
          if(input$name2 == ""){
            if(input$gender=="F"){
              p <- name %>% ggplot(aes(x=Year, y=Rank))+
                geom_line(size=1.5, color="lightpink2")+
                theme_minimal()+
                labs(title=paste("Ranking of Females named",input$name,"in U.S."))+
                coord_cartesian(xlim=c(1910,2017))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                scale_y_reverse(labels=comma)
            } else {
              p <- name %>% 
                ggplot(aes(x=Year, y=Rank))+
                geom_line(size=1.5, color="steelblue1")+
                theme_minimal()+
                labs(title=paste("Ranking of Males named",input$name,"in U.S."))+
                coord_cartesian(xlim=c(1910,2017))+
                theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                scale_y_reverse(labels=comma)
            }} else if (input$name2 != ""){
              if(input$gender=="F" & input$gender2=="F"){
                p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Name))+
                  geom_line(size=1.5)+
                  theme_minimal()+
                  labs(title=paste("Ranking of Females named",input$name,"vs",input$name2))+
                  coord_cartesian(xlim=c(1910,2017))+
                  scale_color_manual("",values=c("lightpink2", "pink"))+
                  theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                  scale_y_reverse(labels=comma)
              } else if(input$gender=="F" & input$gender2=="M"){
                if(input$name == input$name2){
                  p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Gender))+
                    geom_line(size=1.5)+
                    theme_minimal()+
                    labs(title=paste("Ranking of Females named",input$name,"vs Males named",input$name2))+
                    coord_cartesian(xlim=c(1910,2017))+
                    scale_color_manual(values=c("lightpink2", "steelblue1"))+
                    theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                    scale_y_reverse(labels=comma)
                } else {
                  if(input$name < input$name2){
                    p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Name))+
                      geom_line(size=1.5)+
                      theme_minimal()+
                      labs(title=paste("Ranking of Females named",input$name,"vs Males named",input$name2))+
                      coord_cartesian(xlim=c(1910,2017))+
                      scale_color_manual("",values=c("lightpink2", "steelblue1"))+
                      theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                      scale_y_reverse(labels=comma)
                  } else if(input$name > input$name2){
                    p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Name))+
                      geom_line(size=1.5)+
                      theme_minimal()+
                      labs(title=paste("Ranking of Females named",input$name,"vs Males named",input$name2))+
                      coord_cartesian(xlim=c(1910,2017))+
                      scale_color_manual("",values=c("steelblue1", "lightpink2"))+
                      theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                      scale_y_reverse(labels=comma)
                  }
                }
              }else if(input$gender=="M" & input$gender2=="F"){
                if(input$name == input$name2){
                  p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Gender))+
                    geom_line(size=1.5)+
                    theme_minimal()+
                    labs(title=paste("Ranking of Males named",input$name,"vs Females named",input$name2))+
                    coord_cartesian(xlim=c(1910,2017))+
                    scale_color_manual(values=c("lightpink2", "steelblue1"))+
                    theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                    scale_y_reverse(labels=comma)
                } else {
                  if(input$name < input$name2){
                    p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Name))+
                      geom_line(size=1.5)+
                      theme_minimal()+
                      labs(title=paste("Ranking of Males named",input$name,"vs Females named",input$name2))+
                      coord_cartesian(xlim=c(1910,2017))+
                      scale_color_manual("",values=c("steelblue1", "lightpink2"))+
                      theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                      scale_y_reverse(labels=comma)
                  } else if(input$name > input$name2){
                    p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Name))+
                      geom_line(size=1.5)+
                      theme_minimal()+
                      labs(title=paste("Ranking of Males named",input$name,"vs Females named",input$name2))+
                      coord_cartesian(xlim=c(1910,2017))+
                      scale_color_manual("",values=c("lightpink2", "steelblue1"))+
                      theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                      scale_y_reverse(labels=comma)
                  }
                }
              }else if(input$gender=="M" & input$gender2=="M"){
                p <- name3 %>% ggplot(aes(x=Year, y=Rank, color=Name))+
                  geom_line(size=1.5)+
                  theme_minimal()+
                  labs(title=paste("Ranking of Males named",input$name,"vs",input$name2))+
                  coord_cartesian(xlim=c(1910,2017))+
                  scale_color_manual("",values=c("steelblue1", "steelblue3"))+
                  theme(plot.margin=unit(c(1.2,0.1,0,0.2),"cm"))+
                  scale_y_reverse(labels=comma)
              }
            }
      
    }
    
    #draws the plot
    ggplotly(p)
    
    })
  
  
  namewithstate <- eventReactive(input$go, {read_csv(paste('../data/splitnames/',input$name,sep='','.csv')) %>% 
      mutate(Year=as.integer(Year), Count=as.integer(Count)) %>% 
      filter(Gender == input$gender)
                                            
  })
  
  output$map <- renderLeaflet({
    
    req(input$name)
    namewithstate <- namewithstate()

    
    if(input$mapyear=="All"){
      population_state <- read_csv("../data/population_state.csv") %>% filter(Gender==input$gender)
      dat <- namewithstate %>% 
        group_by(State) %>% 
        summarise(Count=sum(Count)) %>% left_join(population_state, by="State") %>% 
        mutate(Percent = round((Count/Population)/0.01, digits = 3))  %>% 
        mutate(CountPerThousand = (Count*1000)/Population)
      } else if (input$mapyear!="All"){
      population_year_state <- read_csv("../data/population_year_state.csv") %>% 
        filter(Gender==input$gender, Year==input$mapyear)
      dat <- filter(namewithstate, Year==input$mapyear) %>% 
        left_join(population_year_state, by=c("State", "Year", "Gender")) %>% 
        mutate(Percent = round((Count/Population)/0.01, digits = 3))  %>% 
        mutate(CountPerThousand = (Count*1000)/Population)
    }
    
    states <- us_states(resolution="low") %>% filter(name!="Puerto Rico") %>% rename(State=state_abbr)

    stat <- left_join(dat, states, by="State")
    
    if(input$gender=="F"){
      pal <- colorNumeric("RdPu", domain=stat$Percent)
    } else if(input$gender=="M"){
      pal <- colorNumeric("GnBu", domain=stat$Percent)
    }
    
    #install.packages("albersusa)
    #spdf <- rmapshaper::ms_simplify(usa_sf(), keep = 0.1)
    
    epsg2163 <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "EPSG:2163",
      proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
      resolutions = 2^(16:7))
    
    leaflet(options = leafletOptions(crs = epsg2163)) %>% 
      setView(lng = -95, lat = 37, zoom = 2) %>% 
      addPolygons(data=states, fillColor = ~pal(stat$Percent), color = "white", weight=2,
                  highlight = highlightOptions(weight=3, color="white", bringToFront = TRUE, opacity = 1),
                  label= paste(states$name, stat$Percent, "%"), fillOpacity = 1) %>% 
      addLegend(data=stat, pal= pal, values= ~Percent, opacity = 1, title = "Percent", position = "bottomleft")
    
  
    })
  

  
  output$meaningTitle <- renderText({
    paste("Meaning and History of the Name",input$name)
  })
  

  
  
  output$meaning <- renderUI({  
    
    req(input$name)
    name <- input$name
    address <- paste("https://www.behindthename.com/name/",name, sep="")
    my.html <- read_html(address)
    mysection <- html_nodes(my.html, "section")
    mystring <- html_text(mysection)[1]
    meaning <- str_split(mystring, "\n")[[1]][3]
    enc2utf8(meaning)
    
  })
  
  output$poptable <- renderTable({
    
    mydat <- data.frame()
    
    if(input$popgender!="All" & input$popyear=="All" & input$popstate=="All"){
      topingender <-read_csv("../data/topingender.csv") %>%
        mutate(Count=as.integer(Count), Rank=as.integer(Rank))
      mydat <- filter(topingender, Gender==input$popgender)
    }
    if(input$popgender!="All" & input$popyear!="All" & input$popstate=="All"){
      topinyearbyGender <-read_csv("../data/topinYearbyGender.csv", col_types = list(col_character(), col_integer(), col_character(), col_integer(), col_integer()))
      mydat <- filter(topinyearbyGender, Gender==input$popgender & Year==input$popyear)
    }
    if(input$popgender!="All" & input$popyear=="All" & input$popstate!="All"){
      topinstatebyGender <-read_csv("../data/topinstatebyGender.csv", col_types = list(col_character(), col_character(), col_character(), col_integer(), col_integer()))
      mydat <- filter(topinstatebyGender, Gender==input$popgender & State==input$popstate)
    }
    if(input$popgender!="All" & input$popyear!="All" & input$popstate!="All"){
      mydat <-filter(topYearStateGender, Year==input$popyear, State==input$popstate, Gender==input$popgender)
    }
    if(input$popgender=="All" & input$popyear!="All" & input$popstate=="All"){
      topinyear <-read_csv("../data/topinyear.csv", col_types = list(col_character(), col_integer(), col_integer(), col_integer()))
      mydat <- filter(topinyear, Year==input$popyear)
    }
    if(input$popgender=="All" & input$popyear=="All" & input$popstate!="All"){
      topinstate <-read_csv("../data/topinstate.csv") %>% 
        mutate(Count=as.integer(Count), Rank=as.integer(Rank))
      mydat <- filter(topinstate, State==input$popstate)
    }
    if(input$popgender=="All" & input$popyear!="All" & input$popstate!="All"){
      mydat <- filter(topYearState, Year==input$popyear, State==input$popstate)
    }
    mydat
    
  }, striped=TRUE, bordered=TRUE, hover=TRUE)

  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

