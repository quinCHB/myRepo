# Shiny Dashboard

## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)

# Load Data
raw_lead <-  read.csv("https://raw.githubusercontent.com/quinCHB/Dashboard-1/main/lead.csv")

ui <- dashboardPage(
  
  dashboardHeader(title = "MN Public Health Data Access Portal"),
  
  dashboardSidebar(
    
    selectInput("par_county",
                label= "Select County of Interest",
                choices= sort(unique(raw_lead$location)),
                selected= "Kittson",
                multiple= FALSE
                )
    
    ),
  dashboardBody(
    fluidRow(box(plotOutput("lead_state")),
             box()),
    fluidRow(box(plotOutput("lead_county")),
             box()),
    fluidRow(box(),
             box()),
    )
)

server <- function(input, output) { 
  
  #State Plot Total
  output$lead_state <-  renderPlot({
   
     raw_lead[raw_lead$location =="Minnesota" & 
              raw_lead$indicator== "Test year (annual method)"&
              raw_lead$Indicator == "Blood lead testing"
              #Requires a comma to work
              ,] |>
      ggplot(aes(x= year, y= pctTested/100, color= ageGroup)) +
      geom_line()+
      geom_point()+
      
      scale_x_discrete(limits = raw_lead$year)+
      labs(
        title = "Lead Testing",
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })
  
  
  output$lead_chb <-  renderPlot({
    
    raw_lead[raw_lead$location =="Minnesota" & 
               raw_lead$indicator== "Test year (annual method)"&
               raw_lead$Indicator == "Blood lead testing"
             #Requires a comma to work
             ,] |>
      ggplot(aes(x= year, y= pctTested/100, color= ageGroup)) +
      geom_line()+
      geom_point()+
      
      scale_x_discrete(limits = raw_lead$year)+
      labs(
        title = "Lead Testing",
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })
  
  
  
  # Get county data subset
  county_sub <- reactive({raw_lead[raw_lead$location == input$par_county,] })
  
  output$lead_county <-  renderPlot({
    #Open parenthesis since it is dynamic
    county_sub() |>
      ggplot(aes(x= year, y= pctTested/100, color= ageGroup)) +
      geom_line()+
      geom_point()+
      
      scale_x_discrete(limits = raw_lead$year)+
      labs(
        title = "Lead Testing",
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })
}

shinyApp(ui=ui, server=server)