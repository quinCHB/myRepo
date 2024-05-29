
source("./Data/df_globalSchsacChb.R")

# Module UI Function
mod_globalparSchsacChbInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    #Check box will be used on every tab to hide some sort of narrative
    checkboxInput(ns("parGlobal_hideNarrative"), label = "HIDE NARRATIVE"),
    selectInput(
      ns("parGlobal_county"),
      label= "Select County of Interest",
      choices= sort(unique(df_globalSchsacChb$county)),
      selected= "Kittson",
      multiple= FALSE,
      width= 350 
    ),
    selectInput(
      ns("parGlobal_region"),
      label= "Select SCHSAC Region",
      choices= NULL,
      selected= NULL,
      multiple= FALSE,
      width= 350 
    ),
    selectInput(
      ns("parGlobal_chb"),
      label= "Select Community Health Board",
      choices= NULL,
      selected= NULL,
      multiple= FALSE,
      width= 350 
    )
  )
}

# Module Server Function using moduleServer
mod_globalparSchsacChbInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$parGlobal_county, {
      
      # Update the region input based on the county input
      # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples. 
      # Currently, the county source is from the region data source but if it ever changes this should help capture it
      if(input$parGlobal_county %in% df_globalSchsacChb$county) {
        updateSelectInput(
          session, "parGlobal_region",
          choices =  unique(df_globalSchsacChb$region[order(input$parGlobal_county == df_globalSchsacChb$county, decreasing = TRUE)]),
          selected = unique(df_globalSchsacChb$region[order(input$parGlobal_county == df_globalSchsacChb$county, decreasing = TRUE)])[1]
        )
        updateSelectInput(
          session, "parGlobal_chb",
          choices =  unique(df_globalSchsacChb$chb[order(input$parGlobal_county == df_globalSchsacChb$county, decreasing = TRUE)]),
          selected = unique(df_globalSchsacChb$chb[order(input$parGlobal_county == df_globalSchsacChb$county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter is mapped to a ggplot
        )
      }
      else {
        updateSelectInput(
          session, "parGlobal_region",
          choices = "",
          selected = ""
        )
        updateSelectInput(
          session, "parGlobal_chb",
          choices = "",
          selected = ""
        )
      }
    })
    # Update the chb input based on the county input
    # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples. 
    # Currently, the county source is from the chb data source but if it ever changes this should help capture it
    # observeEvent(input$parGlobal_county, {
    #   if(input$parGlobal_county %in% df_globalSchsacChb$county) {
    #     updateSelectInput(
    #       session, "parGlobal_chb",
    #       choices =  unique(df_globalSchsacChb$chb[order(input$parGlobal_county == df_globalSchsacChb$county, decreasing = TRUE)]),
    #       selected = unique(df_globalSchsacChb$chb[order(input$parGlobal_county == df_globalSchsacChb$county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter is mapped to a ggplot
    #     )
    #   }
    #   else {
    #     updateSelectInput(
    #       session, "parGlobal_chb",
    #       choices = "",
    #       selected = ""
    #     )
    #   }
    # }
    # )
    #Credit goes to MS Copilot for the following code. Obtained 5/24/2024
    # Create reactive values for the selected county and region
     selectedCounty <- reactive({ input$parGlobal_county })
     selectedRegion <- reactive({ input$parGlobal_region })
     selectedChb <- reactive({ input$parGlobal_chb })
    # 
    # # Return a list of reactive values to be used in other modules
     return(list(
       selectedCounty = selectedCounty,
       selectedRegion = selectedRegion,
       selectedChb = selectedChb
     ))
    
  })
}

# Below is how I can test the application
# Standard Shiny UI
library(shiny)
 ui <- fluidPage(titlePanel("Test"),sidebarLayout(sidebarPanel(mod_globalparSchsacChbInputUI("x")),mainPanel()))
# # Main App Server
 server <- function(input, output, session) {mod_globalparSchsacChbInputServer("x")}
 shinyApp(ui, server)
