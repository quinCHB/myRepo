
childhoodLeadExposureUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    textOutput(NS(id, "text"))
  )
}

childhoodLeadExposureServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
      
    n <- reactive({sum(df()) })
    output$text <- renderText({n()})
      
      
    }
  )
}
