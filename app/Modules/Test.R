


nameUI <- function(id) {
  ns <- NS(id)
  tagList(
  
  )
}


otherModuleServer <- function(id, county, region, chb) {
  moduleServer(id,function(input, output, session) {
    # Use the reactive values
    observe({
      print(paste("Selected county:", county()))
      print(paste("Selected region:", region()))
      print(paste("Selected CHB:", chb()))
    })
    # ... rest of the server logic
      
    }
  )
}