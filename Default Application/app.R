# Shiny Dashboard

## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)

# Load Data
  
# Input parameters
  # State Community Health Services Advisory Committee as of 1_17_2024
  input_schsac_raw <- read.csv(
    "https://raw.githubusercontent.com/quinCHB/Public-Datasources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv"
  )
  
  input_chb_raw <- read.csv(
    "https://raw.githubusercontent.com/quinCHB/Public-Datasources/main/MN%20SCHSAC%20%26%20CHB%20Regions/MN%20CHB%20as%20of%201_17_2024.csv"
  )



#Healthly Homes
lead_raw <-  read.csv(
  "https://raw.githubusercontent.com/quinCHB/Public-Datasources/main/MN%20Public%20Health%20Data%20Access%20Portal/Healthy%20Homes/Childhood%20Lead%20Exposure.csv"
)


ui <- dashboardPage(

  dashboardHeader(title = "MN Public Health Data Access Portal"),

  dashboardSidebar(

    selectInput("par_region",
                label= "Select SCHSAC Region",
                choices= sort(unique(input_schsac_raw$Region)),
                selected= "Northwest",
                multiple= FALSE
    ),

    selectInput("par_CHB",
                label= "Select CHB Region",
                choices= sort(unique(input_chb_raw$CHB)),
                selected= "Quin County",
                multiple= FALSE
    ),

    selectInput("par_county",
                label= "Select County of Interest",
                choices= sort(unique(lead_raw$location)),
                selected= "Kittson",
                multiple= FALSE
                )

    ),
  dashboardBody(
    fluidRow(box(plotOutput("lead_state")),
             box(plotOutput("lead_region"))
             ),
    fluidRow(box(plotOutput("lead_chb")), 
             box(plotOutput("lead_county"))
             )
  )
)

server <- function(input, output) { 
  #Healthy Homes
  
  #Lead
  #State
  output$lead_state <-  renderPlot({
   
    lead_raw[lead_raw$location =="Minnesota" & 
              lead_raw$indicator== "Blood lead testing"&
              lead_raw$indicator.type == "Test year (annual method)"
              #Requires a comma to work
              ,] |>
      ggplot(aes(x= year, y= pctTested, color= ageGroup)) +
      geom_line()+
      geom_point()+
      theme(legend.position="bottom")+
      guides(color = guide_legend(title = "Age Group"))+
      scale_x_discrete(limits = lead_raw$year, guide = guide_axis(n.dodge = 2))+
      labs(
        title = "Blood Lead Testing (Test Year) for all of Minnesota",
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })
  
  #Region
  # Combine Region
  # A lot of sources online say to use all= TRUE after the by condition for an inner. 
  # This does not appear to be correct. It seems like it is executing an outer join not an inner join
   lead_Region <- merge(x= lead_raw, y= input_schsac_raw, by.x= "location", by.y= "County") #, all = TRUE) Don't include the all = TRUE 
  # #https://www.youtube.com/watch?v=zmiC7X9fUmo
  # First sum number tested
   # Next sum denominator
   #Combine in a completed data set
   
   #Sum will be applied to numTested and it will (~) be subsetted by everything after
   lead_RegionGrpTested <-  aggregate(numTested~
                                      ageGroup+
                                      Region+
                                      year+
                                      indicator+
                                      indicator.type+
                                      ebllDescription, lead_Region, FUN=sum)
   #Rename grouped field
   colnames(lead_RegionGrpTested)[colnames(lead_RegionGrpTested) == 
                                           'numTested'] <- 'regionNumTested'
   
   lead_RegionGrpDenominator <-  aggregate(denominator~
                                           ageGroup+
                                           Region+
                                           year+
                                           indicator+
                                           indicator.type+
                                           ebllDescription, lead_Region, FUN=sum)
   #Rename grouped field
   colnames(lead_RegionGrpDenominator)[colnames(lead_RegionGrpDenominator) == 
                                           'denominator'] <- 'regionDenominator'
   
   lead_RegionWithTestGrp <- merge(x= lead_Region, 
                                   y= lead_RegionGrpTested, 
                                   by= c(
                                   "ageGroup",
                                   "Region",
                                   "year",
                                   "indicator",
                                   "indicator.type",
                                   "ebllDescription"), 
                                   all.x = TRUE) #Left join so counties may be displayed moving forward
   lead_RegionComplete <- merge(x= lead_RegionWithTestGrp, 
                                y= lead_RegionGrpDenominator, 
                                by= c(
                                      "ageGroup",
                                      "Region",
                                      "year",
                                      "indicator",
                                      "indicator.type",
                                      "ebllDescription"), 
                                      all.x = TRUE) #Left join so counties may be displayed moving forward
   lead_RegionComplete$regionTestPct <- round(lead_RegionComplete$regionNumTested/lead_RegionComplete$regionDenominator*100,2)
 # #  
  #Reactive Data
  lead_region_sub <- reactive({lead_RegionComplete[lead_RegionComplete$Region == input$par_region &
                                                   lead_RegionComplete$indicator== "Blood lead testing"&
                                                   lead_RegionComplete$indicator.type == "Test year (annual method)",] })

  output$lead_region <-  renderPlot({
    #Open parenthesis since it is dynamic
    lead_region_sub() |>
      ggplot(aes(x= year, y= regionTestPct, color= ageGroup)) +
      geom_line()+
      geom_point()+
      guides(color = guide_legend(title = "Age Group"))+
      scale_x_discrete(limits = lead_region_sub()$year, guide = guide_axis(n.dodge = 2))+
      labs(
        title = paste("Blood Lead Testing (Test Year) for", input$par_region, "Region"),
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })

#CHB
    # Combine CHB
    # This does not appear to be correct. It seems like it is executing an outer join not an inner join
    lead_CHB <- merge(x= lead_raw, y= input_chb_raw, by.x= "location", by.y= "County") #, all = TRUE) Don't include the all = TRUE
    
    # #https://www.youtube.com/watch?v=zmiC7X9fUmo
    # First sum number tested
    # Next sum denominator
    #Combine in a completed data set
    
    #Sum will be applied to numTested and it will (~) be subsetted by everything after
    lead_CHBGrpTested <-  aggregate(numTested~
                                         ageGroup+
                                         CHB+
                                         year+
                                         indicator+
                                         indicator.type+
                                         ebllDescription, lead_CHB, FUN=sum)
    #Rename grouped field
    colnames(lead_CHBGrpTested)[colnames(lead_CHBGrpTested) == 
                                     'numTested'] <- 'chbNumTested'
    
    lead_CHBGrpDenominator <-  aggregate(denominator~
                                         ageGroup+
                                         CHB+
                                         year+
                                         indicator+
                                         indicator.type+
                                         ebllDescription, lead_CHB, FUN=sum)
    #Rename grouped field
    colnames(lead_CHBGrpDenominator)[colnames(lead_CHBGrpDenominator) == 
                                          'denominator'] <- 'chbDenominator'
    
    lead_CHBWithTestGrp <- merge(x= lead_CHB, 
                                 y= lead_CHBGrpTested, 
                                 by= c(
                                 "ageGroup",
                                 "CHB",
                                 "year",
                                 "indicator",
                                 "indicator.type",
                                 "ebllDescription"), 
                                 all.x = TRUE) #Left join so counties may be displayed moving forward
    lead_CHBComplete <- merge(x= lead_CHBWithTestGrp, 
                              y= lead_CHBGrpDenominator, 
                              by= c(
                                 "ageGroup",
                                 "CHB",
                                 "year",
                                 "indicator",
                                 "indicator.type",
                                 "ebllDescription"), 
                                 all.x = TRUE) #Left join so counties may be displayed moving forward
    lead_CHBComplete$CHBTestPct <- round(lead_CHBComplete$chbNumTested/lead_CHBComplete$chbDenominator*100,2)
    # #  
    #Reactive Data
    lead_CHB_sub <- reactive({lead_CHBComplete[lead_CHBComplete$CHB == input$par_CHB &
                                               lead_CHBComplete$indicator== "Blood lead testing"&
                                               lead_CHBComplete$indicator.type == "Test year (annual method)",] })
  
  
  output$lead_chb <-  renderPlot({
    #Open parenthesis since it is dynamic
    lead_CHB_sub() |>
      ggplot(aes(x= year, y= CHBTestPct, color= ageGroup)) +
      geom_line()+
      geom_point()+
      guides(color = guide_legend(title = "Age Group"))+
      scale_x_discrete(limits = lead_raw$year, guide = guide_axis(n.dodge = 2))+
      labs(
        title = paste("Blood Lead Testing (Test Year) for", input$par_CHB),
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })
  
  
  # Get county data subset
  lead_county_sub <- reactive({lead_raw[lead_raw$location == input$par_county & 
                              lead_raw$indicator== "Blood lead testing" &
                              lead_raw$indicator.type == "Test year (annual method)",] 
                            })
  
  output$lead_county <-  renderPlot({
    #Open parenthesis since it is dynamic
    lead_county_sub() |>
      ggplot(aes(x= year, y= pctTested, color= ageGroup)) +
      geom_line()+
      geom_point()+
      guides(color = guide_legend(title = "Age Group"))+
      scale_x_discrete(limits = lead_raw$year, guide = guide_axis(n.dodge = 2))+
      scale_y_continuous(breaks= round(seq(min(lead_county_sub()$pctTested), max(lead_county_sub()$pctTested), by= 2))) +
      labs(
        title = paste("Blood Lead Testing (Test Year) for", input$par_county, "County"),
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })
}

shinyApp(ui=ui, server=server)