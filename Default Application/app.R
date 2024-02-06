# Shiny Dashboard

## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
# dplr

# Load Data
  
# Input parameters
  # State Community Health Services Advisory Committee as of 1_17_2024
  input_schsac_raw <- read.csv(
    "https://raw.githubusercontent.com/quinCHB/Public-Datasources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv"
  )
  
  input_chb_raw <- read.csv(
    "https://raw.githubusercontent.com/quinCHB/Public-Datasources/main/MN%20SCHSAC%20%26%20CHB%20Regions/MN%20CHB%20as%20of%201_17_2024.csv"
  )

#Healthy Homes
lead_raw <-  read.csv(
  "https://raw.githubusercontent.com/quinCHB/Public-Datasources/main/MN%20Public%20Health%20Data%20Access%20Portal/Healthy%20Homes/Childhood%20Lead%20Exposure.csv"
)

# # Split the data frame by the group column
# schsac_split <- split("region_narrative" , "region_narrative$Region")
# 
# # Apply a function to each subset to create a new column with comma-separated values
# schsac_character <- sapply(schsac_split, function(x) paste(x$County, collapse = ", "))
# 
# # Convert the result to a data frame
# schsac_df <- as.data.frame(schsac_character)
# 
# # Create an empty character vector to store the results
# schsac_character_placeholder <- character()
# 
# # Loop through each row of schsac_df
# for (i in 1:nrow(schsac_df)) {
#   # Get the row name and schsac_character_placeholder as strings
#   rowname <- as.character(rownames(schsac_df)[i])
#   schsac_character_placeholder <- as.character(schsac_df$schsac_character[i])
#   
#   # Concatenate them with a separator
#   concat <- paste0(rowname, ":: ", schsac_character_placeholder)
#   
#   # Append the result to the vector
#   schsac_character_placeholder <- c(schsac_character_placeholder, concat)
# }
# 
# # Paste the result vector with semicolons and new lines
# test <- HTML(paste(schsac_character_placeholder, collapse=  "<br/>"))


ui <- dashboardPage(
                    dashboardHeader(title = "MN Public Health Data Access Portal", titleWidth = 400),
                    dashboardSidebar(
                                    width = 350,
                                    
                                    selectInput("par_county",
                                                label= "Select County of Interest",
                                                choices= sort(unique(lead_raw$location)),
                                                selected= "Kittson",
                                                multiple= FALSE
                                                ),
                                    selectInput("par_region",
                                                label= "Select SCHSAC Region",
                                                choices= NULL,
                                                selected= NULL,
                                                multiple= FALSE
                                                ),
                                    selectInput("par_chb",
                                                label= "Select CHB Region",
                                                choices= NULL,
                                                selected= NULL,
                                                multiple= FALSE
                                                ),
                                    
                                    #Sidebar is required
                                    sidebarMenu(
                                              menuItem("Child Health", tabName = "childHealth"),
                                              menuItem("Climate", tabName = "climate"),
                                              menuItem("Diseases & Conditions", tabName = "diseasesConditions"),
                                              menuItem("Environmental Health", tabName = "environmentalHealth"),
                                              menuItem("Health Behaviors/Risk Factors", tabName = "healthBehaviorsRiskFactors"),
                                              menuItem("Health Equity", tabName = "healthEquity"),
                                              menuItem("Healthy Homes", tabName = "healthyHomes")
                                              )
                                
                                    ),
                      dashboardBody(
                                    fluidRow(
                                            column(12,
                                                  tabItems(
                                              ##########################################################################
                                                          tabItem(
                                                                  tabName = "childHealth",
                                                                  tabsetPanel(
                                                                            tabPanel("Asthma"),
                                                                            tabPanel("Health Inequities in Childhood Asthma"),
                                                                            tabPanel("Birth Defects"),
                                                                            tabPanel("Childhood Lead Exposure"),
                                                                            tabPanel("Free/Reduced Price Lunch Eligibility", h5("testing 123")),
                                                                            tabPanel("Immunizations", h5("testing 123")),
                                                                            tabPanel("Oral Health", h5("testing 123"))
                                                                            )
                                                                   ),
                                              ####################################################################################
                                                            tabItem(
                                                                    tabName = "climate",
                                                                    tabsetPanel(
                                                                              tabPanel("Air Quality"),
                                                                              tabPanel("Cold-related Illness"),
                                                                              tabPanel("Climate-related Environmental Health Concerns"),
                                                                              tabPanel("Heat-related Illness"),
                                                                              tabPanel("Hot Weather", h5("testing 123")),
                                                                              tabPanel("Pollen", h5("testing 123"))
                                                                              )
                                                                    ),
                                              ####################################################################################
                                                            tabItem(
                                                                    tabName = "diseasesConditions",
                                                                    tabsetPanel(
                                                                                tabPanel("Asthma"),
                                                                                tabPanel("Birth Defects"),
                                                                                tabPanel("Cancer"),
                                                                                tabPanel("Carbon Monoxide (CO) Poisoning"),
                                                                                tabPanel("Chronic Obstructive Pulmonary Disease (COPD)", h5("testing 123")),
                                                                                tabPanel("Diabetes", h5("testing 123")),
                                                                                tabPanel("Heart Attacks"),
                                                                                tabPanel("Heat-related Illness", h5("testing 123")),
                                                                                tabPanel("Immunizations", h5("testing 123")),
                                                                                tabPanel("Oral Health")
                                                                                )
                                                                    ),
                                              ####################################################################################
                                                            tabItem(
                                                                    tabName = "environmentalHealth",
                                                                    tabsetPanel(
                                                                              tabPanel("Air Quality"),
                                                                              tabPanel("Biomonitoring: Chemicals in people"),
                                                                              tabPanel("Cold-related Illness"),
                                                                              tabPanel("Drinking Water Quality"),
                                                                              tabPanel("Environmental Justice", h5("testing 123")),
                                                                              tabPanel("Heat-related Illness", h5("testing 123")),
                                                                              tabPanel("Pesticide Poisoning"),
                                                                              tabPanel("Traffic", h5("testing 123"))
                                                                              )
                                                                    ),
                                                ####################################################################################
                                                            tabItem(
                                                                    tabName = "healthBehaviorsRiskFactors",
                                                                    tabsetPanel(
                                                                              tabPanel("Health Insurance"),
                                                                              tabPanel("Immunizations"),
                                                                              tabPanel("Obesity"),
                                                                              tabPanel("Oral Health"),
                                                                              tabPanel("Poverty & Income", h5("testing 123")),
                                                                              tabPanel("Smoking", h5("testing 123"))
                                                                              )
                                                                    ),
                                                ####################################################################################
                                                            tabItem(
                                                                    tabName = "healthEquity",
                                                                    tabsetPanel(
                                                                              tabPanel("Health Equity"),
                                                                              tabPanel("Health Inequities in Childhood Lead Exposure"),
                                                                              tabPanel("Health Inequities in Childhood Asthma")
                                                                              )
                                                                    ),
                                                ####################################################################################
                                                            tabItem(
                                                                    tabName = "healthyHomes",
                                                                    tabsetPanel(
                                                                              tabPanel("Carbon Monoxide (CO) Poisoning"),
                                                                              tabPanel("Childhood Lead Exposure",
                                                                                      fluidRow(
                                                                                        box(plotOutput("lead_state")),
                                                                                        box(plotOutput("lead_region"))
                                                                                             ),
                                                                                      fluidRow(
                                                                                        box(plotOutput("lead_chb")), 
                                                                                        box(plotOutput("lead_county"))
                                                                                             )
                                                                                   # fluidRow(htmlOutput("region_narrative"))
                                                                                    ),
                                                                              tabPanel("Drinking Water Quality"),
                                                                              tabPanel("Pesticide Poisoning"),
                                                                              tabPanel("Radon", h5("testing 123")),
                                                                              )
                                                                  )
                                                          )
                                                )
                                         ),
                                    # #Global
                                    fluidRow(
                                            # create a row to contain the box and the tabs
                                            column(12,# use the full width of the row
                                                   tabsetPanel(

                                                              tabPanel("Regions",
                                                                      fluidRow(uiOutput("region_narrative", style = "font-size: 20px;"))
                                                                      ),
                                                              tabPanel("CHBs",
                                                                       fluidRow(uiOutput("chb_narrative", style = "font-size: 20px;"))
                                                              )

                                                    )
                                                   )
                                    # fluidRow(
                                    #         # create a row to contain the box and the tabs
                                    #         column(
                                    #               12, # use the full width of the row
                                    #                fluidRow(uiOutput("chb_narrative", style = "font-size: 20px;"))
                                    #               )
                                    #         )
                              )
                          )
                  )
server <- function(input, output, session) {
  
  
  output$region_narrative <- renderUI({
                                      # Create region data frame for global narrative reference
                                      schsac_raw <- input_schsac_raw
                                      
                                      # Replace the values that are equal to input county by adding the font tab
                                      schsac_raw$County[schsac_raw$County == input$par_county] <-  paste("<font color=red>", schsac_raw$County[schsac_raw$County == input$par_county], "</font>")
                                      
                                      # Split the data frame by the group column
                                      schsac_split <- split(schsac_raw , schsac_raw$Region)

                                      # Apply a function to each subset to create a new column with comma-separated values
                                      schsac_character <- sapply(schsac_split, function(x) paste(x$County, collapse = ", "))

                                      # Convert the result to a data frame
                                      schsac_df <- as.data.frame(schsac_character)

                                      # Create an empty character vector to store the results
                                      schsac_result <- character()

                                      # Loop through each row of schsac_df
                                      for (i in 1:nrow(schsac_df)) {
                                        # Get the row name and schsac_character_placeholder as strings
                                        rowname <- as.character(rownames(schsac_df)[i])
                                        schsac_character_placeholder <- as.character(schsac_df$schsac_character[i])

                                        # Concatenate them with a separator
                                        concat <- paste0(rowname, ":: ", schsac_character_placeholder)

                                        # Append the result to the vector
                                        schsac_result <- c(schsac_result, concat)
                                      }

                                      # Paste the result vector with semicolons and new lines
                                      HTML(paste(schsac_result, collapse=  "<br/>"))
                                  })
  
  output$chb_narrative <- renderUI({
                                      # Create region data frame for global narrative reference
                                      chb_raw <- input_chb_raw
                                      
                                      # Replace the values that are equal to input county by adding the font tab
                                      chb_raw$County[chb_raw$County == input$par_county] <-  paste("<font color=red>", chb_raw$County[chb_raw$County == input$par_county], "</font>")
                                      
                                      # Split the data frame by the group column
                                      chb_split <- split(chb_raw , chb_raw$CHB)
                                      
                                      # Apply a function to each subset to create a new column with comma-separated values
                                      chb_character <- sapply(chb_split, function(x) paste(x$County, collapse = ", "))
                                      
                                      # Convert the result to a data frame
                                      chb_df <- as.data.frame(chb_character)
                                      
                                      # Create an empty character vector to store the results
                                      chb_result <- character()
                                      
                                      # Loop through each row of chb_df 
                                      for (i in 1:nrow(chb_df)) {
                                        # Get the row name and chb_character_placeholder as strings
                                        rowname <- as.character(rownames(chb_df)[i])
                                        chb_character_placeholder <- as.character(chb_df$chb_character[i])
                                        
                                        # Concatenate them with a separator
                                        concat <- paste0(rowname, ":: ", chb_character_placeholder)
                                        
                                        # Append the result to the vector
                                        chb_result <- c(chb_result, concat)
                                      }
                                      
                                      # Paste the result vector with semicolons and new lines
                                      HTML(paste(chb_result, collapse=  "<br/>"))
                                    })
  
  
  # Update the region input based on the county input
  observeEvent(input$par_county, {
        # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples
        if(input$par_county %in% input_schsac_raw$County)
        {
          # Create a vector for all region
          allRegions <- input_schsac_raw$Region 
          
          # Create a vector of all counties
          regionCounties <- input_schsac_raw$County
          
          # Reorder the region by the county input
          orderedRegions <- allRegions[order(regionCounties == input$par_county, decreasing = TRUE)]
          
          # Reorder the counties by the county input
          orderedCounties <- regionCounties[order(regionCounties == input$par_county, decreasing = TRUE)]
          
          # The commented line below will produce a list of all regions with counties in parenthesis. Initially, this was a good thought process, it is not user friendly and can cause confusion
          #labeledRegions <-  paste0(orderedRegions, " (", orderedCounties, ")") # if the following is used, only passes one orderRegions object #setNames(orderedRegions, paste0(orderedRegions, " (", orderedCounties, ")"))
          # Create a named vector of counties with their region names as labels
          labeledRegions <-  setNames(orderedRegions, paste0(orderedRegions))
          
          # Update the choices and selected values of the country input
          updateSelectInput(session, "par_region",
                            choices = labeledRegions,
                            selected = labeledRegions[1]
                            )
        }
    else
      {
        updateSelectInput(session, "par_region",
                          choices = "",
                          selected = ""
                        )
      }
  })
  
  # Update the chb input based on the county input
 observeEvent(input$par_county, {
    # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples
    if(input$par_county %in% input_chb_raw$County) {
      
      # Create a vector of all CHBS
      allCHBS <- input_chb_raw$CHB
      
      # Create a vector of CHB names for each country
      chbCounties <- input_chb_raw$County
      
      # Reorder the region by the county input
      orderedChbs <- allCHBS[order(chbCounties == input$par_county, decreasing = TRUE)]
      
      # Reorder the continents by the continent input
      orderedCounties <- chbCounties[order(chbCounties == input$par_county, decreasing = TRUE)]
      
      # Create a named vector of countries with their continent names as labels
      labeledChbs <- setNames(orderedChbs, paste0(orderedChbs)) #see regions above for more explanation about this line 
      
      # Update the choices and selected values of the country input
      updateSelectInput(session, "par_chb",
                        choices = labeledChbs,
                        selected = labeledChbs[1]
      )
    }
    else {updateSelectInput(session, "par_chb",
                            choices = "",
                            selected = "")
    }
  })
  

  
  
  #Healthy Homes
  
  #Display every other label
  # Get the unique values of x
  xlabels <- unique(lead_raw$year)
  # Remove every other value by subsetting with a logical vector
  xlabels [c (FALSE, TRUE)] <- ""
  
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
      theme(legend.position="bottom", text=element_text(size=21))+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
      scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here AI generated
     #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60)) +
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
  lead_region_sub <- reactive({lead_RegionComplete[gsub(" \\(.*$", "", input$par_region) == lead_RegionComplete$Region & #To improve the label, I have to use gsub to remove everything before space(
                                                   lead_RegionComplete$indicator== "Blood lead testing"&
                                                   lead_RegionComplete$indicator.type == "Test year (annual method)",] })

  output$lead_region <-  renderPlot({
    #Open parenthesis since it is dynamic
    lead_region_sub() |>
      ggplot(aes(x= year, y= regionTestPct, color= ageGroup)) +
      geom_line()+
      geom_point()+
      theme(legend.position="bottom", text=element_text(size=21))+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
      scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here AI generated
    #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60)) +
      labs(
        title = paste("Blood Lead Testing (Test Year) for", gsub(" \\(.*$", "", input$par_region), "Region"), #gsub is used to remove space
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
    lead_CHB_sub <- reactive({lead_CHBComplete[gsub(" \\(.*$", "",input$par_chb) == lead_CHBComplete$CHB  &
                                                
                                               lead_CHBComplete$indicator== "Blood lead testing"&
                                               lead_CHBComplete$indicator.type == "Test year (annual method)",] })
  
  
    output$lead_chb <-  renderPlot({
      #Open parenthesis since it is dynamic
     lead_CHB_sub() |>
        ggplot(aes(x= year, y= CHBTestPct, color= ageGroup))  +
        geom_line()+
        geom_point()+
        theme(legend.position="bottom", text=element_text(size=21))+
        guides(color = guide_legend(title = "Age Group"))+
        scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
        #Had this
       # scale_x_discrete(limits = lead_raw$year, guide = guide_axis(n.dodge = 2))+
        scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here AI generated
        #Sets y axis for the same of all the graphs
        scale_y_continuous(limits= c(0, 60)) +
        labs(
          title = paste("Blood Lead Testing (Test Year) for", input$par_chb),
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
      theme(legend.position="bottom", text=element_text(size=21))+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
      scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here AI generated
    #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60))+
      labs(
        title = paste("Blood Lead Testing (Test Year) for", input$par_county, "County"),
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024")
  })
  
  
  # https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
    output$lead_narrative <- renderUI({
      strRegion <- paste("You selected", "<font color=red>",input$par_region, "</font>", "as the Region")
      strCHB <- paste("Youuuuuu selected", "<font color=red>", input$par_chb, "</font>", "as the CHB")
      strCounty <- paste("You selected", "<font color=red>", input$par_county, "</font>", "as the County")
      HTML(paste(strRegion, strCHB, strCounty,  final_result, sep = '<br/>'))
    })
}

shinyApp(ui=ui, server=server)

