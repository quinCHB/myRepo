# Shiny Dashboard
# Hastag (# ---------------------------------------------------------------) means Primary Header
# Hastag (### *** Definition ###) means Secondary Header
# Hold shift alt o to collapse all
# Hold alt o to expand all

# Load Packages ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
#Uncomment the shinyjs library when testing application so appliaction can be run locally 
#when deployed comment out the shinyjs library since shinylive loads it 
#library(shinyjs)

# Load Data ---------------------------------------------------------------

# Input parameters
  # State Community Health Services Advisory Committee as of 1_17_2024
  input_schsac_raw <- read.csv("https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv")
  # Community Health Board as of 1_17_2024
  input_chb_raw <- read.csv("https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv")

### *** Healthy Homes ***
  
#Lead
lead_raw <-  read.csv("https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20Public%20Health%20Data%20Access%20Portal/Healthy%20Homes/Childhood%20Lead%20Exposure.csv")
#Radon

# User Interface ----------------------------------------------------------
ui <- dashboardPage(
                    dashboardHeader(
                                    title = "MN Public Health Data Access Portal",
                                    titleWidth = 400
                                    #disable = TRUE #uncomment if the header should be hid
                                    ),
                    dashboardSidebar(
                                    width = 350, #This makes the sidebar wider. However, the input boxes seem to have a set dimension resulting in long names still wrapping
                                    # The next items require
                                    #The input parameters are global parameters
                                    checkboxInput(inputId = "par_hide_narrative", label = "HIDE NARRATIVE"),
                                    # Other input elements...
                                    selectInput(
                                                "par_county",
                                                label= "Select County of Interest",
                                                choices= sort(unique(input_schsac_raw$County)),
                                                selected= "Kittson",
                                                multiple= FALSE,
                                                width= 350 
                                              ),
                                    selectInput(
                                                "par_region",
                                                label= "Select SCHSAC Region",
                                                choices= NULL,
                                                selected= NULL,
                                                multiple= FALSE,
                                                width= 350 
                                              ),
                                    selectInput(
                                                "par_chb",
                                                label= "Select Community Health Board",
                                                choices= NULL,
                                                selected= NULL,
                                                multiple= FALSE,
                                                width= 350 
                                              ),
                                    #Sidebar is required to have sub menus because it requires the tabName to reference
                                    sidebarMenu(
                                                menuItem("Region & CHB Defintions", tabName = "regionChbDefinations"),
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
                        shinyjs::useShinyjs(), #Thank you Abby Stamm at MDH for suggesting to only call one function in a package rather then the entire package 
                                    fluidRow(
                                            column(12,
                                                   tabItems(
                                                          ####################################################################################
                                                          tabItem(
                                                                  tabName = "regionChbDefinations",
                                                                  tabsetPanel(
                                                                              id= "willThisWork",
                                                                              tabPanel(
                                                                                      "Region",
                                                                                      fluidRow(
                                                                                              # Narrative section explaining the purpose of the dashboard
                                                                                              column(
                                                                                                    width = 12,
                                                                                                    h1("Welcome to the Landing Page for This Awesome Dashboard...Subjective of Course :)"),
                                                                                                    h3("There are three filters on the left: Select County of Interest, Select SCHSAC Region, & Select Community Health Board."),
                                                                                                    h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Regions will remain in <b>bold</b>.")),
                                                                                                    h3("For this tab, nothing will change if the Select SCHSAC Region and Select Community Health Board filters are updated."),
                                                                                                    h3("The purpose for this tab is to provide a quick reference for what counties fall under which region."),
                                                                                                    #The next line inserts a line between the narrative and the data
                                                                                                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
                                                                                                    )
                                                                                              ),
                                                                                      fluidRow(
                                                                                              uiOutput("region_narrative", style = "font-size: 20px;")
                                                                                              )
                                                                                      ),
                                                                              tabPanel(
                                                                                      "CHB",
                                                                                      fluidRow(
                                                                                              # Narrative section explaining the purpose of the dashboard
                                                                                              column(
                                                                                                    width = 12,
                                                                                                    h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Community Health Boards will remain in <b>bold</b>.")),
                                                                                                    h3("For this tab, nothing will change if the Select SCHSAC Region and Select Community Health Board filters are updated."),
                                                                                                    h3("The purpose for this tab is to provide a quick reference for what counties fall under which Community Health Board."),
                                                                                                    #The next line inserts a line between the narrative and the data
                                                                                                    tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
                                                                                                  )
                                                                                              ),
                                                                                        fluidRow(
                                                                                                column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;")),
                                                                                                column(6, uiOutput("chb_narrative_02", style = "font-size: 20px;"))
                                                                                                )
                                                                                        )
                                                                            )
                                                                  ),
                                                ##########################################################################
                                                            tabItem(
                                                                    tabName = "childHealth",
                                                                    #Dynamic Narrative for child health
                                                                    h5("testing 123"),
  
  
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
                                                                                                 # Narrative section explaining the purpose of the dashboard
                                                                                                  column(
                                                                                                        width= 1,
                                                                                                         selectInput(
                                                                                                                     "par_leadYear",
                                                                                                                     label= "Select Year",
                                                                                                                     choices= sort(unique(lead_raw$year), decreasing = TRUE),
                                                                                                                     selected= max(unique(lead_raw$year)),
                                                                                                                     multiple= FALSE
                                                                                                                    ),
                                                                                                         selectInput(
                                                                                                                     "par_stateRegionChb",
                                                                                                                     label= "Select Comparison",
                                                                                                                     choices= c("All", "CHB", "Region", "State"),
                                                                                                                     selected= "All",
                                                                                                                     multiple= FALSE
                                                                                                                   )
                                                                                                        ),
                                                                                                   # Narrative section explaining the purpose of the dashboard
                                                                                                   column(
                                                                                                          width = 11,
                                                                                                           id = "lead_narrativeHide",
                                                                                                          h4(htmlOutput("lead_narrative"))
                                                                                                          )
                                                                                               ),
                                                                                        fluidRow(
                                                                                                #The next line inserts a line between the narrative and the data.
                                                                                                #It is added here instead of in the narrative since the narrative can have multiple columns. 
                                                                                                #If there are multiple narrative columns and the line is added there than the line consists of multiple breaks 
                                                                                                tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                                                                                                box(plotOutput("lead_state")),
                                                                                                box(plotOutput("lead_region"))
                                                                                               ),
                                                                                        fluidRow(
                                                                                                box(plotOutput("lead_chb")),
                                                                                                box(plotOutput("lead_county"))
                                                                                               )
                                                                                      ),
                                                                                tabPanel("Drinking Water Quality"),
                                                                                tabPanel("Pesticide Poisoning"),
                                                                                tabPanel("Radon", h5("testing 123")),
                                                                                )
                                                                    )
                                                          )
                                                   
                                                  )
                                        )
                              )
                  )




# Home Narratives ---------------------------------------------------------

##############################################################################################
#To improve performance load these once and don't have them run every time the server runs

###########################Region & County Definitions#########################################################
## Region Narrative

# Create region data frame for global narrative reference
schsac_raw <- input_schsac_raw

#Bold Regions so it is easier to understand narrative
schsac_raw$Region <-  paste("<b>", schsac_raw$Region, "</b>")

##############
#############
## CHB Narrative 01 (It is split in half so it displays in two nice columns on the UI
# Create chb data frame for global narrative reference
chb_raw_01 <- input_chb_raw[1:33,]

#Bold CHBs so it is easier to understand narrative
chb_raw_01$CHB <-  paste("<b>", chb_raw_01$CHB, "</b>")

##############
#############
## CHB Narrative 02 (It is split in half so it displays in two nice columns on the UI
# Create chb data frame for global narrative reference
chb_raw_02 <- input_chb_raw[34:nrow(input_chb_raw),] # :nrow means it goes to the end of the data frame

#Bold CHBs so it is easier to understand narrative
chb_raw_02$CHB <-  paste("<b>", chb_raw_02$CHB, "</b>")

################################### Healthy Homes #########################################

#Childhood Lead Exposure

# Combine CHB
# This does not appear to be correct. It seems like it is executing an outer join not an inner join
lead_CHB <- merge(x= lead_raw, y= input_chb_raw, by.x= "location", by.y= "County") #, all = TRUE) Don't include the all = TRUE

# #https://www.youtube.com/watch?v=zmiC7X9fUmo
# First sum number tested
# Next sum denominator
#Combine in a completed data set

#Sum will be applied to numTested and it will (~) be subset by everything after
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

server <- function(input, output, session) {
  
  output$region_narrative <- renderUI({
                                      
                                      
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
                                      
                                      # Paste the result vector and create a new line after each CHB
                                      HTML(paste(schsac_result, collapse=  "<br/>"))
                                      
                                      })
  
  output$chb_narrative_01 <- renderUI({
                                      # Replace the values that are equal to input county by adding the font tab
                                      chb_raw_01$County[chb_raw_01$County == input$par_county] <-  paste("<font color=red>", chb_raw_01$County[chb_raw_01$County == input$par_county], "</font>")

                                      # Split the data frame by the chb
                                      chb_split <- split(chb_raw_01 , chb_raw_01$CHB)

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

                                      # Paste the result vector and create a new line after each CHB
                                      HTML(paste(chb_result, collapse =  "<br/>"))
                                    })
  
  output$chb_narrative_02 <- renderUI({
                                        # Replace the values that are equal to input county by adding the font tab
                                        chb_raw_02$County[chb_raw_02$County == input$par_county] <-  paste("<font color=red>", chb_raw_02$County[chb_raw_02$County == input$par_county], "</font>")
                                        
                                        # Split the data frame by the chb
                                        chb_split <- split(chb_raw_02 , chb_raw_02$CHB)
                                        
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
                                        
                                        # Paste the result vector and create a new line after each CHB
                                        HTML(paste(chb_result, collapse =  "<br/>"))
                                      })
  # Update the region input based on the county input
  observeEvent(input$par_county, {
                                # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples. 
                                # Currently, the county source is from the region datasource but it if ever changes this will handle it 
                                if(input$par_county %in% input_schsac_raw$County) {
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
                                                                                updateSelectInput(
                                                                                                  session, "par_region",
                                                                                                  choices = labeledRegions,
                                                                                                  selected = labeledRegions[1]
                                                                                                  )
                                                                                }
                                  else {
                                        updateSelectInput(
                                                          session, "par_region",
                                                          choices = "",
                                                          selected = ""
                                                        )
                                        }
                                  }
               )
  
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
                                                                              updateSelectInput(
                                                                                                session, "par_chb",
                                                                                                choices = labeledChbs,
                                                                                                selected = labeledChbs[1]
                                                                                                )
                                                                            }
                                else {
                                      updateSelectInput(
                                                        session, "par_chb",
                                                        choices = "",
                                                        selected = ""
                                                        )
                                      }
                                }
            )
 
 # # Hide Narrative when checkbox is selected
 #https://stackoverflow.com/questions/60054418/shiny-tab-hide-show
 observeEvent(input$par_hide_narrative, ignoreNULL = FALSE, ignoreInit = TRUE, {
                                                                                if(isTRUE(input$par_hide_narrative)) {
                                                                                                                      hideTab(inputId = "willThisWork" , target ="Region")
                                                                                                                      } 
                                                                               else {
                                                                                    showTab(inputId = "willThisWork" , target ="Region")
                                                                                    }
                                                                                           
                                                                                }
              )
 
 
 # Observe the input value of the checkbox
 observe({
   if (isTRUE(input$par_hide_narrative)) {
     # Enable the commented-out portion
     #shinyjs::hide(c("par_leadYear","lead_narrativeHide"))
     shinyjs::hide("par_leadYear")
     shinyjs::hide("par_stateRegionChb")
     shinyjs::hide("lead_narrativeHide")
   } else {
     # Disable the portion when checkbox is unchecked
     shinyjs::show("par_leadYear")
     shinyjs::show("par_stateRegionChb")
     shinyjs::show("lead_narrativeHide")
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
                                    theme(
                                          legend.position="bottom", # move legend to bottom rather than have it on the right
                                          text= element_text(size= 21), # increase font size 
                                          plot.title= element_text(hjust = 0.5) # Center the title
                                          )+
                                    guides(color = guide_legend(title = "Age Group"))+
                                    scale_color_discrete(breaks= c('<3 years', '3-<6 years', '<6 years'))+
                                    scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here CoPilot AI generated
                                   #Sets y axis for the same of all the graphs
                                    scale_y_continuous(limits= c(0, 60)) +
                                    labs(
                                        title = "Blood Lead Testing (Test Year) for all of \nMinnesota", #\n means a new line 
                                        x = NULL,
                                        y = "Pct Tested"
                                        #caption = "Data last updated, 1/15/2024"
                                        )
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
  lead_region_sub <- reactive({lead_RegionComplete[input$par_region == lead_RegionComplete$Region & #Changed variable so no longer need to use gsub
                                                   lead_RegionComplete$indicator== "Blood lead testing"&
                                                   lead_RegionComplete$indicator.type == "Test year (annual method)",] })

  output$lead_region <-  renderPlot({
    #Open parenthesis since it is dynamic
    lead_region_sub() |>
      ggplot(aes(x= year, y= regionTestPct, color= ageGroup)) +
      geom_line()+
      geom_point()+
      theme(
        legend.position="bottom", # move legend to bottom rather than have it on the right
        text= element_text(size= 21), # increase font size 
        plot.title= element_text(hjust = 0.5) # Center the title
      )+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
      scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here AI generated
    #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60)) +
      labs(
          title = paste("Blood Lead Testing (Test Year) for \n", input$par_region, "Region"), #\n means a new line 
          x = NULL,
          y = "Pct Tested"
          #caption = "Data last updated, 1/15/2024"
          )
  })

#CHB
    
    #Reactive Data
    validateChb <- reactive({
                            validate(
                                    need(
                                        input$par_chb == lead_CHBComplete$CHB, paste("There is no data for ", input$par_chb)
                                        )
                                    )
    })
    
    #Reactive Data
    # validateChb <- reactive({
    #                         if(input$par_chb == lead_CHBComplete$CHB
    #                            ) 
    #                             {
    #                             validate(
    #                                     paste("There is no data for ", input$par_chb)
    #                                     )
    #                             }
    #                       })
    
    #Reactive Data
    lead_CHB_sub <- reactive({
                              
                              lead_CHBComplete[input$par_chb == lead_CHBComplete$CHB  &
                              lead_CHBComplete$indicator== "Blood lead testing"&
                              lead_CHBComplete$indicator.type == "Test year (annual method)",] 
                            })
  
  
    output$lead_chb <-  renderPlot({
                                  validateChb() #Won't return red error meassage. It will display the validateChb error message 
                                  #Open parenthesis since it is dynamic
                                 lead_CHB_sub() |>
                                    ggplot(aes(x= year, y= CHBTestPct, color= ageGroup))  +
                                    geom_line()+
                                    geom_point()+
                                    theme(
                                      legend.position="bottom", # move legend to bottom rather than have it on the right
                                      text= element_text(size= 21), # increase font size 
                                      plot.title= element_text(hjust = 0.5) # Center the title
                                      )+
                                    guides(color = guide_legend(title = "Age Group"))+
                                    scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
                                    #Had this
                                   # scale_x_discrete(limits = lead_raw$year, guide = guide_axis(n.dodge = 2))+
                                    scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here AI generated
                                    #Sets y axis for the same of all the graphs
                                    scale_y_continuous(limits= c(0, 60)) +
                                    labs(
                                        title = paste("Blood Lead Testing (Test Year) for \n", input$par_chb, "CHB"), #\n means a new line
                                        x = NULL,
                                        y = "Pct Tested",
                                        caption = "Data last updated, 1/15/2024"
                                       )
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
                                      theme(
                                        legend.position="bottom", # move legend to bottom rather than have it on the right
                                        text= element_text(size= 21), # increase font size
                                        plot.title= element_text(hjust = 0.5) # Center the title
                                          )+
                                      guides(color = guide_legend(title = "Age Group"))+
                                      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
                                      scale_x_discrete(limit= lead_raw$year, breaks = seq(min(lead_raw$year), max(lead_raw$year), 2))+ # add breaks argument here AI generated
                                    #Sets y axis for the same of all the graphs
                                      scale_y_continuous(limits= c(0, 60))+
                                      labs(
                                          title = paste("Blood Lead Testing (Test Year) for \n", input$par_county, "County"), #\n means a new line
                                          x = NULL,
                                          y = "Pct Tested"
                                         # caption = "Data last updated, 1/15/2024"
                                         )
                                  })
  
  # https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
  output$lead_narrative <- renderUI({
####################################################################################################################################################################################
## LESS then 3 YEARS OF AGE
      
##### Less than 3 years of age State      
      strAgeLessThanThreeState <- if(
                                      #ifelse is required for the times when a value is not provided
                                      ifelse(
                                            #Any checks if any true values exist. is.na() did not work correctly
                                            any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested),
                                            #If meets criteria and is not null then it will return valid value
                                            lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
                                            0
                                            )
                                          <
                                          #ifelse isn't required for state since state will always return a value
                                          #Unique is required because there is a duplicate for Minnesota in 2019. This was brought up to MDH and since there tool is changing in 2024 they are not going to fix error.
                                          unique(
                                                lead_raw[lead_raw$year == input$par_leadYear &
                                                         lead_raw$ageGroup == "<3 years" &
                                                         lead_raw$location == "Minnesota" &
                                                         lead_raw$indicator== "Blood lead testing" &
                                                         lead_raw$indicator.type == "Test year (annual method)", ]$pctTested
                                                )
                                      )
                                        {" <b>less</b> than <font color=red>"}
                                  else  {" <b>greater</b> than <font color=red>"}

      strAgeLessThanThreeState <- paste0(
                                          strAgeLessThanThreeState,
                                          "Minnesota </font> ",
                                          unique(
                                                lead_raw[ lead_raw$year == input$par_leadYear &
                                                          lead_raw$ageGroup == "<3 years" &
                                                          lead_raw$location == "Minnesota" &
                                                          lead_raw$indicator== "Blood lead testing" &
                                                          lead_raw$indicator.type == "Test year (annual method)", ]$pctTested
                                                 ),
                                          "% (",
                                          unique(
                                                lead_raw[ lead_raw$year == input$par_leadYear &
                                                          lead_raw$ageGroup == "<3 years" &
                                                          lead_raw$location == "Minnesota" &
                                                          lead_raw$indicator== "Blood lead testing" &
                                                          lead_raw$indicator.type == "Test year (annual method)", ]$numTested
                                                 ),
                                          "/",
                                          unique(
                                                lead_raw[ lead_raw$year == input$par_leadYear &
                                                          lead_raw$ageGroup == "<3 years" &
                                                          lead_raw$location == "Minnesota" &
                                                          lead_raw$indicator== "Blood lead testing" &
                                                          lead_raw$indicator.type == "Test year (annual method)", ]$denominator
                                                 ),
                                          ")"
                                        )

##### Less than 3 years of age Region      
      strAgeLessThanThreeRegion <- if(
                                      #ifelse is required for the times when a value is not provided
                                      ifelse(
                                            #Any checks if any true values exist. is.na() did not work correctly
                                            any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested), 
                                            #If meets criteria and is not null then it will return valid value
                                            lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
                                            0
                                            )
                                      <
                                      #ifelse isn't required for regions since regions will always return a value
                                      unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionTestPct)
                                      )
                                        {" <b>less</b> than the <font color=red>"}
                                  else  {" <b>greater</b> than the <font color=red>"}
      
      strAgeLessThanThreeRegion <- paste0(
                                      strAgeLessThanThreeRegion,
                                      input$par_region, 
                                      "</font> region ",
                                      unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionTestPct),
                                      "% (",
                                      unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionNumTested),
                                      "/",
                                      unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionDenominator),
                                      ")"
                                    )
      
##### Less than 3 years of age CHB      
      strAgeLessThanThreeChb <- if(
                                  #ifelse is required for the times when a value is not provided
                                  ifelse(
                                        #Any checks if any true values exist. is.na() did not work correctly
                                        any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested), 
                                        #If meets criteria and is not null then it will return valid value
                                        lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
                                        0
                                        )
                                  <
                                  #ifelse is required for the times when a value is not provided
                                  ifelse(
                                        #Any checks if any true values exist. is.na() did not work correctly
                                        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct)), 
                                        #If meets criteria and is not null then it will return valid value
                                        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct),
                                        0
                                        )
                                  )
                                      {" <b>less</b> than <font color=red>"}
                                else  { " <b>greater</b> than <font color=red>"}

      strAgeLessThanThreeChb <- paste0(
                                      strAgeLessThanThreeChb,
                                      input$par_chb, 
                                      "</font> ",
                                      ifelse(
                                            #Any checks if any true values exist. is.na() did not work correctly
                                            any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct)), 
                                            #If meets criteria and is not null then it will return valid value
                                            unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct),
                                            0
                                            ),
                                      "% (",
                                      ifelse(
                                            #Any checks if any true values exist. is.na() did not work correctly
                                            any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbNumTested)), 
                                            #If meets criteria and is not null then it will return valid value
                                            unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbNumTested),
                                            0
                                            ),
                                      "/",
                                      ifelse(
                                            #Any checks if any true values exist. is.na() did not work correctly
                                            any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbDenominator)), 
                                            #If meets criteria and is not null then it will return valid value
                                            unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbDenominator),
                                            0
                                            ),
                                      ")"
                                      )

#######################Final Narrative snippet     
      strAgeLessThanThree <- paste0(
                                    "In <font color=red>",
                                    input$par_leadYear,
                                    "</font>, ",
                                    ifelse(
                                          #Any checks if any true values exist. is.na() did not work correctly
                                          any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested), 
                                          #If meets criteria and is not null then it will return valid value
                                          lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
                                          0
                                          ),
                                    "% (",
                                    ifelse(
                                          #Any checks if any true values exist. is.na() did not work correctly
                                          any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$numTested), 
                                          #If meets criteria and is not null then it will return valid value
                                          lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$numTested,
                                          0
                                          ),
                                    "/",
                                    ifelse(
                                          #Any checks if any true values exist. is.na() did not work correctly
                                          any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$denominator), 
                                          #If meets criteria and is not null then it will return valid value
                                          lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$denominator,
                                          0
                                          ),
                                    ") of <font color=red>",
                                    input$par_county, 
                                    "</font> residents <b>under the age of 3 </b>had their blood lead levels tested which is",
                                    if(input$par_stateRegionChb %in% c("All","CHB")){strAgeLessThanThreeChb},
                                    if(input$par_stateRegionChb %in% c("All","Region")){strAgeLessThanThreeRegion},
                                    if(input$par_stateRegionChb %in% c("All","State")){strAgeLessThanThreeState}
                                  )
      
####################################################################################################################################################################################
## 3 to 6 YEARS OF AGE
      
##### 3 to 6 years of age State      
      strAgeThreeToSixState <- if(
                                  #ifelse is required for the times when a value is not provided
                                  ifelse(
                                        #Any checks if any true values exist. is.na() did not work correctly
                                        any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested),
                                        #If meets criteria and is not null then it will return valid value
                                        lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
                                        0
                                        )
                                  <
                                  #ifelse isn't required for state since state will always return a value
                                  #Unique is required because there is a duplicate for Minnesota in 2019. This was brought up to MDH and since there tool is changing in 2024 they are not going to fix error.
                                  unique(
                                         lead_raw[lead_raw$year == input$par_leadYear &
                                         lead_raw$ageGroup == "3-<6 years" &
                                         lead_raw$location == "Minnesota" &
                                         lead_raw$indicator== "Blood lead testing" &
                                         lead_raw$indicator.type == "Test year (annual method)", ]$pctTested
                                        )
                                )
                                      {" <b>less</b> than <font color=red>"}
                                else  {" <b>greater</b> than <font color=red>"}

      strAgeThreeToSixState <- paste0(
                                      strAgeThreeToSixState,
                                      "Minnesota </font> ",
                                      unique(
                                            lead_raw[ lead_raw$year == input$par_leadYear &
                                                      lead_raw$ageGroup == "3-<6 years" &
                                                      lead_raw$location == "Minnesota" &
                                                      lead_raw$indicator== "Blood lead testing" &
                                                      lead_raw$indicator.type == "Test year (annual method)", ]$pctTested
                                            ),
                                      "% (",
                                      unique(
                                            lead_raw[ lead_raw$year == input$par_leadYear &
                                                      lead_raw$ageGroup == "3-<6 years" &
                                                      lead_raw$location == "Minnesota" &
                                                      lead_raw$indicator== "Blood lead testing" &
                                                      lead_raw$indicator.type == "Test year (annual method)", ]$numTested
                                            ),
                                      "/",
                                      unique(
                                            lead_raw[ lead_raw$year == input$par_leadYear &
                                                      lead_raw$ageGroup == "3-<6 years" &
                                                      lead_raw$location == "Minnesota" &
                                                      lead_raw$indicator== "Blood lead testing" &
                                                      lead_raw$indicator.type == "Test year (annual method)", ]$denominator
                                            ),
                                      ")"
                                    )
      
      
##### 3 to 6 years of age Region      
      strAgeThreeToSixRegion <- if(
                                  #ifelse is required for the times when a value is not provided
                                  ifelse(
                                        #Any checks if any true values exist. is.na() did not work correctly
                                        any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested), 
                                        #If meets criteria and is not null then it will return valid value
                                        lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
                                        0
                                        )
                                  <
                                    unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionTestPct)
                                  )
                                        {" <b>less</b> than <font color=red>"}
                                  else  {" <b>greater</b> than <font color=red>"}
      
      strAgeThreeToSixRegion <- paste0(
                                      strAgeThreeToSixRegion,
                                      input$par_region, 
                                      "</font> ",
                                      unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionTestPct),
                                      "% (",
                                      unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionNumTested),
                                      "/",
                                      unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionDenominator),
                                      ")"
                                    )
      
##### 3 to 6 years of age CHB      
      strAgeThreeToSixChb <- if(
                                #ifelse is required for the times when a value is not provided
                                ifelse(
                                      #Any checks if any true values exist. is.na() did not work correctly
                                      any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested), 
                                      #If meets criteria and is not null then it will return valid value
                                      lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
                                      0
                                      )
                                  <
                                #ifelse is required for the times when a value is not provided
                                ifelse(
                                      #Any checks if any true values exist. is.na() did not work correctly
                                      any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct)), 
                                      #If meets criteria and is not null then it will return valid value
                                      unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct),
                                      0
                                      )
                                )
                                    {" <b>less</b> than <font color=red>"}
                              else  {" <b>greater</b> than <font color=red>"}

      strAgeThreeToSixChb <- paste0(
                                  strAgeThreeToSixChb,
                                  input$par_chb,
                                  "</font> ",
                                  ifelse(
                                        #Any checks if any true values exist. is.na() did not work correctly
                                        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct)), 
                                        #If meets criteria and is not null then it will return valid value
                                        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct),
                                        0
                                        ),
                                  "% (",
                                  ifelse(
                                        #Any checks if any true values exist. is.na() did not work correctly
                                        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbNumTested)), 
                                        #If meets criteria and is not null then it will return valid value
                                        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbNumTested),
                                        0
                                        ),
                                  "/",
                                  ifelse(
                                        #Any checks if any true values exist. is.na() did not work correctly
                                        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbDenominator)), 
                                        #If meets criteria and is not null then it will return valid value
                                        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbDenominator),
                                        0
                                        ),
                                  ")"
                                )
      #######################Final Narrative snippet     
      strAgeThreeToSix <- paste0(
                                "In <font color=red>",
                                input$par_leadYear,
                                "</font>, ",
                                ifelse(
                                        #Any checks if any true values exist. is.na did not work correctly
                                        any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested), 
                                        #If meets criteria and is not null then it will return valid value
                                        lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
                                        0
                                      ),
                                "% (",
                                ifelse(
                                      #Any checks if any true values exist. is.na did not work correctly
                                      any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$numTested), 
                                      #If meets criteria and is not null then it will return valid value
                                      lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$numTested,
                                      0
                                      ),
                                "/",
                                ifelse(
                                      #Any checks if any true values exist. is.na did not work correctly
                                      any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$denominator), 
                                      #If meets criteria and is not null then it will return valid value
                                      lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$denominator,
                                      0
                                      ),
                                ") of <font color=red>",
                                input$par_county, 
                                "</font> residents ages <b>3 to 6 </b>had their blood lead levels tested which is",
                                if(input$par_stateRegionChb %in% c("All","CHB")){strAgeThreeToSixChb},
                                if(input$par_stateRegionChb %in% c("All","Region")){strAgeThreeToSixRegion},
                                if(input$par_stateRegionChb %in% c("All","State")){strAgeThreeToSixState}
                              )
####################################################################################################################################################################################
## Less Than 6 YEARS OF AGE
      
      ##### < 6 years of age State      
      strAgeLessThanSixState <- if(
                                    #ifelse is required for the times when a value is not provided
                                    ifelse(
                                          #Any checks if any true values exist. is.na() did not work correctly
                                          any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested),
                                          #If meets criteria and is not null then it will return valid value
                                          lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
                                          0
                                          )
                                    <
                                    #ifelse isn't required for state since state will always return a value
                                    #Unique is required because there is a duplicate for Minnesota in 2019. This was brought up to MDH and since there tool is changing in 2024 they are not going to fix error.
                                    unique(
                                           lead_raw[lead_raw$year == input$par_leadYear &
                                           lead_raw$ageGroup == "<6 years" &
                                           lead_raw$location == "Minnesota" &
                                           lead_raw$indicator== "Blood lead testing" &
                                           lead_raw$indicator.type == "Test year (annual method)", ]$pctTested
                                          )
                                  )
                                        {" <b>less</b> than <font color=red>"}
                                  else  {" <b>greater</b> than <font color=red>"}

      strAgeLessThanSixState <- paste0(
                                      strAgeLessThanSixState,
                                      "Minnesota </font> ",
                                      unique(
                                            lead_raw[ lead_raw$year == input$par_leadYear &
                                                      lead_raw$ageGroup == "<6 years" &
                                                      lead_raw$location == "Minnesota" &
                                                      lead_raw$indicator== "Blood lead testing" &
                                                      lead_raw$indicator.type == "Test year (annual method)", ]$pctTested
                                             ),
                                      "% (",
                                      unique(
                                            lead_raw[ lead_raw$year == input$par_leadYear &
                                                      lead_raw$ageGroup == "<6 years" &
                                                      lead_raw$location == "Minnesota" &
                                                      lead_raw$indicator== "Blood lead testing" &
                                                      lead_raw$indicator.type == "Test year (annual method)", ]$numTested
                                            ),
                                      "/",
                                      unique(
                                            lead_raw[ lead_raw$year == input$par_leadYear &
                                                      lead_raw$ageGroup == "<6 years" &
                                                      lead_raw$location == "Minnesota" &
                                                      lead_raw$indicator== "Blood lead testing" &
                                                      lead_raw$indicator.type == "Test year (annual method)", ]$denominator
                                            ),
                                      ")"
                                    )
      
      ##### < 6 years of age Region      
      strAgeLessThanSixRegion <- if(
                                  #ifelse is required for the times when a value is not provided
                                  ifelse(
                                    #Any checks if any true values exist. is.na() did not work correctly
                                    any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested), 
                                    #If meets criteria and is not null then it will return valid value
                                    lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
                                    0
                                  )
                                  <
                                  unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionTestPct)
                                )
                                      {" <b>less</b> than <font color=red>"}
                                else  {" <b>greater</b> than <font color=red>"}
      
      strAgeLessThanSixRegion <- paste0(
                                        strAgeLessThanSixRegion,
                                        input$par_region, 
                                        "</font> ",
                                        unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionTestPct),
                                        "% (",
                                        unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionNumTested),
                                        "/",
                                        unique(lead_region_sub()[lead_region_sub()$year == input$par_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionDenominator),
                                        ")"
                                      )
      
      ##### < 6 years of age CHB      
      strAgeLessThanSixChb <- if(
                                #ifelse is required for the times when a value is not provided
                                ifelse(
                                      #Any checks if any true values exist. is.na() did not work correctly
                                      any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested), 
                                      #If meets criteria and is not null then it will return valid value
                                      lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
                                      0
                                      )
                                <
                                #ifelse is required for the times when a value is not provided
                                ifelse(
                                      #Any checks if any true values exist. is.na() did not work correctly
                                      any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct)), 
                                      #If meets criteria and is not null then it will return valid value
                                      unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct),
                                      0
                                      )
                                )
                                    {" <b>less</b> than <font color=red>"}
                              else  {" <b>greater</b> than <font color=red>"}
      
      strAgeLessThanSixChb <- paste0(
                                    strAgeLessThanSixChb,
                                    input$par_chb,
                                    "</font> ",
                                    ifelse(
                                          #Any checks if any true values exist. is.na() did not work correctly
                                          any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct)), 
                                          #If meets criteria and is not null then it will return valid value
                                          unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct),
                                          0
                                          ),
                                    "% (",
                                    ifelse(
                                          #Any checks if any true values exist. is.na() did not work correctly
                                          any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbNumTested)), 
                                          #If meets criteria and is not null then it will return valid value
                                          unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbNumTested),
                                          0
                                          ),
                                    "/",
                                    ifelse(
                                          #Any checks if any true values exist. is.na() did not work correctly
                                          any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbDenominator)), 
                                          #If meets criteria and is not null then it will return valid value
                                          unique(lead_CHB_sub()[lead_CHB_sub()$year == input$par_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbDenominator),
                                          0
                                          ),
                                    ")"
                                    )
      #######################Final Narrative snippet     
      strAgeLessThanSix <- paste0(
                                  "In <font color=red>",
                                  input$par_leadYear,
                                  "</font>, ",
                                  ifelse(
                                        #Any checks if any true values exist. is.na did not work correctly
                                        any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested), 
                                        #If meets criteria and is not null then it will return valid value
                                        lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
                                        0
                                        ),
                                  "% (",
                                  ifelse(
                                        #Any checks if any true values exist. is.na did not work correctly
                                        any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$numTested), 
                                        #If meets criteria and is not null then it will return valid value
                                        lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$numTested,
                                        0
                                        ),
                                  "/",
                                  ifelse(
                                        #Any checks if any true values exist. is.na did not work correctly
                                        any(lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$denominator), 
                                        #If meets criteria and is not null then it will return valid value
                                        lead_county_sub()[lead_county_sub()$year == input$par_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$denominator,
                                        0
                                        ),
                                  ") of <font color=red>",
                                  input$par_county, 
                                  "</font> residents <b>under the age of 6 </b> had their blood lead levels tested which is",
                                  if(input$par_stateRegionChb %in% c("All","CHB")){strAgeLessThanSixChb},
                                  if(input$par_stateRegionChb %in% c("All","Region")){strAgeLessThanSixRegion},
                                  if(input$par_stateRegionChb %in% c("All","State")){strAgeLessThanSixState}
                              )
      HTML(paste
                (
                strAgeLessThanThree,
                strAgeThreeToSix,
                strAgeLessThanSix,
                sep= '<br/><br/>' #I wanted two line breaks between each string
                )
           )
      
    })
}

shinyApp(ui=ui, server=server)

