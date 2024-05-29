# Shiny Dashboard
# Hastag (# ---------------------------------------------------------------) means Primary Header
# Hastag (### *** Definition ###) means Secondary Header
# Hold shift alt o to collapse all
# Hold alt o to expand all

# Load Packages ---------------------------------------------------------------
library(shiny)
library(ggplot2)
# shinyLive already downloads the following packages so rather than download them again, I just reference them
#library(dplyr)
#library(shinydashboard)
#Loading shinyjs really slowed down my application load time. By loading just the functions I required such as shnyjs::useShinyjs(), my application ran way faster
#library(shinyjs)

# Data
#source("Data/df_globalSchsacChb.R")

source("./Modules/mod_globalParSchsacChb.R")
source("./Modules/Test.R")

# User Interface ----------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "MN Public Health Data Access Portal",
    titleWidth = 400
    #disable = TRUE #uncomment if the header should be hid
  ),
  shinydashboard::dashboardSidebar(
    width = 350, #This makes the sidebar wider. However, the input boxes seem to have a set dimension resulting in long names still wrapping
    #First sucessful module!
    #What makes modules really neat, is the id can be the same for all modules since it doesn't seem 
    #to get referenced until it is passed to the module, If a module has a ui and server, than it will require the same id
    mod_globalparSchsacChbInputUI(id ="x"),
    
    #Sidebar is required to have sub menus because it requires the tabName to reference
    #By having sidebarMenu id, I can reference it and hide other filters with shinyjs 
    shinydashboard::sidebarMenu(
      id= "smID",
      shinydashboard::menuItem("Home", tabName = "tn_homePage"),
      shinydashboard::menuItem("Region & CHB Defintions", tabName = "tn_regionChbDefinations"),
      shinydashboard::menuItem("Child Health", tabName = "tn_childHealth"),
      shinydashboard::menuItem("Climate", tabName = "tb_climate"),
      shinydashboard::menuItem("Diseases & Conditions", tabName = "tn_diseasesConditions"),
      shinydashboard::menuItem("Environmental Health", tabName = "tn_environmepntalHealth"),
      shinydashboard::menuItem("Health Behaviors/Risk Factors", tabName = "tn_healthBehaviorsRiskFactors"),
      shinydashboard::menuItem("Health Equity", tabName = "tn_HealthEquity"),
      shinydashboard::menuItem("Healthy Homes", tabName = "tn_healthyHomes")
    )
  ),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(), #Thank you Abby Stamm at MDH for suggesting to only call one function in a package rather then load entire package
    shinydashboard::tabItems(
      shinydashboard::tabItem(
                 tabName = "tn_homePage", #tabName is what ties the menuItem to the tabItem
                 tabsetPanel(
                   tabPanel(
                     "Home Page",
                     fluidRow(
                       # Narrative section explaining the purpose of the dashboard
                       column(
                         width = 12,
                         tags$h1("Welcome to the Landing Page for This Awesome Dashboard...Subjective of Course :)"),
                         tags$h4("This shinyLive application is designed to replicate the work represeted ",
                                 tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target= "_blank")
                         ),
                         tags$h4("The features displayed on the top left are called parameters that require input from you"),
                         tags$h4("The features displayed on the botton left are called sidebar menus."),
                         tags$h4("Each sidebar menu will have at least one tab associated with it."),
                         tags$h4("For example, this Home sidebar menu has the Home Page tab associated with it."),
                         tags$h3("Currently, only two sidebar menus currently are working"),
                         tags$h4("The Region & CHB Defintions sidebar menu has both tabs (Region and CHB) working"),
                         tags$h4("The Healthy Homes sidebar menu only has the Childhood Lead Exposure tab working"),
                         tags$h4("Limitations to the Childhood Lead Exposure data can be found ",
                                 tags$a(href="https://data.web.health.state.mn.us/lead_metadata", "here!", target= "_blank")
                         )
                       )
                     )
                   )
                 )
               ),
      shinydashboard::tabItem(
                 tabName = "tn_regionChbDefinations",
                 tabsetPanel( type = "tabs", 
                   tabPanel(
                     "Region",
                     fluidRow(
                       # Narrative section explaining the dashboard purpose
                       column(
                         width = 12,
                         h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Regions will remain in <b>bold</b>.")),
                         h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."),
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
                         h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they don't have any functions on this page."),
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
      shinydashboard::tabItem(
                 tabName = "tn_childHealth",
                 tabsetPanel(
                   tabPanel("Asthma"),
                   tabPanel("Health Inequities in Childhood Asthma"),
                   tabPanel("Birth Defects"),
                   tabPanel("Childhood Lead Exposure"),
                   tabPanel("Free/Reduced Price Lunch Eligibility"),
                   tabPanel("Immunizations"),
                   tabPanel("Oral Health")
                 )
               ),
      shinydashboard::tabItem(
                 tabName = "tb_climate",
                 tabsetPanel(
                   tabPanel("Air Quality"),
                   tabPanel("Cold-related Illness"),
                   tabPanel("tbClimate-related Environmental Health Concerns"),
                   tabPanel("Heat-related Illness"),
                   tabPanel("Hot Weather"),
                   tabPanel("Pollen")
                 )
               ),
      shinydashboard::tabItem(
                 tabName = "tn_diseasesConditions",
                 tabsetPanel(
                   tabPanel("Asthma"),
                   tabPanel("Birth Defects"),
                   tabPanel("Cancer"),
                   tabPanel("Carbon Monoxide (CO) Poisoning"),
                   tabPanel("Chronic Obstructive Pulmonary Disease (COPD)"),
                   tabPanel("Diabetes"),
                   tabPanel("Heart Attacks"),
                   tabPanel("Heat-related Illness"),
                   tabPanel("Immunizations"),
                   tabPanel("Oral Health")
                 )
               ),
      shinydashboard::tabItem(
                 tabName = "tn_environmentalHealth",
                 tabsetPanel(
                   tabPanel("Air Quality"),
                   tabPanel("Biomonitoring: Chemicals in people"),
                   tabPanel("Cold-related Illness"),
                   tabPanel("Drinking Water Quality"),
                   tabPanel("Environmental Justice"),
                   tabPanel("Heat-related Illness"),
                   tabPanel("Pesticide Poisoning"),
                   tabPanel("Traffic")
                 )
               ),
      shinydashboard::tabItem(
                 tabName = "tn_healthBehaviorsRiskFactors",
                 tabsetPanel(
                   tabPanel("Health Insurance"),
                   tabPanel("Immunizations"),
                   tabPanel("Obesity"),
                   tabPanel("Oral Health"),
                   tabPanel("Poverty & Income"),
                   tabPanel("Smoking")
                 )
               ),
      shinydashboard::tabItem(
                 tabName = "tn_HealthEquity",
                 tabsetPanel(
                   tabPanel("Health Equity"),
                   tabPanel("Health Inequities in Childhood Lead Exposure"),
                   tabPanel("Health Inequities in Childhood Asthma")
                 )
               ),
      shinydashboard::tabItem(
                 tabName = "tn_healthyHomes",
                 tabsetPanel(
                   id="tpId_healthyHomes",
                   tabPanel("Carbon Monoxide (CO) Poisoning"),
                   tabPanel("Childhood Lead Exposure",
                            mod_localChildhoodLeadExposureUI("x")
                            # 
                            # fluidRow(
                            # 
                            #   # Narrative section explaining the purpose of the dashboard
                            #   column(
                            #     width = 12,
                            #     id = "cID_leadNarrative",
                            #     tags$h4(htmlOutput("lead_narrative")),
                            #     tags$h3("For information about why this data is important please visit ",
                            #             tags$a(href="https://data.web.health.state.mn.us/lead", "here!", target= "_blank")
                            #     ),
                            #     tags$h3("Data for this project can be found ",
                            #             tags$a(href="https://data.web.health.state.mn.us/lead_query", "here!", target= "_blank")
                            #     ),
                            #     tags$h3("Data for this project is stored ",
                            #             tags$a(href="https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20Public%20Health%20Data%20Access%20Portal/Healthy%20Homes/Childhood%20Lead%20Exposure.csv",
                            #                    "here!", target= "_blank")
                            #     )
                            #   )
                            # ),
                            # fluidRow(
                            #   #The next line inserts a line between the narrative and the data.
                            #   #It is added here instead of in the narrative since the narrative can have multiple columns.
                            #   #If there are multiple narrative columns and the line is added there than the line consists of multiple breaks
                            #   tags$hr(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;"),
                            #   box(plotOutput("lead_state")),
                            #   box(plotOutput("lead_region"))
                            # ),
                            # fluidRow(
                            #   box(plotOutput("lead_chb")),
                            #   box(plotOutput("lead_county"))
                            # )
                   ),
                   tabPanel("Drinking Water Quality"),
                   tabPanel("Pesticide Poisoning"),
                   tabPanel("Radon")
                 )
               )
             )
     )
  )
  
  server <- function(input, output, session) {
    
    mod_globalparSchsacChbInputServer(id= "x")
   
    
    globalReactiveCountySchsacChb <- mod_globalparSchsacChbInputServer(id= "x")
    
     # Pass the reactive values to another module
    otherModuleServer(id= "x", 
                      globalReactiveCountySchsacChb$selectedCounty, 
                      globalReactiveCountySchsacChb$selectedRegion, 
                      globalReactiveCountySchsacChb$selectedChb
                      )
  
 # childhoodLeadExposureServer(df_leadUserChoice)
    
    mod_localChildhoodLeadExposureServer("x")
  
  }
  shinyApp(ui=ui, server=server)
  
  