#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinysurveys)
#devtools::load_all("~/shinysurveys/")
library(dplyr)
library(googledrive)
library(googlesheets4)

qs <- readRDS("qs.rds")
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "emilliman4@gmail.com")
gs4_auth(token = drive_token())

# Define UI for application that draws a histogram
ui <- navbarPage("riskmetric Survey", id= "inTabset",
        tabPanel("Introduction",
                 h2("Introduction"),
                 h5("The ",a("riskmetric package", href = "https://pharmar.github.io/riskmetric/index.html"), 
                 " provides a workflow to evaluate the quality of a set of R packages 
                   that involves five major steps. The workflow can help users to choose high quality 
                   R packages, improve package reliability and prove the validity of R packages in a 
                   regulated industry."),
                 br(),
                 h5("One issue with risk is its need for context. For example, a package used for a class 
                   project is of lower risk than the same package being used to determine if a new therapy is effective."),
                 br(),
                 h5("Furthermore, weighting of package assessments which make up a package's risk score have 
                    been done arbitrailiy or not at all. Enter this survey. We hope that by crowd sourcing 
                   a risk rating on a number of packages will allow us to empirically determine weights for each 
                   assessment used in the riskmetric package."),
                 h2("Instructions"),
                 h5("Please rate the following packages based on your preceived risk of using each package in a 
                   regulatory finding."), 
                 br(),
                 h5("The risk of a package is the combination of its quality and its criticality in a users workflow."),
                 h2("Data Collection:"),
                 h5("We will only collect and store the data collected on the form. 
                   Each user's submission will be uniquely identified by a random string generated when you submit the form.
                   We do not collect any other information from your visit to this survey. 
                   The riskmetric team plans to release all the collected data with the riskmetric app"),
        actionButton("jumpToP1", "Start the survey")),
        tabPanel("Survey", value="survey",
            uiOutput("survey")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$survey <- renderUI({
        surveyOutput(qs[c(1:18,sample(19:nrow(qs), 15, FALSE)), ],
                     survey_title = "Community-based risk assessment of select packages",
                     survey_description = "Please rate each package on a scale from 1 to 10; 
                     with 1 being low risk and 10 being high risk")
    })
    
    observeEvent(input$jumpToP1, {
        updateTabsetPanel(session, "inTabset",
                          selected = "survey")
    })
    
    renderSurvey()
    
    observeEvent(input$submit, {
        response_data <- getSurveyData()
        response_data$subject_id <- gsub("/", "", tempfile(pattern = "user", tmpdir = ""))
        response_data$date <- Sys.Date()
        #print(response_data)
        sheet_append("https://docs.google.com/spreadsheets/d/1Orw5uWFqLxAv_eronJlKonLSy0KyJLV8QwApMo5pi-g", response_data)
        showModal(modalDialog(
            title = "Thank you for completeing this survey", 
        ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
