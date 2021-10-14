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
ui <- fluidPage(

    # Application title
    #titlePanel("Survey of R package risk"),
    #actionButton("begin", "Click to Begin"),
    # surveyOutput(df,
    #              survey_title = "Please rate the following packages based 
    #                    on your preception of risk to your oganization's work"),
    uiOutput("survey")
    # surveyOutput(df,
    #              survey_title = "Please rate the following packages based on your preception of risk to your oganization's work")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$survey <- renderUI({
        surveyOutput(qs[c(1,sample(2:nrow(qs), 15, FALSE)), ],
                     survey_title = "Please rate the following packages based on your preception of risk to your oganization's work")
    })
    
    renderSurvey()
    
    observeEvent(input$submit, {
        response_data <- getSurveyData()
        response_data$subject_id <- gsub("/", "", tempfile(pattern = "user", tmpdir = ""))
        #print(response_data)
        sheet_append("https://docs.google.com/spreadsheets/d/1Orw5uWFqLxAv_eronJlKonLSy0KyJLV8QwApMo5pi-g", response_data)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
