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

riskmetric_link <- a("riskmetric package", href = "https://pharmar.github.io/riskmetric/index.html", target="_blank")

ui <- navbarPage(
    "riskmetric Survey",

    id = "inTabset",

    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),

    tabPanel(
        "Introduction",

        div(
            class = "text",

            h2("Introduction"),

            p("The ", riskmetric_link, "provides a workflow to evaluate the quality
        of (a set) of R packages which involves five major steps. The workflow
        can help users to choose high quality R packages, improve package
        reliability and prove the validity of R packages in a regulated
          industry."),

            p("One issue with risk is its need for context. For example, a package used for a class
                   project is of lower risk than the same package being used to determine if a new therapy is effective.
                   The risk of a package is the combination of its quality and its criticality in a user's workflow."),

            p("Furthermore, weighting of package metrics (i.e. an assessment score) which make up a package's summarized risk score have
                    been done arbitrability or not at all. Enter this survey. We hope that by crowd sourcing
                   a risk rating on a number of packages will allow us to empirically determine weights for each
                   package metric used in the riskmetric package."),
            br(),
            h2("Instructions"),
            p("Please rate the packages on the next tab, based on your perceived risk of using each package in
                    your environment. Your environment could be your desktop, RStudio cloud, a company server (GxP or non-GxP), etc.
                    The survey randomly selects 15 packages from the 500-ish most downloaded packages on CRAN for you to rate. You are welcome
                    to revisit survey to rate more packages if you like, however, there is a finite chance of being asked about the same packages
                    over and over due to the anonymity of the survey."),
            br(),
            h2("Data Collection and Privacy"),
            p("We will only collect and store the data submitted on the form.
                 You are free to skip any questions you are not comfortable answering.
                   Each user's submission will be uniquely identified by a random string generated when you submit the form for aggregation and modeling.
                   The riskmetric team plans to release all the collected data with the riskmetric package so that other developers
                    and users can empirically derive their own package metric weights for summarization."),
            br(),
            div(style = "padding-bottom: 10px; padding-left: 60px; padding-right: 60px",
                actionButton("jumpToP1", "Start the survey")))),
    tabPanel("Survey", value="survey", uiOutput("survey")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    renderSurvey()

    output$survey <- renderUI({
        qs_idx <- sample(x = unique(qs$question)[-c(1:4)], size = 15, replace = F)
        qs_idx <- which(qs$question %in% qs_idx)
        surveyOutput(qs[c(1:18,qs_idx), ],
                     survey_title = "Crowd-sourced risk assessment of select packages",
                     survey_description = "Please rate each package on a scale from 1 to 5;
                     with 1 being low risk and 5 being high risk. If you are unfamiliar with
                     the package you may mark NA. All responses are anonymous.")
    })

    observeEvent(input$jumpToP1, {
        updateTabsetPanel(session, "inTabset",
                          selected = "survey")
    })

    observeEvent(input$submit, {
        response_data <- getSurveyData()
        response_data$subject_id <- gsub("/", "", tempfile(pattern = "user", tmpdir = ""))
        response_data$date <- Sys.Date()
        #print(response_data)
        sheet_append("https://docs.google.com/spreadsheets/d/1Orw5uWFqLxAv_eronJlKonLSy0KyJLV8QwApMo5pi-g", response_data)
        showModal(modalDialog(
            title = "Thank you for completing this survey",
        ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
