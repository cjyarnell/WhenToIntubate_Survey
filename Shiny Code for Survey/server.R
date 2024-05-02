#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

######################################################
# Libraries

# install.packages("devtools")
# devtools::install_github("CannaData/shinyCAPTCHA")
# devtools::install_github("thewileylab/shinyREDCap")
library(shinyCAPTCHA)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(scroller)
library(redcapAPI)
library(readxl)

source("translate.R")
source("country.R")

########## Read language table ############################

#read_xlsx("Language_Key.xlsx") %>%
#  saveRDS("Language_Key.rds")

lk <- readRDS("Language_Key.rds")

# set language

language <- "English"

countries <- country_by_language(language)

###########ReCAPTCHA function##############################

# adjust function to remove submit button

recaptchaUI <- function(id, sitekey = Sys.getenv("recaptcha_sitekey"), ...) {
  ns <- NS(id)
  
  tagList(tags$div(
    shiny::tags$script(
      src = "https://www.google.com/recaptcha/api.js",
      async = NA,
      defer = NA
    ),
    tags$script(
      paste0("shinyCaptcha = function(response) {
          Shiny.onInputChange('", ns("recaptcha_response"),"', response);
      }"
      )),
    tags$form(
      class = "shinyCAPTCHA-form",
      action = "?",
      method = "POST",
      tags$div(class = "g-recaptcha", `data-sitekey` = sitekey, `data-callback` = I("shinyCaptcha")),
      tags$br()
      #,tags$input(type = "radio", ...)
    )
  ))
}

######################################################
# countries

flag_widget <- pickerInput(
  inputId = "Country",
  label = translate("DemoQ_Country", language, lk),
  selected = countries$default,
  choices = countries$countries,
  options = list(
    `live-search` = TRUE))

################random string function#######################################

randstring <- function(n=1, length=12){
  randomString <- c(1:n)
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),length, replace=TRUE), collapse="")
  }
  return(randomString)
}

############Survey Variables##################

survey_variables <- list(diagnosis = c(translate("Diagnosis_CAP", language, lk),
                                       translate("Diagnosis_CAP", language, lk),
                                       translate("Diagnosis_COVID", language, lk),
                                       translate("Diagnosis_COVID", language, lk),
                                       translate("Diagnosis_Flu", language, lk),
                                       translate("Diagnosis_Flu", language, lk),
                                       translate("Diagnosis_Pancreatitis", language, lk),
                                       translate("Diagnosis_Sepsis", language, lk),
                                       translate("Diagnosis_Sepsis", language, lk)),
                         age = as.character(c(20:60, 40:70, 55:65)),
                      frailty = c(translate("Frailty_Independent", language, lk), # CFS 1-2
                                  translate("Frailty_IndependentComorbid", language, lk), # CFS 3
                                  translate("Frailty_IndependentComorbid", language, lk), # CFS 3
                                  translate("Frailty_Assistance", language, lk)#, # CFS 5
                                  #"Assistance for housework and bathing", #CFS 6
                                  #"Dependent for personal care" # CFS 7+
                      ),
                      spo2 = as.character(c(85:97, 88:92, 90:97)),
                      fio2 = c("0.4",
                               "0.5",
                               "0.6",
                               "0.7",
                               "0.8",
                               "0.9",
                               "1.0"),
                      o2_device = c(translate("O2_Device_HFNC", language, lk),
                                    translate("O2_Device_NIV", language, lk)),
                      rr = as.character(c(15:30, 20:40, 20:35)),
                      #HR = c("90"),
                      work_of_breathing = c(translate("WoB_none", language, lk),
                                            translate("WoB_none", language, lk),
                                            translate("WoB_neck", language, lk),
                                            translate("WoB_neck", language, lk),
                                            translate("WoB_neck_abdo", language, lk)),
                      #BP = c("100/55"),
                      norepinephrine = c(rep(translate("Pressor_No", language, lk),4),
                                         translate("Pressor_Yes", language, lk)),
                      loc = c(translate("LOC_AO", language, lk),
                              translate("LOC_AO", language, lk),
                              translate("LOC_DO", language, lk),
                              translate("LOC_DNO", language, lk)),
                      #Flow = c("60"),
                      duration = c(translate("Duration_10min", language, lk),
                                   translate("Duration_30min", language, lk),
                                   translate("Duration_1hr", language, lk),
                                   translate("Duration_2hr", language, lk),
                                   translate("Duration_4hr", language, lk)))

AdditionalInfoVariableNames <- 
  c(translate("Add_Info_ABG", language, lk),
    translate("Add_Info_PEEP", language, lk),
    translate("Add_Info_Vt", language, lk),
    translate("Add_Info_CXR", language, lk),
    translate("Add_Info_EsoP", language, lk),
    translate("Add_Info_MoreTime", language, lk))

SpecialtyVariableNames <- c(
  translate("DemoQ_Practice_ICU", language, lk),
  translate("DemoQ_Practice_Anesth", language, lk),
  translate("DemoQ_Practice_EM", language, lk),
  translate("DemoQ_Practice_Other", language, lk)
)

SpecialtyVariableNames_rc <- 
  c("specialty_ccm",
    "specialty_anesthesia",
    "specialty_em",
    "specialty_other")

AdditionalInfoVariableNames_rc <- 
  c("arterial_blood_gas",
    "peep_if_on_niv",
    "tidal_volume_if_on_niv",
    "chest_x_ray",
    "esophageal_pressure",
    "more_observation_time")


############generate a set of covariates for a question############
generate_question_covariates <- 
  function(survey_variables, n=1){
    qc <- data.frame(t(sapply(survey_variables, sample, n)))
    if(qc$age < 40){
      qc$frailty <- translate("Frailty_Independent", language, lk)}
    if(qc$age < 55 & (qc$frailty == translate("Frailty_Assistance", language, lk))){
      qc$frailty <- translate("Frailty_IndependentComorbid", language, lk)}
    if(qc$diagnosis == translate("Diagnosis_COVID", language, lk)){
      qc$norepinephrine <- translate("Pressor_No", language, lk)}
    if(qc$diagnosis == translate("Diagnosis_Sepsis", language, lk)){
      qc$norepinephrine <- sample(c(translate("Pressor_No", language, lk),
                                             translate("Pressor_Yes", language, lk)),
                                  size = 1,
                                  prob = c(0.2, 0.8))}
    qc
  }

############generate table-style output####################
generate_question_tableoutput <- function(qc){
  temp <- data.frame(t(qc)) %>%
    rownames_to_column()
  names(temp) <- c("Variable","Value")
  temp$Variable <- c(translate("Diagnosis", language, lk),
                     translate("Age", language, lk),
                     translate("Frailty", language, lk),
                     translate("SpO2", language, lk),
                     translate("FiO2", language, lk),
                     translate("O2_Device", language, lk),
                     translate("Resp_Rate", language, lk),
                     translate("WoB", language, lk),
                     translate("Pressor", language, lk),
                     translate("LOC", language, lk),
                     translate("Duration_State", language, lk))
  temp
}

############ function to write results to REDCap ##############

# REDCap API Token - redacted -

redcap_token <-  ### insert token here, or better, use a more secure method 
redcap_url <- ### URL for your REDCap implementation

# REDCap sheet has to have the same columns as the table that you are sending to it
  
write_to_redcap <- function(results, vals, redcap_url, redcap_token){
  results = results[vals$count-2,]
  importRecords(redcapConnection(redcap_url, redcap_token), results, force_auto_number = TRUE)
}

############################################
# Define server logic 
function(input, output, session) {
  
  #recaptcha
  reCAPTCHA <- callModule(recaptcha, "test", secret = ###)
  
  
  # generate id for the session
  id <- randstring()
  
  # click counter 
  vals <- reactiveValues(count = 0)
  
  # results dataframe
  results <- as.data.frame(matrix(data = NA,
                                  ncol = 1+ 5 + 2 + 1 + 
                                    length(SpecialtyVariableNames) + 
                                    length(survey_variables) + 
                                    length(AdditionalInfoVariableNames) + 2,
                                  nrow = 100))
  
 resultNames <- c("record_id",
                  "id",
                  "clinician",
                  "role",
                  SpecialtyVariableNames_rc,
                  "specialty_other_text",
                  "duration_practice",
                  "country",
                  names(survey_variables),
                  "response",
                  AdditionalInfoVariableNames_rc,
                  "additional_info_other",
                  "question_response_complete",
                  "datetime")
  
 names(results) <- resultNames
  
 output$myList <- renderUI(HTML(
   "<ul><li> Assume that the patient will agree to intubation if you think clinically indicated </li><li> Assume that the pH is not less than 7.25 </li></ul>"
#   "<ul><li> Assume that the pH is not less than 7.25 </li></ul>"
 ))

  observeEvent(input$Click.Counter, {
    if((vals$count > 0) &
       (reCAPTCHA()$success == FALSE |
       length(input$Clinician) == 0 |
       length(input$Specialty) == 0 |
       length(input$Role) == 0 |
       length(input$DurationPractice) == 0)){
      showModal(modalDialog(title = translate("GentleReminder", language, lk),
                            translate("GentleReminderText", language, lk),
                            fade = FALSE))
      } else {

    
    vals$count <- vals$count+1

    if(vals$count > 0){
    
    qc <- generate_question_covariates(survey_variables)
    
    if (vals$count > 1){
      try(results[vals$count - 1,1:(11+length(qc))] <<- 
            c(1,
              id,
              input$Clinician,
              input$Role,
              (SpecialtyVariableNames[1] %in% input$Specialty),
              (SpecialtyVariableNames[2] %in% input$Specialty),
              (SpecialtyVariableNames[3] %in% input$Specialty),
              (SpecialtyVariableNames[4] %in% input$Specialty),
              input$SpecialtyText,
              input$DurationPractice,
              input$Country,
              qc))
      
       }
    
    
    output$MainAction <- renderUI( {
      dynamicUi()
    })
    
    # for keeping track of how many scenarios answered
    output$numberAnswered <- renderUI( {dynamicAnswered()})
    
    dynamicAnswered <- reactive({p(strong(paste0(translate("Sidebar_Scenarios", language, lk),
             " ", max(vals$count-2, 0), ".")))})
    
    # for table display
    output$table <- renderTable(generate_question_tableoutput(qc),
                                striped = T)
    
    
    # the main output user interface
        dynamicUi <- reactive({
          

          # message at end of survey
      if (vals$count > 11){
          showModal(modalDialog(title = paste(translate("Ending_Thankyou", language, lk),
                                               vals$count-2, translate("Ending_Questions", language, lk),
                                              sep = " "),
                                fade = FALSE))
          Sys.sleep(30)
          stopApp()
      } else if 
          # Demographic survey
          (vals$count <= 1){
        list(
          h4(translate("Sidebar_Thankyou", language, lk)),
          radioButtons(
            "Clinician",
            translate("DemoQ_Clinician", language, lk),
            choices = c(translate("DemoQ_Clinician_Yes", language, lk),
                        translate("DemoQ_Clinician_No", language, lk)),
            selected = character(0),
            inline = FALSE,
            width = NULL,
            choiceNames = NULL,
            choiceValues = NULL
          ),
          radioButtons(
            "Role",
            translate("DemoQ_Role", language, lk),
            choices = c(translate("DemoQ_Role_Attending", language, lk),
                        translate("DemoQ_Role_Trainee", language, lk),
                        translate("DemoQ_Role_Nurse", language, lk),
                        translate("DemoQ_Role_RT", language, lk),
                        translate("DemoQ_Role_OtherClinical", language, lk),
                        translate("DemoQ_Role_NonClinical", language, lk)),
            selected = character(0),
            inline = FALSE,
            width = NULL,
            choiceNames = NULL,
            choiceValues = NULL
          ),
          checkboxGroupButtons(
            inputId = "Specialty",
            label = translate("DemoQ_Practice", language, lk),
            choices = SpecialtyVariableNames,
            direction = "vertical"
          ),
          textInput(inputId = "SpecialtyText", 
                    label = "", 
                    value = "", width = NULL,
                    placeholder = translate("DemoQ_Practice_Othertext", language, lk)),
          numericInput(
            "DurationPractice",
            translate("DemoQ_Duration", language, lk),
            value = 5,
            min = 0, max = 50
          ), 
          flag_widget,
          recaptchaUI("test", sitekey = "6LcOEo8nAAAAAPUSVjjUnDjUF02sXtDyWMKEu3wo")
          
          # radioButtons(
          #   "Country",
          #   "In what country/region do you practice?",
          #   choices = c("Canada",
          #               "USA",
          #               "Central and South America",
          #               "Europe",
          #               "United Kingdom and Ireland",
          #               "Africa",
          #               "Western and Central Asia",
          #               "Southern Asia",
          #               "South-eastern Asia",
          #               "Eastern Asia",
          #               "Australia and New Zealand",
          #               "N/A"),
          #   selected = NULL,
          #   inline = FALSE,
          #   width = NULL,
          #   choiceNames = NULL,
          #   choiceValues = NULL
          # )
)
      } else {
        
        list(h4(translate("ResponseQ", language, lk),),
             tableOutput("table"),
             br(),
             sliderTextInput(
               inputId = "response",
               label = "", 
               grid = TRUE,
               force_edges = TRUE,
               width = "450px",
               choices = c(translate("ResponseQ_DefNo", language, lk), 
                           translate("ResponseQ_ProbNo", language, lk),
                           translate("ResponseQ_Uncertain", language, lk), 
                           translate("ResponseQ_ProbYes", language, lk),
                           translate("ResponseQ_DefYes", language, lk)),
               selected = translate("ResponseQ_Uncertain", language, lk)
             ),
             checkboxGroupButtons(
               inputId = "AdditionalInfoCheck",
               label = translate("AdditionalInfoQ", language, lk), 
               choices = AdditionalInfoVariableNames,
               direction = "vertical"
             ),
             textInput(inputId = "additionalInfoOther", 
                       label = translate("AdditionalInfoOtherQ", language, lk), 
                       value = "", width = NULL,
                       placeholder = NULL)
        )
      }
      
      
    })
        if (vals$count > 2){
          # write input data to the last columns
          try(results[vals$count - 2,
                      (ncol(results) - (3+length(AdditionalInfoVariableNames))):ncol(results)] <- 
                c(input$response, 
                  (AdditionalInfoVariableNames[1] %in% input$AdditionalInfoCheck),
                  (AdditionalInfoVariableNames[2] %in% input$AdditionalInfoCheck),
                  (AdditionalInfoVariableNames[3] %in% input$AdditionalInfoCheck),
                  (AdditionalInfoVariableNames[4] %in% input$AdditionalInfoCheck),
                  (AdditionalInfoVariableNames[5] %in% input$AdditionalInfoCheck),
                  (AdditionalInfoVariableNames[6] %in% input$AdditionalInfoCheck),
                  input$additionalInfoOther, "Complete", as.character(Sys.time())))}
       if(vals$count > 2) {write_to_redcap(results, vals, redcap_url, redcap_token)}
    }
   }
  })
  


  

  
 
  
   
    sliderValues <- reactive({
      
      data.frame(
        Name = c("Response"),
        Value = input$response)
      
    })
    
    output$values <- renderUI({
      sliderValues()
    })

}


