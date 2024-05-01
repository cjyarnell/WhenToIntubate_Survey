#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
# install.packages("remotes")
#remotes::install_github("lgnbhl/scroller")
library(scroller)
library(readxl)

source("translate.R")

########## Read language table ############################

lk <- readRDS("Language_Key.rds")

# set language

language <- "English"

# define UI

fluidPage(
  scroller::use_scroller(),
    # Application title
    titlePanel( translate("Title", language, lk),
              windowTitle=translate("Title", language, lk)),
  
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          p(translate("ThankYou", language, lk)),
          p(em(translate("REB", language, lk))),
          actionButton(inputId='English', label=translate("Language", "English",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate/';"),
          actionButton(inputId='Spanish', label=translate("Language", "Spanish",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Espanol/';"),
          actionButton(inputId='French', label=translate("Language", "French",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Francais/';"),
          actionButton(inputId='Portuguese', label=translate("Language", "Portuguese",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Portuguese/';"),
          actionButton(inputId='German', label=translate("Language", "German",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Deutsch/';"),
          actionButton(inputId='Italian', label=translate("Language", "Italian",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Italiano/';"),
          actionButton(inputId='Mandarin', label=translate("Language", "Chinese",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Chinese/';"),
          actionButton(inputId='Japanese', label=translate("Language", "Japanese",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Nihongo/';"),
          actionButton(inputId='Thai', label=translate("Language", "Thai",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_Thai/';"),
          actionButton(inputId='Indonesian', label=translate("Language", "Indonesian",lk),
                       style = "display: inline-block;",
                       onclick ="location.href='https://cyarnell.shinyapps.io/WhenToIntubate_BahasaIndonesia/';"),
          
          p(strong(paste0(translate("Instructions", language, lk), ":"))),
          p(translate("Step1", language, lk)),
          p(translate("Step2", language, lk)),
     #     uiOutput("myList"),
          p(translate("Step3", language, lk)),
          p(translate("Step4", language, lk)),
          p(em(translate("Email", language, lk))),
          uiOutput("numberAnswered")
     )
        ,

        # Survey
        mainPanel(
          uiOutput("MainAction"),
          actionButton("Click.Counter", translate("NextButton", language, lk)),
          br(""),
          br("")
        )
     ),
     hr(),
  p(translate("Disclaimer", language, lk))
  
)
