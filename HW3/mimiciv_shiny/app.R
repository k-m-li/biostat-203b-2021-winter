#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# The app should provide easy access to the graphical and numerical summaries of variables (demographics, lab measurements, vitals) in the ICU cohort.
#--------
# TODO: comparison of patient vitals + lab measurements in patients with similar demographics
# demographics of interest: gender, tbl_age, ethnicity, language, marital_status, insurance, death30
# lab measurements of interest: bicarbonate, calcium, chloride, creatinine, glucose, magnesium, potassium, sodium, hematocrit
# vitals of interest: heart_rate, non_invasive_blood_pressure_systolic, non_invasive_blood_pressure_mean, respiratory_rate, 
#                     temperature_fahrenheit, arterial_blood_pressure_systolic, arterial_blood_pressure_mean
#
#
# LINK: https://kmli.shinyapps.io/mimiciv_shiny/

library(shiny)
library(ggplot2)
library(tidyverse)
library(pastecs)

#Reading in icu set
icu_cohort <- readRDS("icu_cohort.rds")

#Making tibble for demographic of interest
demtable <- icu_cohort %>% 
    select(subject_id, gender, tbl_age, ethnicity, language, marital_status, insurance, death30)
colnames(demtable) <- c("Subject ID",
                        "Gender",
                        "Age (years)",
                        "Ethnicity",
                        "Language",
                        "Marital status",
                        "Insurance",
                        "Death within 30 days of discharge")

#Making tibble for lab measurements of interest
labtable <- icu_cohort %>%
    select(subject_id, bicarbonate, calcium, chloride, 
           creatinine, glucose, magnesium, potassium, 
           sodium, hematocrit)
colnames(labtable) <- c("Subject ID",
                        "Bicarbonate",
                        "Calcium",
                        "Chloride",
                        "Creatinine",
                        "Glucose",
                        "Magnesium",
                        "Potassium",
                        "Sodium",
                        "Hematocrit")
#Making tibble for vitals of interest 
vittable <- icu_cohort %>%
    select(subject_id, heart_rate, non_invasive_blood_pressure_systolic, 
           non_invasive_blood_pressure_mean, respiratory_rate, 
           temperature_fahrenheit, arterial_blood_pressure_systolic, 
           arterial_blood_pressure_mean)
colnames(vittable) <- c("Subject ID",
                        "Heart rate",
                        "Non-invasive systolic BP",
                        "Mean non-invasive BP",
                        "Respiratory rate",
                        "Temperature (Fahrenheit)",
                        "Systolic arterial BP",
                        "Mean arterial BP")

# UI
ui <- fluidPage(
    headerPanel("MIMIC-IV Patient Data"),
    
    titlePanel("Patient Demographics"),

    # Numeric selection for row number
    numericInput("demnum",
                 label = "Number of rows (max = 50,048)",
                 10,
                 min = 1,
                 max = nrow(icu_cohort)),
    #Table output
    tableOutput("demTable"),
    
    #Drop-down selection for demographics
    selectInput("demchoice",
                label = "Demographic to plot",
                choices = colnames(demtable[,-1])),
    
    #graph output
    plotOutput("demGraph"),
    
    titlePanel("Lab Measurements"),
    
    #sidebar of measurement options
    sidebarLayout(
        
        sidebarPanel(
            radioButtons("labchoice", 
                         label = "Lab Measurement:",
                         choices = colnames(labtable[, -1])),
            numericInput("labnumber",
                         label = "Number of observations (max = 50,048)",
                         value = 25000,
                         min = 1,
                         max = nrow(labtable))
        ),
        
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Graph", plotOutput("labGraph"),),
                    tabPanel("Summary Statistics", verbatimTextOutput("labSumm")),
                    tabPanel("Raw data", 
                             numericInput("labTableN",
                                          label = "Number of rows (max = 50,048)",
                                          10,
                                          min = 1,
                                          max = nrow(labtable)),
                             checkboxGroupInput("labTablechoice",
                                                label = "Lab measurements",
                                                choices = colnames(labtable),
                                                inline = TRUE,
                                                selected = c("Subject ID")
                                                ),
                             tableOutput("labTable")))
        )     
    ),
    
    titlePanel("Vitals"),
    
    #sidebar of measurement options
    sidebarLayout(
        
        sidebarPanel(
            radioButtons("vitchoice", 
                         label = "Vital Measurement:",
                         choices = colnames(vittable[, -1])),
            numericInput("vitnumber",
                         label = "Number of observations (max = 50,048)",
                         value = 25000,
                         min = 1,
                         max = nrow(labtable))
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Graph", plotOutput("vitGraph"),),
                        tabPanel("Summary Statistics", verbatimTextOutput("vitSumm")),
                        tabPanel("Raw data", 
                                 numericInput("vitTableN",
                                              label = "Number of rows (max = 50,048)",
                                              10,
                                              min = 1,
                                              max = nrow(vittable)),
                                 checkboxGroupInput("vitTablechoice",
                                                    label = "Vitals measurements",
                                                    choices = colnames(vittable),
                                                    inline = TRUE,
                                                    selected = c("Subject ID")
                                 ),
                                 tableOutput("vitTable")))
            )
        ),     
    
    
    
    headerPanel("")
)

# Server
server <- function(input, output) {
    
    #Demographic table
    output$demTable <- renderTable({
        head(demtable, input$demnum)
    },
    digits = 0,
    colnames = TRUE)
    
    #Demographic pie charts
    output$demGraph <- renderPlot({
        if(input$demchoice %in% list("Gender",
                                     "Ethnicity",
                                     "Language",
                                     "Marital status",
                                     "Insurance",
                                     "Death within 30 days of discharge")){
            print(
                ggplot(demtable, 
                             aes(x = "", fill = eval(as.name(input$demchoice))
                                                      )
                             ) + 
                          geom_bar(position = "fill", width = 1) +
                          coord_polar("y") +
                          xlab("") +
                          labs(fill = "")
                      )
        } else {
            print(
                ggplot(demtable) +
                    geom_bar(mapping = aes(x = eval(as.name(input$demchoice))),
                             fill = "grey",
                             color = "black") +
                    xlab(input$demchoice)
            )
        }
    })
    
    #Lab measurements
    output$labGraph <- renderPlot({
        ggplot(head(labtable, input$labnumber)) +
            geom_histogram(mapping = aes(x = eval(as.name(input$labchoice))),
                           binwidth = 1,
                           color = "black") +
            xlab(input$labchoice)
    })
    
    output$labSumm <- renderPrint({
        options(digits = 2)
        stat.desc(head(labtable[,input$labchoice], input$labnumber),
                  basic = F)
    })
    
    output$labTable <- renderTable(
        head(labtable[, input$labTablechoice], input$labTableN)
    )
    
    #Vitals
    output$vitGraph <- renderPlot({
        ggplot(head(vittable, input$vitnumber)) +
            geom_histogram(mapping = aes(x = eval(as.name(input$vitchoice))),
                           binwidth = 1,
                           color = "black") +
            xlab(input$vitchoice)
    })
    
    output$vitSumm <- renderPrint({
        options(digits = 2)
        stat.desc(head(vittable[,input$vitchoice], input$vitnumber),
                  basic = F)
    })
    
    output$vitTable <- renderTable(
        head(vittable[, input$vitTablechoice], input$vitTableN)
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
