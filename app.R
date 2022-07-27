
# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)



# Read data
survival <- read.csv("data/survivalchddata.csv")
survival[, 'Gender'] <- as.factor(survival[, 'Gender'])
survival[, 'SerumCreatinine'] <- as.factor(survival[, 'SerumCreatinine'])
survival[, 'Diabetes'] <- as.factor(survival[, 'Diabetes'])
survival[, 'Smoking'] <- as.factor(survival[, 'Smoking'])
survival[, 'BP'] <- as.factor(survival[, 'BP'])
survival[, 'Anaemia'] <- as.factor(survival[, 'Anaemia'])
survival[, 'Sodium'] <- as.factor(survival[, 'Sodium'])
survival[, 'platelets'] <- as.factor(survival[, 'platelets'])
survival[, 'CPK'] <- as.factor(survival[, 'CPK'])
survival[, 'Survival'] <- as.factor(survival[, 'Survival'])


# Read in the RF model
model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(
                
                # Page header
                headerPanel('CHD Survival Prediction App'),
                
                # Input values
                sidebarPanel(
                  
                  h2("Enter CHD Patient Details"),
                  
                  fluidRow(
                    
                    column(6, 
                           sliderInput("age", 
                                       label = h5("Patient Age"),
                                       min = 40, max = 95, value = 65)
                    ),
                    column(6, 
                           sliderInput("time", 
                                       label = h5("Follow Up Time"),
                                       min = 4, max = 285, value = 130) 
                    )
                    
                  ),
                  fluidRow(
                    
                    column(4,
                           selectInput("gender", 
                                       label = h6("Gender"), 
                                       choices = c("Male" = 1,
                                                   "Female" = 0), 
                                       selected = 0)
                    ),
                    
                    column(4,
                           selectInput("smoking", 
                                       label = h6("Does the Patient Smoke?"), 
                                       choices = c("Yes" = 1,
                                                   "No" = 0), 
                                       selected = 0)
                    ),
                    
                    column(3,
                           selectInput("diabetes",
                                       label = h6("Diabetic Patient?"), 
                                       choices = c("Yes" = 1,
                                                   "No" = 0), 
                                       selected = 0))
                    
                    
                  ),
                  
                  fluidRow(
                    
                    column(5,
                           radioButtons("ef", 
                                        label = h5("Ejection Fraction"),
                                        choices = c("less than 30" = 0, 
                                                    "between 30 & 45" = 1,
                                                    "more than 45" = 2),
                                        selected = 1)
                    ),
                    
                    column(5,
                           radioButtons("cpk", 
                                        label = h5("Creatine Phosphokinase (CPK)"),
                                        choices = c("less than 116.5" = 0, 
                                                    "between 116.5 & 250" = 1,
                                                    "between 250 & 582" = 2,
                                                    "higher than 582" = 3),
                                        selected = 1)
                    ),
                    column(3,
                           selectInput("anaemia", 
                                       label = h6("Anaemic Patient?"), 
                                       choices = c("Yes" = 1,
                                                   "No" = 0), 
                                       selected = 0)
                    )
                    
                    
                  ),
                  fluidRow(
                    
                    
                    column(5,
                           radioButtons("plates",
                                        label = h5("Platelets Count"),
                                        choices = c("less than 212 500" = 0,
                                                    "between 212 500 & 262 000" = 1,
                                                    "between 262 000 & 303 500" = 2,
                                                    "more than 303 500" = 3), 
                                        selected = 0)),
                    
                    column(5,
                           radioButtons("srsod",
                                        label = h5("Serum Sodium"),
                                        choices = c("less than 134" = 0,
                                                    "between 134 & 137" = 1,
                                                    "between 137 & 140" = 2,
                                                    "more than 140" = 3), 
                                        selected = 0)
                    )
                    
                  ),
                  fluidRow(
                    
                    column(5,
                           selectInput("sr", 
                                       label = h5("Serume Creatinine"),
                                       choices = c("less than 1" = 0, 
                                                   "higher than 1" = 1),
                                       selected = 1)),
                    column(4,
                           selectInput("bp",
                                       label = h5("High Blood Pressure Patient?"),
                                       choices = c("No" = 0,
                                                   "Yes" = 1), 
                                       selected = 0)
                    )
                    
                  ),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("Age",
               "TIME",
               "Gender",
               "Smoking",
               "Diabetes",
               "EF",
               "CPK",
               "Anaemia",
               "platelets",
               "Sodium",
               "SerumCreatinine",
               "BP"
               ),
      Value = as.character(c(input$age,
                             input$time,
                             input$gender,
                             input$smoking,
                             input$diabetes,
                             input$ef,
                             input$cpk,
                             input$anaemia,
                             input$plates,
                             input$srsod,
                             input$sr,
                             input$bp
                             )),
      stringsAsFactors = FALSE)
    
    Survival <- 0
    df <- rbind(df, Survival)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$BP <- factor(test$BP, levels = c(1,0))
    test$SerumCreatinine <- factor(test$SerumCreatinine, levels = c(1,0))
    test$Sodium <- factor(test$Sodium, levels = c(1,0,2,3))
    test$platelets <- factor(test$platelets, levels = c(1,0,2,3))
    test$Anaemia <- factor(test$Anaemia, levels = c(1,0))
    test$CPK <- factor(test$CPK, levels = c(1,0,2,3))
    test$Diabetes <- factor(test$Diabetes, levels = c(1,0))
    test$Smoking <- factor(test$Smoking, levels = c(1,0))
    test$Gender <- factor(test$Gender, levels = c(1,0))
    test$EF <- factor(test$EF, levels = c(1,0,2))
    test$Survival <- factor(test$Survival, levels = c(1,0))
    
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################

shinyApp(ui = ui, server = server)
