library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(DT)

source("functions.R")

table_out <- data.frame(
  Skupina1 = rep("", 6),
  Skupina2 = rep("", 6),
  Skupina3 = rep("", 6),
  Skupina4 = rep("", 6),
  Skupina5 = rep("", 6)
)

write_csv(table_out, "table_out.csv")

# Define UI

ui <- bootstrapPage(
  theme = shinytheme("lumen"),
  
  titlePanel("Rozvědčík 2019 - Losovátko"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Select type of trend to plot
      selectInput(inputId = "name", label = strong("Jméno"),
                  choices = unique(quests$Name),
                  selected = "Bára"),
      
      actionButton("button1", "Vygeneruj vzorec"),
      
      br(),
      br(),
      
      textInput(inputId = "number", label = strong("Přirozené číslo"), value = 0),
      
      actionButton("button2", "Vyhodnoť vzorec")
    ),
    
    mainPanel(
      #plotOutput(outputId = "formula", height = "550px", width = "100%"),
      #plotOutput(outputId = "skupina", width = "100%")
      dataTableOutput("table_out")
    )
  )
)

# Define server function
server <- function(input, output) {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  gffp <- eventReactive(input$button, {
    generateFunctionForPerson (input$name)
  })
  
  N <- eventReactive(input$button2, {
    as.integer(input$number)
  })
  
  
  # Subset data
  guest <- reactive({
    guests %>% filter(name == input$Name)
  })
  
  output$table_out <- DT::renderDataTable({
    DT::datatable(table_out)
  })
  
  
  output$formula <- renderPlot({
    plot.new()
    generateFormulaPlot(gffp())
  })
  
  output$skupina <- renderPlot({
    plot.new()
    generateSkupinaPlot(evaluateFunctionForPerson(gffp(), N()))
  })
  
  output$test <- renderText({
    gffp()
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)