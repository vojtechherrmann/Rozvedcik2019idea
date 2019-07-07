library(shiny)

# Define UI with external image call
ui <- fluidPage(
  titlePanel("Look at the image below"),
  
  sidebarLayout(sidebarPanel(),
                
                mainPanel(htmlOutput("picture"))))

# Define server with information needed to hotlink image
server <- function(input, output) {
  output$picture <-
    renderText({
      c(
        '<img src="',
        "Vojta_formula.png",
        '">'
      )
    })
}

shinyApp(ui = ui, server = server)

