#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tm)
library(R.utils)
library(data.table)
library(slam)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predict-a-Word!"),

    # Sidebar with sentence input
    sidebarLayout(
        sidebarPanel(
            textInput("sentence",
                        "Sentence:",
                        "")
        ),

        # Show the predicted word(s)
        mainPanel( 
          textOutput("prediction")
        )
    )
)

# Define server logic required to return a word
server <- function(input, output) {
  
  corpus_table <- read.csv('nextword.csv')

    output$prediction <- renderText({
      req(input$sentence)
      sentence<-input$sentence
      last_word <- sub("^.*\\s+(\\S+)$", "\\1", tolower(input$sentence))
      last_word <- sub("\\s+$", "", last_word)
      prediction <- corpus_table$nex[corpus_table$prev == last_word]
      if (is.na(prediction) || length(prediction) == 0) {
        "the"
      } else prediction
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
