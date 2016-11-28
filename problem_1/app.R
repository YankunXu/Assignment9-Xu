library(shiny)
library(babynames)
library(dplyr)
library(tidyverse)


ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(sliderInput("yearInput", "Year", min = 1880, max = 2014,
                             value = c(1900, 2000)),
                 textInput("nameInput", "Name"),
                 radioButtons("sexID", "Sex",  choices = c("Female only", "Male only", "Both"), selected = "Both")),
    
    mainPanel(plotOutput("main_plot"),
              tableOutput("results"))
  ),
  
  titlePanel("Baby Names")
)

server <- function(input, output, session) {
  reduced_df <- reactive({
    sex_vec <- switch(input$sexID,
                      `Female only` = "F",
                      `Male only` = "M",
                      Both = c("F", "M")
    )
    babynames$year<-as.integer(babynames$year)
    
    
    filter(
      babynames, 
      name == input$nameInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2], 
      sex %in% sex_vec 
    )
  })
  
  output$main_plot <- renderPlot({
    
    if(input$sexID == "Female only"){
      ggplot(data = reduced_df(), aes(year, n, colour = sex)) + geom_line() + ggtitle(input$nameInput) +
        scale_color_manual(values=c("red"))
    }
    
    
    else if(input$sexID == "Male only"){
      ggplot(data = reduced_df(), aes(year, n, colour = sex)) + geom_line() + ggtitle(input$nameInput) +
        scale_color_manual(values=c("blue"))
      
    }
    
    else if(input$sexID == c("Both")){
      
      
      ggplot(data = reduced_df(), aes(year, n, colour = sex)) + geom_line() + ggtitle(input$nameInput) +
        scale_color_manual(values=c("red", "blue"))
    }
  })
    
  output$results <- renderTable({ 
    if(input$sexID == "Female only"){
        select(reduced_df(), -prop,-proportion1,-sex)
    }
    else if(input$sexID == "Male only"){
      select(reduced_df(), -prop,-proportion1,-sex)
    }
    
    else if(input$sexID == c("Both")){
      select(reduced_df(), -prop,-proportion1)
    }
    
  })
}


shinyApp(ui = ui, server = server)

