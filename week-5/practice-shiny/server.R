#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#使用者看不到的以運算為主
library(shiny)
fileInput(CON)
# Define server logic required to draw a histogram
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$CON<- renderPlot({
    CON<-read.csv("CON")
    # Render a barplot
    barplot(CON[,input$region]*1000, 
            main=input$region,
            ylab="Economy..GDP.per.Capita.",
            xlab="Countries")
  })
}