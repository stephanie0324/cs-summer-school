#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

CON<-read.csv("CON.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("GDP by region"),
  sidebarLayout(
                sidebarPanel(
                              helpText("Data from kaggle"),
                              selectInput("region",
                                          label="Choose a region",
                                          choices=c("NRA","NE","ASIA","AFR"))
  
                ),
                mainPanel(plotOutput("CON"))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$CON<- renderPlot({
    
    # Render a barplot
    barplot(xx*1000, 
            main=xx,
            ylab="Economy..GDP.per.Capita.",
            xlab="Countries")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

