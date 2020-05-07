#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beta Distribution Proof of Concept"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("shape1",
                        "a :",
                        min = 1,
                        max = 20,
                        value = 3),
            sliderInput("shape2",
                        "b :",
                        min = 1,
                        max = 20,
                        value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("cumplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    set.seed(1234)
    output$distPlot <- renderPlot({
        #Get your data
        x<- sort(rbeta(10000,input$shape1,input$shape2))
        #Get your density
        y<- dbeta(x,shape1=input$shape1,shape2=input$shape2)
        library(ggplot2)
        ggplot(data.frame(x,y),aes(x,y))+geom_line(color='red',alpha=0.8,size=1.5)+
            theme_minimal()+xlab('Quantile')+ylab('Density')
    })
    
    output$cumplot <- renderPlot({
      #Get your data
      x<- seq(0.01,0.99,length.out = 10000)
      #Get your density
      y<- pbeta(x,shape1=input$shape1,shape2=input$shape2)
      library(ggplot2)
      ggplot(data.frame(x,y),aes(x,y))+geom_line(color='blue',alpha=0.8,size=1.5)+
        theme_minimal()+xlab('Quantile')+ylab('Cumulative Probability')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
