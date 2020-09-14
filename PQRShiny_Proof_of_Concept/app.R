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
    titlePanel("Normal Distribution Proof of Concept"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("mean",
                        "Mean :",
                        value = 0),
            numericInput("sd",
                        "Standard Deviation :",
                          value = 1),
            numericInput("x",
                         "x :",
                         value=0.5),
            selectInput("prob_choice",
                        "Test",
                        choices=list("P(X<x)"="x<input$x","P(X>x)"="x>input$x")),
        htmlOutput('prob')
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
        x<- sort(rnorm(10000,input$mean,input$sd))
        #Get your density
        y<- dnorm(x,mean=input$mean,sd=input$sd)
        dat<-data.frame(x,y)
      
        library(dplyr)
        sub_dat<-dat%>%filter(eval(parse(text = input$prob_choice)))
        library(ggplot2)
        ggplot(data.frame(x,y),aes(x,y))+geom_line(color='red',alpha=0.8,size=1.5)+
            geom_ribbon(data=sub_dat,aes(ymax=y),ymin=0,fill='red',alpha=0.3)+
            theme_minimal()+xlab('Quantile')+ylab('Density')
    })
    
    thresh<- reactive({
      ifelse(is.null(input$x),0,input$x)
    })
    
    output$prob<- renderText({
      tl<- ifelse(input$prob_choice=='x<input$x',T,F)
      HTML(paste0('<b>Probability:</b> ', round(pnorm(q=thresh(),mean=input$mean,sd=input$sd,
                  lower.tail=tl),2)))
    })
    
    output$cumplot <- renderPlot({
      #Get your data
      x<- qnorm(mean=input$mean,sd=input$sd,
                p=seq(0.01,0.99,length.out = 1000))
      #Get your density
      y<- pnorm(x,mean=input$mean,sd=input$sd)
      dat<-data.frame(x,y)
      library(dplyr)
      sub_dat<-dat%>%filter(eval(parse(text = input$prob_choice)))
      library(ggplot2)
      ggplot(data.frame(x,y),aes(x,y))+geom_line(color='blue',alpha=0.8,size=1.5)+
        geom_ribbon(data=sub_dat,aes(ymax=y),ymin=0,fill='blue',alpha=0.3)+
        theme_minimal()+xlab('Quantile')+ylab('Cumulative Probability')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
