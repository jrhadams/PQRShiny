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
    titlePanel("Binomial Distribution Proof of Concept"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("size",
                        "Size :",
                        value = 3),
            sliderInput("prob",
                        "Probability :",
                        min = 0,
                        max = 1,
                        value = 0.5),
            numericInput("x",
                         "x :",
                         min=0,max=6,
                         value=1),
            selectInput("prob_choice",
                        "Test",
                        choices=list("P(X<x)"="<",
                                     "P(X>x)"=">",
                                     "P(X==x)"="==",
                                     "P(X<=x)"="<=",
                                     "P(X>=x)"=">=")),
           textOutput(outputId = "Prob_out")
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
  prob_test_val<- reactive({
    ifelse(input$prob_choice=="==",
           dbinom(x=input$x,size=input$size,prob=input$prob),
           ifelse(input$prob_choice=="<=",
                  pbinom(q=input$x,size=input$size,prob=input$prob),
                  ifelse(input$prob_choice==">=",
                         pbinom(q=(input$x-1),size=input$size,prob=input$prob,lower.tail = F),
                         ifelse(input$prob_choice==">",pbinom(q=input$x,size=input$size,prob=input$prob,lower.tail=F),
                                                              pbinom(q=(input$x-1),size=input$size,prob=input$prob)))))
  })
  output$Prob_out <-renderText({prob_test_val()})
    output$distPlot <- renderPlot({
        #Get your data
        #x<- sort(rbinom(n=10000,size=input$size,prob=input$prob))
        #Get your density
        y<- dbinom(0:input$size,size=input$size,prob=input$prob)
        #dat<-unique(data.frame(x,y))
      
        library(dplyr)
        #sub_dat<-dat%>%filter(eval(parse(text = input$prob_choice)))
        library(ggplot2)
#        browser()
        ggplot(data.frame(x=0:input$size,y),aes(x,y))+geom_point(size=2)+
          geom_linerange(aes(xmin=0,xmax=x,ymin=0,ymax=y),color='red',alpha=0.8,size=1.5)+
            theme_minimal()+xlab('Quantile')+ylab('Density')#+
         # geom_vline(xintercept=input$x,lty='dashed',
          #           color='#28df99',size=2)
    })
    
    output$cumplot <- renderPlot({
      #Get your data
#      x<- seq(,length.out = 10000)
      #x<- sort(rbinom(n=10000,size=input$size,prob=input$prob))
      #Get your density
      y<- pbinom(q=0:input$size,size=input$size,prob=input$prob)
      dat<-data.frame(x=0:input$size,y)
      library(dplyr)
#      sub_dat<-dat%>%filter(eval(parse(text = input$prob_choice)))
      library(ggplot2)
      ggplot(dat,aes(x,y))+geom_point(color='black',alpha=0.8,size=2)+
       geom_linerange(aes(xmin=0,xmax=x,ymin=0,ymax=y),color='blue',alpha=0.8,size=1.5)+
        #geom_area(aes(x=ifelse(x==input$x,x,0)))+
        theme_minimal()+xlab('Quantile')+ylab('Cumulative Probability')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
