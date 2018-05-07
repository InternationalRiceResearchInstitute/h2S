
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Expected genetic gain"),
  br(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel= sidebarPanel(
      sliderInput("cycdur", label = "Number of years to complete one breeding cycle",
                  min = 1,  max = 15, value = 7, step=1),
      sliderInput("sliderh2", label = "Heritability in the target region",
                  min = 0,  max = 1, value = .1, step=0.05),
      sliderInput("sliderNcand", label = "Population size: Number of potential new parents for crossing each cycle",
                  min = 10,  max = 1000, value = 200, step=10),
      sliderInput("sliderNpar", label = "Number selected: Number of new parents selected for crossing each cycle",
                  min = 10,  max = 100, value = 30, step=10),
      sliderInput("sliderNcyc", label = "Number of cycles of selection",
                  min = 1,  max = 5, value = 1),
      numericInput("minVal", label = "Phenotypic distribution minimum value", value = 2),
      numericInput("maxVal", label = "Phenotypic distribution maximum value", value = 8)),

    # Main panel for displaying outputs ----
    mainPanel= mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Expected genetic gain",
                           br(),
                           numericInput("rnseed", label = "Replicate of selection program", value = 1),
                           br(),
                           plotOutput("plot1")),
                  tabPanel("Variables",
                           tableOutput("tab")),
                  tabPanel("Phenotypic values",
                           plotOutput("plot2")),
                  tabPanel("Phenotypic value table",
                           tableOutput("phenos")),
                  tabPanel("Trend line",
                           plotOutput("plot3"))
      )
    )
  )
)

server <- function(input, output) {
  library(deltaG)
  library(ggplot2)
  
  #do analysis reactively
  getOutput<- reactive({
    Q<- demoSelection(pop0min= input$minVal,
                      pop0max= input$maxVal, herit= input$sliderh2, 
                      popsize=input$sliderNcand, 
                      numparents= input$sliderNpar, 
                      ncycles= input$sliderNcyc, cycledur=input$cycdur,
                      rnseed= input$rnseed)
  })
  ##make plot1 reactively
  getPlot1<- reactive({
    p1<- getOutput()[1]$plt+theme(axis.text=element_text(size=14,face="plain"),
                                  axis.title=element_text(size=16,face="plain"),
                                  legend.text=element_text(size=14,face="plain"),
                                  legend.title=element_text(size=16,face="plain"),
                                  plot.title=element_text(size=16,face="plain"))
    p1
  })
  ##make plot2 reactively
  getPlot2<- reactive({
    p2<- getOutput()[2]$plt2+theme(axis.text=element_text(size=14,face="plain"),
                                   axis.title=element_text(size=16,face="plain"),
                                   legend.text=element_text(size=14,face="plain"),
                                   legend.title=element_text(size=16,face="plain"),
                                   plot.title=element_text(size=16,face="plain"))
    p2
  })
  getPlot3<- reactive({
    p3<- getOutput()[5]$plt3+theme(axis.text=element_text(size=14,face="plain"),
                                   axis.title=element_text(size=16,face="plain"),
                                   legend.text=element_text(size=14,face="plain"),
                                   legend.title=element_text(size=16,face="plain"),
                                   plot.title=element_text(size=16,face="plain"))
    p3
  })
  #plot1 output
  output$plot1 <- renderPlot({
    print(getPlot1())
  })
  #plot2 output
  output$plot2 <- renderPlot({
    print(getPlot2())
  })
  #plot3 output
  output$plot3 <- renderPlot({
    print(getPlot3())
  })
  #Make variable table ractively
  getSummary<- reactive({
    tab<- getOutput()[3]$tab
  })
  #Make phenos table ractively
  getPhenos<- reactive({
    phenos<- getOutput()[4]$phenos
  })
  #variable table output
  output$tab <- renderTable({
    tab<- getSummary()
    return(tab)
  }, digits=5)
  #Phenos table output
  output$phenos <- renderTable({
    phenos<- getPhenos()
    return(phenos)
  })
}

# Run the application
shinyApp(ui = ui, server = server)