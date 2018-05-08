# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)
# Define UI
ui <- fluidPage(
  # Application title
  img(src = "App Branding.jpg"),
  br(),
  br(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel= sidebarPanel(
      sliderInput("cycdur", label = "Breeding cycle duration",
                  min = 1,  max = 15, value = 7, step=1),
      
      bsTooltip(id = "cycdur", title = "After making a cross, how many years does it take for a progeny of that cross to become a new parent?", 
                placement = "bottom", trigger = "hover"),
      
      sliderInput("sliderh2", label = "Heritability",
                  min = 0,  max = 1, value = .1, step=0.05),
      
      bsTooltip(id = "sliderh2", title = "What is the heritability of the trait or index of traits in the target region?", 
                placement = "bottom", trigger = "hover"),

      sliderInput("sliderNcand", label = "Population size",
                  min = 10,  max = 1000, value = 200, step=10),
      
      bsTooltip(id = "sliderNcand", title = "How many breeding individuals are phenotyped in multiple environments for the target traits?", 
                placement = "bottom", trigger = "hover"),
      
      sliderInput("sliderNpar", label = "Number selected",
                  min = 10,  max = 100, value = 30, step=10),
      
      bsTooltip(id = "sliderNpar", title = "How many parents are selected for crossing?", 
                placement = "bottom", trigger = "hover"),
      
      sliderInput("sliderNcyc", label = "Number of cycles",
                  min = 1,  max = 5, value = 1),
      numericInput("minVal", label = "Phenotypic distribution minimum value", value = 2),
      numericInput("maxVal", label = "Phenotypic distribution maximum value", value = 8)),

    # Main panel for displaying outputs ----
    mainPanel= mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Expected genetic gain",
                           br(),
                           numericInput("rnseed", label = "Replicate", value = 1),
                           bsTooltip(id = "rnseed", title = "This option allows you to repeat the same breeding process again to see how random chance affects the outcome", 
                                     placement = "bottom", trigger = "hover"),
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