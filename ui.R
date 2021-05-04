
library(shiny)
library(shinythemes)

dataset <-read.csv2("RANAL.csv",header=TRUE)


ui <- (fluidPage(
  theme = shinytheme("cyborg"),
  
  titlePanel(h3("Genetic Block Results")),
  
      sidebarLayout(
    
       sidebarPanel(width=3,
            
                 
    selectInput(inputId='Seasons',
                  label='Seasons', 
                   choices=unique(dataset[,c(25)]),
                      selected = NULL,
                       multiple = FALSE,
                        selectize = FALSE,
                         width=120,
                           size=1),
                 
    
    uiOutput("Trials"),
    uiOutput("Orgen"),
    
 
    varSelectInput(inputId='Variabls', 
                   label='Y', 
                   data=dataset[c(2:24)],
                   width=120,
                   multiple=TRUE,
                   selected='ANB')
                
      ),
  mainPanel(
    tabsetPanel(
      tabPanel("PCA",
               fluidRow(1,
                        column(4,width=6.5,plotOutput('mypca')),
                        column(4,width=6.5, plotOutput('mypca2'))),
               hr(),
               fluidRow(2,plotOutput('mycluster'))
      ),
      tabPanel("Plot", plotOutput("myplot")),
      tabPanel("Info", dataTableOutput("Table"))
      
      ),
      br(),
      tags$img(height = 70, 
               width = 420, 
               src = "Lestroislogos.png")
     
    
    )
   )
  )
 )



