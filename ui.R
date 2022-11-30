
packs <- c('remotes','shiny','bslib','shinyDarkmode','dplyr','tidyr','tibble','ggplot2','FactoMineR','factoextra','agricolae','car','multcompView','broom','lawstat','DT','grDevices')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack)) }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

packs2 <-c('shinyDarkmode')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(remotes::install_github,as.list(pack)) }
  do.call(require,as.list(pack)) }
lapply(packs2, InstIfNec)

library(shiny)
library(bslib)
library(shinyDarkmode)



ui <- navbarPage("PRGBP ShinyApp",
                 header=use_darkmode(),
                 theme =bs_theme(
                   bg = "#b4d670", 
                   fg = "#1686b3",
                   primary = "#111111", 
                   secondary = "#48DAC6",
                   base_font = "sans-serif",
                   font_scale =0.8,
                   heading_font = "Helvetica Neue, Helvetica, sans-serif",
                   "input-border-color" = "#EA80FC"
                                    ),
                 
                 
                 selected = "TRENDS",
                   
                  tabPanel("TRENDS",
                          fluidPage(  
                            sidebarLayout(
                              sidebarPanel(width=2,
                                           selectInput(inputId='Triall',
                                                       label='Trials', 
                                                       choices=unique(sort(dataset2$Trials)),
                                                       selected = NULL,
                                                       multiple = FALSE,
                                                       selectize = TRUE,
                                                       width=110),
                                                      
                                           uiOutput("Orgen_lp"),
                                           
                                           varSelectInput(inputId='Variabll', 
                                                          label='Variables', 
                                                          data=dataset2[c(2:4,6:27)],
                                                          width=110,
                                                          multiple=FALSE,
                                                          selected=NULL)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Lineplots",
                                           fluidRow(
                                             column(width=12,plotOutput('lineplots')),
                                             hr(),
                                           column(width=12,plotOutput('lineplots2',width ='1000px',height = '1000px'))
                                           )    
                                  )
                                ),
                                br(),
                                tags$img(height = 70, 
                                         width = 300, 
                                         src = "Lestroislogos.png")
                                
                              )           
                            )       
                          )
                 ),
                 
                 tabPanel("GLOBAL",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(width=2,
                                           selectInput(inputId='Season',
                                                       label='Seasons', 
                                                       choices=unique(sort(dataset2$Seasons)),
                                                       selected = NULL,
                                                       multiple = FALSE,
                                                       selectize = TRUE,
                                                       width=110),
                                                       
                                           
                                           uiOutput("Trial"),    
                                           
                                           varSelectInput(inputId='Variabl', 
                                                          label='Variables', 
                                                          data=dataset2[c(2:4,6:27)],
                                                          width=110,
                                                          multiple=FALSE,
                                                          selected=NULL)
                                           
                              ),
                              mainPanel(
                                tabsetPanel(
                                    tabPanel("Conformity Test",
                                           fluidRow(
                                             checkboxInput("Normality", label = "Normality", value = TRUE),
                                             checkboxInput("Homogenity", label = "Homogenity", value = FALSE),
                                           ),
                                           column(plotOutput('myCTest'),width=8),
                                           hr(),
                                           column(dataTableOutput('myCTest2'),width=8)
                                           
                                  ),
                                  
                                  
                                  tabPanel("Anova", 
                                           fluidRow(
                                             checkboxInput("Parametric", 
                                                           label = "Parametric", value = TRUE),
                                             checkboxInput("Nonparametric", 
                                                           label = "Nonparamertric", value = FALSE),
                                             dataTableOutput('myanova')
                                           )
                                  ),
                                  tabPanel("BoxPlot", 
                                           fluidRow(
                                             checkboxInput("TukeyTest", 
                                                           label = "Tukey Test", value = TRUE),
                                             checkboxInput("PWT", 
                                                           label = "Pairwise.wilcox.test", value = FALSE),
                                             plotOutput('mymplot'),width="1000px",heigth="1000px")
                                  ),
                                  
                                  
                                ),
                                br(),
                                tags$img(height = 70, 
                                         width = 300, 
                                         src = "Lestroislogos.png")
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel("MVA",
                          fluidPage(   
                             sidebarLayout(
                              sidebarPanel(width=2,
                                           selectInput(inputId='Seasons',
                                                       label='Seasons', 
                                                       choices=unique(sort(dataset$Seasons)),
                                                       selected = NULL,
                                                       multiple = TRUE,
                                                       selectize = TRUE,
                                                       width=110),
                                           
                                           
                                           uiOutput("Trials"),
                                           uiOutput("Orgen"),
                                           
                                           varSelectInput(inputId='Variabls', 
                                                          label='Y', 
                                                          data=dataset[c(2:4,6:27)],
                                                          width=110,
                                                          multiple=FALSE,
                                                          selected=NULL)
                                           
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("PCA",
                                           fluidRow(
                                                    column(width=12,plotOutput('mypca', width="600px")),
                                                    hr(),
                                                    column(width=12, plotOutput('mypca2',width="600px"))),
                                           hr(),
                                           fluidRow(plotOutput('mycluster'))
                                  ),
                                  tabPanel("Plot", plotOutput("myplot"), width="auto"),
                                  tabPanel("Info", dataTableOutput("Table"))
                                  
                                ),
                                br(),
                                tags$img(height = 70, 
                                         width = 300, 
                                         src = "Lestroislogos.png")
                                
                              )
                            )
                          )
                 )
)






