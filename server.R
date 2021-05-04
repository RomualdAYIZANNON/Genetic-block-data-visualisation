library(dplyr)
library(tibble)
library(ggplot2)
library(FactoMineR)
library(factoextra)


server <- function(input, output, session) {
  
  df0 <- reactive({
    dataset %>%
      dplyr::filter(Seasons %in% input$Seasons)
      
           
  })
  
  output$Trials <- renderUI({
    
    df0() 
    
    selectInput("Trials",
                "Trials", 
                sort(unique(df0()$Trials)),
                selected=NULL,
                width = 120)
    
      })
  
  df1 <- reactive({
    df0() %>% 
      dplyr::filter(Trials %in% input$Trials)
  })
  
  output$Orgen<- renderUI({
    df1()
    
    selectInput("Orgen", 
                "X",
                sort(unique(df1()$Orgen)),
                selected=NULL,
                width = 120,
                multiple = TRUE)
    
      })
  
  df2 <- reactive({ df1() %>% 
      
      dplyr::filter(Orgen %in% input$Orgen)
    
  })
  
  
  
  filtered_data<-reactive({
    
    df2() %>%
      dplyr::filter(Seasons==input$Seasons,
                    Trials==input$Trials,
                    Orgen==input$Orgen) 
      
  }) 
  
  selected_data <-reactive({ filtered_data()%>% 
      
      dplyr::select(Seasons,
             Trials, 
             Orgen,
             Dura_Parent,
             Progeny_of_dura,
             Dura_parent_origin,
             P_T_parent,
             Progeny_of_TP,
             T_P_origin,
             Rep,
             !!!input$Variabls)

  })
  
filtered_table<-reactive({  
      
   selected_data()%>%
     
      dplyr::select(Orgen, 
             Dura_Parent,
             Progeny_of_dura,
             Dura_parent_origin,
             P_T_parent,
             Progeny_of_TP,
             T_P_origin)
           
            }) 
  
 df3<-reactive({ df1()[,-c(24,27,28)]%>%

     dplyr::filter(Seasons==input$Seasons,
             Trials == input$Trials)%>%
        
        select_if(~sum(!is.na(.)) > 0)%>%
        group_by(Orgen)%>%
        summarise_if(is.numeric, mean, na.rm = TRUE)
        
            })
 
  pca_data<-reactive({
    df3()%>%
      remove_rownames() %>% 
      column_to_rownames(var="Orgen")
   
   })
  
  # Visualization
  
  #Table
  
output$Table <-renderDataTable( filtered_table())
                                 
                                    
  
  #plot

    
    output$myplot <- renderPlot({
    
      
      if (length(input$Variabls)==1) {

        
        p<-ggplot(selected_data(), aes(x=Orgen, y=(!!!input$Variabls)))+
          geom_boxplot(colour=rainbow(n=length(input$Orgen)))+
          stat_summary(fun=mean, colour="black", geom="point", shape=18, size=3)
        
        print(p)
          
}
     
      else if(length(input$Variabls)>1) {
        
        q<-quos(input$Variabls)
          
     p2<- ggplot(selected_data(), aes(,, x=Orgen, y=(!!!q)))+
       geom_boxplot(colour=rainbow(n=length(input$Orgen)))+
       stat_summary(fun=mean, colour="black", geom="point", shape=18, size=3) +
       facet_grid( ,,.~(!!!q) , scales="free", space = "free")
        
        print(p2)
        
        
      }
      
      width = "auto"
      height = "auto"
      res = 100
      })
    
    
    
    output$mypca <- renderPlot({
      
     
     
    
    res.pca<-PCA(pca_data(),scale.unit=TRUE, ncp=2,axes=c(1,2),graph=FALSE)
    
   bm<-factoextra::fviz_pca_biplot(res.pca,
                    repel = TRUE,
                      col.var = "#006400", 
                      col.ind = "#FF3030",
                    cex = 0.7)
                 
                    
      print(bm)
      
      width = "auto"
      height = "auto"
      res = 100
    })
    
    
    output$mypca2 <- renderPlot({
      
    
      
      
      res.pca<-PCA(pca_data(),scale.unit=TRUE, ncp=3,axes=c(1,3),graph=FALSE)
      
      
      bm<-factoextra::fviz_pca_biplot(res.pca,
                                      axes = c(1, 3),
                                      repel = TRUE,
                                      col.var = "#006400", 
                                      col.ind = "#FF3030",
                                      cex = 0.7)
      
      
      print(bm)
      
      width = "auto"
      height = "auto"
      res = 100
    })

output$mycluster<-renderPlot({
  
  pca_data()
  
  
  clst<-factoextra::fviz_dend(hclust(dist(pca_data())), cex = 0.8, k = 5, color_labels_by_k = TRUE)
  
  print(clst)
  
  width = "auto"
  height = "auto"
  res = 100
  
  
})

  }   
      
# Run the application 

