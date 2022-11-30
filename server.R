library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(multcomp)
library(lmerTest)
library(emmeans)
library(FactoMineR)
library(factoextra)
library(agricolae)
library(car)
library(plotly)
library(multcompView)
library(broom)
library(lawstat)
library(DT)
library(grDevices)
library(ggthemes)

server <- function(input, output, session) {
  options(shiny.usecairo = FALSE)
  darkmode(label = "⏳")
  
  ddff0 <- reactive({
    dataset2 |>
      dplyr::filter(Trials %in% input$Triall)|>
      dplyr::filter(Trials== input$Triall)
    
  })
  
  
  ffdata2 <-reactive({ ddff0()|> 
     
      dplyr::select(Trials,
                    Ages,
                    Orgen,
                    PalmID,
                    Rep,
                    !!input$Variabll)|>
      group_by(Ages, Trials, Orgen)|>
      
      summarise_if(is.numeric, mean, na.rm = TRUE)
  
    
    
    
  })
  
  output$Orgen_lp <- renderUI({
    
    ffdata2() 
    
    selectInput("Orgen_lp",
                "Orgen", 
                sort(unique(ffdata2()$Orgen)),
                selected=NULL,
                width = 110,
              multiple=TRUE )
  })
  
  ddff1 <- reactive({
    ffdata2()  |>
      dplyr::filter(Orgen %in% input$Orgen_lp)|>
      dplyr::filter(Orgen== input$Orgen_lp)
    
  })
  
  

  
  
  ddf0 <- reactive({
    dataset2 |>
      dplyr::filter(Seasons %in% input$Season)
    
  })
  
  output$Trial <- renderUI({
    
    ddf0() 
    
    selectInput("Trial",
                "Trials", 
                sort(unique(ddf0()$Trial)),
                selected=NULL,
                width = 110)
  })
  
  
  ddf1 <- reactive({
    ddf0() |> 
      dplyr::filter(Trials %in% input$Trial)
  })
  
  filtered_data2<-reactive({
    
    ddf1() |>
      dplyr::filter(Seasons==input$Season,
                    Trials==input$Trial) 
    
  }) 
  
  ffdata <-reactive({ filtered_data2()|> 
      
      dplyr::select(Seasons,
                    Trials, 
                    Orgen,
                    PalmID,
                    Rep,
                    Order_number,
                    !!input$Variabl)|> 
                    na.omit()|> 
      mutate(Repetition=(Rep-1) %/% n_distinct(Order_number) + 1)|> 
      mutate(Bloc=(Rep-1) %/% n_distinct(Repetition) + 1)
    
      
  })
  
  
  df0 <- reactive({
    dataset |>
      dplyr::filter(Seasons %in% input$Seasons)
    
  })
  
  output$Trials <- renderUI({
    
    df0() 
    
    selectInput("Trials",
                "Trials", 
                sort(unique(df0()$Trials)),
                selected=NULL,
                width = 110)
  })
  
  df1 <- reactive({
    df0() |> 
      dplyr::filter(Trials %in% input$Trials)
  })
  
  output$Orgen<- renderUI({
    df1()
    
    selectInput("Orgen", 
                "X",
                sort(unique(df1()$Orgen)),
                selected=NULL,
                width = 110,
                multiple = TRUE)
    
  })
  
  df2 <- reactive({ df1() |> 
      
      dplyr::filter(Orgen %in% input$Orgen)
    
  })
  
  
  filtered_data<-reactive({
    
    df2() |>
      dplyr::filter(Seasons==input$Seasons,
                    Trials==input$Trials,
                    Orgen==input$Orgen) 
    
  }) 
  
  selected_data <-reactive({ filtered_data()|> 
      
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
                    !!input$Variabls)|> 
      mutate(Orgen=as.factor(Orgen))
                    
  })
  
  filtered_table<-reactive({  
    
    selected_data()|>
      
      dplyr::select(Orgen, 
                    Dura_Parent,
                    Progeny_of_dura,
                    Dura_parent_origin,
                    P_T_parent,
                    Progeny_of_TP,
                    T_P_origin)
  }) 
  
  df3<-reactive({ df1()[,-c(5,31:39)]|>
      
      dplyr::filter(Seasons==input$Seasons,
                    Trials == input$Trials)|>
      
      group_by(Orgen)|>
      select_if(~sum(!is.na(.)) > 0)|>
      summarise_if(is.numeric, mean, na.rm = TRUE)
    
    
  })
  
  pca_data<-reactive({
    df3()|>
      remove_rownames() |>
      
      column_to_rownames(var="Orgen")
    
  })
  
  
  # Visualization
 
  #Table
  
  output$Table <-DT::renderDataTable(datatable(unique(filtered_table()),
                                           
                                               options = 
                                                 list(
                                                   autoWidth = FALSE,
                                                   select.info=FALSE,
                                                   
                                                   pagingType="numbers",
                                                  
                                                   searching=FALSE,
                                                   rownames= FALSE
                                                   
                                                 )
                                                  
                                                  
                                             ))
                                 

  color = grDevices::colors(distinct =T)[grep('gr(a|e)y', grDevices::colors(distinct =T),invert = T)]
  
  
  #plot
  
  output$lineplots <- renderPlot({
    
    req(ffdata2()!=0)
    
    n=length(ffdata2()$Orgen)
    pie(rep(1,n))
    
    col=sample(color,n,replace = TRUE)
    p<-ggplot(ffdata2(), mapping=aes(x=Ages, y=ffdata2()[[input$Variabll]], 
                                     colour=Orgen))+
      
      geom_line(linewidth=0.8)+ 
      scale_x_continuous(limits = c(3,11), expand = c(0, 0))+
      scale_colour_manual(values = col)+
      xlab("Ages")+
      ylab(paste0(input$Variabll))
    
    print(p)
  
  })
  
  
  output$lineplots2 <- renderPlot({
    
    req(ddff1()!=0)
    
    n=length(ddff1()$Orgen)
    pie(rep(1,n))
    
    col=sample(color,n,replace = TRUE)
    p<-ggplot(ddff1(), mapping=aes(x=Ages, y=ddff1()[[input$Variabll]], 
                                     colour=Orgen))+
      
      geom_line(linewidth=0.8)+ 
      scale_size_area()+
      scale_x_continuous(limits = c(3,11), expand = c(0, 0))+
      scale_colour_manual(values = col)+
      xlab("Ages")+
      ylab(paste0(input$Variabll))
    
    print(p)
  })
  
  output$myCTest <- renderPlot({
    
    req(ffdata()!=0)
    
    
    if (input$Normality) {
      
      updateCheckboxInput(session,"Homogenity",value=FALSE)
      
      Orgen<-as.factor(ffdata()$Orgen)
      res_aov <- aov(ffdata()[[input$Variabl]] ~ Orgen,
                     data = ffdata())
      par(mfrow = c(1, 2))
      hist(res_aov$residuals)
      qqPlot(res_aov$residuals,
             id = FALSE )
      
    }else{
      
      updateCheckboxInput(session,"Homogenity",value=TRUE)
      
      Orgen<-as.factor(ffdata()$Orgen)
      
      res_aov <- aov(ffdata()[[input$Variabl]] ~ Orgen,
                     data = ffdata())
      par(mfrow = c(1, 2))
      plot(res_aov, which = 1)
      plot(res_aov, which = 2)
      
    }
    
  })
  
  
  output$myCTest2 <- DT::renderDataTable({
    
    req(ffdata()!=0)
    
    
    
    if (input$Normality) {
      
      updateCheckboxInput(session,"Homogenity",value=FALSE)
      
      dd<-shapiro.test(ffdata()[[input$Variabl]])
      print(tidy(dd))  
      
    }else{
      
      updateCheckboxInput(session,"Homogenity",value=TRUE)
      Orgen<-as.factor(ffdata()$Orgen)
      dd2<-leveneTest(ffdata()[[input$Variabl]],Orgen)  
      print(tidy(dd2))
      
    }
    
  },
  options = 
    list(
         
         select.info=FALSE,
         
         pagingType="numbers",
         
         searching=FALSE,
         rownames= FALSE
    )
  )
  
  
  output$myanova <- DT::renderDataTable({
    
    if(input$Parametric){
      updateCheckboxInput(session,"Nonparametric",value=FALSE)
     req( ffdata()!=0)
      Orgen<-as.factor(ffdata()$Orgen)
      Repetition<-as.factor(ffdata()$Repetition)
      Bloc<-as.factor(ffdata()$Bloc)
      Rep<-as.factor(ffdata()$Rep)
      formule<-as.formula(paste(input$Variabl,"~Repetition+Orgen+(1|Bloc)+(1|Rep)"))
      model1<-lmer(formule, data=ffdata())
      model2<-anova(model1)
          print(broom::tidy(model2))
      
    }else{
      updateCheckboxInput(session,"Nonparametric",value=TRUE)
      ffdata()
      
      aa <- kruskal.test(ffdata()[[input$Variabl]]~as.factor(Orgen), data=ffdata())
      print(tidy(aa))
      
    }
  },
  options = 
    list(
         
         select.info=FALSE,
         
         pagingType="numbers",
         
         searching=FALSE,
         rownames= FALSE
    ))
  
  output$mymplot <- renderPlot({
    if (input$TukeyTest){
      updateCheckboxInput(session,"PWT",value=FALSE)
      n=length(ffdata()$Orgen)
      pie(rep(1,n))
      col=sample(color,n,replace = TRUE)
      
      req(ffdata()!=0)
      Orgen<-as.factor(ffdata()$Orgen)
      Repetition<-as.factor(ffdata()$Repetition)
      Bloc<-as.factor(ffdata()$Bloc)
      Rep<-as.factor(ffdata()$Rep)
      formule<-as.formula(paste(input$Variabl,"~Repetition+Orgen+(1|Bloc)+(1|Rep)"))
      model1<-lmer( formule, data=ffdata())
      moycrois<-cld(emmeans(model1,"Orgen"), Letters = letters)
      moycrois$lettretuk<-trimws(moycrois$.group,"b")
      ggplot(ffdata(), aes(x=Orgen, y=ffdata()[[input$Variabl]], colour=Orgen, fill=Orgen))+
        geom_boxplot(outlier.alpha = 0, alpha=0.25)+
        
        geom_text(data = moycrois, 
                  aes(label = lettretuk, 
                      y=max(ffdata()[[input$Variabl]])+1,
                      size=5))+
        
        ylab(paste0(input$Variabl))+
        theme(legend.position="none")+
        scale_colour_manual(values = col)+
        theme_classic()+
        theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5))+
        theme(legend.position="none")
      
    }else{
      updateCheckboxInput(session,"PWT",value=TRUE)
      req(ffdata()!=0)
      attach(ffdata())
      tri.to.squ<-function(x)
      {
        rn <- row.names(x)
        cn <- colnames(x)
        an <- unique(c(cn,rn))
        myval <-  x[!is.na(x)]
        mymat <-  matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
        for(ext in 1:length(cn))
        {
          for(int in 1:length(rn))
          {
            if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
            mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
            mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
          }
          
        }
        return(mymat)
      }
      pp <- pairwise.wilcox.test(ffdata()[[input$Variabl]],Orgen,p.adjust.method ="holm" )
      pp
      mymat <-tri.to.squ(pp$p.value)
      mymat
      
      myletters<-multcompLetters(mymat,compare="<=",threshold=0.05, Letters=letters)
      myletters
      
      myletters_df<-data.frame(Orgen=names(myletters$Letters),letter = myletters$Letters )
      myletters_df
      n=length(ffdata()$Orgen)
      pie(rep(1,n))
      col=sample(color,n,replace = TRUE)
      ggplot(ffdata(), aes(x=Orgen, y=ffdata()[[input$Variabl]], colour=Orgen, fill=Orgen))+
        geom_boxplot(outlier.alpha = 0, alpha=0.25)+
       
        geom_text(data = myletters_df, 
                  aes(label = letter, 
                      y=max(ffdata()[[input$Variabl]])+1,
                       size=5))+
      
        stat_summary(fun=mean, colour="black", geom="point", shape=18, size=3) +
        ylab(paste0(input$Variabl))+
        theme_classic()+
        theme(legend.position="none")+
        theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5))+ 
        scale_colour_manual(values = col)
       
      
    }
    
    
  })
  
  
  output$myplot <- renderPlot({
    req(selected_data()!=0)
    
    
    n=length(selected_data()$Orgen)
    pie(rep(1,n))
    col=sample(color,n,replace = TRUE)
    if (length(input$Seasons)==1) {
      
      
    p<-ggplot(selected_data(), aes(x=Orgen, y=(!!input$Variabls), fill=Orgen))+
        geom_boxplot(position = position_dodge(1))+
      theme_bw()+
      scale_size_area()+
        stat_summary(fun=mean, colour="black", geom="point",shape=18, size=3)+
        scale_colour_manual(values = col)+
        xlab("Orgens")+
        ylab(paste0(input$Variabls))
      

      print(p)
      
      
    }
    
    else if(length(input$Seasons)>1) {
      
      
      
     p2<- ggplot(selected_data(), mapping=aes(x=Orgen,  y=(!!!input$Variabls),fill=Orgen))+
        geom_boxplot(position = position_dodge(1))+
       scale_size_area()+
       theme_bw()+
        stat_summary(fun=mean, colour="black", geom="point", shape=18, size=3) +
        facet_wrap( .~Seasons,scale='free')+
        scale_colour_manual(values = col)+
        xlab("Orgens")+
       scale_x_discrete()+
        ylab(paste0(input$Variabls))
      
    
  
      print(p2) 
      
    }
    
    
  })
  
  output$mypca <- renderPlot({
    req(pca_data()!=0)
    
    res.pca<-PCA(pca_data(),scale.unit=TRUE, ncp=2,axes=c(1,2),graph=FALSE)
    
    bm<-factoextra::fviz_pca_biplot(res.pca,
                                    repel = TRUE,
                                    geom.var = c("text"),
                                    col.var = "#006400", 
                                    col.ind = "#FF3030",
                                    cex = 0.7)
    print(bm)
    
    
  })
  
  output$mypca2 <- renderPlot({
    req(pca_data()!=0)
    
    res.pca<-PCA(pca_data(),scale.unit=TRUE, ncp=3,axes=c(1,3),graph=FALSE)
    
    bm<-factoextra::fviz_pca_biplot(res.pca,
                                    axes = c(1, 3),
                                    repel = TRUE,
                                    geom.var = c("text"),
                                    col.var = "#006400", 
                                    col.ind = "#FF3030",
                                    cex = 0.7)
    print(bm)
  })
  
  output$mycluster<-renderPlot({
    
    req(pca_data()!=0)
    
    clst<-factoextra::fviz_dend(hclust(dist(pca_data())), cex = 0.8, k = 5, color_labels_by_k = TRUE)
    
    print(clst)
    
  })
}   
