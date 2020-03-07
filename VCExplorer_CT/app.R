#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(tidyr)
library(lubridate)
library(DT)
library(heatmaply)
library(dendextend)


load("ref_tpx_CT.RData") # load .Rdata or run EDA_home_CT.R

nms <- names(params_VC)

nms2 <- nms[-1]

vindex <- function(df){
  vi <- df %>% select(fill) %>% colSums() %>% as.numeric()
  vi2 <- as.numeric(NROW(df) - vi)/NROW(df) 
  return(vi2)
}

cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x),
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  #diag(z) <- 1
  z
}


ui <- fluidPage(
  titlePanel(title=div(img(src="fraunhofer_IME-logo_900p.jpg",
                           height="15%", width="15%", align="right"), "IME Score Basics v.3")),
  
  #tags$h2("Vindex Basics v.1"),
  p("An attempt to visualize a virtual cohort with patient-related deviations from means +/- sd and a possible IME score"),
  #hr(),
  sidebarPanel(
    # selectInput('x', 'CohortParameter', choices = names(nontidy_VC)[c(1,3:115)], selected = "P2_12_Tai.cm"),
    # selectInput('y', 'LongitudinalY', choices = names(nontidy_VC)[c(1,3:115)], selected = "KC_68_Trimg..mm"),
    # selectInput('color', 'Color', choices = names(nontidy_VC)[c(1,3:115)], selected = "KC_63_HbA..mmmolHb."),
    selectInput('x', 'CohortParameter', choices =nms2, selected = "P2_12_Taillenumfang_cm"),
    selectInput('y', 'LongitudinalY', choices = nms2, selected = "KC_68_Triglyceride_mgdl_mmoll"),
    selectInput('color', 'Color', choices = nms, selected = "KC_63_HbA1c_mmol_molHb"),
    
    selectInput('gesch_sel', 'Gender', c(Both = '.', Male = "m", Women = "w"), selected = "m"),
    sliderInput('clus', 'No. of cluster  (Heatmap tab)', 1, 10, 2, step = 1, animate = T),
    #selectInput('facet_col', 'Facet Column', c(None = '.', nms2)),
    selectInput('PatID', 'Patient_no',choices = as.vector(nontidy_VC$ID)[1:100], selected = "Pat_098"),
    selectInput('myPicker','Select groups of parameters', choices = unname(partype), selected = unname(partype)[c(2)]),
    selectInput('class', 'Display', choices = c("heatmap", "scatter"), selected = "scatter")
  ),
  # tags$style(type="text/css", "Gauge.recalculating { opacity: 1.0; }"),
  # tags$style(type="text/css", "Gauge2.recalculating { opacity: 1.0; }"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("VCoh_Means", plotlyOutput('trendPlot'),width = "auto", height = "800px"),
                tabPanel("Longitudinal",fluidRow(
                  splitLayout(cellWidths = c("100%", "100%"), 
                              plotlyOutput('plot2'))),
                  DT::dataTableOutput(outputId="Param"), width = "auto", height = "800px"),
                #tabPanel("Lollipop", plotOutput("Lollipop"),width = "600px", height = "700px"),
                #tabPanel("Vindex", gaugeOutput('Gauge'), width = "700px", height = "700px"),
                tabPanel("IME Score",fluidRow(
                  splitLayout(cellWidths = c("100%","100%")), 
                  plotlyOutput('Gauge'),
                  plotlyOutput('Gauge2'))),
                tabPanel("HeatMaps", fluidRow(
                  splitLayout(cellWidths = c("100%","100%")),
                  plotlyOutput('heat1'),width = "auto", height = "1000px"),
                  DT::dataTableOutput(outputId = "Param2"), width = "auto", height = "800px")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output){
  
  dataset <- eventReactive({
    input$x
    input$color
    input$gesch_sel}, 
    {
      
      if(input$gesch_sel == "."){
      
      tt <- VC_date_id %>% filter(DESCRIPTION %in% c(input$x,input$color)) %>%
        spread(DESCRIPTION,VALUE) %>%
        group_by(ID) %>% 
        summarise(Age = mean(as.numeric(get(input$x)), na.rm = T),
                  Colpar = mean(as.numeric(get(input$color))))
      names(tt)[2] <- input$x
      names(tt)[3] <- input$color
      } else {
        tt <- VC_date_id %>% filter(DESCRIPTION %in% c(input$x,input$color)) %>% 
          spread(DESCRIPTION,VALUE) %>% 
          filter(PP_3_Geschlecht_fct == as.character(input$gesch_sel)) %>%
          group_by(ID) %>% 
          summarise(Age = mean(as.numeric(get(input$x)), na.rm = T),
                    Colpar = mean(as.numeric(get(input$color))))
        names(tt)[2] <- input$x
        names(tt)[3] <- input$color
      }
      
      return(tt)
      
    })
  
  dataset_long <- eventReactive({ 
    input$y 
    input$PatID } , 
    {
      
      ll <- VC_date_id %>% filter(DESCRIPTION == input$y) %>% 
        filter(ID == as.character(input$PatID))
      
      return(ll)
      
    })
  
  colnum <- eventReactive(input$x,{
    yy <- which(names(params_VC) == input$x )
    return(as.numeric(yy))
  })
  
  colcol <- eventReactive(input$color,{
    aa <- which(names(params_VC) == input$color )
    return(as.numeric(aa))
  })
  
  selRows <- eventReactive(input$y,{
    row <- which(names(params_VC) == input$y)
  })
  
  namescol <- eventReactive(input$myPicker, {
    nmcl <- params_VC[,-1] %>% select(map(as.character(input$myPicker),
                                          starts_with,
                                          vars = colnames(.)) %>% unlist()) %>% names()
    
    return(nmcl)
    
  })
  
  nontidy_VC_class <- eventReactive(input$gesch_sel, {
    
    run_params_VC <- nontidy_VC %>% filter(PP_3_Geschlecht_fct == as.character(input$gesch_sel))
    return(run_params_VC)
    
  })
  
  params_VC_class <- eventReactive(input$gesch_sel, {
    
    if(input$gesch_sel == "."){
      temp = reactive({ nontidy_VC })
    }else{
      temp <- reactive({
        tt <- nontidy_VC %>% filter(PP_3_Geschlecht_fct == as.character(input$gesch_sel))
        return(tt)
      })
    }
    
    run_params_VC2 <- as.data.frame(sapply(temp()[,c(2,4:95,97:107)], 
                         function(x) c( "Sdev" = sd(x, na.rm = T),"Mean"= mean(x,na.rm=TRUE),
                                        "n" = length(x),
                                        "Median" = median(x, na.rm = T),
                                        #"CoeffofVariation" = sd(x, na.rm = T)/mean(x,na.rm=TRUE),
                                        "Minimum" = min(x, na.rm = T),
                                        "Maximun" = max(x, na.rm = T),
                                        "Upper Quantile" = quantile(x,1, na.rm = T),
                                        "LowerQuartile" = quantile(x,0, na.rm = T))), stringsAsFactors = F)
    ss <- row.names(run_params_VC2)
    #run_params_VC2$ID <- c("Sdev","Mean","n","Median","Minimum", "Maximum","Upper Quantile", "LowerQuartile")
    run_params_VC2$ID <- ss
    run_params_VC2 <- run_params_VC2 %>% select(ID, everything())
    
    return(run_params_VC2)
    
  })
  
  dataset_grouped2 <- eventReactive({
    input$PatID
    nontidy_VC_class()
    namescol()}, 
    {
    
    #req(input$PatID, input$gesch_sel, nontidy_VC_class(), namescol())
    
    if(input$gesch_sel == "."){
      
      dfs3 <- reactive({
        nontidy_VC[,-c(1,3,96)] %>% select(ID,all_of(namescol())) %>%  filter(ID == input$PatID) %>% 
          select(-ID) #
      })
    }else{
      
      right <- reactive({
        gender %>% filter(ID == input$PatID) %>% select(PP_3_Geschlecht_fct) %>% unlist() %>% head(1) %>% 
          as.character()
      })
      
      if(right() == as.character(input$gesch_sel)){
        
        dfs3 <- reactive({
          nontidy_VC_class()[,-c(1,3,96)] %>% filter(ID == input$PatID) %>% select(ID,all_of(namescol())) %>%
            select(-ID)
        })
        
      }else{
        
        dfs3 <- reactive({
          nontidy_VC_class()[,-c(1,3,96)] %>% filter(ID == input$PatID) %>% select(ID,all_of(namescol())) %>%
            select(-ID)
        })
        
      }
    }
    
    dfs4 <- reactive({ as.data.frame(dfs3(), stringAsFactors = F) })
    #dfs5 <- reactive({lapply(dfs4(), as.numeric) })
    
    #tt <- dfs4
    return(dfs4())
})
  
  dataset_groupedall <- eventReactive({
    input$PatID
    nontidy_VC_class()
    }, 
    {
      
      #req(input$PatID, input$gesch_sel, nontidy_VC_class(), namescol())
      
      if(input$gesch_sel == "."){
        
        dfs3a <- reactive({
          nontidy_VC[,-c(1,3,96)] %>% filter(ID == as.character(input$PatID)) %>% 
            select(-c(ID, DATE)) #
        })
      }else{
        
        right <- reactive({
          gender %>% filter(ID == as.character(input$PatID)) %>% select(PP_3_Geschlecht_fct) %>% unlist() %>% head(1) %>% 
            as.character()
        })
        
        if(right() == as.character(input$gesch_sel)){
          
          dfs3a <- reactive({
            nontidy_VC_class()[,-c(1,3,96)] %>% filter(ID == as.character(input$PatID)) %>% #select(ID,namescol()) %>%
              select(-c(ID, DATE))
          })
          
        }else{
          
          dfs3a <- reactive({
            nontidy_VC_class()[,-c(1,3,96)] %>% filter(ID == as.character(input$PatID)) %>% #select(ID,namescol()) %>%
              select(-c(ID, DATE))
          })
          
        }
      }
      
      dfs4a <- reactive({ as.data.frame(dfs3a(), stringAsFactors = F) })
      #dfs5 <- reactive({lapply(dfs4(), as.numeric) })
      
      #tt <- dfs4
      return(dfs4a())
    })
  
  cohort_uplo <- eventReactive({
    
    namescol()
    params_VC_class()
    input$gesch_sel
    namescol()
  },
  {
    
    #req(namescol(),input$gesch_sel, params_VC_class())
    
    if(input$gesch_sel == "."){
      uplo <- params_VC %>% filter(ID %in% c("Mean", "Sdev")) %>% select(c("ID",all_of(namescol())))
    }else{
      uplo <- params_VC_class() %>% filter(ID %in% c("Mean", "Sdev")) %>% select(c("ID",all_of(namescol())))
    }
    
    #uplo <- params_VC %>% select(c("ID",namescol()))
    #uplo <- uplo %>% filter(ID %in% c("Mean", "Sdev"))
    uplo1 <- t(uplo)
    uplo1 <- as.data.frame(uplo1, stringsAsFactors = F)
    names(uplo1) <- uplo1[1,]
    uplo2 <- uplo1[-1,]
    uplo2[] <- lapply(uplo2, as.numeric)
    return(as.data.frame(uplo2, stringsAsFactors = F))
  })
  
  cohort_uploall <- eventReactive({
    
    namescol()
    params_VC_class()
    input$gesch_sel
  },
  {
    
    #req(namescol(),input$gesch_sel, params_VC_class())
    
    if(input$gesch_sel == "."){
      uplo <- params_VC %>% filter(ID %in% c("Mean", "Sdev")) #%>% select(c("ID",namescol()))
    }else{
      uplo <- params_VC_class() %>% filter(ID %in% c("Mean", "Sdev")) #%>% select(c("ID",namescol()))
    }
    
    uplo1 <- t(uplo)
    uplo1 <- as.data.frame(uplo1, stringsAsFactors = F)
    names(uplo1) <- uplo1[1,]
    uplo2 <- uplo1[-1,]
    uplo2[] <- lapply(uplo2, as.numeric)
    return(as.data.frame(uplo2, stringsAsFactors = F))
  })
  
  ms_gr <- eventReactive({dataset_grouped2()},{
    
    ms_gr <- t(dataset_grouped2())
    ms_gr <- as.data.frame(ms_gr, stringsAsFactors = F)
    names(ms_gr) <- c("time1","time2","time3","time4","time5","time6")
    ms_gr$MeanPat <- apply(ms_gr, 1, function(x) { mean(x, na.rm=T) })
    ms_gr$SdevPat <- apply(ms_gr, 1, function(x) { sd(x, na.rm=T) })
    return(ms_gr)
    
  })
  
  ms_grall <- eventReactive({dataset_groupedall()},{
    
    ms_grall <- t(dataset_groupedall())
    ms_grall <- as.data.frame(ms_grall, stringsAsFactors = F)
    names(ms_grall) <- c("time1","time2","time3","time4","time5","time6")
    ms_grall <- ms_grall[-105,]
    ms_grall[] <- lapply(ms_grall, as.numeric)
    ms_grall$MeanPat <- apply(ms_grall, 1, function(x) { mean(x, na.rm=T) })
    ms_grall$SdevPat <- apply(ms_grall, 1, function(x) { sd(x, na.rm=T) })
    
    return(ms_grall)
    
  })
  
  means_grouped <- eventReactive({
    
    ms_gr()
    cohort_uplo()
    },
    {
    
    #req(dataset_grouped2(),cohort_uplo(), input$PatID, ms_gr())
    
    # combine patient data and cohort averages
    msgrt2 <- reactive({ cbind(ms_gr(), cohort_uplo()) })
    
    # generate fill column important for vindex
    msgrt3 <- msgrt2() %>%
      mutate(upperl = Mean + Sdev,
             lowerl = Mean - Sdev,
             fill = ifelse(MeanPat > upperl | MeanPat < lowerl, 1, 0)) 
    
    #test3a <- msgrt3
    return(as.data.frame(msgrt3,stringsAsFactors = F))
    
  })
  
  means_grouped2 <- eventReactive({
    
    ms_grall()
    cohort_uploall()
  },
  {
    
    #req(dataset_grouped2(),cohort_uplo(), input$PatID, ms_gr())
    
    # combine patient data and cohort averages
    msgrt2all <- reactive({ cbind(ms_grall(), cohort_uploall()) })
    
    # generate fill column important for vindex
    msgrt3all <- msgrt2all() %>%
      mutate(upperl = Mean + Sdev,
             lowerl = Mean - Sdev,
             fill = ifelse(MeanPat > upperl | MeanPat < lowerl, 1, 0)) 
    
    #test3a <- msgrt3
    return(as.data.frame(msgrt3all,stringsAsFactors = F))
    
  })
  
  ttt8 <- eventReactive({
    means_grouped() }, 
    {
      
      vv <- vindex(means_grouped())
      return(vv)
      
    })
  ttt9 <- eventReactive({
    means_grouped2() }, 
    {
      
      vv <- vindex(means_grouped2())
      return(vv)
      
    })
  
  titlepat <- eventReactive({
    input$PatID
    input$myPicker
  },{ paste("\nIME Score for ", input$PatID, " on ", input$myPicker, " parameters",sep = "") })
  
  titlepatall <- eventReactive({
    input$PatID
  },{ paste("\nIME Score for ", input$PatID, " on all parameters",sep = "") })
  
  dataset_heat <- eventReactive({
    nontidy_VC_class()
    namescol()
    input$gesch_sel},
    {
    
    if(input$gesch_sel == "."){
      dfs <- nontidy_VC[,-c(3,96,108:110)] %>% select(all_of(namescol()))
    }else{
      dfs <- nontidy_VC_class() %>% select(-c(3,96,108:110)) %>% select(all_of(namescol()))
    }
    
    names(dfs) <- substr(names(dfs), start = 1, stop = 6)
    
    return(dfs)
    
  })
  
  output$trendPlot <- renderPlotly({
    
    req(dataset(), input$x, colnum(), input$color)
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = "ID", color = as.character(input$color))) + 
      geom_point(size = 3, alpha = 0.7, show.legend = T)  +
      #geom_vline(aes(xintercept = as.numeric(params_VC[2,2]))) +
      geom_vline(aes(xintercept = params_VC[2,colnum()] + 1*params_VC[1,colnum()]),linetype='dashed', size = 1, colour = "grey80") +
      geom_vline(aes(xintercept = params_VC[2,colnum()]-1*params_VC[1,colnum()]),linetype='dashed', size = 1, colour = "grey80") +
      geom_vline(aes(xintercept = params_VC[2,colnum()]),colour="red", size = 1) + 
      labs(caption = "vertical red line is mean +/- SD in black") + xlab(input$x) +
      labs(color = as.character(input$color)) +
      theme(legend.position="right")
    
    # if at least one facet column/row is specified, add it
    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p, width = 1000, height = 700) #%>% layout(height = 780, width = "auto") #%>% 
    # layout(legend = list(orientation = 'h'))
    
  })
  
  output$plot2 <- renderPlotly({
    
    cohort_uplo_date <- reactive({
      uplodate <- VC_date_id %>% filter(DESCRIPTION == input$y) 
      #uplodate$VALUE <- as.numeric(uplodate$VALUE)
      uplodate2 <- uplodate %>% spread(DESCRIPTION,VALUE) %>% select(-ID) %>% 
        select(-all_of(c("PP_3_Geschlecht_fct","groupAge","Su_142_Wenn_nicht_moechtest_du_daran_was_aendern_yn_fct"))) %>% 
        group_by(DATE)  %>% 
        summarise_all(list(meanco = mean , sdevco = sd))
      uplodate2 <- uplodate2 %>% mutate(upperlc = meanco + sdevco, lowerlc = meanco - sdevco)
      
      return(uplodate2)
    })
    
    # test <- cohort_uplo_date()
    # test2 <- dataset_long()
    
    #test3 <- inner_join(cohort_uplo_date(), dataset_long(), by = "DATE")
    req(cohort_uplo_date(), dataset_long(), input$y)
    
    qq <- ggplot(dataset_long()) +
      geom_line(aes(x = as_date(DATE), y = as.numeric(VALUE), color = "red"), size = 2, show.legend = F) +
      geom_ribbon(data = cohort_uplo_date(), aes(x=as_date(DATE), ymin = lowerlc, ymax = upperlc, alpha = 0.02, fill = "#F0E442"), 
                  show.legend = F, color = NA) +
      theme(axis.text.x = element_text(angle = 45), text = element_text(size = 12),legend.position = "none") +
      ylab(input$y) + xlab("Time") + 
      ggtitle(paste("Plot Longitudinal ", input$y, " for ", input$PatID, sep = "")) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") 
    
    ggplotly(qq, width = 1000)
    
  })
  
  output$Param <- DT::renderDataTable({
    
    req(selRows())
    
    datatable(cbind(Measure = params_VC[,1],round(params_VC[,-1],2)),
              options = list(searching = TRUE, lengthMenu = 8, "pageLength" = 10, scrollx = '800px', 
                             columnDefs = list(list(width = '150px', targets = "_all"))) ,
              selection = list(mode='multiple',
                               selected = selRows()), rownames = F) 
  })
  
  output$Gauge = renderPlotly({
    
    # apply vindex function
    
    req(ttt8(), titlepat())
    
    pg <- reactive({
      
      req(ttt8(),titlepat())
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = ttt8()*100,
        title = list(text = titlepat()),
        type = "indicator",
        mode = "gauge+number+delta",
        #delta = list(reference = 100),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          steps = list(
            list(range = c(0, 60), color = "orange"),
            list(range = c(60, 90), color = "lightgreen"),
            list(range = c(90, 100), color = "cyan"))), width = "auto") %>% 
        #threshold = list(
        #  line = list(color = "red", width = 6),
        #  thickness = 0.8,
        #  value = 80))) %>%
        layout(margin = list(l=10,r=20, t = -1),
               paper_bgcolor = "lavender",
               font = list(color = "darkblue", family = "Arial"))
    })
    
    pg()
    
  })
  
  output$Gauge2 = renderPlotly({
    
    # apply vindex function
    
    req(ttt8(), titlepatall())
    
    pg <- reactive({
      
      req(ttt9(),titlepatall())
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = ttt9()*100,
        title = list(text = titlepatall()),
        type = "indicator",
        mode = "gauge+number+delta",
        #delta = list(reference = 100),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          steps = list(
            list(range = c(0, 60), color = "orange"),
            list(range = c(60, 90), color = "lightgreen"),
            list(range = c(90, 100), color = "cyan"))), width = "auto") %>% 
        #threshold = list(
        #  line = list(color = "red", width = 6),
        #  thickness = 0.8,
        #  value = 80))) %>%
        layout(margin = list(l=10,r=20, t = -1),
               paper_bgcolor = "lavender",
               font = list(color = "darkblue", family = "Arial"))
    })
    
    pg()
    
  })
  
  
  output$heat1 <- renderPlotly({
    
    zz <- reactive({
      req(dataset_heat())
      zz <- cor.test.p(dataset_heat())
      #ttt <- -log10(zz)
      diag(zz) <- 1
      return(zz)
    })
    
    
    matr <- reactive({
      req(dataset_heat())
      matr <- as.matrix(cor(dataset_heat()))
      tt <- matr
      return(matr)
    })
    
    dend <- reactive({
      req(matr())
      matr() %>% dist %>% hclust %>% as.dendrogram %>% color_branches(3, col = 1:3)
    })
    
    req(zz(), input$class, matr(), dend())
    
    if(input$class == "heatmap"){
      
      p <- reactive({heatmaply(matr(), limits = c(-1,1), Rowv = dend(),show_dendrogram = c(F,T),
                               node_type = input$class,
                               dendrogram = T, 
                               k_col = input$clus, 
                               row_dend_left = F,
                               showticklabels = TRUE,
                               fontsize_row = 11,
                               fontsize_col = 11,
                               subplot_margin = c(0,0,0.03,0)
      )})
      
      p()
      
    }else{
      
      p <- reactive({heatmaply_cor(matr(), limits = c(-1,1), dendrogram = "column",
                               node_type = input$class,
                               point_size_mat = -log10(zz()), 
                               point_size_name = "-log10(p-value)", 
                               label_names = c("x", "y", "Correlation"), 
                               showticklabels = TRUE,
                               fontsize_row = 11,
                               fontsize_col = 11,
                               subplot_margin = c(0,0,0.05,0)
      )})
      
      p()
      
      
    }
    
    
  })
  output$Param2 <- DT::renderDataTable({
    
    req(selRows(), namescol(), params_VC_class())
    
    ddff <- setNames(data.frame(t(params_VC_class()[,-1])), params_VC_class()[,1])
    ddff <- round(ddff,2)
    ddff$ID <- row.names(ddff)
    
    ddff_sel <- reactive({
      ddff %>% filter(ID %in% namescol())
    })
    
    
    datatable(ddff_sel()[,c(9,2)],
              options = list(searching = TRUE, lengthMenu = 8, "pageLength" = 10, scrollx = '800px', 
                             columnDefs = list(list(width = '150px', targets = "_all"))),
              selection = list(mode='multiple',
                               selected = selRows()),
              rownames = F) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
