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


load("../ref_tpx_CT.RData") # load .Rdata or run EDA_home2.R

#nms <- names(params_VC)

#nms2 <- nms[-1]

vindex <- function(df){
  vi <- df %>% select(fill) %>% colSums() %>% as.numeric()
  vi2 <- as.numeric(NROW(df) - vi)/NROW(df) 
  return(vi2)
}


ui <- fluidPage(
  titlePanel(title=div(img(src="fraunhofer_IME-logo_900p.jpg",
                           height="20%", width="20%", align="right"), "Vindex Basics v.2")),
  
  #tags$h2("Vindex Basics v.1"),
  p("An attempt to visualize a virtual cohort with patient-related deviations from means +/- sd and a possible VINDEX score"),
  #hr(),
  sidebarPanel(
    # selectInput('x', 'CohortParameter', choices = names(nontidy_VC)[c(1,3:115)], selected = "P2_12_Tai.cm"),
    # selectInput('y', 'LongitudinalY', choices = names(nontidy_VC)[c(1,3:115)], selected = "KC_68_Trimg..mm"),
    # selectInput('color', 'Color', choices = names(nontidy_VC)[c(1,3:115)], selected = "KC_63_HbA..mmmolHb."),
    selectInput('x', 'CohortParameter', choices =nms2, selected = "P2_12_Taillenumfang_cm"),
    selectInput('y', 'LongitudinalY', choices = nms2, selected = "KC_68_Triglyceride_mgdl_mmoll"),
    selectInput('color', 'Color', choices = nms2, selected = "KC_63_HbA1c_mmol_molHb"),
    selectInput('class', 'Par.Type', choices = names(partype), selected = "PhysPar"),
    #selectInput('facet_row', 'Facet Row', c(None = '.', nms2), selected = "None"),
    #selectInput('facet_col', 'Facet Column', c(None = '.', nms2)),
    selectInput('PatID', 'Patient_no',choices = as.vector(nontidy_VC$ID)[1:100], selected = "Pat_98"),
    selectInput('myPicker','Select groups of parameters', choices = unname(partype), selected = unname(partype)[c(3)])
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("VCoh_Means", plotlyOutput('trendPlot'),width = "auto", height = "800px"),
                tabPanel("Longitudinal",fluidRow(
                  splitLayout(cellWidths = c("100%", "100%"), 
                              plotlyOutput('plot2'))),
                  DT::dataTableOutput(outputId="Param"), width = "700px", height = "800px"),
                #tabPanel("Lollipop", plotOutput("Lollipop"),width = "600px", height = "700px"),
                #tabPanel("Vindex", gaugeOutput('Gauge'), width = "700px", height = "700px"),
                tabPanel("Vindex",fluidRow(
                  splitLayout(cellWidths = c("100%","100%")), 
                              plotlyOutput('Gauge'),plotlyOutput('Gauge2')))
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataset <- eventReactive({
    input$x
    input$color}, {
    
    tt <- VC_date_id %>% filter(DESCRIPTION %in% c(input$x,input$color)) %>%
      spread(DESCRIPTION,VALUE) %>% 
      group_by(ID) %>% 
      summarise(Age = mean(as.numeric(get(input$x)), na.rm = T),
                Colpar = mean(as.numeric(get(input$color))))
    names(tt)[2] <- input$x
    names(tt)[3] <- input$color

    return(tt)
    
  })
  
  dataset_long <- eventReactive({ input$y 
      input$PatID } , {
    
    ll <- VC_date_id %>% filter(DESCRIPTION == input$y) %>% 
      filter(ID == input$PatID)
    
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
    nmcl <- params_VC[,-1] %>% 
      select(map(input$myPicker, 
                 starts_with, 
                 vars = colnames(.)) %>%
               unlist()) %>% names()
      
    return(nmcl)
      
  })
  
  dataset_grouped <- eventReactive(input$PatID, {
      
    dfs <- nontidy_VC %>% select(ID, namescol()) %>% filter(ID == input$PatID) %>% select(-ID)
    dfs2 <- as.data.frame(dfs, stringAsFactors = F)
    dfs2[] <- lapply(dfs2, as.numeric)
    return(dfs2)
  
  })
  
  titlepat <- eventReactive({
      input$PatID
      input$myPicker
    },{ paste("\nVindex Score for ", input$PatID, " on ", input$myPicker, " parameters",sep = "") })
  
  titlepatall <- eventReactive({
      input$PatID
    },{ paste("\nVindex Score for ", input$PatID, " on all parameters",sep = "") })
  
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
      uplodate$VALUE <- as.numeric(uplodate$VALUE)
      uplodate2 <- uplodate %>% spread(DESCRIPTION,VALUE) %>% select(-ID) %>% group_by(DATE)  %>% 
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
    
    datatable(colls[1:116,],
              options = list(searching = TRUE, lengthMenu = 5, "pageLength" = 10, scrollx = '800px', 
                             columnDefs = list(list(width = '150px', targets = "_all"))) ,
              selection = list(mode='multiple',
                               selected = selRows()), rownames = F) 
  })
  
  output$Gauge = renderPlotly({
    
    means_grouped <- reactive({
        
        dataset_grouped2 <- reactive({
          
          req(namescol(), input$PatID)
          
          dfs3 <- nontidy_VC %>% select(ID, namescol()) %>% filter(ID == input$PatID) %>% select(-ID)
          dfs4 <- as.data.frame(dfs3, stringAsFactors = F)
          dfs4[] <- lapply(dfs4, as.numeric)
          return(dfs4)
          
        })
        
        cohort_uplo <- reactive({
          
          req(namescol())
          
          uplo <- params_VC %>% select(c("ID",namescol()))
          uplo <- uplo %>% filter(ID %in% c("Mean", "Sdev"))
          uplo1 <- t(uplo)
          uplo1 <- as.data.frame(uplo1, stringsAsFactors = F)
          names(uplo1) <- uplo1[1,]
          uplo2 <- uplo1[-1,]
          uplo2[] <- lapply(uplo2, as.numeric)
          return(as.data.frame(uplo2, stringsAsFactors = F))
        })
        
        req(dataset_grouped2(),cohort_uplo(), input$PatID)
        
        #cat(file=stderr(), "ecco", as.character(dataset_grouped()[1,1]) , "bins", "\n")
        
        #testa <- cohort_uplo()
        
        ms_gr <- dataset_grouped2()  %>% t()
        ms_gr2 <- as.data.frame(ms_gr, stringsAsFactors = F)
        names(ms_gr2) <- c("time1","time2","time3","time4","time5","time6")
        ms_gr2[] <- lapply(ms_gr2, as.numeric)
        ms_gr2$MeanPat <- apply(ms_gr2, 1, function(x) { mean(x, na.rm=T) })
        ms_gr2$SdevPat <- apply(ms_gr2, 1, function(x) { sd(x, na.rm=T) })
        # combine patient data and cohort averages
        msgrt2 <- cbind(ms_gr2, cohort_uplo())
        
        # generate fill column important for vindex
        msgrt3 <- msgrt2 %>%
          mutate(upperl = Mean + Sdev,
                 lowerl = Mean - Sdev,
                 fill = ifelse(MeanPat > upperl | MeanPat < lowerl, 1, 0)) 
        
        #test3a <- msgrt3
        
        return(as.data.frame(msgrt3,stringsAsFactors = F))
      })
    
    req(means_grouped(), input$PatID, input$myPicker)
    
    # apply vindex function
    
    ttt8 <- reactive({ 
      req(means_grouped())
      vv <- vindex(means_grouped())

      return(vv)
    })

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
    
    means_grouped2 <- reactive({
        
        dataset_grouped3 <- reactive({
          req(input$PatID)
          dfs5 <- nontidy_VC %>% filter(ID == input$PatID) %>% 
          select(-c(ID, DATE, groupAge, Su_142_Wenn_nicht_moechtest_du_daran_was_aendern_yn_fct,PP_3_Geschlecht_fct))
          dfs6 <- as.data.frame(dfs5, stringAsFactors = F)
          dfs6 <- dfs6[,-2] # remove column with GesFact
          dfs6[] <- lapply(dfs6, as.numeric)
          return(dfs6)
          
        })
        
        #testa <- dataset_grouped3()
        
        cohort_uplo2 <- reactive({
          
          uplo3 <- params_VC %>% filter(ID %in% c("Mean", "Sdev"))
          uplo4 <- t(uplo3)
          uplo4 <- as.data.frame(uplo4, stringsAsFactors = F)
          names(uplo4) <- uplo4[1,]
          uplo5 <- uplo4[-1,]
          uplo5[] <- lapply(uplo5, as.numeric)
          return(as.data.frame(uplo5, stringsAsFactors = F))
        })
        
        req(cohort_uplo2(),dataset_grouped3(), input$PatID)
        
        #cat(file=stderr(), "ecco", as.character(dataset_grouped()[1,1]) , "bins", "\n")
        
        #testb <- cohort_uplo2()
        
        ms_gr3 <- dataset_grouped3() %>% t()
        ms_gr3 <- as.data.frame(ms_gr3, stringsAsFactors = F)
        names(ms_gr3) <- c("time1","time2","time3","time4","time5","time6")
        ms_gr3[] <- lapply(ms_gr3, as.numeric)
        ms_gr3$MeanPat <- apply(ms_gr3, 1, function(x) { mean(x, na.rm=T) })
        ms_gr3$SdevPat <- apply(ms_gr3, 1, function(x) { sd(x, na.rm=T) })
        #ms_gr2 <- ms_gr2[,7:8]
        #ms_gr3 <- ms_gr3[-2,] # remove row with GescFct
        
        #test3b <- ms_gr3
        
        # combine patient data and cohort averages
        msgrt3 <- cbind(ms_gr3, cohort_uplo2())
        
        # generate fill column important for vindex
        msgrt4 <- msgrt3 %>%
          mutate(upperl = Mean + Sdev,
                 lowerl = Mean - Sdev,
                 fill = ifelse(MeanPat > upperl | MeanPat < lowerl, 1, 0)) 
        
        #test2 <- msgrt4
        
        return(as.data.frame(msgrt4,stringsAsFactors = F))
      })
    
    req(means_grouped2(), input$PatID, input$myPicker)
    
    # apply vindex function
    
    ttt88 <- reactive({ 
      
      req(means_grouped2())
      vv <- vindex(means_grouped2())
      return(vv)
    })
    
    
    pg2 <- reactive({
      req(ttt88(),titlepatall())
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = ttt88()*100,
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
    
    pg2()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

