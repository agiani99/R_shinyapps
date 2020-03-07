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
library(flexdashboard)
library(DT)

load("setup.RData") # load .Rdata or run setup.R



ui <- fluidPage(
  titlePanel(title=div(img(src="fraunhofer_IME-logo_900p.jpg",
                                         height="20%", width="20%", align="right"),"IME Score Basics v.1")),
   
  #tags$h2("Vindex Basics v.1"),
  p("An attempt to visualize a virtual cohort with patient-related deviations from means +/- sd and a possible IME Score"),
  #hr(),
  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(original_VC),
                value = 40, step = 10, round = 0),
    selectInput('x', 'X', choices = nms2, selected = "Alter_Jahre"),
    #selectInput('Dataset', 'Cohort', choices = c("synth","original"), selected = "original"),
    selectInput('y', 'Y', choices = nms2, selected = "ID"),
    selectInput('color', 'Color', choices = nms2, selected = "BMI_kg_sqm"),
    
    #selectInput('facet_row', 'Facet Row', c(None = '.', nms2), selected = "None"),
    #selectInput('facet_col', 'Facet Column', c(None = '.', nms2)),
    selectInput('PatID', 'Patient_no',original_VC$ID, selected = "Pat_014"),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 200, max = 2000, value = 700),
    pickerInput(
      inputId = "myPicker", 
      label = "Select/deselect all + format selected", 
      choices = nms2,
      selected = nms2[c(4:5,9:11,13,51,58,60,61,62,63,67,70,106)],
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  ) ,
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotlyOutput('trendPlot'),width = "600px", height = "700px"),
                tabPanel("Barplot", plotlyOutput('Barplot2'),width = "600px", height = "700px"),
                tabPanel("Lollipop", plotOutput("Lollipop"),width = "600px", height = "700px"),
                #tabPanel("Vindex", gaugeOutput('Gauge'), width = "700px", height = "700px"),
                tabPanel("IME Score",
                         fluidRow(
                           splitLayout(cellWidths = c("100%", "90%"), plotlyOutput('Gauge')), #gaugeOutput('Gauge'))#width = "700px", height = "700px"),
                           DT::dataTableOutput(outputId="Table")
                         ))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  params_VC <- as.data.frame(sapply(original_VC[,c(3,5:117)], 
                                      function(x) c( "Sdev" = sd(x, na.rm = T),"Mean"= mean(x,na.rm=TRUE),
                                                                               "n" = length(x),
                                                                               "Median" = median(x, na.rm = T),
                                                                               #"CoeffofVariation" = sd(x, na.rm = T)/mean(x,na.rm=TRUE),
                                                                               "Minimum" = min(x, na.rm = T),
                                                                               "Maximun" = max(x, na.rm = T),
                                                                               "Upper Quantile" = quantile(x,1, na.rm = T),
                                                                               "LowerQuartile" = quantile(x,0, na.rm = T))), stringsAsFactors = F)
  ss <- row.names(params_VC)
  params_VC$ID <- c("Sdev","Mean","n","Median","Minimum", "Maximum","Upper Quantile", "LowerQuartile")
  params_VC <- params_VC %>% select(ID, everything())
  # datasetInput <- reactive({
  #   switch(input$Dataset,
  #          "original" = original_VC,
  #          "synth" = synth_VC)
  # })
  
  # output$PatID <- reactive({
  #   if(input$Dataset == "original"){
  #     original_VC$ID
  #   }else{
  #     synth_VC$ID
  #   }
  #   #input$Dataset()$ID
  # })
   
  # dataset <- reactive({
  #   if(input$Dataset == "original"){
  #     original_VC[sample(nrow(original_VC), input$sampleSize, replace = T),]}
  #     
  #   else{
  #     synth_VC[sample(nrow(synth_VC), input$sampleSize, replace = T),]}
  #     
  # })
  
  dataset <- reactive({
    or1 <- original_VC[sample(nrow(original_VC), input$sampleSize, replace = T),]
    or2 <- as.data.frame(rbind(or1[,c(1,3,5:117)], params_VC[1:2,]), stringsAsFactors = T)
    or2 <- or2[-c(nrow(or2),nrow(or2)-1),]
  })
  
  # colnum <- reactive({
  #   if(input$Dataset == "original"){
  #     which(names(params_orig) == input$x) }
  #   else{
  #     which(names(params_synth) == input$x)}
  # })
  
  colnum <- reactive({ 
    which(names(params_orig) == input$x)
  })
  
  # dataset_scaled <- reactive({
  #   if(input$Dataset == "original"){
  #     tt <- names(original_VC_scaled) %in% c(input$myPicker)
  #     tt <- match(tt, names(original_VC_scaled_ms))
  #     zz <- c(input$PatID, "Mean", "Sdev")
  #     original_VC_scaled_ms[original_VC_scaled_ms$ID %in% zz , names(original_VC_scaled_ms)[tt]]
  #     }
  #   else{
  #     tt <- names(synth_VC_scaled) %in% c(input$myPicker)
  #     tt <- match(tt, names(synth_VC_scaled_ms))
  #     zz <- c(input$PatID, "Mean", "Sdev")
  #     synth_VC_scaled_ms[synth_VC_scaled_ms$ID %in% zz , names(synth_VC_scaled_ms)[tt]]
  #     }
  # })
  
  dataset_scaled <- reactive({
    tt <- reactive({ names(original_VC_scaled) %in% c(input$myPicker) })
    zz <- reactive({ c(input$PatID, "Sdev", "Mean") })
    ttt1 <- original_VC_scaled_ms[original_VC_scaled_ms$ID %in% zz() , ]
    ttt1 <- ttt1[,names(original_VC_scaled_ms)[tt()]]
  })
  
  center_apply <- function(x) {
    apply(x, 2, function(y) y - mean(y, na.rm = T))
  }
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point(size = 2, alpha = 0.5)  +
      geom_vline(aes(xintercept = params_VC[2,colnum()]+1*params_VC[1,colnum()]),linetype='dashed', size = 1, colour = "grey80") +
      geom_vline(aes(xintercept = params_VC[2,colnum()]-1*params_VC[1,colnum()]),linetype='dashed', size = 1, colour = "grey80") +
      geom_vline(aes(xintercept = params_VC[2,colnum()]),colour="red", size = 1) + 
      labs(caption = "vertical red line is mean +/- SD in black") +
      theme(legend.position="bottom")
    
    # if at least one facet column/row is specified, add it
    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% layout(height = input$plotHeight, width = 600, autosize = T) #%>% 
    # layout(legend = list(orientation = 'h'))
      
  })
  output$Barplot2 <- renderPlotly({
    
    dataset_scaled <- reactive({
      tt <- reactive({ names(original_VC_scaled) %in% c(input$myPicker) })
      zz <- reactive({ c(input$PatID, "Sdev", "Mean") })
      ttt1 <- original_VC_scaled_ms[original_VC_scaled_ms$ID %in% zz() , ]
      ttt1 <- ttt1[,names(original_VC_scaled_ms)[tt()]]
    })
    
    ttt2 <- as.data.frame(t(dataset_scaled()), stringsAsFactors = F)
    names(ttt2)[1] <- input$PatID
    ttt2$vars <- row.names(ttt2)
    ttt3 <- ttt2 %>% 
      mutate(fill = ifelse(get(input$PatID) > (Mean + Sdev) | get(input$PatID) < (Mean - Sdev), "Worse", "Better"),
             fill2 = as.factor(fill))
    
    qq <- ggplot(ttt3 , aes(vars)) + 
      geom_bar(aes(x = vars, y = get(input$PatID), fill = fill2), stat = "identity") + 
      geom_point(aes(x = vars, y = Mean, color = "Mean"), shape = 124, size = 3, show.legend = F) +
      theme(legend.box = "horizontal",legend.key = element_blank(), 
           legend.title = element_blank(), legend.position = "top") +
      scale_fill_manual(values=c("Better" = "lightblue2", "Worse" = "magenta3")) +
      theme(axis.text.x = element_text(angle = 45), text = element_text(size = 14)) + 
      ylab("Scaled Values") + xlab("Parameters") + ggtitle(paste("Plot Selected Parameters for ", input$PatID, sep = "")) +
      guides(size = FALSE, fill = FALSE)
    
    ggplotly(qq) %>% layout(height = input$plotHeight, width = 600, autosize = T)
    #layout(margin = list(l=100))
    
  })
  
  output$Lollipop <- renderPlot({
    
    dataset_scaled <- reactive({
      tt <- reactive({ names(original_VC_scaled) %in% c(input$myPicker) })
      zz <- reactive({ c(input$PatID, "Sdev", "Mean") })
      ttt1 <- original_VC_scaled_ms[original_VC_scaled_ms$ID %in% zz() , ]
      ttt1 <- ttt1[,names(original_VC_scaled_ms)[tt()]]
    })
    
    ttt2 <- as.data.frame(t(dataset_scaled()), stringsAsFactors = F)
    names(ttt2)[1] <- input$PatID
    ttt2$vars <- row.names(ttt2)
    ttt4 <- reactive({
      ttt5 <- ttt2 %>% mutate(diffm = get(input$PatID) - Mean,
                            fill = ifelse(get(input$PatID) > (Mean + Sdev) | get(input$PatID) < (Mean - Sdev),
                                          "Worse", "Better"))
      ttt4 <- ttt5[order(ttt5$diffm), ]
    })

    ggplot(ttt4(), aes(reorder(x=vars,X=diffm), y=diffm, label=round(diffm,2))) +
      geom_point(stat='identity', size=12, aes(color = as.factor(fill)))  +
      geom_segment(aes(y = 0,
                       x = vars,
                       yend = diffm,
                       xend = vars,
                       color = fill)) +
      geom_text(color="white", size=4) +
      scale_fill_manual(values=c("Better" = "lightblue2", "Worse" = "magenta3")) +
      labs(title="Diverging Lollipop Chart",
           subtitle="Colored differences exceeding Mean +/- SD") +
      #ylim = c(-0.9,0.9) +
      ylab("Differences from means") + xlab("Exp. Parameters") +
      guides(size = FALSE, color = FALSE) +
      theme_set(theme_bw(base_size = 16)) +
      theme(legend.position = "top") +
      coord_flip()
    
  })
    
  output$Gauge = renderPlotly({ #renderGauge({
    
    dataset_scaled <- reactive({
      tt <- reactive({ names(original_VC_scaled) %in% c(input$myPicker) })
      zz <- reactive({ c(input$PatID, "Sdev", "Mean") })
      ttt1 <- original_VC_scaled_ms[original_VC_scaled_ms$ID %in% zz() , ]
      ttt1 <- ttt1[,names(original_VC_scaled_ms)[tt()]]
    })
    
    vindex <- function(df){
      
      vi <- df %>% mutate(diff = ifelse(get(input$PatID) > (Mean + Sdev) | get(input$PatID) < (Mean - Sdev), 
                                        1/NROW(.), 0)) %>% select(diff) %>% colSums()
      return(as.numeric(vi))
      
    }
    
    ttt2 <- as.data.frame(t(dataset_scaled()), stringsAsFactors = F)
    names(ttt2)[1] <- input$PatID
    ttt2$vars <- row.names(ttt2)
    
    
    # apply vindex function
    
    ttt8 <- 1 - round(vindex(ttt2),2)
    
    # gauge(ttt8*100, 
    #         min = 0, 
    #         max = 100, 
    #         sectors = gaugeSectors(success = c(70, 100), 
    #                                warning = c(50, 70),
    #                                danger = c(0, 50)),
    #       symbol = "%",
    #       label = "Vindex = Count of\nmagenta columns\nover total")
    # 
    titlepat <- reactive ({ paste("\nIME Score for ", input$PatID, sep = "") })
    
    p <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = ttt8*100,
      title = list(text = titlepat()),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = 80),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, 60), color = "orange"),
          list(range = c(60, 80), color = "lightgreen"),
          list(range = c(80, 100), color = "cyan")),
        threshold = list(
          line = list(color = "red", width = 6),
          thickness = 0.8,
          value = 80))) %>%
      layout(margin = list(l=10,r=20, t = -2),
             paper_bgcolor = "lavender",
             font = list(color = "darkblue", family = "Arial"))
    p
    
    })
  
  output$Table <- DT::renderDataTable({
    dataset_scaled <- reactive({
      tt <- reactive({ names(original_VC_scaled) %in% c(input$myPicker) })
      zz <- reactive({ c(input$PatID, "Sdev", "Mean") })
      ttt1 <- original_VC_scaled_ms[original_VC_scaled_ms$ID %in% zz() , ]
      ttt1 <- ttt1[,names(original_VC_scaled_ms)[tt()]]
    })
    ttt2 <- as.data.frame(t(dataset_scaled()), stringsAsFactors = F)
    names(ttt2)[1] <- input$PatID
    ttt2$vars <- row.names(ttt2)
    ttt2$vars[10] <- "Glucose-oGTT_Test_mg_dl_mmol_l"
    ttt2$vars[8] <- "Gesamt-Protein_g_dl_g_l"
    ttt2$vars[7] <- "Pankreatisch-a-Amylase_Plasma_Serum_U_l"
    #head(ttt2, n = nrow(ttt2))
    ttt9 <- reactive({
      ttt2 %>% 
      mutate(fill = ifelse(get(input$PatID) > (Mean + Sdev) | get(input$PatID) < (Mean - Sdev),0,1),
             Params = sub("_.*","", vars)) %>% 
        select(Params, everything()) %>% select(-vars) %>% mutate_if(is.numeric, round, 2)
        
    })
    datatable(head(ttt9()[,1:5],nrow(ttt9())), 
              options = list(searching = FALSE, lengthMenu = 5, "pageLength" = 8,
              columnDefs = list(list(width = '10px', targets = c(2:5)))) , rownames = TRUE) %>% 
      formatStyle(input$PatID, 'fill', backgroundColor = styleEqual(c(0,1), c("orange", "lightblue")))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

