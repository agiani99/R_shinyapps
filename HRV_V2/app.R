#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(extrafont)
loadfonts(device = "win")
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(cowplot)
library(caret)
library(tidyr)
library(broom)
library(psych)
library(ggpubr)
library(shinythemes)
library(ggthemes)
library(shinyWidgets)
library(tidyr)
library(flexdashboard)
library(plotly)
library(DT)
library(wesanderson)

load("HRV_V2_w_msd.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(title=h2('Probands RR report',align="center"),windowTitle="Fraunhofer IME SP App"),
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput('x', 'Proband RR spectrum:',choices = all23$ID, 
                  selected = '8511801330_2018-08-15_11-14-47', width = '100%'),
      selectInput('y', 'Parameter to plot vs time:', choices = c("RR_ms","bpm"),
                  selected = 'bpm', width = '100%'),
      selectInput('z', 'Parameter to plot vs bpm or boxplot:',
                  choices = names(temp), selected = 'RR_ms', width = '100%'),
      selectInput("period", "Observation Period", choices = c("Awake", "Sleep", "All"), 
                  selected = "All", width = '100%'),
      pickerInput(inputId = "myPicker", label = "Select/deselect all + format selected", 
        choices = nms_scaled,
        selected = nms_scaled[c(20,29,30,31,32,33,34,11,35,8)],
        options = list(`actions-box` = TRUE,
                       size = 10,
                       `selected-text-format` = "count > 3"), multiple = TRUE)
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot Single", plotOutput("PlotRR")),
        tabPanel("Correlations ", plotOutput("PlotCO")),
        tabPanel("Box Plot ", plotOutput("PlotCOBox")),
        tabPanel("PCA ", plotOutput("PlotPCA")),
        #tabPanel("Data",tableOutput("Table2")),
        tabPanel("Relax Level",
                 fluidRow(column(12, "Total",gaugeOutput('Gauge'),
                                 splitLayout(cellWidths = c("95%", "95%"),
                                             fluidRow(column(6,"Physical",gaugeOutput('plotphys')),
                                                      column(6,"Psychic",gaugeOutput('plotpsych'))))
                          )
                )
        )
        
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  temp <- all23 %>% select_if(is.numeric)
  
  theme_white <- function (base_size = 12, base_family = "Helvetica") {
    half_line <- base_size/2
    theme(line = element_line(colour = "white", size = 0.5, linetype = 1, lineend = "butt"), 
          rect = element_rect(fill = "white", colour = "transparent", size = 0.5, linetype = 1), 
          text = element_text(family = base_family,
                              face = "plain", colour = "white", size = base_size, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0, 
                              margin = margin(), debug = FALSE), 
          axis.line = element_line(color = "white"), 
          axis.line.x = element_line(color = "white"),
          axis.line.y = element_line(color = "white"),
          axis.text = element_text(size = rel(0.8), colour = "white"), 
          axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1), 
          axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1), 
          axis.ticks = element_line(colour = "white"), 
          axis.ticks.length = unit(half_line/2, "pt"), 
          axis.title.x = element_text(margin = margin(t = 0.8 * half_line, b = 0.8 * half_line/2)), 
          axis.title.y = element_text(angle = 90, 
                                      margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)), 
          legend.background = element_rect(fill = "transparent"), 
          legend.margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2), 
          legend.key = element_rect(fill = "transparent", colour = "white"), 
          legend.key.size = unit(1.2, "lines"), 
          legend.key.height = NULL, 
          #legend.box.margin = margin(),
          legend.key.width = NULL, legend.text = element_text(size = rel(1)), 
          legend.text.align = NULL, legend.title = element_text(hjust = 0), 
          legend.title.align = NULL, legend.position = "top", 
          legend.direction = NULL, legend.justification = "center", 
          panel.background = element_rect(fill = "transparent",colour = NA), 
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "grey90"), 
          panel.grid.minor = element_line(colour = "grey50", size = 0.25), 
          #panel.margin = unit(half_line, "pt"), 
          #panel.margin.x = NULL, 
          #panel.margin.y = NULL, panel.ontop = FALSE, 
          strip.background = element_rect(fill = "transparent", colour = NA), 
          strip.text = element_text(colour = "white",size = rel(0.8)), 
          strip.text.x = element_text(margin = margin(t = half_line, b = half_line)), 
          strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)), 
          strip.switch.pad.grid = unit(0.1, "cm"), 
          strip.switch.pad.wrap = unit(0.1, "cm"), 
          plot.background = element_rect(colour = "transparent"), 
          plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 1.2)),
          plot.caption = element_text(hjust = 0, colour = "yellow", size = 16),
          plot.margin = margin(half_line, half_line, half_line, half_line), complete = TRUE)
  }

  dfname <- reactive({
    get(paste('Prob_', input$x, sep = ""))
   })
    
  output$PlotRR <- renderPlot({
    #par(mar = c(4, 4, 0, 0))
    ggplot(dfname(),aes(OrigTime, get(input$y), color = period )) + 
      geom_line(aes(group = 1), size = 2) + ylab(input$y) +
      scale_x_datetime(date_breaks = "2 hour") +
      labs(title = paste("Spectrum of Prob_", input$x, " - ", all23[all23$ID == input$x, 2]$class,sep = "")) +
      theme_minimal(base_size = 18) +
      theme(legend.position="top",legend.title = element_blank(), 
           legend.text = element_text(colour = "white"),
           plot.title=element_text(family="Helvetica", face="bold", size=18, color = "white"),
           axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
           axis.text.y = element_text(angle = 0, size=13 , color = "white"),
           axis.title.x = element_text(size=14, face="bold", vjust = -1, color = "white"),
           axis.title.y = element_text(size=14, face="bold", vjust = 2, color = "white"))
    },width = 600, height = 700, bg = "transparent")#,execOnResize = TRUE)
    
  output$PlotCO <- renderPlot({
    #par(mar = c(4, 4, .1, .1))
    
    temp2 <- reactive({
      if(input$period == "All"){
        temp <- all23 %>% select_if(is.numeric)
        temp$class <- all23$class
      }else{
        if(input$period == "Awake"){
          temp <- day23 %>% select_if(is.numeric)
          temp$class <- all23$class
        }else{
          temp <- night23 %>% select_if(is.numeric)
          temp$class <- all23$class
        }
      }
      return(temp)
    })
    
    p <- reactive({
      ggscatter(temp2(), x = "bpm", y = input$z, size = 4, shape = "class", color = "class",
                palette = "dark2", add = "reg.line", conf.int = T) + xlab("bpm") + 
        ylab(input$z) +
        theme_white() +
        theme(plot.background = element_rect(fill = "#333366",
                                        colour = "#333366",
                                        size = 0.5, linetype = "solid")) +
        stat_cor(aes(color = class, label =paste(..rr.label.., 
                                                 cut(..p.., breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                                     labels = c("'p-value ****'", 
                                                                "'p-value ***'",
                                                                "'p-value **'",
                                                                "'p-value *'", "'ns'")), sep = "~`,`~")),
                 label.x = 82) + guides(shape = FALSE, color = FALSE)
    })
    
    p()
    
    }, width = 600, height = 700, bg = "transparent")#, execOnResize = TRUE)
    
    
  output$PlotCOBox <- renderPlot({
    
    temp4 <- reactive({
      if(input$period == "All"){
        temp <- all23 %>% select_if(is.numeric)
        temp$class <- all23$class
      }else{
        if(input$period == "Awake"){
          temp <- day23 %>% select_if(is.numeric)
          temp$class <- all23$class
        }else{
          temp <- night23 %>% select_if(is.numeric)
          temp$class <- all23$class
        }
      }
      return(temp)
    })
    
    
    
    pp <- reactive({
      ggplot(temp4(), aes(x = class, y = get(input$z),  fill = class, colour = "yellow")) +
        geom_boxplot(show.legend = F, outlier.colour = "cyan", outlier.shape = 19, 
                     outlier.size = 6, outlier.fill = "cyan",
                     notch = FALSE) + labs(title = paste("Boxplot for ", input$z, " for ", input$period, " values", sep = "")) +
        xlab("Stress Level") + ylab(input$z) + #guides(colour = FALSE) +
        labs(caption = paste(input$period, " values taken into account",sep = "")) +
        theme_white() + 
        theme(plot.background = element_rect(fill = "#333366",
                                              colour = "#333366",
                                              size = 0.5, linetype = "solid")) + # Discrete color
        #scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))
        scale_fill_manual(values = wes_palette("Moonrise3", n = 3))
      
      
      
    })
    
    plot(pp())
    
  }, width = 600, height = 700, bg = "transparent")
    
  output$PlotPCA <- renderPlot({
    
    temp3 <- reactive({
      if(input$period == "All"){
        temp <- all23 %>% select_if(is.numeric)
      }else{
        if(input$period == "Awake"){
          temp <- day23 %>% select_if(is.numeric)
          
        }else{
          temp <- night23 %>% select_if(is.numeric)
          
        }
      }
      return(temp)
    })

    PCA <- prcomp(temp3(), scale. = T, center = T)
    
    # Extract PC axes
    PCAvalues <- data.frame(Species = all23$class, PCA$x, ID = all23$ID, stringsAsFactors = F)
    
    # Extract loadings of the variables
    PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)
    
    # calculate percentage for axes
    percentage <- round(PCA$sdev / sum(PCA$sdev) * 100, 2)
    percentage <- paste(colnames(PCA$x), "(var.explained ", paste( as.character(percentage), "%", ")", sep="") )
    
    # Plot
    ggplot(PCAvalues[,c(1:3,25)], aes(x = PC1, y = PC2, colour = Species)) +
      stat_ellipse(level = 0.95, size = 1, show.legend = FALSE) +
      geom_point(size = 5) +
      xlab(percentage[1]) + ylab(percentage[2]) +
      theme_white() +
      theme(plot.background = element_rect(fill = "#333366",
                                            colour = "#333366",
                                            size = 0.5, linetype = "solid"))
    

  },width = 600, height = 700)
    
    # output$downloadReport <- downloadHandler(
    #   filename = function() {
    #     paste('my-report', sep = '.', switch(
    #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    #     ))
    #   },
    #   
    #   content = function(file) {
    #     src <- normalizePath('report.Rmd')
    #     
    #     # temporarily switch to the temp dir, in case you do not have write
    #     # permission to the current working directory
    #     owd <- setwd(tempdir())
    #     on.exit(setwd(owd))
    #     file.copy(src, 'report.Rmd', overwrite = TRUE)
    #     
    #     library(rmarkdown)
    #     out <- render('report.Rmd', switch(
    #       input$format,
    #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
    #     ))
    #     file.rename(out, file)
    #   }
    # )
    
    
  dataset_scaled_ps <- eventReactive({
    input$x},
    {
      
      tt <- reactive({ names(all23_scaled) %in% names(all23_scaled)[c(20,29,30,31,32,33,34,11,35,8)] })
      zz <- reactive({ c(input$x, "Sdev", "Mean") })
      ttt1 <- all23_scaled_ms[all23_scaled_ms$ID %in% zz() , ]
      ttt1 <- ttt1[,names(all23_scaled_ms)[tt()]]
      return(ttt1)
    })
    
  ttt10 <- eventReactive({
    dataset_scaled_ps()
    input$PatID},
    { 
      
      temp <- as.data.frame(t(dataset_scaled_ps()), stringsAsFactors = F)
      names(temp)[1] <- input$PatID
      temp$vars <- row.names(temp)
      temp <- temp %>% mutate(condition = ifelse(get(input$PatID) > (Mean + Sdev) | get(input$PatID) < (Mean - Sdev),0,1)) %>%
        select(vars, everything())
      
      t <- temp
      
      return(temp)
    })
    
    
  output$Gauge = renderGauge({
    
    req(input$x)
    
    dataset_scaled <- reactive({
      tt <- reactive({ names(all23_scaled) %in% names(all23_scaled)[c(20,29,30,31,32,33,34,11,35,8)] })
      zz <- reactive({ c(input$x, "Sdev", "Mean") })
      ttt1 <- all23_scaled_ms[all23_scaled_ms$ID %in% zz() , ]
      ttt1 <- ttt1[,names(all23_scaled_ms)[tt()]]
    })
    
    vindex <- function(df){
      
      vi <- df %>% mutate(diff = ifelse(get(input$x) > (Mean + Sdev) | get(input$x) < (Mean - Sdev), 
                                        1/NROW(.), 0)) %>% select(diff) %>% colSums()
      return(as.numeric(vi))
      
    }
    
    ttt2 <- as.data.frame(t(dataset_scaled()), stringsAsFactors = F)
    names(ttt2)[1] <- input$x
    ttt2$vars <- row.names(ttt2)
    
    
    # apply vindex function
    
    ttt8 <- 1 - round(vindex(ttt2),2)
    
    gauge(ttt8*100, 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(80, 100), 
                                 warning = c(60, 80),
                                 danger = c(0, 60)),
          symbol = "%",
          label = "Score = \n\nNumber of\n Mean-SD >MEASUREMENTS> Mean+SD\nover total no. of measurements")
    
  })
    
  output$plotphys = renderGauge({
    
    req(input$x)
    
    dataset_scaled_phy <- reactive({
      tt <- reactive({ names(all23_scaled) %in% names(all23_scaled)[c(8,20,33,34)] })
      zz <- reactive({ c(input$x, "Sdev", "Mean") })
      ttt1 <- all23_scaled_ms[all23_scaled_ms$ID %in% zz() , ]
      ttt1 <- ttt1[,names(all23_scaled_ms)[tt()]]
    })
    
    vindex <- function(df){
      
      vi <- df %>% mutate(diff = ifelse(get(input$x) > (Mean + Sdev) | get(input$x) < (Mean - Sdev), 
                                        1/NROW(.), 0)) %>% select(diff) %>% colSums()
      return(as.numeric(vi))
      
    }
    
    tttphys <- as.data.frame(t(dataset_scaled_phy()), stringsAsFactors = F)
    names(tttphys)[1] <- input$x
    tttphys$vars <- row.names(tttphys)
    
    
    # apply vindex function
    
    ttt8 <- 1 - round(vindex(tttphys),2)
    
    gauge(ttt8*100, 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(75, 100), 
                                 warning = c(50, 75),
                                 danger = c(0, 50)),
          symbol = "%",
          label = "Score = \n\nNumber of\n Mean-SD >PHYSICAL MEAS.s> Mean+SD\nover total")
    
  })
    
  output$plotpsych = renderGauge({
    
    req(dataset_scaled_ps(), input$x)
    
    vindex <- function(df){
      
      vi <- df %>% mutate(diff = ifelse(get(input$x) > (Mean + Sdev) | get(input$x) < (Mean - Sdev), 
                                        1/NROW(.), 0)) %>% select(diff) %>% colSums()
      return(as.numeric(vi))
      
    }
    
    tttpsych <- as.data.frame(t(dataset_scaled_ps()), stringsAsFactors = F)
    names(tttpsych)[1] <- input$x
    tttpsych$vars <- row.names(tttpsych)
    
    
    # apply vindex function
    
    ttt8 <- 1 - round(vindex(tttpsych),2)
    
    gauge(ttt8*100, 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(90, 100), 
                                 warning = c(60, 90),
                                 danger = c(0, 60)),
          symbol = "%",
          label = "Score = \n\nNumber of\n Mean-SD >PSYCHIC MEAS.s> Mean+SD\nover total")
    
  })
    
  output$Table2 <- renderTable({ 
    
    req(ttt10())
    
    head(ttt10(), n = NROW(ttt10()) )},hover = TRUE,rownames = TRUE,digits = 2)
    
    # output$Table <- DT::renderDT({
    #   
    #   options(DT.options = list(pageLength = 5))
    #   
    #   req(ttt10(), input$PatID)
    #   
    #   datatable(as.data.frame(ttt10()[,-1]), rownames = ttt10()[,1],options = list(searching = FALSE,
    #                                                                 columnDefs = list(list(width = '50px',
    #                                                                                        targets = "_all")))) %>% 
    #     formatStyle(input$PatID, 'condition', color='white',
    #                 backgroundColor = styleEqual(c(0,1), c("orange", "lightblue")))
    #   
    #   #toprint()
    #   
    #})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)



