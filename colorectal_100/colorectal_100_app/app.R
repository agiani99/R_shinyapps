
library(shiny)
library(tidyverse)
library(DT)
library(gtable)
library(grid)
library(gridExtra)
library(cowplot)
library(scales)
library(d3heatmap)
library(RColorBrewer)


load("Y:/BMBF/ZUKUNFTCluster/Colorectal/colorectal_100_app/reference_100_colorectal.RData")

nms <- names(patients5)
patname <- patients5 %>% filter(!is.na(Name)) %>% select(Name) %>% unique() %>% unlist()

patients_means <- patient2 %>% group_by(Name) %>% select_if(is.numeric) %>% 
  summarise_all(list(mean), na.rm = T) %>% ungroup %>% select(-ID)
ID_name_list <- patients5 %>% group_by(Name) %>% select(Id) %>% unique() %>% filter(!is.na(Name)) %>% ungroup
temp <- observations %>% pivot_wider(names_from = DESCRIPTION, values_from = VALUE)
temp <- inner_join(temp, ID_name_list, by = "Id")
selected_obs <- temp %>% group_by(Name) %>% select(c("Polyp size greatest dimension by CAP cancer protocols",
                                                     "Hemoglobin.gastrointestinal [Presence] in Stool by Immunologic method"
)) %>% 
  summarise_all(list(mean), na.rm = T) %>% ungroup

selected_obs_patients <- inner_join(patients_means, selected_obs, by = "Name")
selected_obs_patients <- inner_join(selected_obs_patients, patients[,c(1,9:12)], by = "Name")
nms_selected <- names(selected_obs_patients)


ui <- fluidPage(
  
  titlePanel(title=div(img(src="fraunhofer IME-logo_900p.jpg",
                           height="20%", width="20%", align="right"), "Cohort 100 Explorer")),
  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min = 1, max = length(table(patients5$Name)),
                value = 20, step = 5, round = 0),
    selectInput('x', 'X/First', choices = nms, selected = "Age"),
    selectInput('y', 'Y/Second', choices = nms, selected = "BMI"),
    selectInput('color', 'Color', choices = nms, selected = "Glucose"),
    
    selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "GENDER"),
    selectInput('facet_col', 'Facet Column', c(None = '.', nms), selected = "MARITAL"),
    selectInput("valID","Individual", choices = sort(as.vector(patname)), selected = "Tod265Casper496"),
    selectInput('Selx', 'Selected X', choices = nms_selected, 
                selected = "EGFR"),
    selectInput('Sely', 'Selected Y', choices = nms_selected, 
                selected = "Polyp size greatest dimension by CAP cancer protocols"),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 100, max = 2000, value = 500)
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("General", plotOutput('trendPlot', height = "600px", width = "500px")),
      tabPanel("Table", dataTableOutput("Table")),
      tabPanel("Outliers", plotOutput("ellipse_plot", height = "800px", width = "500px")),
      tabPanel("Individual", plotOutput("Individual", height = "800px", width = "500px")),
      tabPanel("Correlations", d3heatmapOutput("heatmap", width = "90%", height="600px")),
      tabPanel("Selected plot", plotOutput("Specific", height = "600px", width = "500px"))
    )
  )
)

server <- function(input, output) {
  

  temp <- patients5
  
  dataset <- reactive({
    
    temp2 <- temp %>% filter(!is.na(get(input$color)))
    
    temp2[sample(nrow(temp2), input$sampleSize, replace = T),]
  
    })
  
  
  output$trendPlot <- renderPlot({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point(show.legend = T, size = 4)
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    p <- p + theme_grey() + theme(text=element_text(size=16), legend.position="bottom")
    
    p

  })
  font.size = "10pt"
  
  output$Table <- renderDT({ 
    
    datatable(dataset(), width = "auto", height = "auto", 
              filter="top", selection="multiple", escape=FALSE, 
              options = list(sDom  = '<"top">lrt<"bottom">ip', pageLength = 5,
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                "}"))) 
    })
  
  output$ellipse_plot <- renderPlot({
    
    temp5 <- reactive({
      
      tidyr::gather(temp, name, value, -c(DATE, Name, Id)) %>% 
        filter(name == c(input$x, input$y)) %>% 
        filter(!is.na(Name)) %>%
        group_by(name) %>% 
        summarise(mean = mean(as.numeric(value), na.rm = T),
                                     sd = sd(as.numeric(value), na.rm = T),
                                     threshold = mean + sd,
                                     threshold2 = mean - sd)
      
      
      
    })
    
    t1x <- reactive({temp5() %>% filter(name == input$x) %>% select("threshold") %>% unlist()})
    m1x <- reactive({temp5() %>% filter(name == input$x) %>% select("mean") %>% unlist()})
    t2x <- reactive({temp5() %>% filter(name == input$x) %>% select("threshold2") %>% unlist()})
    t1y <- reactive({temp5() %>% filter(name == input$y) %>% select("threshold") %>% unlist()})
    m2y <- reactive({temp5() %>% filter(name == input$y) %>% select("mean") %>% unlist()})
    t2y <- reactive({temp5() %>% filter(name == input$y) %>% select("threshold2") %>% unlist()})
    
    q1 <- reactive({
      ggplot(dataset(), aes(x = Name, y = get(input$x))) + 
      geom_point(size = 3) + 
      geom_hline(aes(yintercept = t1x()), size = 1, color = "red", linetype="dashed") +
      geom_hline(aes(yintercept = m1x()), size = 1, color = "blue", linetype="dashed") +
      geom_hline(aes(yintercept = t2x()), size = 1, color = "red", linetype="dashed") +
      geom_text(aes(x = Name , y = get(input$x), label=ifelse( get(input$x) > t1x() | get(input$x) < t2x(), 
                                                               as.character(Name),'')),hjust=1,vjust=1) +
      theme_grey() + ylab(input$x) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
      })
    
    q2 <- reactive({ 
      ggplot(dataset(), aes(x = Name, y = get(input$y))) + 
      geom_point(size = 3) + 
      geom_hline(aes(yintercept = t1y()), size = 1, color = "red", linetype="dashed") +
      geom_hline(aes(yintercept = m2y()), size = 1, color = "blue", linetype="dashed") +
      geom_hline(aes(yintercept = t2y()), size = 1, color = "red", linetype="dashed") +
      geom_text(aes(x = Name, y = get(input$y), label=ifelse( get(input$y) > t1y() | get(input$y) < t2y(), 
                                                              as.character(Name),'')),hjust=1,vjust=1) +
      theme_grey() + ylab(input$y) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
      })
    
    plot_grid(q1(), q2(), ncol = 1, align = "h")
    
  })

  output$Individual <- renderPlot({
    
    keyy <- reactive({
      patients5 %>% dplyr::filter(Name == input$valID) %>% select(Id) %>% slice(1) %>% unlist()
      })
    dataset_qol <- reactive({
      patients5 %>% dplyr::filter(DESCRIPTION == "QOL") %>% dplyr::filter(Id == keyy())
      })
    dataset_qaly <- reactive({
      patients5 %>% dplyr::filter(DESCRIPTION == "QALY") %>% dplyr::filter(Id == keyy())
      })
    dataset_daly <- reactive({
      patients5 %>% dplyr::filter(DESCRIPTION == "DALY") %>% dplyr::filter(Id == keyy())
      })
    dataset_polyp <- reactive(patients5 %>% filter(CODE == "33756-8") %>% 
                                mutate( ToHighlight = ifelse( Name == input$valID, "yes", "no" ) ))
    
    qo1 <- reactive({
      ggplot(dataset_qol(), aes(x = DATE, y = VALUE)) + 
        geom_line(linetype="dashed", color="blue", size = 1) + 
        theme_grey() + scale_x_date(date_breaks = "6 month", labels = date_format("%m-%Y")) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("") +
        geom_line(data = dataset_daly(), aes(x = DATE, y = VALUE, linetype="dotted", color="red")) +
        guides(linetype = F, color = F)
      })

    qo2 <- reactive({
      ggplot(dataset_qaly(), aes(x = DATE, y = VALUE)) + 
        geom_line(linetype="dotted", color="red", size = 1) + 
        theme_grey() + scale_x_date(date_breaks = "6 month", labels = date_format("%m-%Y")) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")
      })
    
    qo3 <- reactive({ggplot(dataset_polyp()) +
        geom_col(aes(x = Name, y = VALUE, fill = ToHighlight)) +
        scale_fill_manual( values = c( "yes"="tomato", "no"="gray" ), guide = FALSE ) +
        theme_grey() + ylab("Polyp size in mm") + xlab("Patient Name") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))})
    
    if(length(unique(dataset_polyp()$ToHighlight)) == 1){
      tqs <- reactive({ list(qo1(),qo2()) })
      labelsP = c("QOL in blue & QALY in red","DALY")
      }
    else{
      tqs <- reactive({ list(qo1(),qo2(), qo3()) })
      labelsP = c("QOL in blue & QALY in red","DALY", "Polyp")
    }
    
    plot_grid(plotlist = tqs(), labels = labelsP, ncol = 1, align = "hv", axis = "b")
    })
  
  output$heatmap <- renderD3heatmap({
    
    M <- as.matrix(cor(selected_obs_patients[,-c(1,24:27)], use = "pairwise.complete.obs"))

    d3heatmap(M, dendrogram = "column", digits = 2, col=brewer.pal(9,"Blues"), Colv = T, k_col = 4,
              Rowv = T, xaxis_font_size=10, yaxis_font_size=10)
    
  })
  
  output$Specific <- renderPlot({
    
    # selected_obs_patients_plot <- reactive({
    #   selected_obs_patients %>% filter(!is.na(input$Sely)) %>% as.data.frame(.,stringsAsFactors = F)
    # })
    
    s <- reactive({
      ggplot(selected_obs_patients) + 
      geom_point(aes(get(input$Selx), get(input$Sely), 
                     size = 4, color = GENDER), show.legend = T, na.rm = T) +
      theme_grey() + 
      theme(text=element_text(size=16), 
            axis.text.y = element_text(angle = 90, hjust = 1),
            legend.position="bottom") +
      guides(size = F) +
      xlab(input$Selx) +
      ylab(input$Sely) + 
      geom_smooth(aes(get(input$Selx), get(input$Sely)), method = "lm", se = TRUE)
      
      })
    
    s()
    
    
  })
}

shinyApp(ui, server)
