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
library(gt)
library(gtsummary)
library(ggforce)
library(ggpubr)
library(scales)


load("../merging_longitudinal_app.RData")

nms2 <- as.character(unique(extra_DB$variable))
nms <-  extra_DB %>% filter(substring(Control_Group,1,1) != "T") %>% dplyr::select(Patient_ID) %>% unique() %>% unlist() %>% unname()
nmsnumeric <- extra_DB %>% filter(class == "numeric") %>% dplyr::select(variable) %>% unique() %>% 
    arrange(variable) %>% 
    unlist() %>% 
    as.character()

dataset_pca <- extra_DB %>% 
  filter(class == "numeric") %>% 
  filter(!is.na(value)) %>% dplyr::select(-class) %>% 
  dplyr::filter(substr(variable,1,3) != "Wie") %>% 
  dplyr::filter(substr(variable,1,5) != "Haben") %>% 
  dplyr::filter(substr(variable,1,3) != "Mit") %>% 
  dplyr::distinct(variable, value, Timestamp, .keep_all = TRUE) %>% 
  pivot_wider(.,values_from = value, names_from = variable)
  
#names(dataset_pca)
#table(dataset_pca$Control_Group)

dataset_pca[, 5:87] <- lapply(dataset_pca[, 5:87], as.numeric)

not_all_na <- function(x) any(!is.na(x))
dataset_pca <- dataset_pca %>% select(where(not_all_na))

ui <- fluidPage(
  
  titlePanel(title=div(img(src="ITMP_logo.png", #height="30%", width="20%", 
                             "Vindex Data Basic v.2",align="right"))),
  p("An attempt to visualize longitudinal measurements"),
    #hr(),
  sidebarPanel(
        # selectInput('x', 'CohortParameter', choices = names(nontidy_VC)[c(1,3:115)], selected = "P2_12_Tai.cm"),
        # selectInput('y', 'LongitudinalY', choices = names(nontidy_VC)[c(1,3:115)], selected = "KC_68_Trimg..mm"),
        selectInput('x', 'CohortParameter.1', choices = nmsnumeric, selected = "Blutdruck_Diastolic"),
        selectInput('color', 'CohortParameter.2', choices =nmsnumeric, selected = "Blutdruck_Systolic"),
        selectInput('PatID', 'Training Patient_no',choices = nms, multiple = F,selected = nms[c(1)]),
        checkboxInput("hline",label = "Graph lines on/off", value = FALSE),
        checkboxInput("ribbon", label = "Range/Mean+/-SD on/off", value = T)),
  mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Longitudinal", plotOutput('trendPlot')),
                    tabPanel("Details", tableOutput("summary")),
                    #tabPanel("Boxplot", plotOutput("Boxplot")),
                    tabPanel("BoxPlot",
                             fluidRow(
                               plotOutput("Boxplot"),
                               gt_output("gtsummary"))))
        )
)#,
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }")


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dataset <- eventReactive({
        input$x
        input$color
        input$PatID}, {
            
            tt <- extra_DB %>% filter(variable %in% c(input$x,input$color)) %>% 
                filter(Patient_ID %in% c(input$PatID)) %>% 
                mutate(Timestamp = ymd_hms(Timestamp)) %>%
                arrange(Timestamp) %>% mutate(value = as.numeric(value))
            
            # tt <- extra_DB %>% filter(variable %in% c("Blutdruck_Systolic","Blutdruck_Diastolic")) %>%
            #     filter(Patient_ID %in% c(nms[1])) %>%
            #     mutate(Timestamp = ymd_hms(Timestamp)) %>%
            #     #mutate(date = as.Date(Timestamp)) %>%
            #     arrange(Timestamp) %>% mutate(value = as.numeric(value))

                
            return(tt)
            
        })
    
    dataset_x <- eventReactive({
        input$x
        input$color} , {

            ll <- extra_DB %>% filter(variable %in% c(input$x,input$color)) %>%
                filter(substring(Control_Group,1,1) == "T") %>% 
                dplyr::select(-all_of(c("Gender", "class"))) %>%
                mutate(Timestamp = ymd_hms(Timestamp), date = as.Date(Timestamp)) %>%
                group_by(variable) %>% 
                mutate(maxvalue = max(as.numeric(value), na.rm = T), 
                       minvalue = min(as.numeric(value), na.rm=T),
                       means = mean(as.numeric(value), na.rm = T),
                       stdv = sd(as.numeric(value), na.rm = T) ) %>% 
              ungroup() %>% 
              dplyr::select(value, variable, maxvalue, minvalue, means, stdv)
                
            
            # ll <- extra_DB %>% filter(variable %in% c("Blutdruck_Systolic","Blutdruck_Diastolic")) %>%
            #   filter(substring(Control_Group,1,1) == "T") %>%
            #   dplyr::select(-all_of(c("Gender", "class"))) %>%
            #   mutate(Timestamp = ymd_hms(Timestamp), date = as.Date(Timestamp)) %>%
            #   group_by(variable) %>% 
            #   mutate(maxvalue = max(as.numeric(value), na.rm = T), 
            #          minvalue = min(as.numeric(value), na.rm=T),
            #          means = mean(as.numeric(value), na.rm = T),
            #          stdv = sd(as.numeric(value), na.rm = T) ) %>% 
            #   ungroup() %>% 
            #   dplyr::select(value, variable, maxvalue, minvalue, means, stdv)

            return(ll)

        })

    dataset_merged <- eventReactive({
        dataset()
        dataset_x()} , {
            
            mm <- merge(dataset(), dataset_x(), by = c("variable", "value")) %>% distinct()

            return(mm)

        })
    
   output$trendPlot <- renderPlot({
       
       #req(dataset(), input$hline)
        
        # build graph with ggplot syntax
       
       p <- ggplot(dataset_merged(), aes(x = Timestamp, y = as.numeric(value), color = variable),
                   alpha = 0.7, show.legend = T) +
           geom_point(size = 3) +
           theme(legend.position = "bottom") +
           scale_x_datetime(name= "TIme", date_breaks="weeks", labels=date_format("%m-%Y"))+
           theme(axis.text.x = element_text(angle = 45, hjust=1)) +
           xlab("Time") + ylab("Value")+
           scale_y_continuous(breaks = seq(60, 200, by = 20)) +
           labs(color = "Measurements") +
           ggtitle(paste("Longitudinal Plot of ",nrow(dataset()), " points of chosen variables\n",
                         input$x, "\nand\n", input$color, " for Patient ", input$PatID, " (Areas are taken from Control Group data)",
                         sep = "")) 
           
      if (input$hline) {
           p <- p + geom_line(size = 1)
      }else{ p }
       
       
      if(input$ribbon) {
        p + geom_ribbon(aes(x=Timestamp, ymax = as.numeric(maxvalue), ymin = as.numeric(minvalue), fill = variable), alpha=.1,
                       show.legend = F)
        }
       else{
        p + geom_ribbon(aes(x=Timestamp, ymax = as.numeric(means) + as.numeric(stdv), ymin = as.numeric(means) - as.numeric(stdv), 
                            fill = variable), alpha=.1, show.legend = F)
         }
  })
   
   output$summary <- renderTable({
     
     dataset_merged() %>% select(-class) %>% 
       filter(!is.na(value)) %>% dplyr::distinct(value, Timestamp, .keep_all = TRUE) %>% 
       mutate(Timestamp = ymd_hms(Timestamp), date = as.Date(Timestamp)) %>% dplyr::select(-Timestamp) %>% 
       dplyr::select(Patient_ID, Gender, date, variable, value, minvalue, maxvalue) %>% 
       arrange(date) %>% 
       gt()
     
   })
   
   dataset_true <- eventReactive({
     input$x
     input$color} , {
       
       ll <- extra_DB %>% filter(variable %in% c(input$x,input$color)) %>%
         filter(substring(Control_Group,1,1) == "T") %>% 
         dplyr::select(-all_of(c("class"))) %>%
         mutate(Timestamp = ymd_hms(Timestamp), date = as.Date(Timestamp)) %>% filter(!is.na(value)) %>% mutate(value = as.numeric(value))
       
       
       # ll <- extra_DB %>% filter(variable %in% c("Blutdruck_Systolic","Blutdruck_Diastolic")) %>%
       #     filter(substring(Control_Group,1,1) == "T") %>%
       #     dplyr::select(-all_of(c("class"))) %>%
       #     mutate(Timestamp = ymd_hms(Timestamp), date = as.Date(Timestamp)) %>% filter(!is.na(value)) %>% mutate(value = as.numeric(value))

       return(ll)
       
     })
   
   dataset_merged_true <- eventReactive({
     dataset()
     dataset_true()} , {
       
       mm <- full_join(dataset(), dataset_true(), by = c("Patient_ID","Gender","variable", "value", "Control_Group")) %>% distinct() %>%
         dplyr::select(variable, value, Control_Group) %>% arrange(desc(Control_Group)) 
       
       return(mm)
       
     })
   
   
   output$Boxplot <- renderPlot({
     
     
     w <- dataset_merged_true() %>% mutate(value = as.numeric(value)) %>% 
       ggboxplot(x = "Control_Group", y = "value", add = "jitter", facet.by="variable", legend="none", 
                 color = "Control_Group") +
       stat_compare_means(aes(label = ..p.signif..),
                          method = "t.test", ref.group = "False",
                          comparisons=list(c(1,2))) +
       scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))+
       facet_wrap_paginate( ~ variable, nrow = 1, ncol = 2, page = 1, scales = "free_y") #+
       
       
     w + coord_cartesian(xlim=c(1,2), clip="off") + 
       annotate("text", x=1.5, y=49, label=paste("Control                             ", input$PatID, sep = ""))  
     
     
     
   })
   
   gt_tbl <- eventReactive({
     input$x
     input$color
     dataset_merged_true()
   },{
     ww <- dataset_merged_true() %>% 
       dplyr::mutate(value = as.numeric(value)) %>%
       group_by(variable, Control_Group) %>% 
       dplyr::summarize(mean = mean(as.numeric(value), na.rm = T), 
                        stdev = sd(as.numeric(value), na.rm = T), .groups = "drop") %>% mutate(stdev = ifelse(is.na(stdev), 0, stdev)) %>% 
       gt %>% 
       tab_header(
         title = md("**Summary of chosen variables**"),
         subtitle = paste("Mean and stdev for ", input$x, " and ", input$color, sep = "")
       ) %>%
       tab_stubhead(label = "Control") %>% 
       tab_spanner(
         label = "Statistics",
         columns = c(mean, stdev)
       ) %>% 
       fmt_number(
         columns = c(mean, stdev),
         decimals = 2,
         use_seps = T
       )
     
     return(ww)
   })
   
   output$gtsummary <- render_gt(
     
     
     expr = gt_tbl(),
     width = px(700),
     height = px(700)
     
     
   )
   
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)


