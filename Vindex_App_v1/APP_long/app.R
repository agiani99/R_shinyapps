
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
library(fmsb)

load("../longitudinal_assembly.RData")

nms2 <- as.character(unique(extra_DB_2$variable))
nms <-  extra_DB_2 %>% filter(substring(Control_Group,1,1) != "T") %>% 
  filter(Dropout_03_2022 != "ja") %>%  
  dplyr::select(Patient_ID) %>% unique() %>% unlist() %>% unname()
nmsnumeric <- extra_DB_2 %>% filter(class == "numeric") %>% dplyr::select(variable) %>% unique() %>% 
    unlist() %>% 
    as.character() %>% sort()



# dataset_pca <- extra_DB_2 %>% 
#   filter(class == "numeric") %>% 
#   filter(!is.na(value)) %>% dplyr::select(-class) %>% 
#   dplyr::filter(substr(variable,1,3) != "Wie") %>% 
#   dplyr::filter(substr(variable,1,5) != "Haben") %>% 
#   dplyr::filter(substr(variable,1,3) != "Mit") %>% 
#   dplyr::distinct(variable, value, Timestamp, .keep_all = TRUE) %>% 
#   pivot_wider(.,values_from = value, names_from = variable)
#   
#names(dataset_pca)
#table(dataset_pca$Control_Group)

#dataset_pca[, c(5:6,9:176)] <- lapply(dataset_pca[, c(5:6,9:176)], as.numeric)

#not_all_na <- function(x) any(!is.na(x))
#dataset_pca <- dataset_pca %>% select(where(not_all_na))



ui <- fluidPage(
  
  titlePanel(title=div(img(src="ITMP_logo.png", #height="30%", width="20%", 
                             "Vindex Data Basic v.2",align="right"))),
  p("An attempt to visualize longitudinal measurements"),
    #hr(),
  sidebarPanel(
        # selectInput('x', 'CohortParameter', choices = names(nontidy_VC)[c(1,3:115)], selected = "P2_12_Tai.cm"),
        # selectInput('y', 'LongitudinalY', choices = names(nontidy_VC)[c(1,3:115)], selected = "KC_68_Trimg..mm"),
        selectInput('x', 'CohortParameter.1', choices = nmsnumeric, selected = nmsnumeric[124]),
        selectInput('color', 'CohortParameter.2', choices =nmsnumeric, selected = nmsnumeric[125]),
        selectInput('PatID', 'Training Patient_no',choices = nms, multiple = F,selected = nms[c(1)]),
        checkboxInput("hline",label = "Graph lines on/off", value = FALSE),
        checkboxInput("ribbon", label = "Range/Mean+/-SD on/off", value = T)),
  mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Longitudinal", plotOutput('trendPlot', height = "700px")),
                    tabPanel("Details", gt_output("summary")),
                    #tabPanel("Boxplot", plotOutput("Boxplot")),
                    tabPanel("BoxPlot",
                             fluidRow(
                               plotOutput("Boxplot", height = "500px"),
                               gt_output("gtsummary"))),
                    tabPanel("Radar", plotOutput("Radar1", height = "600px", width = "700px")))
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
            
            tt <- extra_DB_2 %>% filter(variable %in% c(input$x,input$color)) %>% 
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

            ll <- extra_DB_2 %>% filter(variable %in% c(input$x,input$color)) %>%
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
    
    extrainfo <- eventReactive({
      
      input$PatID
      dataset()
      } , {
      
      info <- dataset() %>% select(Kohorte) %>% distinct()
      
      return(info)
      })
    
    
    radar_df <- eventReactive({ 
      
      input$PatID
      
      }, {
        
        averagesPAT <- extra_DB_2 %>% 
          filter(Patient_ID == input$PatID) %>% 
          filter(class== "numeric") %>% 
          dplyr::select(-all_of(c("class", "Control_Group", "Dropout_03_2022", "Timestamp"))) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::mutate(variable = as.character(variable)) %>% 
          dplyr::mutate(value = as.numeric(value)) %>% 
          pivot_wider(names_from = variable, values_from = value, values_fn = ~ mean(.x)) %>% 
          summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
        
        return(averagesPAT)
        
      })
    
    radar_final <- eventReactive({
      
      radar_df()
      input$PatID
      
    }, {
      
      colsel <- intersect(names(averagesDB),names(radar_df()))
      colsel2 <- intersect(names(minnDB), names(maxnDB))
      colsel3 <- intersect(colsel, colsel2)
      
      colradarelisa <- c("Timestamp","MNGDB_SpO2..SauerstoffsÃ.ttigung.....","STUDYDB_Blutdruck_Systolic",
                         "STUDYDB_Blutdruck_Diastolic", "STUDYDB_Blutzucker",  "SECA_BMI.value", "Level",
                         "MNGDB_Mittelwert_Rechts" , "MNGDB_cLac..L....Lactat.Konzentration...mmol.l." )
      
      colsel4 <- intersect(colradarelisa, colsel3)
      
      
      aves <- averagesDB[, colsel4]
      pates <- radar_df()[,colsel4]
      mines <- minnDB[,colsel4]
      maxes <- maxnDB[,colsel4]
      
      rdr <- rbind(mines, maxes, aves, pates)
      
      # colradar <- c("Alter", "SECA_Height.value", "SECA_Weight.value" , "SECA_FMI.value", "SECA_Total.Body.Water.value", "STUDYDB_Blutdruck_Systolic",
      #               "STUDYDB_Blutdruck_Diastolic",
      #               "SECA_Relative.fat.mass.value", "SECA_BMI.value", "STUDYDB_Koerpertemperatur")
      
      
      
      rdr <- rdr[,colsel4]
      rdr
      
      
      df_scaled <- round(apply(rdr, 2, scales::rescale), 2)
      df_scaled <- as.data.frame(df_scaled)
      
      row.names(df_scaled) <- c("Min","Max","Average", input$PatID)
      
      return(df_scaled)
      
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
           scale_y_continuous(breaks = seq(-5, 200, by = 10)) +
           labs(color = "Measurements") +
           ggtitle(paste("Longitudinal Plot of ",nrow(dataset()), " points of chosen variables\n",
                         input$x, "\nand\n", input$color, " for Patient ", input$PatID, " from Cohort ", extrainfo(), 
                         " (Areas are taken from Control Group data)",
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
   
   output$summary <- render_gt({
     
     dataset_merged() %>% select(-class) %>% 
       filter(!is.na(value)) %>% dplyr::distinct(value, Timestamp, .keep_all = TRUE) %>% 
       mutate(Timestamp = ymd_hms(Timestamp), date = as.Date(Timestamp)) %>% dplyr::select(-Timestamp) %>% 
       dplyr::select(Patient_ID, Gender, date, variable, value) %>% 
       arrange(date) %>% 
       gt() %>% tab_header(
         title = md("**Summary of chosen parameters**")#,
         #subtitle = ""
       ) %>% 
       tab_options(
         table.font.size = "12px"
       )
     
   })
   
   dataset_true <- eventReactive({
     input$x
     input$color} , {
       
       ll <- extra_DB_2 %>% filter(variable %in% c(input$x,input$color)) %>%
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
       
       mm <- full_join(dataset(), dataset_true(), by = c("Patient_ID","Gender","variable", "value", "Control_Group")) %>% 
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
   
   schulnote <- eventReactive({
     input$PatID
   },{
     
     le <- Vindex_data_provided_quick_check %>% filter(Patient_ID == input$PatID) %>% select(Level) %>% unlist() %>% unname()
     #unname(unlist(Vindex_data_provided_quick_check[which(Vindex_data_provided_quick_check$Patient_ID == Input$PatID),9]))
     return(le)
   })
   
   
   output$Radar1 <- renderPlot({
     
    
     for (i in 4:nrow(radar_final())) {
       radarchart(
         radar_final()[c(1:3, i), ],
         pfcol = c("#99999980",NA),
         pcol= c(NA,2), plty = 1, plwd = 2,
         title = paste("Proband ", row.names(radar_final())[i], " with score ", 
                       schulnote(), 
                       sep = "")
       )
     }
     
     
      
   })
   
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)





