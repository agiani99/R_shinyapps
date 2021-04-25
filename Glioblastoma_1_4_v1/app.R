library(shiny)
library(tidyverse)
library(readxl)
library(survival)
library(survminer)
library(corrr)
library(reshape2)
library(ggforce)
library(gridExtra)

load("VC_Glioblastoma_1_4.RData")

#local functions

set.seed(1206)

ui <- fluidPage(
    
    # Application title
    #titlePanel("How some parameters affect Hospitalization ditribution in Takoua VC Script"),
    titlePanel(title=div(img(src="ITMP_logo.png",
                             height="12%", width="12%", align="right"),
                         "How some parameters affect Cohort time distribution in Takoua VC Script")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Number of patients:", min = 100, max = 2000, value = 1000),
            sliderInput("vv", "Number of visits:", min = 2, max = 10, value = 7),
            sliderInput("factorz", "Average perturbation", min = 0.5, max = 1.5, value = 1),
            downloadButton("downloadData", "Download as CSV")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
            column(12, plotOutput("distPlot")),
            column(6, plotOutput("Preal")),column(6, plotOutput("KM_plot"))
            )
        )
    )
)

server<- function(input,output,session){
    
    start_cohort <- as.Date("2021-01-01")
    end_cohort <- as.Date("2026-01-01")
    
    
    test <- reactive({
        
        n = input$n
        vv = input$vv
        factorz = input$factorz
        m = nchar(as.character(n))
        
        ID <- paste("Pat_", formatC(seq(1:n),width=m,format='f',digits=0,flag='0'), 
                    sep = "") #patient codes
        
        Gender <- sample(c(1,2), n, replace = T, prob = c(absnumberprop$Gender[1], absnumberprop$Gender[2] ))

        Age_class <- Basic_parameter %>% select(starts_with("Age_group_")) %>% filter(row_number() == 3) %>% 
            mutate_all(., probo ) %>% as.vector()
        
        Age_group <- sample(c("Young_18_49", "Middle_50_59","Old_60_79"), n, replace = T, 
                            prob = Age_class[1,])
        
        Age <- ifelse(Age_group == "Young_18_49", sample(seq(18,49), as.vector(Age_class$Age_group_y)*n, replace = T),
                      ifelse(Age_group == "Middle_50_49", sample(seq(50,59), as.vector(Age_class$Age_group_m)*n, replace = T),
                             sample(seq(60,79), as.vector(Age_class$Age_group_o)*n, replace = T)))
        
        VC_glioblastoma <- data.frame(matrix(ncol = 3, nrow = n), stringsAsFactors = F)
        colnames(VC_glioblastoma) <- c("PATIENT_ID", "Age_group", "Gender")
        
        VC_glioblastoma$PATIENT_ID <- ID
        VC_glioblastoma$Age <- Age
        VC_glioblastoma$Age_group <- Age_group
        VC_glioblastoma$Gender <- Gender
        
        outcome <- list()

        for(i in 1:n) {
            # here define the outcome
            if(VC_glioblastoma$Age_group[i] == "Young_18_49"){
                outcome[i] <- sample(c("Grad_I","Grad_IV"), 1, 
                                     prob = c(absnumberprop$Age_group_y[1]*factorz, 1-absnumberprop$Age_group_y[1]*factorz))
                coutcome <- outcome[i]
            }else {
                if(VC_glioblastoma$Age_group[i] == "Middle_50_59"){
                    outcome[i] <- sample(c("Grad_I","Grad_IV"), 1, 
                                         prob = c(absnumberprop$Age_group_m[1]*factorz, 1-absnumberprop$Age_group_m[1]*factorz))
                    coutcome <- outcome[i]
                }else {
                    outcome[i] <- sample(c("Grad_I","Grad_IV"), 1, 
                                         prob = c(absnumberprop$Age_group_o[1]*factorz, 1-absnumberprop$Age_group_o[1]*factorz))
                    coutcome <- outcome[i]
                }
            }
            outcome <- c(outcome, coutcome)
        }
        
        outcome <- unlist(outcome)
        outcome <- outcome[-n+1]
        rm(coutcome)
        VC_glioblastoma$outcome <- outcome
        
        prop_location <- Basic_parameter %>% select(starts_with("loc_")) %>% filter(row_number() == 3) %>%
            mutate_all(., probo ) %>% as.vector()
        
        location <- ifelse(VC_glioblastoma$outcome == "Grad_IV",
                           sample(c("Frontal", "Temporal","Parietal","Occipital", "Cerebellum", "Other"), n, replace = T,
                                  prob = prop_location[1,]*absnumber$Outcome[2]),
                           sample(c("Frontal", "Temporal","Parietal","Occipital", "Cerebellum", "Other"), n, replace = T,
                                  prob = prop_location[1,]*absnumber$Outcome[1]))
        
        VC_glioblastoma$location <- location
        VC_glioblastoma$outcome <- ifelse(VC_glioblastoma$location == "Cerebellum", sample(c("Grad_I","Grad_IV"),
                                                                                           prop_location$loc_cerebellum*n, replace = T), 
                                          VC_glioblastoma$outcome)
        
        VC_glioblastoma_long <- data.frame(PATIENT_ID = character(0),
                                           Age = numeric(0),
                                           Gender = character(0),
                                           Age_class = character(0),
                                           location = character(0),
                                           outcome = character(0),
                                           Visits_date = as.Date(character(0)),
                                           Perf_status = numeric(0),
                                           Focal_deficit = numeric(0),
                                           Headache = numeric(0),
                                           stringsAsFactors = F
                                           )
        
        
        for(i in seq(1:n)){
            
            patient <- VC_glioblastoma[i,]
            
            if(patient$outcome == "Grad_IV"){
                #vv <- sample(2:7, 1, replace = F)
                start_cohort2 <- as.Date(start_cohort + (sample(seq(8:50),1)*10))
                end_cohort2 <- as.Date(start_cohort2 + (sample(seq(8:50),1)*54))
                if(difftime(end_cohort2, end_cohort, units = "weeks")  > 0){
                    end_cohort2 <- as.Date(end_cohort)
                }
                patient <- do.call("rbind", replicate(11, patient, simplify = FALSE))
                Visits_date <- sort(sample(seq(start_cohort,end_cohort2, by="weeks"), vv, replace = F))
                vs <- sort(sample(c("N", "Y"), vv, replace = T))
                pp <- 11 - vv
                vs1 <- rep(NA, pp)
                Visits_date <- as.Date(c(Visits_date, vs1))
                patient$Visits_date <- Visits_date
                patient$Focal_deficit <- c(vs, vs1)
                patient$Headache <- c(vs, vs1)
                #Perf_status <- rep(sample(c(1:4),1,prob = c(.33,.36,.30,.18), replace = T),vv)
                Perf_status <- sort(sample(c(0:4),vv,prob = c(.33,.36,.20,.08,.03), replace = T), decreasing = T)
                Perf_status <- c(Perf_status,vs1)
                patient$Perf_status <- Perf_status
                
            }else{
                #vv <- sample(2:10, 1, replace = F)
                start_cohort2 <- as.Date(start_cohort + (sample(seq(8:50),1)*10))
                end_cohort2 <- as.Date(start_cohort2 + (sample(seq(8:50),1)*54))
                if(difftime(end_cohort2, end_cohort, units = "weeks")  > 0){
                    end_cohort2 <- as.Date(end_cohort)
                }
                patient <- do.call("rbind", replicate(11, patient, simplify = FALSE))
                Visits_date <- sort(sample(seq(start_cohort,end_cohort2, by="weeks"), vv, replace = F))
                vs <- sort(sample(c("N", "Y"), vv, replace = T))
                pp <- 11 - vv
                vs1 <- rep(NA, pp)
                Visits_date <- as.Date(c(Visits_date, vs1))
                patient$Visits_date <- Visits_date
                patient$Focal_deficit <- c(vs, vs1)
                patient$Headache <- c(vs, vs1)
                #Perf_status <- rep(sample(c(0:3),1,prob = c(.46,.30,.08,.03), replace = T),vv)
                Perf_status <- sort(sample(c(0:3),vv,prob = c(.58,.25,.15,.03), replace = T),decreasing = T)
                Perf_status <- c(Perf_status,vs1)
                patient$Perf_status <- Perf_status
            }
            
            VC_glioblastoma_long <- rbind(VC_glioblastoma_long, patient)
        }
        
        VC_glioblastoma_long <- VC_glioblastoma_long %>%  filter(!is.na(Visits_date))
        
        rm(patient);rm(vv);rm(vs);rm(vs1);rm(pp)
        
        VC_glioblastoma_long_fact <- VC_glioblastoma_long %>% 
            select_at(c("Age_group", "outcome", "location", "Focal_deficit", "Headache")) %>% 
            mutate_all(as.factor)
        
        VC_glioblastoma_long_num <- VC_glioblastoma_long %>% 
            select(-c("Age_group", "outcome", "location", "Focal_deficit", "Headache", "Visits_date", "PATIENT_ID"))
        
        VC_glioblastoma_long2 <- VC_glioblastoma_long %>% select(PATIENT_ID, Visits_date)
        
        VC_glioblastoma_long2 <- cbind(VC_glioblastoma_long2, VC_glioblastoma_long_fact, VC_glioblastoma_long_num)
        VC_glioblastoma_long3 <- VC_glioblastoma_long2 %>% filter(!is.na(Visits_date)) %>% 
            group_by(PATIENT_ID) %>%
            mutate(
                startpat = min(Visits_date),
                endpat = max(Visits_date),
                cohorttime = as.numeric(difftime(endpat, startpat, units = "weeks")),
                outcome = as.numeric(outcome),
                Age_group = as.numeric(Age_group),
                location = as.numeric(location),
                Headache = as.numeric(Headache),
                Focal_deficit = as.numeric(Focal_deficit)) %>% 
            top_n(n=1, cohorttime) %>% ungroup()
        
        test <- as.data.frame(VC_glioblastoma_long3, stringsAsFactors =F)

        
    })
    
    testsave <- reactive({
        
        VC_glioblastoma_long %>% filter(!is.na(Visits_date)) %>% 
            group_by(PATIENT_ID) %>%
            mutate(
                startpat = min(Visits_date),
                endpat = max(Visits_date),
                cohorttime = as.numeric(difftime(endpat, startpat, units = "weeks"))) %>% 
            top_n(n=1, cohorttime) %>% ungroup()
    })
    
    #Kaplan-Meier
    
    pvc <- reactive({    
        
        surv_object <- Surv(time = test()$cohorttime, event = test()$outcome)
        #surv_object <- Surv(time = test$hospitalization, event = test$outcome)
        # dependence on Gender
        
        fit2 <- survfit(surv_object ~ Age_group, data = test())
        #fit2 <- survfit(surv_object ~ Age_class, data = test)
        
        plot(fit2, col = 1:3,lty=1:3, fun = "surv", lwd = 3,
             main="Kaplan Meier survival plot for above virtual cohort\nand relative distribution",
             xlab = "Cohort weeks",
             ylab = "Overall survival probability",
             ylim = c(0, 1),
             xlim= c(0, 250))
        axis(side=1,at=c(seq(from=0,to=250,by=25)))
        legend(1, .3, c("Middle", "Old", "Young"), col = 1:3, lty = 1:3)
        abline(h = 0.5, col = 'darkblue', lwd = 1, lty = 2)
        t <- as.data.frame(summary(fit2)$table)
        for (a in 1:length(t$median)){
            segments(t$median[a],-.05, t$median[a], 0.5, col = 'darkblue', lwd = 1, lty = 2)
        
        #plot(test()$hospitalization/86400, test()$outcome)

    })
    
    pReal <- reactive({
        
        surv_object <- Surv(time = test()$cohorttime, event = test()$outcome)
        #surv_object <- Surv(time = test$hospitalization, event = test$outcome)
        # dependence on Gender
        
        fit3 <- survfit(surv_object ~ location, data = test())
        #fit2 <- survfit(surv_object ~ Age_class, data = test)
        
        plot(fit3, col = 2:7,lty=2:7, fun = "surv", lwd = 2,
             main="Kaplan Meier survival plot for above virtual cohort\nand relative distribution",
             xlab = "Cohort weeks",
             ylab = "Overall survival probability",
             ylim = c(0, 1),
             xlim= c(0, 250))
        axis(side=1,at=c(seq(from=0,to=250,by=25)))
        legend(1, .35, c("Cerebellum","Frontal","Occipital","Other","Parietal","Temporal"), col = 2:7, lty = 2:7)
        abline(h = 0.5, col = 'darkblue', lwd = 1, lty = 2)
        tt <- as.data.frame(summary(fit3)$table)
        for (a in 1:length(tt$median)){
            segments(tt$median[a],-.05, tt$median[a], 0.5, col = 'darkblue', lwd = 1, lty = 2)
        
        
    })
    
    output$distPlot <- renderPlot(
        
        test() %>%
            select_at(c("PATIENT_ID","cohorttime", "Gender", "Age_group", "outcome")) %>% 
            ggplot() +
            geom_density((aes(cohorttime, fill = as.factor(outcome))), alpha = 0.6) +
            guides(fill=guide_legend(title="Outcome 2=Grad_IV 1=Grad_I")) +
            xlab("Cohort weeks") +
            scale_x_continuous(breaks=seq(0, 250, 12))+
            theme(legend.position="bottom") +
            ggtitle(paste("Distribution of Survival weektime for ", input$n, " patients using ", 
                          input$vv, " virtual lab visits", sep = ""))
        
        
        
    )
    
    
    output$KM_plot <- renderPlot(
        
        pvc()
        
    )
    
    output$Preal <- renderPlot(
        
        pReal()
        
    )
    
    
    
    # Downloadable csv of selected dataset ----
    
    output$downloadData <- downloadHandler(
               
        filename = function(){
            a <- paste("test_",format(Sys.time(), "%b_%d_%X"),".csv", sep = "")
            gsub(":", "", a)
        },
        content = function(file) {
            write.csv(testsave(), file, row.names = FALSE)
        }
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
