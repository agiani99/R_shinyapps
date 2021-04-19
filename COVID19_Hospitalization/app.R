#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# reference data here: https://github.com/HAIRLAB/Pre_Surv_COVID_19/tree/master/data

library(shiny)
library(tidyverse)
library(survival)
library(survminer)
library(corrr)
library(reshape2)
library(ggforce)
library(gridExtra)
library(lubridate)

shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))

load("J:/esp/Personal/Andrea Zaliani/takoua/Covid_VC_params_v4.RData")

complement <- function(y, rho, x) {
    if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
    y.perp <- residuals(lm(x ~ y))
    rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

propo <- function(x, na.rm = T) (x / sum(x, na.rm = na.rm))

voutcome <- function(vv){
    hh <- floor(vv/2)
    vout <- c(rep(0,hh),rep(1,vv-hh))
    return(vout)
    
}

set.seed(1206)
rho = -0.7

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("How chosen parameters affect hospitalization \nand Meier-Kaplan analysis in Virtual Cohort generation vs Real-Data_World"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Number of patients:", min = 100, max = 800, value = 375),
            sliderInput("vv", "Number of visits:", min = 2, max = 20, value = 6),
            sliderInput("maxvv", "Date range max available", min = 1, max = 20, value = 10)#,
            # Button
            #downloadButton("downloadData", "Download as CSV")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(12, plotOutput("distPlot")),
                column(6, plotOutput("Preal")),column(6, plotOutput("KMeierPlot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server<- function(input,output,session){
    
    start_cohort <- as.Date("2020-01-10 15:52:20")
    end_cohort <- as.Date("2020-03-04 16:21:51")
    
    test <- reactive({
        
        n = input$n
        #n = 375
        m = nchar(as.character(n))
        
        ID <- paste("Pat_", formatC(seq(1:n),width=m,format='f',digits=0,flag='0'), 
                    sep = "") #patient codes
        
        Gender <- sample(c(1,2), n, replace = T, prob = c(male_percentage, 1- male_percentage))
        #Gender <- sample(c(1,2), n, replace = T, prob = c(0.6, 0.4))
        
        males <- table(Gender)[1]
        females <- table(Gender)[2]
        
        # by eyes with means and sd
        Age_per_gender <- ifelse(Gender == 1, 
                                 c(floor(rnorm(0.8*males, mean = 67, sd = 11)),
                                   floor(rnorm(0.2*males, mean = 30, sd = 10))), 
                                 c(floor(rnorm(0.6*females, mean = 67, sd = 11)),
                                   floor(rnorm(0.4*females, mean = 31, sd = 9))))
        
        Age_per_gender <- ifelse(Age_per_gender < 18, 18, Age_per_gender)
        
        VC_375 <- data.frame(matrix(ncol = 3, nrow = n), stringsAsFactors = F)
        colnames(VC_375) <- c("PATIENT_ID", "age", "gender")
        
        VC_375$PATIENT_ID <- ID
        VC_375$age <- Age_per_gender
        VC_375$gender <- Gender
        
        VC_375 <- VC_375 %>% 
            mutate(Age_class = as.numeric(as.factor(ifelse(age > 60, "Old",ifelse(age < 35,"Young", "Middle")))))
        
        agetable <- round(table(test_covid_sv$Age_class, test_covid_sv$outcome)/nrow(test_covid_sv),3)
        middleage_prob_male <- agetable[1]
        middleage_prob_female <- agetable[4]
        oldeage_prob_male <- agetable[2]
        oldeage_prob_female <- agetable[5]
        totmiddleage <- middleage_prob_male + middleage_prob_female
        totoldeage <- oldeage_prob_male + oldeage_prob_female
        
        probmiddlemale <- middleage_prob_male/totmiddleage # probability of outcome middleaged men
        probmiddlefemale <- middleage_prob_female/totmiddleage # probability of outcome middleaged women
        proboldmale <- oldeage_prob_male/totoldeage  # probability of outcome old men
        proboldfemale <- oldeage_prob_female/totoldeage # probability of outcome old women
        
        VC_375 <- VC_375 %>% 
            rowwise() %>% 
            mutate(outcome = ifelse(gender == 1 & Age_class == 3 , sample(c(0,1), 1, prob = c(0.97,0.03)), 
                                    ifelse(gender == 2 & Age_class == 3 , sample(c(0,1), 1, prob = c(0.97,0.03)),
                                           ifelse(gender == 1 & Age_class == 1 , 
                                                  sample(c(0,1), 1, prob = c(probmiddlemale,1-probmiddlemale)), 
                                                  ifelse(gender == 2 & Age_class == 1 , 
                                                         sample(c(0,1), 1, prob = c(probmiddlefemale,1-probmiddlefemale)),
                                                         ifelse(gender == 1 & Age_class == 2 ,
                                                                sample(c(0,1), 1, prob = c(proboldmale,1-proboldmale)),
                                                                sample(c(0,1), 1, prob = c(proboldfemale,1-proboldfemale))))))))
        
        
        VC_375 <- VC_375 %>% ungroup()
        
        
        # correlations <- correlate(test_covid %>%
        #                               select_if(., is.numeric) %>%
        #                               select(-PATIENT_ID),
        #                           method = "pearson", use = "pairwise.complete.obs")
        
        output <- data.frame(matrix(ncol = length(colnames(maxtestmale))+6, nrow = 1))
        colnames(output) <- c(colnames(VC_375),colnames(maxtestmale), "Visits_date")
        output <- output %>% select(PATIENT_ID, Visits_date, everything())
        output$Visits_date <- as.Date(output$Visits_date, tz="UTC")
        
        rhos <- correlations$outcome[-c(1:3)]
        
        maxvv <- as.integer(input$maxvv)
        #maxvv = 10
        vv <- as.integer(input$vv)
        #vv <- 6
        
        for(i in 1:n){
            
            #maxvv = 10
            #vv <- sample(c(5:8), 1)
            
            patient <- VC_375[i,]
            
            if(VC_375$outcome[i] == 0){
                #vv <- sample(2:6, 1, replace = F)
                patient <- do.call("rbind", replicate(vv, patient, simplify = FALSE))
                deltas <- rnorm(cumsum(vv))
                Visits_date <- sort(start_cohort + (deltas*maxvv*80000))
                patient$Visits_date <- Visits_date
            }else{
                #vv <- sample(2:6, 1, replace = F)
                patient <- do.call("rbind", replicate(vv, patient, simplify = FALSE))
                
                deltas <- rnorm(cumsum(vv))
                if(VC_375$Age_class[i] == 1){
                    Visits_date <- sort(start_cohort +  (deltas*maxvv*40000))
                }else{
                    Visits_date <- sort(start_cohort + 86400 + (deltas*maxvv*20000))
                }
                patient$Visits_date <- Visits_date
            }
            datarnd <- as.data.frame(matrix(ncol = 1, nrow = vv))
            
            if(VC_375$gender[i] == 1){
                
                for(col in seq(1:length(colnames(maxtestmale)))){
                    if(rhos[col]<-0.6 | rhos[col]>0.6){
                        #cdatarnd <- sample(c(mintest[1,col], maxtest[1,col]), vv, replace = T)
                        cdatarnd <- floor(abs(complement(rnorm(vv,mean = as.numeric(durchtestmale[1,col]),sd = as.numeric(trysdm[1,col])),
                                                         rhos[col],voutcome(vv))))
                        #cdatarnd <- ifelse(cdatarnd < 0, as.numeric(durchtestmale[1,col]), cdatarnd)
                        cdatarnd <- sort(unlist(cdatarnd), decreasing = sample(c(F,T), 1))
                        datarnd <- cbind(datarnd, cdatarnd)
                    }else{
                        #cdatarnd <- rnorm(vv, mean = as.numeric(durchtestmale[1,col]), sd = as.numeric(sdtestmale[1,col]))
                        cdatarnd <- rnorm(vv, mean = as.numeric(durchtestmale[1,col]), sd = as.numeric(trysdm[1,col]))
                        #cdatarnd <- ifelse(cdatarnd < 0, as.numeric(durchtestmale[1,col]), cdatarnd)
                        cdatarnd <- sort(unlist(cdatarnd), decreasing = sample(c(F,T), 1))
                        datarnd <- cbind(datarnd, cdatarnd)
                    }
                }
                
                datarnd <- datarnd[,-1]
                colnames(datarnd) <- colnames(maxtestmale)
                datarnd$Visits_date <- Visits_date
                datarnd$PATIENT_ID <- ID[i]
                
                datarnd <- datarnd %>% select(PATIENT_ID, Visits_date, everything())
                #datarnd$Platelet_count <- ifelse(datarnd$Platelet_count <0,0,datarnd$Platelet_count)
                patient2 <- merge(patient, datarnd, by.y = c("PATIENT_ID", "Visits_date"))
                if(i == 1){ 
                    output <- merge(output[-1,], patient2, all = T)
                }else{
                    output <- merge(output, patient2, all = T)
                }
                
            }else{
                
                for(col in seq(1:length(colnames(maxtestmale)))){
                    if(rhos[col]<-0.6 | rhos[col]>0.6){
                        cdatarnd <- floor(abs(complement(rnorm(vv,mean = as.numeric(durchtestfemale[1,col]),sd = as.numeric(trysdm[1,col])),
                                                         rhos[col],voutcome(vv))))
                        cdatarnd <- ifelse(cdatarnd < 0, as.numeric(durchtestfemale[1,col]), cdatarnd)
                        cdatarnd <- sort(unlist(cdatarnd), decreasing = sample(c(F,T), 1))
                        datarnd <- cbind(datarnd, cdatarnd)
                    }else{
                        #cdatarnd <- rnorm(vv, mean = as.numeric(durchtestfemale[1,col]), sd = as.numeric(sdtestfemale[1,col]))
                        cdatarnd <- rnorm(vv, mean = as.numeric(durchtestfemale[1,col]), sd = as.numeric(trysdm[1,col]))
                        cdatarnd <- ifelse(cdatarnd < 0, as.numeric(durchtestfemale[1,col]), cdatarnd)
                        cdatarnd <- sort(unlist(cdatarnd), decreasing = sample(c(F,T), 1))
                        datarnd <- cbind(datarnd, cdatarnd)
                    }
                }  
                
                datarnd <- datarnd[,-1]
                colnames(datarnd) <- colnames(maxtestfemale)
                datarnd$Visits_date <- Visits_date
                datarnd$PATIENT_ID <- ID[i]
                
                datarnd <- datarnd %>% select(c(PATIENT_ID, Visits_date, everything()))
                #datarnd$Platelet_count <- ifelse(datarnd$Platelet_count <0,0,datarnd$Platelet_count)
                patient2 <- merge(patient, datarnd, by.y = c("PATIENT_ID", "Visits_date"))
                if(i == 1){
                    output <- merge(output[-1,], patient2, all = T)
                }else{
                    output <- merge(output, patient2, all = T)
                }
                
            }
        }
        
        VC_375_data <- output
        
        rm(patient);rm(patient2);rm(datarnd);rm(cdatarnd);rm(output)
        
        VC_375_data_KM <- VC_375_data %>% #select(PATIENT_ID, Visits_date, outcome, age, gender) %>% 
            group_by(PATIENT_ID) %>% 
            mutate(start = min(Visits_date),
                   end = max(Visits_date),
                   hospitalization = as.numeric(end - start, units = "days")) %>% 
            select(-start, -end) %>% 
            ungroup() %>% #select(PATIENT_ID, hospitalization, outcome, age) %>%
            mutate(Age_class = as.numeric(as.factor(ifelse(age > 60, "Old", ifelse(age < 35,"Young", "Middle"))))) %>% 
            ungroup()
        
        VC_375_data_KM <- VC_375_data_KM %>% select(-Visits_date) %>% mutate(hospday = hospitalization/86400)
        
    })
    
    test2 <- reactive({
        
        test2 <- test() %>%  select(PATIENT_ID, hospday, outcome, age, gender, hospitalization, Age_class)
        test2 <- as.data.frame(test2, stringsAsFactors =F)
    })
    
    test3 <- reactive({
        test() %>% select(-hospitalization)
    })

    #Kaplan-Meier
    
    pvc <- reactive({    
        
        surv_object <- Surv(time = test2()$hospitalization/86400, event = test2()$outcome)
        #surv_object <- Surv(time = test$hospitalization, event = test$outcome)
        # dependence on Gender
        
        fit2 <- survfit(surv_object ~ Age_class, data = test2())
        #fit2 <- survfit(surv_object ~ Age_class, data = test)
        
        plot(fit2, col = 1:3,lty=1:3, fun = "surv", lwd = 3,
             main="Kaplan Meier survival plot for above virtual cohort and relative distribution",
             xlab = "Hospitalization days",
             ylab = "Overall survival probability",
             ylim = c(0, 1),
             xlim= c(0, 35))
        axis(side=1,at=c(seq(from=0,to=120,by=5)))
        legend(1, .3, c("Middle", "Old", "Young"), col = 1:3, lty = 1:3)
        abline(h = 0.5, col = 'cyan', lwd = 1)
        
        #plot(test()$hospitalization/86400, test()$outcome)
        
        
        
    })
    
    preal <- reactive({
        
        surv_object2 <- Surv(time = test_covid_sv$hospitalization, event = test_covid_sv$outcome)
        
        # dependence on Gender
        
        fit3 <- survfit(surv_object2 ~ Age_class, data =test_covid_sv)
        
        plot(fit3, col = 1:3,lty=1:3, fun = "surv", lwd = 3, 
             main="Kaplan Meier survival plot for COVID-19 study",
             xlab = "Hospitalization days", 
             ylab = "Overall survival probability")
        axis(side=1,at=c(seq(from=0,to=120,by=5))) 
        legend(1, .3, c("Middle", "Old", "Young"), col = 1:3, lty = 1:3)
        abline(h = 0.5, col = 'cyan', lwd = 1)
        
    })
    
    
    output$distPlot <- renderPlot(
        
        test2() %>%
            select_at(c("PATIENT_ID","hospitalization", "gender", "Age_class", "outcome")) %>% 
            ggplot() +
            geom_density((aes(hospitalization, fill = as.factor(outcome))), alpha = 0.6) +
            guides(fill=guide_legend(title="Outcome 1=dead 0=living")) +
            xlab("Hospitalization days") +
            scale_x_continuous(breaks=seq(0, 50, 5))+
            theme(legend.position="bottom") +
            ggtitle(paste("Distribution of Hospitalization for ", input$n, " patients using ", 
                          input$vv, " virtual visits taken from a range of ",
                          input$maxvv, " dates", sep = ""))
        
        
        
    )
    output$KMeierPlot <- renderPlot(
        
        pvc()
        
        
    )
    output$Preal <- renderPlot(
        
        preal()
        
        
    )
    # Downloadable csv of selected dataset ----
    
    #output$downloadData <- downloadHandler(
        
    #    filename = function(){
    #        a <- paste("test_",format(Sys.time(), "%b_%d_%X"),".csv", sep = "")
    #        gsub(":", "", a)
    #    },
        
    #    content = function(file) {
    #        write.csv(test3(), file, row.names = FALSE)
    #    }
    #)
    
}


# Run the application 
shinyApp(ui = ui, server = server)
