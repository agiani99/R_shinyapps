#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(survival)
library(survminer)
library(corrr)
library(reshape2)
library(ggforce)
library(gridExtra)

load("J:/esp/Personal/Andrea Zaliani/takoua/China_cohort/China_cohort_v1.RData")

complement <- function(y, rho, x) {
    if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
    y.perp <- residuals(lm(x ~ y))
    rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

propo <- function(x, na.rm = T) (x / sum(x, na.rm = na.rm))

set.seed(1206)
rho = -0.7

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How chosen parameters affect hospitalization \nand Meier-Kaplan analysis in Virtual Cohort generation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Number of patients:", min = 50, max = 500, value = 200),
            sliderInput("vv", "Number of visits:", min = 2, max = 12, value = 6),
            sliderInput("maxvv", "Date range max available", min = 10, max = 100, value = 40)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(12, plotOutput("distPlot")),
                column(12, plotOutput("KMeierPlot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server<- function(input,output,session){
    
    start_cohort <- as.Date("2019-01-01")
    end_cohort <- as.Date("2021-01-01")
    
    test <- reactive({
        
        n = input$n
        n = 200
        m = nchar(as.character(n))
        
        ID <- paste("Pat_", formatC(seq(1:n),width=m,format='f',digits=0,flag='0'), 
                    sep = "") #patient codes
        
        Gender <- sample(c(1,2), n, replace = T, prob = c(0.688, 0.312))
        location <- sample(c("Urban", "Rural"),n, replace = T, prob = c(0.43, 0.57))
        
        Age <- c(sample(seq(18,55),floor(n*0.707), replace = T), 
                 sample(seq(55,100),ceiling(n*0.293), replace = T))
        Age <- sample(Age, replace = F)
        
        Age_class = ifelse(Age >=55, "Age_gt55", "Age_lt55")
        
        outcome <- list()
        outcome[0] <- ""
        probdeadold <-  0.903
        probdeadyoung <- 0.8126 
        
        for(i in 1:n){
            
            # here define the outcome
            if(Age_class[i] == "Age_gt55"){
                
                outcome[i] <- sample(c("Dead","Living"), 1, prob = c(probdeadold, 1-probdeadold))
                
                coutcome <- outcome[i]
                
            }else{
                
                outcome[i] <- sample(c("Dead", "Living"), 1, prob = c(probdeadyoung, 1-probdeadyoung))
                
                coutcome <- outcome[i]
                
            }
            outcome <- c(outcome, coutcome)
        }
        
        outcome <- unlist(outcome)
        outcome <- outcome[-n+1]
        rm(coutcome)
        
        maxvv <- as.integer(input$maxvv)
        #maxvv = 10
        vv <- as.integer(input$vv)
        #vv <- sample(seq(7,8),1)
        test <- cbind(ID, Gender, Age, Age_class, location, outcome)
        test <- as.data.frame(test, stringsAsFactors = F)
        
        test_long <- data.frame(PATIENT_ID = character(0),
                                Age = numeric(0),
                                Gender = character(0),
                                Age_class = character(0),
                                location = character(0),
                                outcome = character(0),
                                Visits_date = as.Date(character(0)),
                                KPS <- numeric(0),
                                stringsAsFactors = F
        )
    
        for(i in seq(1:n)){
            
            start_cohort2 <- as.Date(start_cohort + (sample(seq(7:maxvv),1)*3))
            patient <- test[i,]
            
            if(patient$outcome == "Dead"){
                end_cohort2 <- as.Date(start_cohort2 + (sample(seq(8:maxvv),1)*3))
                if(difftime(start_cohort,end_cohort2, units = "weeks")  < 8){
                    end_cohort2 <- as.Date(start_cohort2 + (sample(seq(8,maxvv),1)*10))
                }
                patient <- do.call("rbind", replicate(20, patient, simplify = FALSE))
                Visits_date <- sort(sample(seq(start_cohort2,end_cohort2, by="weeks"), vv, replace = F))
                vs <- sort(sample(c("N", "Y"), vv, replace = T))
                pp <- 20 - vv
                vs1 <- rep(NA, pp)
                Visits_date <- as.Date(c(Visits_date, vs1))
                patient$Visits_date <- Visits_date
                
            }else{
                end_cohort2 <- as.Date(start_cohort + (sample(seq(8:maxvv),1)*5))
                if(difftime(start_cohort,end_cohort2, units = "weeks")  < 8){
                    end_cohort2 <- as.Date(start_cohort2 + (sample(seq(8,maxvv),1)*10))
                }
                patient <- do.call("rbind", replicate(20, patient, simplify = FALSE))
                Visits_date <- sort(sample(seq(start_cohort2, end_cohort, by="weeks"), vv, replace = F))
                vs <- sort(sample(c("N", "Y"), vv, replace = T))
                pp <- 20 - vv
                vs1 <- rep(NA, pp)
                patient$Visits_date <- as.Date(c(Visits_date,vs1))
            }
            
            test2 <- floor(abs(complement(rnorm(n,mean=60,sd=10), rho, test$Age)))/12
            Karnopsky_KPS <- sort(sample(test2, vv, replace = T), decreasing = T) #is a measure of CNS activity
            Karnopsky_KPS <- ifelse(Karnopsky_KPS > 90, 90, Karnopsky_KPS)
            Karnopsky_KPS[1] <- ifelse(Karnopsky_KPS[1] < 70, 71, Karnopsky_KPS)
            Karnopsky_KPS <- sort(Karnopsky_KPS, decreasing = T)
            Karnopsky_KPS <- c(Karnopsky_KPS, vs1)
            patient$KPS <-Karnopsky_KPS
            
            test_long <- rbind(test_long, patient)
        
        }
        
        rm(patient);rm(vv);rm(vs);rm(vs1);rm(test);rm(pp);rm(Karnopsky_KPS)
        test_long3 <- test_long %>% filter(!is.na(Visits_date)) %>% 
            group_by(ID) %>% 
            mutate(startpat = min(Visits_date),
                   endpat = max(Visits_date),
                   hospitalization = as.numeric(difftime(endpat, startpat,units = "weeks"))) %>% 
            arrange(desc(Visits_date), .by_group=TRUE) %>% 
            top_n(1, Visits_date) %>% ungroup() %>% 
            mutate(outcome = as.numeric(as.factor(outcome)),
                   Age_class = as.numeric(as.factor(Age_class)),
                   location = as.numeric(as.factor(location)),
                   Gender = as.numeric(Gender))
        
        test <- as.data.frame(test_long3)
    })
        
    p <- reactive({    
        
        surv_object_real <- Surv(time = test()$hospitalization, event = test()$outcome)
        
        # dependence on age_clas
        
        fit2 <- survfit(surv_object_real ~ Age_class, data = as.data.frame(test()))
        
        plot(fit2, col = 2:3,lty=2:3, fun = "Surv", lwd = 3,
             xlab = "Hospitalization weeks", 
             ylab = "Overall survival probability")
        axis(side=1,at=c(seq(from=0,to=120,by=5))) 
        legend(10, .2, c("Age_gt55", "Age_lt55"), col = 2:3, lty = 2:3)
        abline(h = 0.5, col = 'cyan', lwd = 1)
        
    })
    
    
    output$distPlot <- renderPlot(
        
        test() %>%
            select_at(c("ID","hospitalization", "Gender", "Age_class", "outcome")) %>% 
            ggplot() +
            geom_density((aes(hospitalization, fill = as.factor(outcome))), alpha = 0.6) +
            guides(fill=guide_legend(title="Outcome\n1=dead\n2=living")) +
            xlab("Hospitalization weeks") +
            ggtitle(paste("Distribution of Hospitalization for ", input$n, " patients", 
                          "using ", input$vv, " virtual visits \ntaken from a range of ", 
                          input$maxvv, "dates", sep = ""))
        
        
    )
    output$KMeierPlot <- renderPlot(
        
        p()
        
        
    )
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
