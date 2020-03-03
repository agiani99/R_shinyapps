
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
# library(lubridate)
# library(ggrepel)
# library(data.table)
# library(shinythemes)
# library(scales)
# library(ggfortify)
# library(shinyWidgets)
# library(dygraphs)
# library(rpart.plot)
# library(caret)
# library(pROC)
# library(knitr)
# library(DT)
# library(rattle)
# library(SOMbrero)
# library(RColorBrewer)

load("../VC_raw.RData")


#options(shiny.port = 3838)
#options(shiny.host = "153.96.18.87") # own pc notebook
#options(shiny.host = "153.96.18.86") 
#options(shiny.host = "127.0.0.1")

completeFun <- function(data, desiredCols) {
    
    data1 <- data %>% filter(DESCRIPTION2 %in% desiredCols) %>% 
        select(Id, DESCRIPTION2, VALUE, Cohort,Age) %>% 
        mutate(VALUE2 = as.numeric(VALUE)) %>% 
        pivot_wider(names_from = DESCRIPTION2, values_from = VALUE2, 
                    values_fn = list(VALUE2 = mean)) %>% 
        select(-VALUE) %>% 
        group_by(Id) %>% summarise(.cols = desiredCols, list(M = mean),na.rm = T) %>%
        filter(complete.cases(.))
    names(data1) <- make.names(names(data1))
    
    return(data1)
}
extract_points_ellipse <- function(plot){
    # Extract components
    build <- ggplot_build(plot)
    points <- build$data[[1]]
    ell <- build$data[[2]]
    # Find which points are inside the ellipse, and add this to the data
    points2 <- data.frame(points[,1:4])
    points2 <- cbind(points2, in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y)))
    out <- points2 %>% filter(in.ell == FALSE)
    out1 <- completeFun(out, x)
    return(out1)
}
impute_median <- function(x){
    ind_na <- is.na(x)
    x[ind_na] <- median(x[!ind_na] , na.rm = T)
    as.numeric(x)
}

to_model <- VC_wlab %>% filter(TYPE == "numeric") %>% select(DESCRIPTION2) %>% unique() %>% 
    unlist() %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
    #theme = shinytheme("slate"),
    #includeCSS("bootstrap.min.css"),
    
    # Application title
    titlePanel("Exploratory Data Analysis Virtual Cohort Breast Cancer",
               "Fraunhofer IME Shiny App"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("X",
                        "X Value",
                        choices = as.vector(to_model),
                        selected = "Size.maximum dimension in Tumor"),
            selectInput("Y",
                        "Y Value",
                        choices = as.vector(to_model),
                        selected = "Gamma_GT_U_l")),
            #uiOutput("dynsel"),
            # selectInput("status",
            #             "To model:",
            #             choices = as.vector(to_model),
            #             selected = "CRP_Protein_mg_l"),width = 3),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Mapping",plotOutput("histplot", height = "600", width = "700")
                ),
                tabPanel(
                    "Distribution",plotOutput("plot1", height = "600", width = "700")
                ),
                tabPanel(
                    "Patient",plotOutput("distPlot", height = "600", width = "700")
                ),

                tabPanel(
                    "Possible Corr",plotOutput("coPlot", height = "600", width = "700")
                ),
                tabPanel(
                    "AI - SOM",plotOutput("SOMPlot", height = "600", width = "700")
                ),
                tabPanel(
                    "Tree Models",
                    fluidRow(
                        column(12, plotOutput("model", height = "600", width = "700")),
                        column(6, plotOutput("ROC", height = "300", width = "350")),
                        column(6, verbatimTextOutput("CM")))
                ))#,
            #tabPanel(
            #  "Prescription",dygraphOutput("pres", height = "600", width = "700"))
            #)
        )
    ),
    div(HTML("<br>This app was created by <a href='http://www.ime.fraunhofer.de' target='_blank'>Fraunhofer IME-Hamburg</a>.
           <br>Powered by <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a>"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dynsel <- renderUI({
        
        lista <- reactive({c(input$X, input$Y)})

        df  <- reactive({
            completeFun(VC_wlab, lista()) 
        })
        
        p <- reactive({
            
            ggplot(data = df(), aes_string(x = input$X, y = input$X)) +
                geom_point() +
                stat_ellipse(aes_string(x = input$X , y = input$Y), color = "cyan",
                             size = 2,  level = 0.95) +
                ggtitle(paste("Distribution of virtual Patients with " , input$Y, " data available", sep = ""), 
                        subtitle = "Data from Virtual Cohort  cyan line = confidence ellipse (0.95)") +
                theme(plot.title = element_text(hjust = 0.5,size=16))
        })
        
        outell <- reactive({
            extract_points_ellipse(p())
        })
        
        ID <- reactive({
            a <- round(df()[[input$X]],0)
            b <- round(df()[[input$Y]],0)
            
            df()[(a %in% outell()$x & b %in% outell()$y),]$Id
        })
        
        finaldb <- reactive({
            df() %>% filter(Id %in% ID()) %>% group_by(Id) %>% mutate(count = n()) %>% 
                dplyr::select(Id, count) %>% arrange(desc(count)) %>% filter(count > 3)
        })
        
        selectInput("Patient","Pat. No. (>3points)", choices = sort(unique(finaldb()$Id)), selected = NULL)
        
    })
    
    output$histplot <- renderPlot({
        
        number_ticks <- function(n){function(limits) pretty(limits, n)}
        
        lista <- reactive({c(input$X, input$Y)})
        
        df  <- reactive({
            tt <- VC_wlab %>% filter(DESCRIPTION2 %in% lista()) %>% 
                filter(Id %in% pat_id) %>% 
                mutate(VALUE2 = as.numeric(VALUE)) %>% select(-VALUE) %>% 
                pivot_wider(names_from = DESCRIPTION2, values_from = VALUE2,
                            values_fn = list(VALUE2 = mean,na.omit)) %>% 
                select(Id,lista()) %>% 
                group_by(Id) %>% summarise_at(.vars = lista(), list(M = mean),na.rm = T) %>% 
                na.omit() %>% reshape2::melt()
            tt$Cohort <- with(tt, VC_wlab[match(tt$Id, Id),"Cohort"])
            return(tt)
        })

        hh <- reactive({
            p <- ggplot(df()) +
                geom_boxplot(aes(variable, value)) +
                ggtitle(paste("Distribution of " ,lista(), sep = ""), 
                        subtitle = "Data from VC Breast Cancer") +
                theme(plot.title = element_text(hjust = 0.5,size=16)) +
                facet_wrap(~ Cohort, nrow = 1, ncol = 2)
                #scale_y_continuous(value,  breaks = number_ticks(10))
            return(p)
        })
        
        # if(input$Y == "mean_glucose_bld_ghb_est" | input$Y == "fasting glucose") {
        #     hh() + 
        #         geom_hline(yintercept = 250, linetype = "dashed", color = "blue") + 
        #         annotate("text", x = 0, y = 260, label = "Hyperglycemic status cutoff" ) 
        # } else {
        #     if(input$Y == "hba1c") {
        #         hh() + 
        #             geom_hline(yintercept = 6, linetype = "dashed", color = "blue") + 
        #             annotate("text", x = 0, y = 6.5, label = "prediabetic status cutoff" ) +
        #             geom_hline(yintercept = 8, linetype = "dashed", color = "blue") + 
        #             annotate("text", x = 0, y = 8.5, label = "diabetic Status cutoff" )
        #     } else {
        #         hh()
        #     }
        # }
        hh()
        
    })
    
    
    output$plot1 <- renderPlot({
        
        lista <- reactive({c(input$X, input$Y)})
        number_ticks <- function(n) {function(limits) pretty(limits, n)}
        
        
        df  <- reactive({
            completeFun(Fairbanks_Clinical_Variable_compute_T2DM, lista()) %>%
                filter(is.na(pre_diabetic_status) != TRUE)
        })
        
        p <- reactive({
            
            ggplot(data = df(), aes_string(x = input$X, y = input$Y)) +
                geom_point() +
                stat_ellipse(aes_string(x = input$X , y = input$Y), color = "cyan",
                             size = 2,  level = 0.95) +
                ggtitle(paste("Distribution of prediabetic Patients with " , input$Y, " data available", sep = ""), 
                        subtitle = "Data from Fairbanks_clinical_variables_compute 2018  cyan line = confidence ellipse (0.95)") +
                theme(plot.title = element_text(hjust = 0.5, size=16)) +
                scale_y_continuous(input$Y,  breaks = number_ticks(10)) +
                scale_x_continuous(input$X,  breaks = number_ticks(10))
        })
        
        outell <- reactive({
            extract_points_ellipse(p())
        })
        
        ID <- reactive({
            a <- round(df()[[input$X]],0)
            b <- round(df()[[input$Y]],0)
            
            df()[(a %in% outell()$x & b %in% outell()$y),]
        })
        
        if(input$Y == "hba1c" | input$Y == "protein") {
            p()
        } else { if(input$Y == "mean_glucose_bld_ghb_est" | input$Y == "fasting glucose") {
            p() + geom_text_repel(aes_string(x = input$X , y = input$Y ), data = ID(), label = ID()$SID,
                                  show.legend = F, na.rm = T) +
                geom_hline(yintercept = 250, linetype = "dashed", color = "blue") #+ 
            #annotate("text", x = 1000, y = 260, label = "Hyperglycemic Status cutoff" )
        } else { p() + geom_text_repel(aes_string(x = input$X , y = input$Y ), data = ID(), label = ID()$SID,
                                       show.legend = F, na.rm = T) 
        }
        }
    })
    
    output$distPlot <- renderPlot({
        
        lista <- reactive({c(input$X, input$Y)})
        
        PatientX <- reactive({completeFun(Fairbanks_Clinical_Variable_compute_T2DM, lista()) %>% 
                filter(SID == input$Patient) %>%
                arrange(Days_VIS_Sample) %>%
                filter(is.na(pre_diabetic_status) != TRUE) %>% 
                mutate(Diff = c(NA, diff(input$Y))) %>% 
                dplyr::select(hba1c, SID, AGE, input$Y, input$X, Diff)
        })
        
        normalizer <- reactive({
            max(PatientX()[,input$Y], na.rm = TRUE) / max(PatientX()$hba1c, na.rm = TRUE)
        })
        
        number_ticks <- function(n) {function(limits) pretty(limits, n)}
        
        q <- reactive({
            ggplot(data = PatientX()) + 
                geom_line(aes_string(x = input$X, y = input$Y), color = "red", size = 3) +
                guides( size = FALSE) +
                geom_point(aes_string(x = input$X, y = input$Y), size = 5) +
                ggtitle(paste("Patient ", input$Patient, " : time progression of ", input$Y, 
                              sep = ""), 
                        subtitle = "Data from Fairbanks_clinical_variables_compute 2018") +
                geom_col(data = PatientX(), aes(x = Days_VIS_Sample, y = hba1c * normalizer()),
                         width = 30, alpha = 0.3) +
                scale_x_continuous(input$X,  breaks = number_ticks(10)) +
                #xlim(c(-100,2000)) +
                scale_y_continuous(input$Y,  breaks = number_ticks(10)) +
                theme(plot.title = element_text(hjust = 0.5, size=16)) 
            #geom_vline(xintercept = 30 + 
            #             PatientX()[which.max(PatientX()$Diff),"Days_VIS_Sample"]$Days_VIS_Sample, 
            #           linetype = "dashed", 
            #           color = "darkgreen", size = 1)
            #geom_vline(xintercept = 30 + 
            #             subset(PatientX(), 
            #                    Diff == nth(unique(PatientX()$Diff),
            #                                length(unique(PatientX()$Diff))-1))$Days_VIS_Sample, 
            #           linetype = "dashed", 
            #           color = "darkgreen", size = 1)
        })
        
        if(input$Y == "mean_glucose_bld_ghb_est" | input$Y == "fasting glucose") {
            q() + geom_hline(yintercept = 250, linetype = "dashed", color = "blue") + 
                annotate("text", x = 1000, y = 260, label = "Hyperglycemic Status cutoff" ) +
                scale_y_continuous(sec.axis = sec_axis(~./normalizer(), name="hba1c"))
        } else {
            q() +scale_y_continuous(sec.axis = sec_axis(~./normalizer(), name="hba1c"))
        }
    })
    
    output$coPlot <- renderPlot({
        
        lista <- reactive({c(input$X, input$Y)})
        
        df  <- reactive({
            completeFun(Fairbanks_Clinical_Variable_compute_T2DM, lista()) %>%
                filter(is.na(pre_diabetic_status) != TRUE) %>%
                dplyr::select(input$X, input$Y)
        })
        
        lm.fit <- reactive({lm(reformulate(input$X, input$Y), data = df())})
        sstep <- summary(lm.fit())
        
        plotdata <- reactive({
            cbind(df()[,1:2], lm.fit()$fitted.values)
        })
        par(mfrow = c(2, 1))
        plot(plotdata()[,2], plotdata()[,3], 
             main = paste("Regression of ", input$Y, " original vs. fitted", sep = ""),
             ylab=paste(input$Y," fitted values", sep = ""), 
             xlab=paste(input$Y, " original Values", sep = ""))
        abline(0,1,col = "red")
        if(input$Y == "fasting glucose" | input$Y == "mean_glucose_bld_ghb_est") {
            abline(v=250, lty = 2, col = "cyan", lwd = 3)
        }
        plot(plotdata()[,1], plotdata()[,2], 
             main = paste("Regression of ", input$Y, " vs. ", input$X, " R² = ", 
                          round(sstep$r.squared,3), sep = ""),
             ylab=paste(input$Y," values", sep = ""), 
             xlab=paste(input$X, " Values", sep = ""))
        abline(lm.fit(), col = "red")
        
    })
    
    output$SOMPlot <- renderPlot({
        
        lista <- reactive({c(input$X, input$Y)})
        x <- paste("`", input$X, "`", sep = "")
        y <- paste("`", input$Y, "`", sep = "")
        
        
        df  <- reactive({
            completeFun(Fairbanks_Clinical_Variable_compute_T2DM, lista()) %>%
                filter(is.na(pre_diabetic_status) != TRUE) %>%
                dplyr::select(SID, input$X, input$Y)
        })
        tmp <- df()
        
        coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
        
        set.seed(1206)
        
        datas <- function(e) {
            CI = ""
            tmp <- df()
            var1 <- assign(paste("`","tmp$`", input$X, "`", sep = ""), tmp[, lista()[1]])
            var2 <- assign(paste("`","tmp$`", input$Y, "`", sep = ""), tmp[, lista()[2]])
            fit <- lm(unlist(var1) ~ unlist(var2), data = tmp)
            cod <- cooks.distance(fit)
            CI <- as.vector(names(cod)[(cod > 5*mean(cod, na.rm=T))])
            #CIs <- c(CI, as.vector(tmp[var1 > quantile(var1, c(0.95))  | var2 > quantile(var2, c(0.95))  ,"SID"]))
            return(CI)
        }
        
        tt <- unique(datas(lista()))
        
        my.som <- trainSOM(x.data = tmp[tmp$SID %in% tt,], dimension=c(3,3), nb.save=20, 
                           maxit = 500, type = "numeric",
                           radius.type = "letremy", verbose = F)
        
        #plot(my.som, what="energy")
        #plot(my.som, what="prototypes", type="lines", print.title=TRUE)
        #plot(my.som, what="obs", type="hitmap")
        #plot(x = my.som$data[,16], y = my.som$data[,8], col=factor(my.som$clustering), pch=19)
        #summary(my.som)
        par(mar = c(0.1,0.0,6.1,0.0))
        #par(mfrow=c(1,1), mar=c(0.5,5.1,0.0,2.0))
        par(mfrow=c(1,2))
        #plot(1:10,ann=FALSE,type="n",xaxt="n",yaxt="n")
        #for(j in 1:4) for(i in 0:10) mtext(as.character(i),side=j,line=i)
        #pnts = cbind(x=c(4.2,4.3,4.3,4.2), y=c(3.5,3.5,0.51,0.51)) vertical
        var1 <- assign(paste("`","tmp$`", input$X, "`", sep = ""), tmp[, input$X])
        
        my.palette = colorRampPalette(brewer.pal(n=9, name='Blues'))(mean(var1, na.rm = T))
        
        plot(my.som, what="prototypes", type="color", my.palette = my.palette,
             var = c(which(startsWith(names(tmp[unique(datas(x)), lista()]),input$X))), 
             main=paste("SelfOrganizingMap - ", input$X, sep=""))
        #    legend.gradient(pnts, cols = brewer.pal(n=9, name='Blues'), title = "",
        #                    limits = c(round(min(var1),1),round(max(var1),1)))
        legend("bottom", legend = c(round(min(var1),1),round(mean(var1)),round(max(var1),1)), 
               fill = brewer.pal(n=3, name='Blues'), cex = 0.9, ncol = 3)
        var2 <- assign(paste("`","tmp$`", input$Y, "`", sep = ""), tmp[, input$Y])
        my.palette = colorRampPalette(brewer.pal(n=9, name='Blues'))(mean(var2))
        plot(my.som, what="prototypes", type="color", my.palette = my.palette,
             var = c(which(startsWith(names(tmp[unique(datas()), lista()]),input$Y))),
             main=paste("SelfOrganizingMap - ", input$Y, sep=""))
        #legend.gradient(pnts, cols = brewer.pal(n=9, name='Blues'), title = "",
        #                limits = c(round(min(var2),1),round(max(var2),1)))
        legend("bottom", legend = c(round(min(var2),1),round(mean(var2)),round(max(var2),1)), 
               fill = brewer.pal(n=3, name='Blues'), cex = 0.9,ncol = 3)
        
        #plot(my.som, what="obs", type="boxplot", print.title=F)
        #table(my.som$clustering)
    },height = 250, width = 700)
    
    output$model <- renderPlot({
        
        response <- reactive({ input$status })
        
        mdf <- ".y"
        response_mdf <- paste(input$status, mdf, sep = "")
        
        Fairbanks_response2 <- reactive({
            d <- Fairbanks_Clinical_Variable_compute
            colnames(d)[colnames(d)==input$status] <- response_mdf
            d <- completeFun(d, response_mdf)
            d
        })
        
        Fairbanks_response <- reactive({
            Fairbanks_response2() %>% 
                dplyr::select(which(colMeans(is.na(.)) < 0.9))
        })
        
        Fair <- Fairbanks_response() %>% 
            dplyr::select(-ends_with("_status")) %>% 
            mutate_all(. , impute_median) %>% dplyr::select(-c(2,4))
        
        Fairbanks_response_def <- reactive({
            Fair[, colSums(is.na(Fair)) != nrow(Fair)]
        })
        
        y_var <- reactive({
            as.vector(unlist(Fairbanks_response_def()[,response_mdf]))
        })
        #par(mfrow = c(2,1), cex.main = 0.7)
        rpart.fit <- rpart(as.formula(paste(response_mdf," ~ .", sep = "")), data = Fairbanks_response_def(),
                           method = "class",
                           control = rpart.control(maxdepth = 5))
        
        #printcp(rpart.fit)
        #plotcp(rpart.fit, minline = T, lty = 3, col = 2, upper = "none")
        
        pred <- predict(rpart.fit, type = "class")
        CM <- reactive({
            confusionMatrix(data = pred, as.factor(y_var()))
        })
        #fancyRpartPlot(rpart.fit, type = 3, main = paste("Rpart model for Fairbanks ", input$status, " (n= ",
        #                                       NROW(Fairbanks_response_def()),", accuracy = ",
        #                                       round(CM()$overall[1],3),")", sep = ""))
        rpart.plot(rpart.fit, type = 3, extra = 4, under = F, cex = 1.1, fallen.leaves = F,
                   clip.right.labs=T,
                   main = paste("Rpart model for Fairbanks ", input$status, " (n= ",
                                NROW(Fairbanks_response_def()),
                                ", accuracy = ", 
                                round(CM()$overall[1],3),")", sep = ""))
    })
    output$ROC <- renderPlot({
        
        response <- reactive({ input$status })
        
        mdf <- ".y"
        response_mdf <- paste(response(), mdf, sep = "")
        
        Fairbanks_response2 <- reactive({
            d <- Fairbanks_Clinical_Variable_compute
            colnames(d)[colnames(d)==input$status] <- response_mdf
            d <- completeFun(d,response_mdf)
            d
        })
        
        Fairbanks_response <- reactive({
            Fairbanks_response2() %>% 
                dplyr::select(which(colMeans(is.na(.)) < 0.9))
        })
        
        Fair <- Fairbanks_response() %>% dplyr::select(-ends_with("_status")) %>% 
            mutate_all(. , impute_median) %>% dplyr::select(-c(2,4))
        
        Fairbanks_response_def <- reactive({
            Fair[, colSums(is.na(Fair)) != nrow(Fair)]
        })
        
        y_var <- reactive({
            as.vector(unlist(Fairbanks_response_def()[,response_mdf]))
        })
        
        rpart.fit <- rpart(as.formula(paste(response_mdf," ~ .", sep = "")), data = Fairbanks_response_def(),
                           method = "class",
                           control = rpart.control(maxdepth = 5))
        
        pred <- predict(rpart.fit, type = "class")
        CM <- reactive({
            confusionMatrix(data = pred, as.factor(y_var()))
        })
        rpart.ROC <- reactive({
            roc(predictor=as.numeric(pred),
                response=y_var(),
                levels=rev(levels(as.factor(y_var()))))
        })
        par(pty="s")
        plot(rpart.ROC(),main=paste("ROC Curve - Acc = ", round(as.numeric(CM()$overall[1]),4)), 
             xlab = "Specificity = TN / TN + FP",
             ylab = "Sensitivity = TP / TP + FN",
             cex.lab = 0.8)
        grid(nx = NULL, col = "darkgrey")
        
    })
    
    output$CM <- renderPrint({
        
        response <- reactive({ input$status })
        
        mdf <- ".y"
        response_mdf <- paste(response(), mdf, sep = "")
        
        Fairbanks_response2 <- reactive({
            d <- Fairbanks_Clinical_Variable_compute
            colnames(d)[colnames(d)==input$status] <- response_mdf
            d <- completeFun(d,response_mdf)
            dd <- d %>% dplyr::select(-ends_with("_status"))
            dd
        })
        
        Fairbanks_response <- reactive({
            Fairbanks_response2() %>% 
                dplyr::select(which(colMeans(is.na(.)) < 0.9))
        })
        
        Fair <- Fairbanks_response() %>% dplyr::select(-ends_with("_status")) %>% 
            mutate_all(. , impute_median) %>% dplyr::select(-c(2,4))
        
        Fairbanks_response_def <- reactive({
            Fair[, colSums(is.na(Fair)) != nrow(Fair)]
        })
        
        y_var <- reactive({
            as.vector(unlist(Fairbanks_response_def()[,response_mdf]))
        })
        
        rpart.fit <- rpart(as.formula(paste(response_mdf," ~ .", sep = "")), data = Fairbanks_response_def(),
                           method = "class",
                           control = rpart.control(maxdepth = 5))
        
        pred <- predict(rpart.fit, type = "class")
        CM <- confusionMatrix(data = pred, as.factor(y_var()))
        
        print(CM$table)
        round(table(pred, as.factor(y_var()))/NROW(Fairbanks_response_def())*100,1)
        
        
        
    })
    
    # output$pres <- renderDygraph({
    #    
    #    PatientXdy <- reactive({
    #      completeFun(Fairbanks_Clinical_Variable_compute_T2DM, input$Y) %>% 
    #        filter(SID == input$Patient) %>%
    #        arrange(Days_VIS_Sample) %>%
    #        filter(is.na(pre_diabetic_status) != TRUE) %>% 
    #        filter(Days_VIS_Sample >= 0) %>% 
    #        mutate(Diff = c(0, diff(input$Y))) %>%
    #        dplyr::select(Days_VIS_Sample, Diff, input$Y)
    #      })
    #    
    #    toprint <- reactive({
    #      xts(PatientXdy()[,-1], 
    #                   order.by = as.Date(as.numeric(unlist(PatientXdy()[,1])), origin = "2011-04-20"))
    #      })
    #    #toprint2 <- reactive({
    #    #  toprint()[-1,]
    #    #  toprint()[,-1]
    #    # })
    # 
    #    
    #    dyp <- reactive({
    #      dygraph(toprint(), main = "Patient time dependence",
    #            xlab = "Time") %>%
    #        dySeries(input$Y, drawPoints = TRUE, fillGraph = TRUE, axis = "y2") %>%
    #        dySeries("Diff", drawPoints = TRUE, fillGraph = TRUE, axis = "y") %>% 
    #        dyAxis("y2", drawGrid = TRUE) %>%
    #        dyAxis("y", label = "Difference", independentTicks = TRUE) %>%
    #        dyOptions(includeZero = TRUE, 
    #                  axisLineColor = "navy", 
    #                  gridLineColor = "lightblue") #%>% 
    #        #dyHighlight(highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.5,
    #        #            hideOnMouseOut = FALSE)
    #    })
    #    dyp()
    #    
    # 
    # 
    #    #max_diff <- reactive({
    #    #  PatientXdy()[,c(1,3)] %>% 
    #    #    dplyr::filter(top_n(3)) %>% mutate(Diff = as.integer(round(Diff,0)))
    #    #})
    #    
    #    #for (i in 1:3) {
    #    #  dyp() <- dygraphs::dyAnnotation(dyp(), x = max_diff()[i,1], 
    #    #                                  text = paste("Treatment ", i, sep = ""))
    #    #}
    #    #dyp() %>% dyAnnotation(dyp(), x = max_diff()[,1], text="Top 2 treatment")
    #    
    #  })
}

# Run the application 
shinyApp(ui = ui, server = server)

