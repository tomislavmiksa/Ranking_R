library(DBI)
library(odbc)
library(dplyr)
library(tidyr)
library(plotrix)
library(gridExtra)
library(e1071)
library(reshape)
library(sunburstR)
library(gridExtra)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
######## ##     ## ##    ##  ######  ######## ####  #######  ##    ##  ######  
##       ##     ## ###   ## ##    ##    ##     ##  ##     ## ###   ## ##    ## 
##       ##     ## ####  ## ##          ##     ##  ##     ## ####  ## ##       
######   ##     ## ## ## ## ##          ##     ##  ##     ## ## ## ##  ######  
##       ##     ## ##  #### ##          ##     ##  ##     ## ##  ####       ## 
##       ##     ## ##   ### ##    ##    ##     ##  ##     ## ##   ### ##    ## 
##        #######  ##    ##  ######     ##    ####  #######  ##    ##  ######  

##############################################################################################
#                                POLQA MOS DISTRIBUTION                                      #
##############################################################################################
plotMDR <- function(correctFrame,graphName,fileName)
{
  file <- png(file=fileName,width=800,height=600,res=72)
  
  par(mar=c(5,5,5,5)+.1)
  
  poorLqRatio <- round(100.0*(1.0 - nrow(correctFrame[correctFrame$RTT_Delay_ms <= 15.0,]) / nrow(correctFrame)),digits = 2)
  
  hist_plot <- hist( correctFrame$RTT_Delay_ms,
                     breaks = 100,
                     xlim = c(1,800),
                     main = paste(graphName,"\nRTT DISTRIBUTION"),
                     xlab = "RTT",
                     col = "dodgerblue4",
                     ylab = "",
                     yaxt='n',
                     border = "dodgerblue4")
  
  text(x=80000,
       y=0.8*max(hist_plot$counts),
       paste("SAMPLES",
             nrow(correctFrame),
             "\nAVG: ",
             round(mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2),
             "\nSTDEV",
             round(sd(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2),
             "\nP10",
             round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),digits = 2),
             "\nP50",
             round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE),digits = 2),
             "\nP90",
             round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),digits = 2),
             "\nMAX",
             round(max(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2),
             "\nSKEWNESS",
             round(skewness(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2)
       ),
       pos = 4)
  

  
  abline(v=mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),col="darkolivegreen4")
  text(x = mean(correctFrame$RTT_Delay_ms, na.rm = TRUE), 
       y = max(hist_plot$counts),
       cex=1.1,
       col = "darkolivegreen4",
       paste("Average =", round(mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2)),
       pos=4)
  
  abline(v=quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE),col="blue")
  text(x = quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE), 
       y = max(hist_plot$counts),
       col = "blue",
       cex=1.1,
       paste("Median =", round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE), digits = 2)),
       pos=2)
  
  segments(x0=quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),
           y1=0.5*max(hist_plot$counts),
           col="orange")
  text(x = quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE), 
       y = 0.5*max(hist_plot$counts),
       col = "orange",
       paste("P10 =", round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),digits=2)),
       pos=2)
  
  segments(x0=quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),
           y1=0.8*max(hist_plot$counts),
           col="darkmagenta")
  text(x = quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE), 
       y = 0.8*max(hist_plot$counts),
       col = "darkmagenta",
       paste("P90 =", round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),digits=2)),
       pos=4)
  
  segments(x0=max(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           y0=0,
           x1=max(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           y1=0.4*max(hist_plot$counts),
           col="brown4")
  text(x = max(correctFrame$RTT_Delay_ms, na.rm = TRUE), 
       y = 0.4*max(hist_plot$counts),
       col = "brown4",
       paste("MAX\n",max(correctFrame$RTT_Delay_ms, na.rm = TRUE)),
       pos=4)
  
  dev.off()
  layout(1)
  
  return(c(nrow(correctFrame),
           poorLqRatio,
           quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),
           quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE),
           quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),
           min(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           max(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           sd(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           skewness(correctFrame$RTT_Delay_ms, na.rm = TRUE)
  ))
}




plotMDR1 <- function(correctFrame,graphName,fileName)
{
  file <- png(file=fileName,width=800,height=600,res=72)
  
  par(mar=c(5,5,5,5)+.1)
  
  poorLqRatio <- round(100.0*(1.0 - nrow(correctFrame[correctFrame$RTT_Delay_ms <= 15.0,]) / nrow(correctFrame)),digits = 2)
  
  hist_plot <- hist( correctFrame$RTT_Delay_ms,
                     breaks = 400,
                     xlim = c(1,350000),
                     main = paste(graphName,"\nMDR DISTRIBUTION"),
                     xlab = "MDR",
                     col = "dodgerblue4",
                     ylab = "",
                     yaxt='n',
                     border = "dodgerblue4")
  
  text(x=280000,
       y=0.8*max(hist_plot$counts),
       paste("SAMPLES",
             nrow(correctFrame),
             "\nAVG: ",
             round(mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2),
             "\nSTDEV",
             round(sd(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2),
             "\nP10",
             round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),digits = 2),
             "\nP50",
             round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE),digits = 2),
             "\nP90",
             round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),digits = 2),
             "\nMAX",
             round(max(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2),
             "\nSKEWNESS",
             round(skewness(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2)
       ),
       pos = 4)
  
  
  
  abline(v=mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),col="darkolivegreen4")
  text(x = mean(correctFrame$RTT_Delay_ms, na.rm = TRUE), 
       y = max(hist_plot$counts),
       cex=1.1,
       col = "darkolivegreen4",
       paste("Average =", round(mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),digits = 2)),
       pos=4)
  
  abline(v=quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE),col="blue")
  text(x = quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE), 
       y = max(hist_plot$counts),
       col = "blue",
       cex=1.1,
       paste("Median =", round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE), digits = 2)),
       pos=2)
  
  segments(x0=quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),
           y1=0.5*max(hist_plot$counts),
           col="orange")
  text(x = quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE), 
       y = 0.5*max(hist_plot$counts),
       col = "orange",
       paste("P10 =", round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),digits=2)),
       pos=2)
  
  segments(x0=quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),
           y1=0.8*max(hist_plot$counts),
           col="darkmagenta")
  text(x = quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE), 
       y = 0.8*max(hist_plot$counts),
       col = "darkmagenta",
       paste("P90 =", round(quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),digits=2)),
       pos=4)
  
  segments(x0=max(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           y0=0,
           x1=max(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           y1=0.4*max(hist_plot$counts),
           col="brown4")
  text(x = max(correctFrame$RTT_Delay_ms, na.rm = TRUE), 
       y = 0.4*max(hist_plot$counts),
       col = "brown4",
       paste("MAX\n",max(correctFrame$RTT_Delay_ms, na.rm = TRUE)),
       pos=4)
  
  dev.off()
  layout(1)
  
  return(c(nrow(correctFrame),
           poorLqRatio,
           quantile(correctFrame$RTT_Delay_ms, probs = c(0.1), na.rm = TRUE),
           quantile(correctFrame$RTT_Delay_ms, probs = c(0.5), na.rm = TRUE),
           quantile(correctFrame$RTT_Delay_ms, probs = c(0.9), na.rm = TRUE),
           min(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           mean(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           max(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           sd(correctFrame$RTT_Delay_ms, na.rm = TRUE),
           skewness(correctFrame$RTT_Delay_ms, na.rm = TRUE)
  ))
}

##############################################################################################
#                                POLQA CALL MODE DISTRIBUTION                                #
##############################################################################################
plotCM <- function(correctFrame,graphName,fileName)
{
  file <- png(file=fileName,width=800,height=600,res=72)
  
  dfA <- data.frame(correctFrame$L1_callMode_A,correctFrame$L2_callMode_A)
  dfB <- data.frame(correctFrame$L1_callMode_B,correctFrame$L2_callMode_B)
  names(dfA) <- c("L1","L2")
  names(dfB) <- c("L1","L2")
  
  tmpTable <- 100*prop.table(t(table(rbind(dfA,dfB))))
  
  par(mar=c(5,5,5,5)+.1) 
  bp <- barplot(colSums(tmpTable),
                main = paste("Call Mode Samples Distribution\n",graphName),
                ylim = c(0, max(1.25*colSums(tmpTable))),
                yaxt='n',
                ylab = "")
  text(x=bp,
       y=colSums(tmpTable),
       labels = paste(round(colSums(tmpTable),digits=2),"%"),
       pos = 3
  )
  
  tmpTable <- round(rbind(tmpTable,colSums(tmpTable)),digits = 3)
  addtable2plot("topleft",
                title = "Samples per Voice Call Mode [%]",
                table = tmpTable, 
                cex = .9, 
                bty = "o",
                display.rownames=TRUE,
                bg = c("white","grey"), vlines = TRUE, xpad = .25)
  
  
  # sunburst(tempDF)
  
  
  
  dev.off()
  layout(1)
}

##     ##    ###    #### ##    ##    ########  ########   #######   ######   ########     ###    ##     ## 
###   ###   ## ##    ##  ###   ##    ##     ## ##     ## ##     ## ##    ##  ##     ##   ## ##   ###   ### 
#### ####  ##   ##   ##  ####  ##    ##     ## ##     ## ##     ## ##        ##     ##  ##   ##  #### #### 
## ### ## ##     ##  ##  ## ## ##    ########  ########  ##     ## ##   #### ########  ##     ## ## ### ## 
##     ## #########  ##  ##  ####    ##        ##   ##   ##     ## ##    ##  ##   ##   ######### ##     ## 
##     ## ##     ##  ##  ##   ###    ##        ##    ##  ##     ## ##    ##  ##    ##  ##     ## ##     ## 
##     ## ##     ## #### ##    ##    ##        ##     ##  #######   ######   ##     ## ##     ## ##     ## 

setwd("C:/Users/tmiksa/Desktop/Ranking_R")

# LOAD RANKING PARAMETERS FROM INPUT FILE
rankingParams = read.csv("rankingParams.csv")  # read csv file 

# Connect BLNDB11
# HERE YOU HAVE TO SET CORRECT DB NAME
##########################################
con <- dbConnect(odbc::odbc(),.connection_string = 'driver={SQL Server};server=blndb11;database=DE_PreBM_Data_01_archiviert;trusted_connection=true')

# EXTRACT SPEECH CDR FOR OPERATOR 1
kpiReport <-    dbGetQuery(con,paste('SELECT [Validity]
                                     ,HomeOperator
                                     ,[Test_Status]
                                     ,[Test_Type]
                                     ,[Test_Name]
                                     ,[Test_Info]
                                     ,[G_Level_1]
                                     ,[G_Level_2]
                                     ,[RTT_Delay_ms]
                                     ,[RTT_Delay_ms]
                                     ,[Content_Transfered_Size_Bytes]
                                     ,[Content_Transfered_Time_ms]
                                     ,[AVG_VMOS]
                                     ,[TimeToFirstPicture_ms]
                                     FROM NEW_CDR_DATA_2018
                                     WHERE Validity = 1'))

kpiReport <- kpiReport[kpiReport$Test_Status == "Completed",]

kpiReport1 <- kpiReport[kpiReport$HomeOperator == "Telekom",]
kpiReport2 <- kpiReport[kpiReport$HomeOperator == "Vodafone",]
kpiReport3 <- kpiReport[kpiReport$HomeOperator == "O2",]

kpiReport1FDTTUL <- kpiReport[kpiReport$Test_Type == "httpBrowser",]
kpiReport2FDTTUL <- kpiReport[kpiReport$Test_Type == "httpBrowser",]
kpiReport3FDTTUL <- kpiReport[kpiReport$Test_Type == "httpBrowser",]


# RTT_Delay_ms DISTRIBUTION
lqKpis <- plotMDR(kpiReport1FDTTUL[ (kpiReport1FDTTUL$G_Level_1 == "Drive") & (kpiReport1FDTTUL$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-RTT_Delay_ms-DRIVE-CITY-TelekomUL.png")
lqKpis <- plotMDR(kpiReport1FDTTUL[ (kpiReport1FDTTUL$G_Level_1 == "Drive") & (kpiReport1FDTTUL$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-RTT_Delay_ms-DRIVE-RURAL-TelekomUL.png")
lqKpis <- plotMDR(kpiReport1FDTTUL[ (kpiReport1FDTTUL$G_Level_1 == "Walk") & (kpiReport1FDTTUL$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-RTT_Delay_ms-WALK-CITY-TelekomUL.png")
lqKpis <- plotMDR(kpiReport1FDTTUL[ (kpiReport1FDTTUL$G_Level_1 == "Walk") & (kpiReport1FDTTUL$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-RTT_Delay_ms-WALK-TRAIN-TelekomUL.png")

# RTT_Delay_ms DISTRIBUTION
lqKpis <- plotMDR(kpiReport2FDTTUL[ (kpiReport2FDTTUL$G_Level_1 == "Drive") & (kpiReport2FDTTUL$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-RTT_Delay_ms-DRIVE-CITY-VodafoneUL.png")
lqKpis <- plotMDR(kpiReport2FDTTUL[ (kpiReport2FDTTUL$G_Level_1 == "Drive") & (kpiReport2FDTTUL$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-RTT_Delay_ms-DRIVE-RURAL-VodafoneUL.png")
lqKpis <- plotMDR(kpiReport2FDTTUL[ (kpiReport2FDTTUL$G_Level_1 == "Walk") & (kpiReport2FDTTUL$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-RTT_Delay_ms-WALK-CITY-VodafoneUL.png")
lqKpis <- plotMDR(kpiReport2FDTTUL[ (kpiReport2FDTTUL$G_Level_1 == "Walk") & (kpiReport2FDTTUL$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-RTT_Delay_ms-WALK-TRAIN-VodafoneUL.png")

# RTT_Delay_ms DISTRIBUTION
lqKpis <- plotMDR(kpiReport3FDTTUL[ (kpiReport3FDTTUL$G_Level_1 == "Drive") & (kpiReport3FDTTUL$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-RTT_Delay_ms-DRIVE-CITY-TelefonicaUL.png")
lqKpis <- plotMDR(kpiReport3FDTTUL[ (kpiReport3FDTTUL$G_Level_1 == "Drive") & (kpiReport3FDTTUL$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-RTT_Delay_ms-DRIVE-RURAL-TelefonicaUL.png")
lqKpis <- plotMDR(kpiReport3FDTTUL[ (kpiReport3FDTTUL$G_Level_1 == "Walk") & (kpiReport3FDTTUL$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-RTT_Delay_ms-WALK-CITY-TelefonicaUL.png")
lqKpis <- plotMDR(kpiReport3FDTTUL[ (kpiReport3FDTTUL$G_Level_1 == "Walk") & (kpiReport3FDTTUL$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-RTT_Delay_ms-WALK-TRAIN-TelefonicaUL.png")


