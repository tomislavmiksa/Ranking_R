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
plotLQ <- function(correctFrame,graphName,fileName)
{
  file <- png(file=fileName,width=800,height=600,res=72)
  
  par(mar=c(5,5,5,5)+.1)
  
  poorLqRatio <- round(100.0*(1.0 - nrow(correctFrame[correctFrame$CST <= 15.0,]) / nrow(correctFrame)),digits = 2)
  
  hist_plot <- hist( correctFrame$CST,
                     breaks = 400,
                     xlim = c(1,30),
                     main = paste(graphName,"\nCALL SETUP TIME DISTRIBUTION"),
                     xlab = "POLQA LQ [MOS]",
                     col = "dodgerblue4",
                     ylab = "",
                     yaxt='n',
                     border = "dodgerblue4")
  
  text(x=20,
       y=0.8*max(hist_plot$counts),
       paste("SAMPLES",
             nrow(correctFrame),
             "\nAVG: ",
             round(mean(correctFrame$CST, na.rm = TRUE),digits = 2),
             "\nSTDEV",
             round(sd(correctFrame$CST, na.rm = TRUE),digits = 2),
             "\nP10",
             round(quantile(correctFrame$CST, probs = c(0.1), na.rm = TRUE),digits = 2),
             "\nP50",
             round(quantile(correctFrame$CST, probs = c(0.5), na.rm = TRUE),digits = 2),
             "\nP90",
             round(quantile(correctFrame$CST, probs = c(0.9), na.rm = TRUE),digits = 2),
             "\nMAX",
             round(max(correctFrame$CST, na.rm = TRUE),digits = 2),
             "\nSKEWNESS",
             round(skewness(correctFrame$CST, na.rm = TRUE),digits = 2)
       ),
       pos = 4)
  
  segments(x0=15,
           y0=0,
           x1=15,
           y1=0.4*max(hist_plot$counts),
           col="red",
           lty = 3,
           lwd = 2)
  
  text(x = 15.2, 
       y = 0.4*max(hist_plot$counts),
       col = "red",
       paste("CST > 15 =", poorLqRatio, "%"),
       pos=4)
  
  abline(v=mean(correctFrame$CST, na.rm = TRUE),col="darkolivegreen4")
  text(x = mean(correctFrame$CST, na.rm = TRUE), 
       y = max(hist_plot$counts),
       cex=1.1,
       col = "darkolivegreen4",
       paste("Average =", round(mean(correctFrame$CST, na.rm = TRUE),digits = 2)),
       pos=4)
  
  abline(v=quantile(correctFrame$CST, probs = c(0.5), na.rm = TRUE),col="blue")
  text(x = quantile(correctFrame$CST, probs = c(0.5), na.rm = TRUE), 
       y = max(hist_plot$counts),
       col = "blue",
       cex=1.1,
       paste("Median =", round(quantile(correctFrame$CST, probs = c(0.5), na.rm = TRUE), digits = 2)),
       pos=2)
  
  segments(x0=quantile(correctFrame$CST, probs = c(0.1), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$CST, probs = c(0.1), na.rm = TRUE),
           y1=0.5*max(hist_plot$counts),
           col="orange")
  text(x = quantile(correctFrame$CST, probs = c(0.1), na.rm = TRUE), 
       y = 0.5*max(hist_plot$counts),
       col = "orange",
       paste("P10 =", round(quantile(correctFrame$CST, probs = c(0.1), na.rm = TRUE),digits=2)),
       pos=2)
  
  segments(x0=quantile(correctFrame$CST, probs = c(0.9), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$CST, probs = c(0.9), na.rm = TRUE),
           y1=0.8*max(hist_plot$counts),
           col="darkmagenta")
  text(x = quantile(correctFrame$CST, probs = c(0.9), na.rm = TRUE), 
       y = 0.8*max(hist_plot$counts),
       col = "darkmagenta",
       paste("P90 =", round(quantile(correctFrame$CST, probs = c(0.9), na.rm = TRUE),digits=2)),
       pos=4)
  
  segments(x0=max(correctFrame$CST, na.rm = TRUE),
           y0=0,
           x1=max(correctFrame$CST, na.rm = TRUE),
           y1=0.4*max(hist_plot$counts),
           col="brown4")
  text(x = max(correctFrame$CST, na.rm = TRUE), 
       y = 0.4*max(hist_plot$counts),
       col = "brown4",
       paste("MAX\n",max(correctFrame$CST, na.rm = TRUE)),
       pos=4)
  
  dev.off()
  layout(1)
  
  return(c(nrow(correctFrame),
           poorLqRatio,
           quantile(correctFrame$CST, probs = c(0.1), na.rm = TRUE),
           quantile(correctFrame$CST, probs = c(0.5), na.rm = TRUE),
           quantile(correctFrame$CST, probs = c(0.9), na.rm = TRUE),
           min(correctFrame$CST, na.rm = TRUE),
           mean(correctFrame$CST, na.rm = TRUE),
           max(correctFrame$CST, na.rm = TRUE),
           sd(correctFrame$CST, na.rm = TRUE),
           skewness(correctFrame$CST, na.rm = TRUE)
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
con <- dbConnect(odbc::odbc(),.connection_string = 'driver={SQL Server};server=blndb11;database=DE_PreBM_Voice_01_archiviert;trusted_connection=true')

# EXTRACT SPEECH CDR FOR OPERATOR 1
kpiReport <-    dbGetQuery(con,paste('SELECT [Validity]
                                            ,[Operator]
	                                          ,G_Level_1_A
	                                          ,G_Level_2_A
                                            ,[Call_Status]
                                            ,[CallSetupTime(MO:Dial->MO:ConnectAck)] AS CST
                                            FROM NEW_CDR_VOICE_2018 
                                            WHERE Validity = 1'))

kpiReport <- kpiReport[kpiReport$Call_Status == "Completed",]
kpiReport1 <- kpiReport[kpiReport$Operator == "Telekom",]
kpiReport2 <- kpiReport[kpiReport$Operator == "Vodafone",]
kpiReport3 <- kpiReport[kpiReport$Operator == "O2",]
# CST DISTRIBUTION
lqKpis <- plotLQ(kpiReport1[ (kpiReport1$G_Level_1 == "Drive") & (kpiReport1$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-CST-DRIVE-CITY-Telekom.png")
lqKpis <- plotLQ(kpiReport1[ (kpiReport1$G_Level_1 == "Drive") & (kpiReport1$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-CST-DRIVE-RURAL-Telekom.png")
lqKpis <- plotLQ(kpiReport1[ (kpiReport1$G_Level_1 == "Walk") & (kpiReport1$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-CST-WALK-CITY-Telekom.png")
lqKpis <- plotLQ(kpiReport1[ (kpiReport1$G_Level_1 == "Walk") & (kpiReport1$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-CST-WALK-TRAIN-Telekom.png")

# CST DISTRIBUTION
lqKpis <- plotLQ(kpiReport2[ (kpiReport2$G_Level_1 == "Drive") & (kpiReport2$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-CST-DRIVE-CITY-Vodafone.png")
lqKpis <- plotLQ(kpiReport2[ (kpiReport2$G_Level_1 == "Drive") & (kpiReport2$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-CST-DRIVE-RURAL-Vodafone.png")
lqKpis <- plotLQ(kpiReport2[ (kpiReport2$G_Level_1 == "Walk") & (kpiReport2$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-CST-WALK-CITY-Vodafone.png")
lqKpis <- plotLQ(kpiReport2[ (kpiReport2$G_Level_1 == "Walk") & (kpiReport2$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-CST-WALK-TRAIN-Vodafone.png")

# CST DISTRIBUTION
lqKpis <- plotLQ(kpiReport3[ (kpiReport3$G_Level_1 == "Drive") & (kpiReport3$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-CST-DRIVE-CITY-Telefonica.png")
lqKpis <- plotLQ(kpiReport3[ (kpiReport3$G_Level_1 == "Drive") & (kpiReport3$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-CST-DRIVE-RURAL-Telefonica.png")
lqKpis <- plotLQ(kpiReport3[ (kpiReport3$G_Level_1 == "Walk") & (kpiReport3$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-CST-WALK-CITY-Telefonica.png")
lqKpis <- plotLQ(kpiReport3[ (kpiReport3$G_Level_1 == "Walk") & (kpiReport3$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-CST-WALK-TRAIN-Telefonica.png")
