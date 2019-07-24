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
  
  poorLqRatio <- round(100.0*(1.0 - nrow(correctFrame[correctFrame$LQ >= 1.6,]) / nrow(correctFrame)),digits = 2)
  
  hist_plot <- hist( correctFrame$LQ,
                     breaks = 400,
                     xlim = c(1, 5),
                     main = paste(graphName,"\nPOLQA MOS DISTRIBUTION"),
                     xlab = "POLQA LQ [MOS]",
                     col = "dodgerblue4",
                     ylab = "",
                     yaxt='n',
                     border = "dodgerblue4")
  
  text(x=1,
       y=0.8*max(hist_plot$counts),
       paste("SAMPLES",
              nrow(correctFrame),
              "\nAVG: ",
              round(mean(correctFrame$LQ, na.rm = TRUE),digits = 2),
              "\nSTDEV",
              round(sd(correctFrame$LQ, na.rm = TRUE),digits = 2),
              "\nP10",
              quantile(correctFrame$LQ, probs = c(0.1), na.rm = TRUE),
              "\nP50",
              quantile(correctFrame$LQ, probs = c(0.5), na.rm = TRUE),
              "\nP90",
              quantile(correctFrame$LQ, probs = c(0.9), na.rm = TRUE),
              "\nMAX",
               max(correctFrame$LQ, na.rm = TRUE),
              "\nSKEWNESS",
              round(skewness(correctFrame$LQ, na.rm = TRUE),digits = 2)
      ),
      pos = 4)
  
  segments(x0=1.6,
           y0=0,
           x1=1.6,
           y1=0.4*max(hist_plot$counts),
           col="red",
           lty = 3,
           lwd = 2)
  
  text(x = 1.7, 
       y = 0.4*max(hist_plot$counts),
       col = "red",
       paste("MOS < 1.6 =", poorLqRatio, "%"),
       pos=4)
  
  abline(v=mean(correctFrame$LQ, na.rm = TRUE),col="darkolivegreen4")
  text(x = mean(correctFrame$LQ, na.rm = TRUE), 
       y = max(hist_plot$counts),
       cex=1.1,
       col = "darkolivegreen4",
       paste("Average =", round(mean(correctFrame$LQ, na.rm = TRUE),digits = 2)),
       pos=2)
  
  abline(v=quantile(correctFrame$LQ, probs = c(0.5), na.rm = TRUE),col="blue")
  text(x = quantile(correctFrame$LQ, probs = c(0.5), na.rm = TRUE), 
       y = max(hist_plot$counts),
       col = "blue",
       cex=1.1,
       paste("Median =", quantile(correctFrame$LQ, probs = c(0.5), na.rm = TRUE)),
       pos=4)
  
  segments(x0=quantile(correctFrame$LQ, probs = c(0.1), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$LQ, probs = c(0.1), na.rm = TRUE),
           y1=0.5*max(hist_plot$counts),
           col="orange")
  text(x = quantile(correctFrame$LQ, probs = c(0.1), na.rm = TRUE), 
       y = 0.5*max(hist_plot$counts),
       col = "orange",
       paste("P10 =", quantile(correctFrame$LQ, probs = c(0.1), na.rm = TRUE)),
       pos=2)
  
  segments(x0=quantile(correctFrame$LQ, probs = c(0.9), na.rm = TRUE),
           y0=0,
           x1=quantile(correctFrame$LQ, probs = c(0.9), na.rm = TRUE),
           y1=0.8*max(hist_plot$counts),
           col="darkmagenta")
  text(x = quantile(correctFrame$LQ, probs = c(0.9), na.rm = TRUE), 
       y = 0.8*max(hist_plot$counts),
       col = "darkmagenta",
       paste("P90 =", quantile(correctFrame$LQ, probs = c(0.9), na.rm = TRUE)),
       pos=4)
  
  segments(x0=max(correctFrame$LQ, na.rm = TRUE),
           y0=0,
           x1=max(correctFrame$LQ, na.rm = TRUE),
           y1=0.4*max(hist_plot$counts),
           col="brown4")
  text(x = max(correctFrame$LQ, na.rm = TRUE), 
       y = 0.4*max(hist_plot$counts),
       col = "brown4",
       paste("MAX\n",max(correctFrame$LQ, na.rm = TRUE)),
       pos=4)
  
  dev.off()
  layout(1)
  
  return(c(nrow(correctFrame),
           poorLqRatio,
           quantile(correctFrame$LQ, probs = c(0.1), na.rm = TRUE),
           quantile(correctFrame$LQ, probs = c(0.5), na.rm = TRUE),
           quantile(correctFrame$LQ, probs = c(0.9), na.rm = TRUE),
           min(correctFrame$LQ, na.rm = TRUE),
           mean(correctFrame$LQ, na.rm = TRUE),
           max(correctFrame$LQ, na.rm = TRUE),
           sd(correctFrame$LQ, na.rm = TRUE),
           skewness(correctFrame$LQ, na.rm = TRUE)
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
con <- dbConnect(odbc::odbc(),.connection_string = 'driver={SQL Server};server=blndb11;database=DE_PreBM_Voice_03;trusted_connection=true')

# EXTRACT SPEECH CDR FO OPERATOR 1
kpiReport <-    dbGetQuery(con,paste('SELECT * FROM vSpeechCDR2018_Operator1'))
Encoding(kpiReport$G_Level_4) = "latin1"

# LQ DISTRIBUTION
lqKpis <- plotLQ(kpiReport,
                 "OVERALL",
                 "00-LQ-OVERALL.png")
lqKpis <- plotLQ(kpiReport[ (kpiReport$G_Level_1 == "Drive") & (kpiReport$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-LQ-DRIVE-CITY.png")
lqKpis <- plotLQ(kpiReport[ (kpiReport$G_Level_1 == "Drive") & (kpiReport$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-LQ-DRIVE-RURAL.png")
lqKpis <- plotLQ(kpiReport[ (kpiReport$G_Level_1 == "Walk") & (kpiReport$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-LQ-WALK-CITY.png")
lqKpis <- plotLQ(kpiReport[ (kpiReport$G_Level_1 == "Walk") & (kpiReport$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-LQ-WALK-TRAIN.png")

# CALL MODE STATISTICS
lqKpis <- plotCM(kpiReport,
                 "OVERALL",
                 "00-CM-OVERALL.png")
lqKpis <- plotCM(kpiReport[ (kpiReport$G_Level_1 == "Drive") & (kpiReport$G_Level_2 == "City"),],
                 "DRIVE - CITY",
                 "01-CM-DRIVE-CITY.png")
lqKpis <- plotCM(kpiReport[ (kpiReport$G_Level_1 == "Drive") & (kpiReport$G_Level_2 == "Connecting Roads"),],
                 "DRIVE - CONN. ROUTES",
                 "02-CM-DRIVE-RURAL.png")
lqKpis <- plotCM(kpiReport[ (kpiReport$G_Level_1 == "Walk") & (kpiReport$G_Level_2 == "City"),],
                 "WALK - CITY",
                 "03-CM-WALK-CITY.png")
lqKpis <- plotCM(kpiReport[ (kpiReport$G_Level_1 == "Walk") & (kpiReport$G_Level_2 == "Train Route"),],
                 "WALK - TRAIN",
                 "04-CM-WALK-TRAIN.png")


barplot(kpiReport$BW,kpiReport$LQ)
