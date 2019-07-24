library(DBI)
library(odbc)

library(dplyr)
library(tidyr)
library(plotrix)
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

# CREATE FUNCTION TO GRAB ACTIVE SESSIONIDS FROM CDR 2018
rankingCalc <- function(kpiValue, l_thresh, m_thresh, h_thresh, max)
{
  if(l_thresh < m_thresh)
  { 
    if(is.na(kpiValue)) {return(0)}
    if(kpiValue <= l_thresh) {return(0)}
    if((kpiValue - l_thresh) * (m_thresh-kpiValue) >= 0) {return(0.8*max*(kpiValue - l_thresh)/(m_thresh - l_thresh))}
    if((kpiValue - m_thresh) * (h_thresh-kpiValue) >= 0) {return(0.8*max + 0.2*max*(kpiValue - m_thresh)/(h_thresh - m_thresh))}
    if(kpiValue >= h_thresh) {return(max)}
    return(998)
  }
  else
  {
    if(is.na(kpiValue)) {return(0)}
    if(kpiValue >= l_thresh) {return(0)}
    if((kpiValue - l_thresh) * (m_thresh-kpiValue) >= 0) {return(0.8*max*(kpiValue - l_thresh)/(m_thresh - l_thresh))}
    if((kpiValue - m_thresh) * (h_thresh-kpiValue) >= 0) {return(0.8*max + 0.2*max*(kpiValue - m_thresh)/(h_thresh - m_thresh))}
    if(kpiValue <= h_thresh) {return(max)}
    return(999)
  }
}

plotVoice <- function()
{
  ranL2ALL <- kpiReport[which(kpiReport$Rnk > 1 & kpiReport$Rnk < 500),]
  ranL3ALL <- kpiReport[which(kpiReport$Rnk > 1000 & kpiReport$Rnk < 5001),]
  ranL4DCI <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City"),]
  ranL4DCR <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads"),]
  ranL4WCI <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City"),]
  ranL4WTR <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route"),]  
  
  png(file=paste("r","11-L2Voice",".png",sep=""),width=1200,height=7700,res=72)
  i <- i + 1
  layout(matrix(c(1,1,2,2,3,4,5,6,7,8,9,10), 
                nrow=6, 
                ncol=2, 
                byrow = TRUE), 
         widths=c(1,1,1,1,1,1), 
         heights=c(2,2,1,5,1,3))
  
  bp <- barplot(t(cbind(   ranL2ALL$rCallSetupSuccessRatio
                          ,ranL2ALL$rDropCallRatio
                          ,ranL2ALL$rCallSetupTimeAverageValue
                          ,ranL2ALL$rCallSetupTimePoorRatio
                          ,ranL2ALL$rCallSetupTime10PercentileValue
                          ,ranL2ALL$rPOLQAMOSAverageValue
                          ,ranL2ALL$rPOLQAMOSPoorRatio
                          ,ranL2ALL$rPOLQAMOS90PercentileValue)),
                main = "Ranking Voice - OVERAL",
                names.arg = paste(ranL2ALL$G_Level_1,"-",ranL2ALL$G_Level_2), # x-axis labels
                cex.names = 1.5, # makes x-axis labels small enough to show all
                #horiz = TRUE,
                xlab = "Ranking Module",
                ylab = "",
                yaxt='n',
                #xlim = c(0,20), # these two lines allow space for the legend
                ylim = c(0,200), # these two lines allow space for the legend
                col=c("dodgerblue4","firebrick4","darkorange4","darkorange3","darkorange2","deeppink4","deeppink3","deeppink2"),
                width = 1) # these two lines allow space for the legend
  legend("topright",  # "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
         legend = c("Call Setup Success Ratio", 
                    "Drop Call Ratio", 
                    "Call Setup Time - Average Value", 
                    "Call Setup Time - < 15 sec Ratio", 
                    "Call Setup Time - 10 PCTL Value", 
                    "POLQA MOS - Average Value",
                    "POLQA MOS - < 1.6 Ratio",
                    "POLQA MOS - 90 PCTL Value"), #in order from top to bottom
         fill=c("dodgerblue4","firebrick4","darkorange4","darkorange3","darkorange2","deeppink4","deeppink3","deeppink2"),
         #lty=1:1,
         title = "KPI-s")
  text(x=bp, 
       y=0, 
       labels=paste("CSSR:",round(ranL2ALL$rCallSetupSuccessRatio,digits=2)),
       col = "white" ,
       cex = 1.4,
       pos=3, xpd=NA)
  text(x=bp, 
       y=ranL2ALL$rCallSetupSuccessRatio, 
       labels=paste("DCR",round(ranL2ALL$rDropCallRatio,digits=2)),
       col = "white" ,
       cex = 1.4,
       pos=3, xpd=NA)
  text(x=bp, 
       y=ranL2ALL$rCallSetupSuccessRatio + ranL2ALL$rDropCallRatio, 
       labels=paste(
                      "CST (AVG+LOW+HGH): ",round(ranL2ALL$rCallSetupTimeAverageValue,digits=2),
                      "+", round(ranL2ALL$rCallSetupTimePoorRatio,digits=2),
                      "+", round(ranL2ALL$rCallSetupTime10PercentileValue,digits=2)
                    ),
       col = "white" ,
       cex = 1.4,
       pos=3, xpd=NA)
  text(x=bp, 
       y=(ranL2ALL$rCallSetupSuccessRatio 
          + ranL2ALL$rDropCallRatio 
          + ranL2ALL$rCallSetupTimeAverageValue 
          + ranL2ALL$rCallSetupTimePoorRatio 
          + ranL2ALL$rCallSetupTime10PercentileValue 
          + ranL2ALL$rPOLQAMOSAverageValue 
          + ranL2ALL$rPOLQAMOSPoorRatio 
          + ranL2ALL$rPOLQAMOS90PercentileValue),
       labels=paste(
         "TOTAL: ",round((ranL2ALL$rCallSetupSuccessRatio 
                          + ranL2ALL$rDropCallRatio 
                          + ranL2ALL$rCallSetupTimeAverageValue 
                          + ranL2ALL$rCallSetupTimePoorRatio 
                          + ranL2ALL$rCallSetupTime10PercentileValue 
                          + ranL2ALL$rPOLQAMOSAverageValue 
                          + ranL2ALL$rPOLQAMOSPoorRatio 
                          + ranL2ALL$rPOLQAMOS90PercentileValue),digits=2),
         "\n\nPOLQA MOS (AVG+LOW+HGH): ",round(ranL2ALL$rPOLQAMOSAverageValue,digits=2),
         "+", round(ranL2ALL$rPOLQAMOSPoorRatio,digits=2),
         "+", round(ranL2ALL$rPOLQAMOS90PercentileValue,digits=2)
       ),
       col = "black" ,
       cex = 1.4,
       pos=3, xpd=NA)

  bp <- barplot(t(cbind(   ranL2ALL$mCallSetupSuccessRatio           - ranL2ALL$rCallSetupSuccessRatio
                           ,ranL2ALL$mDropCallRatio                   - ranL2ALL$rDropCallRatio
                           ,ranL2ALL$mCallSetupTimeAverageValue       - ranL2ALL$rCallSetupTimeAverageValue
                           ,ranL2ALL$mCallSetupTimePoorRatio          - ranL2ALL$rCallSetupTimePoorRatio
                           ,ranL2ALL$mCallSetupTime10PercentileValue  - ranL2ALL$rCallSetupTime10PercentileValue
                           ,ranL2ALL$mPOLQAMOSAverageValue            - ranL2ALL$rPOLQAMOSAverageValue
                           ,ranL2ALL$mPOLQAMOSPoorRatio               - ranL2ALL$rPOLQAMOSPoorRatio
                           ,ranL2ALL$mPOLQAMOS90PercentileValue       - ranL2ALL$rPOLQAMOS90PercentileValue)),
                main = "Ranking Voice - Points lost - OVERAL",
                names.arg = paste(ranL2ALL$G_Level_1,"-",ranL2ALL$G_Level_2), # x-axis labels
                cex.names = 1.5, # makes x-axis labels small enough to show all
                #horiz = TRUE,
                xlab = "Ranking Module",
                ylab = "",
                yaxt='n',
                #xlim = c(0,20), # these two lines allow space for the legend
                ylim = c(0,50), # these two lines allow space for the legend
                col=c("dodgerblue4","firebrick4","darkorange4","darkorange3","darkorange2","deeppink4","deeppink3","deeppink2"),
                width = 1) # these two lines allow space for the legend
  legend("topright",  # "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
         legend = c("Call Setup Success Ratio", 
                    "Drop Call Ratio", 
                    "Call Setup Time - Average Value", 
                    "Call Setup Time - < 15 sec Ratio", 
                    "Call Setup Time - 10 PCTL Value", 
                    "POLQA MOS - Average Value",
                    "POLQA MOS - < 1.6 Ratio",
                    "POLQA MOS - 90 PCTL Value"), #in order from top to bottom
         fill=c("dodgerblue4","firebrick4","darkorange4","darkorange3","darkorange2","deeppink4","deeppink3","deeppink2"),
         #lty=1:1,
         title = "KPI-s")
  text(x=bp, 
       y=( ( ranL2ALL$mCallSetupSuccessRatio - ranL2ALL$rCallSetupSuccessRatio) 
           + (ranL2ALL$mDropCallRatio - ranL2ALL$rDropCallRatio) 
           + (ranL2ALL$mCallSetupTimeAverageValue - ranL2ALL$rCallSetupTimeAverageValue)
           + (ranL2ALL$mCallSetupTimePoorRatio - ranL2ALL$rCallSetupTimePoorRatio)
           + (ranL2ALL$mCallSetupTime10PercentileValue - ranL2ALL$rCallSetupTime10PercentileValue)
           + (ranL2ALL$mPOLQAMOSAverageValue - ranL2ALL$rPOLQAMOSAverageValue)
           + (ranL2ALL$mPOLQAMOSPoorRatio - ranL2ALL$rPOLQAMOSPoorRatio)
           + (ranL2ALL$mPOLQAMOS90PercentileValue		  - ranL2ALL$rPOLQAMOS90PercentileValue) ),
       labels=paste( "TOTAL: ",round(( ( ranL2ALL$mCallSetupSuccessRatio - ranL2ALL$rCallSetupSuccessRatio) 
                                       + (ranL2ALL$mDropCallRatio - ranL2ALL$rDropCallRatio) 
                                       + (ranL2ALL$mCallSetupTimeAverageValue - ranL2ALL$rCallSetupTimeAverageValue)
                                       + (ranL2ALL$mCallSetupTimePoorRatio - ranL2ALL$rCallSetupTimePoorRatio)
                                       + (ranL2ALL$mCallSetupTime10PercentileValue - ranL2ALL$rCallSetupTime10PercentileValue)
                                       + (ranL2ALL$mPOLQAMOSAverageValue - ranL2ALL$rPOLQAMOSAverageValue)
                                       + (ranL2ALL$mPOLQAMOSPoorRatio - ranL2ALL$rPOLQAMOSPoorRatio)
                                       + (ranL2ALL$mPOLQAMOS90PercentileValue		  - ranL2ALL$rPOLQAMOS90PercentileValue) ), digits = 2),
                     "\n\nCSSR: ",round(( ranL2ALL$mCallSetupSuccessRatio - ranL2ALL$rCallSetupSuccessRatio),digits = 2),
                     "\nDCR: ",round((ranL2ALL$mDropCallRatio - ranL2ALL$rDropCallRatio),digits = 2),
                     "\nCST (AVG+LOW+HGH): ",round((ranL2ALL$mCallSetupTimeAverageValue - ranL2ALL$rCallSetupTimeAverageValue),digits = 2),
                     "+",round((ranL2ALL$mCallSetupTimePoorRatio - ranL2ALL$rCallSetupTimePoorRatio),digits = 2),
                     "+",round((ranL2ALL$mCallSetupTime10PercentileValue - ranL2ALL$rCallSetupTime10PercentileValue),digits = 2),
                     "\nPOLQA MOS (AVG+LOW+HGH): ",round((ranL2ALL$mPOLQAMOSAverageValue - ranL2ALL$rPOLQAMOSAverageValue),digits = 2),
                     "+",round((ranL2ALL$mPOLQAMOSPoorRatio - ranL2ALL$rPOLQAMOSPoorRatio),digits = 2),
                     "+",round((ranL2ALL$mPOLQAMOS90PercentileValue		  - ranL2ALL$rPOLQAMOS90PercentileValue),digits = 2)
                     ) ,
       col = "red" ,
       cex = 1.4,
       pos=3, 
       xpd=NA)

  par(mar=c(5,20,4,1)+.1)
  bp <- barplot(t(cbind(   ranL3ALL$rCallSetupSuccessRatio
                           ,ranL3ALL$rDropCallRatio
                           ,ranL3ALL$rCallSetupTimeAverageValue
                           ,ranL3ALL$rCallSetupTimePoorRatio
                           ,ranL3ALL$rCallSetupTime10PercentileValue
                           ,ranL3ALL$rPOLQAMOSAverageValue
                           ,ranL3ALL$rPOLQAMOSPoorRatio
                           ,ranL3ALL$rPOLQAMOS90PercentileValue)),
                main = "Ranking Voice - G_Level 3",
                horiz=TRUE, las=1,
                names.arg = paste(ranL3ALL$G_Level_1,"-",ranL3ALL$G_Level_2,"\n",ranL3ALL$G_Level_3,"(",ranL3ALL$CallAttempts,")"), # x-axis labels
                cex.names = 1.5, # makes x-axis labels small enough to show all
                #horiz = TRUE,
                xlab = "",
                ylab = "",
                xaxt='n',
                xlim = c(0,200), # these two lines allow space for the legend
                #ylim = c(0,200), # these two lines allow space for the legend
                col=c("dodgerblue4","firebrick4","darkorange4","darkorange3","darkorange2","deeppink4","deeppink3","deeppink2"),
                width = 1) # these two lines allow space for the legend
  legend("topright",  # "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
         legend = c("Call Setup Success Ratio", 
                    "Drop Call Ratio", 
                    "Call Setup Time - Average Value", 
                    "Call Setup Time - < 15 sec Ratio", 
                    "Call Setup Time - 10 PCTL Value", 
                    "POLQA MOS - Average Value",
                    "POLQA MOS - < 1.6 Ratio",
                    "POLQA MOS - 90 PCTL Value"), #in order from top to bottom
         fill=c("dodgerblue4","firebrick4","darkorange4","darkorange3","darkorange2","deeppink4","deeppink3","deeppink2"),
         #lty=1:1,
         title = "KPI-s")
  text(x=(ranL3ALL$rCallSetupSuccessRatio + ranL3ALL$rDropCallRatio + ranL3ALL$rCallSetupTimeAverageValue + ranL3ALL$rCallSetupTimePoorRatio + ranL3ALL$rCallSetupTime10PercentileValue + ranL3ALL$rPOLQAMOSAverageValue + ranL3ALL$rPOLQAMOSPoorRatio + ranL3ALL$rPOLQAMOS90PercentileValue), 
       y=bp, 
       labels=paste(
                    round((ranL3ALL$rCallSetupSuccessRatio + ranL3ALL$rDropCallRatio + ranL3ALL$rCallSetupTimeAverageValue + ranL3ALL$rCallSetupTimePoorRatio + ranL3ALL$rCallSetupTime10PercentileValue + ranL3ALL$rPOLQAMOSAverageValue + ranL3ALL$rPOLQAMOSPoorRatio + ranL3ALL$rPOLQAMOS90PercentileValue), digits=2),
                    "out of",
                    round((ranL3ALL$mCallSetupSuccessRatio + ranL3ALL$mDropCallRatio + ranL3ALL$mCallSetupTimeAverageValue + ranL3ALL$mCallSetupTimePoorRatio + ranL3ALL$mCallSetupTime10PercentileValue + ranL3ALL$mPOLQAMOSAverageValue + ranL3ALL$mPOLQAMOSPoorRatio + ranL3ALL$mPOLQAMOS90PercentileValue), digits=2),
                    "\n",
                    round( (100*(ranL3ALL$rCallSetupSuccessRatio + ranL3ALL$rDropCallRatio + ranL3ALL$rCallSetupTimeAverageValue + ranL3ALL$rCallSetupTimePoorRatio + ranL3ALL$rCallSetupTime10PercentileValue + ranL3ALL$rPOLQAMOSAverageValue + ranL3ALL$rPOLQAMOSPoorRatio + ranL3ALL$rPOLQAMOS90PercentileValue)/(ranL3ALL$mCallSetupSuccessRatio + ranL3ALL$mDropCallRatio + ranL3ALL$mCallSetupTimeAverageValue + ranL3ALL$mCallSetupTimePoorRatio + ranL3ALL$mCallSetupTime10PercentileValue + ranL3ALL$mPOLQAMOSAverageValue + ranL3ALL$mPOLQAMOSPoorRatio + ranL3ALL$mPOLQAMOS90PercentileValue) ), digits=2),
                    "%"
                    ), 
       pos=4, 
       xpd=NA)
  
  dev.off()
  layout(1)
}

plotKPI <- function()
{
  plotLPIs <- c("CallSetupSuccessRatio", "DropCallRatio", "CallSetupTimeAverageValue", "CallSetupTimePoorRatio", "CallSetupTime10PercentileValue", "POLQAMOSAverageValue", "POLQAMOSPoorRatio", "POLQAMOS90PercentileValue", "HTTPTransferFDFSSuccessRatio", "HTTPTransferFDTTDLMDR10PercentileValue", "HTTPTransferFDTTDLMDRAverageValue", "HTTPTransferFDTTDLMDR90PercentileValue", "HTTPTransferFDTTULMDR10PercentileValue", "HTTPTransferFDTTULMDRAverageValue", "HTTPTransferFDTTULMDR90PercentileValue", "HTTPBrowsingTestSuccessRatio", "HTTPBrowsingTestRoundTripTime", "VideoStreamingTestSuccessRatio", "VideoStreamingVideoMOS10PercentileValue", "VideoStreamingVideoMOSAverageValue", "VideoStreamingTimeto1stPictureAverageValue", "VideoStreamingTimeto1stPicturePoorRatio", "MobileApplicationTestSuccessRatio", "MobileApplicationTestContentTransferTimeAverageValue")
  i <- 0
  for(kpi in plotLPIs)
  {
    norm <- kpiReport$normFactorVoice
    if(grepl("Call",kpi))              { kpiReport$norm <- kpiReport$normFactorVoice 	; kpiReport$samples <- kpiReport$CallAttempts}
    if(grepl("POLQA",kpi))             { kpiReport$norm <- kpiReport$normFactorSpeech ; kpiReport$samples <- kpiReport$SpeechTestsAttempts}
    if(grepl("HTTPTransfer",kpi))      { kpiReport$norm <- kpiReport$normFactorTrns 	; kpiReport$samples <- kpiReport$HttpFdfsDlAttemptsCount}
    if(grepl("HTTPBrowsing",kpi))      { kpiReport$norm <- kpiReport$normFactorBRWS 	; kpiReport$samples <- kpiReport$HttpBrowsingAttemptsCount}
    if(grepl("VideoStreaming",kpi))    { kpiReport$norm <- kpiReport$normFactorBRWS 	; kpiReport$samples <- kpiReport$StreamingAttemptsCount}
    if(grepl("MobileApplication",kpi)) { kpiReport$norm <- kpiReport$normFactorBRWS 	; kpiReport$samples <- kpiReport$ApplicationAttemptsCount}

    cat("plotting KPI: ", kpi, "\n")
    r_index <- which(names(kpiReport)==paste("r",kpi,sep=""))
    cat("Ranking index: ",r_index,"\n")
    m_index <- which(names(kpiReport)==paste("m",kpi,sep=""))
    cat("Maximum index: ",m_index,"\n")
    ranL2 <- kpiReport[which(kpiReport$Rnk > 1 & kpiReport$Rnk < 500),]
    ranL3 <- kpiReport[which(kpiReport$Rnk > 1000 & kpiReport$Rnk < 5001),]
    ranL4DCI <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City"),]
    ranL4DCR <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads"),]
    ranL4WCI <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City"),]
    ranL4WTR <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route"),]
    
    png(file=paste("r",(100+i),"-",kpi,".png",sep=""),width=1200,height=5400,res=72)
    i <- i + 1
    layout(matrix(c(1,1,2,3,4,5,6,7,8,9,10,11), 
                  nrow=6, 
                  ncol=2, 
                  byrow = TRUE), 
           widths=c(1,1,1,1,1,1), 
           heights=c(1,1,5,5,1,3))

    # PLOT OVERALL DIAGRAM
    bp <- barplot(ranL2[,r_index],
                  main=paste("Ranking Points", kpi," - Overal"),                    # MAIN TITLE
                  names.arg=paste(ranL2$G_Level_1, " - ", ranL2$G_Level_2),
                  col = "dodgerblue4",
                  ylim=c(0,1.1*max(ranL2[,m_index])),
                  yaxt='n')                                         # REMOVE Y AXIS NUMBERS
    # ADD LABELS ABOVE GRAPH
    text(x=bp, y=ranL2[,r_index], labels=paste(round(ranL2[,r_index],digits=2)," - ",round(100*ranL2[,r_index]/ranL2[,m_index],digits=2),"%"), pos=3, xpd=NA)

    # PLOT G-LEVEL-3
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,10,4,1)+.1)
    bp <- barplot(ranL3[,r_index],
                  main=paste("Ranking Points", kpi," - G_LEVEL_3"),
                  names.arg=paste(ranL3$G_Level_1, " \n ", ranL3$G_Level_3," (",ranL3$CallAttempts,")"),
    col = "dodgerblue4",
    horiz=TRUE, las=1,
    xlim=c(0,1.1*max(ranL3[,m_index])),
    xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=ranL3[,r_index], y=bp, labels=paste(round(ranL3[,r_index],digits=2)," - ",round(100*ranL3[,r_index]/ranL3[,m_index],digits=2),"%"), pos=4, xpd=NA)

    # PLOT G-LEVEL-3 (Points lost per module)
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,15,4,1)+.1)
    bp <- barplot( (ranL3[,m_index]-ranL3[,r_index]) * ranL3$samples / ranL3$norm,
                  main=paste("Ranking Points", kpi," - G_LEVEL_3\npoints lost in module\n(normalized with samples count)"),
                  names.arg=paste( ranL3$G_Level_1, 
                                   " \n ", ranL3$G_Level_3,
                                   " \n(",ranL3$samples,
                                   " - ",
                                   round(100.0*ranL3$samples/ranL3$norm,digits=2),
                                   "%)",
                                   sep=""),
                  col = "red",
                  horiz=TRUE, las=1,
                  xlim=c(0,1.1*max(ranL3[,m_index])),
                  xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=(ranL3[,m_index]-ranL3[,r_index])*ranL3$samples/ranL3$norm, 
         y=bp, 
         labels=paste(round((ranL3[,m_index]-ranL3[,r_index])*ranL3$samples/ranL3$norm, digits=2)," points lost"), 
         pos=4, 
         xpd=NA)
    
    # PLOT G-LEVEL-4
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,15,4,1)+.1)
    bp <- barplot(ranL4DCI[,r_index],
                  main=paste("Ranking Points", kpi," - DRIVE - CITY"),
                  names.arg=paste(ranL4DCI$G_Level_3, " \n ", ranL4DCI$G_Level_4," (",ranL4DCI$CallAttempts,")"),
                  col = "dodgerblue4",
                  horiz=TRUE, las=1,
                  xlim=c(0,1.1*max(ranL4DCI[,m_index])),
                  xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=ranL4DCI[,r_index], y=bp, labels=paste(round(ranL4DCI[,r_index],digits=2)," - ",round(100*ranL4DCI[,r_index]/ranL4DCI[,m_index],digits=2),"%"), pos=4, xpd=NA)
    
    # PLOT G-LEVEL-4 (Points lost per city in drive city)
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,20,4,1)+.1)
    bp <- barplot( (ranL4DCI[,m_index]-ranL4DCI[,r_index]) * ranL4DCI$samples / ranL4DCI$norm,
                   main=paste("Ranking Points", kpi," - G_LEVEL_4\npoints lost in Drive City\n(normalized with samples count)"),
                   names.arg=paste(ranL4DCI$G_Level_3, " \n ", ranL4DCI$G_Level_4," (",ranL4DCI$CallAttempts,")"),
                   col = "red",
                   horiz=TRUE, las=1,
                   xlim=c(0,0.5*max(ranL4DCI[,m_index])),
                   xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=(ranL4DCI[,m_index]-ranL4DCI[,r_index])*ranL4DCI$samples/ranL4DCI$norm, 
         y=bp, 
         labels=paste(round((ranL4DCI[,m_index]-ranL4DCI[,r_index])*ranL4DCI$samples/ranL4DCI$norm, digits=5)," points lost"), 
         pos=4, 
         xpd=NA)
    
    # PLOT G-LEVEL-4
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,20,4,1)+.1)
    bp <- barplot(ranL4DCR[,r_index],
                  main=paste("Ranking Points", kpi," - DRIVE - Conn. Roads"),
                  names.arg=paste(ranL4DCR$G_Level_3, " \n ", ranL4DCR$G_Level_4," (",ranL4DCR$CallAttempts,")"),
                  col = "dodgerblue4",
                  horiz=TRUE, las=1,
                  xlim=c(0,1.1*max(ranL4DCR[,m_index])),
                  xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=ranL4DCR[,r_index], y=bp, labels=paste(round(ranL4DCR[,r_index],digits=2)," - ",round(100*ranL4DCR[,r_index]/ranL4DCR[,m_index],digits=2),"%"), pos=4, xpd=NA)
    
    # PLOT G-LEVEL-4 (Points lost per city in drive connecting routes)
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,20,4,1)+.1)
    bp <- barplot( (ranL4DCR[,m_index]-ranL4DCR[,r_index]) * ranL4DCR$samples / ranL4DCR$norm,
                   main=paste("Ranking Points", kpi," - G_LEVEL_4\npoints lost in Conn. Routes\n(normalized with samples count)"),
                   names.arg=paste(ranL4DCR$G_Level_3, " \n ", ranL4DCR$G_Level_4," (",ranL4DCR$CallAttempts,")"),
                   col = "red",
                   horiz=TRUE, las=1,
                   xlim=c(0,0.5*max(ranL4DCR[,m_index])),
                   xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=(ranL4DCR[,m_index]-ranL4DCR[,r_index])*ranL4DCR$samples/ranL4DCR$norm, 
         y=bp, 
         labels=paste(round((ranL4DCR[,m_index]-ranL4DCR[,r_index])*ranL4DCR$samples/ranL4DCR$norm, digits=5)," points lost"), 
         pos=4, 
         xpd=NA)
    
    # PLOT G-LEVEL-4
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,15,4,1)+.1)
    bp <- barplot(ranL4WCI[,r_index],
                  main=paste("Ranking Points", kpi," - WALK - City"),
                  names.arg=paste(ranL4WCI$G_Level_3, " \n ", ranL4WCI$G_Level_4," (",ranL4WCI$CallAttempts,")"),
                  col = "dodgerblue4",
                  horiz=TRUE, las=1,
                  xlim=c(0,1.1*max(ranL4WCI[,m_index])),
                  xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=ranL4WCI[,r_index], y=bp, labels=paste(round(ranL4WCI[,r_index],digits=2)," - ",round(100*ranL4WCI[,r_index]/ranL4WCI[,m_index],digits=2),"%"), pos=4, xpd=NA)
    
    # PLOT G-LEVEL-4 (Points lost per city in drive connecting routes)
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,15,4,1)+.1)
    bp <- barplot( (ranL4WCI[,m_index]-ranL4WCI[,r_index]) * ranL4WCI$samples / ranL4WCI$norm,
                   main=paste("Ranking Points", kpi," - G_LEVEL_4\npoints lost in Walk City\n(normalized with samples count)"),
                   names.arg=paste(ranL4WCI$G_Level_3, " \n ", ranL4WCI$G_Level_4," (",ranL4WCI$CallAttempts,")"),
                   col = "red",
                   horiz=TRUE, las=1,
                   xlim=c(0,0.5*max(ranL4WCI[,m_index])),
                   xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=(ranL4WCI[,m_index]-ranL4WCI[,r_index])*ranL4WCI$samples/ranL4WCI$norm, 
         y=bp, 
         labels=paste(round((ranL4WCI[,m_index]-ranL4WCI[,r_index])*ranL4WCI$samples/ranL4WCI$norm, digits=5)," points lost"), 
         pos=4, 
         xpd=NA)

    # PLOT G-LEVEL-4
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,20,4,1)+.1)
    bp <- barplot(ranL4WTR[,r_index],
                  main=paste("Ranking Points", kpi," - WALK - Train"),
                  names.arg=paste(ranL4WTR$G_Level_3, " - ", ranL4WTR$G_Level_4,"\n",ranL4WTR$Fleet," - ",ranL4WTR$WagonNumber, " (",ranL4WTR$CallAttempts,")"),
                  col = "dodgerblue4",
                  horiz=TRUE, las=1,
                  xlim=c(0,1.1*max(ranL4WTR[,m_index])),
                  xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=ranL4WTR[,r_index], y=bp, labels=paste(round(ranL4WTR[,r_index],digits=2)," - ",round(100*ranL4WTR[,r_index]/ranL4WTR[,m_index],digits=2),"%"), pos=4, xpd=NA)
    
    # PLOT G-LEVEL-4 (Points lost per city in drive connecting routes)
    # 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'
    par(mar=c(5,20,4,1)+.1)
    bp <- barplot( (ranL4WTR[,m_index]-ranL4WTR[,r_index]) * ranL4WTR$samples / ranL4WTR$norm,
                   main=paste("Ranking Points", kpi," - G_LEVEL_4\npoints lost in Walk Train\n(normalized with samples count)"),
                   names.arg=paste(ranL4WTR$G_Level_3, " - ", ranL4WTR$G_Level_4,"\n",ranL4WTR$Fleet," - ",ranL4WTR$WagonNumber, " (",ranL4WTR$CallAttempts,")"),
                   col = "red",
                   horiz=TRUE, las=1,
                   xlim=c(0,0.5*max(ranL4WTR[,m_index])),
                   xaxt='n')
    # ADD LABELS ABOVE GRAPH
    text(x=(ranL4WTR[,m_index]-ranL4WTR[,r_index])*ranL4WTR$samples/ranL4WTR$norm, 
         y=bp, 
         labels=paste(round((ranL4WTR[,m_index]-ranL4WTR[,r_index])*ranL4WTR$samples/ranL4WTR$norm, digits=5)," points lost"), 
         pos=4, 
         xpd=NA)

    dev.off()
    layout(1)
  }
}

# MAIN PROGRAM
setwd("C:/Users/tmiksa/Desktop/Ranking_R")

# LOAD RANKING PARAMETERS FROM INPUT FILE
rankingParams = read.csv("rankingParams.csv")  # read csv file 

# Connect BLNDB11
# HERE YOU HAVE TO SET CORRECT DB NAME
##########################################
con <- dbConnect(odbc::odbc(),.connection_string = 'driver={SQL Server};server=blndb11;database=DE_PreBM_Voice_02;trusted_connection=true')

# EXTRACT KPI REPORT FOR OPERATOR
  kpiReport <-    dbGetQuery(con,paste("SELECT  Rnk
                                               ,Vendor
                                               ,G_Level_1
                                               ,G_Level_2
                                               ,G_Level_3
                                               ,G_Level_4
                                               ,Fleet
                                               ,WagonNumber
                                               ,MinTimestamp
                                               ,MaxTimestamp
                                               ,VoiceKPIs
                                               ,CallAttempts
                                               ,CallFailed
                                               ,CallDropped
                                               ,CallCompleted
                                               ,CallSetupSuccesRatio
                                               ,CallDropRatio
                                               ,CallSuccesRatio
                                               ,CstMinimum
                                               ,CstAverage
                                               ,CstMaximum
                                               ,CstPercentile10
                                               ,CstPercentile50
                                               ,CstPercentile90
                                               ,CstPoorCount
                                               ,CstGoodCount
                                               ,CstPoorRatio
                                               ,ModeVoLtetoVoLteCount
                                               ,ModeVoLtetoVoLteRatio
                                               ,ModeEndVoLtetoVoLteRatio
                                               ,ModeCsfbRatio
                                               ,SpeechKPIs
                                               ,SpeechTestsAttempts
                                               ,SpeechTestsPoorCount
                                               ,SpeechPolqaMinimun
                                               ,SpeechPolqaPercentile10
                                               ,SpeechPolqaAverage
                                               ,SpeechPolqaPercentile50
                                               ,SpeechPolqaPercentile90
                                               ,SpeechPolqaMaximum
                                               ,SpeechTestsPoorRatio
                                               ,SpeechVoLtePolqaAverage
                                               ,HttpFdfsKPIs
                                               ,HttpFdfsDlAttemptsCount
                                               ,HttpFdfsDlCutoffCount
                                               ,HttpFdfsDlCompletedCount
                                               ,HttpFdfsDlAccessFailedCount
                                               ,HttpFdfsDlAccessSuccessCount
                                               ,HttpFdfsDlAccessFailedRatio
                                               ,HttpFdfsDlCutoffRatio
                                               ,HttpFdfsDlCompletedRatio
                                               ,HttpFdfsUlAttemptsCount
                                               ,HttpFdfsUlCutoffCount
                                               ,HttpFdfsUlCompletedCount
                                               ,HttpFdfsUlAccessFailedCount
                                               ,HttpFdfsUlAccessSuccessCount
                                               ,HttpFdfsUlAccessFailedRatio
                                               ,HttpFdfsUlCutoffRatio
                                               ,HttpFdfsUlCompletedRatio
                                               ,HttpFdfsCompletedRatio
                                               ,HttpFdttDlKPIs
                                               ,HttpFdttDlMdrMinimum
                                               ,HttpFdttDlMdrPercentile10
                                               ,HttpFdttDlMdrAverage
                                               ,HttpFdttDlMdrPercentile50
                                               ,HttpFdttDlMdrPercentile90
                                               ,HttpFdttDlMdrMaximum
                                               ,HttpFdttUlKPIs
                                               ,HttpFdttUlMdrMinimum
                                               ,HttpFdttUlMdrPercentile10
                                               ,HttpFdttUlMdrAverage
                                               ,HttpFdttUlMdrPercentile50
                                               ,HttpFdttUlMdrPercentile90
                                               ,HttpFdttUlMdrMaximum
                                               ,HttpBrowsingKPIs
                                               ,HttpBrowsingAttemptsCount
                                               ,HttpBrowsingCutoffCount
                                               ,HttpBrowsingCompletedCount
                                               ,HttpBrowsingAccessFailedCount
                                               ,HttpBrowsingAccessSuccessCount
                                               ,HttpBrowsingAccessFailedRatio
                                               ,HttpBrowsingCutoffRatio
                                               ,HttpBrowsingCompletedRatio
                                               ,HttpBrowsingTransferTimeMinimum
                                               ,HttpBrowsingTransferTimePercentile10
                                               ,HttpBrowsingTransferTimeAverage
                                               ,HttpBrowsingTransferTimePercentile50
                                               ,HttpBrowsingTransferTimePercentile90
                                               ,HttpBrowsingTransferTimeMaximum
                                               ,HttpBrowsingTransferRttMinimum
                                               ,HttpBrowsingTransferRttPercentile10
                                               ,HttpBrowsingTransferRttAverage
                                               ,HttpBrowsingTransferRttPercentile50
                                               ,HttpBrowsingTransferRttPercentile90
                                               ,HttpBrowsingTransferRttMaximum
                                               ,VideoStreamingKPIs
                                               ,StreamingAttemptsCount
                                               ,StreamingFailedCount
                                               ,StreamingCompletedCount
                                               ,StreamingCompletedRatio
                                               ,StreamingVmosMinimum
                                               ,StreamingVmosPercentile10
                                               ,StreamingVmosAverage
                                               ,StreamingVmosPercentile50
                                               ,StreamingVmosPercentile90
                                               ,StreamingVmosMaximum
                                               ,StreamingVmosPoorCount
                                               ,StreamingVmosGoodCount
                                               ,StreamingVmosPoorRatio
                                               ,StreamingTtfpMinimum
                                               ,StreamingTtfpPercentile10
                                               ,StreamingTtfpAverage
                                               ,StreamingTtfpPercentile50
                                               ,StreamingTtfpPercentile90
                                               ,StreamingTtfpMaximum
                                               ,StreamingTtfpGoodCount
                                               ,StreamingTtfpPoorCount
                                               ,StreamingTtfpPoorRatio
                                               ,SocialMediaKPIs
                                               ,ApplicationAttemptsCount
                                               ,ApplicationFailedCount
                                               ,ApplicationCutoffCount
                                               ,ApplicationCompletedCount
                                               ,ApplicationCompletedRatio
                                               ,ApplicationTransferTimeMinimum
                                               ,ApplicationTransferTimePercentile10
                                               ,ApplicationTransferTimeAverage
                                               ,ApplicationTransferTimePercentile50
                                               ,ApplicationTransferTimePercentile90
                                               ,ApplicationTransferTimeMaximum
                                               ,RatRanKPIs
                                               ,LteRatio
                                               ,UlHttpTransfer64Qamover30Percent
                                               ,DlHttpTransfer264Qamover10Percent
                                         FROM NEW_VDF_KPI_REPORT_OPERATOR_1"))
  
  Encoding(kpiReport$G_Level_4) = "latin1"
  TotalSamplesVoice  <- c(kpiReport$CallAttempts[kpiReport$Rnk == 100],kpiReport$CallAttempts[kpiReport$Rnk == 300],kpiReport$CallAttempts[kpiReport$Rnk == 200],kpiReport$CallAttempts[kpiReport$Rnk == 400])
  TotalSamplesSpeech <- c(kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 100],kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 300],kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 200],kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 400])
  TotalSamplesTrns   <- c(kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 100] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 100],kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 300] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 300],kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 200] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 200],kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 400] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 400])
  TotalSamplesBRWS   <- c(kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 100],kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 300],kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 200],kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 400])
  TotalSamplesVIDEO  <- c(kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 100],kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 300],kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 200],kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 400])
  TotalSamplesSOCIAL <- c(kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 100],kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 300],kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 200],kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 400])
  
  # CREATE NEW COLUMN FOR MAX RANKING POINTS DATA FRAME
  kpiReport["mCallSetupSuccessRatio"] <- NA
  kpiReport["mDropCallRatio"] <- NA
  kpiReport["mCallSetupTimeAverageValue"] <- NA
  kpiReport["mCallSetupTimePoorRatio"] <- NA
  kpiReport["mCallSetupTime10PercentileValue"] <- NA
  kpiReport["mPOLQAMOSAverageValue"] <- NA
  kpiReport["mPOLQAMOSPoorRatio"] <- NA
  kpiReport["mPOLQAMOS90PercentileValue"] <- NA
  kpiReport["mHTTPTransferFDFSSuccessRatio"] <- NA
  kpiReport["mHTTPTransferFDTTDLMDR10PercentileValue"] <- NA
  kpiReport["mHTTPTransferFDTTDLMDRAverageValue"] <- NA
  kpiReport["mHTTPTransferFDTTDLMDR90PercentileValue"] <- NA
  kpiReport["mHTTPTransferFDTTULMDR10PercentileValue"] <- NA
  kpiReport["mHTTPTransferFDTTULMDRAverageValue"] <- NA
  kpiReport["mHTTPTransferFDTTULMDR90PercentileValue"] <- NA
  kpiReport["mHTTPBrowsingTestSuccessRatio"] <- NA
  kpiReport["mHTTPBrowsingTestRoundTripTime"] <- NA
  kpiReport["mVideoStreamingTestSuccessRatio"] <- NA
  kpiReport["mVideoStreamingVideoMOS10PercentileValue"] <- NA
  kpiReport["mVideoStreamingVideoMOSAverageValue"] <- NA
  kpiReport["mVideoStreamingTimeto1stPictureAverageValue"] <- NA
  kpiReport["mVideoStreamingTimeto1stPicturePoorRatio"] <- NA
  kpiReport["mMobileApplicationTestSuccessRatio"] <- NA
  kpiReport["mMobileApplicationTestContentTransferTimeAverageValue"] <- NA
  kpiReport["normFactorVoice"] <- NA
  kpiReport["normFactorSpeech"] <- NA
  kpiReport["normFactorTrns"] <- NA
  kpiReport["normFactorBRWS"] <- NA
  kpiReport["normFactorVIDEO"] <- NA
  kpiReport["normFactorSOCIAL"] <- NA
  
  # CREATE NEW COLUMN FOR RANKING DATA FRAME
  kpiReport["rCallSetupSuccessRatio"] <- NA
  kpiReport["rDropCallRatio"] <- NA
  kpiReport["rCallSetupTimeAverageValue"] <- NA
  kpiReport["rCallSetupTimePoorRatio"] <- NA
  kpiReport["rCallSetupTime10PercentileValue"] <- NA
  kpiReport["rPOLQAMOSAverageValue"] <- NA
  kpiReport["rPOLQAMOSPoorRatio"] <- NA
  kpiReport["rPOLQAMOS90PercentileValue"] <- NA
  kpiReport["rHTTPTransferFDFSSuccessRatio"] <- NA
  kpiReport["rHTTPTransferFDTTDLMDR10PercentileValue"] <- NA
  kpiReport["rHTTPTransferFDTTDLMDRAverageValue"] <- NA
  kpiReport["rHTTPTransferFDTTDLMDR90PercentileValue"] <- NA
  kpiReport["rHTTPTransferFDTTULMDR10PercentileValue"] <- NA
  kpiReport["rHTTPTransferFDTTULMDRAverageValue"] <- NA
  kpiReport["rHTTPTransferFDTTULMDR90PercentileValue"] <- NA
  kpiReport["rHTTPBrowsingTestSuccessRatio"] <- NA
  kpiReport["rHTTPBrowsingTestRoundTripTime"] <- NA
  kpiReport["rVideoStreamingTestSuccessRatio"] <- NA
  kpiReport["rVideoStreamingVideoMOS10PercentileValue"] <- NA
  kpiReport["rVideoStreamingVideoMOSAverageValue"] <- NA
  kpiReport["rVideoStreamingTimeto1stPictureAverageValue"] <- NA
  kpiReport["rVideoStreamingTimeto1stPicturePoorRatio"] <- NA
  kpiReport["rMobileApplicationTestSuccessRatio"] <- NA
  kpiReport["rMobileApplicationTestContentTransferTimeAverageValue"] <- NA

  for(i in 1:nrow(kpiReport)) 
  {
    threshold_l <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    threshold_m <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    threshold_h <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    rankingPoints <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    if(kpiReport$G_Level_1[i] == "Drive" && kpiReport$G_Level_2[i] == "City") 
    {
      threshold_l <- rankingParams$D_CI_L
      threshold_m <- rankingParams$D_CI_M
      threshold_h <- rankingParams$D_CI_H
      rankingPoints <- rankingParams$D_CI_RP
      kpiReport$normFactorVoice[i]  <- TotalSamplesVoice[1]
      kpiReport$normFactorSpeech[i] <- TotalSamplesSpeech[1]
      kpiReport$normFactorTrns[i]   <- TotalSamplesTrns[1]  
      kpiReport$normFactorBRWS[i]   <- TotalSamplesBRWS[1]  
      kpiReport$normFactorVIDEO[i]  <- TotalSamplesVIDEO[1] 
      kpiReport$normFactorSOCIAL[i] <- TotalSamplesSOCIAL[1]
    }
    if(kpiReport$G_Level_1[i] == "Drive" && kpiReport$G_Level_2[i] == "Connecting Roads") 
    {
      threshold_l <- rankingParams$D_CR_L
      threshold_m <- rankingParams$D_CR_M
      threshold_h <- rankingParams$D_CR_H
      rankingPoints <- rankingParams$D_CR_RP
      kpiReport$normFactorVoice[i]  <- TotalSamplesVoice[2]
      kpiReport$normFactorSpeech[i] <- TotalSamplesSpeech[2]
      kpiReport$normFactorTrns[i]   <- TotalSamplesTrns[2]  
      kpiReport$normFactorBRWS[i]   <- TotalSamplesBRWS[2]  
      kpiReport$normFactorVIDEO[i]  <- TotalSamplesVIDEO[2] 
      kpiReport$normFactorSOCIAL[i] <- TotalSamplesSOCIAL[2]
    }
    if(kpiReport$G_Level_1[i] == "Walk" && kpiReport$G_Level_2[i] == "City") 
    {
      threshold_l <- rankingParams$W_CI_L
      threshold_m <- rankingParams$W_CI_M
      threshold_h <- rankingParams$W_CI_H
      rankingPoints <- rankingParams$W_CI_RP
      kpiReport$normFactorVoice[i]  <- TotalSamplesVoice[3]
      kpiReport$normFactorSpeech[i] <- TotalSamplesSpeech[3]
      kpiReport$normFactorTrns[i]   <- TotalSamplesTrns[3]  
      kpiReport$normFactorBRWS[i]   <- TotalSamplesBRWS[3]  
      kpiReport$normFactorVIDEO[i]  <- TotalSamplesVIDEO[3] 
      kpiReport$normFactorSOCIAL[i] <- TotalSamplesSOCIAL[3]
    }
    if(kpiReport$G_Level_1[i] == "Walk" && kpiReport$G_Level_2[i] == "Train Route") 
    {
      threshold_l <- rankingParams$W_TR_L
      threshold_m <- rankingParams$W_TR_M
      threshold_h <- rankingParams$W_TR_H
      rankingPoints <- rankingParams$W_TR_RP
      kpiReport$normFactorVoice[i]  <- TotalSamplesVoice[4]
      kpiReport$normFactorSpeech[i] <- TotalSamplesSpeech[4]
      kpiReport$normFactorTrns[i]   <- TotalSamplesTrns[4]  
      kpiReport$normFactorBRWS[i]   <- TotalSamplesBRWS[4]  
      kpiReport$normFactorVIDEO[i]  <- TotalSamplesVIDEO[4] 
      kpiReport$normFactorSOCIAL[i] <- TotalSamplesSOCIAL[4]
    }
    
    # FILL RANKING TABLES
    if(is.na(threshold_l[1])) {
      kpiReport$mCallSetupSuccessRatio[i]                                     <- 0
      kpiReport$mDropCallRatio[i]                                             <- 0
      kpiReport$mCallSetupTimeAverageValue[i]                                 <- 0
      kpiReport$mCallSetupTimePoorRatio[i]                                    <- 0
      kpiReport$mCallSetupTime10PercentileValue[i]                            <- 0
      kpiReport$mPOLQAMOSAverageValue[i]                                      <- 0
      kpiReport$mPOLQAMOSPoorRatio[i]                                         <- 0
      kpiReport$mPOLQAMOS90PercentileValue[i]                                 <- 0
      kpiReport$mHTTPTransferFDFSSuccessRatio[i]                              <- 0
      kpiReport$mHTTPTransferFDTTDLMDR10PercentileValue[i]                    <- 0
      kpiReport$mHTTPTransferFDTTDLMDRAverageValue[i]                         <- 0
      kpiReport$mHTTPTransferFDTTDLMDR90PercentileValue[i]                    <- 0
      kpiReport$mHTTPTransferFDTTULMDR10PercentileValue[i]                    <- 0
      kpiReport$mHTTPTransferFDTTULMDRAverageValue[i]                         <- 0
      kpiReport$mHTTPTransferFDTTULMDR90PercentileValue[i]                    <- 0
      kpiReport$mHTTPBrowsingTestSuccessRatio[i]                              <- 0
      kpiReport$mHTTPBrowsingTestRoundTripTime[i]                             <- 0
      kpiReport$mVideoStreamingTestSuccessRatio[i]                            <- 0
      kpiReport$mVideoStreamingVideoMOS10PercentileValue[i]                   <- 0
      kpiReport$mVideoStreamingVideoMOSAverageValue[i]                        <- 0
      kpiReport$mVideoStreamingTimeto1stPictureAverageValue[i]                <- 0
      kpiReport$mVideoStreamingTimeto1stPicturePoorRatio[i]                   <- 0
      kpiReport$mMobileApplicationTestSuccessRatio[i]                         <- 0
      kpiReport$mMobileApplicationTestContentTransferTimeAverageValue[i]      <- 0
      kpiReport$rCallSetupSuccessRatio[i]                                     <- 0
      kpiReport$rDropCallRatio[i]                                             <- 0
      kpiReport$rCallSetupTimeAverageValue[i]                                 <- 0
      kpiReport$rCallSetupTimePoorRatio[i]                                    <- 0
      kpiReport$rCallSetupTime10PercentileValue[i]                            <- 0
      kpiReport$rPOLQAMOSAverageValue[i]                                      <- 0
      kpiReport$rPOLQAMOSPoorRatio[i]                                         <- 0
      kpiReport$rPOLQAMOS90PercentileValue[i]                                 <- 0
      kpiReport$rHTTPTransferFDFSSuccessRatio[i]                              <- 0
      kpiReport$rHTTPTransferFDTTDLMDR10PercentileValue[i]                    <- 0
      kpiReport$rHTTPTransferFDTTDLMDRAverageValue[i]                         <- 0
      kpiReport$rHTTPTransferFDTTDLMDR90PercentileValue[i]                    <- 0
      kpiReport$rHTTPTransferFDTTULMDR10PercentileValue[i]                    <- 0
      kpiReport$rHTTPTransferFDTTULMDRAverageValue[i]                         <- 0
      kpiReport$rHTTPTransferFDTTULMDR90PercentileValue[i]                    <- 0
      kpiReport$rHTTPBrowsingTestSuccessRatio[i]                              <- 0
      kpiReport$rHTTPBrowsingTestRoundTripTime[i]                             <- 0
      kpiReport$rVideoStreamingTestSuccessRatio[i]                            <- 0
      kpiReport$rVideoStreamingVideoMOS10PercentileValue[i]                   <- 0
      kpiReport$rVideoStreamingVideoMOSAverageValue[i]                        <- 0
      kpiReport$rVideoStreamingTimeto1stPictureAverageValue[i]                <- 0
      kpiReport$rVideoStreamingTimeto1stPicturePoorRatio[i]                   <- 0
      kpiReport$rMobileApplicationTestSuccessRatio[i]                         <- 0
      kpiReport$rMobileApplicationTestContentTransferTimeAverageValue[i]      <- 0
      }
    else{
      kpiReport$mCallSetupSuccessRatio[i]                                     <- rankingPoints[1]
      kpiReport$mDropCallRatio[i]                                             <- rankingPoints[2]
      kpiReport$mCallSetupTimeAverageValue[i]                                 <- rankingPoints[3]
      kpiReport$mCallSetupTimePoorRatio[i]                                    <- rankingPoints[4]
      kpiReport$mCallSetupTime10PercentileValue[i]                            <- rankingPoints[5]
      kpiReport$mPOLQAMOSAverageValue[i]                                      <- rankingPoints[6]
      kpiReport$mPOLQAMOSPoorRatio[i]                                         <- rankingPoints[7]
      kpiReport$mPOLQAMOS90PercentileValue[i]                                 <- rankingPoints[8]
      kpiReport$mHTTPTransferFDFSSuccessRatio[i]                              <- rankingPoints[9]
      kpiReport$mHTTPTransferFDTTDLMDR10PercentileValue[i]                    <- rankingPoints[10]
      kpiReport$mHTTPTransferFDTTDLMDRAverageValue[i]                         <- rankingPoints[11]
      kpiReport$mHTTPTransferFDTTDLMDR90PercentileValue[i]                    <- rankingPoints[12]
      kpiReport$mHTTPTransferFDTTULMDR10PercentileValue[i]                    <- rankingPoints[13]
      kpiReport$mHTTPTransferFDTTULMDRAverageValue[i]                         <- rankingPoints[14]
      kpiReport$mHTTPTransferFDTTULMDR90PercentileValue[i]                    <- rankingPoints[15]
      kpiReport$mHTTPBrowsingTestSuccessRatio[i]                              <- rankingPoints[16]
      kpiReport$mHTTPBrowsingTestRoundTripTime[i]                             <- rankingPoints[17]
      kpiReport$mVideoStreamingTestSuccessRatio[i]                            <- rankingPoints[18]
      kpiReport$mVideoStreamingVideoMOS10PercentileValue[i]                   <- rankingPoints[19]
      kpiReport$mVideoStreamingVideoMOSAverageValue[i]                        <- rankingPoints[20]
      kpiReport$mVideoStreamingTimeto1stPictureAverageValue[i]                <- rankingPoints[21]
      kpiReport$mVideoStreamingTimeto1stPicturePoorRatio[i]                   <- rankingPoints[22]
      kpiReport$mMobileApplicationTestSuccessRatio[i]                         <- rankingPoints[23]
      kpiReport$mMobileApplicationTestContentTransferTimeAverageValue[i]      <- rankingPoints[24]
      kpiReport$rCallSetupSuccessRatio[i]                                     <- rankingCalc(kpiReport$CallSetupSuccesRatio[i], 				threshold_l[1], threshold_m[1], threshold_h[1], rankingPoints[1])
      kpiReport$rDropCallRatio[i]                                             <- rankingCalc(kpiReport$CallDropRatio[i], 						threshold_l[2], threshold_m[2], threshold_h[2], rankingPoints[2])
      kpiReport$rCallSetupTimeAverageValue[i]                                 <- rankingCalc(kpiReport$CstAverage[i], 						threshold_l[3], threshold_m[3], threshold_h[3], rankingPoints[3])
      kpiReport$rCallSetupTimePoorRatio[i]                                    <- rankingCalc(kpiReport$CstPoorRatio[i], 						threshold_l[4], threshold_m[4], threshold_h[4], rankingPoints[4])
      kpiReport$rCallSetupTime10PercentileValue[i]                            <- rankingCalc(kpiReport$CstPercentile10[i], 					threshold_l[5], threshold_m[5], threshold_h[5], rankingPoints[5])
      kpiReport$rPOLQAMOSAverageValue[i]                                      <- rankingCalc(kpiReport$SpeechPolqaAverage[i], 				threshold_l[6], threshold_m[6], threshold_h[6], rankingPoints[6])
      kpiReport$rPOLQAMOSPoorRatio[i]                                         <- rankingCalc(kpiReport$SpeechTestsPoorRatio[i], 				threshold_l[7], threshold_m[7], threshold_h[7], rankingPoints[7])
      kpiReport$rPOLQAMOS90PercentileValue[i]                                 <- rankingCalc(kpiReport$SpeechPolqaPercentile90[i], 			threshold_l[8], threshold_m[8], threshold_h[8], rankingPoints[8])
      kpiReport$rHTTPTransferFDTTDLMDR10PercentileValue[i]                    <- rankingCalc(kpiReport$HttpFdttDlMdrPercentile10[i], 			threshold_l[10], threshold_m[10], threshold_h[10], rankingPoints[10])
      kpiReport$rHTTPTransferFDTTDLMDRAverageValue[i]                         <- rankingCalc(kpiReport$HttpFdttDlMdrAverage[i], 				threshold_l[11], threshold_m[11], threshold_h[11], rankingPoints[11])
      kpiReport$rHTTPTransferFDTTDLMDR90PercentileValue[i]                    <- rankingCalc(kpiReport$HttpFdttDlMdrPercentile90[i], 			threshold_l[12], threshold_m[12], threshold_h[12], rankingPoints[12])
      kpiReport$rHTTPTransferFDTTULMDR10PercentileValue[i]                    <- rankingCalc(kpiReport$HttpFdttUlMdrPercentile10[i], 			threshold_l[13], threshold_m[13], threshold_h[13], rankingPoints[13])
      kpiReport$rHTTPTransferFDTTULMDRAverageValue[i]                         <- rankingCalc(kpiReport$HttpFdttUlMdrAverage[i], 				threshold_l[14], threshold_m[14], threshold_h[14], rankingPoints[14])
      kpiReport$rHTTPTransferFDTTULMDR90PercentileValue[i]                    <- rankingCalc(kpiReport$HttpFdttUlMdrPercentile90[i], 			threshold_l[15], threshold_m[15], threshold_h[15], rankingPoints[15])
      kpiReport$rHTTPBrowsingTestSuccessRatio[i]                              <- rankingCalc(kpiReport$HttpBrowsingCompletedRatio[i], 		threshold_l[16], threshold_m[16], threshold_h[16], rankingPoints[16])
      kpiReport$rHTTPBrowsingTestRoundTripTime[i]                             <- rankingCalc(kpiReport$HttpBrowsingTransferRttAverage[i], 	threshold_l[17], threshold_m[17], threshold_h[17], rankingPoints[17])
      kpiReport$rVideoStreamingTestSuccessRatio[i]                            <- rankingCalc(kpiReport$StreamingCompletedRatio[i], 			threshold_l[18], threshold_m[18], threshold_h[18], rankingPoints[18])
      kpiReport$rVideoStreamingVideoMOS10PercentileValue[i]                   <- rankingCalc(kpiReport$StreamingVmosPercentile10[i], 			threshold_l[19], threshold_m[19], threshold_h[19], rankingPoints[19])
      kpiReport$rVideoStreamingVideoMOSAverageValue[i]                        <- rankingCalc(kpiReport$StreamingVmosAverage[i], 				threshold_l[20], threshold_m[20], threshold_h[20], rankingPoints[20])
      kpiReport$rVideoStreamingTimeto1stPictureAverageValue[i]                <- rankingCalc(kpiReport$StreamingTtfpAverage[i], 				threshold_l[21], threshold_m[21], threshold_h[21], rankingPoints[21])
      kpiReport$rVideoStreamingTimeto1stPicturePoorRatio[i]                   <- rankingCalc(kpiReport$StreamingTtfpPoorRatio[i], 			threshold_l[22], threshold_m[22], threshold_h[22], rankingPoints[22])
      kpiReport$rMobileApplicationTestSuccessRatio[i]                         <- rankingCalc(kpiReport$ApplicationCompletedRatio[i], 			threshold_l[23], threshold_m[23], threshold_h[23], rankingPoints[23])
      kpiReport$rMobileApplicationTestContentTransferTimeAverageValue[i]      <- rankingCalc(kpiReport$ApplicationTransferTimeAverage[i], 	threshold_l[24], threshold_m[24], threshold_h[24], rankingPoints[24])
      }
  }

  #plotKPI()
  plotVoice()
  
  
  
  write.csv(kpiReport, file = "FinalTable.csv")
# Disconnect Database
##########################################
dbDisconnect(con)