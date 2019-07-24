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
################################################################################################################################################
plotBarCum <- function(inputTab, inputHead, inputNam, inputColo, inputKP, legFlag, inputLabP,inputLab,mi)
{
  if(is.na(mi)) {mi <- 2}
  bp <- barplot(inputTab,
                main = inputHead,
                cex.main = 1.5,
                horiz=TRUE, 
                las=1,
                names.arg = inputNam,
                cex.names = 1.5,
                xlab = "",
                ylab = "",
                xaxt='n',
                xlim = c(0,mi),
                col=inputColo,
                width = 1)
  if(legFlag == "Yes")
  {
    legend("topright",
           legend = inputKP,
           fill=inputColo,
           cex = 1.5,
           title = "")
  }
  text(x=inputLabP, 
       y=bp, 
       labels=inputLab,
       cex=1.5,
       pos=4, 
       xpd=NA)
}

# CREATE FUNCTION TO GRAB ACTIVE SESSIONIDS FROM CDR 2018
rankingCalc <- function(kpiValue, kpiId , glev1, glev2)
{
  l_thresh <- NA
  m_thresh <- NA
  h_thresh <- NA
  max <- NA
  
  # ifelse(x %in% c("Sat", "Sun"), "Weekend", "Weekday")
  
  if (glev1 == "Drive" && glev2 == "City") {
    l_thresh <- rankingParams$D_CI_L[rankingParams$RNK==kpiId]
    m_thresh <- rankingParams$D_CI_M[rankingParams$RNK==kpiId]
    h_thresh <- rankingParams$D_CI_H[rankingParams$RNK==kpiId]
    max <- rankingParams$D_CI_RP[rankingParams$RNK==kpiId]
    }
  if(glev1 == "Drive" && glev2 == "Connecting Roads"){  
    l_thresh <-  rankingParams$D_CR_L[rankingParams$RNK==kpiId]
    m_thresh <-  rankingParams$D_CR_M[rankingParams$RNK==kpiId]
    h_thresh <-  rankingParams$D_CR_H[rankingParams$RNK==kpiId]
    max <- rankingParams$D_CR_RP[rankingParams$RNK==kpiId]
  }
  if(glev1 == "Walk"  && glev2 == "City"){  
    l_thresh <-  rankingParams$W_CI_L[rankingParams$RNK==kpiId]
    m_thresh <-  rankingParams$W_CI_M[rankingParams$RNK==kpiId]
    h_thresh <-  rankingParams$W_CI_H[rankingParams$RNK==kpiId]
    max <- rankingParams$W_CI_RP[rankingParams$RNK==kpiId]
  }
  if(glev1 == "Walk"  && glev2 == "Train Route"){  
    l_thresh <-  rankingParams$W_TR_L[rankingParams$RNK==kpiId]
    m_thresh <-  rankingParams$W_TR_M[rankingParams$RNK==kpiId]
    h_thresh <-  rankingParams$W_TR_H[rankingParams$RNK==kpiId]
    max <- rankingParams$W_TR_RP[rankingParams$RNK==kpiId]
  }
  
  if(is.na(l_thresh)) {return(0)}
  
  if(l_thresh < m_thresh)
  { 
    if(is.na(kpiValue)) {return(c(0,max))}
    if(kpiValue <= l_thresh) {return(c(0,max))}
    if((kpiValue - l_thresh) * (m_thresh-kpiValue) >= 0) {return(c(0.8*max*(kpiValue - l_thresh)/(m_thresh - l_thresh),max))}
    if((kpiValue - m_thresh) * (h_thresh-kpiValue) >= 0) {return(c(0.8*max + 0.2*max*(kpiValue - m_thresh)/(h_thresh - m_thresh),max))}
    if(kpiValue >= h_thresh) {return(c(max,max))}
    return(998)
  }
  else
  {
    if(is.na(kpiValue)) {return(c(0,max))}
    if(kpiValue >= l_thresh) {return(c(0,max))}
    if((kpiValue - l_thresh) * (m_thresh-kpiValue) >= 0) {return(c(0.8*max*(kpiValue - l_thresh)/(m_thresh - l_thresh),max))}
    if((kpiValue - m_thresh) * (h_thresh-kpiValue) >= 0) {return(c(0.8*max + 0.2*max*(kpiValue - m_thresh)/(h_thresh - m_thresh),max))}
    if(kpiValue <= h_thresh) {return(c(max,max))}
    return(999)
  }
  return(997)
}

# FUNCTION L2/L3 plot OVERALL
l23Plot <- function(fn,glevx,dataProcessing,inputNames)
{
  file <- png(file=fn,width=1920,height=7700,res=72)
  layout(matrix(c(1,1,2,3,4,4,5,6,7,7,8,9,10,10,11,12,13,13,14,15,16,16,17,18,19,19,20,21,22,22,23,24,25,25,26,27), 
                nrow=9, 
                ncol=4, 
                byrow = TRUE), 
         widths=c(1,1,1,1,1,1,1,1,1,1), 
         heights=c(1,1,1,1,1,1,1,1,1,1))
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx)
  maxValue <- (max(dataProcessing$ranCallSetupSuccesRatio) 
               + max(dataProcessing$ranCallDropRatio) 
               + max(dataProcessing$summCST) 
               + max(dataProcessing$summPOLQA))
  inputKPIs <- c("Call Setup Success Ratio", 
                 "Drop Call Ratio", 
                 "Call Setup Time", 
                 "POLQA MOS")
  inputColors <- c( "dodgerblue4"
                    ,"firebrick4"
                    ,"darkorange4"
                    ,"deeppink4")
  inputTable <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio
                          ,dataProcessing$ranCallDropRatio
                          ,dataProcessing$summCST
                          ,dataProcessing$summPOLQA))
  inputLabPos <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio
                           + dataProcessing$ranCallDropRatio
                           + dataProcessing$summCST
                           + dataProcessing$summPOLQA))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCallSetupSuccesRatio 
      + dataProcessing$ranCallDropRatio 
      + dataProcessing$summCST
      + dataProcessing$summPOLQA), 
    digits=2),
    "/",
    dataProcessing$totalVoice,
    "=",
    round( (100*(dataProcessing$ranCallSetupSuccesRatio 
                 + dataProcessing$ranCallDropRatio 
                 + dataProcessing$summCST
                 + dataProcessing$summPOLQA)/dataProcessing$totalVoice), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) 
               + max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) 
               + max(dataProcessing$totalCST - dataProcessing$summCST) 
               + max(dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputTable <- t(cbind(   dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
                           ,dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
                           ,dataProcessing$totalCST - dataProcessing$summCST
                           ,dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputLabPos <- t(cbind(    dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
                             + dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
                             + dataProcessing$totalCST - dataProcessing$summCST
                             + dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
      + dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
      + dataProcessing$totalCST - dataProcessing$summCST
      + dataProcessing$totalPOLQA - dataProcessing$summPOLQA), 
    digits=2),
    "/",
    dataProcessing$totalVoice,
    "=",
    round( (100*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
                 + dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
                 + dataProcessing$totalCST - dataProcessing$summCST
                 + dataProcessing$totalPOLQA - dataProcessing$summPOLQA)/dataProcessing$totalVoice), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) 
                 + max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) 
                 + max(dataProcessing$totalCST - dataProcessing$summCST) 
                 + max(dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)
                         ,dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)
                         ,dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$totalCST - dataProcessing$summCST)
                         ,dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$totalPOLQA - dataProcessing$summPOLQA) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)
                           + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)
                           + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$totalCST - dataProcessing$summCST)
                           + dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$totalPOLQA - dataProcessing$summPOLQA) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)
      + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)
      + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$totalCST - dataProcessing$summCST)
      + dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$totalPOLQA - dataProcessing$summPOLQA)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # CALL SETUP SUCCESS RATIO
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nCall Setup Success Ratio")
  maxValue <- (max(dataProcessing$ranCallSetupSuccesRatio))
  inputKPIs <- c("Call Setup Success Ratio")
  inputColors <- c( "dodgerblue4")
  inputTable <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCallSetupSuccesRatio), 
    digits=2),
    "/",
    dataProcessing$maxCallSetupSuccesRatio,
    "=",
    round( (100*(dataProcessing$ranCallSetupSuccesRatio)/dataProcessing$maxCallSetupSuccesRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)

  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nCall Setup Success Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
inputTable <- t(cbind(   dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
inputLabPos <- t(cbind( dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
inputLabel <-        paste(round( 
                                   ( dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio), 
                                   digits=2),
                                   "/",
                                   dataProcessing$maxCallSetupSuccesRatio,
                                   "=",
                                   round( (100*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)/dataProcessing$maxCallSetupSuccesRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nCall Setup Success Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)), 
    digits=2))
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # DROPPED CALL RATIO
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nDropped Call Ratio")
  maxValue <- (max(dataProcessing$ranCallDropRatio))
  inputKPIs <- c("Dropped Call Ratio")
  inputColors <- c( "firebrick4")
  inputTable <- t(cbind(  dataProcessing$ranCallDropRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranCallDropRatio))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCallDropRatio), 
    digits=2),
    "/",
    dataProcessing$maxCallDropRatio,
    "=",
    round( (100*(dataProcessing$ranCallDropRatio)/dataProcessing$maxCallDropRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
 
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nDropped Call Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputTable <- t(cbind(   dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputLabPos <- t(cbind( dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio), 
    digits=2),
    "/",
    dataProcessing$maxCallDropRatio,
    "=",
    round( (100*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)/dataProcessing$maxCallDropRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nDropped Call Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # Average Call Setup Time
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage Call Setup Time")
  maxValue <- (max(dataProcessing$ranCstAverage))
  inputKPIs <- c("Average Call Setup Time")
  inputColors <- c( "darkorange4")
  inputTable <- t(cbind(  dataProcessing$ranCstAverage))
  inputLabPos <- t(cbind(  dataProcessing$ranCstAverage))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCstAverage), 
    digits=2),
    "/",
    dataProcessing$maxCstAverage,
    "=",
    round( (100*(dataProcessing$ranCstAverage)/dataProcessing$maxCstAverage), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage Call Setup Time","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputTable <- t(cbind(   dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputLabPos <- t(cbind( dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCstAverage - dataProcessing$ranCstAverage), 
    digits=2),
    "/",
    dataProcessing$maxCstAverage,
    "=",
    round( (100*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage)/dataProcessing$maxCstAverage), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage Call Setup Time","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # P10 Call Setup Time
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\n10 PCTL Call Setup Time")
  maxValue <- (max(dataProcessing$ranCstPercentile10))
  inputKPIs <- c("10 PCTL Call Setup Time")
  inputColors <- c( "darkorange3")
  inputTable <- t(cbind(  dataProcessing$ranCstPercentile10))
  inputLabPos <- t(cbind(  dataProcessing$ranCstPercentile10))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCstPercentile10), 
    digits=2),
    "/",
    dataProcessing$maxCstPercentile10,
    "=",
    round( (100*(dataProcessing$ranCstPercentile10)/dataProcessing$maxCstPercentile10), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\n10 PCTL Call Setup Time","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputTable <- t(cbind(   dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputLabPos <- t(cbind( dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10), 
    digits=2),
    "/",
    dataProcessing$maxCstPercentile10,
    "=",
    round( (100*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10)/dataProcessing$maxCstPercentile10), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\n10 PCTL Call Setup Time","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # Poor Ratio Call Setup Time
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor Call Setup Time Ratio")
  maxValue <- (max(dataProcessing$ranCstPoorRatio))
  inputKPIs <- c("Poor Call Setup Time Ratio")
  inputColors <- c( "darkorange2")
  inputTable <- t(cbind(  dataProcessing$ranCstPoorRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranCstPoorRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCstPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxCstPoorRatio,
    "=",
    round( (100*(dataProcessing$ranCstPoorRatio)/dataProcessing$maxCstPoorRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor Call Setup Time Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputTable <- t(cbind(   dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputLabPos <- t(cbind( dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxCstPoorRatio,
    "=",
    round( (100*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio)/dataProcessing$maxCstPoorRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor Call Setup Time Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # Average POLQA MOS
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage POLQA MOS")
  maxValue <- (max(dataProcessing$ranSpeechPolqaAverage))
  inputKPIs <- c("Average POLQA MOS")
  inputColors <- c( "deeppink4")
  inputTable <- t(cbind(  dataProcessing$ranSpeechPolqaAverage))
  inputLabPos <- t(cbind(  dataProcessing$ranSpeechPolqaAverage))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranSpeechPolqaAverage), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaAverage,
    "=",
    round( (100*(dataProcessing$ranSpeechPolqaAverage)/dataProcessing$maxSpeechPolqaAverage), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage POLQA MOS","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputTable <- t(cbind(   dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputLabPos <- t(cbind( dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaAverage,
    "=",
    round( (100*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage)/dataProcessing$maxSpeechPolqaAverage), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage POLQA MOS","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputTable <- t(cbind( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage) ))
  inputLabPos <- t(cbind(  dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # 90 PCTL POLQA MOS
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\n90 PCTL POLQA MOS")
  maxValue <- (max(dataProcessing$ranSpeechPolqaPercentile90))
  inputKPIs <- c("90 PCTL POLQA MOS")
  inputColors <- c( "deeppink3")
  inputTable <- t(cbind(  dataProcessing$ranSpeechPolqaPercentile90))
  inputLabPos <- t(cbind(  dataProcessing$ranSpeechPolqaPercentile90))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranSpeechPolqaPercentile90), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaPercentile90,
    "=",
    round( (100*(dataProcessing$ranSpeechPolqaPercentile90)/dataProcessing$maxSpeechPolqaPercentile90), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\n90 PCTL POLQA MOS","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputTable <- t(cbind(   dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputLabPos <- t(cbind( dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaPercentile90,
    "=",
    round( (100*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90)/dataProcessing$maxSpeechPolqaPercentile90), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\n90 PCTL POLQA MOS","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputTable <- t(cbind( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90) ))
  inputLabPos <- t(cbind(  dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  
  # Poor POLQA MOS Ratio
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor POLQA MOS Ratio")
  maxValue <- (max(dataProcessing$ranSpeechTestsPoorRatio))
  inputKPIs <- c("Poor POLQA MOS Ratio")
  inputColors <- c( "deeppink2")
  inputTable <- t(cbind(  dataProcessing$ranSpeechTestsPoorRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranSpeechTestsPoorRatio))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranSpeechTestsPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxSpeechTestsPoorRatio,
    "=",
    round( (100*(dataProcessing$ranSpeechTestsPoorRatio)/dataProcessing$maxSpeechTestsPoorRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor POLQA MOS Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputTable <- t(cbind(   dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputLabPos <- t(cbind( dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxSpeechTestsPoorRatio,
    "=",
    round( (100*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio)/dataProcessing$maxSpeechTestsPoorRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor POLQA MOS Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputTable <- t(cbind( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  #END
  dev.off()
  layout(1)
}

# FUNCTION L4 plot OVERALL
l4Plot <- function(fn,glevx,dataProcessing,inputNames)
{
  if(nrow(dataProcessing) < 5){
    horRs <- nrow(dataProcessing)*1000
  } else{
    horRs <- nrow(dataProcessing)*500
  }
  file <- png(file=fn,width=1920,height=horRs,res=72)
  layout(matrix(c(1,1,2,3,4,4,5,6,7,7,8,9,10,10,11,12,13,13,14,15,16,16,17,18,19,19,20,21,22,22,23,24,25,25,26,27), 
                nrow=9, 
                ncol=4, 
                byrow = TRUE), 
         widths=c(1,1,1,1,1,1,1,1,1,1), 
         heights=c(1,1,1,1,1,1,1,1,1,1))
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx)
  maxValue <- (1.8*max(dataProcessing$ranCallSetupSuccesRatio) 
               + 1.8*max(dataProcessing$ranCallDropRatio) 
               + 1.8*max(dataProcessing$summCST) 
               + 1.8*max(dataProcessing$summPOLQA))
  inputKPIs <- c("Call Setup Success Ratio", 
                 "Drop Call Ratio", 
                 "Call Setup Time", 
                 "POLQA MOS")
  inputColors <- c( "dodgerblue4"
                    ,"firebrick4"
                    ,"darkorange4"
                    ,"deeppink4")
  inputTable <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio
                          ,dataProcessing$ranCallDropRatio
                          ,dataProcessing$summCST
                          ,dataProcessing$summPOLQA))
  inputLabPos <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio
                           + dataProcessing$ranCallDropRatio
                           + dataProcessing$summCST
                           + dataProcessing$summPOLQA))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCallSetupSuccesRatio 
      + dataProcessing$ranCallDropRatio 
      + dataProcessing$summCST
      + dataProcessing$summPOLQA), 
    digits=2),
    "/",
    dataProcessing$totalVoice,
    "=",
    round( (100*(dataProcessing$ranCallSetupSuccesRatio 
                 + dataProcessing$ranCallDropRatio 
                 + dataProcessing$summCST
                 + dataProcessing$summPOLQA)/dataProcessing$totalVoice), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) 
               + max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) 
               + max(dataProcessing$totalCST - dataProcessing$summCST) 
               + max(dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputTable <- t(cbind(   dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
                           ,dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
                           ,dataProcessing$totalCST - dataProcessing$summCST
                           ,dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputLabPos <- t(cbind(    dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
                             + dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
                             + dataProcessing$totalCST - dataProcessing$summCST
                             + dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
      + dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
      + dataProcessing$totalCST - dataProcessing$summCST
      + dataProcessing$totalPOLQA - dataProcessing$summPOLQA), 
    digits=2),
    "/",
    dataProcessing$totalVoice,
    "=",
    round( (100*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio
                 + dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio
                 + dataProcessing$totalCST - dataProcessing$summCST
                 + dataProcessing$totalPOLQA - dataProcessing$summPOLQA)/dataProcessing$totalVoice), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) 
                 + max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) 
                 + max(dataProcessing$totalCST - dataProcessing$summCST) 
                 + max(dataProcessing$totalPOLQA - dataProcessing$summPOLQA))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)
                         ,dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)
                         ,dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$totalCST - dataProcessing$summCST)
                         ,dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$totalPOLQA - dataProcessing$summPOLQA) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)
                           + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)
                           + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$totalCST - dataProcessing$summCST)
                           + dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$totalPOLQA - dataProcessing$summPOLQA) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)
      + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)
      + dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$totalCST - dataProcessing$summCST)
      + dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$totalPOLQA - dataProcessing$summPOLQA)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # CALL SETUP SUCCESS RATIO
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nCall Setup Success Ratio")
  maxValue <- (max(dataProcessing$ranCallSetupSuccesRatio))
  inputKPIs <- c("Call Setup Success Ratio")
  inputColors <- c( "dodgerblue4")
  inputTable <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranCallSetupSuccesRatio))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCallSetupSuccesRatio), 
    digits=2),
    "/",
    dataProcessing$maxCallSetupSuccesRatio,
    "=",
    round( (100*(dataProcessing$ranCallSetupSuccesRatio)/dataProcessing$maxCallSetupSuccesRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nCall Setup Success Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
  inputTable <- t(cbind(   dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
  inputLabPos <- t(cbind( dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio), 
    digits=2),
    "/",
    dataProcessing$maxCallSetupSuccesRatio,
    "=",
    round( (100*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)/dataProcessing$maxCallSetupSuccesRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nCall Setup Success Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallSetupSuccesRatio - dataProcessing$ranCallSetupSuccesRatio)), 
    digits=2))
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # DROPPED CALL RATIO
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nDropped Call Ratio")
  maxValue <- (max(dataProcessing$ranCallDropRatio))
  inputNames <- paste(  dataProcessing$G_Level_1      ,"-"
                        ,dataProcessing$G_Level_2      ,"\n"
                        ,dataProcessing$G_Level_3      ,"("
                        ,dataProcessing$CallAttempts   ,")")
  inputKPIs <- c("Dropped Call Ratio")
  inputColors <- c( "firebrick4")
  inputTable <- t(cbind(  dataProcessing$ranCallDropRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranCallDropRatio))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCallDropRatio), 
    digits=2),
    "/",
    dataProcessing$maxCallDropRatio,
    "=",
    round( (100*(dataProcessing$ranCallDropRatio)/dataProcessing$maxCallDropRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nDropped Call Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputTable <- t(cbind(   dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputLabPos <- t(cbind( dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio), 
    digits=2),
    "/",
    dataProcessing$maxCallDropRatio,
    "=",
    round( (100*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)/dataProcessing$maxCallDropRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nDropped Call Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCallDropRatio - dataProcessing$ranCallDropRatio)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # Average Call Setup Time
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage Call Setup Time")
  maxValue <- (max(dataProcessing$ranCstAverage))
  inputKPIs <- c("Average Call Setup Time")
  inputColors <- c( "darkorange4")
  inputTable <- t(cbind(  dataProcessing$ranCstAverage))
  inputLabPos <- t(cbind(  dataProcessing$ranCstAverage))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCstAverage), 
    digits=2),
    "/",
    dataProcessing$maxCstAverage,
    "=",
    round( (100*(dataProcessing$ranCstAverage)/dataProcessing$maxCstAverage), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage Call Setup Time","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputTable <- t(cbind(   dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputLabPos <- t(cbind( dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCstAverage - dataProcessing$ranCstAverage), 
    digits=2),
    "/",
    dataProcessing$maxCstAverage,
    "=",
    round( (100*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage)/dataProcessing$maxCstAverage), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage Call Setup Time","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstAverage - dataProcessing$ranCstAverage)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # P10 Call Setup Time
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\n10 PCTL Call Setup Time")
  maxValue <- (max(dataProcessing$ranCstPercentile10))
  inputKPIs <- c("10 PCTL Call Setup Time")
  inputColors <- c( "darkorange3")
  inputTable <- t(cbind(  dataProcessing$ranCstPercentile10))
  inputLabPos <- t(cbind(  dataProcessing$ranCstPercentile10))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCstPercentile10), 
    digits=2),
    "/",
    dataProcessing$maxCstPercentile10,
    "=",
    round( (100*(dataProcessing$ranCstPercentile10)/dataProcessing$maxCstPercentile10), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\n10 PCTL Call Setup Time","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputTable <- t(cbind(   dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputLabPos <- t(cbind( dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10), 
    digits=2),
    "/",
    dataProcessing$maxCstPercentile10,
    "=",
    round( (100*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10)/dataProcessing$maxCstPercentile10), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\n10 PCTL Call Setup Time","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPercentile10 - dataProcessing$ranCstPercentile10)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # Poor Ratio Call Setup Time
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor Call Setup Time Ratio")
  maxValue <- (max(dataProcessing$ranCstPoorRatio))
  inputKPIs <- c("Poor Call Setup Time Ratio")
  inputColors <- c( "darkorange2")
  inputTable <- t(cbind(  dataProcessing$ranCstPoorRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranCstPoorRatio))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranCstPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxCstPoorRatio,
    "=",
    round( (100*(dataProcessing$ranCstPoorRatio)/dataProcessing$maxCstPoorRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor Call Setup Time Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputTable <- t(cbind(   dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputLabPos <- t(cbind( dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxCstPoorRatio,
    "=",
    round( (100*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio)/dataProcessing$maxCstPoorRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor Call Setup Time Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio))
  inputTable <- t(cbind( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$CallAttempts/dataProcessing$ModuleVoiceTests*(dataProcessing$maxCstPoorRatio - dataProcessing$ranCstPoorRatio)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # Average POLQA MOS
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage POLQA MOS")
  maxValue <- (max(dataProcessing$ranSpeechPolqaAverage))
  inputKPIs <- c("Average POLQA MOS")
  inputColors <- c( "deeppink4")
  inputTable <- t(cbind(  dataProcessing$ranSpeechPolqaAverage))
  inputLabPos <- t(cbind(  dataProcessing$ranSpeechPolqaAverage))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranSpeechPolqaAverage), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaAverage,
    "=",
    round( (100*(dataProcessing$ranSpeechPolqaAverage)/dataProcessing$maxSpeechPolqaAverage), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage POLQA MOS","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputTable <- t(cbind(   dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputLabPos <- t(cbind( dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaAverage,
    "=",
    round( (100*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage)/dataProcessing$maxSpeechPolqaAverage), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nAverage POLQA MOS","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage))
  inputTable <- t(cbind( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage) ))
  inputLabPos <- t(cbind(  dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaAverage - dataProcessing$ranSpeechPolqaAverage)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # 90 PCTL POLQA MOS
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\n90 PCTL POLQA MOS")
  maxValue <- (max(dataProcessing$ranSpeechPolqaPercentile90))
  inputKPIs <- c("90 PCTL POLQA MOS")
  inputColors <- c( "deeppink3")
  inputTable <- t(cbind(  dataProcessing$ranSpeechPolqaPercentile90))
  inputLabPos <- t(cbind(  dataProcessing$ranSpeechPolqaPercentile90))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranSpeechPolqaPercentile90), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaPercentile90,
    "=",
    round( (100*(dataProcessing$ranSpeechPolqaPercentile90)/dataProcessing$maxSpeechPolqaPercentile90), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\n90 PCTL POLQA MOS","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputTable <- t(cbind(   dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputLabPos <- t(cbind( dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90), 
    digits=2),
    "/",
    dataProcessing$maxSpeechPolqaPercentile90,
    "=",
    round( (100*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90)/dataProcessing$maxSpeechPolqaPercentile90), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\n90 PCTL POLQA MOS","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90))
  inputTable <- t(cbind( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90) ))
  inputLabPos <- t(cbind(  dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechPolqaPercentile90 - dataProcessing$ranSpeechPolqaPercentile90)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  
  # Poor POLQA MOS Ratio
  # RANKING GRAPH
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor POLQA MOS Ratio")
  maxValue <- (max(dataProcessing$ranSpeechTestsPoorRatio))
  inputKPIs <- c("Poor POLQA MOS Ratio")
  inputColors <- c( "deeppink2")
  inputTable <- t(cbind(  dataProcessing$ranSpeechTestsPoorRatio))
  inputLabPos <- t(cbind(  dataProcessing$ranSpeechTestsPoorRatio))
  
  inputLabel <-        paste(round( 
    ( dataProcessing$ranSpeechTestsPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxSpeechTestsPoorRatio,
    "=",
    round( (100*(dataProcessing$ranSpeechTestsPoorRatio)/dataProcessing$maxSpeechTestsPoorRatio), digits=2), "%")
  par(mar=c(5,25,5,2)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor POLQA MOS Ratio","\nGAP TO MAXIMUM")
  maxValue <- (max(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputTable <- t(cbind(   dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputLabPos <- t(cbind( dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputLabel <-        paste(round( 
    ( dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio), 
    digits=2),
    "/",
    dataProcessing$maxSpeechTestsPoorRatio,
    "=",
    round( (100*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio)/dataProcessing$maxSpeechTestsPoorRatio), digits=2), "%")
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Voice - ",glevx,"\nPoor POLQA MOS Ratio","\nPOINTS LOST\n(normilized with Test Attempts)")
  maxValue <-   (max(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio))
  inputTable <- t(cbind( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio) ))
  inputLabPos <- t(cbind(  dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio) ))
  inputLabel <-        paste(round( 
    ( dataProcessing$SpeechTestsAttempts/dataProcessing$ModulePOLQATests*(dataProcessing$maxSpeechTestsPoorRatio - dataProcessing$ranSpeechTestsPoorRatio)), 
    digits=2))
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  #END
  dev.off()
  layout(1)
}

###################################################################################################################################################
#                                                               FUNCTION L4 plot DATA                                                             #
###################################################################################################################################################
l4PlotData <- function(fn,glevx,dataProcessing,inputNames)
{
  if(nrow(dataProcessing) < 5){
    horRs <- nrow(dataProcessing)*1000
  } else{
    horRs <- nrow(dataProcessing)*500
  }
  file <- png(file=fn,width=1920,height=horRs,res=72)
  layout(matrix(c(1,1,2,3,4,4,5,6,7,7,8,9,10,10,11,12,13,13,14,15,16,16,17,18,19,19,20,21,22,22,23,24,25,25,26,27), 
                nrow=9, 
                ncol=4, 
                byrow = TRUE), 
         widths=c(1,1,1,1,1,1,1,1,1,1), 
         heights=c(1,1,1,1,1,1,1,1,1,1))
  
  inputNames <- paste(inputNames, "(", ( dataProcessing$HttpFdfsDlAttemptsCount + dataProcessing$HttpFdfsUlAttemptsCount + dataProcessing$HttpBrowsingAttemptsCount + dataProcessing$StreamingAttemptsCount + dataProcessing$ApplicationAttemptsCount ), ")")
  
  # NORMALIZATION FACTORS FOR POINTS LOST
  dataProcessing$HTTPTTests  <- 0; dataProcessing$HTTPBTests  <- 0; dataProcessing$VSLTBTests  <- 0; dataProcessing$MAFBBTests  <- 0;
  dataProcessing$HTTPTTests[dataProcessing$ModuleHTTPTTests > 0]  <- (dataProcessing$HttpFdfsDlAttemptsCount + dataProcessing$HttpFdfsUlAttemptsCount) / dataProcessing$ModuleHTTPTTests
  dataProcessing$HTTPBTests[dataProcessing$ModuleHTTPBTests > 0]  <- dataProcessing$HttpBrowsingAttemptsCount / dataProcessing$ModuleHTTPBTests
  dataProcessing$VSLTBTests[dataProcessing$ModuleVSLTBTests > 0]  <- dataProcessing$StreamingAttemptsCount / dataProcessing$ModuleVSLTBTests
  dataProcessing$MAFBBTests[dataProcessing$ModuleMAFBBTests > 0]  <- dataProcessing$ApplicationAttemptsCount / dataProcessing$ModuleMAFBBTests
  
  ####################################################### DATA ALL #######################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("HTTP Transfer - FDFS", 
                 "HTTP Transfer - FDTT-DL", 
                 "HTTP Transfer - FDTT-UL", 
                 "HTTP Browsing",
                 "Video Streaming",
                 "Social Media- Facebook")
  inputColors <- c(  "dodgerblue"
                     ,"darkseagreen3"
                     ,"gold2"
                     ,"green3"
                     ,"mediumpurple2"
                     ,"indianred2")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind( dataProcessing$ranHttpFdfsCompletedRatio
                         ,dataProcessing$summHttpTDL
                         ,dataProcessing$summHttpTUL
                         ,dataProcessing$summHttpB
                         ,dataProcessing$summVSLVS
                         ,dataProcessing$summAPPFB ))
  maxValue <- maxValue <- 2.0*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         dataProcessing$totalData,
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /dataProcessing$totalData)), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind( (dataProcessing$maxHttpFdfsCompletedRatio - dataProcessing$ranHttpFdfsCompletedRatio)
                         ,(dataProcessing$totalHttpTDL - dataProcessing$summHttpTDL)
                         ,(dataProcessing$totalHttpTUL - dataProcessing$summHttpTUL)
                         ,(dataProcessing$totalHttpB - dataProcessing$summHttpB)
                         ,(dataProcessing$totalVSLVS - dataProcessing$summVSLVS)
                         ,(dataProcessing$totalAPPFB - dataProcessing$summAPPFB )))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         dataProcessing$totalData,
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /dataProcessing$totalData)), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(  dataProcessing$HTTPTTests*(dataProcessing$maxHttpFdfsCompletedRatio - dataProcessing$ranHttpFdfsCompletedRatio)
                          ,dataProcessing$HTTPTTests*(dataProcessing$totalHttpTDL - dataProcessing$summHttpTDL)
                          ,dataProcessing$HTTPTTests*(dataProcessing$totalHttpTUL - dataProcessing$summHttpTUL)
                          ,dataProcessing$HTTPBTests*(dataProcessing$totalHttpB - dataProcessing$summHttpB)
                          ,dataProcessing$VSLTBTests*(dataProcessing$totalVSLVS - dataProcessing$summVSLVS)
                          ,dataProcessing$MAFBBTests*(dataProcessing$totalAPPFB - dataProcessing$summAPPFB )))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2) )
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### DATA FDFS #######################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("HTTP Transfer - FDFS")
  inputColors <- c(  "dodgerblue")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind( dataProcessing$ranHttpFdfsCompletedRatio))
  maxValue <- maxValue <- 2.0*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         dataProcessing$maxHttpFdfsCompletedRatio,
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /dataProcessing$maxHttpFdfsCompletedRatio)), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind( (dataProcessing$maxHttpFdfsCompletedRatio - dataProcessing$ranHttpFdfsCompletedRatio)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         dataProcessing$maxHttpFdfsCompletedRatio,
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /dataProcessing$maxHttpFdfsCompletedRatio)), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(  dataProcessing$HTTPTTests*(dataProcessing$maxHttpFdfsCompletedRatio - dataProcessing$ranHttpFdfsCompletedRatio)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2) )
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### FDTT DL #######################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("HTTP Transfer - FDTT-DL P10", 
                 "HTTP Transfer - FDTT-DL AVG", 
                 "HTTP Transfer - FDTT-DL P90")
  inputColors <- c(  "darkseagreen3"
                     ,"darkseagreen2"
                     ,"darkseagreen1")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind(  dataProcessing$ranHttpFdttDlMdrPercentile10
                          ,dataProcessing$ranHttpFdttDlMdrAverage
                          ,dataProcessing$ranHttpFdttDlMdrPercentile90 ))
  maxValue <- maxValue <- 2.0*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxHttpFdttDlMdrPercentile10 + dataProcessing$maxHttpFdttDlMdrAverage + dataProcessing$maxHttpFdttDlMdrPercentile90),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) / (dataProcessing$maxHttpFdttDlMdrPercentile10 + dataProcessing$maxHttpFdttDlMdrAverage + dataProcessing$maxHttpFdttDlMdrPercentile90) )), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind(  ( dataProcessing$maxHttpFdttDlMdrPercentile10 - dataProcessing$ranHttpFdttDlMdrPercentile10)
                          ,( dataProcessing$maxHttpFdttDlMdrAverage      - dataProcessing$ranHttpFdttDlMdrAverage)
                          ,( dataProcessing$maxHttpFdttDlMdrPercentile90 - dataProcessing$ranHttpFdttDlMdrPercentile90 )))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxHttpFdttDlMdrPercentile10 + dataProcessing$maxHttpFdttDlMdrAverage + dataProcessing$maxHttpFdttDlMdrPercentile90),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxHttpFdttDlMdrPercentile10 + dataProcessing$maxHttpFdttDlMdrAverage + dataProcessing$maxHttpFdttDlMdrPercentile90) )), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(   dataProcessing$HTTPTTests*( dataProcessing$maxHttpFdttDlMdrPercentile10 - dataProcessing$ranHttpFdttDlMdrPercentile10)
                           ,dataProcessing$HTTPTTests*( dataProcessing$maxHttpFdttDlMdrAverage      - dataProcessing$ranHttpFdttDlMdrAverage)
                           ,dataProcessing$HTTPTTests*( dataProcessing$maxHttpFdttDlMdrPercentile90 - dataProcessing$ranHttpFdttDlMdrPercentile90 ) ))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),
                         "\n", round( inputTable[1,], digits=2),
                         "+", round( inputTable[2,], digits=2),
                         "+", round( inputTable[3,], digits=2))
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### FDTT UL #######################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("HTTP Transfer - FDTT-UL P10", 
                 "HTTP Transfer - FDTT-UL AVG", 
                 "HTTP Transfer - FDTT-UL P90")
  inputColors <- c(  "gold3"
                     ,"gold2"
                     ,"gold1")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind(  dataProcessing$ranHttpFdttUlMdrPercentile10
                          ,dataProcessing$ranHttpFdttUlMdrAverage
                          ,dataProcessing$ranHttpFdttUlMdrPercentile90 ))
  maxValue <- maxValue <- 2.0*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxHttpFdttUlMdrPercentile10 + dataProcessing$maxHttpFdttUlMdrAverage + dataProcessing$maxHttpFdttUlMdrPercentile90),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) / (dataProcessing$maxHttpFdttUlMdrPercentile10 + dataProcessing$maxHttpFdttUlMdrAverage + dataProcessing$maxHttpFdttUlMdrPercentile90) )), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind(  ( dataProcessing$maxHttpFdttUlMdrPercentile10 - dataProcessing$ranHttpFdttUlMdrPercentile10)
                          ,( dataProcessing$maxHttpFdttUlMdrAverage      - dataProcessing$ranHttpFdttUlMdrAverage)
                          ,( dataProcessing$maxHttpFdttUlMdrPercentile90 - dataProcessing$ranHttpFdttUlMdrPercentile90 )))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxHttpFdttUlMdrPercentile10 + dataProcessing$maxHttpFdttUlMdrAverage + dataProcessing$maxHttpFdttUlMdrPercentile90),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxHttpFdttUlMdrPercentile10 + dataProcessing$maxHttpFdttUlMdrAverage + dataProcessing$maxHttpFdttUlMdrPercentile90) )), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(   dataProcessing$HTTPTTests*( dataProcessing$maxHttpFdttUlMdrPercentile10 - dataProcessing$ranHttpFdttUlMdrPercentile10)
                           ,dataProcessing$HTTPTTests*( dataProcessing$maxHttpFdttUlMdrAverage      - dataProcessing$ranHttpFdttUlMdrAverage)
                           ,dataProcessing$HTTPTTests*( dataProcessing$maxHttpFdttUlMdrPercentile90 - dataProcessing$ranHttpFdttUlMdrPercentile90 ) ))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),
                         "\n", round( inputTable[1,], digits=2),
                         "+", round( inputTable[2,], digits=2),
                         "+", round( inputTable[3,], digits=2))
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### HTTP BROWSING #################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("Browsing \n      Completed Ratio", 
                 "Browsing - RTT AVG")
  inputColors <- c(   "green3"
                      ,"green1")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind(  dataProcessing$ranHttpBrowsingCompletedRatio
                          ,dataProcessing$ranHttpBrowsingTransferRttAverage))
  maxValue <- maxValue <- 2*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxHttpBrowsingCompletedRatio + dataProcessing$maxHttpBrowsingTransferRttAverage),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxHttpBrowsingCompletedRatio + dataProcessing$maxHttpBrowsingTransferRttAverage))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind(  (dataProcessing$maxHttpBrowsingCompletedRatio - dataProcessing$ranHttpBrowsingCompletedRatio)
                          ,(dataProcessing$maxHttpBrowsingTransferRttAverage - dataProcessing$ranHttpBrowsingTransferRttAverage) ))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxHttpBrowsingCompletedRatio + dataProcessing$maxHttpBrowsingTransferRttAverage),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxHttpBrowsingCompletedRatio + dataProcessing$maxHttpBrowsingTransferRttAverage))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(   dataProcessing$HTTPBTests*(dataProcessing$maxHttpBrowsingCompletedRatio - dataProcessing$ranHttpBrowsingCompletedRatio)
                           ,dataProcessing$HTTPBTests*(dataProcessing$maxHttpBrowsingTransferRttAverage - dataProcessing$ranHttpBrowsingTransferRttAverage) ))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),
                         "\n", round( inputTable[1,], digits=2),
                         "+", round( inputTable[2,], digits=2))
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### VS COMPLETED #######################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("Video Stream \n      Completed Ratio")
  inputColors <- c(  "mediumpurple2")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind( dataProcessing$ranStreamingCompletedRatio))
  maxValue <- maxValue <- 2.0*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         dataProcessing$maxStreamingCompletedRatio,
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /dataProcessing$maxStreamingCompletedRatio)), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind( (dataProcessing$maxStreamingCompletedRatio - dataProcessing$ranStreamingCompletedRatio)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         dataProcessing$maxStreamingCompletedRatio,
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /dataProcessing$maxStreamingCompletedRatio)), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(  dataProcessing$HTTPTTests*(dataProcessing$maxStreamingCompletedRatio - dataProcessing$ranStreamingCompletedRatio)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2) )
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### VS vMOS #######################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("Video Stream - 10 PCTL vMOS", 
                 "Video Stream - AVG vMOS")
  inputColors <- c(  "mediumpurple3"
                     ,"mediumpurple1")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind( dataProcessing$ranStreamingVmosPercentile10
                         ,dataProcessing$ranStreamingVmosAverage ))
  maxValue <- maxValue <- 2.0*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxStreamingVmosAverage + dataProcessing$maxStreamingVmosPercentile10),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxStreamingVmosAverage + dataProcessing$maxStreamingVmosPercentile10))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind(  (dataProcessing$maxStreamingVmosPercentile10 - dataProcessing$ranStreamingVmosPercentile10)
                          ,(dataProcessing$maxStreamingVmosAverage - dataProcessing$ranStreamingVmosAverage)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxStreamingVmosAverage + dataProcessing$maxStreamingVmosPercentile10),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxStreamingVmosAverage + dataProcessing$maxStreamingVmosPercentile10))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(   dataProcessing$VSLTBTests*(dataProcessing$maxStreamingVmosPercentile10 - dataProcessing$ranStreamingVmosPercentile10)
                           ,dataProcessing$VSLTBTests*(dataProcessing$maxStreamingVmosAverage - dataProcessing$ranStreamingVmosAverage)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),
                         "\n", round( inputTable[1,], digits=2),
                         "+", round( inputTable[2,], digits=2))
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### VS TTFP #######################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("Video Stream - Poor TTFP Ratio", 
                 "Video Stream - AVG TTFP")
  inputColors <- c(  "mediumpurple3"
                     ,"mediumpurple1")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind( dataProcessing$ranStreamingTtfpPoorRatio
                         ,dataProcessing$ranStreamingTtfpAverage ))
  maxValue <- maxValue <- 2.0*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxStreamingTtfpAverage + dataProcessing$maxStreamingTtfpPoorRatio),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxStreamingTtfpAverage + dataProcessing$maxStreamingTtfpPoorRatio))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind(  (dataProcessing$maxStreamingTtfpPoorRatio - dataProcessing$ranStreamingTtfpPoorRatio)
                          ,(dataProcessing$maxStreamingTtfpAverage - dataProcessing$ranStreamingTtfpAverage)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxStreamingTtfpAverage + dataProcessing$maxStreamingTtfpPoorRatio),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxStreamingTtfpAverage + dataProcessing$maxStreamingTtfpPoorRatio))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(   dataProcessing$VSLTBTests*(dataProcessing$maxStreamingTtfpPoorRatio - dataProcessing$ranStreamingTtfpPoorRatio)
                           ,dataProcessing$VSLTBTests*(dataProcessing$maxStreamingTtfpAverage - dataProcessing$ranStreamingTtfpAverage)))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),
                         "\n", round( inputTable[1,], digits=2),
                         "+", round( inputTable[2,], digits=2))
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  ####################################################### HTTP BROWSING #################################################
  # COMMON LISTS FOR ALL THREE GRAPHS
  inputKPIs <- c("Facebook - Completed Ratio", 
                 "Facebook - AVG Transfer Time")
  inputColors <- c(   "indianred3"
                      ,"indianred1")
  
  # RANKING GRAPH
  inputHeader <- paste("Ranking Data - ",glevx)
  inputTable <- t(cbind(  dataProcessing$ranApplicationCompletedRatio
                          ,dataProcessing$ranApplicationTransferTimeAverage))
  maxValue <- maxValue <- 2*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxApplicationCompletedRatio + dataProcessing$maxApplicationTransferTimeAverage),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxApplicationCompletedRatio + dataProcessing$maxApplicationTransferTimeAverage))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)        
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - GAP TO MAXIMUM
  inputHeader <- paste("Ranking Data - ",glevx, "\nGAP TO MAX")
  inputTable <- t(cbind(  (dataProcessing$maxApplicationCompletedRatio - dataProcessing$ranApplicationCompletedRatio)
                          ,(dataProcessing$maxApplicationTransferTimeAverage - dataProcessing$ranApplicationTransferTimeAverage) ))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),   
                         "/", 
                         (dataProcessing$maxApplicationCompletedRatio + dataProcessing$maxApplicationTransferTimeAverage),
                         "=", 
                         round( (100*(colSums(inputTable,na.rm = TRUE) /(dataProcessing$maxApplicationCompletedRatio + dataProcessing$maxApplicationTransferTimeAverage))), digits=2), 
                         "%")
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,5)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  # RANKING GRAPH - POINTS LOST
  inputHeader <- paste("Ranking Data - ",glevx, "\nPOINTS LOST\n(normalized to Test Attempts)")
  inputTable <- t(cbind(   dataProcessing$HTTPBTests*(dataProcessing$maxApplicationCompletedRatio - dataProcessing$ranApplicationCompletedRatio)
                           ,dataProcessing$HTTPBTests*(dataProcessing$maxApplicationTransferTimeAverage - dataProcessing$ranApplicationTransferTimeAverage) ))
  maxValue <- maxValue <- 1.3*(max(colSums(inputTable,na.rm = TRUE)))
  inputLabel <-  paste(  round( colSums(inputTable,na.rm = TRUE), digits=2),
                         "\n", round( inputTable[1,], digits=2),
                         "+", round( inputTable[2,], digits=2))
  inputLabPos <- colSums(inputTable,na.rm = TRUE)
  
  par(mar=c(5,30,5,10)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel,maxValue)
  
  #END
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
con <- dbConnect(odbc::odbc(),.connection_string = 'driver={SQL Server};server=blndb11;database=DE_PreBM_Voice_FinalCheck;trusted_connection=true')

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
                                     ,CallAttempts
                                     ,CallSetupSuccesRatio
                                     ,CallDropRatio
                                     ,CstAverage
                                     ,CstPercentile10
                                     ,CstPoorRatio
                                     ,SpeechTestsAttempts
                                     ,SpeechPolqaAverage
                                     ,SpeechPolqaPercentile90
                                     ,SpeechTestsPoorRatio
                                     ,HttpFdfsDlAttemptsCount
                                     ,HttpFdfsUlAttemptsCount
                                     ,HttpFdfsCompletedRatio
                                     ,HttpFdttDlMdrPercentile10
                                     ,HttpFdttDlMdrAverage
                                     ,HttpFdttDlMdrPercentile90
                                     ,HttpFdttUlMdrPercentile10
                                     ,HttpFdttUlMdrAverage
                                     ,HttpFdttUlMdrPercentile90
                                     ,HttpBrowsingAttemptsCount
                                     ,HttpBrowsingCompletedRatio
                                     ,HttpBrowsingTransferRttAverage
                                     ,StreamingAttemptsCount
                                     ,StreamingCompletedRatio
                                     ,StreamingVmosPercentile10
                                     ,StreamingVmosAverage
                                     ,StreamingTtfpAverage
                                     ,StreamingTtfpPoorRatio
                                     ,ApplicationAttemptsCount
                                     ,ApplicationCompletedRatio
                                     ,ApplicationTransferTimeAverage
                                     FROM NEW_VDF_KPI_REPORT_OPERATOR_2"))
Encoding(kpiReport$G_Level_4) = "latin1"

# SET MODULE TEST ATTEMPTS
kpiReport$ModuleVoiceTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City" ]              <- kpiReport$CallAttempts[kpiReport$Rnk == 100]
kpiReport$ModuleVoiceTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads" ]  <- kpiReport$CallAttempts[kpiReport$Rnk == 300]
kpiReport$ModuleVoiceTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City" ]              <- kpiReport$CallAttempts[kpiReport$Rnk == 200]
kpiReport$ModuleVoiceTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route" ]       <- kpiReport$CallAttempts[kpiReport$Rnk == 400]
kpiReport$ModulePOLQATests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City" ]              <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 100]
kpiReport$ModulePOLQATests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads" ]  <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 300]
kpiReport$ModulePOLQATests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City" ]              <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 200]
kpiReport$ModulePOLQATests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route" ]       <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 400]
kpiReport$ModuleHTTPTTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City" ]              <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 100] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 100]
kpiReport$ModuleHTTPTTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads" ]  <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 300] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 300]
kpiReport$ModuleHTTPTTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City" ]              <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 200] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 200]
kpiReport$ModuleHTTPTTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route" ]       <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 400] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 400]
kpiReport$ModuleHTTPBTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City" ]              <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 100]
kpiReport$ModuleHTTPBTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads" ]  <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 300]
kpiReport$ModuleHTTPBTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City" ]              <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 200]
kpiReport$ModuleHTTPBTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route" ]       <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 400]
kpiReport$ModuleVSLTBTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City" ]              <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 100]
kpiReport$ModuleVSLTBTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads" ]  <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 300]
kpiReport$ModuleVSLTBTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City" ]              <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 200]
kpiReport$ModuleVSLTBTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route" ]       <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 400]
kpiReport$ModuleMAFBBTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City" ]              <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 100]
kpiReport$ModuleMAFBBTests[kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads" ]  <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 300]
kpiReport$ModuleMAFBBTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City" ]              <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 200]
kpiReport$ModuleMAFBBTests[kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route" ]       <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 400]

for(i in 1:nrow(kpiReport)) 
{
  kpiReport$ranCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport$CallSetupSuccesRatio[i]          , 1,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranCallDropRatio[i]                     <- rankingCalc(kpiReport$CallDropRatio[i]                 , 2,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranCstAverage[i]                        <- rankingCalc(kpiReport$CstAverage[i]                    , 3,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranCstPoorRatio[i]                      <- rankingCalc(kpiReport$CstPoorRatio[i]                  , 4,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranCstPercentile10[i]                   <- rankingCalc(kpiReport$CstPercentile10[i]               , 5,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranSpeechPolqaAverage[i]                <- rankingCalc(kpiReport$SpeechPolqaAverage[i]            , 6,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport$SpeechTestsPoorRatio[i]          , 7,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport$SpeechPolqaPercentile90[i]       , 8,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport$HttpFdfsCompletedRatio[i]        , 9,  kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport$HttpFdttDlMdrAverage[i]          , 11, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport$HttpFdttUlMdrAverage[i]          , 14, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport$HttpBrowsingCompletedRatio[i]    , 16, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport$HttpBrowsingTransferRttAverage[i], 17, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranStreamingCompletedRatio[i]           <- rankingCalc(kpiReport$StreamingCompletedRatio[i]       , 18, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport$StreamingVmosPercentile10[i]     , 19, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranStreamingVmosAverage[i]              <- rankingCalc(kpiReport$StreamingVmosAverage[i]          , 20, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranStreamingTtfpAverage[i]              <- rankingCalc(kpiReport$StreamingTtfpAverage[i]          , 21, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport$StreamingTtfpPoorRatio[i]        , 22, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranApplicationCompletedRatio[i]         <- rankingCalc(kpiReport$ApplicationCompletedRatio[i]     , 23, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]   
  kpiReport$ranApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport$ApplicationTransferTimeAverage[i], 24, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[1]
  kpiReport$maxCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport$CallSetupSuccesRatio[i]          , 1, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxCallDropRatio[i]                     <- rankingCalc(kpiReport$CallDropRatio[i]                 , 2, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxCstAverage[i]                        <- rankingCalc(kpiReport$CstAverage[i]                    , 3, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxCstPoorRatio[i]                      <- rankingCalc(kpiReport$CstPoorRatio[i]                  , 4, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxCstPercentile10[i]                   <- rankingCalc(kpiReport$CstPercentile10[i]               , 5, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxSpeechPolqaAverage[i]                <- rankingCalc(kpiReport$SpeechPolqaAverage[i]            , 6, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport$SpeechTestsPoorRatio[i]          , 7, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport$SpeechPolqaPercentile90[i]       , 8, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport$HttpFdfsCompletedRatio[i]        , 9, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport$HttpFdttDlMdrAverage[i]          , 11, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport$HttpFdttUlMdrAverage[i]          , 14, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport$HttpBrowsingCompletedRatio[i]    , 16, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport$HttpBrowsingTransferRttAverage[i], 17, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxStreamingCompletedRatio[i]           <- rankingCalc(kpiReport$StreamingCompletedRatio[i]       , 18, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport$StreamingVmosPercentile10[i]     , 19, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxStreamingVmosAverage[i]              <- rankingCalc(kpiReport$StreamingVmosAverage[i]          , 20, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxStreamingTtfpAverage[i]              <- rankingCalc(kpiReport$StreamingTtfpAverage[i]          , 21, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport$StreamingTtfpPoorRatio[i]        , 22, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxApplicationCompletedRatio[i]         <- rankingCalc(kpiReport$ApplicationCompletedRatio[i]     , 23, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2]   
  kpiReport$maxApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport$ApplicationTransferTimeAverage[i], 24, kpiReport$G_Level_1[i], kpiReport$G_Level_2[i])[2] 
}
# ADD AGGREGATION FOR PLOTTING
kpiReport$summCST   <- (kpiReport$ranCstAverage+kpiReport$ranCstPoorRatio+kpiReport$ranCstPercentile10)
kpiReport$summPOLQA <- (kpiReport$ranSpeechPolqaAverage+kpiReport$ranSpeechTestsPoorRatio+kpiReport$ranSpeechPolqaPercentile90)
kpiReport$summVoice <- (kpiReport$ranCallSetupSuccesRatio+kpiReport$ranCallDropRatio+kpiReport$ranCstAverage+kpiReport$ranCstPoorRatio+kpiReport$ranCstPercentile10+kpiReport$ranSpeechPolqaAverage+kpiReport$ranSpeechTestsPoorRatio+kpiReport$ranSpeechPolqaPercentile90)
kpiReport$summHttpTDL  <- (kpiReport$ranHttpFdttDlMdrPercentile10+kpiReport$ranHttpFdttDlMdrAverage+kpiReport$ranHttpFdttDlMdrPercentile90)
kpiReport$summHttpTUL  <- (kpiReport$ranHttpFdttUlMdrPercentile10+kpiReport$ranHttpFdttUlMdrAverage+kpiReport$ranHttpFdttUlMdrPercentile90)
kpiReport$summHttpT  <- (kpiReport$ranHttpFdfsCompletedRatio+kpiReport$ranHttpFdttDlMdrPercentile10+kpiReport$ranHttpFdttDlMdrAverage+kpiReport$ranHttpFdttDlMdrPercentile90+kpiReport$ranHttpFdttUlMdrPercentile10+kpiReport$ranHttpFdttUlMdrAverage+kpiReport$ranHttpFdttUlMdrPercentile90)
kpiReport$summHttpB  <- (kpiReport$ranHttpBrowsingCompletedRatio+kpiReport$ranHttpBrowsingTransferRttAverage)
kpiReport$summVSLVS  <- (kpiReport$ranStreamingCompletedRatio+kpiReport$ranStreamingVmosPercentile10+kpiReport$ranStreamingVmosAverage+kpiReport$ranStreamingTtfpAverage+kpiReport$ranStreamingTtfpPoorRatio)
kpiReport$summAPPFB  <- (kpiReport$ranApplicationCompletedRatio+kpiReport$ranApplicationTransferTimeAverage)
kpiReport$summData  <- (kpiReport$ranHttpFdfsCompletedRatio+kpiReport$ranHttpFdttDlMdrPercentile10+kpiReport$ranHttpFdttDlMdrAverage+kpiReport$ranHttpFdttDlMdrPercentile90+kpiReport$ranHttpFdttUlMdrPercentile10+kpiReport$ranHttpFdttUlMdrAverage+kpiReport$ranHttpFdttUlMdrPercentile90+kpiReport$ranHttpBrowsingCompletedRatio+kpiReport$ranHttpBrowsingTransferRttAverage+kpiReport$ranStreamingCompletedRatio+kpiReport$ranStreamingVmosPercentile10+kpiReport$ranStreamingVmosAverage+kpiReport$ranStreamingTtfpAverage+kpiReport$ranStreamingTtfpPoorRatio+kpiReport$ranApplicationCompletedRatio+kpiReport$ranApplicationTransferTimeAverage)
kpiReport$totalCST   <- (kpiReport$maxCstAverage+kpiReport$maxCstPoorRatio+kpiReport$maxCstPercentile10)
kpiReport$totalPOLQA <- (kpiReport$maxSpeechPolqaAverage+kpiReport$maxSpeechTestsPoorRatio+kpiReport$maxSpeechPolqaPercentile90)
kpiReport$totalVoice <- (kpiReport$maxCallSetupSuccesRatio+kpiReport$maxCallDropRatio+kpiReport$maxCstAverage+kpiReport$maxCstPoorRatio+kpiReport$maxCstPercentile10+kpiReport$maxSpeechPolqaAverage+kpiReport$maxSpeechTestsPoorRatio+kpiReport$maxSpeechPolqaPercentile90)
kpiReport$totalHttpTDL  <- (kpiReport$maxHttpFdttDlMdrPercentile10+kpiReport$maxHttpFdttDlMdrAverage+kpiReport$maxHttpFdttDlMdrPercentile90)
kpiReport$totalHttpTUL  <- (kpiReport$maxHttpFdttUlMdrPercentile10+kpiReport$maxHttpFdttUlMdrAverage+kpiReport$maxHttpFdttUlMdrPercentile90)
kpiReport$totalHttpT  <- (kpiReport$maxHttpFdfsCompletedRatio+kpiReport$maxHttpFdttDlMdrPercentile10+kpiReport$maxHttpFdttDlMdrAverage+kpiReport$maxHttpFdttDlMdrPercentile90+kpiReport$maxHttpFdttUlMdrPercentile10+kpiReport$maxHttpFdttUlMdrAverage+kpiReport$maxHttpFdttUlMdrPercentile90)
kpiReport$totalHttpB  <- (kpiReport$maxHttpBrowsingCompletedRatio+kpiReport$maxHttpBrowsingTransferRttAverage)
kpiReport$totalVSLVS  <- (kpiReport$maxStreamingCompletedRatio+kpiReport$maxStreamingVmosPercentile10+kpiReport$maxStreamingVmosAverage+kpiReport$maxStreamingTtfpAverage+kpiReport$maxStreamingTtfpPoorRatio)
kpiReport$totalAPPFB  <- (kpiReport$maxApplicationCompletedRatio+kpiReport$maxApplicationTransferTimeAverage)
kpiReport$totalData  <- (kpiReport$maxHttpFdfsCompletedRatio+kpiReport$maxHttpFdttDlMdrPercentile10+kpiReport$maxHttpFdttDlMdrAverage+kpiReport$maxHttpFdttDlMdrPercentile90+kpiReport$maxHttpFdttUlMdrPercentile10+kpiReport$maxHttpFdttUlMdrAverage+kpiReport$maxHttpFdttUlMdrPercentile90+kpiReport$maxHttpBrowsingCompletedRatio+kpiReport$maxHttpBrowsingTransferRttAverage+kpiReport$maxStreamingCompletedRatio+kpiReport$maxStreamingVmosPercentile10+kpiReport$maxStreamingVmosAverage+kpiReport$maxStreamingTtfpAverage+kpiReport$maxStreamingTtfpPoorRatio+kpiReport$maxApplicationCompletedRatio+kpiReport$maxApplicationTransferTimeAverage)
kpiReport$summRanking <- kpiReport$summData + kpiReport$summVoice
kpiReport$totalRanking <- kpiReport$totalData + kpiReport$totalVoice

# SEGRAGATE TRAFFIC
ranL2ALL <- kpiReport[which(kpiReport$Rnk > 1 & kpiReport$Rnk < 500),]
ranL3ALL <- kpiReport[which(kpiReport$Rnk > 1000 & kpiReport$Rnk < 5001),]
ranL4DCI <- kpiReport[which(kpiReport$Rnk > 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City"),]
ranL4DCR <- kpiReport[which(kpiReport$Rnk > 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads"),]
ranL4WCI <- kpiReport[which(kpiReport$Rnk > 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City"),]
ranL4WTR <- kpiReport[which(kpiReport$Rnk > 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route"),]  

inputN <- paste(     ranL2ALL$G_Level_1      ,"-"
                    ,ranL2ALL$G_Level_2      ,"\n"
                    ,ranL2ALL$G_Level_3
                    ,"(" ,ranL2ALL$CallAttempts   ,")")
l23Plot("L2-Voice-VDF.png","G_Level_2",ranL2ALL,inputN)
inputN <- paste(    ranL3ALL$G_Level_1      ,"-"
                    ,ranL3ALL$G_Level_2      ,"\n"
                    ,ranL3ALL$G_Level_3
                    ," (" ,ranL3ALL$CallAttempts   ,")")
l23Plot("L3-Voice-VDF.png","G_Level_3",ranL3ALL,inputN)
inputN <- paste(    ranL4DCI$G_Level_1      ,"-"
                   ,ranL4DCI$G_Level_2      ,"\n"
                   ,ranL4DCI$G_Level_3      ,"-"
                   ,ranL4DCI$G_Level_4      
                   ,"(" ,ranL4DCI$CallAttempts   ,")")
l4Plot("L4-Voice-Drive-City-VDF.png","G_Level_4 (Drive City)",ranL4DCI,inputN)

inputN <- paste(     ranL4DCR$G_Level_1      ,"-"
                    ,ranL4DCR$G_Level_2      ,"\n"
                    ,ranL4DCR$G_Level_4      
                    ,"(" ,ranL4DCR$CallAttempts   ,")")
l4Plot("L4-Voice-Drive-Connecting-VDF.png","G_Level_4 (Drive City)",ranL4DCR,inputN)

inputN <- paste(     ranL4WCI$G_Level_1      ,"-"
                    ,ranL4WCI$G_Level_2      ,"\n"
                    ,ranL4WCI$G_Level_3      ,"-"
                    ,ranL4WCI$G_Level_4      
                    ,"(" ,ranL4WCI$CallAttempts   ,")")
l4Plot("L4-Voice-Walk-City-VDF.png","G_Level_4 (Walk City)",ranL4WCI,inputN)
inputN <- paste(      ranL4WTR$G_Level_1      ,"-"
                      ,ranL4WTR$G_Level_2      ,"\n"
                      ,ranL4WTR$G_Level_4, "\n" ,ranL4WTR$Fleet,"-" , ranL4WTR$WagonNumber 
                     ,"(" ,ranL4WTR$CallAttempts   ,")")
l4Plot("L4-Voice-Walk-Train-VDF.png","G_Level_4 (Walk Train)",ranL4WTR,inputN)


inputN <- paste(     ranL2ALL$G_Level_1      ,"-"
                     ,ranL2ALL$G_Level_2      ,"\n"
                     ,ranL2ALL$G_Level_3)
l4PlotData("L2-Data-VDF.png","G_Level_2",ranL2ALL,inputN)
inputN <- paste(    ranL3ALL$G_Level_1      ,"-"
                    ,ranL3ALL$G_Level_2      ,"\n"
                    ,ranL3ALL$G_Level_3)
l4PlotData("L3-Data-VDF.png","G_Level_3",ranL3ALL,inputN)
inputN <- paste(     ranL4DCI$G_Level_1      ,"-"
                     ,ranL4DCI$G_Level_2      ,"\n"
                     ,ranL4DCI$G_Level_3      ,"-"
                     ,ranL4DCI$G_Level_4      )
l4PlotData("L4-Data-Drive-City-VDF.png","G_Level_4 (Drive City)",ranL4DCI,inputN)

inputN <- paste(      ranL4DCR$G_Level_1      ,"-"
                     ,ranL4DCR$G_Level_2      ,"\n"
                     ,ranL4DCR$G_Level_4      )
l4PlotData("L4-Data-Drive-Conn-VDF.png","G_Level_4 (Drive Connecting Routes)",ranL4DCR,inputN)
inputN <- paste(     ranL4WCI$G_Level_1      ,"-"
                     ,ranL4WCI$G_Level_2      ,"\n"
                     ,ranL4WCI$G_Level_3      ,"-"
                     ,ranL4WCI$G_Level_4      )
l4PlotData("L4-Data-Walk-City-VDF.png","G_Level_4 (Walk City)",ranL4WCI,inputN)
inputN <- paste(      ranL4WTR$G_Level_1      ,"-"
                      ,ranL4WTR$G_Level_2      ,"\n"
                      ,ranL4WTR$G_Level_4, "\n" ,ranL4WTR$Fleet,"-" , ranL4WTR$WagonNumber)
l4PlotData("L4-Data-Walk-Train-VDF.png","G_Level_4 (Walk Train)",ranL4WTR,inputN)

write.csv(kpiReport, file = "VDF-Q3-FinalTable.csv")
# Disconnect Database
##########################################

rm(kpiReport)
dbDisconnect(con)