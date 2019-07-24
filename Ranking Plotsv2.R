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

plotBarCum <- function(inputTab, inputHead, inputNam, inputColo, inputKP, legFlag, inputLabP,inputLab)
{
  bp <- barplot(inputTab,
              main = inputHead,
              horiz=TRUE, 
              las=1,
              names.arg = inputNam,
              cex.names = 1.5,
              xlab = "",
              ylab = "",
              xaxt='n',
              xlim = c(0,200),
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

##        #######      ######   ######  ########  #### ########  ######## 
##       ##     ##    ##    ## ##    ## ##     ##  ##  ##     ##    ##    
##              ##    ##       ##       ##     ##  ##  ##     ##    ##    
##        #######      ######  ##       ########   ##  ########     ##    
##              ##          ## ##       ##   ##    ##  ##           ##    
##       ##     ##    ##    ## ##    ## ##    ##   ##  ##           ##    
########  #######      ######   ######  ##     ## #### ##           ##    

plotVoiceL3 <- function(df1,fn1)
{
  i <- i + 1
  file <- png(file=fn1,width=1920,height=7700,res=72)
  layout(matrix(c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                nrow=4, 
                ncol=4, 
                byrow = TRUE), 
         widths=c(1,1,1,1,1,1), 
         heights=c(1,1,1,5,1,3))

  # RANKING GRAPH
  inputHeader <- "Ranking Voice - G_Level 3"
  inputNames <- paste( df1$G_Level_1      ,"-"
                      ,df1$G_Level_2      ,"\n"
                      ,df1$G_Level_3      ,"("
                      ,df1$CallAttempts   ,")")
  inputKPIs <- c("Call Setup Success Ratio", 
                 "Drop Call Ratio", 
                 "Call Setup Time - Average Value", 
                 "Call Setup Time - < 15 sec Ratio", 
                 "Call Setup Time - 10 PCTL Value", 
                 "POLQA MOS - Average Value",
                 "POLQA MOS - < 1.6 Ratio",
                 "POLQA MOS - 90 PCTL Value")
  inputColors <- c( "dodgerblue4"
                   ,"firebrick4"
                   ,"darkorange4"
                   ,"darkorange3"
                   ,"darkorange2"
                   ,"deeppink4"
                   ,"deeppink3"
                   ,"deeppink2")
  inputTable <- t(cbind( df1$rCallSetupSuccessRatio
                        ,df1$rDropCallRatio
                        ,df1$rCallSetupTimeAverageValue
                        ,df1$rCallSetupTimePoorRatio
                        ,df1$rCallSetupTime10PercentileValue
                        ,df1$rPOLQAMOSAverageValue
                        ,df1$rPOLQAMOSPoorRatio
                        ,df1$rPOLQAMOS90PercentileValue))
  inputLabPos <- t(cbind(  df1$rCallSetupSuccessRatio
                         + df1$rDropCallRatio
                         + df1$rCallSetupTimeAverageValue
                         + df1$rCallSetupTimePoorRatio
                         + df1$rCallSetupTime10PercentileValue
                         + df1$rPOLQAMOSAverageValue
                         + df1$rPOLQAMOSPoorRatio
                         + df1$rPOLQAMOS90PercentileValue))
  
  inputLabel <-        paste(round((   df1$rCallSetupSuccessRatio 
                                     + df1$rDropCallRatio 
                                     + df1$rCallSetupTimeAverageValue 
                                     + df1$rCallSetupTimePoorRatio 
                                     + df1$rCallSetupTime10PercentileValue 
                                     + df1$rPOLQAMOSAverageValue 
                                     + df1$rPOLQAMOSPoorRatio 
                                     + df1$rPOLQAMOS90PercentileValue), digits=2),
                              "/",
                              df1$tVoice,
                              "=",
                              round( (100*(  df1$rCallSetupSuccessRatio 
                                           + df1$rDropCallRatio 
                                           + df1$rCallSetupTimeAverageValue 
                                           + df1$rCallSetupTimePoorRatio 
                                           + df1$rCallSetupTime10PercentileValue 
                                           + df1$rPOLQAMOSAverageValue 
                                           + df1$rPOLQAMOSPoorRatio 
                                           + df1$rPOLQAMOS90PercentileValue)/df1$tVoice), digits=2), "%")
  par(mar=c(5,25,4,1)+.1)         
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"Yes",inputLabPos,inputLabel)

  # GAP TO MAXIMUM
  inputHeader <- "Ranking Voice - G_Level 3 \nGAP TO MAX"
  inputTable <- t(cbind(  df1$gCallSetupSuccessRatio
                         ,df1$gDropCallRatio
                         ,df1$gCallSetupTimeAverageValue
                         ,df1$gCallSetupTimePoorRatio
                         ,df1$gCallSetupTime10PercentileValue
                         ,df1$gPOLQAMOSAverageValue
                         ,df1$gPOLQAMOSPoorRatio
                         ,df1$gPOLQAMOS90PercentileValue))
  inputLabPos <- t(cbind(    df1$gCallSetupSuccessRatio
                           + df1$gDropCallRatio
                           + df1$gCallSetupTimeAverageValue
                           + df1$gCallSetupTimePoorRatio
                           + df1$gCallSetupTime10PercentileValue
                           + df1$gPOLQAMOSAverageValue
                           + df1$gPOLQAMOSPoorRatio
                           + df1$gPOLQAMOS90PercentileValue))
  
  inputLabel <-        paste(round((    df1$gCallSetupSuccessRatio 
                                       + df1$gDropCallRatio 
                                       + df1$gCallSetupTimeAverageValue 
                                       + df1$gCallSetupTimePoorRatio 
                                       + df1$gCallSetupTime10PercentileValue 
                                       + df1$gPOLQAMOSAverageValue 
                                       + df1$gPOLQAMOSPoorRatio 
                                       + df1$gPOLQAMOS90PercentileValue), digits=2),
                             "/",
                             df1$tVoice,
                             "=",
                             round( (100*(    df1$gCallSetupSuccessRatio 
                                            + df1$gDropCallRatio 
                                            + df1$gCallSetupTimeAverageValue 
                                            + df1$gCallSetupTimePoorRatio 
                                            + df1$gCallSetupTime10PercentileValue 
                                            + df1$gPOLQAMOSAverageValue 
                                            + df1$gPOLQAMOSPoorRatio 
                                            + df1$gPOLQAMOS90PercentileValue)/df1$tVoice), digits=2), "%")
  par(mar=c(5,25,4,1)+.1) 
  plotBarCum(inputTable, inputHeader, inputNames, inputColors, inputKPIs,"No",inputLabPos,inputLabel)
  

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
  
  # CALCULATE NORMALIZATION FACOTRS
  TotalSamplesVoice  <- c( kpiReport$CallAttempts[kpiReport$Rnk == 100]
                          ,kpiReport$CallAttempts[kpiReport$Rnk == 300]
                          ,kpiReport$CallAttempts[kpiReport$Rnk == 200]
                          ,kpiReport$CallAttempts[kpiReport$Rnk == 400])
  TotalSamplesSpeech <- c( kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 100]
                          ,kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 300]
                          ,kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 200]
                          ,kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 400])
  TotalSamplesTrns   <- c( kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 100] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 100]
                          ,kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 300] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 300]
                          ,kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 200] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 200]
                          ,kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 400] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 400])
  TotalSamplesBRWS   <- c( kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 100]
                          ,kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 300]
                          ,kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 200]
                          ,kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 400])
  TotalSamplesVIDEO  <- c( kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 100]
                          ,kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 300]
                          ,kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 200]
                          ,kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 400])
  TotalSamplesSOCIAL <- c( kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 100]
                          ,kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 300]
                          ,kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 200]
                          ,kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 400])
  
  kpiReport$normFactorVoice[  kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "City"] <- kpiReport$CallAttempts[kpiReport$Rnk == 100]
  kpiReport$normFactorSpeech[ kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "City"] <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 100]
  kpiReport$normFactorTrns[   kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "City"] <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 100] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 100]
  kpiReport$normFactorBRWS[   kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "City"] <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 100]
  kpiReport$normFactorVIDEO[  kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "City"] <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 100]
  kpiReport$normFactorSOCIAL[ kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "City"] <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 100]
  kpiReport$normFactorVoice[  kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "Connecting Roads"] <- kpiReport$CallAttempts[kpiReport$Rnk == 300]
  kpiReport$normFactorSpeech[ kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "Connecting Roads"] <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 300]
  kpiReport$normFactorTrns[   kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "Connecting Roads"] <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 300] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 300]
  kpiReport$normFactorBRWS[   kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "Connecting Roads"] <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 300]
  kpiReport$normFactorVIDEO[  kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "Connecting Roads"] <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 300]
  kpiReport$normFactorSOCIAL[ kpiReport$G_Level_1 == "Drive" && kpiReport$G_Level_2 == "Connecting Roads"] <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 300]
  kpiReport$normFactorVoice[  kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "City"] <- kpiReport$CallAttempts[kpiReport$Rnk == 200]
  kpiReport$normFactorSpeech[ kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "City"] <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 200]
  kpiReport$normFactorTrns[   kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "City"] <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 200] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 200]
  kpiReport$normFactorBRWS[   kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "City"] <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 200]
  kpiReport$normFactorVIDEO[  kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "City"] <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 200]
  kpiReport$normFactorSOCIAL[ kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "City"] <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 200]
  kpiReport$normFactorVoice[  kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "Train Route"] <- kpiReport$CallAttempts[kpiReport$Rnk == 400]
  kpiReport$normFactorSpeech[ kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "Train Route"] <- kpiReport$SpeechTestsAttempts[kpiReport$Rnk == 400]
  kpiReport$normFactorTrns[   kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "Train Route"] <- kpiReport$HttpFdfsDlAttemptsCount[kpiReport$Rnk == 400] + kpiReport$HttpFdfsUlAttemptsCount[kpiReport$Rnk == 400]
  kpiReport$normFactorBRWS[   kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "Train Route"] <- kpiReport$HttpBrowsingAttemptsCount[kpiReport$Rnk == 400]
  kpiReport$normFactorVIDEO[  kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "Train Route"] <- kpiReport$StreamingAttemptsCount[kpiReport$Rnk == 400]
  kpiReport$normFactorSOCIAL[ kpiReport$G_Level_1 == "Walk" && kpiReport$G_Level_2 == "Train Route"] <- kpiReport$ApplicationAttemptsCount[kpiReport$Rnk == 400]
  
  for(i in 1:nrow(kpiReport)) 
  {
    if(kpiReport$G_Level_1[i] == "Drive" && kpiReport$G_Level_2[i] == "City") 
    {
      threshold_l <- rankingParams$D_CI_L
      threshold_m <- rankingParams$D_CI_M
      threshold_h <- rankingParams$D_CI_H
      rankingPoints <- rankingParams$D_CI_RP
    }
    if(kpiReport$G_Level_1[i] == "Drive" && kpiReport$G_Level_2[i] == "Connecting Roads") 
    {
      threshold_l <- rankingParams$D_CR_L
      threshold_m <- rankingParams$D_CR_M
      threshold_h <- rankingParams$D_CR_H
      rankingPoints <- rankingParams$D_CR_RP
    }
    if(kpiReport$G_Level_1[i] == "Walk" && kpiReport$G_Level_2[i] == "City") 
    {
      threshold_l <- rankingParams$W_CI_L
      threshold_m <- rankingParams$W_CI_M
      threshold_h <- rankingParams$W_CI_H
      rankingPoints <- rankingParams$W_CI_RP
    }
    if(kpiReport$G_Level_1[i] == "Walk" && kpiReport$G_Level_2[i] == "Train Route") 
    {
      threshold_l <- rankingParams$W_TR_L
      threshold_m <- rankingParams$W_TR_M
      threshold_h <- rankingParams$W_TR_H
      rankingPoints <- rankingParams$W_TR_RP
    }
    
    # SET RANKING PARAMETERS
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

  kpiReport$tVoice <-  (kpiReport$mCallSetupSuccessRatio 
                        + kpiReport$mDropCallRatio 
                        + kpiReport$mCallSetupTimeAverageValue 
                        + kpiReport$mCallSetupTimePoorRatio 
                        + kpiReport$mCallSetupTime10PercentileValue 
                        + kpiReport$mPOLQAMOSAverageValue 
                        + kpiReport$mPOLQAMOSPoorRatio 
                        + kpiReport$mPOLQAMOS90PercentileValue)
  
  # GAP TO MAX
  kpiReport$gCallSetupSuccessRatio           <- (kpiReport$mCallSetupSuccessRatio - kpiReport$rCallSetupSuccessRatio)
  kpiReport$gDropCallRatio                   <- (kpiReport$mDropCallRatio - kpiReport$rDropCallRatio)
  kpiReport$gCallSetupTimeAverageValue       <- (kpiReport$mCallSetupTimeAverageValue - kpiReport$rCallSetupTimeAverageValue)
  kpiReport$gCallSetupTimePoorRatio          <- (kpiReport$mCallSetupTimePoorRatio <- kpiReport$rCallSetupTimePoorRatio)
  kpiReport$gCallSetupTime10PercentileValue  <- (kpiReport$mCallSetupTime10PercentileValue -kpiReport$rCallSetupTime10PercentileValu)
  kpiReport$gPOLQAMOSAverageValue            <- (kpiReport$mPOLQAMOSAverageValue - kpiReport$rPOLQAMOSAverageValue)
  kpiReport$gPOLQAMOSPoorRatio               <- (kpiReport$mPOLQAMOSPoorRatio - kpiReport$rPOLQAMOSPoorRatio)
  kpiReport$gPOLQAMOS90PercentileValue       <- (kpiReport$mPOLQAMOS90PercentileValue - kpiReport$rPOLQAMOS90PercentileValue)
  


  #plotKPI()
  ranL2ALL <- kpiReport[which(kpiReport$Rnk > 1 & kpiReport$Rnk < 500),]
  ranL3ALL <- kpiReport[which(kpiReport$Rnk > 1000 & kpiReport$Rnk < 5001),]
  ranL4DCI <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "City"),]
  ranL4DCR <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Drive" & kpiReport$G_Level_2 == "Connecting Roads"),]
  ranL4WCI <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "City"),]
  ranL4WTR <- kpiReport[which(kpiReport$Rnk >= 10000 & kpiReport$G_Level_1 == "Walk"  & kpiReport$G_Level_2 == "Train Route"),]  
  
  filename <- paste("r","012-L3Voice",".png",sep="")
  plotVoiceL3(ranL3ALL,filename)
  
  
  
  write.csv(kpiReport, file = "FinalTable.csv")
# Disconnect Database
##########################################
dbDisconnect(con)