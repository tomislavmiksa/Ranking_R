library(DBI)
library(odbc)
library(lubridate)
library(dplyr)
library(tidyr)
library(plotrix)
library(gridExtra)
library(XLConnect)

######## ##     ## ##    ##  ######  ######## ####  #######  ##    ##  ######  
##       ##     ## ###   ## ##    ##    ##     ##  ##     ## ###   ## ##    ## 
##       ##     ## ####  ## ##          ##     ##  ##     ## ####  ## ##       
######   ##     ## ## ## ## ##          ##     ##  ##     ## ## ## ##  ######  
##       ##     ## ##  #### ##          ##     ##  ##     ## ##  ####       ## 
##       ##     ## ##   ### ##    ##    ##     ##  ##     ## ##   ### ##    ## 
##        #######  ##    ##  ######     ##    ####  #######  ##    ##  ######  

###################################################################################################################################################
#                                                               FUNCTION calculates RANKING based on KPI                                          #
###################################################################################################################################################
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

###################################################################################################################################################
#                                                               FUNCTION specific WEIGHT FACTORS                                                  #
###################################################################################################################################################
factorsCalc <- function(inputcount, moduleCount)
{
  if(is.na(moduleCount)) { return(1.0) }
  if ( moduleCount <= 0 ) { return(1.0) }
  if ( moduleCount < inputcount ) { return(1.0) }
  return( inputcount/moduleCount )
}

##     ##    ###    #### ##    ##    ########  ########   #######   ######   ########     ###    ##     ## 
###   ###   ## ##    ##  ###   ##    ##     ## ##     ## ##     ## ##    ##  ##     ##   ## ##   ###   ### 
#### ####  ##   ##   ##  ####  ##    ##     ## ##     ## ##     ## ##        ##     ##  ##   ##  #### #### 
## ### ## ##     ##  ##  ## ## ##    ########  ########  ##     ## ##   #### ########  ##     ## ## ### ## 
##     ## #########  ##  ##  ####    ##        ##   ##   ##     ## ##    ##  ##   ##   ######### ##     ## 
##     ## ##     ##  ##  ##   ###    ##        ##    ##  ##     ## ##    ##  ##    ##  ##     ## ##     ## 
##     ## ##     ## #### ##    ##    ##        ##     ##  #######   ######   ##     ## ##     ## ##     ## 

# USER INPUT
database     <- "DE_PreBM_Voice_03"
operatorView <- "NEW_VDF_KPI_REPORT_OPERATOR_1"
resultTable  <- "NEW_KPI_REPORT_2019_OPERATOR_1"

setwd("C:/Users/tmiksa/Desktop/Ranking_R")

# LOAD RANKING PARAMETERS FROM INPUT FILE
rankingParams = read.csv("rankingParams.csv")  # read csv file 

# Connect BLNDB11
# HERE YOU HAVE TO SET CORRECT DB NAME
##########################################
conString <- paste('driver={SQL Server};server=blndb11;database=',database,';trusted_connection=true',sep="")
con <- dbConnect(odbc::odbc(),.connection_string = conString)

# EXTRACT KPI REPORT FOR OPERATOR
kpiReport1 <-    dbGetQuery(con,paste("SELECT * FROM",operatorView))
Encoding(kpiReport1$G_Level_4) = "latin1"

# CALL AND TEST ATTEMPTS SET
##########################################
kpiReport1$MODULETESTCOUNT <- "MODULE TEST COUNT >>"

# SET MODULE TEST ATTEMPTS
kpiReport1$ModuleVoiceTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$CallAttempts[kpiReport1$Rnk == 100]
kpiReport1$ModuleVoiceTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "Connecting Roads" ]  <- kpiReport1$CallAttempts[kpiReport1$Rnk == 300]
kpiReport1$ModuleVoiceTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$CallAttempts[kpiReport1$Rnk == 200]
kpiReport1$ModuleVoiceTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "Train Route" ]       <- kpiReport1$CallAttempts[kpiReport1$Rnk == 400]
kpiReport1$ModulePOLQATests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$SpeechTestsAttempts[kpiReport1$Rnk == 100]
kpiReport1$ModulePOLQATests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "Connecting Roads" ]  <- kpiReport1$SpeechTestsAttempts[kpiReport1$Rnk == 300]
kpiReport1$ModulePOLQATests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$SpeechTestsAttempts[kpiReport1$Rnk == 200]
kpiReport1$ModulePOLQATests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "Train Route" ]       <- kpiReport1$SpeechTestsAttempts[kpiReport1$Rnk == 400]
kpiReport1$ModuleHTTPTTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$HttpFdfsDlAttemptsCount[kpiReport1$Rnk == 100] + kpiReport1$HttpFdfsUlAttemptsCount[kpiReport1$Rnk == 100]
kpiReport1$ModuleHTTPTTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "Connecting Roads" ]  <- kpiReport1$HttpFdfsDlAttemptsCount[kpiReport1$Rnk == 300] + kpiReport1$HttpFdfsUlAttemptsCount[kpiReport1$Rnk == 300]
kpiReport1$ModuleHTTPTTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$HttpFdfsDlAttemptsCount[kpiReport1$Rnk == 200] + kpiReport1$HttpFdfsUlAttemptsCount[kpiReport1$Rnk == 200]
kpiReport1$ModuleHTTPTTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "Train Route" ]       <- kpiReport1$HttpFdfsDlAttemptsCount[kpiReport1$Rnk == 400] + kpiReport1$HttpFdfsUlAttemptsCount[kpiReport1$Rnk == 400]
kpiReport1$ModuleHTTPBTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$HttpBrowsingAttemptsCount[kpiReport1$Rnk == 100]
kpiReport1$ModuleHTTPBTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "Connecting Roads" ]  <- kpiReport1$HttpBrowsingAttemptsCount[kpiReport1$Rnk == 300]
kpiReport1$ModuleHTTPBTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$HttpBrowsingAttemptsCount[kpiReport1$Rnk == 200]
kpiReport1$ModuleHTTPBTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "Train Route" ]       <- kpiReport1$HttpBrowsingAttemptsCount[kpiReport1$Rnk == 400]
kpiReport1$ModuleVSLTBTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$StreamingAttemptsCount[kpiReport1$Rnk == 100]
kpiReport1$ModuleVSLTBTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "Connecting Roads" ]  <- kpiReport1$StreamingAttemptsCount[kpiReport1$Rnk == 300]
kpiReport1$ModuleVSLTBTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$StreamingAttemptsCount[kpiReport1$Rnk == 200]
kpiReport1$ModuleVSLTBTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "Train Route" ]       <- kpiReport1$StreamingAttemptsCount[kpiReport1$Rnk == 400]
kpiReport1$ModuleMAFBBTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$ApplicationAttemptsCount[kpiReport1$Rnk == 100]
kpiReport1$ModuleMAFBBTests[kpiReport1$G_Level_1 == "Drive" & kpiReport1$G_Level_2 == "Connecting Roads" ]  <- kpiReport1$ApplicationAttemptsCount[kpiReport1$Rnk == 300]
kpiReport1$ModuleMAFBBTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "City" ]              <- kpiReport1$ApplicationAttemptsCount[kpiReport1$Rnk == 200]
kpiReport1$ModuleMAFBBTests[kpiReport1$G_Level_1 == "Walk"  & kpiReport1$G_Level_2 == "Train Route" ]       <- kpiReport1$ApplicationAttemptsCount[kpiReport1$Rnk == 400]

# RANKING KPIs
##########################################
kpiReport1$RANKINGKPI <- "RANKING EACH KPI >>"

for(i in 1:nrow(kpiReport1)) 
{
  kpiReport1$ranCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport1$CallSetupSuccesRatio[i]          , 1,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranCallDropRatio[i]                     <- rankingCalc(kpiReport1$CallDropRatio[i]                 , 2,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranCstAverage[i]                        <- rankingCalc(kpiReport1$CstAverage[i]                    , 3,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranCstPoorRatio[i]                      <- rankingCalc(kpiReport1$CstPoorRatio[i]                  , 4,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranCstPercentile10[i]                   <- rankingCalc(kpiReport1$CstPercentile10[i]               , 5,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranSpeechPolqaAverage[i]                <- rankingCalc(kpiReport1$SpeechPolqaAverage[i]            , 6,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport1$SpeechTestsPoorRatio[i]          , 7,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport1$SpeechPolqaPercentile90[i]       , 8,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport1$HttpFdfsCompletedRatio[i]        , 9,  kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport1$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport1$HttpFdttDlMdrAverage[i]          , 11, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport1$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport1$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport1$HttpFdttUlMdrAverage[i]          , 14, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport1$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport1$HttpBrowsingCompletedRatio[i]    , 16, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport1$HttpBrowsingTransferRttAverage[i], 17, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranStreamingCompletedRatio[i]           <- rankingCalc(kpiReport1$StreamingCompletedRatio[i]       , 18, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport1$StreamingVmosPercentile10[i]     , 19, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranStreamingVmosAverage[i]              <- rankingCalc(kpiReport1$StreamingVmosAverage[i]          , 20, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranStreamingTtfpAverage[i]              <- rankingCalc(kpiReport1$StreamingTtfpAverage[i]          , 21, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport1$StreamingTtfpPoorRatio[i]        , 22, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranApplicationCompletedRatio[i]         <- rankingCalc(kpiReport1$ApplicationCompletedRatio[i]     , 23, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]   
  kpiReport1$ranApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport1$ApplicationTransferTimeAverage[i], 24, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[1]
  kpiReport1$maxCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport1$CallSetupSuccesRatio[i]          , 1, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxCallDropRatio[i]                     <- rankingCalc(kpiReport1$CallDropRatio[i]                 , 2, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxCstAverage[i]                        <- rankingCalc(kpiReport1$CstAverage[i]                    , 3, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxCstPoorRatio[i]                      <- rankingCalc(kpiReport1$CstPoorRatio[i]                  , 4, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxCstPercentile10[i]                   <- rankingCalc(kpiReport1$CstPercentile10[i]               , 5, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxSpeechPolqaAverage[i]                <- rankingCalc(kpiReport1$SpeechPolqaAverage[i]            , 6, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport1$SpeechTestsPoorRatio[i]          , 7, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport1$SpeechPolqaPercentile90[i]       , 8, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport1$HttpFdfsCompletedRatio[i]        , 9, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport1$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport1$HttpFdttDlMdrAverage[i]          , 11, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport1$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport1$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport1$HttpFdttUlMdrAverage[i]          , 14, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport1$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport1$HttpBrowsingCompletedRatio[i]    , 16, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport1$HttpBrowsingTransferRttAverage[i], 17, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxStreamingCompletedRatio[i]           <- rankingCalc(kpiReport1$StreamingCompletedRatio[i]       , 18, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport1$StreamingVmosPercentile10[i]     , 19, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxStreamingVmosAverage[i]              <- rankingCalc(kpiReport1$StreamingVmosAverage[i]          , 20, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxStreamingTtfpAverage[i]              <- rankingCalc(kpiReport1$StreamingTtfpAverage[i]          , 21, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport1$StreamingTtfpPoorRatio[i]        , 22, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxApplicationCompletedRatio[i]         <- rankingCalc(kpiReport1$ApplicationCompletedRatio[i]     , 23, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2]   
  kpiReport1$maxApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport1$ApplicationTransferTimeAverage[i], 24, kpiReport1$G_Level_1[i], kpiReport1$G_Level_2[i])[2] 
}


kpiReport1$lostCallSetupSuccesRatio             <-  1.0 * ( kpiReport1$maxCallSetupSuccesRatio           - kpiReport1$ranCallSetupSuccesRatio           ) * ( kpiReport1$CallAttempts              / kpiReport1$ModuleVoiceTests ) 
kpiReport1$lostCallDropRatio                    <-  1.0 * ( kpiReport1$maxCallDropRatio                  - kpiReport1$ranCallDropRatio                  ) * ( kpiReport1$CallAttempts              / kpiReport1$ModuleVoiceTests ) 
kpiReport1$lostCstAverage                       <-  1.0 * ( kpiReport1$maxCstAverage                     - kpiReport1$ranCstAverage                     ) * ( kpiReport1$CallAttempts              / kpiReport1$ModuleVoiceTests ) 
kpiReport1$lostCstPoorRatio                     <-  1.0 * ( kpiReport1$maxCstPoorRatio                   - kpiReport1$ranCstPoorRatio                   ) * ( kpiReport1$CallAttempts              / kpiReport1$ModuleVoiceTests ) 
kpiReport1$lostCstPercentile10                  <-  1.0 * ( kpiReport1$maxCstPercentile10                - kpiReport1$ranCstPercentile10                ) * ( kpiReport1$CallAttempts              / kpiReport1$ModuleVoiceTests ) 
kpiReport1$lostSpeechPolqaAverage               <-  1.0 * ( kpiReport1$maxSpeechPolqaAverage             - kpiReport1$ranSpeechPolqaAverage             ) * ( kpiReport1$SpeechTestsAttempts       / kpiReport1$ModulePOLQATests ) 
kpiReport1$lostSpeechTestsPoorRatio             <-  1.0 * ( kpiReport1$maxSpeechTestsPoorRatio           - kpiReport1$ranSpeechTestsPoorRatio           ) * ( kpiReport1$SpeechTestsAttempts       / kpiReport1$ModulePOLQATests ) 
kpiReport1$lostSpeechPolqaPercentile90          <-  1.0 * ( kpiReport1$maxSpeechPolqaPercentile90        - kpiReport1$ranSpeechPolqaPercentile90        ) * ( kpiReport1$SpeechTestsAttempts       / kpiReport1$ModulePOLQATests ) 
kpiReport1$lostHttpFdfsCompletedRatio           <-  1.0 * ( kpiReport1$maxHttpFdfsCompletedRatio         - kpiReport1$ranHttpFdfsCompletedRatio         ) * ( (kpiReport1$HttpFdfsDlAttemptsCount + kpiReport1$HttpFdfsUlAttemptsCount)   / kpiReport1$ModuleHTTPTTests ) 
kpiReport1$lostHttpFdttDlMdrPercentile10        <-  1.0 * ( kpiReport1$maxHttpFdttDlMdrPercentile10      - kpiReport1$ranHttpFdttDlMdrPercentile10      ) * ( (kpiReport1$HttpFdfsDlAttemptsCount + kpiReport1$HttpFdfsUlAttemptsCount)   / kpiReport1$ModuleHTTPTTests ) 
kpiReport1$lostHttpFdttDlMdrAverage             <-  1.0 * ( kpiReport1$maxHttpFdttDlMdrAverage           - kpiReport1$ranHttpFdttDlMdrAverage           ) * ( (kpiReport1$HttpFdfsDlAttemptsCount + kpiReport1$HttpFdfsUlAttemptsCount)   / kpiReport1$ModuleHTTPTTests ) 
kpiReport1$lostHttpFdttDlMdrPercentile90        <-  1.0 * ( kpiReport1$maxHttpFdttDlMdrPercentile90      - kpiReport1$ranHttpFdttDlMdrPercentile90      ) * ( (kpiReport1$HttpFdfsDlAttemptsCount + kpiReport1$HttpFdfsUlAttemptsCount)   / kpiReport1$ModuleHTTPTTests ) 
kpiReport1$lostHttpFdttUlMdrPercentile10        <-  1.0 * ( kpiReport1$maxHttpFdttUlMdrPercentile10      - kpiReport1$ranHttpFdttUlMdrPercentile10      ) * ( (kpiReport1$HttpFdfsDlAttemptsCount + kpiReport1$HttpFdfsUlAttemptsCount)   / kpiReport1$ModuleHTTPTTests ) 
kpiReport1$lostHttpFdttUlMdrAverage             <-  1.0 * ( kpiReport1$maxHttpFdttUlMdrAverage           - kpiReport1$ranHttpFdttUlMdrAverage           ) * ( (kpiReport1$HttpFdfsDlAttemptsCount + kpiReport1$HttpFdfsUlAttemptsCount)   / kpiReport1$ModuleHTTPTTests ) 
kpiReport1$lostHttpFdttUlMdrPercentile90        <-  1.0 * ( kpiReport1$maxHttpFdttUlMdrPercentile90      - kpiReport1$ranHttpFdttUlMdrPercentile90      ) * ( (kpiReport1$HttpFdfsDlAttemptsCount + kpiReport1$HttpFdfsUlAttemptsCount)   / kpiReport1$ModuleHTTPTTests ) 
kpiReport1$lostHttpBrowsingCompletedRatio       <-  1.0 * ( kpiReport1$maxHttpBrowsingCompletedRatio     - kpiReport1$ranHttpBrowsingCompletedRatio     ) * ( kpiReport1$HttpBrowsingAttemptsCount / kpiReport1$ModuleHTTPBTests ) 
kpiReport1$lostHttpBrowsingTransferRttAverage   <-  1.0 * ( kpiReport1$maxHttpBrowsingTransferRttAverage - kpiReport1$ranHttpBrowsingTransferRttAverage ) * ( kpiReport1$HttpBrowsingAttemptsCount / kpiReport1$ModuleHTTPBTests ) 
kpiReport1$lostStreamingCompletedRatio          <-  1.0 * ( kpiReport1$maxStreamingCompletedRatio        - kpiReport1$ranStreamingCompletedRatio        ) * ( kpiReport1$StreamingAttemptsCount    / kpiReport1$ModuleVSLTBTests ) 
kpiReport1$lostStreamingVmosPercentile10        <-  1.0 * ( kpiReport1$maxStreamingVmosPercentile10      - kpiReport1$ranStreamingVmosPercentile10      ) * ( kpiReport1$StreamingAttemptsCount    / kpiReport1$ModuleVSLTBTests ) 
kpiReport1$lostStreamingVmosAverage             <-  1.0 * ( kpiReport1$maxStreamingVmosAverage           - kpiReport1$ranStreamingVmosAverage           ) * ( kpiReport1$StreamingAttemptsCount    / kpiReport1$ModuleVSLTBTests ) 
kpiReport1$lostStreamingTtfpAverage             <-  1.0 * ( kpiReport1$maxStreamingTtfpAverage           - kpiReport1$ranStreamingTtfpAverage           ) * ( kpiReport1$StreamingAttemptsCount    / kpiReport1$ModuleVSLTBTests ) 
kpiReport1$lostStreamingTtfpPoorRatio           <-  1.0 * ( kpiReport1$maxStreamingTtfpPoorRatio         - kpiReport1$ranStreamingTtfpPoorRatio         ) * ( kpiReport1$StreamingAttemptsCount    / kpiReport1$ModuleVSLTBTests ) 
kpiReport1$lostApplicationCompletedRatio        <-  1.0 * ( kpiReport1$maxApplicationCompletedRatio      - kpiReport1$ranApplicationCompletedRatio      ) * ( kpiReport1$ApplicationAttemptsCount  / kpiReport1$ModuleMAFBBTests ) 
kpiReport1$lostApplicationTransferTimeAverage   <-  1.0 * ( kpiReport1$maxApplicationTransferTimeAverage - kpiReport1$ranApplicationTransferTimeAverage ) * ( kpiReport1$ApplicationAttemptsCount  / kpiReport1$ModuleMAFBBTests ) 

# RANKING AGGRGATION VOICE
##########################################
kpiReport1$RnkVoiceL1 <- "RANKING AGGR VOICE L1 >>"

kpiReport1$ranCSSR   <-  kpiReport1$ranCallSetupSuccesRatio
kpiReport1$ranDCR    <-  kpiReport1$ranCallDropRatio
kpiReport1$ranCST    <- (kpiReport1$ranCstAverage+kpiReport1$ranCstPoorRatio+kpiReport1$ranCstPercentile10)
kpiReport1$ranPOLQA  <- (kpiReport1$ranSpeechPolqaAverage+kpiReport1$ranSpeechTestsPoorRatio+kpiReport1$ranSpeechPolqaPercentile90)
kpiReport1$maxCSSR   <-  kpiReport1$maxCallSetupSuccesRatio
kpiReport1$maxDCR    <-  kpiReport1$maxCallDropRatio
kpiReport1$maxCST    <- (kpiReport1$maxCstAverage+kpiReport1$maxCstPoorRatio+kpiReport1$maxCstPercentile10)
kpiReport1$maxPOLQA  <- (kpiReport1$maxSpeechPolqaAverage+kpiReport1$maxSpeechTestsPoorRatio+kpiReport1$maxSpeechPolqaPercentile90)
kpiReport1$lostCSSR  <-  kpiReport1$lostCallSetupSuccesRatio
kpiReport1$lostDCR   <-  kpiReport1$lostCallDropRatio
kpiReport1$lostCST   <- (kpiReport1$lostCstAverage+kpiReport1$lostCstPoorRatio+kpiReport1$lostCstPercentile10)
kpiReport1$lostPOLQA <- (kpiReport1$lostSpeechPolqaAverage+kpiReport1$lostSpeechTestsPoorRatio+kpiReport1$lostSpeechPolqaPercentile90)

kpiReport1$RnkVoice <- "RANKING AGGR VOICE >>"

kpiReport1$ranVoice <- (kpiReport1$ranCallSetupSuccesRatio+kpiReport1$ranCallDropRatio+kpiReport1$ranCstAverage+kpiReport1$ranCstPoorRatio+kpiReport1$ranCstPercentile10+kpiReport1$ranSpeechPolqaAverage+kpiReport1$ranSpeechTestsPoorRatio+kpiReport1$ranSpeechPolqaPercentile90)
kpiReport1$maxVoice <- (kpiReport1$maxCallSetupSuccesRatio+kpiReport1$maxCallDropRatio+kpiReport1$maxCstAverage+kpiReport1$maxCstPoorRatio+kpiReport1$maxCstPercentile10+kpiReport1$maxSpeechPolqaAverage+kpiReport1$maxSpeechTestsPoorRatio+kpiReport1$maxSpeechPolqaPercentile90)
kpiReport1$lostVoice <-(kpiReport1$lostCallSetupSuccesRatio+kpiReport1$lostCallDropRatio+kpiReport1$lostCstAverage+kpiReport1$lostCstPoorRatio+kpiReport1$lostCstPercentile10+kpiReport1$lostSpeechPolqaAverage+kpiReport1$lostSpeechTestsPoorRatio+kpiReport1$lostSpeechPolqaPercentile90)


# RANKING AGGRGATION DATA
##########################################
kpiReport1$RnkDataL1 <- "RANKING AGGR DATA L1 >>"

kpiReport1$ranHttpT  <- (kpiReport1$ranHttpFdfsCompletedRatio+kpiReport1$ranHttpFdttDlMdrPercentile10+kpiReport1$ranHttpFdttDlMdrAverage+kpiReport1$ranHttpFdttDlMdrPercentile90+kpiReport1$ranHttpFdttUlMdrPercentile10+kpiReport1$ranHttpFdttUlMdrAverage+kpiReport1$ranHttpFdttUlMdrPercentile90)
kpiReport1$ranHttpB  <- (kpiReport1$ranHttpBrowsingCompletedRatio+kpiReport1$ranHttpBrowsingTransferRttAverage)
kpiReport1$ranVSLVS  <- (kpiReport1$ranStreamingCompletedRatio+kpiReport1$ranStreamingVmosPercentile10+kpiReport1$ranStreamingVmosAverage+kpiReport1$ranStreamingTtfpAverage+kpiReport1$ranStreamingTtfpPoorRatio)
kpiReport1$ranAPPFB  <- (kpiReport1$ranApplicationCompletedRatio+kpiReport1$ranApplicationTransferTimeAverage)
kpiReport1$maxHttpT  <- (kpiReport1$maxHttpFdfsCompletedRatio+kpiReport1$maxHttpFdttDlMdrPercentile10+kpiReport1$maxHttpFdttDlMdrAverage+kpiReport1$maxHttpFdttDlMdrPercentile90+kpiReport1$maxHttpFdttUlMdrPercentile10+kpiReport1$maxHttpFdttUlMdrAverage+kpiReport1$maxHttpFdttUlMdrPercentile90)
kpiReport1$maxHttpB  <- (kpiReport1$maxHttpBrowsingCompletedRatio+kpiReport1$maxHttpBrowsingTransferRttAverage)
kpiReport1$maxVSLVS  <- (kpiReport1$maxStreamingCompletedRatio+kpiReport1$maxStreamingVmosPercentile10+kpiReport1$maxStreamingVmosAverage+kpiReport1$maxStreamingTtfpAverage+kpiReport1$maxStreamingTtfpPoorRatio)
kpiReport1$maxAPPFB  <- (kpiReport1$maxApplicationCompletedRatio+kpiReport1$maxApplicationTransferTimeAverage)
kpiReport1$lostHttpT  <- (kpiReport1$lostHttpFdfsCompletedRatio+kpiReport1$lostHttpFdttDlMdrPercentile10+kpiReport1$lostHttpFdttDlMdrAverage+kpiReport1$lostHttpFdttDlMdrPercentile90+kpiReport1$lostHttpFdttUlMdrPercentile10+kpiReport1$lostHttpFdttUlMdrAverage+kpiReport1$lostHttpFdttUlMdrPercentile90)
kpiReport1$lostHttpB  <- (kpiReport1$lostHttpBrowsingCompletedRatio+kpiReport1$lostHttpBrowsingTransferRttAverage)
kpiReport1$lostVSLVS  <- (kpiReport1$lostStreamingCompletedRatio+kpiReport1$lostStreamingVmosPercentile10+kpiReport1$lostStreamingVmosAverage+kpiReport1$lostStreamingTtfpAverage+kpiReport1$lostStreamingTtfpPoorRatio)
kpiReport1$lostAPPFB  <- (kpiReport1$lostApplicationCompletedRatio+kpiReport1$lostApplicationTransferTimeAverage)

kpiReport1$RnkData <- "RANKING AGGR DATA >>"

kpiReport1$ranData  <- (kpiReport1$ranHttpFdfsCompletedRatio+kpiReport1$ranHttpFdttDlMdrPercentile10+kpiReport1$ranHttpFdttDlMdrAverage+kpiReport1$ranHttpFdttDlMdrPercentile90+kpiReport1$ranHttpFdttUlMdrPercentile10+kpiReport1$ranHttpFdttUlMdrAverage+kpiReport1$ranHttpFdttUlMdrPercentile90+kpiReport1$ranHttpBrowsingCompletedRatio+kpiReport1$ranHttpBrowsingTransferRttAverage+kpiReport1$ranStreamingCompletedRatio+kpiReport1$ranStreamingVmosPercentile10+kpiReport1$ranStreamingVmosAverage+kpiReport1$ranStreamingTtfpAverage+kpiReport1$ranStreamingTtfpPoorRatio+kpiReport1$ranApplicationCompletedRatio+kpiReport1$ranApplicationTransferTimeAverage)
kpiReport1$maxData  <- (kpiReport1$maxHttpFdfsCompletedRatio+kpiReport1$maxHttpFdttDlMdrPercentile10+kpiReport1$maxHttpFdttDlMdrAverage+kpiReport1$maxHttpFdttDlMdrPercentile90+kpiReport1$maxHttpFdttUlMdrPercentile10+kpiReport1$maxHttpFdttUlMdrAverage+kpiReport1$maxHttpFdttUlMdrPercentile90+kpiReport1$maxHttpBrowsingCompletedRatio+kpiReport1$maxHttpBrowsingTransferRttAverage+kpiReport1$maxStreamingCompletedRatio+kpiReport1$maxStreamingVmosPercentile10+kpiReport1$maxStreamingVmosAverage+kpiReport1$maxStreamingTtfpAverage+kpiReport1$maxStreamingTtfpPoorRatio+kpiReport1$maxApplicationCompletedRatio+kpiReport1$maxApplicationTransferTimeAverage)
kpiReport1$lostData  <- (kpiReport1$lostHttpFdfsCompletedRatio+kpiReport1$lostHttpFdttDlMdrPercentile10+kpiReport1$lostHttpFdttDlMdrAverage+kpiReport1$lostHttpFdttDlMdrPercentile90+kpiReport1$lostHttpFdttUlMdrPercentile10+kpiReport1$lostHttpFdttUlMdrAverage+kpiReport1$lostHttpFdttUlMdrPercentile90+kpiReport1$lostHttpBrowsingCompletedRatio+kpiReport1$lostHttpBrowsingTransferRttAverage+kpiReport1$lostStreamingCompletedRatio+kpiReport1$lostStreamingVmosPercentile10+kpiReport1$lostStreamingVmosAverage+kpiReport1$lostStreamingTtfpAverage+kpiReport1$lostStreamingTtfpPoorRatio+kpiReport1$lostApplicationCompletedRatio+kpiReport1$lostApplicationTransferTimeAverage)

# RANKING AGGRGATION OVERALL
##########################################
kpiReport1$RANKINGTOTAL <- "RANKING TOTAL >>"
kpiReport1$ranRanking <- kpiReport1$ranData + kpiReport1$ranVoice
kpiReport1$maxRanking <- kpiReport1$maxData + kpiReport1$maxVoice
kpiReport1$lostRanking <- kpiReport1$lostData + kpiReport1$lostVoice

dbWriteTable(con, resultTable, as.data.frame(kpiReport1),overwrite = TRUE)

