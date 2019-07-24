library(DBI)
library(odbc)

library(sunburstR)
library(dplyr)
library(tidyr)
library(plotrix)

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

setwd("C:/Users/tmiksa/Desktop/R-Project")
######## ##     ## ##    ##  ######  ######## ####  #######  ##    ##  ######  
##       ##     ## ###   ## ##    ##    ##     ##  ##     ## ###   ## ##    ## 
##       ##     ## ####  ## ##          ##     ##  ##     ## ####  ## ##       
######   ##     ## ## ## ## ##          ##     ##  ##     ## ## ## ##  ######  
##       ##     ## ##  #### ##          ##     ##  ##     ## ##  ####       ## 
##       ##     ## ##   ### ##    ##    ##     ##  ##     ## ##   ### ##    ## 
##        #######  ##    ##  ######     ##    ####  #######  ##    ##  ######  

# CREATE FUNCTION TO GRAB ACTIVE SESSIONIDS FROM CDR 2018
active.sessions <- function(f_g_lev_1,f_g_lev_2,f_g_lev_3,f_g_lev_4,f_t_name,f_w_name,f_region,f_vendor)
{
  sessionID <- paste("SELECT DISTINCT SessionId 
                     FROM vDataCDR2018_Operator1 
                     WHERE   G_Level_1 like '", f_g_lev_1, "'",
                     " and G_Level_2 like '", f_g_lev_2, "'",
                     " and G_Level_3 like '", f_g_lev_3, "'",
                     " and G_Level_4 like '", f_g_lev_4, "'",
                     " and Train_Name like '", f_t_name, "'",
                     " and Wagon_Number like '", f_w_name, "'",
                     " and Region like '", f_region, "'",
                     " and Vendor like '", f_vendor, 
                     "' ORDER BY SessionId",
                     sep = "")
  return(sessionID)
}

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
  

# Disconnect Database
##########################################
dbDisconnect(con)