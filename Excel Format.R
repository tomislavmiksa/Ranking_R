library(DBI)
library(odbc)
library(lubridate)
library(dplyr)
library(tidyr)
library(plotrix)
library(gridExtra)

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

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
                       
###################################################################################################################################################
#                                                               FUNCTION plot GRAPH                                                               #
###################################################################################################################################################
plotBarCum <- function(inputData, inputHeader, inputNames, inputColors ,inputLab)
{
  maxi = max(inputData)
  if(is.na(maxi)) { maxi = 500 }
  bp <- barplot(inputData,
                main = inputHeader,
                cex.main = 1.5,
                horiz=TRUE, 
                las=1,
                names.arg = inputNames,
                cex.names = 1.5,
                col=inputColors,
                xlab = "",
                ylab = "",
                xaxt='n',
                xlim = c(0, maxi),
                width = 1)
  text(x=inputData, 
       y=bp, 
       labels=inputLab,
       cex=1.2,
       pos=4, 
       xpd=NA)
}

###################################################################################################################################################
#                                                               FUNCTION plot RANKING splitted BARS                                               #
###################################################################################################################################################
plotBarCum1 <- function(inputTab, inputHead, inputNam, inputColo, inputKP, legFlag, inputLabP,inputLab,mi)
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
                xlim = c(0,4*mi),
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

###################################################################################################################################################
#                                                               FUNCTION plot RANKING                                                             #
###################################################################################################################################################
plotSimpleGraph <- function(operators, 
                            kpi1, 
                            kpi2, 
                            kpi3, 
                            mkpi1, 
                            mkpi2, 
                            mkpi3, 
                            lkpi1, 
                            lkpi2, 
                            lkpi3,
                            nkpi1, 
                            nkpi2, 
                            nkpi3,
                            col1,
                            col2,
                            col3,
                            kpiName)
{
  # mar=c(bottom, left, top, right)
  par(mar=c(5,15,5,10)+.1)

  # OPERATOR 1 RANKING
  plotBarCum(inputData   = kpi1, 
             inputHeader = paste(kpiName,"\n",operators[1],"Ranking"), 
             inputNames  = nkpi1, 
             inputColors = c(col1[1]), 
             inputLab    = paste(round(kpi1,digits = 2), 
                                 "of", 
                                 round(mkpi1,digits = 2),
                                 "\n",
                                 round(100.0*kpi1/mkpi1,digits = 2),
                                 "%")
             )
  
  # OPERATOR 2 RANKING
  plotBarCum(inputData   = kpi2, 
             inputHeader = paste(kpiName,"\n",operators[2],"Ranking"), 
             inputNames  = nkpi2, 
             inputColors = c(col2[1]), 
             inputLab    = paste(round(kpi2,digits = 2), 
                                 "of", 
                                 round(mkpi2,digits = 2),
                                 "\n",
                                 round(100.0*kpi2/mkpi2,digits = 2),
                                 "%")
             )
  
  # OPERATOR 3 RANKING
  plotBarCum(inputData   = kpi3, 
             inputHeader = paste(kpiName,"\n",operators[3],"Ranking"), 
             inputNames  = nkpi3, 
             inputColors = c(col3[1]), 
             inputLab    = paste(round(kpi3,digits = 2), 
                                 "of", 
                                 round(mkpi3,digits = 2),
                                 "\n",
                                 round(100.0*kpi3/mkpi3,digits = 2),
                                 "%")
             )
  # OPERATOR 1 GAP
  plotBarCum(inputData   = (mkpi1-kpi1), 
             inputHeader = paste(kpiName,"\n",operators[1],"Gap to MAX"), 
             inputNames  = nkpi1, 
             inputColors = c(col1[2]), 
             inputLab    = paste(round((mkpi1-kpi1),digits = 2), 
                                 "of", 
                                 round(mkpi1,digits = 2),
                                 "\n",
                                 round(100.0*(mkpi1-kpi1)/mkpi1,digits = 2),
                                 "%")
  )
  
  # OPERATOR 2 GAP
  plotBarCum(inputData   = (mkpi2-kpi2), 
             inputHeader = paste(kpiName,"\n",operators[2],"Gap to MAX"), 
             inputNames  = nkpi2, 
             inputColors = c(col2[2]), 
             inputLab    = paste(round((mkpi2-kpi2),digits = 2), 
                                 "of", 
                                 round(mkpi2,digits = 2),
                                 "\n",
                                 round(100.0*(mkpi2-kpi2)/mkpi2,digits = 2),
                                 "%")
  )
  
  # OPERATOR 3 GAP
  plotBarCum(inputData   = (mkpi3-kpi3), 
             inputHeader = paste(kpiName,"\n",operators[3],"Gap to MAX"), 
             inputNames  = nkpi3, 
             inputColors = c(col3[2]), 
             inputLab    = paste(round((mkpi3-kpi3),digits = 2), 
                                 "of", 
                                 round(mkpi3,digits = 2),
                                 "\n",
                                 round(100.0*(mkpi3-kpi3)/mkpi3,digits = 2),
                                 "%")
  )  
  # OPERATOR 1 LOST
  plotBarCum(inputData   = lkpi1, 
             inputHeader = paste(kpiName,"\n",operators[1],"Points Lost"), 
             inputNames  = nkpi1, 
             inputColors = c(col1[3]), 
             inputLab    = paste(round(lkpi1,digits = 2), 
                                 "of", 
                                 round(mkpi1,digits = 2),
                                 "\n",
                                 round(100.0*lkpi1/mkpi1,digits = 2),
                                 "%")
  )
  
  # OPERATOR 2 LOST
  plotBarCum(inputData   = lkpi2, 
             inputHeader = paste(kpiName,"\n",operators[2],"Points Lost"), 
             inputNames  = nkpi2, 
             inputColors = c(col2[3]), 
             inputLab    = paste(round(lkpi2,digits = 2), 
                                 "of", 
                                 round(mkpi2,digits = 2),
                                 "\n",
                                 round(100.0*lkpi2/mkpi2,digits = 2),
                                 "%")
  )
  
  # OPERATOR 3 LOST
  plotBarCum(inputData   = lkpi3, 
             inputHeader = paste(kpiName,"\n",operators[3],"Points Lost"), 
             inputNames  = nkpi3, 
             inputColors = c(col3[3]), 
             inputLab    = paste(round(lkpi3,digits = 2), 
                                 "of", 
                                 round(mkpi3,digits = 2),
                                 "\n",
                                 round(100.0*lkpi3/mkpi3,digits = 2),
                                 "%")
  )

}

###################################################################################################################################################
#                                                               FUNCTION plot RANKING                                                             #
###################################################################################################################################################
plotCumulativeGraph <- function(operators, kpi1, kpi2, kpi3, mkpi1, mkpi2, mkpi3, lkpi1, lkpi2, lkpi3,nkpi1, nkpi2, nkpi3,col1,col2,col3,leg1,leg2,leg3,kpiName)
  {
    # mar=c(bottom, left, top, right)
    par(mar=c(5,15,5,10)+.1)
    
    # OPERATOR 1 RANKING
    plotBarCum1( kpi1, 
                 "Total Ranking", 
                 nkpi1, 
                 col1, 
                 leg1, 
                 "Yes", 
                 rowSums(kpi1),
                 round( rowSums(kpi1), digits = 2 ),
                 max( rowSums(kpi1) )
                 )
                 
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

# EXTRACT KPI REPORT FOR OPERATOR
kpiReport1 <-    dbGetQuery(con,paste("SELECT * FROM NEW_VDF_KPI_REPORT_OPERATOR_1"))
kpiReport2 <-    dbGetQuery(con,paste("SELECT * FROM NEW_VDF_KPI_REPORT_OPERATOR_2"))
kpiReport3 <-    dbGetQuery(con,paste("SELECT * FROM NEW_VDF_KPI_REPORT_OPERATOR_3"))
Encoding(kpiReport1$G_Level_4) = "latin1"
Encoding(kpiReport2$G_Level_4) = "latin1"
Encoding(kpiReport3$G_Level_4) = "latin1"

# CALL AND TEST ATTEMPTS SET
##########################################
kpiReport1$MODULETESTCOUNT <- "MODULE TEST COUNT >>"
kpiReport2$MODULETESTCOUNT <- "MODULE TEST COUNT >>"
kpiReport3$MODULETESTCOUNT <- "MODULE TEST COUNT >>"
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
# SET MODULE TEST ATTEMPTS
kpiReport2$ModuleVoiceTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$CallAttempts[kpiReport2$Rnk == 100]
kpiReport2$ModuleVoiceTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "Connecting Roads" ]  <- kpiReport2$CallAttempts[kpiReport2$Rnk == 300]
kpiReport2$ModuleVoiceTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$CallAttempts[kpiReport2$Rnk == 200]
kpiReport2$ModuleVoiceTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "Train Route" ]       <- kpiReport2$CallAttempts[kpiReport2$Rnk == 400]
kpiReport2$ModulePOLQATests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$SpeechTestsAttempts[kpiReport2$Rnk == 100]
kpiReport2$ModulePOLQATests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "Connecting Roads" ]  <- kpiReport2$SpeechTestsAttempts[kpiReport2$Rnk == 300]
kpiReport2$ModulePOLQATests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$SpeechTestsAttempts[kpiReport2$Rnk == 200]
kpiReport2$ModulePOLQATests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "Train Route" ]       <- kpiReport2$SpeechTestsAttempts[kpiReport2$Rnk == 400]
kpiReport2$ModuleHTTPTTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$HttpFdfsDlAttemptsCount[kpiReport2$Rnk == 100] + kpiReport2$HttpFdfsUlAttemptsCount[kpiReport2$Rnk == 100]
kpiReport2$ModuleHTTPTTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "Connecting Roads" ]  <- kpiReport2$HttpFdfsDlAttemptsCount[kpiReport2$Rnk == 300] + kpiReport2$HttpFdfsUlAttemptsCount[kpiReport2$Rnk == 300]
kpiReport2$ModuleHTTPTTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$HttpFdfsDlAttemptsCount[kpiReport2$Rnk == 200] + kpiReport2$HttpFdfsUlAttemptsCount[kpiReport2$Rnk == 200]
kpiReport2$ModuleHTTPTTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "Train Route" ]       <- kpiReport2$HttpFdfsDlAttemptsCount[kpiReport2$Rnk == 400] + kpiReport2$HttpFdfsUlAttemptsCount[kpiReport2$Rnk == 400]
kpiReport2$ModuleHTTPBTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$HttpBrowsingAttemptsCount[kpiReport2$Rnk == 100]
kpiReport2$ModuleHTTPBTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "Connecting Roads" ]  <- kpiReport2$HttpBrowsingAttemptsCount[kpiReport2$Rnk == 300]
kpiReport2$ModuleHTTPBTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$HttpBrowsingAttemptsCount[kpiReport2$Rnk == 200]
kpiReport2$ModuleHTTPBTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "Train Route" ]       <- kpiReport2$HttpBrowsingAttemptsCount[kpiReport2$Rnk == 400]
kpiReport2$ModuleVSLTBTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$StreamingAttemptsCount[kpiReport2$Rnk == 100]
kpiReport2$ModuleVSLTBTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "Connecting Roads" ]  <- kpiReport2$StreamingAttemptsCount[kpiReport2$Rnk == 300]
kpiReport2$ModuleVSLTBTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$StreamingAttemptsCount[kpiReport2$Rnk == 200]
kpiReport2$ModuleVSLTBTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "Train Route" ]       <- kpiReport2$StreamingAttemptsCount[kpiReport2$Rnk == 400]
kpiReport2$ModuleMAFBBTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$ApplicationAttemptsCount[kpiReport2$Rnk == 100]
kpiReport2$ModuleMAFBBTests[kpiReport2$G_Level_1 == "Drive" & kpiReport2$G_Level_2 == "Connecting Roads" ]  <- kpiReport2$ApplicationAttemptsCount[kpiReport2$Rnk == 300]
kpiReport2$ModuleMAFBBTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "City" ]              <- kpiReport2$ApplicationAttemptsCount[kpiReport2$Rnk == 200]
kpiReport2$ModuleMAFBBTests[kpiReport2$G_Level_1 == "Walk"  & kpiReport2$G_Level_2 == "Train Route" ]       <- kpiReport2$ApplicationAttemptsCount[kpiReport2$Rnk == 400]
# SET MODULE TEST ATTEMPTS
kpiReport3$ModuleVoiceTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$CallAttempts[kpiReport3$Rnk == 100]
kpiReport3$ModuleVoiceTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "Connecting Roads" ]  <- kpiReport3$CallAttempts[kpiReport3$Rnk == 300]
kpiReport3$ModuleVoiceTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$CallAttempts[kpiReport3$Rnk == 200]
kpiReport3$ModuleVoiceTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "Train Route" ]       <- kpiReport3$CallAttempts[kpiReport3$Rnk == 400]
kpiReport3$ModulePOLQATests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$SpeechTestsAttempts[kpiReport3$Rnk == 100]
kpiReport3$ModulePOLQATests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "Connecting Roads" ]  <- kpiReport3$SpeechTestsAttempts[kpiReport3$Rnk == 300]
kpiReport3$ModulePOLQATests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$SpeechTestsAttempts[kpiReport3$Rnk == 200]
kpiReport3$ModulePOLQATests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "Train Route" ]       <- kpiReport3$SpeechTestsAttempts[kpiReport3$Rnk == 400]
kpiReport3$ModuleHTTPTTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$HttpFdfsDlAttemptsCount[kpiReport3$Rnk == 100] + kpiReport3$HttpFdfsUlAttemptsCount[kpiReport3$Rnk == 100]
kpiReport3$ModuleHTTPTTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "Connecting Roads" ]  <- kpiReport3$HttpFdfsDlAttemptsCount[kpiReport3$Rnk == 300] + kpiReport3$HttpFdfsUlAttemptsCount[kpiReport3$Rnk == 300]
kpiReport3$ModuleHTTPTTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$HttpFdfsDlAttemptsCount[kpiReport3$Rnk == 200] + kpiReport3$HttpFdfsUlAttemptsCount[kpiReport3$Rnk == 200]
kpiReport3$ModuleHTTPTTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "Train Route" ]       <- kpiReport3$HttpFdfsDlAttemptsCount[kpiReport3$Rnk == 400] + kpiReport3$HttpFdfsUlAttemptsCount[kpiReport3$Rnk == 400]
kpiReport3$ModuleHTTPBTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$HttpBrowsingAttemptsCount[kpiReport3$Rnk == 100]
kpiReport3$ModuleHTTPBTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "Connecting Roads" ]  <- kpiReport3$HttpBrowsingAttemptsCount[kpiReport3$Rnk == 300]
kpiReport3$ModuleHTTPBTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$HttpBrowsingAttemptsCount[kpiReport3$Rnk == 200]
kpiReport3$ModuleHTTPBTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "Train Route" ]       <- kpiReport3$HttpBrowsingAttemptsCount[kpiReport3$Rnk == 400]
kpiReport3$ModuleVSLTBTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$StreamingAttemptsCount[kpiReport3$Rnk == 100]
kpiReport3$ModuleVSLTBTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "Connecting Roads" ]  <- kpiReport3$StreamingAttemptsCount[kpiReport3$Rnk == 300]
kpiReport3$ModuleVSLTBTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$StreamingAttemptsCount[kpiReport3$Rnk == 200]
kpiReport3$ModuleVSLTBTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "Train Route" ]       <- kpiReport3$StreamingAttemptsCount[kpiReport3$Rnk == 400]
kpiReport3$ModuleMAFBBTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$ApplicationAttemptsCount[kpiReport3$Rnk == 100]
kpiReport3$ModuleMAFBBTests[kpiReport3$G_Level_1 == "Drive" & kpiReport3$G_Level_2 == "Connecting Roads" ]  <- kpiReport3$ApplicationAttemptsCount[kpiReport3$Rnk == 300]
kpiReport3$ModuleMAFBBTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "City" ]              <- kpiReport3$ApplicationAttemptsCount[kpiReport3$Rnk == 200]
kpiReport3$ModuleMAFBBTests[kpiReport3$G_Level_1 == "Walk"  & kpiReport3$G_Level_2 == "Train Route" ]       <- kpiReport3$ApplicationAttemptsCount[kpiReport3$Rnk == 400]

# RANKING KPIs
##########################################
kpiReport1$RANKINGKPI <- "RANKING EACH KPI >>"
kpiReport2$RANKINGKPI <- "RANKING EACH KPI >>"
kpiReport3$RANKINGKPI <- "RANKING EACH KPI >>"
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

for(i in 1:nrow(kpiReport2)) 
{
  kpiReport2$ranCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport2$CallSetupSuccesRatio[i]          , 1,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranCallDropRatio[i]                     <- rankingCalc(kpiReport2$CallDropRatio[i]                 , 2,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranCstAverage[i]                        <- rankingCalc(kpiReport2$CstAverage[i]                    , 3,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranCstPoorRatio[i]                      <- rankingCalc(kpiReport2$CstPoorRatio[i]                  , 4,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranCstPercentile10[i]                   <- rankingCalc(kpiReport2$CstPercentile10[i]               , 5,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranSpeechPolqaAverage[i]                <- rankingCalc(kpiReport2$SpeechPolqaAverage[i]            , 6,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport2$SpeechTestsPoorRatio[i]          , 7,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport2$SpeechPolqaPercentile90[i]       , 8,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport2$HttpFdfsCompletedRatio[i]        , 9,  kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport2$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport2$HttpFdttDlMdrAverage[i]          , 11, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport2$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport2$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport2$HttpFdttUlMdrAverage[i]          , 14, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport2$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport2$HttpBrowsingCompletedRatio[i]    , 16, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport2$HttpBrowsingTransferRttAverage[i], 17, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranStreamingCompletedRatio[i]           <- rankingCalc(kpiReport2$StreamingCompletedRatio[i]       , 18, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport2$StreamingVmosPercentile10[i]     , 19, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranStreamingVmosAverage[i]              <- rankingCalc(kpiReport2$StreamingVmosAverage[i]          , 20, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranStreamingTtfpAverage[i]              <- rankingCalc(kpiReport2$StreamingTtfpAverage[i]          , 21, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport2$StreamingTtfpPoorRatio[i]        , 22, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranApplicationCompletedRatio[i]         <- rankingCalc(kpiReport2$ApplicationCompletedRatio[i]     , 23, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]   
  kpiReport2$ranApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport2$ApplicationTransferTimeAverage[i], 24, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[1]
  kpiReport2$maxCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport2$CallSetupSuccesRatio[i]          , 1, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxCallDropRatio[i]                     <- rankingCalc(kpiReport2$CallDropRatio[i]                 , 2, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxCstAverage[i]                        <- rankingCalc(kpiReport2$CstAverage[i]                    , 3, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxCstPoorRatio[i]                      <- rankingCalc(kpiReport2$CstPoorRatio[i]                  , 4, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxCstPercentile10[i]                   <- rankingCalc(kpiReport2$CstPercentile10[i]               , 5, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxSpeechPolqaAverage[i]                <- rankingCalc(kpiReport2$SpeechPolqaAverage[i]            , 6, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport2$SpeechTestsPoorRatio[i]          , 7, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport2$SpeechPolqaPercentile90[i]       , 8, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport2$HttpFdfsCompletedRatio[i]        , 9, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport2$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport2$HttpFdttDlMdrAverage[i]          , 11, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport2$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport2$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport2$HttpFdttUlMdrAverage[i]          , 14, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport2$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport2$HttpBrowsingCompletedRatio[i]    , 16, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport2$HttpBrowsingTransferRttAverage[i], 17, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxStreamingCompletedRatio[i]           <- rankingCalc(kpiReport2$StreamingCompletedRatio[i]       , 18, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport2$StreamingVmosPercentile10[i]     , 19, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxStreamingVmosAverage[i]              <- rankingCalc(kpiReport2$StreamingVmosAverage[i]          , 20, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxStreamingTtfpAverage[i]              <- rankingCalc(kpiReport2$StreamingTtfpAverage[i]          , 21, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport2$StreamingTtfpPoorRatio[i]        , 22, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxApplicationCompletedRatio[i]         <- rankingCalc(kpiReport2$ApplicationCompletedRatio[i]     , 23, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2]   
  kpiReport2$maxApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport2$ApplicationTransferTimeAverage[i], 24, kpiReport2$G_Level_1[i], kpiReport2$G_Level_2[i])[2] 
}

for(i in 1:nrow(kpiReport3)) 
{
  kpiReport3$ranCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport3$CallSetupSuccesRatio[i]          , 1,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranCallDropRatio[i]                     <- rankingCalc(kpiReport3$CallDropRatio[i]                 , 2,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranCstAverage[i]                        <- rankingCalc(kpiReport3$CstAverage[i]                    , 3,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranCstPoorRatio[i]                      <- rankingCalc(kpiReport3$CstPoorRatio[i]                  , 4,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranCstPercentile10[i]                   <- rankingCalc(kpiReport3$CstPercentile10[i]               , 5,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranSpeechPolqaAverage[i]                <- rankingCalc(kpiReport3$SpeechPolqaAverage[i]            , 6,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport3$SpeechTestsPoorRatio[i]          , 7,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport3$SpeechPolqaPercentile90[i]       , 8,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport3$HttpFdfsCompletedRatio[i]        , 9,  kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport3$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport3$HttpFdttDlMdrAverage[i]          , 11, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport3$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport3$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport3$HttpFdttUlMdrAverage[i]          , 14, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport3$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport3$HttpBrowsingCompletedRatio[i]    , 16, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport3$HttpBrowsingTransferRttAverage[i], 17, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranStreamingCompletedRatio[i]           <- rankingCalc(kpiReport3$StreamingCompletedRatio[i]       , 18, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport3$StreamingVmosPercentile10[i]     , 19, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranStreamingVmosAverage[i]              <- rankingCalc(kpiReport3$StreamingVmosAverage[i]          , 20, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranStreamingTtfpAverage[i]              <- rankingCalc(kpiReport3$StreamingTtfpAverage[i]          , 21, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport3$StreamingTtfpPoorRatio[i]        , 22, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranApplicationCompletedRatio[i]         <- rankingCalc(kpiReport3$ApplicationCompletedRatio[i]     , 23, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]   
  kpiReport3$ranApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport3$ApplicationTransferTimeAverage[i], 24, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[1]
  kpiReport3$maxCallSetupSuccesRatio[i]              <- rankingCalc(kpiReport3$CallSetupSuccesRatio[i]          , 1, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxCallDropRatio[i]                     <- rankingCalc(kpiReport3$CallDropRatio[i]                 , 2, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxCstAverage[i]                        <- rankingCalc(kpiReport3$CstAverage[i]                    , 3, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxCstPoorRatio[i]                      <- rankingCalc(kpiReport3$CstPoorRatio[i]                  , 4, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxCstPercentile10[i]                   <- rankingCalc(kpiReport3$CstPercentile10[i]               , 5, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxSpeechPolqaAverage[i]                <- rankingCalc(kpiReport3$SpeechPolqaAverage[i]            , 6, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxSpeechTestsPoorRatio[i]              <- rankingCalc(kpiReport3$SpeechTestsPoorRatio[i]          , 7, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxSpeechPolqaPercentile90[i]           <- rankingCalc(kpiReport3$SpeechPolqaPercentile90[i]       , 8, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpFdfsCompletedRatio[i]            <- rankingCalc(kpiReport3$HttpFdfsCompletedRatio[i]        , 9, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpFdttDlMdrPercentile10[i]         <- rankingCalc(kpiReport3$HttpFdttDlMdrPercentile10[i]     , 10, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpFdttDlMdrAverage[i]              <- rankingCalc(kpiReport3$HttpFdttDlMdrAverage[i]          , 11, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpFdttDlMdrPercentile90[i]         <- rankingCalc(kpiReport3$HttpFdttDlMdrPercentile90[i]     , 12, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpFdttUlMdrPercentile10[i]         <- rankingCalc(kpiReport3$HttpFdttUlMdrPercentile10[i]     , 13, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpFdttUlMdrAverage[i]              <- rankingCalc(kpiReport3$HttpFdttUlMdrAverage[i]          , 14, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpFdttUlMdrPercentile90[i]         <- rankingCalc(kpiReport3$HttpFdttUlMdrPercentile90[i]     , 15, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpBrowsingCompletedRatio[i]        <- rankingCalc(kpiReport3$HttpBrowsingCompletedRatio[i]    , 16, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxHttpBrowsingTransferRttAverage[i]    <- rankingCalc(kpiReport3$HttpBrowsingTransferRttAverage[i], 17, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxStreamingCompletedRatio[i]           <- rankingCalc(kpiReport3$StreamingCompletedRatio[i]       , 18, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxStreamingVmosPercentile10[i]         <- rankingCalc(kpiReport3$StreamingVmosPercentile10[i]     , 19, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxStreamingVmosAverage[i]              <- rankingCalc(kpiReport3$StreamingVmosAverage[i]          , 20, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxStreamingTtfpAverage[i]              <- rankingCalc(kpiReport3$StreamingTtfpAverage[i]          , 21, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxStreamingTtfpPoorRatio[i]            <- rankingCalc(kpiReport3$StreamingTtfpPoorRatio[i]        , 22, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxApplicationCompletedRatio[i]         <- rankingCalc(kpiReport3$ApplicationCompletedRatio[i]     , 23, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2]   
  kpiReport3$maxApplicationTransferTimeAverage[i]    <- rankingCalc(kpiReport3$ApplicationTransferTimeAverage[i], 24, kpiReport3$G_Level_1[i], kpiReport3$G_Level_2[i])[2] 
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
kpiReport2$lostCallSetupSuccesRatio             <-  1.0 * ( kpiReport2$maxCallSetupSuccesRatio           - kpiReport2$ranCallSetupSuccesRatio           ) * ( kpiReport2$CallAttempts              / kpiReport2$ModuleVoiceTests ) 
kpiReport2$lostCallDropRatio                    <-  1.0 * ( kpiReport2$maxCallDropRatio                  - kpiReport2$ranCallDropRatio                  ) * ( kpiReport2$CallAttempts              / kpiReport2$ModuleVoiceTests ) 
kpiReport2$lostCstAverage                       <-  1.0 * ( kpiReport2$maxCstAverage                     - kpiReport2$ranCstAverage                     ) * ( kpiReport2$CallAttempts              / kpiReport2$ModuleVoiceTests ) 
kpiReport2$lostCstPoorRatio                     <-  1.0 * ( kpiReport2$maxCstPoorRatio                   - kpiReport2$ranCstPoorRatio                   ) * ( kpiReport2$CallAttempts              / kpiReport2$ModuleVoiceTests ) 
kpiReport2$lostCstPercentile10                  <-  1.0 * ( kpiReport2$maxCstPercentile10                - kpiReport2$ranCstPercentile10                ) * ( kpiReport2$CallAttempts              / kpiReport2$ModuleVoiceTests ) 
kpiReport2$lostSpeechPolqaAverage               <-  1.0 * ( kpiReport2$maxSpeechPolqaAverage             - kpiReport2$ranSpeechPolqaAverage             ) * ( kpiReport2$SpeechTestsAttempts       / kpiReport2$ModulePOLQATests ) 
kpiReport2$lostSpeechTestsPoorRatio             <-  1.0 * ( kpiReport2$maxSpeechTestsPoorRatio           - kpiReport2$ranSpeechTestsPoorRatio           ) * ( kpiReport2$SpeechTestsAttempts       / kpiReport2$ModulePOLQATests ) 
kpiReport2$lostSpeechPolqaPercentile90          <-  1.0 * ( kpiReport2$maxSpeechPolqaPercentile90        - kpiReport2$ranSpeechPolqaPercentile90        ) * ( kpiReport2$SpeechTestsAttempts       / kpiReport2$ModulePOLQATests ) 
kpiReport2$lostHttpFdfsCompletedRatio           <-  1.0 * ( kpiReport2$maxHttpFdfsCompletedRatio         - kpiReport2$ranHttpFdfsCompletedRatio         ) * ( (kpiReport2$HttpFdfsDlAttemptsCount + kpiReport2$HttpFdfsUlAttemptsCount)   / kpiReport2$ModuleHTTPTTests ) 
kpiReport2$lostHttpFdttDlMdrPercentile10        <-  1.0 * ( kpiReport2$maxHttpFdttDlMdrPercentile10      - kpiReport2$ranHttpFdttDlMdrPercentile10      ) * ( (kpiReport2$HttpFdfsDlAttemptsCount + kpiReport2$HttpFdfsUlAttemptsCount)   / kpiReport2$ModuleHTTPTTests ) 
kpiReport2$lostHttpFdttDlMdrAverage             <-  1.0 * ( kpiReport2$maxHttpFdttDlMdrAverage           - kpiReport2$ranHttpFdttDlMdrAverage           ) * ( (kpiReport2$HttpFdfsDlAttemptsCount + kpiReport2$HttpFdfsUlAttemptsCount)   / kpiReport2$ModuleHTTPTTests ) 
kpiReport2$lostHttpFdttDlMdrPercentile90        <-  1.0 * ( kpiReport2$maxHttpFdttDlMdrPercentile90      - kpiReport2$ranHttpFdttDlMdrPercentile90      ) * ( (kpiReport2$HttpFdfsDlAttemptsCount + kpiReport2$HttpFdfsUlAttemptsCount)   / kpiReport2$ModuleHTTPTTests ) 
kpiReport2$lostHttpFdttUlMdrPercentile10        <-  1.0 * ( kpiReport2$maxHttpFdttUlMdrPercentile10      - kpiReport2$ranHttpFdttUlMdrPercentile10      ) * ( (kpiReport2$HttpFdfsDlAttemptsCount + kpiReport2$HttpFdfsUlAttemptsCount)   / kpiReport2$ModuleHTTPTTests ) 
kpiReport2$lostHttpFdttUlMdrAverage             <-  1.0 * ( kpiReport2$maxHttpFdttUlMdrAverage           - kpiReport2$ranHttpFdttUlMdrAverage           ) * ( (kpiReport2$HttpFdfsDlAttemptsCount + kpiReport2$HttpFdfsUlAttemptsCount)   / kpiReport2$ModuleHTTPTTests ) 
kpiReport2$lostHttpFdttUlMdrPercentile90        <-  1.0 * ( kpiReport2$maxHttpFdttUlMdrPercentile90      - kpiReport2$ranHttpFdttUlMdrPercentile90      ) * ( (kpiReport2$HttpFdfsDlAttemptsCount + kpiReport2$HttpFdfsUlAttemptsCount)   / kpiReport2$ModuleHTTPTTests ) 
kpiReport2$lostHttpBrowsingCompletedRatio       <-  1.0 * ( kpiReport2$maxHttpBrowsingCompletedRatio     - kpiReport2$ranHttpBrowsingCompletedRatio     ) * ( kpiReport2$HttpBrowsingAttemptsCount / kpiReport2$ModuleHTTPBTests ) 
kpiReport2$lostHttpBrowsingTransferRttAverage   <-  1.0 * ( kpiReport2$maxHttpBrowsingTransferRttAverage - kpiReport2$ranHttpBrowsingTransferRttAverage ) * ( kpiReport2$HttpBrowsingAttemptsCount / kpiReport2$ModuleHTTPBTests ) 
kpiReport2$lostStreamingCompletedRatio          <-  1.0 * ( kpiReport2$maxStreamingCompletedRatio        - kpiReport2$ranStreamingCompletedRatio        ) * ( kpiReport2$StreamingAttemptsCount    / kpiReport2$ModuleVSLTBTests ) 
kpiReport2$lostStreamingVmosPercentile10        <-  1.0 * ( kpiReport2$maxStreamingVmosPercentile10      - kpiReport2$ranStreamingVmosPercentile10      ) * ( kpiReport2$StreamingAttemptsCount    / kpiReport2$ModuleVSLTBTests ) 
kpiReport2$lostStreamingVmosAverage             <-  1.0 * ( kpiReport2$maxStreamingVmosAverage           - kpiReport2$ranStreamingVmosAverage           ) * ( kpiReport2$StreamingAttemptsCount    / kpiReport2$ModuleVSLTBTests ) 
kpiReport2$lostStreamingTtfpAverage             <-  1.0 * ( kpiReport2$maxStreamingTtfpAverage           - kpiReport2$ranStreamingTtfpAverage           ) * ( kpiReport2$StreamingAttemptsCount    / kpiReport2$ModuleVSLTBTests ) 
kpiReport2$lostStreamingTtfpPoorRatio           <-  1.0 * ( kpiReport2$maxStreamingTtfpPoorRatio         - kpiReport2$ranStreamingTtfpPoorRatio         ) * ( kpiReport2$StreamingAttemptsCount    / kpiReport2$ModuleVSLTBTests ) 
kpiReport2$lostApplicationCompletedRatio        <-  1.0 * ( kpiReport2$maxApplicationCompletedRatio      - kpiReport2$ranApplicationCompletedRatio      ) * ( kpiReport2$ApplicationAttemptsCount  / kpiReport2$ModuleMAFBBTests ) 
kpiReport2$lostApplicationTransferTimeAverage   <-  1.0 * ( kpiReport2$maxApplicationTransferTimeAverage - kpiReport2$ranApplicationTransferTimeAverage ) * ( kpiReport2$ApplicationAttemptsCount  / kpiReport2$ModuleMAFBBTests ) 
kpiReport3$lostCallSetupSuccesRatio             <-  1.0 * ( kpiReport3$maxCallSetupSuccesRatio           - kpiReport3$ranCallSetupSuccesRatio           ) * ( kpiReport3$CallAttempts              / kpiReport3$ModuleVoiceTests ) 
kpiReport3$lostCallDropRatio                    <-  1.0 * ( kpiReport3$maxCallDropRatio                  - kpiReport3$ranCallDropRatio                  ) * ( kpiReport3$CallAttempts              / kpiReport3$ModuleVoiceTests ) 
kpiReport3$lostCstAverage                       <-  1.0 * ( kpiReport3$maxCstAverage                     - kpiReport3$ranCstAverage                     ) * ( kpiReport3$CallAttempts              / kpiReport3$ModuleVoiceTests ) 
kpiReport3$lostCstPoorRatio                     <-  1.0 * ( kpiReport3$maxCstPoorRatio                   - kpiReport3$ranCstPoorRatio                   ) * ( kpiReport3$CallAttempts              / kpiReport3$ModuleVoiceTests ) 
kpiReport3$lostCstPercentile10                  <-  1.0 * ( kpiReport3$maxCstPercentile10                - kpiReport3$ranCstPercentile10                ) * ( kpiReport3$CallAttempts              / kpiReport3$ModuleVoiceTests ) 
kpiReport3$lostSpeechPolqaAverage               <-  1.0 * ( kpiReport3$maxSpeechPolqaAverage             - kpiReport3$ranSpeechPolqaAverage             ) * ( kpiReport3$SpeechTestsAttempts       / kpiReport3$ModulePOLQATests ) 
kpiReport3$lostSpeechTestsPoorRatio             <-  1.0 * ( kpiReport3$maxSpeechTestsPoorRatio           - kpiReport3$ranSpeechTestsPoorRatio           ) * ( kpiReport3$SpeechTestsAttempts       / kpiReport3$ModulePOLQATests ) 
kpiReport3$lostSpeechPolqaPercentile90          <-  1.0 * ( kpiReport3$maxSpeechPolqaPercentile90        - kpiReport3$ranSpeechPolqaPercentile90        ) * ( kpiReport3$SpeechTestsAttempts       / kpiReport3$ModulePOLQATests ) 
kpiReport3$lostHttpFdfsCompletedRatio           <-  1.0 * ( kpiReport3$maxHttpFdfsCompletedRatio         - kpiReport3$ranHttpFdfsCompletedRatio         ) * ( (kpiReport3$HttpFdfsDlAttemptsCount + kpiReport3$HttpFdfsUlAttemptsCount)   / kpiReport3$ModuleHTTPTTests ) 
kpiReport3$lostHttpFdttDlMdrPercentile10        <-  1.0 * ( kpiReport3$maxHttpFdttDlMdrPercentile10      - kpiReport3$ranHttpFdttDlMdrPercentile10      ) * ( (kpiReport3$HttpFdfsDlAttemptsCount + kpiReport3$HttpFdfsUlAttemptsCount)   / kpiReport3$ModuleHTTPTTests ) 
kpiReport3$lostHttpFdttDlMdrAverage             <-  1.0 * ( kpiReport3$maxHttpFdttDlMdrAverage           - kpiReport3$ranHttpFdttDlMdrAverage           ) * ( (kpiReport3$HttpFdfsDlAttemptsCount + kpiReport3$HttpFdfsUlAttemptsCount)   / kpiReport3$ModuleHTTPTTests ) 
kpiReport3$lostHttpFdttDlMdrPercentile90        <-  1.0 * ( kpiReport3$maxHttpFdttDlMdrPercentile90      - kpiReport3$ranHttpFdttDlMdrPercentile90      ) * ( (kpiReport3$HttpFdfsDlAttemptsCount + kpiReport3$HttpFdfsUlAttemptsCount)   / kpiReport3$ModuleHTTPTTests ) 
kpiReport3$lostHttpFdttUlMdrPercentile10        <-  1.0 * ( kpiReport3$maxHttpFdttUlMdrPercentile10      - kpiReport3$ranHttpFdttUlMdrPercentile10      ) * ( (kpiReport3$HttpFdfsDlAttemptsCount + kpiReport3$HttpFdfsUlAttemptsCount)   / kpiReport3$ModuleHTTPTTests ) 
kpiReport3$lostHttpFdttUlMdrAverage             <-  1.0 * ( kpiReport3$maxHttpFdttUlMdrAverage           - kpiReport3$ranHttpFdttUlMdrAverage           ) * ( (kpiReport3$HttpFdfsDlAttemptsCount + kpiReport3$HttpFdfsUlAttemptsCount)   / kpiReport3$ModuleHTTPTTests ) 
kpiReport3$lostHttpFdttUlMdrPercentile90        <-  1.0 * ( kpiReport3$maxHttpFdttUlMdrPercentile90      - kpiReport3$ranHttpFdttUlMdrPercentile90      ) * ( (kpiReport3$HttpFdfsDlAttemptsCount + kpiReport3$HttpFdfsUlAttemptsCount)   / kpiReport3$ModuleHTTPTTests ) 
kpiReport3$lostHttpBrowsingCompletedRatio       <-  1.0 * ( kpiReport3$maxHttpBrowsingCompletedRatio     - kpiReport3$ranHttpBrowsingCompletedRatio     ) * ( kpiReport3$HttpBrowsingAttemptsCount / kpiReport3$ModuleHTTPBTests ) 
kpiReport3$lostHttpBrowsingTransferRttAverage   <-  1.0 * ( kpiReport3$maxHttpBrowsingTransferRttAverage - kpiReport3$ranHttpBrowsingTransferRttAverage ) * ( kpiReport3$HttpBrowsingAttemptsCount / kpiReport3$ModuleHTTPBTests ) 
kpiReport3$lostStreamingCompletedRatio          <-  1.0 * ( kpiReport3$maxStreamingCompletedRatio        - kpiReport3$ranStreamingCompletedRatio        ) * ( kpiReport3$StreamingAttemptsCount    / kpiReport3$ModuleVSLTBTests ) 
kpiReport3$lostStreamingVmosPercentile10        <-  1.0 * ( kpiReport3$maxStreamingVmosPercentile10      - kpiReport3$ranStreamingVmosPercentile10      ) * ( kpiReport3$StreamingAttemptsCount    / kpiReport3$ModuleVSLTBTests ) 
kpiReport3$lostStreamingVmosAverage             <-  1.0 * ( kpiReport3$maxStreamingVmosAverage           - kpiReport3$ranStreamingVmosAverage           ) * ( kpiReport3$StreamingAttemptsCount    / kpiReport3$ModuleVSLTBTests ) 
kpiReport3$lostStreamingTtfpAverage             <-  1.0 * ( kpiReport3$maxStreamingTtfpAverage           - kpiReport3$ranStreamingTtfpAverage           ) * ( kpiReport3$StreamingAttemptsCount    / kpiReport3$ModuleVSLTBTests ) 
kpiReport3$lostStreamingTtfpPoorRatio           <-  1.0 * ( kpiReport3$maxStreamingTtfpPoorRatio         - kpiReport3$ranStreamingTtfpPoorRatio         ) * ( kpiReport3$StreamingAttemptsCount    / kpiReport3$ModuleVSLTBTests ) 
kpiReport3$lostApplicationCompletedRatio        <-  1.0 * ( kpiReport3$maxApplicationCompletedRatio      - kpiReport3$ranApplicationCompletedRatio      ) * ( kpiReport3$ApplicationAttemptsCount  / kpiReport3$ModuleMAFBBTests ) 
kpiReport3$lostApplicationTransferTimeAverage   <-  1.0 * ( kpiReport3$maxApplicationTransferTimeAverage - kpiReport3$ranApplicationTransferTimeAverage ) * ( kpiReport3$ApplicationAttemptsCount  / kpiReport3$ModuleMAFBBTests ) 


# RANKING AGGRGATION VOICE
##########################################
kpiReport1$RnkVoiceL1 <- "RANKING AGGR VOICE L1 >>"
kpiReport2$RnkVoiceL1 <- "RANKING AGGR VOICE L1 >>"
kpiReport3$RnkVoiceL1 <- "RANKING AGGR VOICE L1 >>"

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
kpiReport2$ranCSSR   <-  kpiReport2$ranCallSetupSuccesRatio
kpiReport2$ranDCR    <-  kpiReport2$ranCallDropRatio
kpiReport2$ranCST    <- (kpiReport2$ranCstAverage+kpiReport2$ranCstPoorRatio+kpiReport2$ranCstPercentile10)
kpiReport2$ranPOLQA  <- (kpiReport2$ranSpeechPolqaAverage+kpiReport2$ranSpeechTestsPoorRatio+kpiReport2$ranSpeechPolqaPercentile90)
kpiReport2$maxCSSR   <-  kpiReport2$maxCallSetupSuccesRatio
kpiReport2$maxDCR    <-  kpiReport2$maxCallDropRatio
kpiReport2$maxCST    <- (kpiReport2$maxCstAverage+kpiReport2$maxCstPoorRatio+kpiReport2$maxCstPercentile10)
kpiReport2$maxPOLQA  <- (kpiReport2$maxSpeechPolqaAverage+kpiReport2$maxSpeechTestsPoorRatio+kpiReport2$maxSpeechPolqaPercentile90)
kpiReport2$lostCSSR  <-  kpiReport2$lostCallSetupSuccesRatio
kpiReport2$lostDCR   <-  kpiReport2$lostCallDropRatio
kpiReport2$lostCST   <- (kpiReport2$lostCstAverage+kpiReport2$lostCstPoorRatio+kpiReport2$lostCstPercentile10)
kpiReport2$lostPOLQA <- (kpiReport2$lostSpeechPolqaAverage+kpiReport2$lostSpeechTestsPoorRatio+kpiReport2$lostSpeechPolqaPercentile90)
kpiReport3$ranCSSR   <-  kpiReport3$ranCallSetupSuccesRatio
kpiReport3$ranDCR    <-  kpiReport3$ranCallDropRatio
kpiReport3$ranCST    <- (kpiReport3$ranCstAverage+kpiReport3$ranCstPoorRatio+kpiReport3$ranCstPercentile10)
kpiReport3$ranPOLQA  <- (kpiReport3$ranSpeechPolqaAverage+kpiReport3$ranSpeechTestsPoorRatio+kpiReport3$ranSpeechPolqaPercentile90)
kpiReport3$maxCSSR   <-  kpiReport3$maxCallSetupSuccesRatio
kpiReport3$maxDCR    <-  kpiReport3$maxCallDropRatio
kpiReport3$maxCST    <- (kpiReport3$maxCstAverage+kpiReport3$maxCstPoorRatio+kpiReport3$maxCstPercentile10)
kpiReport3$maxPOLQA  <- (kpiReport3$maxSpeechPolqaAverage+kpiReport3$maxSpeechTestsPoorRatio+kpiReport3$maxSpeechPolqaPercentile90)
kpiReport3$lostCSSR  <-  kpiReport3$lostCallSetupSuccesRatio
kpiReport3$lostDCR   <-  kpiReport3$lostCallDropRatio
kpiReport3$lostCST   <- (kpiReport3$lostCstAverage+kpiReport3$lostCstPoorRatio+kpiReport3$lostCstPercentile10)
kpiReport3$lostPOLQA <- (kpiReport3$lostSpeechPolqaAverage+kpiReport3$lostSpeechTestsPoorRatio+kpiReport3$lostSpeechPolqaPercentile90)

kpiReport1$RnkVoice <- "RANKING AGGR VOICE >>"
kpiReport2$RnkVoice <- "RANKING AGGR VOICE >>"
kpiReport3$RnkVoice <- "RANKING AGGR VOICE >>"

kpiReport1$ranVoice <- (kpiReport1$ranCallSetupSuccesRatio+kpiReport1$ranCallDropRatio+kpiReport1$ranCstAverage+kpiReport1$ranCstPoorRatio+kpiReport1$ranCstPercentile10+kpiReport1$ranSpeechPolqaAverage+kpiReport1$ranSpeechTestsPoorRatio+kpiReport1$ranSpeechPolqaPercentile90)
kpiReport1$maxVoice <- (kpiReport1$maxCallSetupSuccesRatio+kpiReport1$maxCallDropRatio+kpiReport1$maxCstAverage+kpiReport1$maxCstPoorRatio+kpiReport1$maxCstPercentile10+kpiReport1$maxSpeechPolqaAverage+kpiReport1$maxSpeechTestsPoorRatio+kpiReport1$maxSpeechPolqaPercentile90)
kpiReport1$lostVoice <-(kpiReport1$lostCallSetupSuccesRatio+kpiReport1$lostCallDropRatio+kpiReport1$lostCstAverage+kpiReport1$lostCstPoorRatio+kpiReport1$lostCstPercentile10+kpiReport1$lostSpeechPolqaAverage+kpiReport1$lostSpeechTestsPoorRatio+kpiReport1$lostSpeechPolqaPercentile90)
kpiReport2$ranVoice <- (kpiReport2$ranCallSetupSuccesRatio+kpiReport2$ranCallDropRatio+kpiReport2$ranCstAverage+kpiReport2$ranCstPoorRatio+kpiReport2$ranCstPercentile10+kpiReport2$ranSpeechPolqaAverage+kpiReport2$ranSpeechTestsPoorRatio+kpiReport2$ranSpeechPolqaPercentile90)
kpiReport2$maxVoice <- (kpiReport2$maxCallSetupSuccesRatio+kpiReport2$maxCallDropRatio+kpiReport2$maxCstAverage+kpiReport2$maxCstPoorRatio+kpiReport2$maxCstPercentile10+kpiReport2$maxSpeechPolqaAverage+kpiReport2$maxSpeechTestsPoorRatio+kpiReport2$maxSpeechPolqaPercentile90)
kpiReport2$lostVoice <-(kpiReport2$lostCallSetupSuccesRatio+kpiReport2$lostCallDropRatio+kpiReport2$lostCstAverage+kpiReport2$lostCstPoorRatio+kpiReport2$lostCstPercentile10+kpiReport2$lostSpeechPolqaAverage+kpiReport2$lostSpeechTestsPoorRatio+kpiReport2$lostSpeechPolqaPercentile90)
kpiReport3$ranVoice <- (kpiReport3$ranCallSetupSuccesRatio+kpiReport3$ranCallDropRatio+kpiReport3$ranCstAverage+kpiReport3$ranCstPoorRatio+kpiReport3$ranCstPercentile10+kpiReport3$ranSpeechPolqaAverage+kpiReport3$ranSpeechTestsPoorRatio+kpiReport3$ranSpeechPolqaPercentile90)
kpiReport3$maxVoice <- (kpiReport3$maxCallSetupSuccesRatio+kpiReport3$maxCallDropRatio+kpiReport3$maxCstAverage+kpiReport3$maxCstPoorRatio+kpiReport3$maxCstPercentile10+kpiReport3$maxSpeechPolqaAverage+kpiReport3$maxSpeechTestsPoorRatio+kpiReport3$maxSpeechPolqaPercentile90)
kpiReport3$lostVoice <-(kpiReport3$lostCallSetupSuccesRatio+kpiReport3$lostCallDropRatio+kpiReport3$lostCstAverage+kpiReport3$lostCstPoorRatio+kpiReport3$lostCstPercentile10+kpiReport3$lostSpeechPolqaAverage+kpiReport3$lostSpeechTestsPoorRatio+kpiReport3$lostSpeechPolqaPercentile90)


# RANKING AGGRGATION DATA
##########################################
kpiReport1$RnkDataL1 <- "RANKING AGGR DATA L1 >>"
kpiReport2$RnkDataL1 <- "RANKING AGGR DATA L1 >>"
kpiReport3$RnkDataL1 <- "RANKING AGGR DATA L1 >>"

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

kpiReport2$ranHttpT  <- (kpiReport2$ranHttpFdfsCompletedRatio+kpiReport2$ranHttpFdttDlMdrPercentile10+kpiReport2$ranHttpFdttDlMdrAverage+kpiReport2$ranHttpFdttDlMdrPercentile90+kpiReport2$ranHttpFdttUlMdrPercentile10+kpiReport2$ranHttpFdttUlMdrAverage+kpiReport2$ranHttpFdttUlMdrPercentile90)
kpiReport2$ranHttpB  <- (kpiReport2$ranHttpBrowsingCompletedRatio+kpiReport2$ranHttpBrowsingTransferRttAverage)
kpiReport2$ranVSLVS  <- (kpiReport2$ranStreamingCompletedRatio+kpiReport2$ranStreamingVmosPercentile10+kpiReport2$ranStreamingVmosAverage+kpiReport2$ranStreamingTtfpAverage+kpiReport2$ranStreamingTtfpPoorRatio)
kpiReport2$ranAPPFB  <- (kpiReport2$ranApplicationCompletedRatio+kpiReport2$ranApplicationTransferTimeAverage)
kpiReport2$maxHttpT  <- (kpiReport2$maxHttpFdfsCompletedRatio+kpiReport2$maxHttpFdttDlMdrPercentile10+kpiReport2$maxHttpFdttDlMdrAverage+kpiReport2$maxHttpFdttDlMdrPercentile90+kpiReport2$maxHttpFdttUlMdrPercentile10+kpiReport2$maxHttpFdttUlMdrAverage+kpiReport2$maxHttpFdttUlMdrPercentile90)
kpiReport2$maxHttpB  <- (kpiReport2$maxHttpBrowsingCompletedRatio+kpiReport2$maxHttpBrowsingTransferRttAverage)
kpiReport2$maxVSLVS  <- (kpiReport2$maxStreamingCompletedRatio+kpiReport2$maxStreamingVmosPercentile10+kpiReport2$maxStreamingVmosAverage+kpiReport2$maxStreamingTtfpAverage+kpiReport2$maxStreamingTtfpPoorRatio)
kpiReport2$maxAPPFB  <- (kpiReport2$maxApplicationCompletedRatio+kpiReport2$maxApplicationTransferTimeAverage)
kpiReport2$lostHttpT  <- (kpiReport2$lostHttpFdfsCompletedRatio+kpiReport2$lostHttpFdttDlMdrPercentile10+kpiReport2$lostHttpFdttDlMdrAverage+kpiReport2$lostHttpFdttDlMdrPercentile90+kpiReport2$lostHttpFdttUlMdrPercentile10+kpiReport2$lostHttpFdttUlMdrAverage+kpiReport2$lostHttpFdttUlMdrPercentile90)
kpiReport2$lostHttpB  <- (kpiReport2$lostHttpBrowsingCompletedRatio+kpiReport2$lostHttpBrowsingTransferRttAverage)
kpiReport2$lostVSLVS  <- (kpiReport2$lostStreamingCompletedRatio+kpiReport2$lostStreamingVmosPercentile10+kpiReport2$lostStreamingVmosAverage+kpiReport2$lostStreamingTtfpAverage+kpiReport2$lostStreamingTtfpPoorRatio)
kpiReport2$lostAPPFB  <- (kpiReport2$lostApplicationCompletedRatio+kpiReport2$lostApplicationTransferTimeAverage)

kpiReport3$ranHttpT  <- (kpiReport3$ranHttpFdfsCompletedRatio+kpiReport3$ranHttpFdttDlMdrPercentile10+kpiReport3$ranHttpFdttDlMdrAverage+kpiReport3$ranHttpFdttDlMdrPercentile90+kpiReport3$ranHttpFdttUlMdrPercentile10+kpiReport3$ranHttpFdttUlMdrAverage+kpiReport3$ranHttpFdttUlMdrPercentile90)
kpiReport3$ranHttpB  <- (kpiReport3$ranHttpBrowsingCompletedRatio+kpiReport3$ranHttpBrowsingTransferRttAverage)
kpiReport3$ranVSLVS  <- (kpiReport3$ranStreamingCompletedRatio+kpiReport3$ranStreamingVmosPercentile10+kpiReport3$ranStreamingVmosAverage+kpiReport3$ranStreamingTtfpAverage+kpiReport3$ranStreamingTtfpPoorRatio)
kpiReport3$ranAPPFB  <- (kpiReport3$ranApplicationCompletedRatio+kpiReport3$ranApplicationTransferTimeAverage)
kpiReport3$maxHttpT  <- (kpiReport3$maxHttpFdfsCompletedRatio+kpiReport3$maxHttpFdttDlMdrPercentile10+kpiReport3$maxHttpFdttDlMdrAverage+kpiReport3$maxHttpFdttDlMdrPercentile90+kpiReport3$maxHttpFdttUlMdrPercentile10+kpiReport3$maxHttpFdttUlMdrAverage+kpiReport3$maxHttpFdttUlMdrPercentile90)
kpiReport3$maxHttpB  <- (kpiReport3$maxHttpBrowsingCompletedRatio+kpiReport3$maxHttpBrowsingTransferRttAverage)
kpiReport3$maxVSLVS  <- (kpiReport3$maxStreamingCompletedRatio+kpiReport3$maxStreamingVmosPercentile10+kpiReport3$maxStreamingVmosAverage+kpiReport3$maxStreamingTtfpAverage+kpiReport3$maxStreamingTtfpPoorRatio)
kpiReport3$maxAPPFB  <- (kpiReport3$maxApplicationCompletedRatio+kpiReport3$maxApplicationTransferTimeAverage)
kpiReport3$lostHttpT  <- (kpiReport3$lostHttpFdfsCompletedRatio+kpiReport3$lostHttpFdttDlMdrPercentile10+kpiReport3$lostHttpFdttDlMdrAverage+kpiReport3$lostHttpFdttDlMdrPercentile90+kpiReport3$lostHttpFdttUlMdrPercentile10+kpiReport3$lostHttpFdttUlMdrAverage+kpiReport3$lostHttpFdttUlMdrPercentile90)
kpiReport3$lostHttpB  <- (kpiReport3$lostHttpBrowsingCompletedRatio+kpiReport3$lostHttpBrowsingTransferRttAverage)
kpiReport3$lostVSLVS  <- (kpiReport3$lostStreamingCompletedRatio+kpiReport3$lostStreamingVmosPercentile10+kpiReport3$lostStreamingVmosAverage+kpiReport3$lostStreamingTtfpAverage+kpiReport3$lostStreamingTtfpPoorRatio)
kpiReport3$lostAPPFB  <- (kpiReport3$lostApplicationCompletedRatio+kpiReport3$lostApplicationTransferTimeAverage)

kpiReport1$RnkData <- "RANKING AGGR DATA >>"
kpiReport2$RnkData <- "RANKING AGGR DATA >>"
kpiReport3$RnkData <- "RANKING AGGR DATA >>"

kpiReport1$ranData  <- (kpiReport1$ranHttpFdfsCompletedRatio+kpiReport1$ranHttpFdttDlMdrPercentile10+kpiReport1$ranHttpFdttDlMdrAverage+kpiReport1$ranHttpFdttDlMdrPercentile90+kpiReport1$ranHttpFdttUlMdrPercentile10+kpiReport1$ranHttpFdttUlMdrAverage+kpiReport1$ranHttpFdttUlMdrPercentile90+kpiReport1$ranHttpBrowsingCompletedRatio+kpiReport1$ranHttpBrowsingTransferRttAverage+kpiReport1$ranStreamingCompletedRatio+kpiReport1$ranStreamingVmosPercentile10+kpiReport1$ranStreamingVmosAverage+kpiReport1$ranStreamingTtfpAverage+kpiReport1$ranStreamingTtfpPoorRatio+kpiReport1$ranApplicationCompletedRatio+kpiReport1$ranApplicationTransferTimeAverage)
kpiReport1$maxData  <- (kpiReport1$maxHttpFdfsCompletedRatio+kpiReport1$maxHttpFdttDlMdrPercentile10+kpiReport1$maxHttpFdttDlMdrAverage+kpiReport1$maxHttpFdttDlMdrPercentile90+kpiReport1$maxHttpFdttUlMdrPercentile10+kpiReport1$maxHttpFdttUlMdrAverage+kpiReport1$maxHttpFdttUlMdrPercentile90+kpiReport1$maxHttpBrowsingCompletedRatio+kpiReport1$maxHttpBrowsingTransferRttAverage+kpiReport1$maxStreamingCompletedRatio+kpiReport1$maxStreamingVmosPercentile10+kpiReport1$maxStreamingVmosAverage+kpiReport1$maxStreamingTtfpAverage+kpiReport1$maxStreamingTtfpPoorRatio+kpiReport1$maxApplicationCompletedRatio+kpiReport1$maxApplicationTransferTimeAverage)
kpiReport1$lostData  <- (kpiReport1$lostHttpFdfsCompletedRatio+kpiReport1$lostHttpFdttDlMdrPercentile10+kpiReport1$lostHttpFdttDlMdrAverage+kpiReport1$lostHttpFdttDlMdrPercentile90+kpiReport1$lostHttpFdttUlMdrPercentile10+kpiReport1$lostHttpFdttUlMdrAverage+kpiReport1$lostHttpFdttUlMdrPercentile90+kpiReport1$lostHttpBrowsingCompletedRatio+kpiReport1$lostHttpBrowsingTransferRttAverage+kpiReport1$lostStreamingCompletedRatio+kpiReport1$lostStreamingVmosPercentile10+kpiReport1$lostStreamingVmosAverage+kpiReport1$lostStreamingTtfpAverage+kpiReport1$lostStreamingTtfpPoorRatio+kpiReport1$lostApplicationCompletedRatio+kpiReport1$lostApplicationTransferTimeAverage)
kpiReport2$ranData  <- (kpiReport2$ranHttpFdfsCompletedRatio+kpiReport2$ranHttpFdttDlMdrPercentile10+kpiReport2$ranHttpFdttDlMdrAverage+kpiReport2$ranHttpFdttDlMdrPercentile90+kpiReport2$ranHttpFdttUlMdrPercentile10+kpiReport2$ranHttpFdttUlMdrAverage+kpiReport2$ranHttpFdttUlMdrPercentile90+kpiReport2$ranHttpBrowsingCompletedRatio+kpiReport2$ranHttpBrowsingTransferRttAverage+kpiReport2$ranStreamingCompletedRatio+kpiReport2$ranStreamingVmosPercentile10+kpiReport2$ranStreamingVmosAverage+kpiReport2$ranStreamingTtfpAverage+kpiReport2$ranStreamingTtfpPoorRatio+kpiReport2$ranApplicationCompletedRatio+kpiReport2$ranApplicationTransferTimeAverage)
kpiReport2$maxData  <- (kpiReport2$maxHttpFdfsCompletedRatio+kpiReport2$maxHttpFdttDlMdrPercentile10+kpiReport2$maxHttpFdttDlMdrAverage+kpiReport2$maxHttpFdttDlMdrPercentile90+kpiReport2$maxHttpFdttUlMdrPercentile10+kpiReport2$maxHttpFdttUlMdrAverage+kpiReport2$maxHttpFdttUlMdrPercentile90+kpiReport2$maxHttpBrowsingCompletedRatio+kpiReport2$maxHttpBrowsingTransferRttAverage+kpiReport2$maxStreamingCompletedRatio+kpiReport2$maxStreamingVmosPercentile10+kpiReport2$maxStreamingVmosAverage+kpiReport2$maxStreamingTtfpAverage+kpiReport2$maxStreamingTtfpPoorRatio+kpiReport2$maxApplicationCompletedRatio+kpiReport2$maxApplicationTransferTimeAverage)
kpiReport2$lostData  <- (kpiReport2$lostHttpFdfsCompletedRatio+kpiReport2$lostHttpFdttDlMdrPercentile10+kpiReport2$lostHttpFdttDlMdrAverage+kpiReport2$lostHttpFdttDlMdrPercentile90+kpiReport2$lostHttpFdttUlMdrPercentile10+kpiReport2$lostHttpFdttUlMdrAverage+kpiReport2$lostHttpFdttUlMdrPercentile90+kpiReport2$lostHttpBrowsingCompletedRatio+kpiReport2$lostHttpBrowsingTransferRttAverage+kpiReport2$lostStreamingCompletedRatio+kpiReport2$lostStreamingVmosPercentile10+kpiReport2$lostStreamingVmosAverage+kpiReport2$lostStreamingTtfpAverage+kpiReport2$lostStreamingTtfpPoorRatio+kpiReport2$lostApplicationCompletedRatio+kpiReport2$lostApplicationTransferTimeAverage)
kpiReport3$ranData  <- (kpiReport3$ranHttpFdfsCompletedRatio+kpiReport3$ranHttpFdttDlMdrPercentile10+kpiReport3$ranHttpFdttDlMdrAverage+kpiReport3$ranHttpFdttDlMdrPercentile90+kpiReport3$ranHttpFdttUlMdrPercentile10+kpiReport3$ranHttpFdttUlMdrAverage+kpiReport3$ranHttpFdttUlMdrPercentile90+kpiReport3$ranHttpBrowsingCompletedRatio+kpiReport3$ranHttpBrowsingTransferRttAverage+kpiReport3$ranStreamingCompletedRatio+kpiReport3$ranStreamingVmosPercentile10+kpiReport3$ranStreamingVmosAverage+kpiReport3$ranStreamingTtfpAverage+kpiReport3$ranStreamingTtfpPoorRatio+kpiReport3$ranApplicationCompletedRatio+kpiReport3$ranApplicationTransferTimeAverage)
kpiReport3$maxData  <- (kpiReport3$maxHttpFdfsCompletedRatio+kpiReport3$maxHttpFdttDlMdrPercentile10+kpiReport3$maxHttpFdttDlMdrAverage+kpiReport3$maxHttpFdttDlMdrPercentile90+kpiReport3$maxHttpFdttUlMdrPercentile10+kpiReport3$maxHttpFdttUlMdrAverage+kpiReport3$maxHttpFdttUlMdrPercentile90+kpiReport3$maxHttpBrowsingCompletedRatio+kpiReport3$maxHttpBrowsingTransferRttAverage+kpiReport3$maxStreamingCompletedRatio+kpiReport3$maxStreamingVmosPercentile10+kpiReport3$maxStreamingVmosAverage+kpiReport3$maxStreamingTtfpAverage+kpiReport3$maxStreamingTtfpPoorRatio+kpiReport3$maxApplicationCompletedRatio+kpiReport3$maxApplicationTransferTimeAverage)
kpiReport3$lostData  <- (kpiReport3$lostHttpFdfsCompletedRatio+kpiReport3$lostHttpFdttDlMdrPercentile10+kpiReport3$lostHttpFdttDlMdrAverage+kpiReport3$lostHttpFdttDlMdrPercentile90+kpiReport3$lostHttpFdttUlMdrPercentile10+kpiReport3$lostHttpFdttUlMdrAverage+kpiReport3$lostHttpFdttUlMdrPercentile90+kpiReport3$lostHttpBrowsingCompletedRatio+kpiReport3$lostHttpBrowsingTransferRttAverage+kpiReport3$lostStreamingCompletedRatio+kpiReport3$lostStreamingVmosPercentile10+kpiReport3$lostStreamingVmosAverage+kpiReport3$lostStreamingTtfpAverage+kpiReport3$lostStreamingTtfpPoorRatio+kpiReport3$lostApplicationCompletedRatio+kpiReport3$lostApplicationTransferTimeAverage)

# RANKING AGGRGATION OVERALL
##########################################
kpiReport1$RANKINGTOTAL <- "RANKING TOTAL >>"
kpiReport2$RANKINGTOTAL <- "RANKING TOTAL >>"
kpiReport3$RANKINGTOTAL <- "RANKING TOTAL >>"

kpiReport1$ranRanking <- kpiReport1$ranData + kpiReport1$ranVoice
kpiReport1$maxRanking <- kpiReport1$maxData + kpiReport1$maxVoice
kpiReport2$ranRanking <- kpiReport2$ranData + kpiReport2$ranVoice
kpiReport2$maxRanking <- kpiReport2$maxData + kpiReport2$maxVoice
kpiReport3$ranRanking <- kpiReport3$ranData + kpiReport3$ranVoice
kpiReport3$maxRanking <- kpiReport3$maxData + kpiReport3$maxVoice
kpiReport1$lostRanking <- kpiReport1$lostData + kpiReport1$lostVoice
kpiReport2$lostRanking <- kpiReport2$lostData + kpiReport2$lostVoice
kpiReport3$lostRanking <- kpiReport3$lostData + kpiReport3$lostVoice

# WRITE TO XLSX
##########################################
fileName <- paste(format(Sys.Date(), "%Y"),
                  format(Sys.Date(), "%m"),
                  format(Sys.Date(), "%d"),
                  format(Sys.time(), "%H%M"),
                  "-Ranking-Report.xls",
                  sep = ""
                )
wb <- loadWorkbook(fileName, create = TRUE)

createSheet ( wb , "Operator1" )
writeWorksheet(wb, 
               kpiReport1, 
               sheet = "Operator1", 
               startRow = 1, 
               startCol = 1, 
               header = TRUE)

createSheet ( wb , "Operator2" )
writeWorksheet(wb, 
               kpiReport2, 
               sheet = "Operator2", 
               startRow = 1, 
               startCol = 1, 
               header = TRUE)
createSheet ( wb , "Operator3" )
writeWorksheet(wb, 
               kpiReport3, 
               sheet = "Operator3", 
               startRow = 1, 
               startCol = 1, 
               header = TRUE)

# Ratios and percents
csDecim <- createCellStyle(wb, name = "decimal")
setDataFormat(csDecim, format = "0.000")
rc = expand.grid(row = 2:2000, col = 12:255);

setCellStyle(wb, sheet = "Operator1", row = rc$row, col = rc$col, cellstyle = csDecim)
setCellStyle(wb, sheet = "Operator2", row = rc$row, col = rc$col, cellstyle = csDecim)
setCellStyle(wb, sheet = "Operator3", row = rc$row, col = rc$col, cellstyle = csDecim)

csCount <- createCellStyle(wb, name = "count")
setDataFormat(csCount, format = "0")
cols <- c(12,13,14,15,25,26,28,34,44,45,46,47,48,52,53,54,55,56,76,77,78,79,80,97,98,99,107,108,116,117,120,121,122,123,135,136,137,138,139,140,141)
for(i in cols)
{
  setCellStyle(wb, sheet = "Operator1", row = 2:2000, col = i, cellstyle = csCount)
  setCellStyle(wb, sheet = "Operator2", row = 2:2000, col = i, cellstyle = csCount)
  setCellStyle(wb, sheet = "Operator3", row = 2:2000, col = i, cellstyle = csCount)
}

# MIN AND MAX TIMESTAMP
csDate <- createCellStyle(wb, name = "date")
setDataFormat(csDate, format = "yyyy-mm-dd")
rc = expand.grid(row = 2:2000, col = 9:10); setCellStyle(wb, sheet = "Operator1", row = rc$row, col = rc$col, cellstyle = csDate)
rc = expand.grid(row = 2:2000, col = 9:10); setCellStyle(wb, sheet = "Operator2", row = rc$row, col = rc$col, cellstyle = csDate)
rc = expand.grid(row = 2:2000, col = 9:10); setCellStyle(wb, sheet = "Operator3", row = rc$row, col = rc$col, cellstyle = csDate)

# Ratios and percents
csPcnt <- createCellStyle(wb, name = "percent")
setDataFormat(csPcnt, format = "0.000%")
cols <- c(16,17,18,27,29,30,31,41,49,50,51,57,58,59,60,81,82,83,100,109,118,124,132)
for(i in cols)
{
  setCellStyle(wb, sheet = "Operator1", row = 2:2000, col = i, cellstyle = csPcnt)
  setCellStyle(wb, sheet = "Operator2", row = 2:2000, col = i, cellstyle = csPcnt)
  setCellStyle(wb, sheet = "Operator3", row = 2:2000, col = i, cellstyle = csPcnt)
}
???
csMark <- createCellStyle(wb, name = "marker")
setFillPattern(csMark, fill = XLC$FILL.SOLID_FOREGROUND)
setFillBackgroundColor(csMark, color = XLC$"COLOR.LEMON_CHIFFON")
setFillForegroundColor(csMark, color = XLC$"COLOR.LEMON_CHIFFON")

cols <- c(11,32,43,61,68,75,96,119,131,135,142,215,228,232,245,249)
for(i in cols)
{
  setCellStyle(wb, sheet = "Operator1", row = 2:2000, col = i, cellstyle = csMark)
  setCellStyle(wb, sheet = "Operator2", row = 2:2000, col = i, cellstyle = csMark)
  setCellStyle(wb, sheet = "Operator3", row = 2:2000, col = i, cellstyle = csMark)
}

setColumnWidth(wb, sheet = "Operator1", column = 1:300, width = -1)
setColumnWidth(wb, sheet = "Operator2", column = 1:300, width = -1)
setColumnWidth(wb, sheet = "Operator3", column = 1:300, width = -1)



#############################################################################################################################
#                                         LAYER 2 RANKING SHEETS                                                            #
#############################################################################################################################

operatorsList <- c("Telekom","Vodafone","Telefonica")

# CREATE NEW SHEET
createSheet(wb, name = "L2Ranking")
# EXTRACT DATA TO PLOT
rnk1 <- subset(kpiReport1, 
               Rnk == 100 | Rnk == 200 | Rnk == 300 | Rnk == 400,
               select = Rnk:lostRanking)
rnk2 <- subset(kpiReport2, 
               Rnk == 100 | Rnk == 200 | Rnk == 300 | Rnk == 400,
               select = Rnk:lostRanking)
rnk3 <- subset(kpiReport3, 
               Rnk == 100 | Rnk == 200 | Rnk == 300 | Rnk == 400,
               select = Rnk:lostRanking)

# ADD BASIC GRAPHS TO A WORKSHEET
############################################################################################################################
createName(wb, name = "L2Ranking", formula = "L2Ranking!$A$1")
file <- png(file="graph1.png",width=1200,height=1600,res=72)
par(mfrow = c(6,3))
plotSimpleGraph(operators = operatorsList, 
                kpi1      = rnk1$ranRanking, 
                kpi2      = rnk2$ranRanking, 
                kpi3      = rnk3$ranRanking, 
                mkpi1     = rnk1$maxRanking, 
                mkpi2     = rnk2$maxRanking, 
                mkpi3     = rnk3$maxRanking, 
                lkpi1     = rnk1$lostRanking, 
                lkpi2     = rnk2$lostRanking, 
                lkpi3     = rnk3$lostRanking,
                nkpi1     = paste(rnk1$G_Level_1,"\n",rnk1$G_Level_2), 
                nkpi2     = paste(rnk2$G_Level_1,"\n",rnk2$G_Level_2), 
                nkpi3     = paste(rnk3$G_Level_1,"\n",rnk3$G_Level_2),
                col1      = c("dodgerblue4", "firebrick4", "red"),
                col2      = c("dodgerblue4", "firebrick4", "red"),
                col3      = c("dodgerblue4", "firebrick4", "red"),
                kpiName   = "Total"
                )   # FUNCTION PLOTS 9 GRAPHS
par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk1$ranVoice, rnk1$ranData )), 
             inputHead   = paste("Total Ranking", operatorsList[1]), 
             inputNam    = paste(rnk1$G_Level_1,"\n",rnk1$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "Yes", 
             inputLabP   = t(cbind( rnk1$ranData + rnk1$ranVoice ) ),
             inputLab    = paste( round( (rnk1$ranData + rnk1$ranVoice ), digits = 2 ),
                                  "of",
                                  (rnk1$maxData + rnk1$maxVoice),
                                  "\n",
                                  round( rnk1$ranVoice , digits = 2 ),
                                  "+",
                                  round( rnk1$ranData , digits = 2 )
             ),
             mi          = max(rnk1$ranData + rnk1$ranVoice) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk2$ranVoice, rnk2$ranData )), 
             inputHead   = paste("Total Ranking", operatorsList[2]), 
             inputNam    = paste(rnk2$G_Level_1,"\n",rnk2$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "Yes", 
             inputLabP   = t(cbind( rnk2$ranData + rnk2$ranVoice ) ),
             inputLab    = paste( round( (rnk2$ranData + rnk2$ranVoice ), digits = 2 ),
                                  "of",
                                  (rnk2$maxData + rnk2$maxVoice),
                                  "\n",
                                  round( rnk2$ranVoice , digits = 2 ),
                                  "+",
                                  round( rnk2$ranData , digits = 2 )
             ),
             mi          = max(rnk2$ranData + rnk2$ranVoice) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk3$ranVoice, rnk3$ranData )), 
             inputHead   = paste("Total Ranking", operatorsList[3]), 
             inputNam    = paste(rnk3$G_Level_1,"\n",rnk3$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "Yes", 
             inputLabP   = t(cbind( rnk3$ranData + rnk3$ranVoice ) ),
             inputLab    = paste( round( (rnk3$ranData + rnk3$ranVoice ), digits = 2 ),
                                  "of",
                                  (rnk3$maxData + rnk3$maxVoice),
                                  "\n",
                                  round( rnk3$ranVoice , digits = 2 ),
                                  "+",
                                  round( rnk3$ranData , digits = 2 )
             ),
             mi          = max(rnk3$ranData + rnk3$ranVoice) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk1$maxVoice - rnk1$ranVoice, rnk1$maxData - rnk1$ranData )), 
             inputHead   = paste("GAP to MAX", operatorsList[1]), 
             inputNam    = paste(rnk1$G_Level_1,"\n",rnk1$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "No", 
             inputLabP   = t(cbind( rnk1$maxVoice - rnk1$ranVoice + rnk1$maxData - rnk1$ranData ) ),
             inputLab    = paste( round( (rnk1$maxVoice - rnk1$ranVoice + rnk1$maxData - rnk1$ranData ), digits = 2 ),
                                  "of",
                                  (rnk1$maxData + rnk1$maxVoice),
                                  "\n",
                                  round( rnk1$maxVoice - rnk1$ranVoice , digits = 2 ),
                                  "+",
                                  round( rnk1$maxData - rnk1$ranData , digits = 2 )
             ),
             mi          = max(rnk1$maxVoice - rnk1$ranVoice + rnk1$maxData - rnk1$ranData) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk2$maxVoice - rnk2$ranVoice, rnk2$maxData - rnk2$ranData )), 
             inputHead   = paste("GAP to MAX", operatorsList[2]), 
             inputNam    = paste(rnk2$G_Level_1,"\n",rnk2$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "No", 
             inputLabP   = t(cbind( rnk2$maxVoice - rnk2$ranVoice + rnk2$maxData - rnk2$ranData ) ),
             inputLab    = paste( round( (rnk2$maxVoice - rnk2$ranVoice + rnk2$maxData - rnk2$ranData ), digits = 2 ),
                                  "of",
                                  (rnk2$maxData + rnk2$maxVoice),
                                  "\n",
                                  round( rnk2$maxVoice - rnk2$ranVoice , digits = 2 ),
                                  "+",
                                  round( rnk2$maxData - rnk2$ranData , digits = 2 )
             ),
             mi          = max(rnk2$maxVoice - rnk2$ranVoice + rnk2$maxData - rnk2$ranData) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk3$maxVoice - rnk3$ranVoice, rnk3$maxData - rnk3$ranData )), 
             inputHead   = paste("GAP to MAX", operatorsList[3]), 
             inputNam    = paste(rnk3$G_Level_1,"\n",rnk3$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "No", 
             inputLabP   = t(cbind( rnk3$maxVoice - rnk3$ranVoice + rnk3$maxData - rnk3$ranData ) ),
             inputLab    = paste( round( (rnk3$maxVoice - rnk3$ranVoice + rnk3$maxData - rnk3$ranData ), digits = 2 ),
                                  "of",
                                  (rnk3$maxData + rnk3$maxVoice),
                                  "\n",
                                  round( rnk3$maxVoice - rnk3$ranVoice , digits = 2 ),
                                  "+",
                                  round( rnk3$maxData - rnk3$ranData , digits = 2 )
             ),
             mi          = max(rnk3$maxVoice - rnk3$ranVoice + rnk3$maxData - rnk3$ranData) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk1$lostVoice, rnk1$lostData )), 
             inputHead   = paste("Lost Points", operatorsList[1]), 
             inputNam    = paste(rnk1$G_Level_1,"\n",rnk1$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "No", 
             inputLabP   = t(cbind( rnk1$lostData + rnk1$lostVoice ) ),
             inputLab    = paste( round( (rnk1$lostData + rnk1$lostVoice ), digits = 2 ),
                                  "of",
                                  (rnk1$maxData + rnk1$maxVoice),
                                  "\n",
                                  round( rnk1$lostVoice , digits = 2 ),
                                  "+",
                                  round( rnk1$lostData , digits = 2 )
             ),
             mi          = max(rnk1$lostData + rnk1$lostVoice) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk2$lostVoice, rnk2$lostData )), 
             inputHead   = paste("Lost Points", operatorsList[2]), 
             inputNam    = paste(rnk2$G_Level_1,"\n",rnk2$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "No", 
             inputLabP   = t(cbind( rnk2$lostData + rnk2$lostVoice ) ),
             inputLab    = paste( round( (rnk2$lostData + rnk2$lostVoice ), digits = 2 ),
                                  "of",
                                  (rnk2$maxData + rnk2$maxVoice),
                                  "\n",
                                  round( rnk2$lostVoice , digits = 2 ),
                                  "+",
                                  round( rnk2$lostData , digits = 2 )
             ),
             mi          = max(rnk2$lostData + rnk2$lostVoice) )

par(mar=c(5,15,5,10)+.1)
plotBarCum1( inputTab    = t(cbind( rnk3$lostVoice, rnk3$lostData )), 
             inputHead   = paste("Lost Points", operatorsList[3]), 
             inputNam    = paste(rnk3$G_Level_1,"\n",rnk3$G_Level_2), 
             inputColo   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
             inputKP     = c("Voice", "Data"), 
             legFlag     = "No", 
             inputLabP   = t(cbind( rnk3$lostData + rnk3$lostVoice ) ),
             inputLab    = paste( round( (rnk3$lostData + rnk3$lostVoice ), digits = 2 ),
                                  "of",
                                  (rnk3$maxData + rnk3$maxVoice),
                                  "\n",
                                  round( rnk3$lostVoice , digits = 2 ),
                                  "+",
                                  round( rnk3$lostData , digits = 2 )
             ),
             mi          = max(rnk3$lostData + rnk3$lostVoice) )
dev.off()
addImage(wb, filename = "graph1.png", name="L2Ranking", originalSize = TRUE)

# ADD VOICE GRAPHS TO A WORKSHEET
############################################################################################################################
createName(wb, name = "L2Ranking1", formula = "L2Ranking!$AA$3")
file <- png(file="graph2.png",width=1200,height=2400,res=72)
par(mfrow = c(9,3))



dev.off()
addImage(wb, filename = "graph2.png", name="L2Ranking1", originalSize = TRUE)


saveWorkbook( wb )
#rm(list=ls())


