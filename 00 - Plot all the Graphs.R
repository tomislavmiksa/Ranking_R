###################################################################################################################################################
#                                                               FUNCTION plot RANKING splitted BARS                                               #
###################################################################################################################################################
plotBar <- function(inputTable, 
                    inputHeader, 
                    inputNames, 
                    inputColors, 
                    inputKPIs, 
                    legendFlag, 
                    inputLabelPosition,
                    inputLabel,
                    maxValue)
{
  if(is.na(maxValue)) {maxValue <- 2}
  bp <- barplot(inputTable,
                main = inputHeader,
                cex.main = 1.5,
                horiz=TRUE, 
                las=1,
                names.arg = inputNames,
                cex.names = 1.5,
                xlab = "",
                ylab = "",
                xaxt='n',
                xlim = c(0,4.0*maxValue),
                col=inputColors,
                width = 1)
  if(legendFlag == "Yes")
  {
    legend("topright",
           legend = inputKPIs,
           fill=inputColors,
           cex = 1.5,
           title = "")
  }
  text(x=inputLabelPosition, 
       y=bp, 
       labels=inputLabel,
       cex=1.2,
       pos=4, 
       xpd=NA)
}

###################################################################################################################################################
#                                                               FUNCTION plot RANKING                                                             #
###################################################################################################################################################
rnkGeneral <- function(rnk, level, fn)
{
  if (nrow(rnk) < 5) {
    heightFile <- nrow(rnk)*300
  } else {
    heightFile <- nrow(rnk)*200
  }
  file <- png(file=fn,width=1200,height=heightFile,res=72)
  par(mfrow = c(2,3))
  
  if (level == 2) {
    names <- paste(rnk$G_Level_1,rnk$G_Level_2)
  }
  if (level == 3) {
    names <- paste(rnk$G_Level_1,rnk$G_Level_2,"\n",rnk$G_Level_3)
  }
  if (level == 4) {
    names <- paste(rnk$G_Level_3,"\n",rnk$G_Level_4)
  }
  
  # OVERALL RANKING
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$ranRanking, 
          inputHeader           = "Total Ranking", 
          inputNames            = names, 
          inputColors           = c("dodgerblue4"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranRanking,
          inputLabel            = round(rnk$ranRanking,digits = 2),
          maxValue              = max(rnk$ranRanking) )

  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxRanking - rnk$ranRanking), 
          inputHeader           = "Total Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("red"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxRanking - rnk$ranRanking),
          inputLabel            = round((rnk$maxRanking - rnk$ranRanking),digits = 2),
          maxValue              = max(rnk$maxRanking - rnk$ranRanking) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostRanking, 
          inputHeader           = "Total Points Lost", 
          inputNames            = names, 
          inputColors           = c("red"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostRanking,
          inputLabel            = round(rnk$lostRanking,digits = 2),
          maxValue              = max(rnk$lostRanking) )

  # OVERALL RANKING
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranVoice, rnk$ranData )), 
           inputHeader   = "Total Ranking", 
           inputNames    = names,
           inputColors   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
           inputKPIs     = c("Voice", "Data"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranData + rnk$ranVoice ) ),
           inputLabel    = paste( round( (rnk$ranData + rnk$ranVoice ), digits = 2 ),
                                  "of",
                                  (rnk$maxData + rnk$maxVoice),
                                  "\n",
                                  round( rnk$ranVoice , digits = 2 ),
                                  "+",
                                  round( rnk$ranData , digits = 2 )
           ),
           maxValue          = max(rnk$ranData + rnk$ranVoice) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxVoice - rnk$ranVoice), (rnk$maxData - rnk$ranData) )), 
           inputHeader   = "Total Gap to MAX", 
           inputNames    = names,
           inputColors   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
           inputKPIs     = c("Voice", "Data"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxData - rnk$ranData) + (rnk$maxVoice - rnk$ranVoice) ) ),
           inputLabel    = paste( round( ( (rnk$maxData - rnk$ranData) + (rnk$maxVoice - rnk$ranVoice) ), digits = 2 ),
                                  "of",
                                  (rnk$maxData + rnk$maxVoice),
                                  "\n",
                                  round( (rnk$maxVoice - rnk$ranVoice) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxData - rnk$ranData) , digits = 2 )
           ),
           maxValue          = max((rnk$maxData - rnk$ranData) + (rnk$maxVoice - rnk$ranVoice)) )

  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostVoice, rnk$lostData )), 
           inputHeader   = "Total Points Lost", 
           inputNames    = names,
           inputColors   = c( rgb(148, 171,  33, maxColorValue=255),
                              rgb( 78, 166, 220, maxColorValue=255) ), 
           inputKPIs     = c("Voice", "Data"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostData + rnk$lostVoice ) ),
           inputLabel    = paste( round( (rnk$lostData + rnk$lostVoice ), digits = 2 ),
                                  "of",
                                  (rnk$maxData + rnk$maxVoice),
                                  "\n",
                                  round( rnk$lostVoice , digits = 2 ),
                                  "+",
                                  round( rnk$lostData , digits = 2 )
           ),
           maxValue          = max(rnk$lostData + rnk$lostVoice) )
  
  dev.off()
}

###################################################################################################################################################
#                                                               FUNCTION plot VOICE                                                               #
###################################################################################################################################################

rnkVoice <- function(rnk, level, fn)
{
  if (nrow(rnk) < 5) {
    heightFile <- nrow(rnk)*800
  } else {
    heightFile <- nrow(rnk)*600
  }
  file <- png(file=fn,width=1600,height=heightFile,res=72)
  layout(matrix(c(1,1,2,3,
                  4,4,5,6,
                  7,7,8,9,
                  10,10,11,12,
                  13,13,14,15,
                  16,16,17,18,
                  19,19,20,21,
                  22,22,23,24,
                  25,25,26,27,
                  28,28,29,30,
                  31,31,32,33,
                  34,34,35,36), 
                nrow=12, 
                ncol=4, 
                byrow = TRUE), 
         widths=c(1,1,1,1,1,1,1,1,1,1), 
         heights=c(1,1,1,1,1,1,1,1,1,1))
  
  if (level == 2) {
    names <- paste(rnk$G_Level_1,rnk$G_Level_2)
  }
  if (level == 3) {
    names <- paste(rnk$G_Level_1,rnk$G_Level_2,"\n",rnk$G_Level_3)
  }
  if (level == 4) {
    names <- paste(rnk$G_Level_3,"\n",rnk$G_Level_4)
  }
  
  # OVERALL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranVoice, 
          inputHeader           = "Total Voice", 
          inputNames            = names, 
          inputColors           = c(rgb(148, 171,  33, maxColorValue=255)), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranVoice,
          inputLabel            = paste(round(rnk$ranVoice,digits = 2), "of", rnk$maxVoice), 
          maxValue              = max(rnk$ranVoice) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxVoice - rnk$ranVoice), 
          inputHeader           = "Voice Gap to MAX", 
          inputNames            = names, 
          inputColors           = c(rgb(148, 171,  33, maxColorValue=255)), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxVoice - rnk$ranVoice),
          inputLabel            = paste(round((rnk$maxVoice - rnk$ranVoice),digits = 2), "of", rnk$maxVoice), 
          maxValue              = max(rnk$maxVoice - rnk$ranVoice) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostVoice, 
          inputHeader           = "Voice Points Lost", 
          inputNames            = names, 
          inputColors           = c(rgb(148, 171,  33, maxColorValue=255)), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostVoice,
          inputLabel            = paste(round(rnk$lostVoice,digits = 2), "of", rnk$maxVoice), 
          maxValue              = max(rnk$lostVoice) )
  
  # OVERALL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranCSSR, rnk$ranDCR, rnk$ranCST, rnk$ranPOLQA )), 
           inputHeader   = "Voice Ranking", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "darkorchid1", "dodgerblue1", "darkseagreen1"), 
           inputKPIs     = c("CSSR", "DCR", "CST", "LQ"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranDCR + rnk$ranCSSR + rnk$ranCST + rnk$ranPOLQA ) ),
           inputLabel    = paste( round( ( rnk$ranDCR + rnk$ranCSSR + rnk$ranCST + rnk$ranPOLQA ), digits = 2 ),
                                  "of",
                                  (rnk$maxDCR + rnk$maxCSSR + rnk$maxCST + rnk$maxPOLQA ),
                                  "\n",
                                  round( rnk$ranCSSR , digits = 2 ),
                                  "+",
                                  round( rnk$ranDCR , digits = 2 ),
                                  "+",
                                  round( rnk$ranCST , digits = 2 ),
                                  "+",
                                  round( rnk$ranPOLQA , digits = 2 )
           ),
           maxValue          = max(rnk$ranDCR + rnk$ranCSSR + rnk$ranCST + rnk$ranPOLQA) )

  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxCSSR - rnk$ranCSSR), (rnk$maxDCR - rnk$ranDCR), (rnk$maxCST - rnk$ranCST), (rnk$maxPOLQA - rnk$ranPOLQA) )), 
           inputHeader   = "Voice Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "darkorchid1", "dodgerblue1", "darkseagreen1"), 
           inputKPIs     = c("CSSR", "DCR", "CST", "LQ"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxDCR - rnk$ranDCR) + (rnk$maxCSSR - rnk$ranCSSR) + (rnk$maxCST - rnk$ranCST) + (rnk$maxPOLQA - rnk$ranPOLQA) ) ),
           inputLabel    = paste( round( ( (rnk$maxDCR - rnk$ranDCR) + (rnk$maxCSSR - rnk$ranCSSR) + (rnk$maxCST - rnk$ranCST) + (rnk$maxPOLQA - rnk$ranPOLQA) ), digits = 2 ),
                                  "of",
                                  (rnk$maxDCR + rnk$maxCSSR + rnk$maxCST + rnk$maxPOLQA ),
                                  "\n",
                                  round( (rnk$maxCSSR - rnk$ranCSSR) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxDCR - rnk$ranDCR) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxCST - rnk$ranCST) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxPOLQA - rnk$ranPOLQA) , digits = 2 )
           ),
           maxValue          = max((rnk$maxDCR - rnk$ranDCR) + (rnk$maxCSSR - rnk$ranCSSR) + (rnk$maxCST - rnk$ranCST) + (rnk$maxPOLQA - rnk$ranPOLQA)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostCSSR, rnk$lostDCR, rnk$lostCST, rnk$lostPOLQA )), 
           inputHeader   = "Voice Points Lost", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "darkorchid1", "dodgerblue1", "darkseagreen1"), 
           inputKPIs     = c("CSSR", "DCR", "CST", "LQ"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostDCR + rnk$lostCSSR + rnk$lostCST + rnk$lostPOLQA ) ),
           inputLabel    = paste( round( ( rnk$lostDCR + rnk$lostCSSR + rnk$lostCST + rnk$lostPOLQA ), digits = 2 ),
                                  "of",
                                  (rnk$maxDCR + rnk$maxCSSR + rnk$maxCST + rnk$maxPOLQA ),
                                  "\n",
                                  round( rnk$lostCSSR , digits = 2 ),
                                  "+",
                                  round( rnk$lostDCR , digits = 2 ),
                                  "+",
                                  round( rnk$lostCST , digits = 2 ),
                                  "+",
                                  round( rnk$lostPOLQA , digits = 2 )
           ),
           maxValue          = max(rnk$lostDCR + rnk$lostCSSR + rnk$lostCST + rnk$lostPOLQA) )

  # CSSR RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranCSSR, 
          inputHeader           = "Ranking CSSR", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranCSSR,
          inputLabel            = paste( round(rnk$ranCSSR,digits = 2), "of", round(rnk$maxCSSR,digits = 2) ),
          maxValue              = max(rnk$ranCSSR) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxCSSR - rnk$ranCSSR), 
          inputHeader           = "CSSR Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxCSSR - rnk$ranCSSR),
          inputLabel            = paste( round(rnk$maxCSSR - rnk$ranCSSR,digits = 2), "of", round(rnk$maxCSSR,digits = 2) ),
          maxValue              = max(rnk$maxCSSR - rnk$ranCSSR) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostCSSR, 
          inputHeader           = "CSSR Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostCSSR,
          inputLabel            = paste( round(rnk$lostCSSR,digits = 2), "of", round(rnk$maxCSSR,digits = 2) ),
          maxValue              = max(rnk$lostCSSR) )
  
  # DCR RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranDCR, 
          inputHeader           = "Ranking DCR", 
          inputNames            = names, 
          inputColors           = c("darkorchid1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranDCR,
          inputLabel            = paste( round(rnk$ranDCR,digits = 2), "of", round(rnk$maxDCR,digits = 2) ),
          maxValue              = max(rnk$ranDCR) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxDCR - rnk$ranDCR), 
          inputHeader           = "DCR Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkorchid1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxDCR - rnk$ranDCR),
          inputLabel            = paste( round(rnk$maxDCR - rnk$ranDCR,digits = 2), "of", round(rnk$maxDCR,digits = 2) ),
          maxValue              = max(rnk$maxDCR - rnk$ranDCR) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostDCR, 
          inputHeader           = "DCR Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkorchid1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostDCR,
          inputLabel            = paste( round(rnk$lostDCR,digits = 2), "of", round(rnk$maxDCR,digits = 2) ),
          maxValue              = max(rnk$lostDCR) )
  
  # OVERALL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranCstAverage, rnk$ranCstPoorRatio, rnk$ranCstPercentile10 )), 
           inputHeader   = "Call Setup Time Ranking", 
           inputNames    = names,
           inputColors   = c( "dodgerblue1" , "dodgerblue2", "dodgerblue3"), 
           inputKPIs     = c("CST AVG", "CST > 15s", "CST P10"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranCstPoorRatio + rnk$ranCstAverage + rnk$ranCstPercentile10 ) ),
           inputLabel    = paste( round( ( rnk$ranCstPoorRatio + rnk$ranCstAverage + rnk$ranCstPercentile10 ), digits = 2 ),
                                  "of",
                                  (rnk$maxCstPoorRatio + rnk$maxCstAverage + rnk$maxCstPercentile10 ),
                                  "\n",
                                  round( rnk$ranCstAverage , digits = 2 ),
                                  "+",
                                  round( rnk$ranCstPoorRatio , digits = 2 ),
                                  "+",
                                  round( rnk$ranCstPercentile10 , digits = 2 )
           ),
           maxValue          = max(rnk$ranCstPoorRatio + rnk$ranCstAverage + rnk$ranCstPercentile10) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxCstAverage - rnk$ranCstAverage), (rnk$maxCstPoorRatio - rnk$ranCstPoorRatio), (rnk$maxCstPercentile10 - rnk$ranCstPercentile10) )), 
           inputHeader   = "Call Setup Time Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "dodgerblue1" , "dodgerblue2", "dodgerblue3"), 
           inputKPIs     = c("CST AVG", "CST > 15s", "CST P10"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxCstPoorRatio - rnk$ranCstPoorRatio) + (rnk$maxCstAverage - rnk$ranCstAverage) + (rnk$maxCstPercentile10 - rnk$ranCstPercentile10) ) ),
           inputLabel    = paste( round( ( (rnk$maxCstPoorRatio - rnk$ranCstPoorRatio) + (rnk$maxCstAverage - rnk$ranCstAverage) + (rnk$maxCstPercentile10 - rnk$ranCstPercentile10) ), digits = 2 ),
                                  "of",
                                  (rnk$maxCstPoorRatio + rnk$maxCstAverage + rnk$maxCstPercentile10 ),
                                  "\n",
                                  round( (rnk$maxCstAverage - rnk$ranCstAverage) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxCstPoorRatio - rnk$ranCstPoorRatio) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxCstPercentile10 - rnk$ranCstPercentile10) , digits = 2 )
           ),
           maxValue          = max((rnk$maxCstPoorRatio - rnk$ranCstPoorRatio) + (rnk$maxCstAverage - rnk$ranCstAverage) + (rnk$maxCstPercentile10 - rnk$ranCstPercentile10)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostCstAverage, rnk$lostCstPoorRatio, rnk$lostCstPercentile10 )), 
           inputHeader   = "Call Setup Time Points Lost", 
           inputNames    = names,
           inputColors   = c( "dodgerblue1" , "dodgerblue2", "dodgerblue3"), 
           inputKPIs     = c("CST AVG", "CST > 15s", "CST P10"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostCstPoorRatio + rnk$lostCstAverage + rnk$lostCstPercentile10 ) ),
           inputLabel    = paste( round( ( rnk$lostCstPoorRatio + rnk$lostCstAverage + rnk$lostCstPercentile10 ), digits = 2 ),
                                  "of",
                                  (rnk$maxCstPoorRatio + rnk$maxCstAverage + rnk$maxCstPercentile10 ),
                                  "\n",
                                  round( rnk$lostCstAverage , digits = 2 ),
                                  "+",
                                  round( rnk$lostCstPoorRatio , digits = 2 ),
                                  "+",
                                  round( rnk$lostCstPercentile10 , digits = 2 )
           ),
           maxValue          = max(rnk$lostCstPoorRatio + rnk$lostCstAverage + rnk$lostCstPercentile10) )

  # CstAverage RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranCstAverage, 
          inputHeader           = "Ranking CST Average", 
          inputNames            = names, 
          inputColors           = c("dodgerblue1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranCstAverage,
          inputLabel            = paste( round(rnk$ranCstAverage,digits = 2), "of", round(rnk$maxCstAverage,digits = 2) ),
          maxValue              = max(rnk$ranCstAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxCstAverage - rnk$ranCstAverage), 
          inputHeader           = "CST Average Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("dodgerblue1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxCstAverage - rnk$ranCstAverage),
          inputLabel            = paste( round(rnk$maxCstAverage - rnk$ranCstAverage,digits = 2), "of", round(rnk$maxCstAverage,digits = 2) ),
          maxValue              = max(rnk$maxCstAverage - rnk$ranCstAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostCstAverage, 
          inputHeader           = "CST Average Points Lost", 
          inputNames            = names, 
          inputColors           = c("dodgerblue1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostCstAverage,
          inputLabel            = paste( round(rnk$lostCstAverage,digits = 2), "of", round(rnk$maxCstAverage,digits = 2) ),
          maxValue              = max(rnk$lostCstAverage) )
    
  # CST Poor Ratio RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranCstPoorRatio, 
          inputHeader           = "Ranking CST Poor Ratio", 
          inputNames            = names, 
          inputColors           = c("dodgerblue2"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranCstPoorRatio,
          inputLabel            = paste( round(rnk$ranCstPoorRatio,digits = 2), "of", round(rnk$maxCstPoorRatio,digits = 2) ),
          maxValue              = max(rnk$ranCstPoorRatio) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxCstPoorRatio - rnk$ranCstPoorRatio), 
          inputHeader           = "CST Poor Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("dodgerblue2"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxCstPoorRatio - rnk$ranCstPoorRatio),
          inputLabel            = paste( round(rnk$maxCstPoorRatio - rnk$ranCstPoorRatio,digits = 2), "of", round(rnk$maxCstPoorRatio,digits = 2) ),
          maxValue              = max(rnk$maxCstPoorRatio - rnk$ranCstPoorRatio) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostCstPoorRatio, 
          inputHeader           = "CST Poor Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("dodgerblue2"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostCstPoorRatio,
          inputLabel            = paste( round(rnk$lostCstPoorRatio,digits = 2), "of", round(rnk$maxCstPoorRatio,digits = 2) ),
          maxValue              = max(rnk$lostCstPoorRatio) )
  
  # CST P10 RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranCstPercentile10, 
          inputHeader           = "Ranking CST P10", 
          inputNames            = names, 
          inputColors           = c("dodgerblue3"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranCstPercentile10,
          inputLabel            = paste( round(rnk$ranCstPercentile10,digits = 2), "of", round(rnk$maxCstPercentile10,digits = 2) ),
          maxValue              = max(rnk$ranCstPercentile10) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxCstPercentile10 - rnk$ranCstPercentile10), 
          inputHeader           = "CST P10 Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("dodgerblue3"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxCstPercentile10 - rnk$ranCstPercentile10),
          inputLabel            = paste( round(rnk$maxCstPercentile10 - rnk$ranCstPercentile10,digits = 2), "of", round(rnk$maxCstPercentile10,digits = 2) ),
          maxValue              = max(rnk$maxCstPercentile10 - rnk$ranCstPercentile10) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostCstPercentile10, 
          inputHeader           = "CST P10 Points Lost", 
          inputNames            = names, 
          inputColors           = c("dodgerblue3"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostCstPercentile10,
          inputLabel            = paste( round(rnk$lostCstPercentile10,digits = 2), "of", round(rnk$maxCstPercentile10,digits = 2) ),
          maxValue              = max(rnk$lostCstPercentile10) )
  
  # POLQA RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranSpeechPolqaAverage, rnk$ranSpeechTestsPoorRatio, rnk$ranSpeechPolqaPercentile90 )), 
           inputHeader   = "Speech Quality Ranking", 
           inputNames    = names,
           inputColors   = c( "darkseagreen1" , "darkseagreen2", "darkseagreen3"), 
           inputKPIs     = c("MOS AVG", "MOS < 1,6", "MOS P90"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranSpeechTestsPoorRatio + rnk$ranSpeechPolqaAverage + rnk$ranSpeechPolqaPercentile90 ) ),
           inputLabel    = paste( round( ( rnk$ranSpeechTestsPoorRatio + rnk$ranSpeechPolqaAverage + rnk$ranSpeechPolqaPercentile90 ), digits = 2 ),
                                  "of",
                                  (rnk$maxSpeechTestsPoorRatio + rnk$maxSpeechPolqaAverage + rnk$maxSpeechPolqaPercentile90 ),
                                  "\n",
                                  round( rnk$ranSpeechPolqaAverage , digits = 2 ),
                                  "+",
                                  round( rnk$ranSpeechTestsPoorRatio , digits = 2 ),
                                  "+",
                                  round( rnk$ranSpeechPolqaPercentile90 , digits = 2 )
           ),
           maxValue          = max(rnk$ranSpeechTestsPoorRatio + rnk$ranSpeechPolqaAverage + rnk$ranSpeechPolqaPercentile90) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage), (rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio), (rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90) )), 
           inputHeader   = "Speech Quality Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "darkseagreen1" , "darkseagreen2", "darkseagreen3"), 
           inputKPIs     = c("MOS AVG", "MOS < 1,6", "MOS P90"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio) + (rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage) + (rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90) ) ),
           inputLabel    = paste( round( ( (rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio) + (rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage) + (rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90) ), digits = 2 ),
                                  "of",
                                  (rnk$maxSpeechTestsPoorRatio + rnk$maxSpeechPolqaAverage + rnk$maxSpeechPolqaPercentile90 ),
                                  "\n",
                                  round( (rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90) , digits = 2 )
           ),
           maxValue          = max((rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio) + (rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage) + (rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostSpeechPolqaAverage, rnk$lostSpeechTestsPoorRatio, rnk$lostSpeechPolqaPercentile90 )), 
           inputHeader   = "Speech Quality Points Lost", 
           inputNames    = names,
           inputColors   = c( "darkseagreen1" , "darkseagreen2", "darkseagreen3"), 
           inputKPIs     = c("MOS AVG", "MOS < 1,6", "MOS P90"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostSpeechTestsPoorRatio + rnk$lostSpeechPolqaAverage + rnk$lostSpeechPolqaPercentile90 ) ),
           inputLabel    = paste( round( ( rnk$lostSpeechTestsPoorRatio + rnk$lostSpeechPolqaAverage + rnk$lostSpeechPolqaPercentile90 ), digits = 2 ),
                                  "of",
                                  (rnk$maxSpeechTestsPoorRatio + rnk$maxSpeechPolqaAverage + rnk$maxSpeechPolqaPercentile90 ),
                                  "\n",
                                  round( rnk$lostSpeechPolqaAverage , digits = 2 ),
                                  "+",
                                  round( rnk$lostSpeechTestsPoorRatio , digits = 2 ),
                                  "+",
                                  round( rnk$lostSpeechPolqaPercentile90 , digits = 2 )
           ),
           maxValue          = max(rnk$lostSpeechTestsPoorRatio + rnk$lostSpeechPolqaAverage + rnk$lostSpeechPolqaPercentile90) )
  
  # POLQA AVG RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranSpeechPolqaAverage, 
          inputHeader           = "Ranking POLQA AVG", 
          inputNames            = names, 
          inputColors           = c("darkseagreen1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranSpeechPolqaAverage,
          inputLabel            = paste( round(rnk$ranSpeechPolqaAverage,digits = 2), "of", round(rnk$maxSpeechPolqaAverage,digits = 2) ),
          maxValue              = max(rnk$ranSpeechPolqaAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage), 
          inputHeader           = "POLQA AVG Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkseagreen1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage),
          inputLabel            = paste( round(rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage,digits = 2), "of", round(rnk$maxSpeechPolqaAverage,digits = 2) ),
          maxValue              = max(rnk$maxSpeechPolqaAverage - rnk$ranSpeechPolqaAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostSpeechPolqaAverage, 
          inputHeader           = "POLQA AVG Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkseagreen1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostSpeechPolqaAverage,
          inputLabel            = paste( round(rnk$lostSpeechPolqaAverage,digits = 2), "of", round(rnk$maxSpeechPolqaAverage,digits = 2) ),
          maxValue              = max(rnk$lostSpeechPolqaAverage) )
  
  # POLQA POOR MOS Ratio RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranSpeechTestsPoorRatio, 
          inputHeader           = "Ranking POLQA POOR MOS Ratio", 
          inputNames            = names, 
          inputColors           = c("darkseagreen2"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranSpeechTestsPoorRatio,
          inputLabel            = paste( round(rnk$ranSpeechTestsPoorRatio,digits = 2), "of", round(rnk$maxSpeechTestsPoorRatio,digits = 2) ),
          maxValue              = max(rnk$ranSpeechTestsPoorRatio) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio), 
          inputHeader           = "POLQA POOR MOS Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkseagreen2"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio),
          inputLabel            = paste( round(rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio,digits = 2), "of", round(rnk$maxSpeechTestsPoorRatio,digits = 2) ),
          maxValue              = max(rnk$maxSpeechTestsPoorRatio - rnk$ranSpeechTestsPoorRatio) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostSpeechTestsPoorRatio, 
          inputHeader           = "POLQA POOR MOS Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkseagreen2"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostSpeechTestsPoorRatio,
          inputLabel            = paste( round(rnk$lostSpeechTestsPoorRatio,digits = 2), "of", round(rnk$maxSpeechTestsPoorRatio,digits = 2) ),
          maxValue              = max(rnk$lostSpeechTestsPoorRatio) )
  
  # POLQA P90 MOS Ratio RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranSpeechPolqaPercentile90, 
          inputHeader           = "Ranking POLQA P90 MOS Ratio", 
          inputNames            = names, 
          inputColors           = c("darkseagreen3"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranSpeechPolqaPercentile90,
          inputLabel            = paste( round(rnk$ranSpeechPolqaPercentile90,digits = 2), "of", round(rnk$maxSpeechPolqaPercentile90,digits = 2) ),
          maxValue              = max(rnk$ranSpeechPolqaPercentile90) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90), 
          inputHeader           = "POLQA P90 MOS Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkseagreen3"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90),
          inputLabel            = paste( round(rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90,digits = 2), "of", round(rnk$maxSpeechPolqaPercentile90,digits = 2) ),
          maxValue              = max(rnk$maxSpeechPolqaPercentile90 - rnk$ranSpeechPolqaPercentile90) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostSpeechPolqaPercentile90, 
          inputHeader           = "POLQA P90 MOS Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkseagreen3"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostSpeechPolqaPercentile90,
          inputLabel            = paste( round(rnk$lostSpeechPolqaPercentile90,digits = 2), "of", round(rnk$maxSpeechPolqaPercentile90,digits = 2) ),
          maxValue              = max(rnk$lostSpeechPolqaPercentile90) )
  dev.off()
}

###################################################################################################################################################
#                                                               FUNCTION plot DATA                                                                #
###################################################################################################################################################

rnkData <- function(rnk, level, fn)
{
  if (nrow(rnk) < 5) {
    heightFile <- nrow(rnk)*1600
  } else {
    heightFile <- nrow(rnk)*1400
  }
  file <- png(file=fn,width=1600,height=heightFile,res=72)
  layout(matrix(c(1,1,2,3,
                  4,4,5,6,
                  7,7,8,9,
                  10,10,11,12,
                  13,13,14,15,
                  16,16,17,18,
                  19,19,20,21,
                  22,22,23,24,
                  25,25,26,27,
                  28,28,29,30,
                  31,31,32,33,
                  34,34,35,36,
                  37,37,38,39,
                  40,40,41,42,
                  43,43,44,45,
                  46,46,47,48,
                  49,49,50,51,
                  52,52,53,54,
                  55,55,56,57,
                  58,58,59,60,
                  61,61,62,63,
                  64,64,65,66,
                  67,67,68,69), 
                nrow=23, 
                ncol=4, 
                byrow = TRUE), 
         widths=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), 
         heights=c(1,1,1,1,1,1,1,1,1,1))
  
  if (level == 2) {
    names <- paste(rnk$G_Level_1,rnk$G_Level_2)
  }
  if (level == 3) {
    names <- paste(rnk$G_Level_1,rnk$G_Level_2,"\n",rnk$G_Level_3)
  }
  if (level == 4) {
    names <- paste(rnk$G_Level_3,"\n",rnk$G_Level_4)
  }
  
  # OVERALL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranData, 
          inputHeader           = "Total Data", 
          inputNames            = names, 
          inputColors           = c(rgb( 78, 166, 220, maxColorValue=255)), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranData,
          inputLabel            = paste(round(rnk$ranData,digits = 2), "of", rnk$maxData), 
          maxValue              = max(rnk$ranData) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxData - rnk$ranData), 
          inputHeader           = "Data Gap to MAX", 
          inputNames            = names, 
          inputColors           = c(rgb( 78, 166, 220, maxColorValue=255)), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxData - rnk$ranData),
          inputLabel            = paste(round((rnk$maxData - rnk$ranData),digits = 2), "of", rnk$maxData), 
          maxValue              = max(rnk$maxData - rnk$ranData) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostData, 
          inputHeader           = "Data Points Lost", 
          inputNames            = names, 
          inputColors           = c(rgb( 78, 166, 220, maxColorValue=255)), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostData,
          inputLabel            = paste(round(rnk$lostData,digits = 2), "of", rnk$maxData), 
          maxValue              = max(rnk$lostData) )
  
  # OVERALL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranHttpT, rnk$ranHttpB, rnk$ranVSLVS, rnk$ranAPPFB )), 
           inputHeader   = "Data Ranking", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "darkorchid1", "dodgerblue1", "darkseagreen1"), 
           inputKPIs     = c("HttpTransfer", "HttpBrowsing", "Video Streaming", "Mobile Application"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranHttpB + rnk$ranHttpT + rnk$ranVSLVS + rnk$ranAPPFB ) ),
           inputLabel    = paste( round( ( rnk$ranHttpB + rnk$ranHttpT + rnk$ranVSLVS + rnk$ranAPPFB ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpB + rnk$maxHttpT + rnk$maxVSLVS + rnk$maxAPPFB ),
                                  "\n",
                                  round( rnk$ranHttpT , digits = 2 ),
                                  "+",
                                  round( rnk$ranHttpB , digits = 2 ),
                                  "+",
                                  round( rnk$ranVSLVS , digits = 2 ),
                                  "+",
                                  round( rnk$ranAPPFB , digits = 2 )
           ),
           maxValue          = max(rnk$ranHttpB + rnk$ranHttpT + rnk$ranVSLVS + rnk$ranAPPFB) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxHttpT - rnk$ranHttpT), (rnk$maxHttpB - rnk$ranHttpB), (rnk$maxVSLVS - rnk$ranVSLVS), (rnk$maxAPPFB - rnk$ranAPPFB) )), 
           inputHeader   = "Data Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "darkorchid1", "dodgerblue1", "darkseagreen1"), 
           inputKPIs     = c("HttpTransfer", "HttpBrowsing", "Video Streaming", "Mobile Application"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxHttpB - rnk$ranHttpB) + (rnk$maxHttpT - rnk$ranHttpT) + (rnk$maxVSLVS - rnk$ranVSLVS) + (rnk$maxAPPFB - rnk$ranAPPFB) ) ),
           inputLabel    = paste( round( ( (rnk$maxHttpB - rnk$ranHttpB) + (rnk$maxHttpT - rnk$ranHttpT) + (rnk$maxVSLVS - rnk$ranVSLVS) + (rnk$maxAPPFB - rnk$ranAPPFB) ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpB + rnk$maxHttpT + rnk$maxVSLVS + rnk$maxAPPFB ),
                                  "\n",
                                  round( (rnk$maxHttpT - rnk$ranHttpT) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxHttpB - rnk$ranHttpB) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxVSLVS - rnk$ranVSLVS) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxAPPFB - rnk$ranAPPFB) , digits = 2 )
           ),
           maxValue          = max((rnk$maxHttpB - rnk$ranHttpB) + (rnk$maxHttpT - rnk$ranHttpT) + (rnk$maxVSLVS - rnk$ranVSLVS) + (rnk$maxAPPFB - rnk$ranAPPFB)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostHttpT, rnk$lostHttpB, rnk$lostVSLVS, rnk$lostAPPFB )), 
           inputHeader   = "Data Points Lost", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "darkorchid1", "dodgerblue1", "darkseagreen1"), 
           inputKPIs     = c("HttpTransfer", "HttpBrowsing", "Video Streaming", "Mobile Application"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostHttpB + rnk$lostHttpT + rnk$lostVSLVS + rnk$lostAPPFB ) ),
           inputLabel    = paste( round( ( rnk$lostHttpB + rnk$lostHttpT + rnk$lostVSLVS + rnk$lostAPPFB ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpB + rnk$maxHttpT + rnk$maxVSLVS + rnk$maxAPPFB ),
                                  "\n",
                                  round( rnk$lostHttpT , digits = 2 ),
                                  "+",
                                  round( rnk$lostHttpB , digits = 2 ),
                                  "+",
                                  round( rnk$lostVSLVS , digits = 2 ),
                                  "+",
                                  round( rnk$lostAPPFB , digits = 2 )
           ),
           maxValue          = max(rnk$lostHttpB + rnk$lostHttpT + rnk$lostVSLVS + rnk$lostAPPFB) )
  
  # OVERALL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpFdfsCompletedRatio, 
          inputHeader           = "FDFS Success Ratio Ranking", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpFdfsCompletedRatio,
          inputLabel            = paste(round(rnk$ranHttpFdfsCompletedRatio,digits = 2), "of", rnk$maxHttpFdfsCompletedRatio), 
          maxValue              = max(rnk$ranHttpFdfsCompletedRatio) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpFdfsCompletedRatio - rnk$ranHttpFdfsCompletedRatio), 
          inputHeader           = "FDFS Success Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpFdfsCompletedRatio - rnk$ranHttpFdfsCompletedRatio),
          inputLabel            = paste(round((rnk$maxHttpFdfsCompletedRatio - rnk$ranHttpFdfsCompletedRatio),digits = 2), "of", rnk$maxHttpFdfsCompletedRatio), 
          maxValue              = max(rnk$maxHttpFdfsCompletedRatio - rnk$ranHttpFdfsCompletedRatio) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpFdfsCompletedRatio, 
          inputHeader           = "FDFS Success Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "TOTAL", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpFdfsCompletedRatio,
          inputLabel            = paste(round(rnk$lostHttpFdfsCompletedRatio,digits = 2), "of", rnk$maxHttpFdfsCompletedRatio), 
          maxValue              = max(rnk$lostHttpFdfsCompletedRatio) )

  # HTTP FDTT DL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranHttpFdttDlMdrPercentile10, rnk$ranHttpFdttDlMdrAverage, rnk$ranHttpFdttDlMdrPercentile90 )), 
           inputHeader   = "HTTP FDTT DL Ranking", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "firebrick2", "firebrick3"), 
           inputKPIs     = c("HTTP FDTT DL P10", "HTTP FDTT DL AVG", "HTTP FDTT DL P90"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranHttpFdttDlMdrAverage + rnk$ranHttpFdttDlMdrPercentile10 + rnk$ranHttpFdttDlMdrPercentile90 ) ),
           inputLabel    = paste( round( ( rnk$ranHttpFdttDlMdrAverage + rnk$ranHttpFdttDlMdrPercentile10 + rnk$ranHttpFdttDlMdrPercentile90 ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpFdttDlMdrAverage + rnk$maxHttpFdttDlMdrPercentile10 + rnk$maxHttpFdttDlMdrPercentile90 ),
                                  "\n",
                                  round( rnk$ranHttpFdttDlMdrPercentile10 , digits = 2 ),
                                  "+",
                                  round( rnk$ranHttpFdttDlMdrAverage , digits = 2 ),
                                  "+",
                                  round( rnk$ranHttpFdttDlMdrPercentile90 , digits = 2 )
           ),
           maxValue          = max(rnk$ranHttpFdttDlMdrAverage + rnk$ranHttpFdttDlMdrPercentile10 + rnk$ranHttpFdttDlMdrPercentile90) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10), (rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage), (rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90) )), 
           inputHeader   = "HTTP FDTT DL Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "firebrick2", "firebrick3"), 
           inputKPIs     = c("HTTP FDTT DL P10", "HTTP FDTT DL AVG", "HTTP FDTT DL P90"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage) + (rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10) + (rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90) ) ),
           inputLabel    = paste( round( ( (rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage) + (rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10) + (rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90) ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpFdttDlMdrAverage + rnk$maxHttpFdttDlMdrPercentile10 + rnk$maxHttpFdttDlMdrPercentile90 ),
                                  "\n",
                                  round( (rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90) , digits = 2 )
           ),
           maxValue          = max((rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage) + (rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10) + (rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostHttpFdttDlMdrPercentile10, rnk$lostHttpFdttDlMdrAverage, rnk$lostHttpFdttDlMdrPercentile90 )), 
           inputHeader   = "HTTP FDTT DL Points Lost", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "firebrick2", "firebrick3"), 
           inputKPIs     = c("HTTP FDTT DL P10", "HTTP FDTT DL AVG", "HTTP FDTT DL P90"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostHttpFdttDlMdrAverage + rnk$lostHttpFdttDlMdrPercentile10 + rnk$lostHttpFdttDlMdrPercentile90 ) ),
           inputLabel    = paste( round( ( rnk$lostHttpFdttDlMdrAverage + rnk$lostHttpFdttDlMdrPercentile10 + rnk$lostHttpFdttDlMdrPercentile90 ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpFdttDlMdrAverage + rnk$maxHttpFdttDlMdrPercentile10 + rnk$maxHttpFdttDlMdrPercentile90 ),
                                  "\n",
                                  round( rnk$lostHttpFdttDlMdrPercentile10 , digits = 2 ),
                                  "+",
                                  round( rnk$lostHttpFdttDlMdrAverage , digits = 2 ),
                                  "+",
                                  round( rnk$lostHttpFdttDlMdrPercentile90 , digits = 2 )
           ),
           maxValue          = max(rnk$lostHttpFdttDlMdrAverage + rnk$lostHttpFdttDlMdrPercentile10 + rnk$lostHttpFdttDlMdrPercentile90) )
  
  # HTTP FDTT DL P10 RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpFdttDlMdrPercentile10, 
          inputHeader           = "HTTP FDTT DL P10 Ranking", 
          inputNames            = names, 
          inputColors           = c("firebrick2"), 
          inputKPIs             = "HTTP FDTT DL P10", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpFdttDlMdrPercentile10,
          inputLabel            = paste(round(rnk$ranHttpFdttDlMdrPercentile10,digits = 2), "of", rnk$maxHttpFdttDlMdrPercentile10), 
          maxValue              = max(rnk$ranHttpFdttDlMdrPercentile10) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10), 
          inputHeader           = "HTTP FDTT DL P10 Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick2"), 
          inputKPIs             = "HTTP FDTT DL P10", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10),
          inputLabel            = paste(round((rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10),digits = 2), "of", rnk$maxHttpFdttDlMdrPercentile10), 
          maxValue              = max(rnk$maxHttpFdttDlMdrPercentile10 - rnk$ranHttpFdttDlMdrPercentile10) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpFdttDlMdrPercentile10, 
          inputHeader           = "HTTP FDTT DL P10 Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick2"), 
          inputKPIs             = "HTTP FDTT DL P10", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpFdttDlMdrPercentile10,
          inputLabel            = paste(round(rnk$lostHttpFdttDlMdrPercentile10,digits = 2), "of", rnk$maxHttpFdttDlMdrPercentile10), 
          maxValue              = max(rnk$lostHttpFdttDlMdrPercentile10) )
  
  # HTTP FDTT DL AVG RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpFdttDlMdrAverage, 
          inputHeader           = "HTTP FDTT DL AVG Ranking", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "HTTP FDTT DL AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpFdttDlMdrAverage,
          inputLabel            = paste(round(rnk$ranHttpFdttDlMdrAverage,digits = 2), "of", rnk$maxHttpFdttDlMdrAverage), 
          maxValue              = max(rnk$ranHttpFdttDlMdrAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage), 
          inputHeader           = "HTTP FDTT DL AVG Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "HTTP FDTT DL AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage),
          inputLabel            = paste(round((rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage),digits = 2), "of", rnk$maxHttpFdttDlMdrAverage), 
          maxValue              = max(rnk$maxHttpFdttDlMdrAverage - rnk$ranHttpFdttDlMdrAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpFdttDlMdrAverage, 
          inputHeader           = "HTTP FDTT DL AVG Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "HTTP FDTT DL AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpFdttDlMdrAverage,
          inputLabel            = paste(round(rnk$lostHttpFdttDlMdrAverage,digits = 2), "of", rnk$maxHttpFdttDlMdrAverage), 
          maxValue              = max(rnk$lostHttpFdttDlMdrAverage) )
  
  # HTTP FDTT DL P90 RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpFdttDlMdrPercentile90, 
          inputHeader           = "HTTP FDTT DL P90 Ranking", 
          inputNames            = names, 
          inputColors           = c("firebrick3"), 
          inputKPIs             = "HTTP FDTT DL P90", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpFdttDlMdrPercentile90,
          inputLabel            = paste(round(rnk$ranHttpFdttDlMdrPercentile90,digits = 2), "of", rnk$maxHttpFdttDlMdrPercentile90), 
          maxValue              = max(rnk$ranHttpFdttDlMdrPercentile90) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90), 
          inputHeader           = "HTTP FDTT DL P90 Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick3"), 
          inputKPIs             = "HTTP FDTT DL P90", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90),
          inputLabel            = paste(round((rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90),digits = 2), "of", rnk$maxHttpFdttDlMdrPercentile90), 
          maxValue              = max(rnk$maxHttpFdttDlMdrPercentile90 - rnk$ranHttpFdttDlMdrPercentile90) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpFdttDlMdrPercentile90, 
          inputHeader           = "HTTP FDTT DL P90 Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick3"), 
          inputKPIs             = "HTTP FDTT DL P90", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpFdttDlMdrPercentile90,
          inputLabel            = paste(round(rnk$lostHttpFdttDlMdrPercentile90,digits = 2), "of", rnk$maxHttpFdttDlMdrPercentile90), 
          maxValue              = max(rnk$lostHttpFdttDlMdrPercentile90) )
  
  # HTTP FDTT UL RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranHttpFdttUlMdrPercentile10, rnk$ranHttpFdttUlMdrAverage, rnk$ranHttpFdttUlMdrPercentile90 )), 
           inputHeader   = "HTTP FDTT UL Ranking", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "firebrick2", "firebrick3"), 
           inputKPIs     = c("HTTP FDTT UL P10", "HTTP FDTT UL AVG", "HTTP FDTT UL P90"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranHttpFdttUlMdrAverage + rnk$ranHttpFdttUlMdrPercentile10 + rnk$ranHttpFdttUlMdrPercentile90 ) ),
           inputLabel    = paste( round( ( rnk$ranHttpFdttUlMdrAverage + rnk$ranHttpFdttUlMdrPercentile10 + rnk$ranHttpFdttUlMdrPercentile90 ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpFdttUlMdrAverage + rnk$maxHttpFdttUlMdrPercentile10 + rnk$maxHttpFdttUlMdrPercentile90 ),
                                  "\n",
                                  round( rnk$ranHttpFdttUlMdrPercentile10 , digits = 2 ),
                                  "+",
                                  round( rnk$ranHttpFdttUlMdrAverage , digits = 2 ),
                                  "+",
                                  round( rnk$ranHttpFdttUlMdrPercentile90 , digits = 2 )
           ),
           maxValue          = max(rnk$ranHttpFdttUlMdrAverage + rnk$ranHttpFdttUlMdrPercentile10 + rnk$ranHttpFdttUlMdrPercentile90) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10), (rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage), (rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90) )), 
           inputHeader   = "HTTP FDTT UL Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "firebrick2", "firebrick3"), 
           inputKPIs     = c("HTTP FDTT UL P10", "HTTP FDTT UL AVG", "HTTP FDTT UL P90"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage) + (rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10) + (rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90) ) ),
           inputLabel    = paste( round( ( (rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage) + (rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10) + (rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90) ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpFdttUlMdrAverage + rnk$maxHttpFdttUlMdrPercentile10 + rnk$maxHttpFdttUlMdrPercentile90 ),
                                  "\n",
                                  round( (rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90) , digits = 2 )
           ),
           maxValue          = max((rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage) + (rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10) + (rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostHttpFdttUlMdrPercentile10, rnk$lostHttpFdttUlMdrAverage, rnk$lostHttpFdttUlMdrPercentile90 )), 
           inputHeader   = "HTTP FDTT UL Points Lost", 
           inputNames    = names,
           inputColors   = c( "firebrick1" , "firebrick2", "firebrick3"), 
           inputKPIs     = c("HTTP FDTT UL P10", "HTTP FDTT UL AVG", "HTTP FDTT UL P90"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostHttpFdttUlMdrAverage + rnk$lostHttpFdttUlMdrPercentile10 + rnk$lostHttpFdttUlMdrPercentile90 ) ),
           inputLabel    = paste( round( ( rnk$lostHttpFdttUlMdrAverage + rnk$lostHttpFdttUlMdrPercentile10 + rnk$lostHttpFdttUlMdrPercentile90 ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpFdttUlMdrAverage + rnk$maxHttpFdttUlMdrPercentile10 + rnk$maxHttpFdttUlMdrPercentile90 ),
                                  "\n",
                                  round( rnk$lostHttpFdttUlMdrPercentile10 , digits = 2 ),
                                  "+",
                                  round( rnk$lostHttpFdttUlMdrAverage , digits = 2 ),
                                  "+",
                                  round( rnk$lostHttpFdttUlMdrPercentile90 , digits = 2 )
           ),
           maxValue          = max(rnk$lostHttpFdttUlMdrAverage + rnk$lostHttpFdttUlMdrPercentile10 + rnk$lostHttpFdttUlMdrPercentile90) )
  
  # HTTP FDTT UL P10 RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpFdttUlMdrPercentile10, 
          inputHeader           = "HTTP FDTT UL P10 Ranking", 
          inputNames            = names, 
          inputColors           = c("firebrick2"), 
          inputKPIs             = "HTTP FDTT UL P10", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpFdttUlMdrPercentile10,
          inputLabel            = paste(round(rnk$ranHttpFdttUlMdrPercentile10,digits = 2), "of", rnk$maxHttpFdttUlMdrPercentile10), 
          maxValue              = max(rnk$ranHttpFdttUlMdrPercentile10) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10), 
          inputHeader           = "HTTP FDTT UL P10 Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick2"), 
          inputKPIs             = "HTTP FDTT UL P10", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10),
          inputLabel            = paste(round((rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10),digits = 2), "of", rnk$maxHttpFdttUlMdrPercentile10), 
          maxValue              = max(rnk$maxHttpFdttUlMdrPercentile10 - rnk$ranHttpFdttUlMdrPercentile10) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpFdttUlMdrPercentile10, 
          inputHeader           = "HTTP FDTT UL P10 Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick2"), 
          inputKPIs             = "HTTP FDTT UL P10", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpFdttUlMdrPercentile10,
          inputLabel            = paste(round(rnk$lostHttpFdttUlMdrPercentile10,digits = 2), "of", rnk$maxHttpFdttUlMdrPercentile10), 
          maxValue              = max(rnk$lostHttpFdttUlMdrPercentile10) )
  
  # HTTP FDTT UL AVG RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpFdttUlMdrAverage, 
          inputHeader           = "HTTP FDTT UL AVG Ranking", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "HTTP FDTT UL AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpFdttUlMdrAverage,
          inputLabel            = paste(round(rnk$ranHttpFdttUlMdrAverage,digits = 2), "of", rnk$maxHttpFdttUlMdrAverage), 
          maxValue              = max(rnk$ranHttpFdttUlMdrAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage), 
          inputHeader           = "HTTP FDTT UL AVG Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "HTTP FDTT UL AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage),
          inputLabel            = paste(round((rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage),digits = 2), "of", rnk$maxHttpFdttUlMdrAverage), 
          maxValue              = max(rnk$maxHttpFdttUlMdrAverage - rnk$ranHttpFdttUlMdrAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpFdttUlMdrAverage, 
          inputHeader           = "HTTP FDTT UL AVG Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick1"), 
          inputKPIs             = "HTTP FDTT UL AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpFdttUlMdrAverage,
          inputLabel            = paste(round(rnk$lostHttpFdttUlMdrAverage,digits = 2), "of", rnk$maxHttpFdttUlMdrAverage), 
          maxValue              = max(rnk$lostHttpFdttUlMdrAverage) )
  
  # HTTP FDTT UL P90 RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpFdttUlMdrPercentile90, 
          inputHeader           = "HTTP FDTT UL P90 Ranking", 
          inputNames            = names, 
          inputColors           = c("firebrick3"), 
          inputKPIs             = "HTTP FDTT UL P90", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpFdttUlMdrPercentile90,
          inputLabel            = paste(round(rnk$ranHttpFdttUlMdrPercentile90,digits = 2), "of", rnk$maxHttpFdttUlMdrPercentile90), 
          maxValue              = max(rnk$ranHttpFdttUlMdrPercentile90) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90), 
          inputHeader           = "HTTP FDTT UL P90 Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("firebrick3"), 
          inputKPIs             = "HTTP FDTT UL P90", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90),
          inputLabel            = paste(round((rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90),digits = 2), "of", rnk$maxHttpFdttUlMdrPercentile90), 
          maxValue              = max(rnk$maxHttpFdttUlMdrPercentile90 - rnk$ranHttpFdttUlMdrPercentile90) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpFdttUlMdrPercentile90, 
          inputHeader           = "HTTP FDTT UL P90 Points Lost", 
          inputNames            = names, 
          inputColors           = c("firebrick3"), 
          inputKPIs             = "HTTP FDTT UL P90", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpFdttUlMdrPercentile90,
          inputLabel            = paste(round(rnk$lostHttpFdttUlMdrPercentile90,digits = 2), "of", rnk$maxHttpFdttUlMdrPercentile90), 
          maxValue              = max(rnk$lostHttpFdttUlMdrPercentile90) )
  
  # HTTP Browsing RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranHttpBrowsingCompletedRatio, rnk$ranHttpBrowsingTransferRttAverage )), 
           inputHeader   = "HTTP Browsing Ranking", 
           inputNames    = names,
           inputColors   = c( "darkorchid1" , "darkorchid2"), 
           inputKPIs     = c("HTTP Browsing Success Ratio", "HTTP Browsing RTT AVG"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranHttpBrowsingTransferRttAverage + rnk$ranHttpBrowsingCompletedRatio ) ),
           inputLabel    = paste( round( ( rnk$ranHttpBrowsingTransferRttAverage + rnk$ranHttpBrowsingCompletedRatio ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpBrowsingTransferRttAverage + rnk$maxHttpBrowsingCompletedRatio ),
                                  "\n",
                                  round( rnk$ranHttpBrowsingCompletedRatio , digits = 2 ),
                                  "+",
                                  round( rnk$ranHttpBrowsingTransferRttAverage , digits = 2 )
           ),
           maxValue          = max(rnk$ranHttpBrowsingTransferRttAverage + rnk$ranHttpBrowsingCompletedRatio) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio), (rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage) )), 
           inputHeader   = "HTTP Browsing Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "darkorchid1" , "darkorchid2"), 
           inputKPIs     = c("HTTP Browsing Success Ratio", "HTTP Browsing RTT AVG"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage) + (rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio) ) ),
           inputLabel    = paste( round( ( (rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage) + (rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio) ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpBrowsingTransferRttAverage + rnk$maxHttpBrowsingCompletedRatio ),
                                  "\n",
                                  round( (rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage) , digits = 2 )
           ),
           maxValue          = max((rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage) + (rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostHttpBrowsingCompletedRatio, rnk$lostHttpBrowsingTransferRttAverage )), 
           inputHeader   = "HTTP Browsing Points Lost", 
           inputNames    = names,
           inputColors   = c( "darkorchid1" , "darkorchid2"), 
           inputKPIs     = c("HTTP Browsing Success Ratio", "HTTP Browsing RTT AVG"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostHttpBrowsingTransferRttAverage + rnk$lostHttpBrowsingCompletedRatio ) ),
           inputLabel    = paste( round( ( rnk$lostHttpBrowsingTransferRttAverage + rnk$lostHttpBrowsingCompletedRatio ), digits = 2 ),
                                  "of",
                                  (rnk$maxHttpBrowsingTransferRttAverage + rnk$maxHttpBrowsingCompletedRatio ),
                                  "\n",
                                  round( rnk$lostHttpBrowsingCompletedRatio , digits = 2 ),
                                  "+",
                                  round( rnk$lostHttpBrowsingTransferRttAverage , digits = 2 )
                                  ),
           maxValue          = max(rnk$lostHttpBrowsingTransferRttAverage + rnk$lostHttpBrowsingCompletedRatio) )
  
  # HTTP Browsing Completed Ratio RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpBrowsingCompletedRatio, 
          inputHeader           = "HTTP Browsing Completed Ratio Ranking", 
          inputNames            = names, 
          inputColors           = c("darkorchid1"), 
          inputKPIs             = "HTTP Browsing Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpBrowsingCompletedRatio,
          inputLabel            = paste(round(rnk$ranHttpBrowsingCompletedRatio,digits = 2), "of", rnk$maxHttpBrowsingCompletedRatio), 
          maxValue              = max(rnk$ranHttpBrowsingCompletedRatio) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio), 
          inputHeader           = "HTTP Browsing Completed Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkorchid1"), 
          inputKPIs             = "HTTP Browsing Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio),
          inputLabel            = paste(round((rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio),digits = 2), "of", rnk$maxHttpBrowsingCompletedRatio), 
          maxValue              = max(rnk$maxHttpBrowsingCompletedRatio - rnk$ranHttpBrowsingCompletedRatio) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpBrowsingCompletedRatio, 
          inputHeader           = "HTTP Browsing Completed Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkorchid1"), 
          inputKPIs             = "HTTP Browsing Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpBrowsingCompletedRatio,
          inputLabel            = paste(round(rnk$lostHttpBrowsingCompletedRatio,digits = 2), "of", rnk$maxHttpBrowsingCompletedRatio), 
          maxValue              = max(rnk$lostHttpBrowsingCompletedRatio) )
  
  # HTTP Browsing RTT AVG RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranHttpBrowsingTransferRttAverage, 
          inputHeader           = "HTTP Browsing RTT AVG Ranking", 
          inputNames            = names, 
          inputColors           = c("darkorchid2"), 
          inputKPIs             = "HTTP Browsing RTT AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranHttpBrowsingTransferRttAverage,
          inputLabel            = paste(round(rnk$ranHttpBrowsingTransferRttAverage,digits = 2), "of", rnk$maxHttpBrowsingTransferRttAverage), 
          maxValue              = max(rnk$ranHttpBrowsingTransferRttAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage), 
          inputHeader           = "HTTP Browsing RTT AVG Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkorchid2"), 
          inputKPIs             = "HTTP Browsing RTT AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage),
          inputLabel            = paste(round((rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage),digits = 2), "of", rnk$maxHttpBrowsingTransferRttAverage), 
          maxValue              = max(rnk$maxHttpBrowsingTransferRttAverage - rnk$ranHttpBrowsingTransferRttAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostHttpBrowsingTransferRttAverage, 
          inputHeader           = "HTTP Browsing RTT AVG Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkorchid2"), 
          inputKPIs             = "HTTP Browsing RTT AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostHttpBrowsingTransferRttAverage,
          inputLabel            = paste(round(rnk$lostHttpBrowsingTransferRttAverage,digits = 2), "of", rnk$maxHttpBrowsingTransferRttAverage), 
          maxValue              = max(rnk$lostHttpBrowsingTransferRttAverage) )
  
  # VIDEO STREAM RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranStreamingCompletedRatio, rnk$ranStreamingVmosPercentile10, rnk$ranStreamingVmosAverage, rnk$ranStreamingTtfpPoorRatio, rnk$ranStreamingTtfpAverage )), 
           inputHeader   = "Video Stream Ranking", 
           inputNames    = names,
           inputColors   = c( "dodgerblue" , "lightblue1", "lightblue2", "royalblue1","royalblue2"), 
           inputKPIs     = c("Streaming Completed Ratio", "Streaming vMOS P10", "Streaming vMOS AVG", "Streaming TTFP Poor Ratio", "Streaming TTFP AVG"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranStreamingVmosPercentile10 + rnk$ranStreamingCompletedRatio + rnk$ranStreamingVmosAverage + rnk$ranStreamingTtfpPoorRatio + rnk$ranStreamingTtfpAverage ) ),
           inputLabel    = paste( round( ( rnk$ranStreamingVmosPercentile10 + rnk$ranStreamingCompletedRatio + rnk$ranStreamingVmosAverage + rnk$ranStreamingTtfpPoorRatio + rnk$ranStreamingTtfpAverage ), digits = 2 ),
                                  "of",
                                  (rnk$maxStreamingVmosPercentile10 + rnk$maxStreamingCompletedRatio + rnk$maxStreamingVmosAverage + rnk$maxStreamingTtfpPoorRatio+ rnk$maxStreamingTtfpAverage ),
                                  "\n",
                                  round( rnk$ranStreamingCompletedRatio , digits = 2 ),
                                  "+",
                                  round( rnk$ranStreamingVmosPercentile10 , digits = 2 ),
                                  "+",
                                  round( rnk$ranStreamingVmosAverage , digits = 2 ),
                                  "+",
                                  round( rnk$ranStreamingTtfpPoorRatio , digits = 2 ),
                                  "+",
                                  round( rnk$ranStreamingTtfpAverage , digits = 2 )
           ),
           maxValue          = max(rnk$ranStreamingVmosPercentile10 + rnk$ranStreamingCompletedRatio + rnk$ranStreamingVmosAverage + rnk$ranStreamingTtfpPoorRatio + rnk$ranStreamingTtfpAverage) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio), (rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10), (rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage), (rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio), (rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage) )), 
           inputHeader   = "Video Stream Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "dodgerblue" , "lightblue1", "lightblue2", "royalblue1","royalblue2"), 
           inputKPIs     = c("Streaming Completed Ratio", "Streaming vMOS P10", "Streaming vMOS AVG", "Streaming TTFP Poor Ratio", "Streaming TTFP AVG"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10) + (rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio) + (rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage) + (rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio) + (rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage) ) ),
           inputLabel    = paste( round( ( (rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10) + (rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio) + (rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage) + (rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio) + (rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage) ), digits = 2 ),
                                  "of",
                                  (rnk$maxStreamingVmosPercentile10 + rnk$maxStreamingCompletedRatio + rnk$maxStreamingVmosAverage + rnk$maxStreamingTtfpPoorRatio + rnk$maxStreamingTtfpAverage),
                                  "\n",
                                  round( (rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage) , digits = 2 )
           ),
           maxValue          = max((rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10) + (rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio) + (rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage) + (rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio) + (rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostStreamingCompletedRatio, rnk$lostStreamingVmosPercentile10, rnk$lostStreamingVmosAverage, rnk$lostStreamingTtfpPoorRatio, rnk$lostStreamingTtfpAverage )), 
           inputHeader   = "Video Stream Points Lost", 
           inputNames    = names,
           inputColors   = c( "dodgerblue" , "lightblue1", "lightblue2", "royalblue1","royalblue2"), 
           inputKPIs     = c("Streaming Completed Ratio", "Streaming vMOS P10", "Streaming vMOS AVG", "Streaming TTFP Poor Ratio", "Streaming TTFP AVG"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostStreamingVmosPercentile10 + rnk$lostStreamingCompletedRatio + rnk$lostStreamingVmosAverage + rnk$lostStreamingTtfpPoorRatio + rnk$lostStreamingTtfpAverage ) ),
           inputLabel    = paste( round( ( rnk$lostStreamingVmosPercentile10 + rnk$lostStreamingCompletedRatio + rnk$lostStreamingVmosAverage + rnk$lostStreamingTtfpPoorRatio + rnk$lostStreamingTtfpAverage ), digits = 2 ),
                                  "of",
                                  (rnk$maxStreamingVmosPercentile10 + rnk$maxStreamingCompletedRatio + rnk$maxStreamingVmosAverage + rnk$maxStreamingTtfpPoorRatio + rnk$maxStreamingTtfpAverage ),
                                  "\n",
                                  round( rnk$lostStreamingCompletedRatio , digits = 2 ),
                                  "+",
                                  round( rnk$lostStreamingVmosPercentile10 , digits = 2 ),
                                  "+",
                                  round( rnk$lostStreamingVmosAverage , digits = 2 ),
                                  "+",
                                  round( rnk$lostStreamingTtfpPoorRatio , digits = 2 ),
                                  "+",
                                  round( rnk$lostStreamingTtfpAverage , digits = 2 )
           ),
           maxValue          = max(rnk$lostStreamingVmosPercentile10 + rnk$lostStreamingCompletedRatio + rnk$lostStreamingVmosAverage + rnk$lostStreamingTtfpPoorRatio + rnk$lostStreamingTtfpAverage) )
  
  # Video Streaming Completed Ratio RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranStreamingCompletedRatio, 
          inputHeader           = "Video Streaming Completed Ratio Ranking", 
          inputNames            = names, 
          inputColors           = c("dodgerblue"), 
          inputKPIs             = "Video Streaming Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranStreamingCompletedRatio,
          inputLabel            = paste(round(rnk$ranStreamingCompletedRatio,digits = 2), "of", rnk$maxStreamingCompletedRatio), 
          maxValue              = max(rnk$ranStreamingCompletedRatio) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio), 
          inputHeader           = "Video Streaming Completed Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("dodgerblue"), 
          inputKPIs             = "Video Streaming Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio),
          inputLabel            = paste(round((rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio),digits = 2), "of", rnk$maxStreamingCompletedRatio), 
          maxValue              = max(rnk$maxStreamingCompletedRatio - rnk$ranStreamingCompletedRatio) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostStreamingCompletedRatio, 
          inputHeader           = "Video Streaming Completed Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("dodgerblue"), 
          inputKPIs             = "Video Streaming Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostStreamingCompletedRatio,
          inputLabel            = paste(round(rnk$lostStreamingCompletedRatio,digits = 2), "of", rnk$maxStreamingCompletedRatio), 
          maxValue              = max(rnk$lostStreamingCompletedRatio) )
  
  # Video Streaming vMOS P10 RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranStreamingVmosPercentile10, 
          inputHeader           = "Video Streaming vMOS P10 Ranking", 
          inputNames            = names, 
          inputColors           = c("lightblue1"), 
          inputKPIs             = "Video Streaming vMOS P10", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranStreamingVmosPercentile10,
          inputLabel            = paste(round(rnk$ranStreamingVmosPercentile10,digits = 2), "of", rnk$maxStreamingVmosPercentile10), 
          maxValue              = max(rnk$ranStreamingVmosPercentile10) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10), 
          inputHeader           = "Video Streaming vMOS P10 Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("lightblue1"), 
          inputKPIs             = "Video Streaming vMOS P10", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10),
          inputLabel            = paste(round((rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10),digits = 2), "of", rnk$maxStreamingVmosPercentile10), 
          maxValue              = max(rnk$maxStreamingVmosPercentile10 - rnk$ranStreamingVmosPercentile10) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostStreamingVmosPercentile10, 
          inputHeader           = "Video Streaming vMOS P10 Points Lost", 
          inputNames            = names, 
          inputColors           = c("lightblue1"), 
          inputKPIs             = "Video Streaming vMOS P10", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostStreamingVmosPercentile10,
          inputLabel            = paste(round(rnk$lostStreamingVmosPercentile10,digits = 2), "of", rnk$maxStreamingVmosPercentile10), 
          maxValue              = max(rnk$lostStreamingVmosPercentile10) )
  
  # Video Streaming vMOS AVG RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranStreamingVmosAverage, 
          inputHeader           = "Video Streaming vMOS AVG Ranking", 
          inputNames            = names, 
          inputColors           = c("lightblue2"), 
          inputKPIs             = "Video Streaming vMOS AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranStreamingVmosAverage,
          inputLabel            = paste(round(rnk$ranStreamingVmosAverage,digits = 2), "of", rnk$maxStreamingVmosAverage), 
          maxValue              = max(rnk$ranStreamingVmosAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage), 
          inputHeader           = "Video Streaming vMOS AVG Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("lightblue2"), 
          inputKPIs             = "Video Streaming vMOS AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage),
          inputLabel            = paste(round((rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage),digits = 2), "of", rnk$maxStreamingVmosAverage), 
          maxValue              = max(rnk$maxStreamingVmosAverage - rnk$ranStreamingVmosAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostStreamingVmosAverage, 
          inputHeader           = "Video Streaming vMOS AVG Points Lost", 
          inputNames            = names, 
          inputColors           = c("lightblue2"), 
          inputKPIs             = "Video Streaming vMOS AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostStreamingVmosAverage,
          inputLabel            = paste(round(rnk$lostStreamingVmosAverage,digits = 2), "of", rnk$maxStreamingVmosAverage), 
          maxValue              = max(rnk$lostStreamingVmosAverage) )
  
  # Video Streaming Poor TTFP Ratio RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranStreamingTtfpPoorRatio, 
          inputHeader           = "Video Streaming Poor TTFP Ratio Ranking", 
          inputNames            = names, 
          inputColors           = c("royalblue1"), 
          inputKPIs             = "Video Streaming Poor TTFP Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranStreamingTtfpPoorRatio,
          inputLabel            = paste(round(rnk$ranStreamingTtfpPoorRatio,digits = 2), "of", rnk$maxStreamingTtfpPoorRatio), 
          maxValue              = max(rnk$ranStreamingTtfpPoorRatio) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio), 
          inputHeader           = "Video Streaming Poor TTFP Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("royalblue1"), 
          inputKPIs             = "Video Streaming Poor TTFP Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio),
          inputLabel            = paste(round((rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio),digits = 2), "of", rnk$maxStreamingTtfpPoorRatio), 
          maxValue              = max(rnk$maxStreamingTtfpPoorRatio - rnk$ranStreamingTtfpPoorRatio) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostStreamingTtfpPoorRatio, 
          inputHeader           = "Video Streaming Poor TTFP Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("royalblue1"), 
          inputKPIs             = "Video Streaming Poor TTFP Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostStreamingTtfpPoorRatio,
          inputLabel            = paste(round(rnk$lostStreamingTtfpPoorRatio,digits = 2), "of", rnk$maxStreamingTtfpPoorRatio), 
          maxValue              = max(rnk$lostStreamingTtfpPoorRatio) )
  # Video Streaming TTFP AVG RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranStreamingTtfpAverage, 
          inputHeader           = "Video Streaming TTFP AVG Ranking", 
          inputNames            = names, 
          inputColors           = c("royalblue2"), 
          inputKPIs             = "Video Streaming TTFP AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranStreamingTtfpAverage,
          inputLabel            = paste(round(rnk$ranStreamingTtfpAverage,digits = 2), "of", rnk$maxStreamingTtfpAverage), 
          maxValue              = max(rnk$ranStreamingTtfpAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage), 
          inputHeader           = "Video Streaming TTFP AVG Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("royalblue2"), 
          inputKPIs             = "Video Streaming TTFP AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage),
          inputLabel            = paste(round((rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage),digits = 2), "of", rnk$maxStreamingTtfpAverage), 
          maxValue              = max(rnk$maxStreamingTtfpAverage - rnk$ranStreamingTtfpAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostStreamingTtfpAverage, 
          inputHeader           = "Video Streaming TTFP AVG Points Lost", 
          inputNames            = names, 
          inputColors           = c("royalblue2"), 
          inputKPIs             = "Video Streaming TTFP AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostStreamingTtfpAverage,
          inputLabel            = paste(round(rnk$lostStreamingTtfpAverage,digits = 2), "of", rnk$maxStreamingTtfpAverage), 
          maxValue              = max(rnk$lostStreamingTtfpAverage) )
  
  # Mobile Application RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$ranApplicationCompletedRatio, rnk$ranApplicationTransferTimeAverage )), 
           inputHeader   = "Mobile Application Ranking", 
           inputNames    = names,
           inputColors   = c( "darkseagreen1" , "darkseagreen2"), 
           inputKPIs     = c("Mobile Application Success Ratio", "Mobile Application Transfer Time AVG"), 
           legendFlag     = "Yes", 
           inputLabelPosition   = t(cbind( rnk$ranApplicationTransferTimeAverage + rnk$ranApplicationCompletedRatio ) ),
           inputLabel    = paste( round( ( rnk$ranApplicationTransferTimeAverage + rnk$ranApplicationCompletedRatio ), digits = 2 ),
                                  "of",
                                  (rnk$maxApplicationTransferTimeAverage + rnk$maxApplicationCompletedRatio ),
                                  "\n",
                                  round( rnk$ranApplicationCompletedRatio , digits = 2 ),
                                  "+",
                                  round( rnk$ranApplicationTransferTimeAverage , digits = 2 )
           ),
           maxValue          = max(rnk$ranApplicationTransferTimeAverage + rnk$ranApplicationCompletedRatio) )
  
  # GAP TO MAX
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( (rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio), (rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage) )), 
           inputHeader   = "Mobile Application Gap to MAX", 
           inputNames    = names,
           inputColors   = c( "darkseagreen1" , "darkseagreen2"), 
           inputKPIs     = c("Mobile Application Success Ratio", "Mobile Application Transfer Time AVG"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( (rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage) + (rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio) ) ),
           inputLabel    = paste( round( ( (rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage) + (rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio) ), digits = 2 ),
                                  "of",
                                  (rnk$maxApplicationTransferTimeAverage + rnk$maxApplicationCompletedRatio ),
                                  "\n",
                                  round( (rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio) , digits = 2 ),
                                  "+",
                                  round( (rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage) , digits = 2 )
           ),
           maxValue          = max((rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage) + (rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio)) )
  
  # Points Lost
  par(mar=c(5,15,5,10)+.1)
  plotBar( inputTable    = t(cbind( rnk$lostApplicationCompletedRatio, rnk$lostApplicationTransferTimeAverage )), 
           inputHeader   = "Mobile Application Points Lost", 
           inputNames    = names,
           inputColors   = c( "darkseagreen1" , "darkseagreen2"), 
           inputKPIs     = c("Mobile Application Success Ratio", "Mobile Application Transfer Time AVG"), 
           legendFlag     = "No", 
           inputLabelPosition   = t(cbind( rnk$lostApplicationTransferTimeAverage + rnk$lostApplicationCompletedRatio ) ),
           inputLabel    = paste( round( ( rnk$lostApplicationTransferTimeAverage + rnk$lostApplicationCompletedRatio ), digits = 2 ),
                                  "of",
                                  (rnk$maxApplicationTransferTimeAverage + rnk$maxApplicationCompletedRatio ),
                                  "\n",
                                  round( rnk$lostApplicationCompletedRatio , digits = 2 ),
                                  "+",
                                  round( rnk$lostApplicationTransferTimeAverage , digits = 2 )
           ),
           maxValue          = max(rnk$lostApplicationTransferTimeAverage + rnk$lostApplicationCompletedRatio) )
  
  # Mobile Application Completed Ratio RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranApplicationCompletedRatio, 
          inputHeader           = "Mobile Application Completed Ratio Ranking", 
          inputNames            = names, 
          inputColors           = c("darkseagreen1"), 
          inputKPIs             = "Mobile Application Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranApplicationCompletedRatio,
          inputLabel            = paste(round(rnk$ranApplicationCompletedRatio,digits = 2), "of", rnk$maxApplicationCompletedRatio), 
          maxValue              = max(rnk$ranApplicationCompletedRatio) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio), 
          inputHeader           = "Mobile Application Completed Ratio Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkseagreen1"), 
          inputKPIs             = "Mobile Application Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio),
          inputLabel            = paste(round((rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio),digits = 2), "of", rnk$maxApplicationCompletedRatio), 
          maxValue              = max(rnk$maxApplicationCompletedRatio - rnk$ranApplicationCompletedRatio) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostApplicationCompletedRatio, 
          inputHeader           = "Mobile Application Completed Ratio Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkseagreen1"), 
          inputKPIs             = "Mobile Application Completed Ratio", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostApplicationCompletedRatio,
          inputLabel            = paste(round(rnk$lostApplicationCompletedRatio,digits = 2), "of", rnk$maxApplicationCompletedRatio), 
          maxValue              = max(rnk$lostApplicationCompletedRatio) )
  
  # Mobile Application Transfer Time AVG RANKING
  par(mar=c(5,20,5,10)+.1)
  plotBar(inputTable            = rnk$ranApplicationTransferTimeAverage, 
          inputHeader           = "Mobile Application Transfer Time AVG Ranking", 
          inputNames            = names, 
          inputColors           = c("darkseagreen2"), 
          inputKPIs             = "Mobile Application Transfer Time AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$ranApplicationTransferTimeAverage,
          inputLabel            = paste(round(rnk$ranApplicationTransferTimeAverage,digits = 2), "of", rnk$maxApplicationTransferTimeAverage), 
          maxValue              = max(rnk$ranApplicationTransferTimeAverage) )
  
  # GAP TO MAXIMUM
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = (rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage), 
          inputHeader           = "Mobile Application Transfer Time AVG Gap to MAX", 
          inputNames            = names, 
          inputColors           = c("darkseagreen2"), 
          inputKPIs             = "Mobile Application Transfer Time AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = (rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage),
          inputLabel            = paste(round((rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage),digits = 2), "of", rnk$maxApplicationTransferTimeAverage), 
          maxValue              = max(rnk$maxApplicationTransferTimeAverage - rnk$ranApplicationTransferTimeAverage) )
  
  # POINTS LOST
  par(mar=c(5,15,5,10)+.1)
  plotBar(inputTable            = rnk$lostApplicationTransferTimeAverage, 
          inputHeader           = "Mobile Application Transfer Time AVG Points Lost", 
          inputNames            = names, 
          inputColors           = c("darkseagreen2"), 
          inputKPIs             = "Mobile Application Transfer Time AVG", 
          legendFlag            = "No", 
          inputLabelPosition    = rnk$lostApplicationTransferTimeAverage,
          inputLabel            = paste(round(rnk$lostApplicationTransferTimeAverage,digits = 2), "of", rnk$maxApplicationTransferTimeAverage), 
          maxValue              = max(rnk$lostApplicationTransferTimeAverage) )
  
  dev.off()
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
kpiReport1 <-    dbGetQuery(con,paste("SELECT * FROM",resultTable))
Encoding(kpiReport1$G_Level_4) = "latin1"

extract <- subset(kpiReport1, 
                  Rnk == 100 | Rnk == 200 | Rnk == 300 | Rnk == 400,
                  select = Rnk:lostRanking)

rnkGeneral(extract, 2, "L2_100.png")
rnkVoice(extract,   2, "L2_101.png")
rnkData(extract,    2, "L2_102.png")

extract <- subset(kpiReport1, 
                  Rnk > 500,
                  select = Rnk:lostRanking)

extract <- subset(extract, 
                  Rnk < 1000,
                  select = Rnk:lostRanking)
