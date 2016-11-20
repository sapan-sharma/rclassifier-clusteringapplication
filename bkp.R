library(neuralnet)


TrimCharVec <- function(colName)
{
  val <- substr(colName, 1, nchar(colName))
  return(val)
}



ApplyToBranch <- function(col){
  retval <- ''
  if (grep('biomedical',tolower(col)) == 1){retval <- 'biomedical'}
  if (grep('biotech',tolower(col)) == 1){retval <- 'biotechnology'}
  if (grep('chemical',tolower(col)) == 1){retval <- 'chemical'}
}



#FP  TP  TN  FN 
confusionMatrix <- function(df, predictedCol, actualCol){
  fp <- nrow(subset(df, df[predictedCol] == 1 & df[actualCol] == 0))
  tp <- nrow(subset(df, df[predictedCol] == 1 & df[actualCol] == 1))
  tn <- nrow(subset(df, df[predictedCol] == 0 & df[actualCol] == 0))
  fn <- nrow(subset(df, df[predictedCol] == 0 & df[actualCol] == 1))
  total <- nrow(df)
  accuracy = (tp+tn)/total
  return(accuracy)
}



classifierNeuralNetwork <- function(dev, val, actual_output, predict_output, varnames){
  #add + sign between explortory variables
  varnames <- c(varnames,actual_output)
  varnames <- paste(varNamesForPred, collapse = "+")
  # Add output variable and convert to a formula object
  form <- as.formula(paste(actual_output, varnames, sep = " ~ "))
  m <- model.matrix(form, dev)
  
  varnames_mat <- colnames(m)
  varnames_mat_1 <- gsub('([[:punct:]])|\\s+','_',varnames_mat)
  colnames(m) <- varnames_mat_1
  
  dev[actual_output] <- sapply(dev[actual_output], as.numeric)
  
  m1 <- cbind(m, dev[actual_output])
  
  varnames_mat_1 <- varnames_mat_1[!varnames_mat_1 %in% '_Intercept_']
  varnames_mat_1 <- paste(varnames_mat_1 , collapse = '+')
  
  form_mat <- as.formula(paste(actual_output, varnames_mat_1, sep=" ~ "))
  nn <- neuralnet(form_mat, data=m1, hidden=c(5,3))
  
  f1 <- compute(nn, m1[,2:64])
  result <- sapply(f1$net.result, round)
  dev[predict_output] <- result
  
  #confusion matrix for dev
  accuracy_dev <- confusionMatrix(dev, predictedCol = predict_output, actualCol = actual_output)
  
  #val[predict_output] <- compute(nn, val)
  #confusion matrix for val
  accuracy_val <- confusionMatrix(val, predictedCol = predict_output, actualCol = actual_output)
  
  save(nn, file = paste('/media/sapan/ub-new/mast/kossine/classifiers/', actual_output,'_neuralNet.RData', sep = ''))
  
  return(list('a'=val, 'b'=dev, 'c'=accuracy_val, 'd'=accuracy_dev))
}




#----------------------------------------------------------------------------------------------------------------------------------------

data <- read.csv('/media/sapan/ub-new/mast/kossine/SampleSPAData.csv')

courses <- c('OOPM (Java)','Data Structures (DSF)','AOAD','Android Programming','DSA','Web Progamming / IP','Automata Theory / TCS',
             'DBMS','Ethical Hacking','Circuit And Transmission Lines','Data Structures (DSF)','Advance Java',
             'Linux Administration','Analog Electronics-2','Applied Mathematics-2')
dataPrc <- data
#adding actual output columns
for (course in courses){
  dataPrc[course] <- 0
}
#filling values in actual output columns
out <- strsplit(as.character(data$course), ", ")
i<-1
for (cname in out){
  #print(i)
  for (inner in seq(1, length(cname), 1)){
    colName <- TrimCharVec(cname[[inner]])
    if(colName %in% colnames(dataPrc)){
      dataPrc[i, colName] <- 1
    }
  }
  i<-i+1
}
#correcting actual output column names
names(dataPrc)[names(dataPrc)=='OOPM (Java)'] <- 'OOPM_Java'
dataPrc$OOPM_Java <- factor(dataPrc$OOPM_Java)
names(dataPrc)[names(dataPrc)=='Data Structures (DSF)']<-'Data_Structures'
dataPrc$Data_Structures <- factor(dataPrc$Data_Structures)
names(dataPrc)[names(dataPrc)=='Android Programming']<-'Android_Programming'
dataPrc$Android_Programming <- factor(dataPrc$Android_Programming)
names(dataPrc)[names(dataPrc)=='Web Progamming / IP']<-'Web_Progamming_IP'
dataPrc$Web_Progamming_IP <- factor(dataPrc$Web_Progamming_IP)
names(dataPrc)[names(dataPrc)=='Automata Theory / TCS']<-'Automata_Theory_TCS'
dataPrc$Automata_Theory_TCS <- factor(dataPrc$Automata_Theory_TCS)
names(dataPrc)[names(dataPrc)=='Ethical Hacking']<-'Ethical_Hacking'
dataPrc$Ethical_Hacking <- factor(dataPrc$Ethical_Hacking)
names(dataPrc)[names(dataPrc)=='Circuit And Transmission Lines']<-'Circuits_Transm'
dataPrc$Circuits_Transm <- factor(dataPrc$Circuits_Transm)
names(dataPrc)[names(dataPrc)=='Advance Java']<-'Advance_Java'
dataPrc$Advance_Java <- factor(dataPrc$Advance_Java)
names(dataPrc)[names(dataPrc)=='Linux Administration']<-'LADMIN'
dataPrc$LADMIN <- factor(dataPrc$LADMIN)
names(dataPrc)[names(dataPrc)=='Analog Electronics-2']<-'Analog_2'
dataPrc$Analog_2 <- factor(dataPrc$Analog_2)
names(dataPrc)[names(dataPrc)=='Applied Mathematics-2']<-'Applied_Math_2'
dataPrc$Applied_Math_2 <- factor(dataPrc$Applied_Math_2)

#adding predicted output columns
predColNames <- c('pred_OOPM_Java','pred_Data_Structures','pred_Android_Programming','pred_Web_Progamming_IP','pred_Automata_Theory_TCS',
                  'pred_Ethical_Hacking','pred_Circuits_Transm','pred_Advance_Java','pred_LADMIN','pred_Analog_2','pred_Applied_Math_2')
dataPrc[predColNames] <- 0

#shuffle row-wise
dataPrc <- dataPrc[sample(nrow(dataPrc)),]
dev <- dataPrc[1:(nrow(dataPrc)*0.7),]
val <- dataPrc[(nrow(dataPrc)*0.7):nrow(dataPrc),]

varNamesForPred <- c('stream', 'branch', 'gender', 'city', 'Data_Structures', 'AOAD', 'Android_Programming', 'DSA', 'Web_Progamming_IP', 'Automata_Theory_TCS',
                     'DBMS', 'Ethical_Hacking')

v <- 'pred_OOPM_Java'
for (col in v){
  # tryCatch({
  #   actual_output = substr(col, 6, nchar(col))
  #   print(actual_output)
  #   predict_output = col
  #   varNamesForPred <- varNamesForPred[!varNamesForPred %in% actual_output]
  #   values <- classifierNeuralNetwork(dev, val, actual_output, predict_output, varNamesForPred)
  #   val <-values$a
  #   dev <- values$b
  #   accuracy_val <- values$c
  #   accuracy_dev <- values$d
  #   varNamesForPred <- c(varNamesForPred, actual_output)
  #   
  # }, error = function(e){
  #   print(e)
  #   accuracy_val <- NA
  #   accuracy_dev <- NA
  # }, finally = function(e){
  #   
  # })
  # print(accuracy_dev)
  # print(accuracy_val)
  actual_output = substr(col, 6, nchar(col))
  print(actual_output)
  predict_output = col
  varNamesForPred <- varNamesForPred[!varNamesForPred %in% actual_output]
  values <- classifierNeuralNetwork(dev, val, actual_output, predict_output, varNamesForPred)
  val <-values$a
  dev <- values$b
  accuracy_val <- values$c
  accuracy_dev <- values$d
  varNamesForPred <- c(varNamesForPred, actual_output)
  print(c('accuracy:',accuracy_val))
}

write.csv(file = '/media/sapan/ub-new/mast/kossine/result_dev.csv', x=dev,fileEncoding = 'utf-8')
write.csv(file = '/media/sapan/ub-new/mast/kossine/result_val.csv', x=val)
