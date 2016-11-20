library(randomForest)
#library(help=randomForest)
#help(randomForest)
# library(plyr)
# library(crossval)
# library(varhandle)




TrimCharVec <- function(colName)
{
  val <- substr(colName, 1, nchar(colName))
  return(val)
}




ApplyToBranch <- function(col){
  retval <- ''
  if (grep('thakur',tolower(col)) == 1){retval <- 'biomedical'}
  if (grep('biotech',tolower(col)) == 1){retval <- 'biotechnology'}
  if (grep('chemical',tolower(col)) == 1){retval <- 'chemical'}
}


TCollegeName <- function(x){
  
  if (length(i <- grep('thakur', x))){return('thakur college')}
  else if (length(i <- grep('thakur', x))){return('thakur college')}
  else if (length(i <- grep('atharva', x))){return('atharva college')}
  else if (length(i <- grep('sardar patel', x))){return('sardar patel college')}
  else if (length(i <- grep('bharti vidyapeeth', x))){return('bharti vidyapeeth college')}
  else if (length(i <- grep('d j', x))){return('dj sanghvi college')}
  else if (length(i <- grep('dj', x))){return('dj sanghvi college')}
  else if (length(i <- grep('datta meghe', x))){return('datta meghe college')}
  else if (length(i <- grep('dwarkadas', x))){return('dwarkadas sanghvi college')}
  else if (length(i <- grep('agnel', x))){return('father agnel college')}
  else if (length(i <- grep(tolower('Conceicao'), x))){return('father conceicao college')}
  else if (length(i <- grep(tolower('CRCE'), x))){return('father conceicao college')}
  else if (length(i <- grep('somaiya', x))){return('kj somaiya college')}
  else if (length(i <- grep('francis', x))){return('st francis college')}
  else if (length(i <- grep('vjti', x))){return('vjti college')}
  else if (length(i <- grep('terna', x))){return('terna college')}
  else if (length(i <- grep('padmabhusan', x))){return('padmabhusan college')}
  else if (length(i <- grep('vivekanand', x))){return('vesit college')}
  else if (length(i <- grep('veermata', x))){return('veermata jijabai college')}
  else if (length(i <- grep('rajiv', x))){return('rajiv gandhi college')}
  else if (length(i <- grep('rgit', x))){return('rajiv gandhi college')}
  else if (length(i <- grep('watumull', x))){return('watumull college')}
  else if (length(i <- grep('thadomal', x))){return('thadomal college')}
  else if (length(i <- grep('xavier', x))){return('xavier college')}
  else{return('others')}
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





classifierRandomForest <- function(dev, val, actual_output, predict_output, varNamesForPred){
  #add + sign between explortory variables
  varnames <- paste(varNamesForPred, collapse = "+")
  #print(varnames)
  # Add output variable and convert to a formula object
  form <- as.formula(paste(actual_output, varnames, sep = " ~ "))
  
  rf <- randomForest(form, dev, ntree=500, importance=T)
  #plot(rf)
  dev[predict_output] <- predict(rf, dev)
  #confusion matrix for dev
  accuracy_dev <- confusionMatrix(dev, predictedCol = predict_output, actualCol = actual_output)
  val[predict_output] <- predict(rf, val)
  #confusion matrix for val
  accuracy_val <- confusionMatrix(val, predictedCol = predict_output, actualCol = actual_output)
  
  #save classifier in a file
  save(rf, file = paste('/media/sapan/ub-new/mast/kossine/classifiers/', actual_output,'.RData'))
  #saveRDS(rf, file = paste('/media/sapan/ub-new/mast/kossine/classifiers/', actual_output,'.RData'))
  
  return(list('a'=val, 'b'=dev, 'c'=accuracy_val, 'd'=accuracy_dev))
}




PreprocessData <- function(data){
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
  
  dataPrc$AOAD <- factor(dataPrc$AOAD)
  dataPrc$DSA <- factor(dataPrc$DSA)
  
  #correcting college name
  dataPrc$collegeName <- tolower(dataPrc$collegeName)
  dataPrc$collegeName <- sapply(X = dataPrc$collegeName, FUN = TCollegeName)
  
  dataPrc$collegeName <- factor(dataPrc$collegeName)
  #print(str(dataPrc))
  #adding predicted output columns
  dataPrc[predColNames] <- 0
  
  return(dataPrc)
}





#----------------------------------------------------------------------------------------------------------------------------------------
predColNames <- c('pred_OOPM_Java','pred_Data_Structures','pred_Android_Programming','pred_Web_Progamming_IP','pred_Automata_Theory_TCS',
                  'pred_Ethical_Hacking','pred_Circuits_Transm','pred_Advance_Java','pred_LADMIN','pred_Analog_2','pred_Applied_Math_2','pred_AOAD','pred_DSA')

#classify <- function(sample, real = NA){
classify <- function(){
  data <- read.csv('/media/sapan/ub-new/mast/kossine/SampleSPAData.csv')
  #data <- read.csv(sample)
  dataPrc <- PreprocessData(data)
  
  #shuffle row-wise
  dataPrc <- dataPrc[sample(nrow(dataPrc)),]
  dev <- dataPrc[1:(nrow(dataPrc)*0.7),]
  val <- dataPrc[(nrow(dataPrc)*0.7):nrow(dataPrc),]
  
  varNamesForPred <- names(dev)
  varNamesForPred <- varNamesForPred[!varNamesForPred %in% c('collegeAddress','address','city','course','pincode')]#branch,collegeName
  varNamesForPred <- varNamesForPred[!varNamesForPred %in% predColNames]
  
  
  for (col in predColNames){
    tryCatch({
      actual_output = substr(col, 6, nchar(col))
      print(actual_output)
      predict_output = col
      varNamesForPred <- varNamesForPred[!varNamesForPred %in% actual_output]
      values <- classifierRandomForest(dev, val, actual_output, predict_output, varNamesForPred)
      val <-values$a
      dev <- values$b
      accuracy_val <- values$c
      accuracy_dev <- values$d
      varNamesForPred <- c(varNamesForPred, actual_output)
      print(c('accuracy:',accuracy_val))
    }, error = function(e){
      print(e)
      accuracy_val <- NA
      accuracy_dev <- NA
    }, finally = function(e){
      
    })
  }
  
  write.csv(file = '/media/sapan/ub-new/mast/kossine/result_dev.csv', x=dev,fileEncoding = 'utf-8')
  write.csv(file = '/media/sapan/ub-new/mast/kossine/result_val.csv', x=val)
}

#load classifiers
folderpath = '/media/sapan/ub-new/mast/kossine/classifiers/'
train <- read.csv('/media/sapan/ub-new/mast/kossine/SampleSPAData.csv')
real <- read.csv('/media/sapan/ub-new/mast/kossine/InputData.csv')
train <- PreprocessData(train)
real <- PreprocessData(real)



levels(real$course) <- levels(train$course)
levels(real$first_name) <- levels(train$first_name)

levels(real$last_name) <- levels(train$last_name)

levels(real$stream) <- levels(train$stream)

levels(real$branch) <- levels(train$branch)
levels(real$collegeName) <- levels(train$collegeName)
levels(real$collegeAddress) <- levels(train$collegeAddress)
levels(real$gender) <- levels(train$gender)
levels(real$address) <- levels(train$address)
levels(real$city) <- levels(train$city)
levels(real$pincode) <- levels(train$pincode)
levels(real$OOPM_Java) <- levels(train$OOPM_Java)
levels(real$Data_Structures) <- levels(train$Data_Structures)
levels(real$AOAD) <- levels(train$AOAD)
levels(real$Android_Programming) <- levels(train$Android_Programming)
levels(real$DSA) <- levels(train$DSA)
levels(real$Web_Progamming_IP) <- levels(train$Web_Progamming_IP)
levels(real$Automata_Theory_TCS) <- levels(train$Automata_Theory_TCS)
levels(real$Ethical_Hacking) <- levels(train$Ethical_Hacking)
levels(real$Circuits_Transm) <- levels(train$Circuits_Transm)
levels(real$Advance_Java) <- levels(train$Advance_Java)
levels(real$LADMIN) <- levels(train$LADMIN)
levels(real$Analog_2) <- levels(train$Analog_2)
levels(real$Applied_Math_2) <- levels(train$Applied_Math_2)



for (col in predColNames){
  tryCatch({
    name = substr(col, 6, nchar(col))
    load(file = paste(folderpath, name,'.RData', sep = ''))
    real[predColNames] <- predict(rf, real)
    print(col)
    #confusion matrix for val
    accuracy_val <- confusionMatrix(real, predictedCol = col, actualCol = name)
    print(c('accuracy:',accuracy_val))
  }, error = function(e){
    print(e)
  }, finally = function(e){})
}

classify()