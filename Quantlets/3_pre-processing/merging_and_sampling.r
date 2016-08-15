####For running this code successfully, please make sure the categorical_dummy_encoding.r and continuous_ImputingAndScaling.r have been ran already.

rm(list=ls())

user=(Sys.info()[6])
Desktop=paste("C:/Users/",user,"/Desktop/",sep="")



home=paste(Desktop,"MEMS/S4/R Programming/German/",sep="")
setwd(home)


#Loading the data
dat=read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", as.is=T,header=F,sep=" ")

setwd(paste(home,'Model',sep=''))

colnames(dat)[21]=paste('dv')

dat$id=rownames(dat)

dat_cat = read.csv("preproc_data/cat_data.csv",as.is=T)
 
dat_cont2 = read.csv("preproc_data/cont_data.csv",as.is=T)


##Combining all datasets 
data_final=cbind(dat[,c("id","dv")],dat_cont2,dat_cat)

##Changing Target variable's values from 1 and 2  to 0 and 1 resp. as it is more comprehensible form for logR algorithm
data_final$dv=data_final$dv-1



### Sampling into training, validation and test datasets.

## We would want to have similar ratio of goods vs bads in all the samples to maintain for unbiased training and testing. CARET package provides this functionality with control over target variable.
library(caret)

set.seed(3456)

##dividing rows in 5 folds with control over target variable 
devIndex = as.data.frame(createFolds(data_final$dv, k=5,  list = TRUE))

data_div_1= data_final[ devIndex$Fold1, c("id","dv")]
data_div_2= data_final[ devIndex$Fold2, c("id","dv")]
data_div_3= data_final[ devIndex$Fold3, c("id","dv")]
data_div_4= data_final[ devIndex$Fold4, c("id","dv")]
data_div_5= data_final[ devIndex$Fold5, c("id","dv")]


md_rows = rbind(data_div_1,data_div_2,data_div_3) ## Training dataset 60%
val_rows = data_div_4 							  ## Validation dataset 20%
test_rows = data_div_5 							  ## Test dataset 20%

data_md = merge( data_final , md_rows,by=c('id','dv'))
data_val = merge( data_final , val_rows,by=c('id','dv'))
data_test = merge( data_final , test_rows,by=c('id','dv'))

data_md$sample="train"
data_val$sample="val"
data_test$sample='test'


full_dv_data= rbind(data_md[,(colnames(data_md))], data_val[,(colnames(data_val))], data_test[,(colnames(data_test))])
class(full_dv_data$id)


full_dv_data$id=as.numeric(full_dv_data$id)
class(full_dv_data$id)
head(full_dv_data)


full_dv_data=full_dv_data[order(full_dv_data[,'id']),]

dim(full_dv_data)
length(unique(full_dv_data$id))

write.csv(full_dv_data,"preproc_data/dv.csv",row.names=F)
data_final=full_dv_data




###removing 0 variance predictors
data_final = data_final[sapply(data_final, function(x) length(levels(factor(x,exclude=NULL)))>1)]

nzv=nearZeroVar(data_final)
nzv
#11 16 17 23 26 27 29 34 46 53 58


if(length(nzv)>0) data_final=data_final[,-nzv]

###Separating data sets and removing Sample and dv_binary column

##Checking for the target variable distribution in over all dataset
nrow(data_final[data_final$dv==0,])/nrow(data_final)*100
#70

data_final_train=data_final[data_final$sample=='train',c(1:ncol(data_final)-1)]  
nrow(data_final_train[data_final_train$dv==0,])/nrow(data_final_train)*100
#70.333

data_final_val=data_final[data_final$sample=='val',c(1:ncol(data_final)-1)]
nrow(data_final_val[data_final_val$dv==0,])/nrow(data_final_val)*100
#71.5

data_final_test=data_final[data_final$sample=='test',c(1:ncol(data_final)-1)]
nrow(data_final_test[data_final_test$dv==0,])/nrow(data_final_test)*100
#67.5


### taking output in CSV format
write.csv(data_final_train,"preproc_data/train.csv",row.names=F)
write.csv(data_final_val,"preproc_data/val.csv",row.names=F)
write.csv(data_final_test,"preproc_data/test.csv",row.names=F)

