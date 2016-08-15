rm(list=ls())

user=(Sys.info()[6])
Desktop=paste("C:/Users/",user,"/Desktop/",sep="")


home=Desktop
setwd(home)

#Creating Working Folder
dir.create(paste(home,'MEMS',sep=''))
dir.create(paste(home,'MEMS/S4',sep=''))
dir.create(paste(home,'MEMS/S4/R Programming',sep=''))
dir.create(paste(home,'MEMS/S4/R Programming/German',sep=''))



home=paste(Desktop,"MEMS/S4/R Programming/German/",sep="")
setwd(home)


#Loading the data
dat=read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", as.is=T,header=F,sep=" ")

dir.create(paste(home,'Model',sep=''))

setwd(paste(home,'Model',sep=''))

dir.create("preproc_data")

 
#Categorical columns
grep("A",dat)


dat_cat=dat[,grep("A",dat)]

###Dummy Encoding

## Flags creation for categorical columns with 2 unique values
# After looking into data dictionary, we know which ones are such columns. Encoding them as 0 and 1

unique(dat_cat[,ncol(dat_cat)-1])

dat_cat[,ncol(dat_cat)-1]=ifelse((dat_cat[,ncol(dat_cat)-1])==unique(dat_cat[,ncol(dat_cat)-1])[[1]],1,0)

unique(dat_cat[,ncol(dat_cat)])

dat_cat[,ncol(dat_cat)]=ifelse((dat_cat[,ncol(dat_cat)])==unique(dat_cat[,ncol(dat_cat)])[[1]],1,0)


## Flags creation for categorical dataset with more than 2 values



library(caret)
dummies = dummyVars( ~ ., data = dat_cat[,1:(ncol(dat_cat)-2)])
category_data_dummies=as.data.frame(predict(dummies, newdata = dat_cat[,1:(ncol(dat_cat)-2)]))
dim(category_data_dummies)
head(category_data_dummies)

##Joining both kind of variables in one dataset
data_cat=cbind(dat_cat[,c(ncol(dat_cat)-1,ncol(dat_cat))],category_data_dummies)

#Saving
write.csv(data_cat,"preproc_data/cat_data.csv",row.names=F)