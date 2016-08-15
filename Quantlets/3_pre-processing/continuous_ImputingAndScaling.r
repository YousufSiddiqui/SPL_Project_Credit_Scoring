rm(list=ls())

user=(Sys.info()[6])
Desktop=paste("C:/Users/",user,"/Desktop/",sep="")


home=Desktop
setwd(home)

##Creating Working Folder
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




####Handling Continuous Variables

dat_cont=dat[, setdiff(1:20,grep("A",dat))]



###Handling Outliers
## Function for replacing outlier on Z value>3 with NA
remove_outliers = function(x, na.rm = TRUE, ...) 
{
  VarMean = mean(x, na.rm = na.rm, ...)
  VarSD = sd(x,na.rm = na.rm, ...)
  y = x
  y[abs((x - VarMean)/VarSD) >3] = NA
  y
}


##applying above function

dat_cont= sapply(dat_cont,function(x)remove_outliers(x))


##Checking number of NAs
table(is.na(dat_cont))


##Imputing NA with mean of the Variables


means = colMeans(dat_cont[,colnames(dat_cont)],na.rm=T)
for(idv in colnames(dat_cont)) dat_cont[is.na(dat_cont[,idv]),idv] =means[idv]


##Checking if any NAs left
table(is.na(dat_cont))




###Scaling of Continuous variables

##MinMaxScaling (-1 to 1)

min_max_scaling=function(col)((col-min(col))/(max(col)-min(col))*2-1)

##check
min_max_scaling(c(1,2,3,4))
# [1] -1.0000000 -0.3333333  0.3333333  1.0000000

dat_cont=as.data.frame(dat_cont)	
dat_cont2 = sapply(dat_cont,function(col)min_max_scaling(col))

##saving
write.csv(dat_cont2,"preproc_data/cont_data.csv",row.names=F)