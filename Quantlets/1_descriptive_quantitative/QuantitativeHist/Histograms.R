library(formatR)
tidy_source(file = "Histogramoutput.R")

getwd()


C_D = read.table('german.data')

ncol(C_D)
nrow(C_D)

#Setting Variable Names
colnames(C_D) = c("Account_Status","Months","Credit_History","Credit_Purpose","Credit_Amount","Savings_Account"
                  ,"Employed_Since","Installment_Rate","Status_and_Sex","Other_Debtors","Residence_Since",
                  "Property","Age","Other_Installement_Plans","Housing_Type","Existing_Credit","Job_Type",
                  "Dependents","Telephone","Foreign_Worker","Cost_Matrix")



#####Histograms######
graphics.off()


par(mfrow=c(3,3))

#Variable: Months

g1 = C_D$Months
m1 = mean(g1)
std1 = sqrt(var(g1))
h1 = hist(g1,col ="gray",breaks = 10, prob=TRUE,
          ylim = c(0,0.05),xlim = c(min(C_D$Months),max(C_D$Months)),xlab = "No.of Months", 
          main = "Histogram of Variable: Months")
c1 = curve(dnorm(x, mean=m1, sd=std1), 
      col="red", lwd=2, add=TRUE, yaxt="n")


#Variable:Age


g2 = C_D$Age
m2 = mean(g2)
std2 = sqrt(var(g2))
h2 = hist(g2,col = "gray",breaks = 20, prob= TRUE,
          ylim = c(0,0.05),xlim = c(min(C_D$Age),max(C_D$Age)),xlab = "Age in Years", 
          main = "Histogram of Variable: Age")
c2 = curve(dnorm(x, mean=m2, sd=std2), 
           col="red", lwd=2, add=TRUE, yaxt="n")
#Variable:Credit Amount

g3 = C_D$Credit_Amount
m3 = mean(g3)
std3 = sqrt(var(g3))
h3 = hist(g3,col = "gray",breaks = 20, prob= TRUE,
          ylim = c(0,0.00035),xlim = c(min(C_D$Credit_Amount),max(C_D$Credit_Amount)),xlab = "Credit Amount", 
          main = "Histogram of Variable: Credit Amount")
c3 = curve(dnorm(x, mean=m3, sd=std3), 
           col="red", lwd=2, add=TRUE, yaxt="n")

#Variable: Residence Since 

g4 = C_D$Residence_Since
m4 = mean(g4)
std4 = sqrt(var(g4))
h4 = hist(g4,col = "gray",breaks = 5,prob= TRUE,
          ylim = c(0,0.7),xlim = c(min(C_D$Residence_Since),max(C_D$Residence_Since)),xlab = "Residence_Since", 
          main = "Histogram of Variable: Residence Since")
c4 = curve(dnorm(x, mean=m4, sd=std4), 
           col="red", lwd=2, add=TRUE, yaxt="n")



#Variable: Installment Rate


g5 = C_D$Installment_Rate
m5 = mean(g5)
std5 = sqrt(var(g5))
h5 = hist(g5,col = "gray",breaks = 5,prob= TRUE,ylim = c(0,0.5),
          xlim = c(min(C_D$Installment_Rate),max(C_D$Installment_Rate)),xlab = "Installment Rate", 
          main = "Histogram of Variable: Installment Rate")
c5 = curve(dnorm(x, mean=m5, sd=std5), 
           col="red", lwd=2, add=TRUE, yaxt="n")

#Variable: Existing Credit


g6 = C_D$Existing_Credit
m6 = mean(g6)
std6 = sqrt(var(g6))
h6 = hist(g6,col = "gray",breaks = 5,prob= TRUE,ylim = c(0,1.5),
          xlim = c(min(C_D$Existing_Credit),max(C_D$Existing_Credit)),xlab = "Existing Credit", 
          main = "Histogram of Variable: Existing Credit")
c6 = curve(dnorm(x, mean=m6, sd=std6), 
           col="red", lwd=2, add=TRUE, yaxt="n")


#Variable: Dependents


g7 = C_D$Dependents
m7 = mean(g7)
std7 = sqrt(var(g7))
h7 = hist(g7,col = "gray",breaks = 4,prob= TRUE,ylim = c(0,4.5),
          xlim = c(min(C_D$Dependents),max(C_D$Dependents)),xlab = "Dependents", 
          main = "Histogram of Variable: Dependents")
c7 = curve(dnorm(x, mean=m7, sd=std7), 
           col="red", lwd=2, add=TRUE, yaxt="n")


