library(formatR)
tidy_source(file = "boxplot.R")

getwd()


C_D = read.table('german.data')

ncol(C_D)
nrow(C_D)
C_D$All = c("All")
#Setting Variable Names
colnames(C_D) = c("Account_Status","Months","Credit_History","Credit_Purpose","Credit_Amount","Savings_Account"
                  ,"Employed_Since","Installment_Rate","Status_and_Sex","Other_Debtors","Residence_Since",
                  "Property","Age","Other_Installement_Plans","Housing_Type","Existing_Credit","Job_Type",
                  "Dependents","Telephone","Foreign_Worker","Cost_Matrix")

Good = subset(C_D, Cost_Matrix == '1')
Bad = subset(C_D, Cost_Matrix == '2')


#####Boxplots######
graphics.off()

par(mfrow=c(3,2))


#Variable: Months
mmonths = mean(C_D$Months)

boxplot(C_D$Months, at = 1, xlim = c(0.5, 3.5), main="Boxplot of Variable: Months by Decision", 
        xlab="Decision", ylab="Months", col="gray", boxwex = 1.2)
axis(1, at=1, labels=c('All'))
boxplot(Good$Months, at = 2, add = TRUE, main="Boxplot of Variable: Months by Decision", 
        xlab="Decision", ylab="Months", col="chartreuse", boxwex = 1.2)
axis(1, at=2, labels=c('Good'))
boxplot(Bad$Months, at = 3, add = TRUE, main="Boxplot of Variable: Months by Decision", 
        xlab="Decision", ylab="Months", col="firebrick1", boxwex = 1.2)
axis(1, at=3, labels=c('Bad'))
abline(h=mean(mmonths), col="red")



#Variable: Age

mAge = mean(C_D$Age)

boxplot(C_D$Age, at = 1, xlim = c(0.5, 3.5), ylim = c(15,65), main="Boxplot of Variable: Age by Decision", 
        xlab="Decision", ylab="Age", col="gray", boxwex = 1.2)
axis(1, at=1, labels=c('All'))
boxplot(Good$Age, at = 2, ylim = c(15,65), add = TRUE, main="Boxplot of Variable: Age by Decision", 
        xlab="Decision", ylab="Age", col="chartreuse", boxwex = 1.2)
axis(1, at=2, labels=c('Good'))
boxplot(Bad$Age, at = 3, ylim = c(15,65), add = TRUE, main="Boxplot of Variable: Age by Decision", 
        xlab="Decision", ylab="Age", col="firebrick1", boxwex = 1.2)
axis(1, at=3, labels=c('Bad'))
abline(h=mean(mAge), col="red")



#Variable: Credit Amount

mCredit_Amount = mean(C_D$Credit_Amount)

boxplot(C_D$Credit_Amount, at = 1, xlim = c(0.5, 3.5), ylim = c(0,10000), main="Boxplot of Variable: Credit Amount by Decision", 
        xlab="Decision", ylab="Credit Amount", col="gray", boxwex = 1.2, breaks = 2000)
axis(1, at=1, labels=c('All'))
boxplot(Good$Credit_Amount, at = 2, ylim = c(0,10000), add = TRUE, main="Boxplot of Variable: Credit Amount by Decision", 
        xlab="Decision", ylab="Credit Amount", col="chartreuse", boxwex = 1.2, breaks = 2000)
axis(1, at=2, labels=c('Good'))
boxplot(Bad$Credit_Amount, at = 3, ylim = c(0,10000), add = TRUE, main="Boxplot of Variable: Credit Amount by Decision", 
        xlab="Decision", ylab="Credit Amount", col="firebrick1", boxwex = 1.2, breaks = 2000)
axis(1, at=3, labels=c('Bad'))
abline(h=mean(mCredit_Amount), col="red")

#Variable: Installment Rate

mInstallment_Rate = mean(C_D$Installment_Rate)

boxplot(C_D$Installment_Rate, at = 1, xlim = c(0.5, 3.5), ylim = c(0.5,4.5), main="Boxplot of Variable: Installment Rate by Decision", 
        xlab="Decision", ylab="Installment Rate (%)", col="gray", boxwex = 1.2)
axis(1, at=1, labels=c('All'))
boxplot(Good$Installment_Rate, at = 2, ylim = c(0.5,4.5), add = TRUE, main="Boxplot of Variable: Installment Rate by Decision", 
        xlab="Decision", ylab="Installment Rate (%)", col="chartreuse", boxwex = 1.2)
axis(1, at=2, labels=c('Good'))
boxplot(Bad$Installment_Rate, at = 3, ylim = c(0.5,4.5), add = TRUE, main="Boxplot of Variable: Installment Rate by Decision", 
        xlab="Decision", ylab="Installment Rate (%)", col="firebrick1", boxwex = 1.2)
axis(1, at=3, labels=c('Bad'))
abline(h=mean(mInstallment_Rate), col="red")

#Variable: Residence Since

mResidence_Since = mean(C_D$Residence_Since)

boxplot(C_D$Residence_Since, at = 1, xlim = c(0.5, 3.5), ylim = c(0.5,4.5), main="Boxplot of Variable: Residence Since by Decision", 
        xlab="Decision", ylab="Residence Since", col="gray", boxwex = 1.2, breaks = 0.5)
axis(1, at=1, labels=c('All'))
boxplot(Good$Residence_Since, at = 2, ylim = c(0.5,4.5), add = TRUE, main="Boxplot of Variable: Residence Since by Decision", 
        xlab="Decision", ylab="Residence Since", col="chartreuse", boxwex = 1.2, breaks = 0.5)
axis(1, at=2, labels=c('Good'))
boxplot(Bad$Residence_Since, at = 3, ylim = c(0.5,4.5), add = TRUE, main="Boxplot of Variable: Residence Since by Decision", 
        xlab="Decision", ylab="Residence Since", col="firebrick1", boxwex = 1.2, breaks = 0.5)
axis(1, at=3, labels=c('Bad'))
abline(h=mean(mResidence_Since), col="red")


#Variable: Existing Credit

mExisting_Credit = mean(C_D$Existing_Credit)

boxplot(C_D$Existing_Credit, at = 1, xlim = c(0.5, 3.5), ylim = c(0.5,3.5), main="Boxplot of Variable: Existing Credit by Decision", 
        xlab="Decision", ylab="Existing Credit", col="gray", boxwex = 1.2)
axis(1, at=1, labels=c('All'))
boxplot(Good$Existing_Credit, at = 2, ylim = c(0.5,3.5), add = TRUE, main="Boxplot of Variable: Existing Credit by Decision", 
        xlab="Decision", ylab="Existing Credit", col="chartreuse", boxwex = 1.2)
axis(1, at=2, labels=c('Good'))
boxplot(Bad$Existing_Credit, at = 3, ylim = c(0.5,3.5), add = TRUE, main="Boxplot of Variable: Existing Credit by Decision", 
        xlab="Decision", ylab="Existing Credit", col="firebrick1", boxwex = 1.2)
axis(1, at=3, labels=c('Bad'))
abline(h=mean(mExisting_Credit), col="red")



