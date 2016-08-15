library(formatR)
tidy_source(file = "Bar_Plotoutput.R")
graphics.off()

C_D = read.table('german.data')

ncol(C_D)
nrow(C_D)

#Setting Variable Names
colnames(C_D) = c("Account_Status","Months","Credit_History","Credit_Purpose","Credit_Amount","Savings_Account"
                  ,"Employed_Since","Installment_Rate","Status_and_Sex","Other_Debtors","Residence_Since",
                  "Property","Age","Other_Installement_Plans","Housing_Type","Existing_Credit","Job_Type",
                  "Dependents","Telephone","Foreign_Worker","Cost_Matrix")




###BarPlots###

par(mfrow=c(2,3))

count.purpose = table(C_D$Credit_Purpose)
b1 = barplot(count.purpose, main="Barplot of Credit Purpose",
        xlab="Purpose of Loan",ylab = "Frequency",
        ylim = c(0,1000))

count.status = table(C_D$Account_Status)
b2 = barplot(count.status, main="Barplot of Account Status",
             xlab="Account Status",ylab = "Frequency",
             ylim = c(0,1000))

count.history = table(C_D$Credit_History)
b3 = barplot(count.history, main="Barplot of Credit History",
             xlab="Credit History",ylab = "Frequency",
             ylim = c(0,1000))


count.savings = table(C_D$Savings_Account)
b4 = barplot(count.savings, main="Barplot of Savings Account",
             xlab="Purpose of Loan",ylab = "Frequency",
             ylim = c(0,1000))



count.employ.since = table(C_D$Employed_Since)
b5 = barplot(count.employ.since, main="Barplot of Employement Duration",
             xlab="Employment Duration",ylab = "Frequency",
             ylim = c(0,1000))



count.status.sex = table(C_D$Status_and_Sex)
b6 = barplot(count.status.sex, main="Barplot of Status and Sex",
             xlab="Status and Sex",ylab = "Frequency",
             ylim = c(0,1000))

par(mfrow=c(2,3))

count.debtors = table(C_D$Other_Debtors)
b7 = barplot(count.history, main="Barplot of Other_Debtors",
             xlab="Other Debtors",ylab = "Frequency",
             ylim = c(0,1000))


count.other.installment.plans = table(C_D$Other_Installement_Plans)
b8 = barplot(count.other.installment.plans, main="Barplot of Other Installment ",
             xlab="Other Installment Plans",ylab = "Frequency",
             ylim = c(0,1000))

count.housing = table(C_D$Housing_Type)
b9 = barplot(count.housing, main="Barplot of Housing Type",
             xlab="Employment Duration",ylab = "Frequency",
             ylim = c(0,1000))

count.property = table(C_D$Property)
b10 = barplot(count.property, main="Barplot of Property",
             xlab="Property",ylab = "Frequency",
             ylim = c(0,1000))

count.job.type = table(C_D$Job_Type)
b11 = barplot(count.job.type, main="Barplot of Job Type",
              xlab="Types of Job",ylab = "Frequency",
              ylim = c(0,1000))

count.telephone = table(C_D$Telephone)
b12 = barplot(count.telephone, main="Barplot of Telephone",
              xlab="Telephone", ylab = "Frequency",
              ylim = c(0,1000))






