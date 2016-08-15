library(formatR)
library(vcd)
tidy_source(file = "boxplot.R")

getwd()


C_D = read.table('german.data')

ncol(C_D)
nrow(C_D)

#Setting Variable Names
colnames(C_D) = c("Account_Status","Months","Credit_History","Credit_Purpose","Credit_Amount","Savings_Account"
                  ,"Employed_Since","Installment_Rate","Status_and_Sex","Other_Debtors","Residence_Since",
                  "Property","Age","Other_Installement_Plans","Housing_Type","Existing_Credit","Job_Type",
                  "Dependents","Telephone","Foreign_Worker","Cost_Matrix")

#####Mosaic plots######
graphics.off()

par(mfrow=c(3,2))

#Variable: Status_and_Sex

## A91 : male   : divorced/separated
## A92 : female : divorced/separated/married
## A93 : male   : single
## A94 : male   : married/widowed
## A95 : female : single

levels(C_D$Status_and_Sex) = c("M/DSe", "F/DSeM", "M/Si", "M/MW", "F/Si")
levels(C_D$Cost_Matrix) = c("Good", "Bad")

Status_Sex = table(C_D$Status_and_Sex, C_D$Cost_Matrix)

mosaicplot(Status_Sex, cex = 0.60, color = TRUE, border = "azure3")

#Variable: Job_Type

## A171 : unemployed/ unskilled  - non-resident
## A172 : unskilled - resident
## A173 : skilled employee / official
## A174 : management/ self-employed/highly qualified employee/ officer

levels(C_D$Job_Type) = c("Unemp/Uns/NR", "Uns/Res", "Skilled/Off", "M/SE/HQE/O")
levels(C_D$Cost_Matrix) = c("Good", "Bad")

Job = table(C_D$Job_Type, C_D$Cost_Matrix)

mosaicplot(Job, cex = 0.60, color = TRUE, border = "azure3")


#Variable: Credit history

## A30 : no credits taken/all credits paid back duly
## A31 : all credits at this bank paid back duly
## A32 : existing credits paid back duly till now
## A33 : delay in paying off in the past
## A34 : critical account/other credits existing (not at this bank)

levels(C_D$Credit_History) = c("NCR/D", "Th.Bank.D","Ex.Cr.D.", "Delay", "Cr.Acc.")
levels(C_D$Cost_Matrix) = c("Good", "Bad")

Credit_Hist= table(C_D$Credit_History, C_D$Cost_Matrix)

mosaicplot(Credit_Hist, cex = 0.60, color = TRUE, border = "azure3")


#Variable: Savings_Account

## A61 :          ... <  100 DM
## A62 :   100 <= ... <  500 DM
## A63 :   500 <= ... < 1000 DM
## A64 :          .. >= 1000 DM
## A65 :   unknown/ no savings account

levels(C_D$Savings_Account) = c("0-100", "100-500", "500-1000", "1000+", "Unk/N.S.A")
levels(C_D$Cost_Matrix) = c("Good", "Bad")

Savings_Acc= table(C_D$Savings_Account, C_D$Cost_Matrix)

mosaicplot(Savings_Acc, cex = 0.60, color = TRUE, border = "azure3")

#Variable: Employed_Since

## A71 : unemployed
## A72 :       ... < 1 year
## A73 : 1  <= ... < 4 years  
## A74 : 4  <= ... < 7 years
## A75 :       .. >= 7 years

help(mosaic)

levels(C_D$Employed_Since) = c("Unemp", "0-1", "1-4", "4-7", "7+")
levels(C_D$Cost_Matrix) = c("Good", "Bad")

Employed_Sin= table(C_D$Employed_Since, C_D$Cost_Matrix)

mosaicplot(Employed_Sin, cex = 0.60, color = TRUE, border = "azure3")




