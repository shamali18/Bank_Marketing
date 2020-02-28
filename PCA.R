library(data.table)
library(tidyverse)  # data manipulation
library(data.table) # fast file reading
library(gridExtra)  # arranging ggplot in grid
library(rmarkdown)
library(tinytex)
library(latexpdf)
library(latex2exp)
bank <- read.csv("C:/Users/Shamali/Desktop/Rutgers Spring/multivariat/project/bank-marketing-dataset/bank.csv")
bank1<- bank[ ,c(1,5,6,10,12,13,14,15,17)]
# Performing PCA

# Using prcomp to compute the principal components (eigenvalues and eigenvectors). 
# With scale=TRUE, variable means are set to zero, and variances set to one
bank1_pca <- prcomp(bank1[,-1],scale=FALSE)
bank1_pca
summary(bank1_pca)

(eigen_bank <- bank1_pca$sdev^2)
names(eigen_bank) <- paste("PC",1:8,sep="")
eigen_bank
sumlambdas <- sum(eigen_bank)
sumlambdas
propvar <- eigen_bank/sumlambdas
propvar
cumvar_bank <- cumsum(propvar)
cumvar_bank
matlambdas <- rbind(eigen_bank,propvar,cumvar_bank)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)

summary(bank1_pca)
bank1_pca$rotation
print(bank1_pca)

# Sample scores stored in bank_pca$x
bank1_pca$x

# Identifying the scores by their deposit status
deposit_pca <- cbind(data.frame(bank1),bank1_pca$x)
deposit_pca

# Means of scores for all the PC's classified by Deposit status
tabmeansPC <- aggregate(deposit_pca[,2:6],by=list(Deposit=bank$deposit),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$Deposit)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans

# Standard deviations of scores for all the PC's classified by Deposit status
tabsdsPC <- aggregate(deposit_pca[,2:6],by=list(Deposit=bank$deposit),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds

#T-test
t.test(PC1~bank$deposit,data=deposit_pca)
t.test(PC2~bank$deposit,data=deposit_pca)
t.test(PC3~bank$deposit,data=deposit_pca)
t.test(PC4~bank$deposit,data=deposit_pca)
t.test(PC5~bank$deposit,data=deposit_pca)

#F-test
var.test(PC1~bank$deposit,data=deposit_pca)
var.test(PC2~bank$deposit,data=deposit_pca)
var.test(PC3~bank$deposit,data=deposit_pca)
var.test(PC4~bank$deposit,data=deposit_pca)
var.test(PC5~bank$deposit,data=deposit_pca)

plot(eigen_bank, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_bank), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
