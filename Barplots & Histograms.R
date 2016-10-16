BP <- read.csv("E:/School/4th Year/Fall 2016/MATH 410 Scientific Data Analysis I/Lab 1/Ch03_Blood_Pressure.csv")

summary(BP)

# determine the order of the factors
BP$Blood.pressure <- factor(BP$Blood.pressure, levels=c("Low", "Normal", "High"), ordered = TRUE)
BP$Age <- factor(BP$Age, levels=c("Under 30", "30 - 49", "Over 50"), ordered = TRUE)

# create a table from the dataset and add margins
T <- table(BP$Age, BP$Blood.pressure)
T1 <- table(BP$Blood.pressure, BP$Age, dnn = c("Pressure", "Age range"))
addmargins(T1, 1)
addmargins(T1, 2)
addmargins(T1, c(1,2))

# display a barplot with the frequency of each age group
BPcounts <- margin.table(T1,1)
AGEcounts <- sort(margin.table(T1,2))
barplot(AGEcounts, las=1, xlab="age Group (years)", ylab="Frequency", ylim=c(0,250))
barplot(BPcounts, las=1, xlab="Blood Pressure", ylab="Frequency", ylim=c(0,250))

# display the distribution of blood pressure by age group (barplots)
barplot(T1, beside = TRUE, main="Distribution of Blood Pressure by Age Group",
        xlab="Age Group (years)", ylab="Frequency", las=1, ylim=c(0,100))

legend(1.05, 100, c("High", "Normal", "Low"), col=c("grey80", "grey50", "grey20"), pch=15)

# display the stacked distribution of blood pressure by age group (stacked barplots)
barplot(T1, main="Distribution of Blood Pressure by Age Group", legend.text = TRUE,
        ylim = c(0,200), xlab = "Age Group(years)", ylab = "Count", las=1, args.legend = list(x="topleft"))

# develop a table of proportions + percentages 
TP <- prop.table(T1); TP # cell percentages
round(TP,2) ### proportions
TPMAR <- addmargins(TP,c(1,2))
round(100*TPMAR,2) ### percentages

# proportions table conditioned by rows: percentages
TProw <- prop.table(T1,1)
TProwMAR <- addmargins(TProw,2)
round(100*TProwMAR,2)

# proportions table conditioned by columns: percentages
TPcol <- prop.table(T1,2)
TPcolMAR <- addmargins(TPcol,1)
round(100*TPcolMAR,2)

# distribution of blood pressure by age group (percentage wise)
# stacked barplot
barplot(100*TPcol,main="Distribution of Blood Pressure by Age Group",xlab="Age Group (years)",
        ="Percent",las=1)
legend(3.05,99,c("High","Normal","Low"),col=c("grey80","grey50","grey20"),pch=15,
       cex=.9,bg="white")

# distributed bar plot
barplot(100*TPcol,main="Distribution of Blood Pressure by Age Group",xlab="Age Group (years)",
        beside=TRUE,ylab="Percent",las=1)
legend(3.5,50,c("High","Normal","Low"),col=c("grey80","grey50","grey20"),pch=15,
       cex=.9,bg="white")

# import this dataset to create new histograms
hardwater <- read.csv("E:/School/4th Year/Fall 2016/MATH 410 Scientific Data Analysis I/Lab 1/Hard_water_Derby.csv",header=TRUE)
summary(hardwater)

# histograms of mortality & calcium
hist(hardwater$Mortality,las=1,right=FALSE,xlab="Mortality (deaths per 100,000)",main="",border="white",col="grey")
hist(hardwater$Calcium,las=1,right=FALSE,xlab="Calcium concentration (ppm)",main="",border="white",col="grey")

# use different # of bins to create the mortality histogram.5 bins show an approximately normal distribution 
hist(hardwater$Mortality,15,las=1,right=FALSE,xlab="",main="",border="white",col="grey")
hist(hardwater$Mortality,10,las=1,right=FALSE,xlab="",main="",border="white",col="grey")
hist(hardwater$Mortality,5,las=1,right=FALSE,xlab="Mortality (deaths per 100,000)",main="",border="white",col="grey")








