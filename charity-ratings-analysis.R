install.packages("ggplot2") 
library(ggplot2)

#Read the data
Scatterplot <- read.csv("Desktop/大三/Regression/RData/Charities_Combined_Class_Charities.csv") 

#Correlation statistics
cor.test(Scatterplot$OS,Scatterplot$ATS,method = "pearson")
cor.test(Scatterplot$OS,as.numeric(Scatterplot$AF))

#Change the label
Scatterplot$AF <- factor(Scatterplot$AF, levels = c(0,1), labels = c("No","Yes"))

#Relationship between OS & ATS
scatterplot <- ggplot(Scatterplot, aes(x = ATS, y = OS)) + geom_point(color ="hotpink", alpha = 0.7) + labs( title = "Relationship Between OS and ATS", x = "Accountability and Transparency Score", y = "Overall Rating" ) + theme_minimal() 
print(scatterplot) 
#Relationship between OS & AF
scatterplot <- ggplot(Scatterplot, aes(x = AF, y = OS)) + geom_point(color ="lightpink", alpha = 0.7) + labs( title = "Relationship Between OS and AF", x = "Audited Financial Statements", y = "Overall Rating" ) + theme_minimal() 
print(scatterplot) 


