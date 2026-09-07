library(ggplot2)
#Read the data
Employment <- read_csv("~/Desktop/大三/Regression/RData/Employment.csv") 

#Univariate Analysis
summary(Employment$CUNR)
sd(Employment$CUNR)
CUNR <- ggplot(Employment, aes(x = Employment$CUNR)) +
  geom_histogram(binwidth = 0.5, fill="lightblue", color="black") +
  labs(title="Histogram of CUNR",
       x="Unemployment Rate (%)",
       y="Frequency") +
  theme_minimal()
print(CUNR)

summary(Employment$CLFPR)
sd(Employment$CLFPR)
CLFPR <- ggplot(Employment, aes(x = Employment$CLFPR)) +
  geom_histogram(binwidth = 0.5, fill="plum", color="black") +
  labs(title="Histogram of CLFPR",
       x="Unemployment Rate (%)",
       y="Frequency") +
  theme_minimal()
print(CLFPR)

#Correlation statistics
cor.test(Employment$CUNR,Employment$CLFPR,method = "pearson")
#Relationship between CLFPR & CUNR
employment <- ggplot(Employment, aes(x = CUNR, y = CLFPR)) + geom_point(color ="deeppink", alpha = 0.7) + labs( title = "Relationship Between CLFPR & CUNR", x = "Unemployment Rate", y = "Labor Force Participation Rate" ) + theme_minimal() 
print(employment) 

#Label the points by color to determine that the pattern differs over time
#Create the subsets
Old_Era <- subset(Employment, Year <= 2008)
New_Era <- subset(Employment, Year >= 2009)
#Setup the initial plot (Old Era)
plot(Old_Era$CLFPR ~ Old_Era$CUNR, 
     xlim = c(min(Employment$CUNR), max(Employment$CUNR)),
     ylim = c(min(Employment$CLFPR), max(Employment$CLFPR)),
     xlab = "Unemployment Rate", 
     ylab = "Labor Force Participation Rate", 
     main = "The Structural Shift: Year-by-Year Labels", 
     pch = 16, col = "gray")
#Add the New Era points (Hotpink)
points(New_Era$CLFPR ~ New_Era$CUNR, pch = 16, col = "hotpink")
legend("topright",
       legend = c("1980-2008", "2009-2024"),
       col = c("gray", "hotpink"),
       pch = 16)
#Correlation statistics
cor.test(Old_Era$CUNR,Old_Era$CLFPR,method = "pearson")
cor.test(New_Era$CUNR,New_Era$CLFPR,method = "pearson")

#Regression Analyses
summary(lm(Old_Era$CLFPR ~ Old_Era$CUNR))
summary(lm(New_Era$CLFPR ~ New_Era$CUNR))

#Residual Analysis for subsets
#1. Saved the model
old_model <- lm(CLFPR ~ CUNR, data = Old_Era)
#Add the standardized residuals to your dataframe
Old_Era$Std_Res <- rstandard(old_model)
#Filter the dataframe to show only the rows where residuals are > 2 or < -2
outliers_old <- Old_Era[abs(Old_Era$Std_Res) > 2, ]
#View the list
print(outliers_old)

#2. Saved the model
new_model <- lm(CLFPR ~ CUNR, data = New_Era)
#Add the standardized residuals to your dataframe
New_Era$Std_Res <- rstandard(new_model)
#Filter the dataframe to show only the rows where residuals are > 2 or < -2
outliers_new <- New_Era[abs(New_Era$Std_Res) > 2, ]
#View the list
print(outliers_new)

#1. Cook's Distance for Old Era
Old_Era$Cooks <- cooks.distance(old_model)
threshold_old <- 4 / nrow(Old_Era)
influential_old <- Old_Era[Old_Era$Cooks > threshold_old, ]
cat("Cook's Distance Threshold (4/n):", threshold_old, "\n")
print(influential_old)

#2. Cook's Distance for New Era
New_Era$Cooks <- cooks.distance(new_model)
threshold_new <- 4 / nrow(New_Era)
influential_new <- New_Era[New_Era$Cooks > threshold_new, ]
cat("Cook's Distance Threshold (4/n):", threshold_new, "\n")
print(influential_new)

