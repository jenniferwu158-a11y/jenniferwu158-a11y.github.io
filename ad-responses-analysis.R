install.packages("car") 
install.packages("sur") 
install.packages("effectsize")
install.packages("emmeans") 
Ads <- read.csv("~/Desktop/大三/Regression/Homework/HW6/Ads.csv") 
# Set up the factors
Ads$Day <- as.factor(Ads$Day)
Ads$Section <- as.factor(Ads$Section)
# 1. Normaity Check
# Make sure the sample sizes are at least 30
table(Ads$Day, Ads$Section)
# Verify that the skewness ratios are all less than 2 in magnitude
library(sur)
tapply(Ads$Responses, list(Ads$Day, Ads$Section), skew.ratio)
# 2. Homogeneity of variances
# Run Levene's Test for the interaction of Day and Section
library(car)
leveneTest(Responses ~ Day * Section, data = Ads)
library(ggplot2)
ggplot(Ads, aes(x = factor(Day), y = Responses, color = factor(Section), group = Section)) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  
  # 1. Label the X-axis (Day)
  scale_x_discrete(labels = c("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday")) +
  
  # 2. Label the Legend (Section) and apply the colors
  scale_color_manual(
    name = "Section", 
    values = c("#81D8D0", "#999999", "#ffae49"),
    labels = c("1" = "news", "2" = "business", "3" = "sports")
  ) + 
  
  labs(title = "Figure 1: Responses by Day and Section",
       x = "Day",
       y = "Responses") +
  theme_minimal()
# Two way ANOVA
# 1. Set contrasts for Type III SS (Crucial for unbalanced data)
options(contrasts = c("contr.sum", "contr.poly"))
# 2. Run the Model
# The asterisk (*) includes Main Effect A, Main Effect B, and the Interaction
model <- lm(Responses ~ Day * Section, data = Ads)
# 3. Generate the ANOVA Table
Anova(model, type = "III")

library(effectsize)
# Calculate partial eta squared for your model
eta_squared(model, partial = TRUE)

library(emmeans) 
# Calculate the estimated marginal means for Type 
simple_effects <- emmeans(model, ~ Day | Section) 
# Run the post hoc test (tukey is the default) 
results <- pairs(simple_effects, adjust = "tukey")
print(results)
