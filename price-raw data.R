#Set Working Directory 
setwd("H:/sta215")

#Install packages 
install.packages("haven")

#load haven package
library(readr)


#Load the data 
stfinal <- read_delim("stfinal - sheet 2.csv")


######### table 1 descriptive statistics ############### 

#presence of blood
mean(stfinal$presenceofblood)
sd(stfinal$presenceofblood)
hist(stfinal$presenceofblood)
summary(stfinal$presenceofblood)
min(stfinal$presenceofblood)
max(stfinal$presenceofblood)

#fight scenes
mean(stfinal$fightscenes)
sd(stfinal$fightscenes)
hist(stfinal$fightscenes)
summary(stfinal$fightscenes)
min(stfinal$fightscenes)
max(stfinal$fightscenes)

#kisses
mean(stfinal$kisses)
sd(stfinal$kisses)
hist(stfinal$kisses)
summary(stfinal$kisses)
min(stfinal$kisses)
max(stfinal$kisses)

#11powers
mean(stfinal$"11powers")
sd(stfinal$"11powers")
hist(stfinal$"11powers")
summary(stfinal$"11powers")
min(stfinal$"11powers")
max(stfinal$"11powers")

#deaths
mean(stfinal$deaths)
sd(stfinal$deaths)
hist(stfinal$deaths)
summary(stfinal$deaths)
min(stfinal$deaths)
max(stfinal$deaths)

#music
mean(stfinal$music)
sd(stfinal$music)
hist(stfinal$music)
summary(stfinal$music)
min(stfinal$music)
max(stfinal$music)

#creature
mean(stfinal$creature)
sd(stfinal$creature)
hist(stfinal$creature)
summary(stfinal$creature)
min(stfinal$creature)
max(stfinal$creature)


#table 2 contingency table 
table(stfinal$fightscenes,stfinal$presenceofblood)
chisq.test(stfinal$fightscenes,stfinal$presenceofblood)

#figure 1 box plot 
boxplot(stfinal$music,stfinal$creature)

#anova
anova <- aov(creature ~ music, data = stfinal)

#Figure 2 scatter plot presence of 11 powers and death
plot(stfinal$"11powers",stfinal$deaths)
linear_relationship <- lm(stfinal$deaths ~ stfinal$"11powers", stfinal =stfinal)
summary(linear_relationship)
abline(linear_relationship)
#add x line and y line for means
meany <- mean(stfinal$deaths)
meanx <- mean(stfinal$"11powers")

abline(v = meanx, col = "black")
abline(v = meany, col = "black")

#Figure 3 Residual Plot 
plot(stfinal$kisses,residuals(linear_relationship))
abline(h = 0, col ="black")








