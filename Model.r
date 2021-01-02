#Cleaning of data for stats

d_des <- read.csv('world_clean.csv', na.strings = "")
d <- read.csv('world_clean3.csv', na.strings = "")

d_des$literacy <- ifelse(is.na(d$literacy), mean(d$literacy, na.rm=TRUE), d$literacy)
d_des$pop_age <- ifelse(is.na(d$pop_age), mean(d$pop_age, na.rm=TRUE), d$pop_age)

#Descriptive stats for lit and age
summary(d_des)
sd(d_des$literacy)
sd(d_des$pop_age)

#Cleaning of data for model

d$literacy <- ifelse(is.na(d$literacy), mean(d$literacy, na.rm=TRUE), d$literacy)
d$pop_age <- ifelse(is.na(d$pop_age), mean(d$pop_age, na.rm=TRUE), d$pop_age)
d$spendeduc <- ifelse(is.na(d$spendeduc), mean(d$spendeduc, na.rm=TRUE), d$spendeduc)
d$gender_unequal <- ifelse(is.na(d$gender_unequal), mean(d$gender_unequal, na.rm=TRUE), d$gender_unequal)
d$dem_score14 <- ifelse(is.na(d$dem_score14), mean(d$dem_score14, na.rm=TRUE), d$dem_score14)
d$hdi <- ifelse(is.na(d$hdi), mean(d$hdi, na.rm=TRUE), d$hdi)
d$effectiveness <- ifelse(is.na(d$effectiveness), mean(d$effectiveness, na.rm=TRUE), d$effectiveness)
d$confidence <- ifelse(is.na(d$confidence), mean(d$confidence, na.rm=TRUE), d$confidence)


#space for graphs if needed 


#Tests

cor.test(d$literacy, d$pop_age)


#Linear regression model

m <- lm(literacy ~ pop_age, d)
summary(m)

#Descriptive stats for multi-regression

summary(d)

#Multiple linear regression model

m2 <- lm(literacy ~ pop_age + hdi + effectiveness, d)
summary(m2)

#Compare the models

anova(m, m2)

