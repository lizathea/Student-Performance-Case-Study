library(dplyr)
library(epiDisplay)
setwd("D://Data_analysis//")
getwd()
d <- read.csv("Student_Performance.csv")
d <- data.frame(d)
str(d)
summary(d)

colnames(d) <- tolower(colnames(d))

# Rename the column
colnames(d)[colnames(d) == "hours.studied"] <- "hours_studied"
colnames(d)[colnames(d) == "previous.scores"] <- "prev_scores"
colnames(d)[colnames(d) == "extracurricular.activities"] <- "extra_activities"
colnames(d)[colnames(d) == "sleep.hours"] <- "sleep_hours"
colnames(d)[colnames(d) == "sample.question.papers.practiced"] <- "sample_practiced"
colnames(d)[colnames(d) == "performance.index"] <- "performance"
str(d)

# Check for duplicate rows
dup <- duplicated(d)
num_duplicates <- sum(dup)
num_duplicates
d <- d[dup==F,]
str(d)

# Check missing value
d[d == ""] <- NA
missing_value <- is.na(d)
missing_count <- colSums(missing_value)
missing_count
str(d)

summary(d)
summ(d)

tab1(d$extra_activities)

#Explore data in each variable
#Performance Index
hist(d$performance, main="Distribution of Performance Index", col=c("green"))

#Hour Studied
hist(d$hours_studied, 
     main = "Distribution of Hours Studied", 
     col = "steelblue", 
     xlab = "Hours Studied", 
     ylab = "Frequency",,
     xlim = c(0,10),
     ylim = c(0, 2500), 
     breaks = max(d$hours_studied) - min(d$hours_studied) + 1)

#Previous Scores
hist(d$prev_scores, 
     main = "Distribution of Previous Scores", 
     col = "orange")

#Extra Activities
tab1(d$extra_activities, bar.value="percent", 
     main="Distribution of Extra-Activities", col=c("darkgreen","orange"))
mtext(side=3, "Percent", adj=-0.1, padj=0)

#Sleep Hours
hist(d$sleep_hours, 
     main = "Distribution of Sleep Hours", 
     col = "mediumpurple", 
     xlab = "Sleep Hours", 
     ylab = "Frequency",
     xlim = c(4,10),
     ylim = c(0, 3500), 
     breaks = max(d$sleep_hours) - min(d$sleep_hours) + 1)

#Sample Practice
hist(d$sample_practiced, 
     main = "Distribution of Sample Practiced", 
     col = "skyblue", 
     xlab = "Sample Practiced", 
     ylab = "Frequency",
     xlim = c(0,10),
     ylim = c(0, 2000), 
     breaks = max(d$sample_practiced) - min(d$sample_practiced) + 1)

# การวิเคราะห์ตัวแปรทีละคู่ระหว่างตัวแปรตามและตัวแปรอิสระ
#Simple Linear Regression

a1 <- lm(performance ~ hours_studied, data = d)
summary(a1)

a2 <- lm(performance ~ prev_scores, data = d)
summary(a2)

d$extra_activities <- factor(d$extra_activities)
levels(d$extra_activities) <- list("No" = 0, "Yes" = 1)
a3 <- lm(performance ~ extra_activities, data = d)
summary(a3)

a4 <- lm(performance ~ sleep_hours, data = d)
summary(a4)

a5 <- lm(performance ~ sample_practiced, data = d)
summary(a5)

d$extra_activities <- factor(d$extra_activities, levels = c("No", "Yes"), 
                      labels = c(0,1))
d$extra_activities <- as.numeric(as.character(d$extra_activities))

# Correlation
cor(d)
pairs(d, col="steelblue")

x

xtx <- t(x)%*%x
xtx

xtx.inv <- solve(xtx)
xtx.inv

xty <- t(x)%*%y
xty

b <- xtx.inv%*%xty
b

# Fit the regression model
mod <- lm(performance ~ hours_studied + prev_scores + extra_activities + 
          sleep_hours + sample_practiced, data = d)
anova(mod)
summary(mod)

# Backward Elimination
backwardmodel <- step(mod, direction = "backward", trace = TRUE)

# Summary of the model
summary(backwardmodel)

# การวินิจฉัยตัวแบบสุดท้าย
windows()
par(mfrow=c(1,3))
plot(backwardmodel$fitted.values, backwardmodel$residuals,las=1)
hist(backwardmodel$residuals,las=1)
qqnorm(backwardmodel$residuals,las=1)
qqline(backwardmodel$residuals)




