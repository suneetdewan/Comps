## Suneet Dewan
## Founding to Fortune: The Effect of Human Capital on Entrepreneurial Success

# The first part is comprised on regression codes on the main dataset, ie, one where all the non-response are removed
# Near the end, I added the code for the regressions on the dataset including the 86 who come back



WaveB <- read.csv("clean_npremoved.csv")
dataB <- read.csv("clean_include_waveB_skips.csv")

# Remove the observations with empty Education
WaveB <- WaveB[-375,]
which(! complete.cases(WaveB))

# Remove the observations that have missing values
WaveB <- WaveB[-c(65, 101, 190, 269, 427, 571, 713, 752), ]

WaveB$code <- factor(WaveB$industry_code)
WaveB$education_smaller <- factor(WaveB$education_smaller, levels = c("High School or Below", "Some college", "Bachelors degree", "Graduate degree"))



# SURVIVAL

# Only Control
survive3_logit_0 <- glm(survive_3 ~ white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit_0)

# General + Control
survive3_logit_1 <- glm(survive_3 ~ education_smaller + work_experience + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit_1)

# General + Industry + Control
survive3_logit_2 <- glm(survive_3 ~ education_smaller + work_experience+ industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight, na.action = na.exclude)
summary(survive3_logit_2)

# General + Entrepreneurial + Control
survive3_logit_3 <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight, na.action = na.exclude)
summary(survive3_logit_3)

#everything
survive3_logit <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit)

anova()
anova(survive3_logit_0, survive3_logit, test = "Chisq")



#PROFIT

# Only Control
profit3_logit_0 <- glm(profit ~ white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(profit3_logit_0)

# General + Control
profit3_logit_1 <- glm(profit ~ education_smaller + work_experience + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(profit3_logit_1)

# General + Industry + Control
profit3_logit_2 <- glm(profit ~ education_smaller + work_experience+ industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight, na.action = na.exclude)
summary(profit3_logit_2)

# General + Entrepreneurial + Control
profit3_logit_3 <- glm(profit ~ education_smaller + work_experience + manager_exp + entrep_exp + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight, na.action = na.exclude)
summary(profit3_logit_3)

#everything
profit3_logit <- glm(profit ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(profit3_logit)

anova(profit3_logit_0, profit3_logit, test = "Chisq")



# REVENUE

# Only Control
rev3_logit_0 <- glm(rev ~ white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(rev3_logit_0)

# General + Control
rev3_logit_1 <- glm(rev ~ education_smaller + work_experience + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(rev3_logit_1)

# General + Industry + Control
rev3_logit_2 <- glm(rev ~ education_smaller + work_experience+ industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight, na.action = na.exclude)
summary(rev3_logit_2)

# General + Entrepreneurial + Control
rev3_logit_3 <- glm(rev ~ education_smaller + work_experience + manager_exp + entrep_exp + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight, na.action = na.exclude)
summary(rev3_logit_3)

#everything
rev3_logit <- glm(rev ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit)

anova(rev3_logit_0, rev3_logit, test = "Chisq")



# Checking multi-collinearity
library(car)
vif(survive3_logit)






## Secondary Regression run on the dataset where the 86 respondents who come back are added


dataB <- data[-160,]
dataB$code <- factor(dataB$industry_code)
dataB$education_smaller <- factor(dataB$education_smaller, levels = c("High School or Below", "Some college", "Bachelors degree", "Graduate degree"))
which(! complete.cases(dataB))
dataB <- dataB[-c(39, 160, 219, 388, 546, 589, 811, 847, 936)]

survive_C_logit <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = dataB, family = 'binomial', weights = dataB$Weight_C)
summary(survive_C_logit)

rev_C_logit <- glm(rev ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = dataB, family = 'binomial', weights = dataB$Weight_C)
summary(rev_C_logit)

profit_C_logit <- glm(profit ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = dataB, family = 'binomial', weights = dataB$Weight_C)
summary(profit_C_logit)


