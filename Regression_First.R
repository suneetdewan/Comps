WaveB <- read.csv("clean_npremoved.csv")
WaveC <- read.csv("clean_only_WaveC.csv")
dataB <- read.csv("clean_include_waveB_skips.csv")

count(data$education_smaller)

# Remove the observations with empty Education
WaveB <- WaveB[-375,]
WaveC <- WaveC[-160,]
dataB <- data[-160,]

# Factoring the Industry Codes
WaveB$code <- factor(WaveB$industry_code)
WaveC$code <- factor(WaveC$industry_code)
dataB$code <- factor(dataB$industry_code)

# Ordering the education factors
WaveB$education_smaller <- factor(WaveB$education_smaller, levels = c("High School or Below", "Some college", "Bachelors degree", "Graduate degree"))
WaveC$education_smaller <- factor(WaveC$education_smaller, levels = c("High School or Below", "Some college", "Bachelors degree", "Graduate degree"))
dataB$education_smaller <- factor(dataB$education_smaller, levels = c("High School or Below", "Some college", "Bachelors degree", "Graduate degree"))


# WAVE B DATA:

which(! complete.cases(WaveB))
WaveB <- WaveB[-c(65, 101, 190, 269, 427, 571, 713, 752), ]

which(! complete.cases(dataB))
dataB <- dataB[-c(39, 160, 219, 388, 546, 589, 811, 847, 936)]

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

anova(survive3_logit_0, survive3_logit, test = "Chisq")

library(stargazer)
Nice_B <- WaveB
Nice_B$X <- NULL
Nice_B$survive_2 <- NULL
Nice_B$profit_2 <- NULL
Nice_B$WT_WAVEA <- NULL
Nice_B$WT_WAVEB <- NULL
Nice_B$WT_WAVEC <- NULL
Nice_B$WT_Weight <- NULL
Nice_B$team_size <- NULL
Nice_B$Weight <- NULL
Nice_B$net_worth_dummy <- NULL

stargazer(Nice_B, type = "text", title = "Descriptive Statistics", digits = 2, nobs = FALSE, out = "DescriptiveB_2.txt")

correlation.matrix <- cor(WaveB[,c("survive_3", "rev", "profit", "high_school_or_below", "some_college", "bachelors", "graduate_degree", "work_experience", "manager_exp", "entrep_exp", "industry_exp", "major_reason","white", "Male", "parents")])
stargazer(correlation.matrix, type = "text", out = "correlation2.txt")


stargazer(survive3_logit_1, survive3_logit_3, survive3_logit_2, survive3_logit, title="Results", align=TRUE, digit = 3, type = "text", out = "Survive_3_new.txt")




#without 35 hours
survive3_logit_without35 <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit_without35)


#without managerial
survive3_logit_without_manger <- glm(survive_3 ~ education_smaller + work_experience + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit_without_manger)




# INCLUDE PEOPLE WHO SKIPPED ANSWERING WAVE B AND CAME BACK FOR WAVE C:

survive3_logit_data <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp +  industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = data, family = 'binomial', weights = data$Weight_C, na.action = na.exclude)
summary(survive3_logit_data)

survive3_logit_data_B <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp +  industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = data, family = 'binomial', weights = data$Weight_C, na.action = na.exclude)
summary(survive3_logit_data_B)

survive3_logit_data_without35 <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + REGIONA, data = data, family = 'binomial', weights = data$Weight_C, na.action = na.exclude)
summary(survive3_logit_data_without35)


# Which ones were removed:
which(! complete.cases(WaveB))
# They were removed because did not have work experience data

profit3_logit <- glm(profit_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(profit3_logit)

profit3_logit_C <- glm(profit_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveC, family = 'binomial', weights = WaveC$Weight)
summary(profit3_logit_C)


profit3_logit_without35 <- glm(profit_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(profit3_logit_without35)


# Revenue
rev3_logit <- glm(rev_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveC, family = 'binomial', weights = WaveC$Weight)
summary(rev3_logit)

rev3_logit_B <- glm(rev_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(rev3_logit_B)



library(pscl)
pR2(survive3_logit)
pR2(rev3_logit)

survive3_logit_entrep <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit_entrep)
pR2(survive3_logit_entrep)


survive3_logit_indus <- glm(survive_3 ~ education_smaller + work_experience + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours, data = WaveB, family = 'binomial', weights = WaveB$Weight)
summary(survive3_logit_indus)
pR2(survive3_logit_indus)

# As of right now, the industry experience is better than entrepreneurial experience






survive_C_logit <- glm(survive_3 ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = dataB, family = 'binomial', weights = dataB$Weight_C)
summary(survive_C_logit)

rev_C_logit <- glm(rev ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = dataB, family = 'binomial', weights = dataB$Weight_C)
summary(rev_C_logit)

profit_C_logit <- glm(profit ~ education_smaller + work_experience + manager_exp + entrep_exp + industry_exp + major_reason + white + parents + Gender + alone_or_not + net_worth:net_worth_dummy + code + X35hours + REGIONA, data = dataB, family = 'binomial', weights = dataB$Weight_C)
summary(profit_C_logit)

stargazer(survive_C_logit, rev_C_logit, profit_C_logit,  title="REGRESSIONS", align=TRUE, digit = 3, type = "text", out = "APPENDIX_3.txt")
