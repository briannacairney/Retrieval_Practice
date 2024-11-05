# Analyses for manuscript - Exp. 1

# Load libraries
library(lme4)
library(emmeans)

# Load data
load("Data/FullData_Factors.Rdata")

# Set factor levels
contrasts(FullData$ConditionFactor) # NF = -0.5; FB = 0.5
contrasts(FullData$GroupFactor)     # NONAUT = -0.5; AUT = 0.5


# Does feedback affect final recall accuracy and to the same extent for non/autistic adults?
Model_1 <- glmer(T2_SCORE ~ ConditionFactor * GroupFactor + (1|SubID), 
                 family= 'binomial', 
                 data = FullData)
summary(Model_1)

# Did the main effect of feedback hold true depending on retrieval practice accuracy?
Model_2 <- glmer(T2_SCORE ~ ConditionFactor * GroupFactor * T1_SCORE + (1|SubID), 
                 family= 'binomial', 
                 data = FullData)
summary(Model_2)

# Understanding the Condition x T1 Accuracy interaction using estimated marginal means
emm_Mod2 <- emmeans(Model_2, ~ ConditionFactor | T1_SCORE)
summary(emm_Mod2)
emm_Mod2pairs <- pairs(emmeans(Model_2, ~ ConditionFactor | T1_SCORE))
summary(emm_Mod2pairs)

