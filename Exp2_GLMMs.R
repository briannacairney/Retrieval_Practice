# Analyses for manuscript - Exp. 2

# Load libraries
library(lme4)
library(emmeans)

# Load data
load("Data/Exp2_FullData_Factors.Rdata")

# Check factor levels
contrasts(FullData$ConditionFactor) # NF = -0.5; FB = 0.5
contrasts(FullData$GroupFactor)     # NonAut = -0.5; Aut = 0.5

# Does feedback affect final recall accuracy and to the same extent for non/autistic adults?
# (not included in manuscript)
Model_1 <- glmer(SCORE ~ ConditionFactor * GroupFactor + (1|PROLIFIC_PID), 
                 family= 'binomial', 
                 data = FullData)
summary(Model_1)

# Did the main effect of feedback hold true depending on retrieval practice accuracy?
Model_2 <- glmer(SCORE ~ ConditionFactor * GroupFactor * PracAccuracy + (1|PROLIFIC_PID), 
                 family= 'binomial', 
                 data = FullData)
summary(Model_2)

# Understanding the Condition x T1 Accuracy interaction using estimated marginal means
emm_Mod2 <- emmeans(Model_2, ~ ConditionFactor | PracAccuracy)
summary(emm_Mod2)
emm_Mod2pairs <- pairs(emmeans(Model_2, ~ ConditionFactor | PracAccuracy))
summary(emm_Mod2pairs)
