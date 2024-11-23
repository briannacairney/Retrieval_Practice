# Exp. 2 script to test for differences between practice trial types on final cued recall 
# Created 11.18.24 based off of /Volumes/Reggie/Metacog/07_Exp_2/04_ComparePracTrials.R to include Group 

library(dplyr)

load('Data/ScoredData.Rdata')

# Create a new variable that combines Trial Type and Condition (i.e., Prac-Restudy, Test-FB, and Test-NF)
ScoredData2 <- ScoredData
ScoredData2$TrialType <- ""
for(i in 1:nrow(ScoredData2)) {
  if(ScoredData2$PRAC_TRIAL_TYPE[i] == "PRAC-TEST") {
    ScoredData2$TrialType[i] <- paste(ScoredData2$PRAC_TRIAL_TYPE[i], "-", ScoredData2$Condition[i], sep = "")
  } else {
    ScoredData2$TrialType[i] <- ScoredData2$PRAC_TRIAL_TYPE[i]
  }
}

# Get mean scores
SubMeans <- ScoredData2 %>%
  group_by(PROLIFIC_PID, TrialType, Condition, Group) %>%
  summarize(Sub_Mean = mean(SCORE))

# Set up contrasts for Practice Trial Type (Study vs. Test)
SubMeans$TrialType <- as.factor(SubMeans$TrialType)
# Set subjects as factors
SubMeans$PROLIFIC_PID <- as.factor(SubMeans$PROLIFIC_PID)
# Set Group as factors
SubMeans$Group <- as.factor(SubMeans$Group)

# ANOVA using ezANOVA
library(ez)

ezLimit <- ezANOVA(data = SubMeans,
                   dv = Sub_Mean, 
                   wid = PROLIFIC_PID,
                   between = TrialType * Group)
ezLimit 

#          Effect DFn  DFd          F           p  p<.05          ges
#       TrialType   2  314  3.4741238  0.03218536      *  0.021649122
#           Group   1  314  2.0524827  0.15295301         0.006494120
# TrialType:Group   2  314  0.6545893  0.52036307         0.004152047
# Levene's Test for Homogeneity of Variance
# DFn DFd         SSn       SSd         F          p  p<.05
#   5 314  0.03899219  7.937188  0.308511  0.9076879       




# To belabor the point... 

# Split up data by practice trial type 
SubMeans_Study <- SubMeans %>%
  filter(TrialType == "PRAC-RESTUDY")
mean(SubMeans_Study$Sub_Mean) # 0.199375
SubMeans_TEST_FB <- SubMeans %>%
  filter(TrialType == "PRAC-TEST-FB")
mean(SubMeans_TEST_FB$Sub_Mean) # 0.2746875
SubMeans_TEST_NF <- SubMeans %>%
  filter(TrialType == "PRAC-TEST-NF")
mean(SubMeans_TEST_NF$Sub_Mean) # 0.2415625

## t-Tests for pair-wise comparisons ##
# Test-FB vs. Test-NF
t.test(SubMeans_TEST_FB$Sub_Mean, SubMeans_TEST_NF$Sub_Mean, paired = FALSE) # NS
t.test(SubMeans_TEST_FB$Sub_Mean, SubMeans_Study$Sub_Mean, paired = FALSE)   # t = 2.6234, df = 164.37, p-value = 0.009525
t.test(SubMeans_TEST_NF$Sub_Mean, SubMeans_Study$Sub_Mean, paired = FALSE)   # NS

# Split further by Group + pair-wise comparisons 
SubMeans_Study_Aut <- SubMeans_Study %>%
  filter(Group == "Aut")
SubMeans_Study_NonAut <- SubMeans_Study %>%
  filter(Group == "NonAut")
t.test(SubMeans_Study_Aut$Sub_Mean, SubMeans_Study_NonAut$Sub_Mean) # NS

SubMeans_TEST_FB_Aut <- SubMeans_TEST_FB %>%
  filter(Group == "Aut")
SubMeans_TEST_FB_NonAut <- SubMeans_TEST_FB %>%
  filter(Group == "NonAut")
t.test(SubMeans_TEST_FB_Aut$Sub_Mean, SubMeans_TEST_FB_NonAut$Sub_Mean) # t = 1.7975, df = 77.564, p-value = 0.07615
effectsize::cohens_d(SubMeans_TEST_FB_Aut$Sub_Mean, SubMeans_TEST_FB_NonAut$Sub_Mean) # 0.40 (0.2 = small; 0.5 = medium)

SubMeans_TEST_NF_Aut <- SubMeans_TEST_NF %>%
  filter(Group == "Aut")
SubMeans_TEST_NF_NonAut <- SubMeans_TEST_NF %>%
  filter(Group == "NonAut")
t.test(SubMeans_TEST_NF_Aut$Sub_Mean, SubMeans_TEST_NF_NonAut$Sub_Mean) # NS




# From 11.18.24
library(lmerTest)
model <- lmer(Sub_Mean ~ TrialType * Condition * Group + (1|PROLIFIC_PID), data = SubMeans)
summary(model)
anova(model, type = "III")

library(emmeans)
# Pairwise comparisons for TrialType
emmeans(model, pairwise ~ TrialType)
# Interaction effects (TrialType by Condition by Group)
emmeans(model, pairwise ~ TrialType | Condition * Group)


