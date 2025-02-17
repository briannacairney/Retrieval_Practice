library(tidyr)
library(dplyr)

load("Data/Exp1_Demos/Exp1DemoData_Aut.RData")
load("Data/Exp1_Demos/Exp1DemoData_NonAut.RData")
load("Data/Exp1_Demos/Exp1DemoData.RData")


##### DESCRIPTIVES #####
## Age ##
Age <- DemoData %>%
  group_by(Group) %>%
  summarize(
    Min = min(age),
    Max = max(age),
    Mean = mean(age), 
    SD = sd(age)
  )
Age

# t-Test for age differences 
t.test(DemoData_Aut$age, DemoData_NonAut$age, paired = FALSE)
# t = 0.31808, df = 76.769, p-value = 0.7513


## Gender ##
unique(DemoData$gender)
Gender <- DemoData %>%
  group_by(Group) %>%
  summarize(
    Man = sum(gender == "MAN"), 
    Woman = sum(gender == "WOMAN"),
    NonBinary = sum(gender == "NONBINARY"), 
    NoAns = sum(gender == "NOANSWER"),
    Other = sum(gender == "OTHER")
  )
Gender

# Chi-square test for gender differences 
genderTable <- table(DemoData_Aut$gender, DemoData_NonAut$gender)
gender_chiSq <- chisq.test(genderTable)
gender_chiSq
# X-squared = 3.3836, df = 4, p-value = 0.4958



## Education ##
unique(DemoData$education)
Education <- DemoData %>%
  group_by(Group) %>%
  summarize(
    HS_GED = sum(education == "HS_GED"),
    SOMECOLLEGE = sum(education == "SOMECOLLEGE"),
    AA_AS = sum(education == "AA_AS"),
    BA_BS = sum(education == "BA_BS"),
    MA_MS = sum(education == "MA_MS"),
    PHD = sum(education == "PHD")
  )
Education

# Chi-square test for education differences 
edTable <- table(DemoData_Aut$education, DemoData_NonAut$education)
ed_chiSq <- chisq.test(edTable)
ed_chiSq
# X-squared = 22.694, df = 20, p-value = 0.304

Ed_long <- Education %>%
  pivot_longer(cols = c(HS_GED, SOMECOLLEGE, AA_AS, BA_BS, MA_MS, PHD),
               names_to = "Education",
               values_to = "Count")

edTable2 <- table(Ed_long$Group, Ed_long$Count)
ed_chiSq2 <- chisq.test(edTable2)
ed_chiSq2
# X-squared = 10, df = 9, p-value = 0.3505


## ETHNICITY ##
colnames(DemoData)
# Change T/F to 1/0
updateTF <- c("TRUE" = 1, "FALSE" = 0)
DemoData$AfAm_Black <- as.integer(updateTF[DemoData$AfAm_Black])
DemoData$Indig <- as.integer(updateTF[DemoData$Indig])
DemoData$AsianAm <- as.integer(updateTF[DemoData$AsianAm])
DemoData$C_White <- as.integer(updateTF[DemoData$C_White])
DemoData$Latin <- as.integer(updateTF[DemoData$Latin])
DemoData$MidEastNAf <- as.integer(updateTF[DemoData$MidEastNAf])
DemoData$HW_PI <- as.integer(updateTF[DemoData$HW_PI])

# Get counts 
Ethnicity <- DemoData %>%
  group_by(Group) %>%
  summarize(
    AfAm_Black = sum(AfAm_Black),
    Indig = sum(Indig),
    AsianAm = sum(AsianAm),
    C_White = sum(C_White),
    Latin = sum(Latin),
    MidEastNAf = sum(MidEastNAf),
    HW_PI = sum(HW_PI),
    .groups = 'drop'
  ) %>%
  as.data.frame()
Ethnicity

Ethnicity_long <- Ethnicity %>%
  pivot_longer(cols = c(AfAm_Black, Indig, AsianAm, C_White, Latin, MidEastNAf, HW_PI),
               names_to = "Ethnicity",
               values_to = "Count")

# Chi-square test for education differences 
ethTable <- table(Ethnicity_long$Group, Ethnicity_long$Count)
eth_chiSq <- chisq.test(ethTable)
eth_chiSq
# X-squared = 5.3333, df = 7, p-value = 0.6194

