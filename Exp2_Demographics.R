# Exp. 2 Demographics 

library(tidyr)
library(dplyr)

load("Data/Exp2_Demos/Exp2DemoData_Aut.RData")
load("Data/Exp2_Demos/Exp2DemoData_NonAut.RData")
load("Data/Exp2_Demos/Exp2Demos.RData")

# Descriptives
## AGE ##
Demographics$Age <- as.integer(Demographics$Age)
Age <- Demographics %>%
  group_by(Group) %>%
  summarize(
    Min = min(Age),
    Max = max(Age),
    Mean = mean(Age), 
    SD = sd(Age)
  )
Age

# t-Test for age differences 
t.test(DemoData_Aut$Age, DemoData_NonAut$Age, paired = FALSE)
# t = 1.8265, df = 158, p-value = 0.06966


## GENDER ##
unique(Demographics$Gender)
Gender <- Demographics %>%
  group_by(Group) %>%
  summarize(
    Woman = sum(Gender == "WOMAN"),
    Man = sum(Gender == "MAN"), 
    NonBinary = sum(Gender == "NONBINARY"), 
    NoAns = sum(Gender == ""),
    Other = sum(Gender == "OTHER")
  )
Gender

# Chi-square test for gender differences 
genderTable <- table(DemoData_Aut$Gender, DemoData_NonAut$Gender)
gender_chiSq <- chisq.test(genderTable)
gender_chiSq
# X-squared = 41.318, df = 8, p-value = 1.818e-06

# Review additional information provided about gender
GenderAdd <- Demographics %>%
  filter(GenderAdd != "")
GenderAdd_Aut <- GenderAdd %>%
  filter(Group == "Aut")
# 2 trans women; 1 trans man; 1 gender neutral/non-conforming; 3 "NA"
GenderAdd_NonAut <- GenderAdd %>%
  filter(Group == "NonAut")
# No self-reports of transgender (n=3 "NA")


## EDUCATION ##
unique(Demographics$Edu)
Education <- Demographics %>%
  group_by(Group) %>%
  summarize(
    HS_GED = sum(Edu == "HIGH SCHOOL DIPLOMA OR GED"),
    SOMECOLLEGE = sum(Edu == "SOME COLLEGE"), 
    ASSOCIATE = sum(Edu == "ASSOCIATE'S DEGREE"), 
    BACH = sum(Edu == "BACHELOR'S DEGREE"), 
    MASTER = sum(Edu == "MASTER'S DEGREE"),
    PHD_DOC = sum(Edu == "PHD OR DOCTORATE"),
    Other = sum(Edu == "OTHER"), 
    NORESP = sum(Edu == "")
  )
Education

# Chi-square test for gender differences 
EduTable <- table(DemoData_Aut$Edu, DemoData_NonAut$Edu)
Edu_chiSq <- chisq.test(EduTable)
Edu_chiSq
# X-squared = 18.479, df = 30, p-value = 0.9503


## ETHNICITY ##
unique(Demographics$Race)
Ethnicity <- Demographics %>%
  group_by(Group) %>%
  summarize(
    White = sum(Race == "CAUCASIAN / WHITE"),
    White_Latine = sum(Race == "CAUCASIAN / WHITE,HISPANIC / LATINE"),
    Black = sum(Race == "AFRICAN AMERICAN / BLACK"),
    Black_Latine = sum(Race == "AFRICAN AMERICAN / BLACK,HISPANIC / LATINE"),
    Hispanic = sum(Race == "HISPANIC / LATINE"),
    Asian = sum(Race == "ASIAN AMERICAN"),
    Indig = sum(Race == "AMERICAN INDIAN / NATIVE AMERICAN"),
    Multiracial = sum((Race == "AFRICAN AMERICAN / BLACK,CAUCASIAN / WHITE") + (Race == "AFRICAN AMERICAN / BLACK,ASIAN AMERICAN,HISPANIC / LATINE") + (Race == "AMERICAN INDIAN / NATIVE AMERICAN,CAUCASIAN / WHITE")),
    Other_NA = sum(Race == "OTHER / PREFER NOT TO ANSWER")
  )
Ethnicity

# Chi-square test for gender differences 
EthnicityTable <- table(DemoData_Aut$Race, DemoData_NonAut$Race)
Ethnicity_chiSq <- chisq.test(EthnicityTable)
Ethnicity_chiSq
# X-squared = 52.073, df = 56, p-value = 0.6243

AutEth <- Ethnicity %>% filter(Group == "Aut")
sum(AutEth[1, 2:10]) # 80

NonAutEth <- Ethnicity %>% filter(Group == "NonAut")
sum(NonAutEth[1, 2:10]) # 80