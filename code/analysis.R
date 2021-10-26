
  
  #1.1. Cleaning workspace
  
rm(list=ls(all=T))

#1.2. Load libraries

# if needed tinytex::install_tinytex()
library(tinytex)
library(readxl)
library(here)
library(tidyverse)
library(broom)
library(ez)
library(ggpubr)
library(rstatix)
library(readr)
library(coin)
library(lsr)
library(FSA)
library(psych)
library(doBy)
library(car)
options(scipen = 999)

#1.3. Read files

library(readr)
my_data <- read_csv("https://raw.githubusercontent.com/LMD-nat/gesture-14/main/data/g14.csv", 
                    col_types = cols(...1 = col_skip()), 
                    na = "NA")

##2. Analyses
#2.1. Participants' demographic characteristics (Table 1)

#Age in months
age <- my_data %>%
  group_by(Lang_Group) %>%
  summarize(Num = length(Age_Months),
            Mean = mean(Age_Months),
            SD = sd(Age_Months),
            SE = SD/sqrt(Num))

#Maternal education
mat_education <- my_data %>%
  group_by(Lang_Group) %>%
  summarize(Num = length(Mat_Edu_Years),
            Mean = mean(Mat_Edu_Years),
            SD = sd(Mat_Edu_Years),
            SE = SD/sqrt(Num))

#Dominant language
dom_lang <- my_data %>%
  group_by (Lang_Group, Dom_Lang) %>%
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

#Language exposure to dominant language
my_data <- my_data%>%
  mutate(exposure_dom_lang = case_when(Dom_Lang =="EN" ~ LanguageExpEng,
                                       Dom_Lang == "FR" ~ LanguageExpFr,
                                       TRUE ~ 999))

expo_dom_lge <- my_data %>% group_by(Lang_Group) %>%
  summarize(Num = length(exposure_dom_lang),
            Mean = mean(exposure_dom_lang),
            SD = sd(exposure_dom_lang),
            SE = SD/sqrt(Num))

#Gender
gender <-my_data %>%
  group_by (Lang_Group, Gender) %>%
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

#2.2. Vocabulary Measures
#2.2.1. Descriptive statistics for vocabulary measures

#Changing language group from character to factor
my_data<- my_data%>% mutate(Lang_Group = as.factor(Lang_Group))

#Creating data-frame including only vocabulary measures
vocabulary <- my_data %>%
  select('Lang_Group', 'PROD_Dom', 'PROD_NonDom', 'PROD_NonDom_ANOVA','COMP_Dom', 'COMP_NonDom', 'COMP_NonDom_ANOVA','COMP_Total','PROD_Total')

#Receptive-dominant/unique language
describeBy(vocabulary$COMP_Dom, vocabulary$Lang_Group, mat = TRUE) 
#Receptive-non-dominant language
describeBy(vocabulary$COMP_NonDom, vocabulary$Lang_Group, mat = TRUE)
#Receptive total (English + French)
describeBy(vocabulary$COMP_Total, vocabulary$Lang_Group, mat = TRUE) 
#Expressive-dominant/unique language
describeBy(vocabulary$PROD_Dom, vocabulary$Lang_Group, mat = TRUE) 
#Expressive-non-dominant language
describeBy(vocabulary$PROD_NonDom, vocabulary$Lang_Group, mat = TRUE)
#Expressive-total (English + French)
describeBy(vocabulary$PROD_Total, vocabulary$Lang_Group, mat = TRUE) 

#2.2.2. Anovas and pairwise tests for vocabulary measures

##Receptive vocabulary dominant/unique language
#Checking normality
shapiro.test(vocabulary$COMP_Dom)
#Checking homogeneity of variance
leveneTest(COMP_Dom ~ Lang_Group, data = vocabulary)
#ANOVA
receptive.dom.aov <- vocabulary %>% anova_test(COMP_Dom ~ Lang_Group)
receptive.dom.aov
#Pairwise comparisons
receptice.dom.pwc <- vocabulary %>% tukey_hsd(COMP_Dom ~ Lang_Group)
receptice.dom.pwc

## Receptive vocabulary non-dominant language
#Checking normality
shapiro.test(vocabulary$COMP_NonDom_ANOVA)
#Checking homogeneity of variance
leveneTest(COMP_NonDom_ANOVA ~ Lang_Group, data = vocabulary)
#ANOVA
receptive.nondom.aov <- vocabulary %>% anova_test(COMP_NonDom_ANOVA ~ Lang_Group)
receptive.nondom.aov
#Pairwise comparisons
receptive.nondom.pwc<- vocabulary %>% tukey_hsd(COMP_NonDom_ANOVA ~ Lang_Group)
receptive.nondom.pwc

## Total receptive vocabulary (English + French)
#Checking normality
shapiro.test(vocabulary$COMP_Total)
#Checking homogeneity of variance
leveneTest(COMP_Total ~ Lang_Group, data = vocabulary)
#One-way analysis of means (not assuming equal variances)
oneway.test(COMP_Total ~ Lang_Group, data = vocabulary)
#Pairwise t-tests with no assumption of equal variances
pairwise.t.test(vocabulary$COMP_Total, vocabulary$Lang_Group,
                p.adjust.method = "BH", pool.sd = FALSE)

##Expressive vocabulary dominant/unique language
#Checking normality
shapiro.test(vocabulary$PROD_Dom)
#Checking homogeneity of variance
leveneTest(PROD_Dom ~ Lang_Group, data = vocabulary)
#ANOVA
expressive.dom.aov <- vocabulary %>% anova_test(PROD_Dom ~ Lang_Group)
expressive.dom.aov
#Pairwise comparisons
expressive.dom.pwc <- vocabulary %>% tukey_hsd(PROD_Dom ~ Lang_Group)
expressive.dom.pwc

##Expressive vocabulary non-dominant language
#Checking normality
shapiro.test(vocabulary$PROD_NonDom_ANOVA)
#Checking homogeneity of variance
leveneTest(PROD_NonDom_ANOVA ~ Lang_Group, data = vocabulary)
#One-way analysis of means (not assuming equal variances)
oneway.test(PROD_NonDom_ANOVA ~ Lang_Group, data = vocabulary)
#Pairwise t-tests with no assumption of equal variances
pairwise.t.test(vocabulary$PROD_NonDom_ANOVA, vocabulary$Lang_Group,
                p.adjust.method = "BH", pool.sd = FALSE)

##Total expressive vocabulary (English + French)
#Checking normality
shapiro.test(vocabulary$PROD_Total)
#Checking homogeneity of variance
leveneTest(PROD_Total ~ Lang_Group, data = vocabulary)
#ANOVA
expressive.total.aov <- vocabulary %>% anova_test(PROD_Total ~ Lang_Group)
expressive.total.aov
#Pairwise comparisons
expressive.total.pwc <- vocabulary %>% tukey_hsd(PROD_Total ~ Lang_Group)
expressive.total.pwc

#2.3. Gestures and actions analyses
#2.3.1. Checking assumptions for ANOVAs

#Checking normality
describe(my_data$A_SUM,type = 3)
describe(my_data$C_SUM,type = 3)
describe(my_data$D_SUM,type = 3)
describe(my_data$E_SUM,type = 3)
describe(my_data$CDE_SUM,type = 3)
describe(my_data$Tot_End,type = 3)

describeBy(my_data$A_SUM, my_data$Lang_Group, mat = TRUE) 
describeBy(my_data$C_SUM, my_data$Lang_Group, mat = TRUE) 
describeBy(my_data$D_SUM, my_data$Lang_Group, mat = TRUE) 
describeBy(my_data$E_SUM, my_data$Lang_Group, mat = TRUE) 
describeBy(my_data$CDE_SUM, my_data$Lang_Group, mat = TRUE)
describeBy(my_data$Tot_End, my_data$Lang_Group, mat = TRUE) 

#Checking homogeneity of variance
leveneTest(A_SUM ~ Lang_Group*Gender, data = my_data)
leveneTest(C_SUM ~ Lang_Group*Gender, data = my_data)
leveneTest(D_SUM ~ Lang_Group*Gender, data = my_data)
leveneTest(E_SUM ~ Lang_Group*Gender, data = my_data)
leveneTest(CDE_SUM ~ Lang_Group*Gender, data = my_data)
leveneTest(Tot_End ~ Lang_Group*Gender, data = my_data)

#Visualizations
res.aov_A <- aov(A_SUM ~ Lang_Group*Gender, data = my_data)
plot(res.aov_A, 2)

res.aov_C <- aov(C_SUM ~ Lang_Group*Gender, data = my_data)
plot(res.aov_C, 2)

res.aov_D <- aov(D_SUM ~ Lang_Group*Gender, data = my_data)
plot(res.aov_D, 2)

res.aov_E <- aov(E_SUM ~ Lang_Group*Gender, data = my_data)
plot(res.aov_E, 2)

res.aov_CDE <- aov(CDE_SUM ~ Lang_Group*Gender, data = my_data)
plot(res.aov_CDE, 2)

res.aov_Tot_End <- aov(Tot_End ~ Lang_Group*Gender, data = my_data)
plot(res.aov_Tot_End, 2)

#2.3.2. ANOVAs and pairwise tests for gestures and actions sections

##ANOVA Section A and planned t-test
ANOVA_sectionA <- my_data %>% anova_test(A_SUM ~ Lang_Group * Gender, type = 3)
ANOVA_sectionA

t_test_sectionA <- t.test(A_SUM~Gender, data = my_data, var.equal = TRUE)
t_test_sectionA
cohensD(my_data$A_SUM~my_data$Gender)

##ANOVA Section C and planned t-test
ANOVA_sectionC <- my_data %>% anova_test(C_SUM ~ Lang_Group * Gender, type = 3)
ANOVA_sectionC

t_test_sectionC <- t.test(C_SUM~Gender, data = my_data, var.equal = TRUE)
t_test_sectionC
cohensD(my_data$C_SUM~my_data$Gender)

##Anova Section D, Kruskal-Wallis (nonparametric) and planned t-test

#Nonparametric
kruskal.test(D_SUM~Lang_Group, data = my_data)
kruskal_effsize(D_SUM~Lang_Group, data = my_data)

kruskal.test(D_SUM~Gender, data = my_data)
kruskal_effsize(D_SUM~Gender, data = my_data)
pairwise.wilcox.test(my_data$D_SUM, my_data$Gender,
                     p.adjust.method = "BH")

#Parametric
ANOVA_sectionD <- my_data %>% anova_test(D_SUM ~ Lang_Group * Gender, type = 3)
ANOVA_sectionD

t_test_sectionD <- t.test(D_SUM~Gender, data = my_data, var.equal = TRUE)
t_test_sectionD
cohensD(my_data$D_SUM~my_data$Gender)

##ANOVA Section E
ANOVA_sectionE <- my_data %>% anova_test(E_SUM ~ Lang_Group * Gender, type = 3)
ANOVA_sectionE

##ANOVA Section CDE and planned t-test
ANOVA_sectionCDE <- my_data %>% anova_test(CDE_SUM ~ Lang_Group * Gender, type = 3)
ANOVA_sectionCDE

t_test_sectionCDE <- t.test(CDE_SUM~Gender, data = my_data, var.equal = TRUE)
t_test_sectionCDE
cohensD(my_data$CDE_SUM~my_data$Gender)

##ANOVA Total Endorsement and planned t-test
ANOVA_TotEnd <- my_data %>% anova_test(Tot_End ~ Lang_Group * Gender, type = 3)
ANOVA_TotEnd

t_test_TotEnd <- t.test(Tot_End~Gender, data = my_data, var.equal = TRUE)
t_test_TotEnd
cohensD(my_data$Tot_End~my_data$Gender)

#2.4. Group means on gesture and actions by language group (Table 3)

#Means and SD for all sections
means_gestures_byLangGroup <- my_data %>%
  group_by(Lang_Group) %>%
  summarize(across(A_SUM:Tot_End,
                   list(num = length, mean = mean, sd = sd)))

#2.5. Group means on gesture and actions by gender (Table 4)

#Means and SD for all sections
means_gestures_byGender <- my_data %>%
  group_by(Gender) %>%
  summarize(across(A_SUM:Tot_End,
                   list(num = length, mean = mean, sd = sd)))

#2.6. Exploratory analyses
#2.6.1. Analyses by language dominance 

#Means and SD for each section by language dominance
means_gestures_byDomLang <- my_data %>%
  group_by(Dom_Lang) %>%
  summarize(across(A_SUM:Tot_End,
                   list(num = length, mean = mean, sd = sd)))

###t-tests###

##Checking normality
#*indicates the distribution is non-normal
#Shapiro-Wilk normality test for En on Section A
with(my_data, shapiro.test(A_SUM[Dom_Lang == "EN"]))# W = 0.96283, p-value = 0.02253*
#Shapiro-Wilk normality test for Fr on Section A
with(my_data, shapiro.test(A_SUM[Dom_Lang == "FR"]))# W = 0.95653, p-value = 0.01414*
#Shapiro-Wilk normality test for En on Section C
with(my_data, shapiro.test(C_SUM[Dom_Lang == "EN"]))# W = 0.98128, p-value = 0.3074
#Shapiro-Wilk normality test for Fr on Section C
with(my_data, shapiro.test(C_SUM[Dom_Lang == "FR"]))# W = 0.97702, p-value = 0.2104
#Shapiro-Wilk normality test for En on Section D
with(my_data, shapiro.test(D_SUM[Dom_Lang == "EN"]))# W = 0.8784, p-value < 0.001*
#Shapiro-Wilk normality test for Fr on Section D
with(my_data, shapiro.test(D_SUM[Dom_Lang == "FR"]))# W = 0.83751, p-value < 0.001*
#Shapiro-Wilk normality test for En on Section E
with(my_data, shapiro.test(E_SUM[Dom_Lang == "EN"]))# W = 0.97122, p-value = 0.07439
#Shapiro-Wilk normality test for Fr on Section E
with(my_data, shapiro.test(E_SUM[Dom_Lang == "FR"]))# W = 0.95949, p-value = 0.02067*
#Shapiro-Wilk normality test for En on Section CDE
with(my_data, shapiro.test(CDE_SUM[Dom_Lang == "EN"]))# W = 0.98552, p-value = 0.5237
#Shapiro-Wilk normality test for Fr on Section CDE
with(my_data, shapiro.test(CDE_SUM[Dom_Lang == "FR"]))# W = 0.96719, p-value = 0.05698
#Shapiro-Wilk normality test for En on Total Endorsement
with(my_data, shapiro.test(Tot_End[Dom_Lang == "EN"]))# W = 0.98373, p-value = 0.4221
#Shapiro-Wilk normality test for Fr on Total Endorsement
with(my_data, shapiro.test(Tot_End[Dom_Lang == "FR"]))# W = 0.96186, p-value = 0.02816*

##Checking homogeneity of variances using F-test
SectionA.variance <- var.test(A_SUM ~ Dom_Lang, data = my_data)%>%tidy()
SectionA.variance # equality of variances assumed
SectionC.variance <- var.test(C_SUM ~ Dom_Lang, data = my_data)%>%tidy()
SectionC.variance # equality of variances assumed
SectionD.variance <- var.test(D_SUM ~ Dom_Lang, data = my_data)%>%tidy()
SectionD.variance # equality of variances assumed
SectionE.variance <- var.test(E_SUM ~ Dom_Lang, data = my_data)%>%tidy()
SectionE.variance # equality of variances assumed
SectionCDE.variance <- var.test(CDE_SUM ~ Dom_Lang, data = my_data)%>%tidy()
SectionCDE.variance # equality of variances assumed
SectionTot_End.variance <- var.test(Tot_End ~ Dom_Lang, data = my_data)%>%tidy()
SectionTot_End.variance # equality of variances assumed

#t-test Section A
t_test_dominance_sectionA <- t.test(A_SUM ~ Dom_Lang, data = my_data, var.equal = TRUE)
t_test_dominance_sectionA 
cohensD(my_data$A_SUM~my_data$Dom_Lang)
#Non-parametric 
wilcox.test(A_SUM~Dom_Lang, data = my_data) 

#t-test Section C
t_test_dominance_sectionC <- t.test(C_SUM ~ Dom_Lang, data = my_data, var.equal = TRUE)
t_test_dominance_sectionC 
cohensD(my_data$C_SUM~my_data$Dom_Lang)

#t-test Section D
t_test_dominance_sectionD <- t.test(D_SUM ~ Dom_Lang, data = my_data, var.equal = TRUE)
t_test_dominance_sectionD
cohensD(my_data$D_SUM~my_data$Dom_Lang)
#Non-parametric 
wilcox.test(D_SUM~Dom_Lang, data = my_data) 

#t-test Section E
t_test_dominance_sectionE <- t.test(E_SUM ~ Dom_Lang, data = my_data, var.equal = TRUE)
t_test_dominance_sectionE 
cohensD(my_data$E_SUM~my_data$Dom_Lang)
#Non-parametric 
wilcox.test(E_SUM~Dom_Lang, data = my_data) 

#t-test Sections CDE 
t_test_dominance_sectionCDE <- t.test(CDE_SUM ~ Dom_Lang, data = my_data, var.equal = TRUE)
t_test_dominance_sectionCDE
cohensD(my_data$CDE_SUM~my_data$Dom_Lang)

#t-test Total Endorsement 
t_test_dominance_TotEnd <- t.test(Tot_End ~ Dom_Lang, data = my_data, var.equal = TRUE)
t_test_dominance_TotEnd
cohensD(my_data$Tot_End~my_data$Dom_Lang)
#Non-parametric 
wilcox.test(Tot_End~Dom_Lang, data = my_data) 

#2.6.2. Correlations between vocabulary measures and gestures for all sections (Table 5)

corr_allsections <- my_data %>%
  group_by(Lang_Group)%>%
  cor_test(
    vars = c("A_SUM", "C_SUM", "D_SUM", "E_SUM"),
    vars2 = c("COMP_Dom","COMP_NonDom_ANOVA", "COMP_Total", "PROD_Dom", "PROD_NonDom_ANOVA", "PROD_Total")
  )

#2.7. Cohen's Kappa (agreement between forms for bilingual and exposed infants)

#Load library
library(irr)

#Read_files
kappa_data <-read_csv(here("kappa_analyses.csv"), na = c("NA"))

#Split data into sections
kappa_sectionA<-kappa_data%>%
  filter(section == "A")

kappa_sectionC<-kappa_data%>%
  filter(section == "C")

kappa_sectionD<-kappa_data%>%
  filter(section == "D")

kappa_sectionE<-kappa_data%>%
  filter(section == "E")

kappa_sectionCDE<-kappa_data%>%
  filter(section == "C"|section =="D"|section=="E")

#Calculate Kappa
kappa2(kappa_sectionA[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionC[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionD[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionE[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionCDE[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_data[, c("score_en","score_fr")],weight = "unweighted")



#2.7. Cohen's Kappa (agreement between forms for bilingual and exposed infants)

#Load library
library(irr)

#Read_files
kappa_data <-read_csv(("https://raw.githubusercontent.com/LMD-nat/gesture-14/main/data/g14_kappa.csv"), na = "NA")

#Split data into sections
kappa_sectionA<-kappa_data%>%
  filter(section == "A")

kappa_sectionC<-kappa_data%>%
  filter(section == "C")

kappa_sectionD<-kappa_data%>%
  filter(section == "D")

kappa_sectionE<-kappa_data%>%
  filter(section == "E")

kappa_sectionCDE<-kappa_data%>%
  filter(section == "C"|section =="D"|section=="E")

#Calculate Kappa
kappa2(kappa_sectionA[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionC[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionD[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionE[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_sectionCDE[, c("score_en","score_fr")],weight = "unweighted")
kappa2(kappa_data[, c("score_en","score_fr")],weight = "unweighted")



##3. Analyses requested by reviewers

#3.1. Checks for homogeneity of variance for each anova using Levene's Test

##Checking homogeneity of levenes using F-test
SectionA.levene <- leveneTest(A_SUM ~ Dom_Lang*Gender, data = my_data)%>%tidy()
round(SectionA.levene, 3)

SectionC.levene <- leveneTest(C_SUM ~ Dom_Lang*Gender, data = my_data)%>%tidy()
round(SectionC.levene, 3)

SectionD.levene <- leveneTest(D_SUM ~ Dom_Lang*Gender, data = my_data)%>%tidy()
round(SectionD.levene, 3)

SectionE.levene <- leveneTest(E_SUM ~ Dom_Lang*Gender, data = my_data)%>%tidy()
round(SectionE.levene, 3)

SectionCDE.levene <- leveneTest(CDE_SUM ~ Dom_Lang*Gender, data = my_data)%>%tidy()
round(SectionCDE.levene, 3)

SectionTot_End.levene <- leveneTest(Tot_End ~ Dom_Lang*Gender, data = my_data)%>%tidy()
round(SectionTot_End.levene, 3)


#3.1.2 Checks for homogeneity of variance for each anova using Bartlett's Test: assumes normality

##Checking homogeneity of bartletts using F-test
SectionA.bartlett <- bartlett.test(A_SUM ~ interaction(Dom_Lang,Gender), data = my_data)%>%tidy()
SectionA.bartlett # equality of variance assumed
SectionC.bartlett <- bartlett.test(C_SUM ~ interaction(Dom_Lang,Gender), data = my_data)%>%tidy()
SectionC.bartlett # equality of variance assumed
SectionD.bartlett <- bartlett.test(D_SUM ~ interaction(Dom_Lang,Gender), data = my_data)%>%tidy()
SectionD.bartlett # equality of variance not assumed here
SectionE.bartlett <- bartlett.test(E_SUM ~ interaction(Dom_Lang,Gender), data = my_data)%>%tidy()
SectionE.bartlett # equality of variance assumed
SectionCDE.bartlett <- bartlett.test(CDE_SUM ~ interaction(Dom_Lang,Gender), data = my_data)%>%tidy()
SectionCDE.bartlett # equality of variance assumed
SectionTot_End.bartlett <- bartlett.test(Tot_End ~ interaction(Dom_Lang,Gender), data = my_data)%>%tidy()
SectionTot_End.bartlett # equality of variance assumed


#3.2. Anovas with language dominance entered as a factor

##ANOVA Section A and planned t-test
ANOVA_sectionA_dom <- my_data %>% anova_test(A_SUM ~ Lang_Group * Gender * Dom_Lang, type = 3)
ANOVA_sectionA_dom

t_test_sectionA_dom <- t.test(A_SUM~Dom_Lang, data = my_data, var.equal = TRUE)
t_test_sectionA_dom
cohensD(my_data$A_SUM~my_data$Dom_Lang)

##ANOVA Section C and planned t-test
ANOVA_sectionC_dom <- my_data %>% anova_test(C_SUM ~ Lang_Group * Gender * Dom_Lang, type = 3)
ANOVA_sectionC_dom

t_test_sectionC_dom <- t.test(C_SUM~Dom_Lang, data = my_data, var.equal = TRUE)
t_test_sectionC_dom
cohensD(my_data$C_SUM~my_data$Dom_Lang)


##ANOVA Section D and planned t-test
ANOVA_sectionD_dom <- my_data %>% anova_test(D_SUM ~ Lang_Group * Gender * Dom_Lang, type = 3)
ANOVA_sectionD_dom

t_test_sectionD_dom <- t.test(D_SUM~Dom_Lang, data = my_data, var.equal = TRUE)
t_test_sectionD_dom
cohensD(my_data$D_SUM~my_data$Dom_Lang)

##ANOVA Section E
ANOVA_sectionE_dom <- my_data %>% anova_test(E_SUM ~ Lang_Group * Gender * Dom_Lang, type = 3)
ANOVA_sectionE_dom

t_test_sectionE_dom <- t.test(E_SUM~Dom_Lang, data = my_data, var.equal = TRUE)
t_test_sectionE_dom
cohensD(my_data$E_SUM~my_data$Dom_Lang)

##ANOVA Section CDE
ANOVA_sectionCDE_dom <- my_data %>% anova_test(CDE_SUM ~ Lang_Group * Gender * Dom_Lang, type = 3)
ANOVA_sectionCDE_dom

t_test_sectionCDE_dom <- t.test(CDE_SUM~Dom_Lang, data = my_data, var.equal = TRUE)
t_test_sectionCDE_dom
cohensD(my_data$CDE_SUM~my_data$Dom_Lang)

##ANOVA Total Endorsement and planned t-test
ANOVA_TotEnd_dom <- my_data %>% anova_test(Tot_End ~ Lang_Group * Gender * Dom_Lang, type = 3)
ANOVA_TotEnd_dom

t_test_TotEnddom <- t.test(Tot_End~Dom_Lang, data = my_data, var.equal = TRUE)
t_test_TotEnddom
cohensD(my_data$Tot_End~my_data$Dom_Lang)

