#Load necessary packages
library(readxl)
library(dplyr)
library(rstatix)

#read excel file 
roiindices<-read_excel("U:\\PhD\\3. Core FTD\\Results\\ROI\\ROI z-scores_coreFTD.xlsx")

roiindices <-roiindices %>%
  filter(Dx!="test")

#Convert Dx from character vector to factor
roiindices$Dx<-factor(roiindices$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


#Means and SDs for magnitude indices

roiindices%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    ATLmag=mean(ATL_mag_resid,na.rm=TRUE),
    ATLmagsd=sd(ATL_mag_resid,na.rm=TRUE),
    OFCmag=mean(OFC_mag_resid,na.rm=TRUE),
    OFCmagsd=sd(OFC_mag_resid,na.rm = TRUE))

#check for normality

roiindices%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
  shapirotest=shapiro_test(ATL_mag_resid),
  shapirotest2=shapiro_test(OFC_mag_resid)
  )

#stats test - ATL Mag

roiindices %>%
  anova_test(ATL_mag_resid~Dx) %>%
  add_significance()


roiindices %>%
  tukey_hsd(ATL_mag_resid~Dx) %>%
  add_significance()

#stats test - OFC Mag

roiindices%>%
  anova_test(OFC_mag_resid~Dx) %>%
  add_significance()

roiindices %>%
  tukey_hsd(OFC_mag_resid~Dx) %>%
  add_significance()

# CORRELATIONS ------------------------------------------------------------

roiindices %>%
  #group_by(Dx) %>%
  cor_test(ATL_mag_resid,OFC_mag_resid) %>%
  add_significance()

roiindices %>%
  group_by(Dx) %>%
  cor_test(ATL_sym_resid,OFC_sym_resid) %>%
  add_significance()


#Create absolute value for asymmetry comparisons

roiindices <-roiindices %>%
  mutate(ATL_sym_ab = sqrt(ATL_sym_resid*ATL_sym_resid)) %>%
  mutate(OFC_sym_ab = sqrt(OFC_sym_resid*OFC_sym_resid))

roiindices%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    ATLsym=mean(ATL_sym_ab,na.rm=TRUE),
    ATLsymsd=sd(ATL_sym_ab,na.rm=TRUE),
    OFCsym=mean(OFC_sym_ab,na.rm=TRUE),
    OFCsymsd=sd(OFC_sym_ab,na.rm=TRUE)
  )


#stats test - ATL sym

roiindices%>%
  anova_test(ATL_sym_ab~Dx) %>%
  add_significance()

roiindices%>%
  tukey_hsd(ATL_sym_ab~Dx) %>%
  add_significance()

#stats test - OFC sym

roiindices%>%
  anova_test(OFC_sym_ab~Dx) %>%
  add_significance()

roiindices%>%
  tukey_hsd(OFC_sym_ab~Dx) %>%
  add_significance()

#correlations

roiindices %>%
  group_by(Dx) %>%
  cor_test(ATL_sym_ab,OFC_sym_ab) %>%
  add_significance()

roiindices %>%
  group_by(Dx) %>%
  cor_test(ATL_mag_resid,OFC_mag_resid) %>%
  add_significance()

roiindices %>%
  group_by(Dx) %>%
  cor_test(ATL_mag_resid,ATL_sym_ab) %>%
  add_significance()

