
#load necessary packages
library(tidyverse)
library(rstatix)
library(ggpubr)
library(readxl)
library(dunn.test)
library(tibble)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Face to name matching ---------------------------------------------------
facename<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Face to name matching") 

#convert to tibble
facename<-as_tibble(facename)

#Convert Dx from character vector to factor
facename$Dx<-factor(facename$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

facename.stats<-facename%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(TOTAL_facetoname,na.rm=TRUE),
    sd=sd(TOTAL_facetoname,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
facename.stats

facename%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    facenameshap=shapiro_test(TOTAL_facetoname)
  )

ggqqplot(facename,"TOTAL_facetoname", facet.by = "Dx")

facename %>%
  kruskal_test(TOTAL_facetoname~Dx) %>%
  add_significance()

dunn_test(facename, TOTAL_facetoname~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(facename, "Dx", "TOTAL_facetoname",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="face-name",
                legend="none",
                ylim=c(0,44))

fig4+ggeasy::easy_center_title()

# Face to profession ------------------------------------------------------
faceprof<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Face to profession") 

#convert to tibble
faceprof<-as_tibble(faceprof)

#Convert Dx from character vector to factor
faceprof$Dx<-factor(faceprof$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

faceprof.stats<-faceprof%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(total,na.rm=TRUE),
    sd=sd(total,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
faceprof.stats

faceprof%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    faceprofshap=shapiro_test(total)
  )

ggqqplot(faceprof,"total", facet.by = "Dx")

faceprof%>%
  kruskal_test(total~Dx) %>%
  add_significance()

dunn_test(faceprof, total~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(faceprof, "Dx", "total",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="faceprof",
                legend="none",
                ylim=c(0,44))

fig4+ggeasy::easy_center_title()

# Landmark to name  -------------------------------------------------------
landmark<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Landmark to name matching") 

#convert to tibble
landmark<-as_tibble(landmark)

#Convert Dx from character vector to factor
landmark$Dx<-factor(landmark$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

landmark.stats<-landmark%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(TOTAL_landmarks,na.rm=TRUE),
    sd=sd(TOTAL_landmarks,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
landmark.stats

landmark%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    landmarkshap=shapiro_test(TOTAL_landmarks)
  )

ggqqplot(landmark,"TOTAL_landmarks", facet.by = "Dx")

landmark%>%
  levene_test(TOTAL_landmarks~Dx)

landmark%>%
  welch_anova_test(TOTAL_landmarks~Dx)%>%
  add_significance()

landmark%>%
  games_howell_test(TOTAL_landmarks ~ Dx,detailed = TRUE)

fig4<-ggboxplot(landmark, "Dx", "TOTAL_landmarks",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="landmarks",
                legend="none",
                ylim=c(0,42))

fig4+ggeasy::easy_center_title()


# Face matching -----------------------------------------------------------

facematching<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Face Matching Upright") 

#convert to tibble
facematching<-as_tibble(facematching)

#Convert Dx from character vector to factor
facematching$Dx<-factor(facematching$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

facematching.stats<-facematching%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    famousmean=mean(Famoustotal,na.rm=TRUE),
    famoussd=sd(Famoustotal,na.rm=TRUE),
    unfamiliarmean=mean(Unfamiliartotal,na.rm=TRUE),
    unfamiliarsd=sd(Unfamiliartotal,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
facematching.stats

facematching%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    famousshap=shapiro_test(Famoustotal),
    unfamilarshap=shapiro_test(Unfamiliartotal)
  )

ggqqplot(facematching,"Famoustotal", facet.by = "Dx")
ggqqplot(facematching,"Unfamiliartotal", facet.by = "Dx")

#comparing famous faces

facematching%>%
  kruskal_test(Famoustotal~Dx)%>%
  add_significance()

dunn_test(facematching, Famoustotal~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(facematching, "Dx", "Famoustotal",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="famous faces",
                legend="none",
                ylim=c(0,22))

fig4+ggeasy::easy_center_title()


#comparing unfamiliar faces

facematching%>%
  kruskal_test(Unfamiliartotal~Dx)%>%
  add_significance()

dunn_test(facematching, Unfamiliartotal~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(facematching, "Dx", "Unfamiliartotal",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="unfamiliar faces",
                legend="none",
                ylim=c(0,22))

fig4+ggeasy::easy_center_title()


# Social roles ------------------------------------------------------------
socialroles<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Social roles") 


#Convert Dx from character vector to factor
socialroles$Dx<-factor(socialroles$Dx,levels=c("Control","Left TLE","Right TLE","bvFTD","SD"))

socialroles.stats<-socialroles%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    socrolmean=mean(TOTALsocroles,na.rm=TRUE),
    socrolsd=sd(TOTALsocroles,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
socialroles.stats

socialroles%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    wpmshap=shapiro_test(TOTALsocroles)
  )

ggqqplot(socialroles,"TOTALsocroles", facet.by = "Dx")

socialroles%>%
  kruskal_test(TOTALsocroles~Dx)%>%
  add_significance()


dunn_test(socialroles, TOTALsocroles~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(socialroles, "Dx", "TOTALsocroles",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="social roles",
                legend="none",
                ylim=c(0,35))

fig4+ggeasy::easy_center_title()

# Social concepts ---------------------------------------------------------

socialconcepts<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Social synonym") 

#Convert Dx from character vector to factor
socialconcepts$Dx<-factor(socialconcepts$Dx,levels=c("Control","Left TLE","Right TLE","bvFTD","SD"))

#social synonym

#calculate mean and standard deviation

socialconcepts%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    socialmean=mean(Social_socialsynonym,na.rm=TRUE),
    socialsd=sd(Social_socialsynonym,na.rm=TRUE),
    nonsocialmean=mean(NonSocial_socialsynonym,na.rm=TRUE),
    nonsocialsd=sd(NonSocial_socialsynonym,na.rm = TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))

#shapiro test for normality

socialconcepts%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    socialshap=shapiro_test(Social_socialsynonym),
    nonsocialshap=shapiro_test(NonSocial_socialsynonym)
  )

#QQ plots

ggqqplot(socialconcepts,"Social_socialsynonym", facet.by = "Dx")

ggqqplot(socialconcepts,"NonSocial_socialsynonym", facet.by = "Dx")

#correlation between social and non social concepts

socialconcepts %>%
  group_by(Dx) %>%
  cor_test(Social_socialsynonym,NonSocial_socialsynonym, method="spearman") %>%#
  add_significance()

#Kruskal-Wallis test

#social
socialconcepts %>%
  kruskal_test(Social_socialsynonym~Dx)%>%
  add_significance()

#post hoc dunn test
dunn_test(socialconcepts, Social_socialsynonym~Dx, p.adjust.method = "holm", detailed = TRUE)


socialconcepts %>%
  kruskal_test(NonSocial_socialsynonym~Dx) %>%
  add_significance()


dunn_test(socialconcepts, NonSocial_socialsynonym~Dx, p.adjust.method = "holm", detailed = TRUE)



fig4<-ggboxplot(socialconcepts, "Dx", "Social_socialsynonym",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="social concepts",
                legend="none",
                ylim=c(0,36))

fig4+ggeasy::easy_center_title()



fig4<-ggboxplot(socialconcepts, "Dx", "NonSocial_socialsynonym",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="non-social concepts",
                legend="none",
                ylim=c(0,36))

fig4+ggeasy::easy_center_title()

# Basic emotion -----------------------------------------------------------

basicemotion<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Basic Emotion") 

#Convert Dx from character vector to factor
basicemotion$Dx<-factor(basicemotion$Dx,levels=c("Control","Left TLE","Right TLE","bvFTD","SD"))

basicemotion.stats<-basicemotion%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(TOTALbasic,na.rm=TRUE),
    sd=sd(TOTALbasic,na.rm=TRUE)
  )

basicemotion.stats

%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
basicemotion.stats

basicemotion%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    landmarkshap=shapiro_test(TOTALbasic)
  )

ggqqplot(basicemotion,"TOTALbasic", facet.by = "Dx")

basicemotion%>%
  kruskal_test(TOTALbasic~Dx)%>%
  add_significance()

dunn_test(basicemotion, TOTALbasic~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(basicemotion, "Dx", "TOTALbasic",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="basic emotions",
                legend="none",
                ylim=c(0,19))

fig4+ggeasy::easy_center_title()



# Complex emotion ---------------------------------------------------------
complexemotion<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Complex Emotion") 

#convert to tibble
complexemotion<-as_tibble(complexemotion)

#Convert Dx from character vector to factor
complexemotion$Dx<-factor(complexemotion$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

complexemotion.stats<-complexemotion%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(complexTOTAL,na.rm=TRUE),
    sd=sd(complexTOTAL,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
complexemotion.stats

complexemotion%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    landmarkshap=shapiro_test(complexTOTAL)
  )

ggqqplot(complexemotion,"complexTOTAL", facet.by = "Dx")

complexemotion%>%
  levene_test(complexTOTAL~Dx)

complexemotion%>%
  welch_anova_test(complexTOTAL~Dx)%>%
  add_significance()

complexemotion%>%
  games_howell_test(complexTOTAL ~ Dx,detailed = TRUE)

fig4<-ggboxplot(complexemotion, "Dx", "complexTOTAL",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="complex emotions",
                legend="none",
                ylim=c(0,23))

fig4+ggeasy::easy_center_title()

# SNQ ---------------------------------------------------------------------
snq<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="SNQ") 

#convert to tibble
snq<-as_tibble(snq)

#Convert Dx from character vector to factor
snq$Dx<-factor(snq$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

snq.stats<-snq%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(SNQ_total,na.rm=TRUE),
    sd=sd(SNQ_total,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
snq.stats

snq%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    landmarkshap=shapiro_test(SNQ_total,na.rm=TRUE)
  )

snq%>%
  kruskal_test(SNQ_total~Dx)%>%
  add_significance()

dunn_test(snq, SNQ_total~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(snq, "Dx", "SNQ_total",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="snq",
                legend="none",
                ylim=c(0,22))

fig4+ggeasy::easy_center_title()

# TASIT -------------------------------------------------------------------

tasit<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="TASIT") 

#convert to tibble
tasit<-as_tibble(tasit)

#Convert Dx from character vector to factor
tasit$Dx<-factor(tasit$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

#sincere

tasit.stats<-tasit%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(sincere,na.rm=TRUE),
    sd=sd(sincere,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
tasit.stats

tasit%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    landmarkshap=shapiro_test(sincere,na.rm=TRUE)
  )

ggqqplot(tasit,"sincere", facet.by = "Dx")


tasit %>%
  kruskal_test(sincere~Dx) %>%
  add_significance()


#sarcasm

tasit.stats<-tasit%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    mean=mean(sarcasm,na.rm=TRUE),
    sd=sd(sarcasm,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
tasit.stats

tasit%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    landmarkshap=shapiro_test(sarcasm)
  )

tasit%>%
  kruskal_test(sarcasm~Dx)%>%
  add_significance()

dunn_test(tasit, sarcasm~Dx, p.adjust.method = "holm", detailed = TRUE)

fig4<-ggboxplot(tasit, "Dx", "sarcasm",  fill = "Dx",
                palette = "simpsons",
                add = c("boxplot"),
                title="tasit",
                legend="none",
                ylim=c(0,24))

fig4+ggeasy::easy_center_title()

