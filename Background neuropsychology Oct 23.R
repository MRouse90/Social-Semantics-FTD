#Background Neuropsychology - CORE FTD TLE patients 


#load necessary packages
library(readxl)
library(dplyr)

library(tidyverse)
library(rstatix)
library(ggpubr)
library(dunn.test)
library(ggplot2)
library(ggpubr)


#### NEUROPSYCHOLOGY ####e
Acer<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="ACE-R")

#Convert Dx from character vector to factor
Acer$Dx<-factor(Acer$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

#ACE-R means& SDs
Acer.stats<-Acer%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    Acermean=mean(ACER_TOTAL,na.rm=TRUE),
    AcerSD=sd(ACER_TOTAL,na.rm=TRUE),
    mmsemean=mean(MMSE,na.rm=TRUE),
    mmsesd=sd(MMSE, na.rm = TRUE),
    attentionmean=mean(ACER_ATTENTION,na.rm=TRUE),
    attentionSD=sd(ACER_ATTENTION,na.rm = TRUE),
    memorymean=mean(ACER_MEMORY,na.rm=TRUE),
    memorysd=sd(ACER_MEMORY,na.rm = TRUE),
    fluencymean=mean(ACER_FLUENCY,na.rm=TRUE),
    fluencysd=sd(ACER_FLUENCY,na.rm=TRUE),
    languagemean=mean(ACER_LANGUAGE,na.rm=TRUE),
    languagesd=sd(ACER_LANGUAGE,na.rm = TRUE),
    visuospatialmean=mean(ACER_VISUOSPATIAL,na.rm=TRUE),
    visuospatialsd=sd(ACER_VISUOSPATIAL,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
Acer.stats



#ACER TOTAL

#check data for normality

Acer%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    ACER=shapiro_test(ACER_TOTAL)
  )

ggqqplot(Acer, "ACER_TOTAL", facet.by = "Dx")

Acer%>%
  kruskal_test(ACER_TOTAL~Dx)%>%
  add_significance()

dunn_test(Acer, ACER_TOTAL~Dx, p.adjust.method = "holm", detailed = TRUE)


#MMSE
Acer%>%
  group_by(Dx) %>%
  summarise(
    count=n(),
    MMSE=shapiro_test(MMSE)
  )

ggqqplot(Acer,"MMSE",facet.by = "Dx")

Acer%>%
  kruskal_test(MMSE~Dx) %>%
  add_significance()

dunn_test(Acer, MMSE~Dx, p.adjust.method = "holm", detailed = TRUE)

#Attention
Acer%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    attention=shapiro_test(ACER_ATTENTION)
  )

ggqqplot(Acer, "ACER_ATTENTION", facet.by = "Dx")

Acer%>%
  kruskal_test(ACER_ATTENTION~Dx)%>%
  add_significance()

dunn_test(Acer, ACER_ATTENTION~Dx, p.adjust.method = "holm", detailed = TRUE)


#Memory
Acer%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    memory=shapiro_test(ACER_MEMORY)
  )

ggqqplot(Acer, "ACER_MEMORY", facet.by = "Dx")

Acer%>%
  kruskal_test(ACER_MEMORY~Dx)%>%
  add_significance()

dunn_test(Acer, ACER_MEMORY~Dx, p.adjust.method = "holm", detailed = TRUE)


#Fluency
Acer%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    fluency=shapiro_test(ACER_FLUENCY)
  )

ggqqplot(Acer, "ACER_FLUENCY", facet.by = "Dx")

Acer%>%
  kruskal_test(ACER_FLUENCY~Dx)%>%
  add_significance()

dunn_test(Acer, ACER_FLUENCY~Dx, p.adjust.method = "holm", detailed = TRUE)

#Language
Acer%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    language=shapiro_test(ACER_LANGUAGE)
  )

ggqqplot(Acer, "ACER_LANGUAGE", facet.by = "Dx")

Acer%>%
  kruskal_test(ACER_LANGUAGE~Dx)%>%
  add_significance()

dunn_test(Acer, ACER_LANGUAGE~Dx, p.adjust.method = "holm", detailed = TRUE)



#Visuospatial

Acer%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    vsp=shapiro_test(ACER_VISUOSPATIAL)
  )

ggqqplot(Acer, "ACER_VISUOSPATIAL", facet.by = "Dx")

Acer%>%
  kruskal_test(ACER_VISUOSPATIAL~Dx)%>%
  add_significance()

dunn_test(Acer, ACER_VISUOSPATIAL~Dx, p.adjust.method = "holm", detailed = TRUE)



#### Cambridge Naming ####
CamNaming<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Cambridge Naming") 

#convert to tibble
CamNaming<-as_tibble(CamNaming)

#Convert Dx from character vector to factor
CamNaming$Dx<-factor(CamNaming$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

#Descriptive statistics
CamNaming.stats<-CamNaming%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    CamNamingmean=mean(CamNaming,na.rm=TRUE),
    CamNamingsd=sd(CamNaming,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
CamNaming.stats

CamNaming%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    naming=shapiro_test(CamNaming)
  )

ggqqplot(CamNaming, "CamNaming", facet.by = "Dx")

CamNaming%>%
  kruskal_test(CamNaming~Dx)%>%
  add_significance()

dunn_test(CamNaming, CamNaming~Dx, p.adjust.method = "holm", detailed = TRUE)



#### Boston Naming ####
BosNaming<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Boston Naming") 

#convert to tibble
BosNaming<-as_tibble(BosNaming)

#Convert Dx from character vector to factor
BosNaming$Dx<-factor(BosNaming$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

BosNaming.stats<-BosNaming%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    BosNamingmean=mean(BosNaming,na.rm=TRUE),
    BosNamingsd=sd(BosNaming,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
BosNaming.stats

BosNaming%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    BosNaming=shapiro_test(BosNaming)
  )

ggqqplot(BosNaming,"BosNaming", facet.by = "Dx")

BosNaming%>%
  kruskal_test(BosNaming~Dx)%>%
  add_significance()

dunn_test(BosNaming, BosNaming~Dx, p.adjust.method = "holm", detailed = TRUE)


#### Camel and Cactus ####
CCT<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="CCT") 

#convert to tibble
CCT<-as_tibble(CCT)

#Convert Dx from character vector to factor
CCT$Dx<-factor(CCT$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

CCT.stats<-CCT%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    CCTmean=mean(CCT_total,na.rm=TRUE),
    CCTsd=sd(CCT_total,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
CCT.stats

CCT%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    CCTshap=shapiro_test(CCT_total)
  )

ggqqplot(CCT,"CCT_total", facet.by = "Dx")

CCT%>%
  kruskal_test(CCT_total~Dx)%>%
  add_significance()


dunn_test(CCT, CCT_total~Dx, p.adjust.method = "holm", detailed = TRUE)

#### end ####

#### brixton ####
brixton<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Brixton") 

#convert to tibble
brixton<-as_tibble(brixton)

#Convert Dx from character vector to factor
brixton$Dx<-factor(brixton$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

brixton.stats<-brixton%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    brixtonmean=mean(Brixton_scaledscore,na.rm=TRUE),
    brixtonsd=sd(Brixton_scaledscore,na.rm=TRUE),
    brixtonerrorsmean=mean(Brixton_errors,na.rm=TRUE),
    brixtonerrorssd=sd(Brixton_errors,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
brixton.stats

brixton%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    brixtonshap=shapiro_test(Brixton_scaledscore),
    brixtonerrorsshap=shapiro_test(Brixton_errors)
  )

ggqqplot(brixton,"Brixton_scaledscore", facet.by = "Dx")

brixton%>%
  kruskal_test(Brixton_scaledscore~Dx)%>%
  add_significance()

dunn_test(brixton, Brixton_scaledscore~Dx, p.adjust.method = "holm", detailed = TRUE)


#### end ####

#### synonym ####
Synonym<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Synonym") 

#convert to tibble
Synonym<-as_tibble(Synonym)

#Convert Dx from character vector to factor
Synonym$Dx<-factor(Synonym$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

syn.stats<-Synonym%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    synmean=mean(Synonym_total,na.rm=TRUE),
    synsd=sd(Synonym_total,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
syn.stats

Synonym%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    synshap=shapiro_test(Synonym_total)
  )

ggqqplot(Synonym,"Synonym_total", facet.by = "Dx")

Synonym%>%
  kruskal_test(Synonym_total~Dx)%>%
  add_significance()

dunn_test(Synonym, Synonym_total~Dx, p.adjust.method = "holm", detailed = TRUE)


#### end ####

#### ravens ####
raven<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Ravens") 

#convert to tibble
raven<-as_tibble(raven)

#Convert Dx from character vector to factor
raven$Dx<-factor(raven$Dx,levels=c("Control","Left","Right","bvFTD","SD"))


raven.stats<-raven%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    ravenmean=mean(Ravens,na.rm=TRUE),
    ravensd=sd(Ravens,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
raven.stats

raven%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    ravenshap=shapiro_test(Ravens)
  )

ggqqplot(raven,"Ravens", facet.by = "Dx")

raven%>%
  kruskal_test(Ravens~Dx)%>%
  add_significance()


dunn_test(raven, Ravens~Dx, p.adjust.method = "holm", detailed = TRUE)


####end####

# WPM ---------------------------------------------------------------------

wpm<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="WPM") 


#Convert Dx from character vector to factor
wpm$Dx<-factor(wpm$Dx,levels=c("Control","Left TLE","Right TLE","bvFTD","SD"))

wpm.stats <- wpm%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    wpmmean=mean(wpm_reduced,na.rm=TRUE),
    wpmsd=sd(wpm_reduced,na.rm = TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
wpm.stats


wpm%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    wpmshap=shapiro_test(wpm_reduced)
  )


ggqqplot(wpm,"wpm_reduced", facet.by = "Dx")

wpm%>%
  kruskal_test(wpmtotal~Dx)%>%
  add_significance()

dunn_test(wpm, wpmtotal~Dx, p.adjust.method = "holm", detailed = TRUE)

