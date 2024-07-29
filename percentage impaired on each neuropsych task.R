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


#Attention
Acer%>%
  group_by(Dx)%>%
  count(ACER_ATTENTION<18,na.rm=TRUE)
  
   
#Memory
Acer%>%
  group_by(Dx)%>%
  count(ACER_MEMORY<21,na.rm=TRUE)



#Fluency
Acer%>%
  group_by(Dx)%>%
  count(ACER_FLUENCY<12,na.rm=TRUE)

#Language
Acer%>%
  group_by(Dx)%>%
  count(ACER_LANGUAGE<25,na.rm=TRUE)



#Visuospatial
Acer%>%
  group_by(Dx)%>%
  count(ACER_VISUOSPATIAL<15,na.rm=TRUE)



#### Cambridge Naming ####
CamNaming<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Cambridge Naming") 

#Convert Dx from character vector to factor
CamNaming$Dx<-factor(CamNaming$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

CamNaming %>%
  group_by(Dx) %>%
  count(CamNaming<32,na.rm=TRUE)



#### Boston Naming ####
BosNaming<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Boston Naming") 


#Convert Dx from character vector to factor
BosNaming$Dx<-factor(BosNaming$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

BosNaming %>%
  group_by(Dx) %>%
  count(BosNaming<29,na.rm=TRUE)



#### Camel and Cactus ####
CCT<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="CCT") 

#Convert Dx from character vector to factor
CCT$Dx<-factor(CCT$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

CCT %>%
  group_by(Dx) %>%
  count(CCT_total<29,na.rm=TRUE)



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

#convert to tibble
wpm<-as_tibble(wpm)

#Convert Dx from character vector to factor
wpm$Dx<-factor(wpm$Dx,levels=c("Control","Left","Right","bvFTD","SD"))

wpm.stats<-wpm%>%
  group_by(Dx)%>%
  summarise(
    N=n(),
    wpmmean=mean(wpmtotal,na.rm=TRUE),
    wpmsd=sd(wpmtotal,na.rm=TRUE)
  )%>%
  mutate(across(where(is.numeric),~num(.,digits=1)))
wpm.stats

wpm%>%
  group_by(Dx)%>%
  summarise(
    count=n(),
    wpmshap=shapiro_test(wpmtotal)
  )


ggqqplot(wpm,"wpmtotal", facet.by = "Dx")

wpm%>%
  kruskal_test(wpmtotal~Dx)%>%
  add_significance()

dunn_test(wpm, wpmtotal~Dx, p.adjust.method = "holm", detailed = TRUE)

