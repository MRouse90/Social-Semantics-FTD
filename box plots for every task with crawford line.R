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


#rename
Acer <- Acer %>%
  rename('Left TLE'='Left') %>%
  rename('Right TLE'='Right')


#Convert Dx from character vector to factor
Acer$Dx<-factor(Acer$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))



#ACE-R TOTAL

figX<-ggboxplot(Acer, "Dx", "ACER_TOTAL",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                  title="ACE-R Total",
                xlab="Group",
                ylab="ACE-R total score",
                  legend="top",
                  ylim=c(0,100))



figX+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=18)+
  font("ylab",size=18)+
  font("title",size=22)+
  labs(tag="A")+
  geom_hline(yintercept = 93,linetype="dashed",color="dark green",size=1)

#MMSE


figY<-ggboxplot(Acer, "Dx", "MMSE",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="MMSE",
                xlab="Group",
                ylab="MMSE score",
                legend="top",
                ylim=c(0,30))



figY+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=18)+
  font("ylab",size=18)+
  font("title",size=22)+
  labs(tag="B")+
  geom_hline(yintercept = 29,linetype="dashed",color="dark green",size=1)



#ACE-R Attention


  fig1<-ggboxplot(Acer, "Dx", "ACER_ATTENTION",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="ACE-R Attention",
                xlab="Group",
                ylab="ACE-R Attention score",
                legend="top",
                ylim=c(0,18))




  fig1<-fig1+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="A")+
  geom_hline(yintercept = 18,linetype="dashed",color="dark green",size=1)+
    theme(axis.text.x=element_blank())



#ACE-R Memory


fig2<-ggboxplot(Acer, "Dx", "ACER_MEMORY",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="ACE-R Memory",
                xlab="Group",
                ylab="ACE-R Memory score",
                legend="top",
                ylim=c(0,26))



fig2 <-fig2+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="B")+
  geom_hline(yintercept = 21,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())




#ACE-R Fluency


fig3<-ggboxplot(Acer, "Dx", "ACER_FLUENCY",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="ACE-R Fluency",
                xlab="Group",
                ylab="ACE-R Fluency score",
                legend="top",
                ylim=c(0,14))



fig3<-fig3+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="C")+
  geom_hline(yintercept = 12,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



#ACE-R Language


fig4<-ggboxplot(Acer, "Dx", "ACER_LANGUAGE",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="ACE-R Language",
                xlab="Group",
                ylab="ACE-R Language score",
                legend="top",
                ylim=c(0,26))



fig4<-fig4+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="D")+
  geom_hline(yintercept = 25,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


#ACE-R VSP

fig5<-ggboxplot(Acer, "Dx", "ACER_VISUOSPATIAL",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="ACE-R Visuospatial",
                xlab="Group",
                ylab="ACE-R Visuospatial score",
                legend="top",
                ylim=c(0,16))



fig5<-fig5+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="E")+
  geom_hline(yintercept = 15,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())




#### Cambridge Naming ####
CamNaming<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Cambridge Naming") 


#Convert Dx from character vector to factor
CamNaming$Dx<-factor(CamNaming$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))
                                           

#Cambridge Naming

fig6<-ggboxplot(CamNaming, "Dx", "CamNaming",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="Cambridge Naming",
                xlab="Group",
                ylab="Cambridge Naming score",
                legend="top",
                ylim=c(0,32))



fig6<-fig6+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="F")+
  geom_hline(yintercept = 32,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



#### Boston Naming ####
BosNaming<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Boston Naming") 


#Convert Dx from character vector to factor
BosNaming$Dx<-factor(BosNaming$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


#Boston Naming

fig7<-ggboxplot(BosNaming, "Dx", "BosNaming",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="Boston Naming",
                xlab="Group",
                ylab="Boston Naming score",
                legend="top",
                ylim=c(0,30))



fig7<-fig7+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="G")+
  geom_hline(yintercept = 29,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


#### Camel and Cactus ####
CCT<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="CCT") 


#Convert Dx from character vector to factor
CCT$Dx<-factor(CCT$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))

fig8<-ggboxplot(CCT, "Dx", "CCT_total",  fill = "Dx",
                palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                add = c("boxplot"),
                title="Camel and Cactus",
                xlab="Group",
                ylab="Camel and Cactus score",
                legend="top",
                ylim=c(0,32))



fig8<-fig8+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="H")+
  geom_hline(yintercept = 29,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())
  
fig8


#### end ####



#### synonym ####
Synonym<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Synonym") 


#Convert Dx from character vector to factor
Synonym$Dx<-factor(Synonym$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))

fig9<-ggboxplot(Synonym, "Dx", "Synonym_total",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Synonym judgement test",
                 xlab="Group",
                 ylab="Synonym judgement score",
                 legend="top",
                 ylim=c(0,48))



fig9<-fig9+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="I")+
  geom_hline(yintercept = 48,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())

#### ravens ####
raven<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Ravens") 


#Convert Dx from character vector to factor
raven$Dx<-factor(raven$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))

fig10<-ggboxplot(raven, "Dx", "Ravens",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Raven's Progressive Matrices set B",
                 xlab="Group",
                 ylab="Raven's score",
                 legend="top",
                 ylim=c(0,12))



fig10<-fig10+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="J")+  
  geom_hline(yintercept = 8,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


#### brixton ####
brixton<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Brixton") 


#Convert Dx from character vector to factor
brixton$Dx<-factor(brixton$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))

fig11<-ggboxplot(brixton, "Dx", "Brixton_scaledscore",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Brixton Spatial Anticipation Test",
                 xlab="Group",
                 ylab="Brixton score",
                 legend="top",
                 ylim=c(0,10))



fig11<-fig11+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="K")+
  geom_hline(yintercept = 3,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


#### end ####


# Face to name matching ---------------------------------------------------
facename<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Face to name matching") 


#Convert Dx from character vector to factor
facename$Dx<-factor(facename$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


fig12<-ggboxplot(facename, "Dx", "TOTAL_facetoname",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Face-name matching",
                 xlab="Group",
                 ylab="Face-name score",
                 legend="top",
                 ylim=c(0,44))



fig12<-fig12+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="L")+
  geom_hline(yintercept = 33,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



# Face to profession ------------------------------------------------------
faceprof<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Face to profession") 


#Convert Dx from character vector to factor
faceprof$Dx<-factor(faceprof$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))

fig13<-ggboxplot(faceprof, "Dx", "total",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Face-profession matching",
                 xlab="Group",
                 ylab="Face-profession score",
                 legend="top",
                 ylim=c(0,44))



fig13<-fig13+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="M")+
  geom_hline(yintercept = 34,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



# Landmark to name  -------------------------------------------------------
landmark<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Landmark to name matching") 

#Convert Dx from character vector to factor
landmark$Dx<-factor(landmark$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


fig14<-ggboxplot(landmark, "Dx", "TOTAL_landmarks",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Landmark-name matching",
                 xlab="Group",
                 ylab="Landmark-name score",
                 legend="top",
                 ylim=c(0,42))



fig14<-fig14+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="N")+
  geom_hline(yintercept = 36,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())




# Face matching -----------------------------------------------------------

facematching<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Face Matching Upright") 


#Convert Dx from character vector to factor
facematching$Dx<-factor(facematching$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


#famous face matching

fig15<-ggboxplot(facematching, "Dx", "Famoustotal",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Famous face matching",
                 xlab="Group",
                 ylab="Famous face matching score",
                 legend="top",
                 ylim=c(0,22))



fig15<-fig15+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="O")+
  geom_hline(yintercept = 20,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


#unfamiliar face matching

fig16<-ggboxplot(facematching, "Dx", "Unfamiliartotal",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Unfamiliar face matching",
                 xlab="Group",
                 ylab="Unfamiliar face matching score",
                 legend="top",
                 ylim=c(0,22))



fig16<-fig16+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="P")+
  geom_hline(yintercept = 18,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


# Social concepts ---------------------------------------------------------

socialconcepts<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Social synonym") 

#Convert Dx from character vector to factor
socialconcepts$Dx<-factor(socialconcepts$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


#social concepts


fig17<-ggboxplot(socialconcepts, "Dx", "Social_socialsynonym",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Social abstract synonym judgement",
                 xlab="Group",
                 ylab="Social abstract synonym score",
                 legend="top",
                 ylim=c(0,36))



fig17<-fig17+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="Q")+
  geom_hline(yintercept = 32,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())

#non-social 


fig18<-ggboxplot(socialconcepts, "Dx", "NonSocial_socialsynonym",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Non-social abstract synonym judgement",
                 xlab="Group",
                 ylab="Non-social abstract synonym score",
                 legend="top",
                 ylim=c(0,36))



fig18<-fig18+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="R")+
  geom_hline(yintercept = 35,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())

# Social roles ------------------------------------------------------------
socialroles<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Social roles") 


#Convert Dx from character vector to factor
socialroles$Dx<-factor(socialroles$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


fig19<-ggboxplot(socialroles, "Dx", "TOTALsocroles",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Social WPM",
                 xlab="Group",
                 ylab="Social roles score",
                 legend="top",
                 ylim=c(0,35))



fig19<-fig19+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="S")+
  geom_hline(yintercept = 33,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


#non-social WPM

WPM<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="WPM") 


#Convert Dx from character vector to factor
WPM$Dx<-factor(WPM$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


fig20<-ggboxplot(WPM, "Dx", "wpmtotal",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Non-social WPM",
                 xlab="Group",
                 ylab="Non-social WPM score",
                 legend="top",
                 ylim=c(0,36))



fig20<-fig20+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="T")+
  geom_hline(yintercept = 36,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



# Basic emotion -----------------------------------------------------------

basicemotion<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Basic Emotion") 


#Convert Dx from character vector to factor
basicemotion$Dx<-factor(basicemotion$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))



fig21<-ggboxplot(basicemotion, "Dx", "TOTALbasic",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Basic emotion matching",
                 xlab="Group",
                 ylab="Basic emotion matching score",
                 legend="top",
                 ylim=c(0,19))



fig21<-fig21+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="U")+
  geom_hline(yintercept = 14,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



# Complex emotion ---------------------------------------------------------
complexemotion<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="Complex Emotion") 

#Convert Dx from character vector to factor
complexemotion$Dx<-factor(complexemotion$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


fig22<-ggboxplot(complexemotion, "Dx", "complexTOTAL",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Complex emotion matching",
                 xlab="Group",
                 ylab="Complex emotion matching score",
                 legend="top",
                 ylim=c(0,23))



fig22<-fig22+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="V")+
  geom_hline(yintercept = 16,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



# SNQ ---------------------------------------------------------------------
snq<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="SNQ") 


#Convert Dx from character vector to factor
snq$Dx<-factor(snq$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))


fig23<-ggboxplot(snq, "Dx", "SNQ_total",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="Social norms questionnaire",
                 xlab="Group",
                 ylab="Social norms questionnaire score",
                 legend="top",
                 ylim=c(0,22))



fig23<-fig23+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="W")+
  geom_hline(yintercept = 18,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())


# TASIT -------------------------------------------------------------------

tasit<-read_excel("U:\\PhD\\3. core FTD\\Results\\Social Concept Results.xlsx",sheet="TASIT") 


#Convert Dx from character vector to factor
tasit$Dx<-factor(tasit$Dx,levels=c("Control","bvFTD","SD","Left TLE","Right TLE"))

fig24<-ggboxplot(tasit, "Dx", "sarcasm",  fill = "Dx",
                 palette = c("grey", "#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 add = c("boxplot"),
                 title="TASIT-sarcasm",
                 xlab="Group",
                 ylab="TASIT-sarcasm score",
                 legend="top",
                 ylim=c(0,20))



fig24<-fig24+ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=10)+
  font("ylab",size=10)+
  font("title",size=12)+
  labs(tag="X")+
  geom_hline(yintercept = 16,linetype="dashed",color="dark green",size=1)+
  theme(axis.text.x=element_blank())



#ggarragange

figZ<-ggarrange(fig1,fig2,fig3,fig4, fig5, fig6, fig7,fig8,fig9,fig10,fig11,fig12,fig13,fig14,fig15,fig16,fig17,
          fig18,fig19,fig20,fig21,fig22,fig23,fig24,ncol=6,nrow=4,common.legend = TRUE)
figZ
