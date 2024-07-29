#This script is for creating the ROI scatter plots 
#displaying asymmetry and magnitude indices for the patients for the Carer Q study#

#Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)


# Imaging Scatter Plots ---------------------------------------------------

#read excel file 
roiindices<-read_excel("U:\\PhD\\3. Core FTD\\Results\\ROI\\ROI z-scores.xlsx")

#remove non-FTD

 roiindices <-roiindices %>%
   filter(Dx!="test") %>%
   filter(Dx!="Left TLE") %>%
   filter(Dx!="Right TLE") 


#Convert Group and Dx from character vector to factor
roiindices$Dx<-factor(roiindices$Dx,levels=c("bvFTD","SD"))

#change name of Dx to Group for figures
roiindices <-roiindices %>%
  rename(Group=Dx)


#open carer PCA factor scores

neuropsych<-read_excel("U:\\PhD\\3. Core FTD\\Results\\PCA\\FTD PCA factor scores.xlsx",sheet=1)

#remove non-FTD

neuropsych <- neuropsych %>%
  filter(Dx!="Control") %>%
  filter(Dx!="Left") %>%
  filter(Dx!="Right") 

#Convert Group and Dx from character vector to factor
neuropsych$Dx<-factor(neuropsych$Dx,levels=c("bvFTD","SD"))

#change name of Dx to Group for figures
neuropsych <- neuropsych %>%
  rename(blah=Group)%>%
  rename(Group=Dx)

neuropsych <- neuropsych %>%
  rename(neuropsych_id=ID)


#merge datasets
combo<-merge(roiindices,neuropsych,by="neuropsych_id")

remove(neuropsych)
remove(roiindices)

combo <- combo %>%
  rename(Group=Group.x)

# Correlations between ROI & PC factor scores---------------------------------


fig1<-ggscatter(combo,x="par_ope_mag_resid",y="pc3",
                shape = 21, size = 12,
                alpha=0.85,
                xlim=c(-0.50,0.15),
                ylim=c(-2.5,2.5),
                fill = "Group",
                palette = "simpsons",
                xlab="ATL magnitude",
                ylab = "FTD severity",
                legend="none",
                add="reg.line",
                cor.coef = TRUE
                
)

fig1+scale_size(range=c(3, 22))+ 
  font("xlab",size=18)+
  font("ylab",size=18)






fig2<-ggscatter(combo,x="acc_mag_resid",y="pc1",
                shape = 21, size = "whole_brain",
                alpha=0.85,
                xlim=c(-0.35,0.06),
                ylim=c(-2.5,2.5),
                fill = "Group",
                palette = "simpsons",
                xlab="ACC magnitude",
                ylab = "Apathy factor score",
                legend="none",
                add="reg.line"
                
)

fig2+scale_size(range=c(3, 22))+ 
  font("xlab",size=18)+
  font("ylab",size=18)





fig3<-ggscatter(combo,x="insula_mag_resid",y="pc2",
                shape = 21, size = "whole_brain",
                alpha=0.85,
                xlim=c(-0.40,0.04),
                ylim=c(-2,3),
                fill = "Group",
                palette = "simpsons",
                xlab="Insula magnitude",
                ylab = "Challenging behaviours factor score",
                legend="none",
                add="reg.line"
                
)

fig3+scale_size(range=c(3, 22))+ 
  font("xlab",size=18)+
  font("ylab",size=18)


fig4<-ggscatter(combo,x="mfg_mag_resid",y="pc3",
                shape = 21, size = "whole_brain",
                alpha=0.85,
                xlim=c(-0.30,0.12),
                ylim=c(-2.5,2.5),
                fill = "Group",
                palette = "simpsons",
                xlab="MFG magnitude",
                ylab = "ADLS factor score",
                legend="none",
                add="reg.line"
                
)

fig4+scale_size(range=c(3, 22))+ 
  font("xlab",size=18)+
  font("ylab",size=18)




