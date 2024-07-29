#This script is for creating the ROI scatter plots 
#displaying asymmetry and magnitude indices for the patients#

#Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)


# Imaging Scatter Plots ---------------------------------------------------

#read excel file 
roiindices<-read_excel("U:\\PhD\\3. Core FTD\\Results\\ROI\\ROI z-scores_coreFTD.xlsx")

# roiindices <-roiindices %>%
#   filter(Dx!="test")

#Convert Group and Dx from character vector to factor
roiindices$Dx<-factor(roiindices$Dx,levels=c("bvFTD","SD","Left TLE","Right TLE"))

#change name of Dx to Group for figures
roiindices <-roiindices %>%
  rename(Group=Dx)


# Residuals scatter plots -------------------------------------------------


#ATL mag versus ATL sym z-scores RESIDUALS

fig2a<-ggscatter(roiindices,x="ATL_sym_resid",y="ATL_mag_resid",
                 shape = 21, size = 6,
                 xlim=c(-0.45,0.45),
                 ylim=c(-0.9,0.1),
                 fill = "Group",
                 palette = c("#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF","pink"),
                 xlab="ATL Asymmetry",
                 ylab = "ATL Magnitude",
                 legend="none",
                 #label="neuropsych_id",
                 title="A", 
                 
)


fig2a <- fig2a + 
  c(geom_vline(xintercept = 0,linetype="dashed",color="black",size=1))+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=18)+
  font("ylab",size=18)+
  font("title",size=22)+
  geom_curve(x = 0, 
             y = 0.03,
             xend = 0.43, 
             yend = -0.40,colour="black",size=1.5,linetype="dashed",curvature = 0)+
  geom_curve(x = 0.43,y = -0.40,xend = -0.01,yend = -0.84,colour="black",size=1.5,linetype="dashed",curvature = 0)+
  geom_curve(x = -0.01, y = -0.84, xend = -0.44, yend = -0.41,colour="black",size=1.5,linetype="dashed",curvature = 0.05)+
  geom_curve(x = -0.44, y = -0.41, xend = 0.00, yend = 0.03,colour="black",size=1.5,linetype="dashed",curvature = 0)+
  geom_vline(xintercept = 0,linetype="dashed",color="black",size=1)

fig2a
  


#OFC mag versus OFC sym z-scores RESIDUALS


fig2b<-ggscatter(roiindices,x="OFC_sym_resid",y="OFC_mag_resid",
                 shape = 21, size = 6,
                 xlim=c(-0.45,0.45),
                 ylim=c(-0.9,0.1),
                 fill = "Group",
                 palette = c("#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF","pink"),
                 xlab="OFC Asymmetry",
                 ylab = "OFC Magnitude",
                 legend="none",
                 # label="neuropsych_id",
                 title="B"
)

fig2b

fig2b <- fig2b +
  c(geom_vline(xintercept = 0,linetype="dashed",color="black",size=1))+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=18)+
  font("ylab",size=18)+
  font("title",size=22)+
  geom_curve(x = -0.01, y = 0.03,
             xend = 0.42, 
             yend = -0.38,colour="black",size=1.5,linetype="dashed",curvature = 0)+
  geom_curve(x = 0.42, 
             y = -0.38, 
             xend = 0.01, 
             yend = -0.81,colour="black",size=1.5,linetype="dashed",curvature = 0)+
  geom_curve(x = 0.01,
             y = -0.81,
             xend = -0.42,
             yend = -0.38,colour="black",size=1.5,linetype="dashed",curvature = 0.05)+
  geom_curve(x = -0.42, 
             y = -0.38,
             xend = -0.01, 
             yend = 0.03,colour="black",size=1.5,linetype="dashed",curvature = 0)+
  geom_vline(xintercept = 0,linetype="dashed",color="black",size=1)

fig2b


#ATL asymetry v OFC asymetry residuals

fig2c<-ggscatter(roiindices,x="ATL_sym_resid",y="OFC_sym_resid",
                 shape = 21, size = 6,
                 xlim=c(-0.45,0.45),
                 ylim=c(-0.45,0.45),
                 fill = "Group",
                 palette = c("#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 xlab="ATL Asymmetry",
                 ylab = "OFC Asymmetry",
                 legend="none",
                 # label="neuropsych_id",
                 title="C"
)
fig2c


fig2c <- fig2c + 
  c(geom_vline(xintercept = 0,linetype="dashed",color="black",size=1))+
  geom_hline(yintercept = 0,linetype="dashed",color="black",size=1)+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=18)+
  font("ylab",size=18)+
         font("title",size=22)
)


fig2c




#OFC mag v ATL mag residuals

fig2d<-ggscatter(roiindices,x="OFC_mag_resid",y="ATL_mag_resid",
                 shape = 21, size = 6,
                 xlim=c(-0.80,0.10),
                 ylim=c(-0.85,0.1),
                 fill = "Group",
                 palette = c("#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
                 xlab="OFC Magnitude",
                 ylab = "ATL Magnitude",
                 legend="none",
                 title="D")

fig2d <- fig2d +
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+
  font("xlab",size=18)+
  font("ylab",size=18)+
  font("title", size=22)+
  geom_curve(x = 0.03, 
             y = 0.03,
             xend = 0.03, 
             yend = -0.84,colour="black",size=1.5,linetype="dashed",curvature = 0)+geom_curve(x = 0.03, 
                                                                                              y = -0.84,
                                                                                              xend = -0.81, 
                                                                                              yend = -0.84,colour="black",size=1.5,linetype="dashed",curvature = 0)+geom_curve(x = -0.81, 
                                                                                                                                                                               y = -0.84,
                                                                                                                                                                               xend = -0.81, 
                                                                                                                                                                               yend = 0.03,colour="black",size=1.5,linetype="dashed",curvature = 0
                                                                                              )+geom_curve(x = -0.81, y = 0.03, xend = 0.03, 
                                                                                                           yend = 0.03,colour="black",size=1.5,linetype="dashed",curvature = 0)
fig2d



# Combine into one panel figure -------------------------------------------

#Once you have your figures, it's really handy to use ggarrange to combine all of them into. 
#See example below:
fig2 <- ggarrange(fig2a, fig2b,fig2c,fig2d,
                  font.label = list(size = 12),
                  ncol=2, nrow=2,
                  legend = "top")
fig2 +
  ggeasy::easy_center_title()+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =2))
fig2  


