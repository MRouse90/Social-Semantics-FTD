#load packages
library(readxl)
library(dplyr)
library(ggpubr)
library(rstatix)


#read excel file 
pca<-read_excel("U:\\PhD\\3. Core FTD\\Results\\PCA\\FTD PCA factor scores.xlsx")

roiindices<-read_excel("U:\\PhD\\3. Core FTD\\Results\\ROI\\ROI z-scores.xlsx")

roiindices<-roiindices %>%
  select(whole_brain, neuropsych_id)

roiindices <- roiindices %>%
  rename(ID=neuropsych_id)



#merge datasets
pca<-merge(pca,roiindices,by="ID")


#correlate whole brain with FTD severity


fig1<-ggscatter(pca,x="pc1",y="whole_brain",
                shape = 21, size = 8,
                alpha=0.85,
                xlim=c(-2.5,1.6),
                ylim=c(-0.09,0.01),
                fill = "Dx",
                palette = "simpsons",
                xlab="PC1: FTD severity",
                ylab = "Total grey matter volume residual value",
                legend="none",
                add="reg.line"
                )

fig1<-fig1+scale_size(range=c(3, 22))+ 
  font("xlab",size=22)+
  font("ylab",size=22)+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))


fig1

#correlate whole brain with semantics


fig2<-ggscatter(pca,x="pc2",y="whole_brain",
                shape = 21, size = 8,
                alpha=0.85,
                xlim=c(-1.7,2),
                ylim=c(-0.09,0.01),
                fill = "Dx",
                palette = "simpsons",
                xlab="PC2: Semantic memory",
                ylab = "Total grey matter volume residual value",
                legend="none",
                add="reg.line"
)

fig2<-fig2+scale_size(range=c(3, 22))+ 
  font("xlab",size=22)+
  font("ylab",size=22)+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))

fig2

#correlate whole brain with executive functioning


fig3<-ggscatter(pca,x="pc3",y="whole_brain",
                shape = 21, size = 8,
                alpha=0.85,
                xlim=c(-1.4,2.6),
                ylim=c(-0.09,0.01),
                fill = "Dx",
                palette = "simpsons",
                xlab="PC3: Executive function",
                ylab = "Total grey matter volume residual value",
                legend="none",
                add="reg.line"
                
)

fig3<-fig3+scale_size(range=c(3, 22))+ 
  font("xlab",size=22)+
  font("ylab",size=22)+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))

fig3

pca <-pca %>%
  rename(Group=Dx)



# Combine into one panel figure -------------------------------------------

#Once you have your figures, it's really handy to use ggarrange to combine all of them into. 
#See example below:
figX <- ggarrange(fig1, fig2,fig3,
                  font.label = list(size = 12),
                  ncol=3, nrow=1,
                  legend = "none"
)
figX



#Calculate correlations

#check for normality

pca%>%
  summarise(
    count=n(),
    pc1shap=shapiro_test(pc1),
    pc2shap=shapiro_test(pc2),
  pc3shap=shapiro_test(pc3)
  )


pca %>% 
cor_test(pc2,whole_brain,method="pearson")
  


