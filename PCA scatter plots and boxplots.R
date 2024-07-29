#load packages
library(readxl)
library(dplyr)
library(ggpubr)
library(rstatix)


#read excel file 
pca<-read_excel("U:\\PhD\\3. Core FTD\\Results\\PCA\\FTD PCA factor scores.xlsx")

#Convert Group and Dx from character vector to factor
pca$Dx<-factor(pca$Dx,levels=c("bvFTD","SD","Left TLE","Right TLE"))
# pca$subgroup<-factor(pca$subgroup,levels = c("bvFTD","left SD","right SD"))
# pca$ATLOFC<-factor(pca$ATLOFC,levels = c("frontal","temporal"))

#remove unneeded variables
# pca <-pca %>%
#   select(-(Dx2:FRS))

#pca<-pca %>%
#  drop_na(subgroup)


#Scatter plot comparing PC1 versus PC2 with marginal histogram

#first rename Dx for figure
pca <-pca %>%
  rename(group=Group) %>%
  rename(Group=Dx)

#figure 3b

fig3a<-ggscatterhist(
  pca, x = "pc1", y = "pc2",
  color = "Group", size = 8, alpha = 0.9,
  palette = c("#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
  xlab="PC1: FTD severity",
  ylab = "PC2: Semantic memory",
  legend = "none",
  margin.params = list(fill = "Group", color = "black", size = 0.2)
)

fig3a$sp <- fig3a$sp +
  geom_hline(yintercept = 1.46, linetype = "dashed", color = "black",linewidth=1.10) +
  geom_vline(xintercept = 0.32, linetype = "dashed", color = "black",linewidth=1.10)+theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+font("xlab",size=18)+font("ylab",size=18)

fig3a

#Scatter plot comparing PC1 versus PC3 with marginal histogram

fig3b<-ggscatterhist(
  pca, x = "pc2", y = "pc3",
  color = "Group", size = 8, alpha = 0.9,
  palette = c("#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
  xlab="PC2: Semantic memory",
  ylab = "PC3: Executive function",
  ellipse="FALSE",
  legend = "none",
  margin.params = list(fill = "Group", color = "black", size = 0.2)
)

fig3b$sp <- fig3b$sp +
  geom_hline(yintercept = 0.34, linetype = "dashed", color = "black", linewidth=1.10) +
  geom_vline(xintercept = 1.46, linetype = "dashed", color = "black",linewidth=1.10)+
  theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+font("xlab",size=18)+font("ylab",size=18)
fig3b



#Scatter plot comparing PC1 versus PC3 with marginal histogram

fig3c<-ggscatterhist(
  pca, x = "pc3", y = "pc1",
  color = "Group", size = 8, alpha = 0.9,
  palette = c("#FED439FF","#709AE1FF","#D2AF81FF","#FD7446FF"),
  xlab="PC3: Executive function",
  ylab = "PC1: FTD severity",
  legend = "none",
  ellipse="FALSE",
  margin.params = list(fill = "Group", color = "black", size = 0.2,xlim=c(-4,4))
)

fig3c$sp <- fig3c$sp +
  geom_hline(yintercept = 0.34, linetype = "dashed", color = "black",linewidth=1.10) +
  geom_vline(xintercept = 0.32, linetype = "dashed", color = "black",linewidth=1.10)+theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))+font("xlab",size=18)+font("ylab",size=18)

fig3c




#Once you have your figures, it's really handy to use ggarrange to combine all of them into. 
#See example below:
fig3 <- ggarrange(fig3a, fig3b,fig3c,
                  font.label = list(size = 12),
                  ncol=2, nrow=2,
                  legend = "top")
fig3
