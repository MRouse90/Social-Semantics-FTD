#make factor loadings figure for core FTD paper 
library(readxl)
library(reshape2)
library(ggplot2)

factorloadings<-read_excel("U:\\PhD\\3. Core FTD\\Results\\PCA\\PCA factor loadings.xlsx",sheet="factor_loadings")

factorloadings_melt<-melt(factorloadings, id='Task')

factorloadings_melt$Component<-gl(3, 24, labels=c('PC1:FTD severity', 'PC2:Semantic memory', 'PC3:Executive function'))

factorloadings_melt$Component <- as.factor(factorloadings_melt$Component)

# plot on ggplot

figure3a<-ggplot(factorloadings_melt, aes(x = value, y = Task)) +
  facet_wrap(~Component, nrow=1) +
  scale_y_discrete(limits = unique(rev(factorloadings$Task))) +
  geom_bar(stat = 'identity', position = 'identity') +
  geom_vline(xintercept = .5, colour='red', linetype='dashed') +
  geom_vline(xintercept = -.5, colour='red', linetype='dashed') +
  theme_bw() +
  theme(strip.background = element_rect(colour = "black"), strip.text = element_text(size=14)) +
  xlab('Factor Loadings') + 
  ylab('Task') +
  theme(text = element_text(size=14))

figure3a+theme(plot.background = element_rect(colour="black",fill = NA,linewidth =1.5))

