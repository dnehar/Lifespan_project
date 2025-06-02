library(dplyr)
library(ggplot2)

# load metadata
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

  MetaData <- LifeSpan_ALL_MetaData
  Number <- data.frame(MetaData %>% count(Names, Groups)) #Gender,
  Number %>% group_by(Groups) %>% summarize(Mean= mean(n), SD=sd(n)) -> LNB
  
  #head(LNB)
  px <- ggplot(data=Number, aes(x=Groups, y=n, fill=Groups)) +
    geom_violin(aes(fill = Groups), trim = TRUE)  +
    stat_summary(fun.data = "mean_sdl") +
    geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1) + #,binwidth = 20
    #+ coord_flip() 
    scale_fill_manual(values=col_age_gp) +
    ylab("number of cells") +
    xlab("age groups") +
    scale_x_discrete(limits = c("HI","HC","HY","HO"))+
    theme(legend.position="none", 
          axis.text.x=element_text(size=18), 
          axis.text.y=element_text(size=18), 
          axis.title.x = element_text(face="bold", size=18),
          axis.title.y = element_text(face="bold", size=18),
          plot.title = element_text(hjust = 0.5,face='bold',size=14),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black', size=1))
  print(px)
