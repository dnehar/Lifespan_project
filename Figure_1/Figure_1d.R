library(dplyr)
library(ggplot2)
  
# load metadata  
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()

# colors 
col_age_gp <- c('HI'='#99d8c9',
                'HC'='#9ecae1',
                'HY'='#fec44f',
                'HO'='#bcbddc')

# PCA
proportions <- LifeSpan_ALL_MetaData %>%

  group_by(Names, Final_annotations) %>%
  summarise(n = n(), Groups = first(Groups), Sex = first(Sex), Ethnicity = first(Ethnicity_v2), Age_months = first(Age_months), Lineage = first(Lineage_v2)) %>%
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame()


t.proportions <- proportions %>%
  reshape2::dcast(Names + Groups + Sex + Ethnicity + Age_months ~ Final_annotations, value.var = "freq", fill = 0)

pca <- prcomp(t.proportions[,6:ncol(t.proportions)], center = T)

d  <- round(pca$sdev^2/sum(pca$sdev^2)*100, digits=1)
xl <- sprintf("PC 1: %.1f %%", d[1])
yl <- sprintf("PC 2: %.1f %%", d[2])
zl <- sprintf("PC 3: %.1f %%", d[3])


t.proportions <- cbind(t.proportions, data.frame("PC1" = as.numeric(pca$x[,1]), "PC2"= as.numeric(pca$x[,2]), "PC3"= as.numeric(pca$x[,3])))
t.proportions$Groups <- factor( t.proportions$Groups, levels = c("HI","HC","HY","HO"))
head(t.proportions)

plt_pca <- t.proportions %>% 
  ggplot(aes(x = PC1, y = PC2, group = Groups, color = Groups)) +
  geom_point(aes(color = Groups, shape = Sex), size = 5) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values=col_age_gp) + 
  scale_color_manual(values=col_age_gp) + 
  theme(axis.text.y=element_text(size=18), 
        axis.text.x=element_text(size=18),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20)) +
  ggtitle("PBMCs")  +

  
  stat_ellipse(type = "norm", linetype = 1, level = 0.8)+ 
  labs(x=xl,y=yl) +
  ggtitle("PBMCs ")  +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size=20, face="bold"))
print(plt_pca)

