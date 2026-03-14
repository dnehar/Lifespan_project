
library(dplyr); library(ggplot2); 


cols <- c(
  # Groups
  "Infants" = "#0072B2",
  "Child" = "#56B4E9",
  "Adolescent" = "#009E73", 
  "Young" = "#F0E442",
  "Middle_aged" = "#E69F00",
  "Older" ="#D55E00",
  "Oldest_old" = "#CC79A7"
)

# load metadata  
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()


proportions <- LifeSpan_ALL_MetaData %>%
  mutate(ReCluster = factor(LS_L2)) %>%
  group_by(sample_id, ReCluster) %>%
  summarise(n = n(), Groups = first(Age_groups), Age_months = first(Age_in_yrs),  Lineage = first(lifespan_L1)) %>%
  mutate(freq = n / sum(n) *100) %>%
  ungroup() %>%
  as.data.frame()
t.proportions <- proportions %>%
  reshape2::dcast(sample_id + Groups + Age_months ~ ReCluster, value.var = "freq", fill = 0)
head(t.proportions)

pca <- prcomp(t.proportions[,4:ncol(t.proportions)], center = T) #***

d  <- round(pca$sdev^2/sum(pca$sdev^2)*100, digits=1)
xl <- sprintf("PC 1: %.1f %%", d[1])
yl <- sprintf("PC 2: %.1f %%", d[2])
zl <- sprintf("PC 3: %.1f %%", d[3])


# ├├ plot PC1 - PC2 ####
t.proportions <- cbind(t.proportions, data.frame("PC1" = as.numeric(pca$x[,1]), "PC2"= as.numeric(pca$x[,2]), "PC3"= as.numeric(pca$x[,3])))
t.proportions$Groups <- factor(t.proportions$Groups, levels = age_groups)
head(t.proportions)

plt_pca_L2 <- t.proportions %>% 
  ggplot(aes(x = PC1, y = PC2, group = Groups, color = Groups)) +
  geom_point(aes(color = Groups), size = 3) + #, shape = Sex
  theme_bw() +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values=cols) + 
  scale_color_manual(values=cols) + 
  theme(axis.text.y=element_text(size=18), 
        axis.text.x=element_text(size=18),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20)) +
  
  stat_ellipse(type = "norm", linetype = 1, level = 0.4)+ 
  labs(x=xl,y=yl) +
  ggtitle("PBMCs - Level 2 (n=18) ")  +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size=20, face="bold"))
plt_pca_L2


ggsave("./Figure_2026/LS_pca_plot_L2_012562026.pdf", plt_pca_L2,
       width=3, height=3,  units="in", scale=3)
