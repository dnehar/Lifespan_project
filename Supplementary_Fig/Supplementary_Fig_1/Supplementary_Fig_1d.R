# =============================================================================
# Supplementary Fig. 1d  — violin plots showing number of cells accross age groups
#
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Piechart_Platform_ageGroups_03182026.pdf
# =============================================================================

library(dplyr); library(ggplot2)

# --- Load metadata (pbmcs_v1.rds available at dnehar/Lifespan_project/pbmcs_v1.rds) ---
# MetaData is a list containing:
#   $meta_small : per-cell metadata (cell type annotations, sample IDs, age groups, etc.)
#   $pheno      : per-sample metadata (sample_id, age, sex, etc.)
MetaData <- readRDS('./pbmcs_v1.rds')
LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% as.data.frame()
pheno <- MetaData[['pheno']] %>% as.data.frame()

age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')

# color palette ---
cols <- c("Infants" = "#0072B2",
                 "Child" = "#56B4E9",
                 "Adolescent" = "#009E73", 
                 "Young" = "#F0E442",
                 "Middle_aged" = "#E69F00",
                 "Older" ="#D55E00",
                 "Oldest_old" = "#CC79A7")

Number <- data.frame(MetaData %>% count(sample_id, Age_groups)) #Gender,
Number %>% group_by(Age_groups) %>% summarize(Mean= mean(n), SD=sd(n)) -> LNB

head(LNB)
px <- ggplot(data=Number, aes(x=Age_groups, y=n, fill=Age_groups)) +
  geom_violin(aes(fill = Age_groups), trim = TRUE)  +
  stat_summary(fun.data = "mean_sdl") +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1) + #,binwidth = 20
  #+ coord_flip() 
  scale_fill_manual(values=cols) +
  ylab("Number of cells") +
  xlab("Age groups") +
  scale_x_discrete(limits = age_groups) +
  theme(legend.position="none", 
        axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=18), 
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        plot.title = element_text(hjust = 0.5,face='bold',size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill=NA, color = 'black', size=1))
print(px)

ggsave("./violin_plot_number_cells_age_group_03162026.pdf", px , width=3, height=1.2,  units="in", scale=3)

