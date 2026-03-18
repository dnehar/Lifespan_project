# =============================================================================
# Supplementary Fig. 1e  — violin plots showing the number genes per samples across age groups
#
# Input:  pbmcs_v1.rds  — available at dnehar/Lifespan_project/pbmcs_v1.rds
# Output: ./Number_of_genes_per_age_groups.pdf
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


Infants <- MetaData %>% filter (Age_groups =="Infants") %>% dplyr::select(n_genes)  #84769    9
Child <- MetaData %>% filter (Age_groups =="Child") %>% dplyr::select(n_genes)  #16238     9
Adolescent <- MetaData %>% filter (Age_groups =="Adolescent") %>% dplyr::select(n_genes)  #84769    9
Young <- MetaData %>% filter (Age_groups =="Young") %>% dplyr::select(n_genes)  #16238     9
Middle_aged <- MetaData %>% filter (Age_groups =="Middle_aged") %>% dplyr::select(n_genes)  #16238     9
Older <- MetaData %>% filter (Age_groups =="Older") %>% dplyr::select(n_genes)  #84769    9
Oldest_old <- MetaData %>% filter (Age_groups =="Oldest_old") %>% dplyr::select(n_genes)  #16238     9

mat <-  MetaData %>% dplyr::select(Age_groups,n_genes)
head(mat)
mat1 <- melt(mat)
#-- reorder levels 
mat1$Groups <- factor(mat1$Age_groups, levels = c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old'))

K <- ggplot(mat1, aes(x=value, fill=Groups)) + 
  geom_histogram(position="identity") +
  geom_vline(aes(xintercept=mean(value)), color="black",
             linetype="dashed")+
  facet_wrap(~Groups,ncol = 7 ,scales = "free") + 
  scale_fill_manual(values=cols) + #***
  scale_color_grey()+
  theme(legend.position="none", 
        axis.text.y=element_text(size=18), 
        axis.text.x=element_text(size=18, angle = 90),
        axis.title.x = element_text(face="bold", size=18),
        axis.title.y = element_text(face="bold", size=18),
        plot.title = element_text(hjust = 0.5,face='bold',size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, color = 'black', size=1))+
  theme(strip.text.x = element_text(size = 16),
        strip.background = element_rect(colour = 'black',fill='#C0C0C0'),
        panel.background = element_rect(fill = "white")) +
  xlab("number of genes in age groups")
print(K)
ggsave("./Number_of_genes_per_age_groups.pdf",   
       K , width=5, height=1.2,  units="in", scale=3)


