
  # =============================================================================
# Supplementary Fig. 7f— 
# =============================================================================

  #https://www.science.org/doi/10.1126/sciimmunol.abf0125?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed
  GD_T <- readRDS('/Users/nehard/MyProjects/DU_lab/LS95/public_data/GSE149356_Human_GDT_Seurat.rds')
  GD_T <- UpdateSeuratObject(GD_T)


  Supplementary Figure. 7f (left panel):
  # SOX4 expression
  gd_FP <- FeaturePlot(GD_T, features = c('SOX4'),
              pt.size =0.5, 
              cols =c("lightgrey", "#e6550d")) &  theme_void()  & theme(plot.title = element_text(size = 20, face = 'italic',hjust = 0.5,family = 'Helvetica')) 
  ggsave("../PANELS/FP_SOX4_dgTcells_04172025.pdf", 
           gd_FP,
         width=2.5, height=2,  units="in", scale=3, dpi=100)
 
 
 Supplementary Figure. 7f (right panel):

 DP_sox4 <- DimPlot(GD_T, cols= c('Adult'='#4c459c','Newborn'='#96daf7'),
                       group.by = 'group')
    
    
     
    #0 custom color scale
    scale.col <- cet_pal(16, name = "fire")
    # make plot
    ppp <- DP_sox4[[1]]  & 
      stat_density_2d(aes_string(x = "UMAP_1", y = "UMAP_2",fill = 'group'), 
                      linewidth = 0.3, 
                      geom = "density_2d_filled", 
                      colour = "black",
                      alpha = 0.6, 
                      n = 150, h = c(1.2, 1.2)) & 
      facet_wrap(vars(group), nrow = 1) & 
      ylab('UMAP_2') & xlab('UMAP_1') & scale_fill_manual(values=c('Adult'='#4c459c','Newborn'='#96daf7')) &
      #scale_x_discrete(limits=c('HI','HC','HY','HO')) &
      theme(legend.position="none", 
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title=element_blank(),
            strip.text.x = element_text(size = 14, face = 'bold')) #, axis.title.x=element_blank(), axis.title.y=element_blank())
    ppp

 Supplementary Figure. 7g:
   
# Violin plots 
    VP_gd <- VlnPlot(GD_T, 
                     features = c('TRDC','SOX4'),
                     group.by = 'group', ncol = 2, 
                     cols=c('Adult'='#4c459c','Newborn'='#96daf7'),
                     pt.size = 0) & theme(legend.position="none", 
                                            axis.ticks.x=element_blank(),
                                            axis.ticks.y=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank())
    
    VP_gd

   ggsave(".violinplot_gdTcells_GSE149356_04172025.pdf", VP_gd,
           width=1.5, height=1,  units="in", scale=3)
