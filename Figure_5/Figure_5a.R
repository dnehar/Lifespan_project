


#color 
cols <- c('B_ISGhi'='#9ecae1',
            'B_memory'='#283779',
            'B_ABC'='#41b8ea',
            'PCs'='#8856a7',
            'B_transitional'='#756bb1',
            'B_naive'='#1c9099')
            

# plot umap B cells

p_umap_subset <- LifeSpan_ALL_MetaData %>% 
dplyr::filter(Lineage %in% 'B_cells') %>% 
filter (!Final_annotations %in% c('IGLC6_IGLC7_Bcells','B_C9')) %>% 
ggplot(aes(x=SC_umap1, y=SC_umap2,  color=Final_annotations)) +
geom_point(size=0.5) + #, alpha = 1
scale_color_manual(values=cols) + 
theme_void() 
print(p_umap_subset)
