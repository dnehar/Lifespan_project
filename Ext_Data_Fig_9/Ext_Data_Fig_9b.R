



p_tonic <- LifeSpan_ALL_MetaData %>% 
     
     mutate(Groups = factor(Groups, levels = c("HI", "HC", "HY", "HO"))) %>%
     mutate(ReCluster = factor(Final_annotations, levels = ordered_SC)) %>% #*****
     group_by(Groups, Names, ReCluster) %>%
     summarise(n = n()) %>% #, Set = first(Set)
     
     #summarise(n = n(), Age_months = first(as.numeric(Age_months)), Gender = first(as.numeric(Gender))) %>% #, Set = first(Set)
     mutate(freq = n / sum(n) *100) %>%
     ungroup() %>%
     as.data.frame() %>%
     filter(ReCluster %in% c( 'CD4_ISGhi')) %>% 
     
     #plot the number of cells
     ggplot(aes(x=Names, y=n)) + 
     geom_bar(stat="identity",fill='#697d35', color="black") + #CD4 T cells:  #697d35 #CD14 mo: #f15d64,  B cell: "#4c459c"
     #geom_bar(stat="identity", color="black") +
     
     #geom_text(aes(label=n), vjust=-0.7, size=5) +
     ylab("Number of cells") +
     xlab("Individuals") +
     #scale_y_continuous(limits = c(0,100)) +
     # scale_x_discrete(limits=c("0", as.character(seq(1:(nbr_cl-1))))) + #labels= labels
     scale_x_discrete(limits=ordered_names) + #labels= labels
     #scale_x_discrete(limits= c("HI", "HC", "HY", "HO")) + #labels= labels
     #scale_fill_manual(values=cols) + 
     
     theme(axis.text.y=element_text(size=16), 
           axis.text.x=element_text(size=16, angle = 90),
           axis.title.x = element_text(face="bold", size=18),
           axis.title.y = element_text(face="bold", size=18),
           legend.position = "none") +
     # theme(axis.text.y=element_text(size=18), 
     #       axis.text.x=element_text(size=18), 
     #       axis.title.x = element_text(face="bold", size=20),
     #       axis.title.y = element_text(face="bold", size=20),
     #       plot.title = element_text(hjust = 0.5,face='bold',size=16),
     #       panel.grid.major = element_blank(), 
     #       panel.grid.minor = element_blank(),
     #       panel.border = element_rect(fill=NA, color = 'black', size=1))+
     ggtitle("ISGhi CD4 T cells ")  +
     theme(plot.title = element_text(hjust = 0.5, vjust = 2, size=20, face="bold"))
   
   p_tonic
