

#color 
cols <- c('GD_T_SOX4'= '#56bbbf',           
          'CD8_GD'= '#80622f')
age_groups <- c("HI", "HC", "HY", "HO")
my_comparisons <- combn(age_groups,2, FUN = list, simplify = T)

# subset to be plotted 
subset_to_be_plotted <- c('GD_T_SOX4','CD8_GD')

