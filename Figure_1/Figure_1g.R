


# colors 
  cols_L2 <-  c('B_naive' = '#41b8ea',
    'B_memory' = '#283779',
    'CD4_T_ISGhi' = '#697d35',
    'CD4_T_Memory' = '#1c572b',
    'CD4_T_Naive' = '#193a1c',
    'CD4_Tregs'='#137d82',
    'CD8_T_Effector'='#fba919',
    'CD8_T_Naive'='#f37421', 
    'CD14_mo'='#f6a2a7', 
    'CD16_mo'='#f9d3d7',
    'DCs'='#ed2024', 
    'HSPC'='#b0479a',
    'Mgk'='#932169', 
    'NK_CD16'='#fee000', 
    'NK_XCL1'='#f2e4a0', 
    'PCs'='#232323ff', 
    'gd_Tcells'='#80622f', 
    'pDC'='#a5a4a4')

# load meta data 
LifeSpan_ALL_MetaData <- read.csv("meta/LifeSpan_ALL_Annotated_MetaData_09122024.csv", row.names = 1) %>% as.data.frame()
