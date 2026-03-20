
# Libraries & functions UI  ------------

library(RColorBrewer)
library(tidyr)
library(dplyr)
library(reshape2)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(pheatmap)
library(threejs)
library(plotly)
library(DT)
library(rsconnect)
library(rintrojs)
library(anndata)
library(cetcolor)
library(ggpubr)

#-- to be able to hit 'enter' button
jscode <- '
  $(function() {
  var $els = $("[data-proxy-click]");
  $.each(
  $els,
  function(idx, el) {
  var $el = $(el);
  var $proxy = $("#" + $el.data("proxyClick"));
  $el.keydown(function (e) {
  if (e.keyCode == 13) {
  $proxy.click();
  }
  });
  }
  );
  });
  '


ui <- 
  #dashboardPage(dashboardHeader(title="Test Page"),
  fluidPage(
    theme = shinytheme("cerulean"), # united
    
    navbarPage('Lifespan project',
               
               #├ Paper information/Overview ---- ####
               
               tabPanel('Overview',
                        
                        br(),
                        
                        h4("The data presented is associated with the following publication:"),
                        h6("Nehar-Belaid et al., under revision - Preprint: PMID: 40666903"),
                        # box(width = 2, tags$a("", 
                        #                       href="XXXX", 
                        #                       target="_blank",
                        #                       tags$img(src = "NatureImm.png", height = "25", width = "100")
                        # )),
                        # 
                        fluidRow(style = 'margin-left: 10%; margin-right: 10%;',
                                 # image of journal and link to the preprint/etc
                                 box()
                        ),
                        
                        br(),
                        
                        #h4("Experimental design/Pipeline:",style = 'margin-left: 10%; margin-right: 10%;',),
                        #br(),
                        
                        fluidRow(style = 'margin-left: 10%; margin-right: 10%;',
                                 box(width = 2, tags$a("", 
                                                       target="_blank",
                                                       tags$img(src ="Overview3.png", 
                                                                height = "400", 
                                                                width = "650")
                                 ))),
                        
                        br(),
                        
                        h4("Data will be available in GEO (GSE233321) and dbGAP (phs003259.v1.p1):"),
                        #h5("GSE233321 and phs003259.v1.p1"),
                        br(),
                        fluidRow(
                          box(width = 2, tags$a("", 
                                                href="https://www.ncbi.nlm.nih.gov/geo/", 
                                                target="_blank",
                                                tags$img(src = "GEO.png", height = "50", width = "100")
                          )),
                          
                          box(width = 2, tags$a("", 
                                                href="https://www.ncbi.nlm.nih.gov/gap/", 
                                                target="_blank",
                                                tags$img(src = "dbgap.png", height = "30", width = "80")
                          )),
                          box()
                          
                          
                        ),
                        br(),
                        
                        
                        h4("This study is a collaboration of the following groups:"),
                        br(),
                        
                        fluidRow(
                          box(width = 2, tags$a("", 
                                                href="https://www.jax.org", target="_blank",
                                                tags$img(src = "jax.png", height = "45", width = "130")
                          ))
                          
                          # box(width = 2, tags$a("", 
                          #                       href="https://drukierinstitute.weill.cornell.edu/", 
                          #                       target="_blank",
                          #                       tags$img(src = "WCM.png", height = "40", width = "130")
                          # )),  
                        ),
                        br(),
                        
                        h4("This study was funded and supported by:"),
                        br(),
                        fluidRow(
                          box(width = 2, tags$a("", 
                                                href="https://www.niams.nih.gov/", 
                                                target="_blank",
                                                tags$img(src = "NIAMS.png", height = "50", width = "100")
                          )),
                          box()
                        ),
                        br(),
                        
                        h4("Source Code/Questions?"),
                        fluidRow(
                          # image of journal and link to the preprint/etc
                          box(width = 2, tags$a("", 
                                                href="https://github.com/dnehar/SingleCells_SLE_paper", 
                                                target="_blank", 
                                                tags$img(src = "GitHub.png", height = "45", width = "80")
                          ))
                          
                        ),
                        br(),
                        br(),
                        
                        
               ), #end tabPanel Overview
               
               
               #├ Data upload ####    
               
               tabPanel('Data Summary',
                        
                        #-- rds file upload 
                        fluidRow(column(
                          2,
                          offset = 3,
                          uiOutput('datasets')
                        )),
                        # fluidRow(style = 'margin-left: 10%; margin-right: 10%;',
                        #   column(2,offset=5,
                        #          tags$head(tags$script(HTML(jscode))),
                        #          actionButton('goButton0', 'Load dataset'))),
                        br(),
                        br(),
                        
                        
                        
                        # ├├ LS overview #### 
                        
                        fluidRow(style = 'margin-left: 10%; margin-right: 10%;',
                                 #div(plotOutput('plot'), style = 'width: 60%; display: inline-block; vertical-align: middle;'),
                                 #style = 'display: inline-block; vertical-align: middle;',
                                 box(width = 2,offset = 1, tags$a("", 
                                                                  target="_blank",
                                                                  tags$img(src ="LS95_overvieww.png", 
                                                                           height = "400", 
                                                                           width = "650",  align="center"))
                                 )),
                        
                        
                        #├├ data information #### 
                        fluidRow(style = 'margin-left: 10%; margin-right: 10%;',
                                 column(5, offset = 1,
                                        htmlOutput("text1")
                                 ),
                                 
                                 tags$style(
                                   "#text1{color: black;
                 font-size: 18px;
                 font-style: regular;
                 }"),
                        ), 
                        
                        br(),
                        br(),
                        
                        # ├├ umap overview #### 
                        
                        # mainPanel(column(10, 
                        #                  offset = 5,
                        #                  fluidRow(plotOutput("UMAP_overview")))
                        # )
                        
                        
               ), #end tabPanel Data opload 
               
               
               #├ PBMCS -------####
               navbarMenu('PBMCs',
                          
                          #├├ 1- Boxplot ####
                          tabPanel('Boxplots', 
                                   #-- header          
                                   div(h3('Subsets vs. age groups (% PBMCs)'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(
                                     column(2, offset = 4,
                                            #-- select umap plot of interest 
                                            selectInput("Condition_boxplot", 
                                                        label= "Select", 
                                                        multiple=F, 
                                                        choices = c("PBMC_level1"="LS_L1",
                                                                    "PBMC_level2"="LS_L2",
                                                                    "PBMC_level3"="LS_L3")), 
                                            
                                            #--- # of rows size 
                                            textInput("nb_columns", label = ("Select # of rows"), value = '2'),
                                            #-- action button 
                                            actionButton('goButton1', 'Click to see box plot')
                                     ),
                                     `data-proxy-click` = "goButton1"),         
                                   
                                   br(),       
                                   br(),
                                   br(),
                                   
                                   #-- boxplot 
                                   mainPanel(column(10, 
                                                    offset = 5,
                                                    fluidRow(plotOutput("BoxPlot")))
                                   )
                          ),#)), boxplot
                          
                          
                          #├├ 2- Corplot -------####
                          tabPanel('Corplots', 
                                   #-- header          
                                   div(h3('Subsets vs. age (% PBMCs)'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(column(4, offset = 4,
                                                   #--- condition selection
                                                   selectInput("condition_corplot", 
                                                               label= "Select", 
                                                               multiple=F, 
                                                               choices = c("PBMC_level1"="LS_L1",
                                                                           "PBMC_level2"="LS_L2",
                                                                           "PBMC_level3"="LS_L3")),
                                                   
                                                   #--- # of rows size 
                                                   textInput("nb_columns2", label = ("Select # of rows"), value = '2'),
                                                   
                                                   #-- action button
                                                   actionButton('goButton3', 'Click to see Corplots'))),
                                   
                                   br(),
                                   br(),  
                                   
                                   #-- corplot
                                   mainPanel(column(10, 
                                                    offset = 5,
                                                    fluidRow(plotOutput("CorPlot")))
                                   )
                                   
                          ), #corplot PBMCs 
                          
                          
                          #├├ 3- Barplots -------####
                          tabPanel('Barplots', 
                                   #-- header          
                                   div(h3('Subsets vs. age (% PBMCs)'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(column(12, offset = 4,
                                                   #--- condition selection
                                                   selectInput("condition_barplot", 
                                                               label= "Select", 
                                                               multiple=F, 
                                                               choices = c("PBMC_level1"="LS_L1",
                                                                           "PBMC_level2"="LS_L2",
                                                                           "PBMC_level3"="LS_L3")),
                                                   
                                                   #--- # of rows size 
                                                   #textInput("nb_columns2", label = ("Select # of rows"), value = '3'),
                                                   br(),
                                                   
                                                   #-- action button
                                                   actionButton('goButton5', 'Click to see Barplot'))),
                                   
                                   br(),
                                   br(),  
                                   br(),
                                   br(),  
                                   
                                   #-- barplot
                                   mainPanel(column(12, 
                                                    offset = 3,
                                                    style = "height:200px",
                                                    fluidRow(plotOutput("BarPlot")))
                                   )
                                   
                          ) #corplot PBMCs 
               ),
               
               
               #├ SUBSETS -------####
               navbarMenu('SUBSETS',
                          
                          #├├  0- Umaps  -------####     
                          tabPanel('Umaps', 
                                   #-- header          
                                   div(h2('Umap plots - immune subsets'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(
                                     column(2, offset = 4,
                                            #-- select umap plot of interest 
                                            selectInput("Condition_umap", 
                                                        label= "Select subset", 
                                                        multiple=F, 
                                                        choices = c("Dendritic cells"='Dendritic_Cells',
                                                                    "Monocytes"= 'Monocytes', 
                                                                    'NK_cells'='NK_cells',
                                                                    'B_cells'='Bcells',
                                                                    'CD4_Tcells'='CD4_Tcells',
                                                                    'CD8_Tcells'='CD8_Tcells',
                                                                    'CD4_T_memory'='CD4_T_memory',
                                                                    'T_helpers'='T_helpers',
                                                                    'Tregs'='Tregs',
                                                                    'gd_Tcells'='gammadelta_T')), 
                                            
                                            
                                            #--- # of rows size 
                                            textInput("point_size_umap", label = ("Select point size"), value = '0.5'),
                                            #-- action button 
                                            actionButton('goButton8', 'Click to see umap plot')
                                     ),
                                     `data-proxy-click` = "goButton8"),         
                                   
                                   br(),       
                                   br(),
                                   br(),       
                                   
                                   
                                   #-- umap plot  
                                   mainPanel(column(12, 
                                                    offset = 3,
                                                    style = "height:200px",
                                                    fluidRow(plotOutput("UMAP1")))
                                   )
                          ), #boxplot2
                          
                          
                          #├├  1- Boxplot  -------####
                          tabPanel('Boxplots', 
                                   #-- header          
                                   div(h3('Subsets vs. age groups (% lineage)'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(
                                     column(2, offset = 4,
                                            #-- select umap plot of interest 
                                            selectInput("Condition_boxplot2", 
                                                        label= "Select subset", 
                                                        multiple=F, 
                                                        choices = c("Dendritic cells"='DCs',
                                                                    "Monocytes"= 'monocytes', 
                                                                    'NK_cells'='NK_cells',
                                                                    'B_cells'='B_cells',
                                                                    'CD4_Tcells'='CD4_Tcells',
                                                                    'CD8_Tcells'='CD8_Tcells',
                                                                    'gd_Tcells'='gd_Tcells')), 
                                            
                                            
                                            #--- # of rows size 
                                            textInput("nb_raws", label = ("Select # of rows"), value = '1'),
                                            #-- action button 
                                            actionButton('goButton2', 'Click to see box plot')
                                     ),
                                     `data-proxy-click` = "goButton2"),         
                                   
                                   br(),       
                                   br(),
                                   br(),
                                   
                                   #-- boxplot 
                                   mainPanel(column(8, 
                                                    offset = 5,
                                                    fluidRow(plotOutput("BoxPlot2")))
                                   )
                          ), #boxplot2
                          
                          
                          #├├ 2-Corplots  ------- ####
                          tabPanel('Corplots', 
                                   #-- header          
                                   div(h3('Subsets vs. age (% lineage)'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(column(4, offset = 4,
                                                   #--- condition selection
                                                   selectInput("condition_corplot2", 
                                                               label= "Select subset", 
                                                               multiple=F, 
                                                               choices = c("Dendritic cells"='DCs',
                                                                           "Monocytes"= 'monocytes', 
                                                                           'NK_cells'='NK_cells',
                                                                           'B_cells'='B_cells',
                                                                           'CD4_Tcells'='CD4_Tcells',
                                                                           'CD8_Tcells'='CD8_Tcells',
                                                                           'gd_Tcells'='gd_Tcells')),
                                                   
                                                   #--- # of rows size 
                                                   textInput("nb_rows2", label = ("Select # of rows"), value = '1'),
                                                   
                                                   #-- action button
                                                   actionButton('goButton4', 'Click to see Corplots'))),
                                   
                                   br(),
                                   br(),  
                                   
                                   #-- corplot
                                   mainPanel(column(10, 
                                                    offset = 5,
                                                    fluidRow(plotOutput("CorPlot2")))
                                   )
                                   
                          ), #corplot subets
                          
                          #├├ 3-Barplots  ------- ####
                          tabPanel('Barplots', 
                                   #-- header          
                                   div(h2('Subsets vs. age (% lineage)'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(column(12, offset = 4,
                                                   #--- condition selection
                                                   selectInput("condition_corplot2", 
                                                               label= "Select subset", 
                                                               multiple=F, 
                                                               choices = c("Dendritic cells"='DCs',
                                                                           "Monocytes"= 'monocytes', 
                                                                           'NK_cells'='NK_cells',
                                                                           'B_cells'='B_cells',
                                                                           'CD4_Tcells'='CD4_Tcells',
                                                                           'CD8_Tcells'='CD8_Tcells',
                                                                           'gd_Tcells'='gd_Tcells')),
                                                   
                                                   
                                                   br(),
                                                   
                                                   #-- action button
                                                   actionButton('goButton6', 'Click to see Borplot'))),
                                   
                                   
                                   br(),
                                   br(),  
                                   br(),
                                   br(), 
                                   #-- corplot
                                   mainPanel(column(12, 
                                                    offset = 3,
                                                    style = "height:200px",
                                                    fluidRow(plotOutput("BarPlot2")))
                                   )
                                   
                          ) #corplot subets
                          
                          
               ),
               
               
               #├ Demographics -------####
               navbarMenu('Demographics',
                          tabPanel('Piecharts', 
                                   #-- header          
                                   div(h2('Demographics & data information'), align = 'center'),  
                                   br(),
                                   br(),  
                                   br(),
                                   
                                   #-- multiple choices 
                                   fluidRow(
                                     column(2, offset = 4,
                                            #-- select umap plot of interest 
                                            selectInput("condition_demographics", 
                                                        label= "Select", 
                                                        multiple=F, 
                                                        choices = c("Sex"="Sex",
                                                                    #"Infered_CMV"="Infered_CMV",
                                                                    'Platform'='Platform',
                                                                    'CMV'='Infered_CMV')), 
                                            
                                            #-- action button 
                                            actionButton('goButton7', 'Click to see pie charts')
                                     ),
                                     `data-proxy-click` = "goButton7"),         
                                   
                                   br(),       
                                   br(),
                                   br(),
                                   
                                   #-- boxplot 
                                   mainPanel(column(8, 
                                                    offset = 5,
                                                    fluidRow(plotOutput("Demographics")))
                                   )
                          ))#)), Demographics
               
               
               
    )#navbarPage (global)
  )#fluidPage (global)





#------------------------------------------- server ------------------------------------  ####

server <- function(input, output) {
  options(shiny.maxRequestSize = 2000 * 1024 ^ 2)
  
  
  
  #--- FUNCTIONS ----- ####
  
  diffLRT = function(x, y, xmin = 1) {
    lrtX = bimodLikData(x)
    lrtY = bimodLikData(y)
    lrtZ = bimodLikData(c(x, y))
    lrt_diff = 2 * (lrtX + lrtY - lrtZ)
    return(pchisq(lrt_diff, 3, lower.tail = F))
  }
  
  bimodLikData = function(x, xmin = 0) {
    x1 = x[x <= xmin]
    x2 = x[x > xmin]
    xal = minmax(length(x2) / length(x),
                 min = 1e-5,
                 max = (1 - 1e-5))
    likA = length(x1) * log(1 - xal)
    mysd = sd(x2)
    if (length(x2) < 2) {
      mysd = 1
    }
    likB = length(x2) * log(xal) + sum(dnorm(x2, mean(x2), mysd, log = TRUE))
    return(likA + likB)
  }
  
  ainb = function(a, b) {
    a2 = a[a %in% b]
    return(a2)
  }
  
  minmax = function(data, min, max) {
    data2 = data
    data2[data2 > max] = max
    data2[data2 < min] = min
    return(data2)
  }
  set.ifnull = function(x, y) {
    if (is.null(x))
      return(y)
    return(x)
  }
  
  expMean = function(x) {
    return(log(mean(exp(x) - 1) + 1))
  }
  
  
  DiffExpTest = function(expression,
                         cells.1,
                         cells.2,
                         genes.use = NULL,
                         print.bar = TRUE) {
    genes.use = set.ifnull(genes.use, rownames(expression))
    p_val = unlist(lapply(genes.use, function(x)
      diffLRT(
        as.numeric(expression[x, cells.1]), as.numeric(expression[x, cells.2])
      )))
    to.return = data.frame(p_val, row.names = genes.use)
    return(to.return)
  }
  
  #----- DEG calculation
  DiffExpTest = function(expression, cells.1, cells.2, genes.use = NULL, print.bar = TRUE) {
    genes.use = set.ifnull(genes.use, rownames(expression))
    p_val = unlist(lapply(genes.use, function(x) diffLRT(as.numeric(expression[x, cells.1]), as.numeric(expression[x, cells.2]))))
    to.return = data.frame(p_val, row.names = genes.use)
    return(to.return)
  }
  
  
  
  #---- INPUTS ---- ####
  
  #data <- reactive({
  #observeEvent(input$datasetid, {
  
  output$datasets <- renderUI({
    file.ids <- list.files("./files/")
    selectInput("datasetid",
                label="Dataset", 
                choices=file.ids, 
                selected=2)
  })
  
  observeEvent(input$datasetid, {
    
    # ===============================================================#
                          #├├  load data ####
    # ===============================================================#
    
    inFile <- input$datasetid
    #print(inFile)
    if (is.null(inFile))
      return(NULL)
    file.in <- file.path("./files", inFile)
    print(file.in)
    
    MetaData <- readRDS(file.in)
    cat(stderr(), 'Loaded')
    
    #print( head(data[,(ncol(data)-10):ncol(data)]))
    
    #- cluster (dataset name)
    CL.name <- gsub(".rds", "", inFile)
    
    # load umap coordinates 
    LS_list <- readRDS("./files/umaps_coordinates.rds")
    
    # meta data 
    LifeSpan_ALL_MetaData <- MetaData[['meta_small']] %>% filter (!LS_L4 %in% c('doublets')) %>% as.data.frame()
    pheno <- MetaData[['pheno']] %>% as.data.frame()
    
    # ordered sample id (based on age)
    ordered_ids <- as.character(pheno$sample_id)

    # Remove rows where LS_L1 is NA or blank/whitespace
    LifeSpan_ALL_MetaData <- LifeSpan_ALL_MetaData %>%
      mutate(LS_L1 = trimws(LS_L1)) %>%              # strip leading/trailing spaces
      filter(!is.na(LS_L1), LS_L1 != "") %>%         # keep only labeled cells
      droplevels()   
    
    #├├  colors ####
    
    cols <- c(
      # Level 1
      "CD4_Tcells" = "#193a1c",
      "CD8_Tcells" = "#f37421",
      "gd_Tcells" = "#80622f",
      "NK_cells" = "#fee000",
      "B_cells" = "#1c9099",
      "PCs" = "#8856a7",
      "monocytes" = "#f6a2a7",
      "DCs" = "#ed2024",
      "HSPC" = "#b0479a",
      
      # Level 2
      "B_naive" = "#1c9099",
      "B_memory" = "#283779",
      "CD4_ISGhi" = "#697d35",
      "CD4_memory" = "#90aa3c",
      "CD4_naive" = "#193a1c",
      "CD4_Tregs" = "#137d82",
      "CD8_memory" = "#fba919",
      "CD8_naive" = "#f37421",
      "CD14_mono" = "#f6a2a7",
      "CD16_mono" = "#f9d3d7",
      "Mgk" = "#932169",
      "CD56bright_NK" = "#f2e4a0",
      "CD56dim_NK" = "#fee000",
      "pDCs" = "#a5a4a4",
      
      # Level 3
      "ISGhi_CD14_mono" = "#f15d64",
      "CD4_Proliferating" = "#2a9d8f",
      "CD8_CM" = "#f59e2f",
      "CD8_GZMK" = "#fba919",
      "CD8_MAIT" = "#fbb36a",
      "CD8_TEMRA" = "#d28529",
      "CD8_gdT" = "#80622f",
      "CD8aa" = "#c46b1c",
      "B_transitional" = "#756bb1",
      "B_ABC" = "#41b8ea",
      "B_ISGhi" = "#9ecae1",
      "moDC" = "#ed2024",
      "cDC1" = "#771215",
      "cDC2" = "#d84598",
      "AXL_DC" = "#a41e21",
      "pDC" = "#a5a4a4",
      "Adaptive_NK" = "#feb24c",
      "Proliferating_NK" = "#ccb72d",
      
      # Level 4 additions
      "gdT_Vd2_GZMK" = "#d29734",
      "gdT_Vd2_GZMB" = "#d8bd93",
      "gdT_Vd1_SOX4" = "#56bbbf",
      "gdT_Vd1_KLRF1" = "#993404",
      "gdT_Vd1_Naive" = "#ffeda0",
      "Tregs_naive" = "#137d82",
      "Tregs_mem" = "#56bbbf",
      "CD4_naive_SOX4-" = "#193a1c",
      "CD4_naive_SOX4+" = "#a4de02ff",
      "CD8_naive_SOX4+" = "#ffdeadff",
      "CD8_naive_SOX4-" = "#f37421",
      
      #Tmem - helpers
      'TH2'= '#1c7b3d',
      'TH17'= '#3cb54a',
      'CXCR5+_TFH-like'= '#74c168',
      'TH10'= '#a4de02ff',#'#a4de02ff', 
      'TPH'= '#697d35',
      'GZMK_TH1_like'= '#7fcdbb',
      'doublets'='#a8ddb5',
      'CD4_TEMRA'='#1c572b',
      'TH22'= '#edf8b1',
      
      # Groups
      "Infants" = "#0072B2",
      "Child" = "#56B4E9",
      "Adolescent" = "#009E73", 
      "Young" = "#F0E442",
      "Middle_aged" = "#E69F00",
      "Older" ="#D55E00",
      "Oldest_old" = "#CC79A7"
    )
    
    
    
    length(cols)
    
    my_comparisons <- list (c('Infants', 'Child'),
                            c('Child','Adolescent'),
                            c('Adolescent', 'Young'),
                            c('Young', 'Middle_aged'),
                            c('Middle_aged', 'Older'),
                            c('Older', 'Oldest_old'))
    # Level 1: LS_L1  (n=9 clusters)
    order_LS_L1<- c("monocytes","DCs","NK_cells" ,"B_cells" ,
                    "PCs","HSPC","CD4_Tcells", "CD8_Tcells" ,"gd_Tcells")
    
    # Level 2: LS_L2  (n=18 clusters)
    order_LS_L2 <- c('CD14_mono', 'CD16_mono', 'DCs', 'pDCs', 'Mgk','HSPC',
                     'CD56bright_NK', 'CD56dim_NK', 'gd_Tcells',
                     'B_naive', 'B_memory','PCs', 
                     'CD4_naive','CD4_ISGhi', 'CD4_memory', 'CD4_Tregs', 
                     'CD8_naive', 'CD8_memory')
    
    # Level 3 LS_L3  (n=32 clusters)
    order_LS_L3 <- c('CD14_mono','ISGhi_CD14_mono', 'CD16_mono',
                     'CD4_naive','CD4_ISGhi', 'CD4_Tregs','CD4_memory','CD4_Proliferating',
                     'CD8_naive', 'CD8_CM', 'CD8_GZMK','CD8_TEMRA', 'CD8_MAIT', 'CD8_gdT', 'CD8aa',
                     'B_transitional', 'B_naive','B_memory', 'B_ABC', 'B_ISGhi',  'PCs',  
                     'moDC', 'cDC1', 'cDC2', 'AXL_DC', 'pDC',
                     'HSPC', 'Mgk',
                     'CD56dim_NK','CD56bright_NK', 'Adaptive_NK', 'Proliferating_NK')
    
    # Level 4: LS_L4  (n=47 clusters)
    order_LS_L4 <- c('CD14_mono', 'CD16_mono','ISGhi_CD14_mono','cDC1', 'cDC2','moDC','AXL_DC', 'pDC', 'HSPC', 'Mgk',
                     'B_transitional','B_naive', 'B_ABC', 'B_ISGhi', 'B_memory',  'PCs',
                     'CD56bright_NK', 'CD56dim_NK', 'Adaptive_NK',  'Proliferating_NK',
                     'CD8_naive_SOX4+','CD8_naive_SOX4-', 'CD8_CM', 'CD8_GZMK', 'CD8_MAIT','CD8aa','CD8_TEMRA',
                     'CD4_naive_SOX4+', 'CD4_naive_SOX4-',  'CD4_ISGhi', 'CD4_Proliferating', 'CD4_TEMRA',
                     'Tregs_mem', 'Tregs_naive', 'GZMK_TH1_like','TH10', 'TH17', 'TH2', 'TH22', 'TPH', 'CXCR5+_TFH-like', 
                     'gdT_Vd1_Naive', 'gdT_Vd1_SOX4', 'gdT_Vd1_KLRF1',  'gdT_Vd2_GZMB', 'gdT_Vd2_GZMK','doublets')
    
    
    group_sex_order <- c("Infants_M","Infants_F","Child_M","Child_F",
                         "Adolescent_M", "Adolescent_F","Young_M","Young_F",      
                         "Middle_aged_F", "Middle_aged_M", "Older_M","Older_F", 
                         "Oldest_old_M","Oldest_old_F")
    
    age_groups <- c('Infants', 'Child','Adolescent', 'Young', 'Middle_aged', 'Older', 'Oldest_old')
    my_comparisons <- list (c('Infants', 'Child'),
                            c('Child','Adolescent'),
                            c('Adolescent', 'Young'),
                            c('Young', 'Middle_aged'),
                            c('Middle_aged', 'Older'),
                            c('Older', 'Oldest_old'))
    
      #-----  goButtons ------ ####
    v0 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton0, {
      v0$doPlot <- input$goButton0
    })
    
    v1 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton1, {
      v1$doPlot <- input$goButton1
    })
    
    v2 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton2, {
      v2$doPlot <- input$goButton2
    })
    
    v3 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton3, {
      v3$doPlot <- input$goButton3
    })
    
    v4 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton4, {
      v4$doPlot <- input$goButton4
    })
    
    v5 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton5, {
      v5$doPlot <- input$goButton5
    })
    
    v6 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton6, {
      v6$doPlot <- input$goButton6
    })
    
    v7 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton7, {
      v7$doPlot <- input$goButton7
    })
    
    v8 <- reactiveValues(doPlot = FALSE)
    observeEvent(input$goButton8, {
      v8$doPlot <- input$goButton8
    })
    
    #├ OVERVIEW - DATA upload +  : number of cells & co ---------  
    
    output$text1 <-  renderUI({
      #-- Progress bar
      withProgress(message = 'Uploading in progress',
                   value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.05)
                       
                     }
                   })
      #--- text with stat summary 
      L1 <- paste0("Some infromation about our dataset: ")
      L2 <- paste0("Number of  cells: 1,191,563 cells")
      L3 <- paste0("Number of highly variable genes: 2000 genes")
      L4 <- paste0("Level 1 clustering: n=", length(unique(LifeSpan_ALL_MetaData$LS_L1)), " clusters") #(Fig.1f & Fig.1g)
      L5 <- paste0("Level 2 clustering: n=", length(unique(LifeSpan_ALL_MetaData$LS_L2)), " clusters") # (Fig.1b & Fig.1d)
      L6 <- paste0("Level 3 clustering: n=", length(unique(LifeSpan_ALL_MetaData$LS_L3)), " clusters")
      L7 <- paste0("Level 4 clustering: n=", length(unique(LifeSpan_ALL_MetaData$LS_L4)), " subclusters")
      
      
      HTML(
        paste0('<br/>','<br/>',
               L1, '<br/>', '<br/>',
               L2, '<br/>',
               L3,'<br/>',
               L4,'<br/>',
               L5,'<br/>',
               L6,'<br/>',
               L7)
      )
      
    })
    
    
    #├ UMAP overview  #####
    
    
    
    
    # output$UMAP_overview <- renderPlot({
    #  
    #    isolate ({ 
    #      
    #      scale.col <- cet_pal(16, name = "fire")
    #      
    #      p_umap1 <- ggplot(mydataaa) +
    #        geom_point(aes(x=X_umap1, y=X_umap2,  color=Groups), #***
    #                   size=0.05, alpha = 0.4) + #, alpha = 1
    #        scale_color_manual(values= rep('#d9d9d9', 4),'clusters')+ #col_age_gp
    #        theme_void() +
    #        #theme(plot.title = element_text(size = 20, face = "bold", vjust = 0.03)) +
    #     
    #        
    #        stat_density_2d(aes_string(x = "X_umap1", y = "X_umap2",fill = 'Groups'), 
    #                        linewidth = 0.3, 
    #                        geom = "density_2d_filled", 
    #                        colour = "black",
    #                        alpha = 0.4, 
    #                        n = 150, 
    #                        h = c(1.2, 1.2)) & 
    #        facet_wrap(vars(Groups), nrow = 1, 
    #                   labeller = labeller(Groups = c("HC" = "Children (n=139,147)",
    #                                                  "HI" = "Infants (n=182,303)",
    #                                                  "HO" = "Older adults (n=135,043)",
    #                                                  "HY" = "Young adults (n=126,051)"
    #                                                  ))) & 
    #        ylab('UMAP_2') & xlab('UMAP_1') & scale_fill_manual(values=col_age_gp) &
    #        scale_x_discrete(limits=c('HC','HI','HO','HY')) & #labels= labels
    # 
    #        theme(legend.position="none", 
    #              axis.ticks.x=element_blank()
    #              ,axis.ticks.y=element_blank(),
    #              plot.title=element_blank(),
    #              strip.text.x = element_text(size = 12, face = 'bold')) #, axis.title.x=element_blank(), axis.title.y=element_blank())
    #      p_umap1
    #      
    #      
    #    }) 
    #  })
    
    
    
    #├ PBMCs ####
    
    #├├ 1- Boxplot - PBMCs ####
    output$BoxPlot <- renderPlot({ 
      
      if (v1$doPlot == FALSE)
        return() 
      isolate ({ 
        
        
        
        my_comp_age <-  my_comparisons #combn(age_groups,2, FUN = list, simplify = T)
        
        TobePlotted <-  input$Condition_boxplot
        #print(TobePlotted)
        
        # prepare data 
        meta <-  data.frame(dplyr::select(LifeSpan_ALL_MetaData, 'clusters'=TobePlotted, Age_groups, sample_id))
        #print(head(meta))
        
        Boxplot_age <- meta %>% 
          #mutate(ReCluster = as.character(TBP_boxplot)) %>%
          #mutate(TobePlotted) %>%
          mutate(ReCluster = factor(clusters)) %>%
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          group_by(Groups, sample_id, ReCluster) %>%
          summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>%
          
          ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
          geom_boxplot(outlier.shape = NA) +
          geom_jitter(size = 0.2) +
          theme_bw()  +  #THEME +
          
          ggpubr::stat_compare_means(comparisons = my_comparisons, 
                                     label = "p.signif", 
                                     hide.ns = F, 
                                     vjust = 0.5) + 
          
          theme(legend.position = "none", 
                strip.text = element_text(size = 11, face = 'bold')) +
          
          facet_wrap(.~ReCluster, scales = "free_y", nrow = as.numeric(input$nb_columns)) + 
          
          scale_fill_manual(values=cols) + 
          theme(axis.text.y=element_text(size=16), 
                axis.text.x=element_text(size=16, angle=90),
                axis.title.x = element_text(face="bold", size=18),
                axis.title.y = element_text(face="bold", size=18)) + #    ylab('% PBMC') + xlab('Age groups') #    ylab('% PBMC') + xlab('Age groups')
          ylab('% PBMC') + xlab(' ')
        
        Boxplot_age
      }) 
    })
    
    #├├ 2- Corplots PBMCs ------- ####
    
    output$CorPlot <- renderPlot({ 
      
      if (v3$doPlot == FALSE)
        return() 
      isolate ({ 
        
        TobePlotted2 <-  input$condition_corplot
        #print(TobePlotted2)        
        
        # prepare data 
        meta <-  data.frame(dplyr::select(LifeSpan_ALL_MetaData, 'clusters'=TobePlotted2, Age_groups, sample_id, Age_in_yrs))
        #print(head(meta))
        
        # ALL AGE GROUPS
        p_corr <- meta %>%
          mutate(ReCluster = factor(clusters)) %>%
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
          
          summarise(n = n()) %>% 
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>%

          ggplot(aes(x = Age_in_yrs, y = freq, fill=ReCluster)) +
          geom_point(aes(shape = Groups, color=ReCluster)) +
          geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
          #geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color=ReCluster)) +
          scale_fill_manual(values=cols) + 
          scale_color_manual(values = cols)+
          ggpubr::stat_cor() +
          theme_bw() +
          theme(legend.position = "none", 
                strip.text = element_text(size = 11, face ='bold')) +
          facet_wrap(.~ReCluster, scales = "free_y", nrow = as.numeric(input$nb_columns2)) +
          theme(axis.text.y=element_text(size=16), 
                axis.text.x=element_text(size=16),
                axis.title.x = element_text(face="bold", size=18),
                axis.title.y = element_text(face="bold", size=18), 
                strip.text.x = element_text(size = 11, face ='bold')) + #    ylab('% PBMC') + xlab('Age groups') #    ylab('% PBMC') + xlab('Age groups'
          #    ylab('% PBMC') + xlab('Age groups')
          ylab('% in PBMCs') + xlab('Age (months)')
        #P2/P1/P3/P4
        p_corr
        
        
      })    
    })
    
    #├├ 3- Barplots -------####
    
    output$BarPlot <- renderPlot({ 
      
      if (v5$doPlot == FALSE)
        return() 
      isolate ({ 
        
        
        TobePlotted5 <-  input$condition_barplot
        #print(TobePlotted5)        
        
        # prepare data 
        meta5 <-  data.frame(dplyr::select(LifeSpan_ALL_MetaData, 'clusters'=TobePlotted5, Age_groups, sample_id))
        #print(head(meta5))
        
        # ├├├  Individual ####
        
        BP <- meta5 %>% 
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          mutate(ReCluster = factor(clusters)) %>%
          group_by(Groups, sample_id, ReCluster) %>%
          summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>%
          ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group = Groups)) +
          geom_bar(stat = "identity") + #, color = "black"
          scale_fill_manual(values=cols) + #***
          scale_x_discrete(limits=ordered_ids) + #labels= labels
          theme(axis.text.y=element_text(size=18), 
                axis.text.x=element_blank(),#, angle = 90
                axis.title.x = element_text(size=18),
                axis.ticks.x=element_blank(),
                #axis.ticks.y=element_blank(),
                axis.title.y = element_text(size=18),
                #panel.border = element_rect(fill=NA, color = 'black', size=1),
                legend.direction = "horizontal", 
                legend.position = 'bottom',
                legend.text.align = 1,
                plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
          ylab('% in PBMC') + 
          xlab('Individuals (n=95; ordered based on their age)') + 
          
          ggtitle('Individuals')
        
        BP 
        
        
        # ├├├  Age groups  ####
        
        BP_gp_pbmc <- meta5 %>% 
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          mutate(ReCluster = factor(clusters)) %>%
          #filter(ReCluster %in% subset_to_be_plotted) %>% 
          group_by(Groups, ReCluster) %>%
          #filter(Groups %in% c("HO_M",'HO_F')) %>% 
          summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>% #head()
          ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = ReCluster)) +
          geom_bar(stat = "identity", color = "black") +
          scale_fill_manual(values=cols) + #***
          scale_x_discrete(limits=age_groups) + #labels= labels
          theme(axis.text.y=element_text(size=18), 
                axis.text.x=element_text(size=18, angle = 0),
                axis.title.x = element_text(size=0),
                axis.ticks.x=element_blank(),
                axis.title.y = element_text(size=0),
                legend.position = "none",
                plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
          ggtitle('Age groups')
        
        
        gg_all_pbmc <- ggarrange(BP, BP_gp_pbmc, widths = c(2,1) )
        gg_all_pbmc
        
      })    
    })
    
    #├ SUBSETS ####
    
    #├├ 0- Umaps ####
    
    output$UMAP1 <- renderPlot({ 
      
      if (v8$doPlot == FALSE)
        return() 
      isolate ({ 
        
        TobePlotted8 <-  input$Condition_umap
        #print(TobePlotted2)
        
        #meta8 <-  LifeSpan_ALL_MetaData %>%  dplyr::select(LS_L1, LS_L4, Groups, SC_umap1, SC_umap2) %>% 
          #dplyr::filter(LS_L1 %in% TobePlotted8) %>% data.frame()
        
        
        meta8 <- LS_list[[TobePlotted8]]
        print(meta8)
        #order groups 
        # meta8$Age_groups = factor(meta8$Age_groups, 
        #                       levels=age_groups)
        # 
        
        # p_umap1 <- meta8 %>% ggplot(aes(x=SC_umap1, y=SC_umap2,  color=LS_L4)) +
        #   geom_point(size=as.numeric(input$point_size_umap)) + #, alpha = 1
        #   scale_color_manual(values=cols) + 
        #   theme_void() 
        
        p_umap1 <- meta8 %>%
          ggplot(aes(x = SC_umap1, y = SC_umap2, color = Final_annotations)) +
          geom_point(size = 0.1) +
          scale_color_manual(values = cols, drop = FALSE) +
          theme_void() +
          guides(color = guide_legend(override.aes = list(size = 3)))
        
        p_umap1
        
        
        #  p_umap2 <- p_umap1  &
        #   stat_density_2d(aes_string(x = "SC_umap1", y = "SC_umap2",fill = 'Groups'),
        #                   linewidth = 0.3,
        #                   geom = "density_2d_filled",
        #                   colour = "black",
        #                   alpha = 0.4,
        #                   n = 150,
        #                   h = c(1.2, 1.2)) &
        #   # 
        #    facet_wrap(vars(Groups), nrow = 4,
        #               labeller = labeller(Age_groups =
        #                                     c(
        #                                       'Infants'='Infants (n=36): 2m-2y',
        #                                       'Child'='Child (n=26): 2y-12y',
        #                                       'Adolescent'='Adolescent (n=20): 12y-18y',
        #                                       'Young'='Young (n=24): 18y-40y',
        #                                       'Middle_aged'='Middle_aged (n=16): 40y-65y',
        #                                       'Older'='Older (n=33): 65y-85y',
        #                                       'Oldest_old'='Oldest_old (n=12): 85y-105y'
        #                                     ))) &
        #  ylab('UMAP_2') & xlab('UMAP_1') & scale_fill_manual(values=cols) &
        #  #scale_x_discrete(limits=c('HI','HC','HY','HO')) &
        #   theme(legend.position="none",
        #         axis.ticks.x=element_blank(),
        #         axis.ticks.y=element_blank(),
        #         plot.title=element_blank(),
        #         strip.text.x = element_text(size = 14, face = 'bold'))
        # 
        # p_umap1 | p_umap2
        
      }) 
    })
    
    #├├ 1- Boxplot ####
    
    output$BoxPlot2 <- renderPlot({ 
      
      if (v2$doPlot == FALSE)
        return() 
      isolate ({ 
        
        my_comp_age <- my_comparisons
        TobePlotted2 <-  input$Condition_boxplot2
        #print(TobePlotted2)
        
        # prepare data 
        meta2 <-  LifeSpan_ALL_MetaData %>%  dplyr::select(LS_L1, LS_L4, Age_groups, sample_id) %>% dplyr::filter(LS_L1 %in% TobePlotted2) %>% data.frame()
        #print(head(meta2))
        #print(dim(meta2))
        
        Boxplot_age2 <- meta2 %>% 
          mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>%
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          group_by(Groups, sample_id, ReCluster) %>%
          summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>%
          
          ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = Groups)) +
          geom_boxplot(outlier.shape = NA) +
          geom_jitter(size = 0.2) +
          theme_bw()  +  #THEME +
          
          ggpubr::stat_compare_means(comparisons = my_comparisons, 
                                     label = "p.signif", 
                                     hide.ns = F, 
                                     vjust = 0.5) + 
          
          theme(legend.position = "none", 
                strip.text = element_text(size = 11, face = 'bold')) +
          facet_wrap(.~ReCluster, scales = "free_y",  as.numeric(input$nb_raws)) + 
          
          scale_fill_manual(values=cols) + 
          theme(axis.text.y=element_text(size=16), 
                axis.text.x=element_text(size=16, angle=90),
                axis.title.x = element_text(face="bold", size=18),
                axis.title.y = element_text(face="bold", size=18)) + #    ylab('% PBMC') + xlab('Age groups') #    ylab('% PBMC') + xlab('Age groups')
          ylab(paste0('% in ', unique(meta2$LS_L1))) + xlab(' ')
        
        Boxplot_age2
      }) 
    })
    
    #├├ 2- Corplots   ####
    output$CorPlot2 <- renderPlot({ 
      
      if (v4$doPlot == FALSE)
        return() 
      isolate ({ 
        
        TobePlotted3 <-  input$condition_corplot2
        #print(TobePlotted3)
        
        # prepare data 
        meta3 <-  LifeSpan_ALL_MetaData %>%  dplyr::select(LS_L1, LS_L4, Age_groups, sample_id, Age_in_yrs) %>% dplyr::filter(LS_L1 %in% TobePlotted3) %>% data.frame()
        print(head(meta3))
        #meta <-  data.frame(dplyr::select(LifeSpan_ALL_MetaData, 'clusters'=TobePlotted2, Age_groups, sample_id, Age_in_yrs))
        
        #├├├ ALL age groups   ####
        p_corr_all <- meta3 %>%
          mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>%
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
          
          #filter(Groups %in% c("HI")) %>% 
          summarise(n = n()) %>% #, Age_in_yrs = first(Age_in_yrs), Gender = first(Gender)) %>% #, Set = first(Set)
          #summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>%
          
          
          #filter(Groups %in% age_groups) %>% 
          
          ggplot(aes(x = Age_in_yrs, y = freq, fill=ReCluster)) +
          geom_point(aes(shape = Groups, color=ReCluster)) +
          geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
          #geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color=ReCluster)) +
          scale_fill_manual(values=cols) + 
          scale_color_manual(values = cols)+
          ggpubr::stat_cor() +
          theme_bw() +
          theme(legend.position = "none", 
                strip.text = element_text(size = 11, face ='bold')) +
          facet_wrap(.~ReCluster, scales = "free_y", nrow = as.numeric(input$nb_rows2)) +
          theme(axis.text.y=element_text(size=12), 
                axis.text.x=element_text(size=12),
                axis.title.x = element_text(size=0),
                axis.title.y = element_text(face="bold", size=14), 
                strip.text.x = element_text(size = 11, face ='bold'),
                plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
          ylab(paste0('% in ', unique(meta3$LS_L1))) + 
          #xlab('Age (months)') +
          ggtitle('All age groups')
        
        
        
        #├├├ INFANTS ONLY   ####
        p_corr_inf <- meta3 %>%
          mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>%
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          group_by(Groups, sample_id, Age_in_yrs, ReCluster) %>%
          
          #filter(Groups %in% c("HI")) %>% 
          summarise(n = n()) %>% #, Age_in_yrs = first(Age_in_yrs), Gender = first(Gender)) %>% #, Set = first(Set)
          #summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>%
          
          filter(Groups %in% c('Infants')) %>% 
          
          ggplot(aes(x = Age_in_yrs, y = freq, fill=ReCluster)) +
          geom_point(aes(shape = Groups, color=ReCluster)) +
          geom_smooth(method = "lm", aes(color=ReCluster)) + #, color = c('#f37421ff','#ffdeadff')
          #geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color=ReCluster)) +
          scale_fill_manual(values=cols) + 
          scale_color_manual(values = cols)+
          ggpubr::stat_cor() +
          theme_bw() +
          theme(legend.position = "none", 
                strip.text = element_text(size = 11, face ='bold')) +
          facet_wrap(.~ReCluster, scales = "free_y", nrow = as.numeric(input$nb_rows2)) +
          theme(axis.text.y=element_text(size=12), 
                axis.text.x=element_text(size=12),
                axis.title.x = element_text(face="bold", size=14),
                axis.title.y = element_text(face="bold", size=14), 
                strip.text.x = element_text(size = 11, face ='bold'), 
                plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
          ylab(paste0('% in ', unique(meta3$LS_L1))) + 
          xlab('Age (months)') +
          ggtitle('Infants only') 
        
        
        gg_all_pbmc2 <- ggarrange(p_corr_all, p_corr_inf, ncol = 1)
        gg_all_pbmc2
        
        
      })    
    })
    
    
    
    #├├ 3- Barplot ####
    
    output$BarPlot2 <- renderPlot({ 
      
      if (v6$doPlot == FALSE)
        return() 
      isolate ({ 
        
        TobePlotted6 <-  input$condition_corplot2
        #print(TobePlotted6)
        
        # prepare data 
        LifeSpan_ALL_MetaData %>%  dplyr::select(LS_L1, LS_L4, Age_groups, sample_id, Age_in_yrs) %>% 
          dplyr::filter(LS_L1 %in% TobePlotted6) %>% data.frame() -> meta6  
        
        # ├├├  Individuals  ####    
        BP2 <- meta6 %>% 
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>%
          group_by(Groups, sample_id, ReCluster) %>%
          summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>%
          ggplot(aes(x = sample_id, y = freq, fill = ReCluster, group = Groups)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values=cols) + #***
          scale_x_discrete(limits=ordered_ids) + #labels= labels
          theme(axis.text.y=element_text(size=18),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                #axis.text.x=element_text(size=16, angle = 90),
                axis.title.x = element_text(size=18),
                axis.ticks.y=element_blank(),
                axis.title.y = element_text( size=18), #face="bold",
                #panel.border = element_rect(fill=NA, color = 'black', size=1),
                legend.direction = "horizontal", 
                legend.position = 'bottom',
                legend.text.align = 1, 
                plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
          ylab('% in lineage') + 
          xlab('Individuals (n=167; ordered based on their age)') +
          ggtitle('Individuals')
        
        
        #BP2
        
        # ├├├  Age groups  ####
        
        BP_gp <- meta6 %>% 
          mutate(Groups = factor(Age_groups, levels = age_groups)) %>%
          mutate(ReCluster = factor(LS_L4, levels = order_LS_L4)) %>%
          group_by(Groups, ReCluster) %>%
          summarise(n = n()) %>% #, Set = first(Set)
          mutate(freq = n / sum(n) *100) %>%
          ungroup() %>%
          as.data.frame() %>% #head()
          ggplot(aes(x = Groups, y = freq, fill = ReCluster, group = ReCluster)) +
          geom_bar(stat = "identity", color = "black") +
          scale_fill_manual(values=cols) + #***
          scale_x_discrete(limits=age_groups) + #labels= labels
          theme(axis.text.y=element_text(size=18), 
                axis.text.x=element_text(size=18, angle = 0),
                axis.title.x = element_text(size=0),
                axis.ticks.x=element_blank(),
                axis.title.y = element_text(size=0),
                legend.position = "none",
                plot.title = element_text(face='bold', color = 'black', size = 20, hjust = 0.5)) +
          ggtitle('Age groups')
        
        
        #ylab('% in lineage') #+ xlab('age groups')
        
        gg_all <- ggarrange(BP2, BP_gp, widths = c(2,1) )
        gg_all
        #BP2|BP_gp
        
        
        
      })    
    })
    
    
    #├ Demographics ####
    
    output$Demographics <- renderPlot({ 
      
      if (v7$doPlot == FALSE)
        return() 
      isolate ({ 
        
        
        Pheno <- data.frame(MetaData[['pheno']])
        #print(head(Pheno))
        TobePlotted7 <-  input$condition_demographics
        
        # prepare data 
        meta <-  data.frame(Pheno %>% dplyr::select('demo'=TobePlotted7, Age_groups))
        #print(head(meta))
        
        #meta <-  data.frame(dplyr::select(MetaData, 'clusters'=TobePlotted, Groups, sample_id))
        
        Pheno2 <- data.frame(meta %>% group_by(Age_groups, demo) %>% summarise(n = n()) %>%   mutate(freq = n / sum(n) *100))
        #print(Pheno2)
        
        #my_col <- c('F'= '#a8ddb5', M='#feb24c')
        Pheno2$Groups = factor(Pheno2$Age_groups, 
                               levels=age_groups)
        
        
        pPC <- Pheno2 %>% 
          ggplot(aes(x = '', y = freq, fill = demo, group = demo)) +
          geom_bar(stat="identity", width=1) +
          
          # --- NEW: Add counts in the middle of each slice ---
          geom_text(aes(label = n),
                    position = position_stack(vjust = 0.5),
                    size = 4, color = "black", fontface = "bold") +
          coord_polar("y", start=0) +
          facet_wrap(.~Groups, 
                     scales = "free_y", 
                     nrow = 2,
                     labeller = labeller(Age_groups =
                                           c(
                                             'Infants'='Infants (n=36): 2m-2y',
                                             'Child'='Child (n=26): 2y-12y',
                                             'Adolescent'='Adolescent (n=20): 12y-18y',
                                             'Young'='Young (n=24): 18y-40y',
                                             'Middle_aged'='Middle_aged (n=16): 40y-65y',
                                             'Older'='Older (n=33): 65y-85y',
                                             'Oldest_old'='Oldest_old (n=12): 85y-105y'
                                           ))) +
          #scale_fill_manual(values = cols) +
          theme_classic() + 
          theme_void() +
          theme( #legend.position="none",
            # The new stuff
            strip.text = element_text(size = 15, face = "bold")) #+
        #scale_fill_manual(values=my_col) + #***
        # remove background, grid, numeric labels
        pPC
        
        
      }) 
    })
    
    
    
  }) ##observeEvent
}#server 



shinyApp(ui = ui, server = server)


