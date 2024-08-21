library(shiny)
library(dplyr)
library(Seurat)
library(ggplot2)
library(DT)
library(shinythemes)
library(xlsx)
library(BiocManager)
library(shinycssloaders)
library(shinybusy)
library(svglite)
library(patchwork)
library(bslib)
library(babyplots)
library(shinyjs)

ui <-navbarPage(
  theme = bs_theme(bootswatch = "flatly"),title = "Zebrafish Spinal Cord Regeneration Atlas",
  tabPanel("Get Started",
           tags$head(includeHTML("google-analytics.html")),
           tags$img(src="welcome image.png", height="100%", width="100%", alt="MHM Lab database logo",deleteFile=F),
           h3("Introduction"),
           hr(),
           p(style = "text-align: justify",
             "Welcome to the Single Nuclei Atlas of Zebrafish Spinal Cord (SC) Regeneration.
To achieve a comprehensive understanding of the pro-regenerative cell identities that direct innate SC repair,
we performed Single Nuclear RNA Sequencing (snRNA-seq) at 0-, 1-, 3-, and 6-weeks post-injury (wpi).
Nuclei from two biological replicates were sequenced using the 10x Genomics platform (3’ v3.1 chemistry)
and obtained approximately 58,973 nuclei for downstream analysis using the Seurat package.
Nuclei were filtered to avoid doublets, empty droplets and high ambient RNA contamination.
For cell type identification, we cross-compared the differentially expressed (DE) top markers of each cluster with
a custom-made database consisting of markers of different cell types,
commonly found in the brain/spinal cord of vertebrates, as reported in existing literature." ),
           h3("Upload data"),
           hr(),
           wellPanel(
             h5(strong("Instructions")),
             p(style= "text-align: justify",em("To start analyzing the snRNA-seq data, you need to first upload the desired dataset. There are three types of datasets available here:

1) Full wildtype dataset that contains 58k cells.
(This data takes around 30 seconds to load.)

2) Wildtype dataset that contains randomly selected 6k cells from all cell types. This dataset will upload quickly (10 seconds) compared to the full wildtype dataset with 58k cells.

3) Neuron subset that contains 16k cells that we identified as neurons in the full dataset. This takes around 15 seconds to upload.

Select the required dataset from the dropdown list and then click the Upload Data button. Once uploaded, the data features will be displayed below. Then, please proceed to cluster analysis.")),
             br(),
             selectInput("scRNA_data_choices", "Select the dataset: ", choices = list(
                                                                                      "Wildtype dataset_Random 6k cells",
                                                                                      "Full wildtype dataset_58k cells",
                                                                                      "Neuron subset"
                                                                                      )),
             actionButton(inputId = "scRNA_data_upload", label="Upload Data"),
             br(),
             p(),
             strong(textOutput("scRNA_data_upload_success")%>% withSpinner(type=5))
           ),
           br(),
           h4(strong(textOutput("data_feature_title"))),
           hr(),
           br(),
           tableOutput("datafeatures")%>% withSpinner(type=5),
           br(),
           hr(),
           p(style= "text-align: center",em(strong("Copyright(C)2023 Mokalled Lab.  This website is powered using Shiny and R."))),
           p(style= "text-align: center",strong("Version: v2.0.0, Last updated: 09-Nov-2023")),
           p(style= "text-align: center",strong(em("Contact: vishnu@wustl.edu")))
           
           
           
  ),
  tabPanel("Analysis",
           
           tabsetPanel(
             tabPanel("UMAP plots",
                      wellPanel(
                        h5(strong("Instructions")),
                        p(style= "text-align: justify",em("Here, you will be able to make 2D and 3D UMAP plots and Cell proportion
                                                          of your selected dataset. You can select the grouping criteria using the 
                                                          options below.")),
                        column(width=3,selectizeInput(inputId = "umap_plots_group", label = strong("Grouping factor : "), choices= NULL ,multiple = F,options = list(create=F, placeholder = "Select a group"))),
                        actionButton(inputId = "umap_plots_make", label="Plot Graphs")
                      ),
                      br(),
                      h4(strong(textOutput("UMAP_cluster_title"))),
                      hr(),
                      
                      hr(),
                      plotOutput("UMAP")%>% withSpinner(type=5),
                      br(),
                      p(),
                      
                      fluidRow(
                        
                        column(width=3,offset = 2 ,downloadButton("Download_umap","Download Plot (.png)")),
                        column(width=3,offset=2, downloadButton("Download_umap2","Download Plot (.pdf)"))),
                      br(),
                      p(),
                      h4(strong(textOutput("cluster_distribution_title"))),
                      hr(),
                      br(),
                      plotOutput("cluster_distribution")%>% withSpinner(type=5),
                      br(),
                      p(),
                      
                      fluidRow(
                        column(width=3, downloadButton("Download_distribution","Download Plot (.png)")),
                        column(width=3,offset=2, downloadButton("Download_distribution2","Download Plot (.pdf)"))),
                      
                      br(),
                      p(),
                      h4(strong(textOutput("UMAP_3D_title"))),
                      hr(),
                      br(),
                      babyplotOutput("UMAP_3D")%>% withSpinner(type=5),
                      
                      br(),
                      p(),

                      
                      ),
             tabPanel("Avg. gene expression",
                      br(),
                      p(),
                      
                        
                                 wellPanel(
                                   h5(strong("Instructions")),
                                   p(style= "text-align: justify",em("Here, you will be able to generate average gene expression
                                                                     of a gene your interest across different clusters. First select the
                                                                     grouping criteria by which you want to see the average expression. Then, select
                                                                     your gene of interest in the box below and then click plot. Default
                                                                     RNA Assay is used for the analysis")),
                                   br(),
                                   selectizeInput(inputId = "avg_exp_group", label = strong("Grouping factor : "), choices= NULL ,multiple = F,options = list(create=F, placeholder = "Select a group")),
                                   selectizeInput(inputId = "gene_avg1", label = strong("Gene name :"), choices= NULL , options = list(create=F, placeholder = "type gene name")),
                                   
                                   fluidRow(column(width = 2, actionButton(inputId = "avg1_plot_graph", label="Plot")),
                                            column(width=3, downloadButton("Download_avg1_plot_png","Download Plot (.png)")),
                                            column(width=3, downloadButton("Download_avg1_plot_svg","Download Plot (.pdf)")),
                                            column(width=3, downloadButton("Dwd_avg_exp","Download avg exp (.csv)"))
                                 )
                                 ),
                      br(),
                                 h4(strong(textOutput("avg1_plot_title"))),
                                 br(),
                                 plotOutput(outputId = "graph_avg1")%>% withSpinner(type=5)

                      ),
             tabPanel("Topmarkers",
                      br(),
                      p(),
                      navlistPanel(
                        tabPanel("Conserved Markers",
                                 wellPanel(
                                   h5(strong("Instructions")),
                                   p(style= "text-align: justify",em("Here, you will be able to generate Top markers of each clusters conserved 
                                                                     across timepoints. These are the markers that are consistently expressed.
                                                                     All markers should be expressed in atleast 10% of cells in that cluster
                                                                     and the difference in average_logfc should be more than or equal to 0.25. 
                                                                     Please select your cluster of interest and then click generate markers. This might
                                                                     take upto 5 minutes. Tippett's method in metap package is used  for combining p-values."
                                                                     )),
                                   
                                   
                                   br(), 
                                   selectizeInput(inputId = "Cons_mrk_group", label = strong("Grouping factor : "), choices= NULL ,multiple = F,options = list(create=F, placeholder = "Select a group")),
                                   br(),
                                   fluidRow(column(width = 3, selectizeInput(inputId = "clusters_cons",label = NULL, choices= NULL , options = list(create=F, placeholder = "select cluster"))),
                                            column(width=3,offset=0, actionButton(inputId = "gnrt_cons_mark", label="Generate")) ,
                                            column(width=3,offset=1, downloadButton("Dwd_cons_mark","Download Conserved markers")))
                                 ),
                                 h5(style=  "color:blue",textOutput("Caution_Cons_marker")),
                                 br(),
                                 h4(strong(textOutput("Cons_marker_title"))),
                                 br(),
                                 dataTableOutput("Cons_marker")%>% withSpinner(type=5)
                                 ),
                        tabPanel("Distinguishing Markers",
                                 wellPanel(
                                   h5(strong("Instructions")),
                                   p(style= "text-align: justify",em("Here, you will be able to generate Top markers of each individual clusters 
                                                                     distinguishing between your choice of other clusters. You can select a single or multiple clusters to distinguish from.
                                                                      All markers should be expressed in atleast 25% of cells in that cluster
                                                                     and the difference in average_logfc should be more than or equal to 0.25. 
                                                                     Please select your cluster of interest and then click generate markers. By default Wilcoxon Rank Sum test is used to identify
                                                                     differentially expressed genes between two groups of cells"
                                   ), em(strong("note: if you don't select any particular distinguishing cluster, it will generate markers distinguishing from all other clusters. This might
                                                take upto 5 minutes."))),
                                   br(), 
                                   selectizeInput(inputId = "DE_mrk_group", label = strong("Grouping factor : "), choices= NULL ,multiple = F,options = list(create=F, placeholder = "Select a group")),
                                   br(),
                                   fluidRow(column(width = 3, selectizeInput(inputId = "clusters_id1",label = strong("Select cluster of Interest"), choices= NULL , options = list(create=F, placeholder = "select cluster"))),
                                            column(width=3,offset=1, selectizeInput(inputId = "clusters_id2",label = strong("Select clusters to distinguish from"),  multiple = T,choices= NULL , options = list(create=F, placeholder = "select clusters")))),
                                   
                                   fluidRow(column(width=3,offset=0, actionButton(inputId = "gnrt_diff_mark", label="Generate")) ,
                                            column(width=3,offset=1, downloadButton("Dwd_diff_mark","Download Differential markers"))),
                                   
                                 ),
                                 
                                 h4(strong(textOutput("Diff_marker_title"))),
                                 br(),
                                 h5(style=  "color:blue",textOutput("Caution_Diff_marker")),
                                 br(),
                                 dataTableOutput("Diff_marker")%>% withSpinner(type=5)
                                 ),
                        tabPanel("Find All Markers",
                                 wellPanel(
                                 h5(strong("Instructions")),
                                 p(style= "text-align: justify",em("Here, you will be able to generate differential markers of all clusters compared to every other cluster in a single file.
                                                                      All markers should be expressed in atleast 25% of cells in that cluster
                                                                     and the difference in average_logfc should be more than or equal to 0.25. 
                                                                     Select the respective data type and click generate all markers. By default Wilcoxon Rank Sum test is used to identify
                                                                     differentially expressed genes between two groups of cells"
                                 ), em(strong("note: this might take several minutes depending on the size of the dataset and total number of clusters"))),
                                 br(),
                                 selectizeInput(inputId = "AllDE_mrk_group", label = strong("Grouping factor : "), choices= NULL ,multiple = F,options = list(create=F, placeholder = "Select a group")),
                                 br(),
                                 fluidRow(
                                          column(width=3,offset=0, actionButton(inputId = "gnrt_all_marker", label="Generate All markers")),
                                          column(width=3,offset=1, downloadButton("Dwd_all_marker","Download All Markers"))
                                          ), 
                                 
                                 ),
                                 h4(strong(textOutput("All_marker_title"))),
                                 br(),
                                 h5(style=  "color:red",textOutput("Caution_All_marker")),
                                 br(),
                                 dataTableOutput("All_marker")%>% withSpinner(type=5)
                                 
                                 )
                      ),
                      ),
             tabPanel("Visualize Markers",
                      br(),
                      p(),
                      navlistPanel(
                        tabPanel("Dot Plot",
                                 wellPanel(
                                   h5(strong("Instructions")),
                                   p(style= "text-align: justify",em("Here, you will be able to generate Dotplot for your genes of interest.
                                                                     You can choose the split option if you wish make dotplot split by timepoint. 
                                                                      
                                                                     Select the condition and click Plot. By default, RNA Assay is used to make this plot."
                                   )),
                                   br(),
                                   selectizeInput(inputId = "Dotplot_group", label = strong("Grouping factor : "), choices= NULL ,multiple = F,options = list(create=F, placeholder = "Select a group")),
                                   br(),
                                   fluidRow(column(width=3,offset=0 ,selectInput("Dotplot_choices", label = strong("Split Timepoint? "), choices = list("No","Yes"))),
                                            column(width=5,offset=0, selectizeInput(inputId = "Dotplot_genes",label = strong("Type your genes"),  multiple = T,choices= NULL , options = list(create=F, placeholder = "Enter genes"))),
                                            ),
                                   br(),
                                   fluidRow(column(width=3,offset=0, actionButton(inputId = "make_Dotplot", label="Plot")),
                                            column(width=3,offset=0, downloadButton("Dwd_dotplot_png","Download Plot (.png)")),
                                            column(width=3,offset=0, downloadButton("Dwd_dotplot_svg","Download Plot (.pdf)"))
                                   ), 
                                   
                                 ),
                                 h4(strong(textOutput("Dotplot_title"))),
                                 br(),
                                 plotOutput(outputId = "Dotplot_out")%>% withSpinner(type=5),
                                 h5(style=  "color:blue","Note: If the plot legends are not visible, please download the plot")
                                 ),
                        tabPanel("Feature Plot",
                                 wellPanel(
                                   h5(strong("Instructions")),
                                   p(style= "text-align: justify",em("Here, you will be able to generate Feature plot for your gene of interest.
                                                                    And you can generate Feature plot for the marker with all timepoints merged or split. 
                                                                      
                                                                     Select the respective data type and click Plot. By default, RNA Assay is used to make this plot."
                                   )),
                                   br(),
                                  
                                   fluidRow(column(width=3,offset=0 ,selectInput("Feature_choices", label = strong("Split the featureplot? "), choices = list("No","Yes"))),
                                            column(width=5,offset=0, selectizeInput(inputId = "Feature_gene",label = strong("Enter your gene"),  multiple = F,choices= NULL , options = list(create=F, placeholder = "Enter gene"))),
                                   ),
                                   br(),
                                   fluidRow(column(width=3,offset=0, actionButton(inputId = "make_Feature", label="Plot")),
                                            column(width=3,offset=0, downloadButton("Dwd_feature_png","Download Plot (.png)")),
                                            column(width=3,offset=0, downloadButton("Dwd_feature_svg","Download Plot (.pdf)"))
                                   ), 
                                   
                                 ),
                                 h4(strong(textOutput("Feature_title"))),
                                 br(),
                                 plotOutput(outputId = "Feature_out")%>% withSpinner(type=5),
                                 
                                 ),
                        tabPanel("Violin Plot",
                                 wellPanel(
                                   h5(strong("Instructions")),
                                   p(style= "text-align: justify",em("Here, you will be able to generate Violin plot for your gene of interest.
                                                                     And you can generate Feature plot for the marker with all timepoints merged or split. 
                                                                      
                                                                     Select the respective data type and click Plot. By default, RNA Assay is used to make this plot."
                                   )),
                                   br(),
                                   selectizeInput(inputId = "Violin_group", label = strong("Grouping factor : "), choices= NULL ,multiple = F,options = list(create=F, placeholder = "Select a group")),
                                   br(),
                                   fluidRow(column(width=3,offset=0 ,selectInput("Violin_choices", label = strong("Split the plot? "), choices = list("No","Yes"))),
                                            column(width=5,offset=0, selectizeInput(inputId = "Violin_gene",label = strong("Type your gene"),  multiple = F,choices= NULL , options = list(create=F, placeholder = "Enter gene"))),
                                   ),
                                   br(),
                                   fluidRow(column(width=3,offset=0, actionButton(inputId = "make_Violin", label="Plot")),
                                            column(width=3,offset=0, downloadButton("Dwd_Violin_png","Download Plot (.png)")),
                                            column(width=3,offset=0, downloadButton("Dwd_Violin_svg","Download Plot (.pdf)"))
                                   ), 
                                   
                                 ),
                                 h4(strong(textOutput("Violin_title"))),
                                 br(),
                                 plotOutput(outputId = "Violin_out")%>% withSpinner(type=5),
                                 h5(style=  "color:blue","Note: If the plot legends are not visible, please download the plot.")
                                 )
                      )
                      )
           )
           )

)