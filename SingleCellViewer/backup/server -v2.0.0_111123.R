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
library(scales)



server<- function(input, output, session){
  options(shiny.usecairo=T)
  
  

  
  #Upload scRNA-seq data from user input choices
  scRNAdata_choice<-eventReactive(input$scRNA_data_upload,{input$scRNA_data_choices})
  
  data<-eventReactive(input$scRNA_data_upload,{
    if (scRNAdata_choice()=="Full wildtype dataset_58k cells"){
      zebra<-readRDS(file="ekab_uninj136_complete_res0.4_labelled.rds")
    }else if(scRNAdata_choice()=="Neuron subset"){
      zebra<-readRDS(file = "ekab_Neuronal subset_complete_0.6_labelled.rds")
    }else if(scRNAdata_choice()=="Wildtype dataset_Random 6k cells"){
      zebra<-readRDS(file="ekab_complete_labelled_6k.rds")
    }
  })
  
  observeEvent(input$scRNA_data_upload,{
    cat(file=stderr(),paste0("Uploaded data : ", scRNAdata_choice(), "\n"))
  })
  
  
  output$scRNA_data_upload_success<-renderText({
    paste(scRNAdata_choice(),"has been successfully uploaded! ",sep = " ")
  })
  
  
  #Extracting metadata 
  
  
  meta<-eventReactive(input$scRNA_data_upload,{
    meta<-data()@meta.data
    #print(meta)
  })
  
  
   observeEvent(input$scRNA_data_upload,{
    meta_cols<-colnames(meta())
    gp<-meta_cols[grep("\\.ID",meta_cols)]
    gpo<-append(gp,c("seurat_clusters","Timepoint_rep","replicate"), after = 0)
    updateSelectizeInput(session,inputId = "umap_plots_group",choices = gpo, server=T, selected = gpo[length(gpo)])
    gp1<-append(gp,c("seurat_clusters","Timepoint"), after = 0)
    updateSelectizeInput(session,inputId = "avg_exp_group",choices = gp1, server=T, selected = gp1[length(gp1)])
    gp2<-append(gp,c("seurat_clusters"), after = 0)
    updateSelectizeInput(session,inputId = "Cons_mrk_group",choices = gp2, server=T, selected = gp2[length(gp2)])
    updateSelectizeInput(session,inputId = "DE_mrk_group",choices = gp2, server=T, selected = gp2[length(gp2)])
    updateSelectizeInput(session,inputId = "AllDE_mrk_group",choices = gp2, server=T, selected = gp2[length(gp2)])
    updateSelectizeInput(session,inputId = "Dotplot_group",choices = gp2, server=T, selected = gp2[length(gp2)])
    updateSelectizeInput(session,inputId = "Violin_group",choices = gp2, server=T, selected = gp2[length(gp2)])
  })
  
  # Generating the data features in a table format
  
  output$data_feature_title<-renderText({
    paste("Features of",scRNAdata_choice(),sep = " ")
  })
  
  output$datafeatures<-renderTable({
    features<-matrix(nrow = 4, ncol=2)
    features[1,]<-c("No. of Cells", length(row.names(meta())))
    features[2,]<-c("Avg. no. of genes/nucleus",mean(meta()[,3]))
    features[3,]<-c("Avg. no. of UMIs/nucleus",mean(meta()[,2]))
    features[4,]<-c("Avg. Mitochondrial-%/nucleus",mean(meta()[,4]))
    colnames(features)<- c("Feature",scRNAdata_choice() )
    return(features)
    
    
  })
  
 # 
  
  
  #Identify the number of Identities to define plot width and height
  n_idents<- reactive(length(levels(Idents(data()))))
  
  

  
 
  
  output$UMAP_cluster_title<-renderText({
    paste("UMAP Plot of",scRNAdata_choice(),sep = " ")
  })
  
## creating umap
  umap_plot<-eventReactive(input$umap_plots_make,{
    #print(input$umap_plots_group)
    DimPlot(data(), reduction = "umap",label=F, repel=T, split.by = "Timepoint",label.size = 5, pt.size = 1, group.by = input$umap_plots_group)
  })    

    
    output$UMAP<-renderPlot({
      umap_plot()
    },res=100)
    
    
    
    output$Download_umap<-downloadHandler(
      filename=function(){
        paste("Umap-plot_", scRNAdata_choice(), "_grouped_",input$umap_plots_group,".png", sep="")
      },
      content=function(file){
        ggsave(file, plot= umap_plot(), device= "png", width = 25, height = 6 )
      }
    )
    
    output$Download_umap2<-downloadHandler(
      filename=function(){
        paste("Umap-plot_", scRNAdata_choice(),"_grouped_",input$umap_plots_group, ".pdf", sep="")
      },
      content=function(file){
        cairo_pdf(filename = file, width = 25, height = 6, pointsize = 12, family = "sans", fallback_resolution = 300)
        print(DimPlot(data(), reduction = "umap",label=F, repel=T, split.by = "Timepoint",label.size = 5, pt.size = 1))
        dev.off()
      },
      contentType = "application/pdf"
    )

  
  
  
  output$cluster_distribution_title<-renderText({
    paste("Cluster distribution of",scRNAdata_choice(),sep = " ")
  })
  
  distribution_plot<- eventReactive(input$umap_plots_make,{
    #Creating cluster distribution plot
    x<-which(colnames(meta())==input$umap_plots_group)
    #print(input$umap_plots_group)
    #print(x)
    table_1<-as.data.frame(table(Clusters=meta()[,x], Conditions=data()$"Timepoint"))
    
    distribution_plot<-ggplot(table_1, aes(fill=Conditions, y=Freq, x=Clusters)) + 
                                  geom_bar(position="fill", stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 14, face = "bold"),
                                                                                   axis.text.y = element_text(size = 12, face = "bold"),
                                                                                   legend.text = element_text(size = 14,face = "bold"),
                                                                           legend.key.size = unit(0.5, "in"))
    
    return(distribution_plot)
    
  })
  


    
   

    
    output$cluster_distribution<-renderPlot({
      distribution_plot()
    })
    
    output$Download_distribution<-downloadHandler(
      filename=function(){
        paste("distribution-plot_", scRNAdata_choice(), "_grouped_",input$umap_plots_group,".png", sep="")
      },
      content=function(file){
        ggsave(file, plot= distribution_plot(), device= "png", width = 10, height = 6 )
      }
    )
    
    output$Download_distribution2<-downloadHandler(
      filename=function(){
        paste("distribution-plot_", scRNAdata_choice(), "_grouped_",input$umap_plots_group,".pdf", sep="")
      },
      content=function(file){
        cairo_pdf(filename = file, width = 10, height = 6, pointsize = 12, family = "sans", fallback_resolution = 300)
        print(distribution_plot())
        dev.off()
      },
      contentType = "application/pdf"
    )
    
    output$cluster_distribution_title<-renderText({
      paste("Cluster distribution of",scRNAdata_choice(),sep = " ")
    })
    
  
  

  
  #Creating 3D UMAP
  
  
  output$UMAP_3D_title<-renderText({
    paste("3D_UMAP Plot of",scRNAdata_choice(),sep = " ")
  })
  
  umap_3d<-eventReactive(input$umap_plots_make,{
    x<-which(colnames(meta())==input$umap_plots_group)
    #print(input$umap_plots_group)
    #print(x)
    data_umap3d <- data()@reductions$UMAP_3D@cell.embeddings
    clusters<- meta()[,x]
    n_color<-length(unique(meta()[,x]))
    
    print(  pointCloud(data_umap3d, "categories", clusters, turntable = T, showUI = T, rotationRate = .01,
                 xScale = 5, yScale = 5,zScale = 5, size = 2, showAxes = T, colorScale = "custom",customColorScale = hue_pal()(n_color)))
    
    #return(ddd)
    
  })

  output$UMAP_3D<- renderBabyplot(umap_3d())
  
  #Creating cluster averages and plotting data
  
  cluster.averages<- eventReactive(input$avg1_plot_graph,{
                          print("avg_Exp")
                          print(input$avg_exp_group)
                          cv<-AverageExpression(data(), return.seurat = F, verbose = F, assays = "Decontx", group.by = input$avg_exp_group)
                          return(cv)
                          
                        })
  
  genelist<-eventReactive(input$avg1_plot_graph,{
              cv<- cluster.averages()
              gl<-rownames(cv$Decontx)
              return(gl)
            })
  
  observeEvent(input$scRNA_data_upload,{
    updateSelectizeInput(session, inputId = "gene_avg1", selected = genelist()[5812] ,
                         choices = genelist(),server= T)
    
  })

  
  

    
    user_input_avg1<-eventReactive(input$avg1_plot_graph, {input$gene_avg1})
    
    observeEvent(input$avg1_plot_graph,{
      cat(file=stderr(),paste0("Plotted average expression of across clusters of  ", input$gene_avg1,"_grouped_",input$avg_exp_group, "\n"))
    })
    
    output$avg1_plot_title<-renderText({
      paste(" Avg gene expression of", user_input_avg1(),sep = " ")
    })
    
    graph_data_avg1<-eventReactive(input$avg1_plot_graph,{
      cluster.averages<- cluster.averages()
      index<- which(genelist()==user_input_avg1())
      Avg_expression<-as.character(as.vector(cluster.averages$Decontx[index,]))
      clusters<-colnames(cluster.averages$Decontx)
      graph_data_clus<-reactive(data.frame(clusters, Avg_expression))
      ggplot(graph_data_clus(), aes(x=clusters, y=Avg_expression, fill= clusters)) + 
        geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10, face = "bold"))
    })
    
    output$graph_avg1<-renderPlot(graph_data_avg1())
    output$Download_avg1_plot_png<-downloadHandler(
      filename=function(){
        paste("Average_expression_clusters_", user_input_avg1(),"_grouped_",input$avg_exp_group, ".png", sep="")
      },
      content=function(file){
        ggsave(file, plot= graph_data_avg1(), device= "png", width = 10, height = 6 )
      }
    )
    
    output$Download_avg1_plot_svg<-downloadHandler(
      filename=function(){
        paste("Average_expression_", user_input_avg1(),"_grouped_",input$avg_exp_group, ".pdf", sep="")
      },
      content=function(file){
        cairo_pdf(filename = file, width = 25, height = 6, pointsize = 12, family = "sans", fallback_resolution = 300)
        print(graph_data_avg1())
        dev.off()
      },
      contentType = "application/pdf"
    )
    
    output$Dwd_avg_exp<-downloadHandler(
      filename=function(){
        paste("Avg gene exp_", user_input_avg1(),"_grouped_",input$avg_exp_group ,".csv", sep="")
      },
      content=function(file){
        write.csv(cluster.averages$Decontx,file)
      }
    )
  
  
  

  
  # Conserved markers between Timepoints
  
  
    observeEvent(input$Cons_mrk_group,{
      if (!is.null(input$Cons_mrk_group) && input$Cons_mrk_group != "") {
        #print("Conserved markers")
        x<-which(colnames(meta())==input$Cons_mrk_group)
        #print(x)
        zebra<-data()
        met<-meta()
        Idents(zebra)<-met[,x]
        cluster_names<-as.character(levels(Idents(zebra)))
        updateSelectizeInput(session, inputId = "clusters_cons", selected = cluster_names[1] ,choices = cluster_names,server= T)
      }
      
      top_cons_marker<- eventReactive(input$gnrt_cons_mark,{
        top_marker<-FindConservedMarkers(zebra, ident.1=input$clusters_cons,min.pct = .1, grouping.var = "Timepoint",assay = "Decontx")
      })
      output$Cons_marker<-DT::renderDataTable(
        top_cons_marker(),option=list(pageLength=10),escape=F
      )

    })
    

    consmarker_title<-eventReactive(input$gnrt_cons_mark,{input$clusters_cons})
    


    observeEvent(input$gnrt_cons_mark,{
      cat(file=stderr(),paste0("Generated conserved markers of  ", input$clusters_cons," group_by ", input$Cons_mrk_group, "\n"))
 
    })
    
    output$Caution_Cons_marker<-renderText(
      paste0("Note: You are calculating conserver markers of the cluster ", input$clusters_cons ," against different timepoints and this might take upto 5 mintutes" )
    )

    
    output$Cons_marker_title<-renderText(
      paste("Top markers of Cluster ",consmarker_title(),sep = " ")
    )
    

    
    output$Dwd_cons_mark<-downloadHandler(
      filename=function(){
        paste("Conserved markers of_", input$clusters_cons, " group_by ", input$Cons_mrk_group,".csv", sep="")
      },
      content=function(file){
        write.csv(top_cons_marker(),file)
      }
    )


  

  
  #Differential markers
  
  observeEvent(input$DE_mrk_group,{
    
    if (!is.null(input$DE_mrk_group) && input$DE_mrk_group != "") {
      #print("Diff exp")
      #print(input$DE_mrk_group)
      x<-which(colnames(meta())==input$DE_mrk_group)
      zebra<-data()
      met<-meta()
      Idents(zebra)<-met[,x]
      cluster_names<-as.character(levels(Idents(zebra)))
      observe({updateSelectizeInput(session, inputId = "clusters_id1", selected = cluster_names[1] ,choices = cluster_names,server= T)})
      observe({updateSelectizeInput(session, inputId = "clusters_id2", selected = NULL ,choices = cluster_names,server= T)})
    }
    
    top_diff_marker<- eventReactive(input$gnrt_diff_mark,{
      diff_marker<-FindMarkers(zebra, ident.1=input$clusters_id1,ident.2= input$clusters_id2,min.pct = .25)
    })
    
    output$Diff_marker<-DT::renderDataTable(
      top_diff_marker(),option=list(pageLength=10),escape=F
    )
    
  })
  
  
  diffmarker_title<-eventReactive(input$gnrt_diff_mark,{
    if (is.null(input$clusters_id2)==T){
      name<- paste("Distinguishing marker of", input$clusters_id1, "vs all", sep = " ")
    }else{
      name<- paste("Distinguishing marker of", input$clusters_id1, "vs",input$clusters_id2, sep = " " )
    }
  })
  
  
  
  observeEvent(input$gnrt_diff_mark,{
    cat(file=stderr(),paste0("Generated ", diffmarker_title()," group_by ", input$DE_mrk_group, "\n"))
  })
  
  
  output$Caution_Diff_marker<-renderText(
    paste0("Note: If you are generating DE markers against all other clusters, it might take upto 5 minutes" )
  )
  
  
  output$Diff_marker_title<-renderText(
    diffmarker_title()
  )
  
  output$Dwd_diff_mark<-downloadHandler(
    filename=function(){
      paste(diffmarker_title(), " group_by ", input$DE_mrk_group,".csv", sep="")
    },
    content=function(file){
      write.csv(top_diff_marker(),file)
    }
  )
  


  
  
  #Find All markers
  
  observeEvent(input$AllDE_mrk_group,{
    if (!is.null(input$AllDE_mrk_group) && input$AllDE_mrk_group != "") {
      x<-which(colnames(meta())==input$AllDE_mrk_group)
      zebra<-data()
      met<-meta()
      Idents(zebra)<-met[,x]
      
    }
    all_markers<- eventReactive(input$gnrt_all_marker,{FindAllMarkers(zebra, min.pct = 0.25)})
    output$All_marker<-DT::renderDataTable(
      all_markers(),option=list(pageLength=10),escape=F
    )
  })
    

    
    allmarker_title<-eventReactive(input$gnrt_all_marker,{
      paste("All markers of",scRNAdata_choice(), sep=" ")})
    
    output$All_marker_title<-renderText(
      allmarker_title()
    )
    
    
    
    
    observeEvent(input$gnrt_all_marker,{
      cat(file=stderr(),paste0("Generated all markers"," group_by ", input$AllDE_mrk_group,"\n"))
    })
    
    output$Caution_All_marker<-renderText(
      paste0("Note: This might take several minutes depending on the number of clusters !!!" )
    )
    
    

    
    output$Dwd_all_marker<-downloadHandler(
      filename=function(){
        paste(allmarker_title(), " group_by ", input$AllDE_mrk_group,".csv", sep="")
      },
      content=function(file){
        write.csv(all_markers(),file)
      }
    )

  

  
  # Make Dotplots
  
  genelist<- eventReactive(input$scRNA_data_upload,{
    genes<- rownames(data()@assays$SCT@data)
  })
  
  
    
    observe({updateSelectizeInput(session, inputId = "Dotplot_genes", selected = genelist()[22668] ,choices = genelist(),server= T)})
    
    dotplot_title<-eventReactive(input$make_Dotplot,{
      paste0("Dotplot")})
    
    observeEvent(input$make_Dotplot,{
      cat(file=stderr(),paste0("Generated dot plot for  ", input$Dotplot_genes," group_by ", input$Dotplot_group, "\n"))
    })
    
    output$Dotplot_title<-renderText(
      dotplot_title()
    )
    
    dotplot<-  eventReactive(input$make_Dotplot,{
      if (input$Dotplot_choices =="Yes"){
        DotPlot(data(),features=rev(unique(input$Dotplot_genes)), dot.scale = 10, split.by = "Timepoint", group.by = input$Dotplot_group,cols = c("blue","blue","blue","blue"),assay = "Decontx")
      }else{
        DotPlot(data(),features=rev(unique(input$Dotplot_genes)), group.by = input$Dotplot_group, assay = "Decontx") + RotatedAxis()  
      }
    })
    
    #defining the width and height of the dotplots based on the input from the user
    width_dot<- eventReactive(input$make_Dotplot,{
      genes_n<- length(unique(input$Dotplot_genes))
      if(genes_n<5){
        w= 6
      }else{
        w=genes_n*2
      }
      return(w)
    })
    
    height_dot<- eventReactive(input$make_Dotplot,{
      if(input$Dotplot_choices=="Yes"){
        h= n_idents()*2
      }else{
        h=n_idents()*1.1
      }
      return(h)
    })
    
    
    output$Dotplot_out<-renderPlot({
      dotplot()
    }, res=80)
    
    
    output$Dwd_dotplot_png<-downloadHandler(
      filename=function(){
        paste("Dot plot of ", scRNAdata_choice()," group_by ", input$Dotplot_group,".png", sep="")
      },
      content=function(file){
        ggsave(file, plot= dotplot(), device= "png", width = width_dot(), height = height_dot(),limitsize = FALSE)
      }
    )
    
    output$Dwd_dotplot_svg<-downloadHandler(
      filename=function(){
        paste("Dot plot of ", scRNAdata_choice(), " group_by ", input$Dotplot_group,".pdf", sep="")
      },
      content=function(file){
        cairo_pdf(filename = file, width = width_dot(), height = height_dot(), pointsize = 12, family = "sans", fallback_resolution = 300)
        print(dotplot())
        dev.off()
      },
      contentType = "application/pdf"
    )
    

  
  # Make feature plots
  observe({updateSelectizeInput(session, inputId = "Feature_gene", selected = genelist()[22668] ,choices = genelist(),server= T)})
  feature_choice<- eventReactive(input$make_Feature,{input$Feature_choices})
  

  
  split_choice<- reactive(
    if (feature_choice()=="No"){
      split_choice = NULL
    }else{
      split_choice = "Timepoint"
    }
  )
  
  width_choice<- reactive(
    if (feature_choice()=="Yes"){
      width_choice =26
    }else{
      width_choice =8
    }
  )
  
  
  featureplot<- eventReactive(input$make_Feature, {
    FeaturePlot(data(), features = input$Feature_gene, split.by = split_choice(),order = T, pt.size = 1, label = F,repel = T )
  })
  
  observeEvent(input$make_Feature,{
    cat(file=stderr(),paste0("Generated feature plot for  ", input$Feature_gene, "\n"))
  })
  
  feature_title<-eventReactive(input$make_Feature,{
    paste("Feature Plot of", input$Feature_gene, sep= " " )})
  
  output$Feature_title<-renderText(
    feature_title()
  )
  
  output$Feature_out<-renderPlot({
    featureplot()
  }, res=80)
  
  output$Dwd_feature_png<-downloadHandler(
    filename=function(){
      paste(feature_title(),".png", sep="")
    },
    content=function(file){
      ggsave(file, plot= featureplot(), device= "png", width = width_choice(), height = 6 )
    }
  )
  
  output$Dwd_feature_svg<-downloadHandler(
    filename=function(){
      paste(feature_title(), ".pdf", sep="")
    },
    content=function(file){
      cairo_pdf(filename = file, width = width_choice(), height = 6, pointsize = 12, family = "sans", fallback_resolution = 300)
      print(featureplot())
      dev.off()
    },
    contentType = "application/pdf"
  )
  
  # Make Violin plot
  
  observe({updateSelectizeInput(session, inputId = "Violin_gene", selected = genelist()[22668] ,choices = genelist(),server= T)})
  violin_choice<- eventReactive(input$make_Violin,{input$Violin_choices})
  

  
  split_violin_choice<- reactive(
    if (violin_choice()=="No"){
      split_violin_choice = NULL
    }else{
      split_violin_choice = "Timepoint"
    }
  )
  
  
  
  violinplot<- eventReactive(input$make_Violin, {
    VlnPlot(data(), features = input$Violin_gene, split.by = split_violin_choice(), group.by = input$Violin_group)
  })
  
  observeEvent(input$make_Violin,{
    cat(file=stderr(),paste0("Generated violin plot for  ", input$Violin_gene," group_by ", input$Violin_group, "\n"))
  })
  
  violin_title<-eventReactive(input$make_Violin,{
    paste("Violin Plot of", input$Violin_gene, sep= " " )})
  
  output$Violin_title<-renderText(
    violin_title()
  )
  
  output$Violin_out<-renderPlot({
    violinplot()
  }, res=80)
  
  
  width_violin<- eventReactive(input$make_Violin,{
    if(input$Dotplot_choices=="Yes"){
      h= n_idents()*4
    }else{
      h=n_idents()*1.1
    }
    return(h)
  })
  
  output$Dwd_Violin_png<-downloadHandler(
    filename=function(){
      paste(violin_title()," group_by ", input$Violin_group,".png", sep="")
    },
    content=function(file){
      ggsave(file, plot= violinplot(), device= "png", width = width_violin(), height = 6 )
    }
  )
  
  output$Dwd_Violin_svg<-downloadHandler(
    filename=function(){
      paste(violin_title(), " group_by ", input$Violin_group,".pdf", sep="")
    },
    content=function(file){
      cairo_pdf(filename = file, width = width_violin(), height = 6, pointsize = 12, family = "sans", fallback_resolution = 300)
      print(violinplot())
      dev.off()
    },
    contentType = "application/pdf"
  )
  
  session$onSessionEnded(function() {
    stopApp()
  }) 
}