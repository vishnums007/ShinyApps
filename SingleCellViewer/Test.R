library(Seurat)
library(ggplot2)
zebra<-readRDS(file="ekab_complete_labelled_6k.rds")
colnames(zebra@meta.data)
zebra@meta.data<- zebra@meta.data[,-11]
colnames(zebra@meta.data)
colnames(zebra@meta.data)[18]<-"Coarse.Cluster"
colnames(zebra@meta.data)[19]<-"Fine.Cluster"
# colnames(zebra@meta.data)[22]<-"iNeuron.fine"
colnames(zebra@meta.data)
Idents(zebra)<-zebra$Neuron.Coarse.Cluster.ID
saveRDS(zebra, file="ekab_Neuronal subset_complete_0.6_labelled.rds")

DotPlot(zebra,features="sox2", group.by = "input$Dotplot_group", assay = "Decontx") + RotatedAxis()  
avg<-AverageExpression(zebra, return.seurat = F, verbose = F, assays = "Decontx")
avg
Avg_expression<-as.numeric(as.vector(cluster.averages$RNA[index,]))