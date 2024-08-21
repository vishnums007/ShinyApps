library(kinship2)
library(xlsx)
av<-read.csv("Archive.csv")

plot<- pedigree(id=df$Stock.no., dadid = df$father,momid = df$mother, sex = df$sex)







d = data.frame(
  offspring = c("G2I1", "G2I2", "G2I3", "G3I1", "G3I2", "G3I3", "G3I4", "G4I1", "G4I2", "G4I3", "G4I4", "G5I1", "G5I2", "G5I3"  ),
  parent1   = c("G1I1", "G1I2", "G1I1", "G2I1", "G2I3", "G2I1", "G2I3", "G3I2", "G3I2", "G3I1", "G3I4", "G4I3", "G4I3", "G4I1" ),
  parent2   = c("G1I3", "G1I2", "G1I2", "G2I2", "G2I2", "G2I2", "G2I3", "G3I4", "G3I1", "G3I2", "G3I4", "G4I1", "G4I1", "G4I2" ),
  stringsAsFactors = F
)

d2 = data.frame(from=c(d$parent1,d$parent2), to=rep(d$offspring,2))
g=graph_from_data_frame(d2)
#co=layout.reingold.tilford(g, flip.y=T)
co1 <- layout_as_tree(g, root = which(grepl("G1", V(g)$name)))
#plot(g,layout=co, edge.arrow.size=0.5)
plot(g,layout=co1, edge.arrow.size=0.25)

t<-26222
stl<-av[,2]
fst<-av[,10]
mst<-av[,11]



itarget<-26222
stocklist<-av[,2]
dadstock<-av[,10]
momstock<-av[,11]
df<- matrix(nrow = 15, ncol = 6, dimnames = list(NULL, c("Offstock","offspring","momstock","mom","dadstock","dad")))

i=1
x=1
t_loc<- which(itarget==stocklist)
df[1,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])

ntarget<-df[x,3]
t_loc<- which(ntarget==stocklist)
df[i+1,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])
ntarget<-df[x,5]
t_loc<- which(ntarget==stocklist)
df[i+2,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])

x=2
i=3
ntarget<-df[x,3]
t_loc<- which(ntarget==stocklist)
df[i+1,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])
ntarget<-df[x,5]
t_loc<- which(ntarget==stocklist)
df[i+2,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])

x=3
i=5
ntarget<-df[x,3]
t_loc<- which(ntarget==stocklist)
df[i+1,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])
ntarget<-df[x,5]
t_loc<- which(ntarget==stocklist)
df[i+2,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])

x=4
i=7
ntarget<-df[x,3]
t_loc<- which(ntarget==stocklist)
df[i+1,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])
ntarget<-df[x,5]
t_loc<- which(ntarget==stocklist)
df[i+2,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])



library(igraph)
library(visNetwork)


genealogy<-function(itarget,archive){
av<-archive
stocklist<-av[,2]
dadstock<-av[,10]
momstock<-av[,11]
df<- matrix(nrow = 15, ncol = 6, dimnames = list(NULL, c("Offstock","offspring","momstock","mom","dadstock","dad")))
i=2
x=1
t_loc<- which(itarget==stocklist)
df[1,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])

while (x<8) {
  momtarget<-df[x,3]
  check1<-is.element(momtarget,stocklist)
  if(check1==T){
    t_loc<- which(momtarget==stocklist)
    df[i,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])
    
    dadtarget<-df[x,5]
    if(dadtarget==momtarget){
      x=x+1
      i=i+1
      
    } else{
      check2<-is.element(dadtarget,stocklist)
      if(check2==T){
        t_loc<- which(dadtarget==stocklist)
        df[i+1,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])
        i=i+2
        x=x+1
      }else{
        x=x+1
        i=i+1
      }
    }
    
    
  }else {
    dadtarget<-df[x,5]
    check2<-is.element(dadtarget,stocklist)
    if(check2==T){
      t_loc<- which(dadtarget==stocklist)
      df[i,]<- c(av[t_loc,2],paste(av[t_loc,4],av[t_loc,5], sep = " "),av[t_loc,10], av[t_loc,8],av[t_loc,11], av[t_loc,9])
      i=i+1
      x=x+1
    }else{
      x<-x+1
    }
  }
}
return(df)
}

stock<-26163
av<-read.csv("Archive.csv")

famtree<-as.data.frame(genealogy(stock, av), stringsAsFactors = F)

d = data.frame(
  offspring = famtree$Offstock[!is.na(famtree$Offstock)],
  mom   = famtree$momstock[!is.na(famtree$momstock)],
  dad   = famtree$dadstock[!is.na(famtree$dadstock)],
  stringsAsFactors = F
)

d2 = data.frame(from=c(d$mom,d$dad), to=rep(d$offspring,2))
g=graph_from_data_frame(d2)
#co=layout.reingold.tilford(g, flip.y=T)
co1 <- layout_as_tree(g, root = which(grepl("G1", V(g)$name)))
plot(g,layout=co1, edge.arrow.size=1, edge.curved=0.3,edge.color="black",edge.width=1.5,vertex.size=25, vertex.frame.color="black", vertex.label.color="black", vertex.color="orange",
    vertex.label.cex=1.2, title="Sxyz")
plot(g,layout=co1, edge.arrow.size=0.5)



locate_stockn<-function(file, stocknumber){
  tanks<- file$Stock.number.
  tanks<-tanks[!is.na(tanks)]
  tank_loc<-which(stocknumber==tanks)
  return(tank_loc)
}
