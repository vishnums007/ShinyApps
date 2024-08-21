find_empty_tanks<-function(file){
  tanks<- file$Tank.Name
  tanks<-tanks[!is.na(tanks)]
  empty_tanks<-c()
  for (i in 1:length(tanks)){
    x<-length(unlist(strsplit(file[i,4],"")))
    #print(x)
    if(x==0){
      empty_tanks<-append(empty_tanks,file[i,1])
    }
  }
  return(list(empty_tanks))
}

find_occupied_tanks<-function(file){
  tanks<- file$Tank.Name
  tanks<-tanks[!is.na(tanks)]
  occupied_tanks<-c()
  for (i in 1:length(tanks)){
    x<-length(unlist(strsplit(file[i,4],"")))
    #print(x)
    if(x>0){
      occupied_tanks<-append(occupied_tanks,file[i,1])
    }
  }
  return(list(occupied_tanks))
}


locate_tank<-function(file, tankname){
  tanks<- file$Tank.Name
  tanks<-tanks[!is.na(tanks)]
  tank_loc<-which(tankname==tanks)
  return(tank_loc)
}


locate_stockn<-function(file, stocknumber){
  tanks<- file$Stock.number.
  tanks<-tanks[!is.na(tanks)]
  tank_loc<-which(stocknumber==tanks)
  return(tank_loc)
}

orig_val<-function(check, list_check){
  if(check==""){
    loc=1
    return(loc)
  }else{
  loc<-which(check==list_check)
  loc<-loc[1]
  if(is.na(loc)){
    loc=1
  }
  return(loc)
  }
}

track_editing<- function(mat){
  edits<-c()
  for(i in 1:length(row.names(mat))){
    check<- mat[i,1]==mat[i,2]
    #print(check)
    if (check==F){
      edits<-append(edits, paste0("Edited ", row.names(mat)[i]," from ",mat[i,1], " to ",mat[i,2]))
      #print(edits)
    }
    
  }
  return(edits)
  
}

check_errors<- function(values){
  error_check<-FALSE
  for(i in values){
    if( is.na(i)||i==""){
      error_check<-TRUE
    }
    
  }
  return(error_check)
  
}

dup_check<-function(userinput,list ){
  dup_check<- is.element(userinput, list)
  return(dup_check)
}

x<-seq(1:100)
dup_check(10,x)


orig_val_notes<-function(check, list_check){
  if(is.na(check)||check==""){
    loc=1
  }else{
    loc<-which(check==list_check)
    loc<-loc[1]
    if(is.na(loc)){
      loc=1
    }
  }
  return(loc)
}



zebra<-read.csv("Johnson nursery .csv")
notes_list<-unique(zebra[,8]) 
notes_list<-append(notes_list,"Enter notes", after = 0)
ov_notes<- orig_val_notes(zebra[1, 8], notes_list)