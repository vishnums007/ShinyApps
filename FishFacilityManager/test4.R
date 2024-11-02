
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(shinydashboard)
library(shinyauthr)
library(svglite)
library(patchwork)
library(bslib)
library(ggplot2)
library(DT)
library(RSQLite)
library(DBI)
library(lubridate)
library(dplyr)
library(glue)
library(igraph)
library(Cairo)



nursery_info <- read.csv(file = "Nursery_room_info.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
adult_info <- read.csv(file = "Adult_room_info.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
Chien_Room<-read.csv(file = "Chien_room.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
Johnson_Room<-read.csv(file = "Johnson_room.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
Streisinger_room<-read.csv(file = "Streisinger_room.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
Johnson_Nursery<-read.csv(file = "Johnson nursery .csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
Walker_Nursery<-read.csv(file = "Walker nursery.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
Archive<- read.csv(file = "Archive.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")
Database_logs<-read.csv(file = "log.csv", header = TRUE, fill = TRUE, encoding = "UTF-8")

fishdb<-list(nursery_info=nursery_info,adult_info=adult_info, 
                   Chien_Room=Chien_Room, Johnson_Room=Johnson_Room, 
                   Streisinger_room=Streisinger_room, Johnson_Nursery=Johnson_Nursery,
                   Walker_Nursery=Walker_Nursery, 
                   Archive=Archive, Database_logs=Database_logs  )


saveRDS(fishdb, "fishdatabase.rds")


fishdb<-readRDS("fishdatabase.rds")


locate_room<-function(df, roomname){
  rooms<-df[,1]
  rooms<-rooms[!is.na(rooms)]
  room_loc<-which(roomname==rooms)
  return(room_loc)
}

room_loc<-locate_room(combined_info, "Johnson Fish Room")
zebra<-fishdb[[room_loc]]




log_data<-fishdb[["Database_logs"]]
archive<-fishdb[["Archive"]]

fishdb$Archive<-archive
fishdb$Database_logs<-log_data
nursery_selected<-fishdb[["nursery_info"]]$db_list_name[room_loc]
fishdb[[nursery_selected]]<-zebra1
saveRDS(fishdb, "fishdatabase.rds")
