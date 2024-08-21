#rm(list = ls(all.names = TRUE))

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
options(java.parameters = "-Xss256m")
library(xlsx)
library(RSQLite)
library(DBI)
library(lubridate)
library(dplyr)
library(glue)
library(igraph)
library(Cairo)

#essential functions

find_empty_tanks<-function(file){
  tanks<- file$Tank.Name
  tanks<-tanks[!is.na(tanks)]
  empty_tanks<-c()
  for (i in 1:length(tanks)){
    x<-length(unlist(strsplit(file[i,4],"")))
    #print(paste(tanks[i],x))
    if(x<2){
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
    if(x>1){
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
  loc<-which(check==list_check)
  loc<-loc[1]
  if(is.na(loc)){
    loc=1
  }
  return(loc)
  
}

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


track_editing<- function(mat){
  edits<-c()
  for(i in 1:length(row.names(mat))){
    check<- mat[i,1]==mat[i,2]
    #print(check)
    if (check==F){
      edits<-append(edits, paste0("Edited ", row.names(mat)[i]," from ",as.character(mat[i,1]), " to ",as.character(mat[i,2]), " , "))
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

check_stockn<-function(stockn){
  stockn<-suppressWarnings(as.numeric(stockn))
  check<- is.na(stockn)
  return(check)
}

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


# How many days should sessions last?
cookie_expiry <- 7

user_base <- tibble(
  user = c("Mokalledlab", "vishnu"),
  password = c("zebrafish", "vms"),
  permissions = c("standard", "admin"),
  password_hash = sapply(c("zebrafish", "vms"), sodium::password_store),
  name = c("Mokalled lab", "Vishnu")
)



###UI side code

ui<- dashboardPage(
  dashboardHeader(title = "Fish Database", tags$li(class="dropdown",style="padding:8px;", shinyauthr::logoutUI("logout"))),
  
  
  
  #side bar 
  
  dashboardSidebar(
    collapsed = T,
    div(htmlOutput("welcome"), style = "padding: 20px"),
    sidebarMenu(
      menuItem("Veiw Fish stocks",tabName = "view_stocks",icon = icon("search")),
      menuItem("Add Fish Stocks",tabName = "add_stocks",icon = icon("plus-square")),
      menuItem("Transfer Fish",tabName = "transfer_stocks", icon = icon("exchange-alt")),
      menuItem("Edit Fish Stocks",tabName = "edit_stocks", icon = icon("pencil")),
      menuItem("Genealogy",tabName = "familytree", icon = icon("timeline")),
      menuItem("Archive",tabName = "archive", icon = icon("box-archive")),
      menuItem("User logs",tabName = "logbook", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
      
    )
  ),
  
  #main body
  dashboardBody(
    #login body
    shinyauthr::loginUI("login",
                        cookie_expiry = cookie_expiry,
                        additional_ui = tagList(
                          tags$head(tags$style(".table{margin: 0 auto;}"),
                                    tags$script(src="loginwindow.js",type="text/javascript"),
                                    includeScript("returnClick.js"))
                        )),
    includeScript("scrolldown.js"),
    
    tabItems(
      tabItem(tabName = "view_stocks", uiOutput("view_stocks_ui")),
      tabItem(tabName = "add_stocks", uiOutput("add_stocks_ui")),
      tabItem(tabName = "transfer_stocks",uiOutput("transfer_stocks_ui")),
      tabItem(tabName = "edit_stocks", uiOutput("edit_stock_ui")),
      tabItem(tabName = "archive", uiOutput("archive_ui")),
      tabItem(tabName = "logbook", uiOutput("logbook_ui")),
      tabItem(tabName = "familytree", uiOutput("familytree_ui")),
      tabItem(tabName = "about", uiOutput("about_ui"))
      
    )
  )
)

#server side codes
server <- function(input, output, session) { 
  
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  user_info <- reactive({
    credentials()$info
  })
  
  output$welcome <- renderText({
    req(credentials()$user_auth)
    
    glue("Welcome {user_info()$name}")
  })
  
  options(shiny.usecairo=T)
  
  
  ###UI code for show fish stocks#####################
  
  output$view_stocks_ui<-renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = strong("Instructions:"), "
            Here, you will be able to visualize all the fish stocks currently present in the
            Johnson, Streisinger and Nursery rooms. Please the select the required fish room
            from the dropdown box and click view. You can also download the current sheet using 
              the download button.")),
      wellPanel(
        
        selectInput("Fishroom_choices", "Select the Fish room: ", choices = list("Johnson Fish Room",
                                                                                 "Streisinger Fish Room",
                                                                                 "Walker Nursery",
                                                                                 "Johnson Nursery"
        )),
        
        actionButton(inputId = "fishroom_upload", label="Show Fish Stocks"),
        
        downloadButton("Download_facility_map","Download Room map (.xlsx)")
      )
      ,
      fluidRow(
        wellPanel(
          div(h4(strong(textOutput("Fishroom_title"))),
              dataTableOutput("fishroom_map"),
              style = "width: 100%; overflow-x: auto;")
        )
      )
    )
  })
  ###################################################################################  
  ##server side code for show fish stocks
  fishroom_choice<-eventReactive(input$fishroom_upload,{input$Fishroom_choices})
  room_map<-eventReactive(input$fishroom_upload,{
    if (fishroom_choice()=="Johnson Fish Room"){
      zebra<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
    }else if(fishroom_choice()=="Streisinger Fish Room"){
      zebra<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
    }else if(fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
    }else if(fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
    }
  })
  
  output$Fishroom_title<-renderText({
    paste(fishroom_choice())
  })
  
  output$fishroom_map<-DT::renderDataTable(
    room_map(),option=list(pageLength=80),escape=F
  )
  
  output$Download_facility_map<-downloadHandler(
    filename=function(){
      paste("Fish stocks in_", fishroom_choice(), ".xlsx", sep="")
    },
    content=function(file){
      write.xlsx(room_map(),file, showNA = F,row.names = F)
    }
  )
  ################################################################################################################
  #####UI part for adding stocks#######################################################################
  
  output$add_stocks_ui<-renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = strong("Instructions:"), "
            Here, you will be able to add fish stocks in to the
            Johnson, Streisinger and Nursery rooms. First select the tab depending whether you want to add larval fish
            or Adult Fish. Then, fill out the respective form shown in the given tab and
        hit submit. Then, you will be asked to verify the information you provided and finally,
        click on the confirm button to add your stock to the database. Once you hit confirm button, your 
        action will be recorded in the log book.You can add another stock by hitting the reset button.")
      ),
      tabsetPanel(
        
        tabPanel("Add Larvae",
                 div(id="larval_addition",
                     wellPanel(
                       p(style=  "color:blue",strong("Please select the required nursery first from the drop down list below and then click load to update the 
                                                         current status of the respective fish room."
                       )),
                       selectInput("Nursery_choices_add", "Select the Nursery room: ", choices = list(
                         "Walker Nursery",
                         "Johnson Nursery"
                       )),
                       fluidRow(column(width = 3,
                                       actionButton(inputId = "nursery_add_select", label="Load"),
                       )),
                       
                       
                       br(),
                       numericInput(inputId = "nTanks_add_n", label="No. of tanks* :",value = 1 , min=1, max=20, step =1),
                       #numericInput(inputId = "Stock_no_add_n", label="Stock Number* :", value = 18000, min =18000),
                       selectizeInput(inputId = "Stock_no_add_n",label = "Stock Number* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter stock number")),
                       dateInput(inputId = "DOB_add_n",label = "Date of birth* :", max = Sys.Date(), format = "dd-MM-yyyy"),
                       selectizeInput(inputId = "Fish_name_add_n",label = "Fish name* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter your fish name")),
                       selectizeInput(inputId = "Fish_Genotype_add_n",label = "Genotype* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter your fish genotype")),
                       numericInput(inputId = "nFish_add_n", label="No of fish* :", value = 1 , min=1),
                       selectizeInput(inputId = "Fish_Responsible_add_n",label = "Responsible* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter owner name")),
                       numericInput(inputId = "Stock_no_fparent_add_n", label="Female Parent Stock Number* :", value = 18000, min =18000),
                       selectizeInput(inputId = "Fish_fParent_add_n",label = "Female Parent Genotype* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter female parent")),
                       numericInput(inputId = "Stock_no_Mparent_add_n", label="Male Parent Stock Number* :", value = 18000, min =18000),
                       selectizeInput(inputId = "Fish_MParent_add_n",label = "Male Parent Genotype* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter male parent")),
                       textInput(inputId = "Experiment_n",label = "Notes: "),
                       
                       p(style=  "color:red",strong("*Fields are mandatory."
                       )),
                       actionButton(inputId = "Add_nursery", label="Submit"),
                       h4(style=  "color:red",textOutput("error_add_Nfish"))
                     ),
                     
                     br(),
                     div(id = "add_nursery_cnfm_ui",
                         wellPanel(
                           useShinyjs(),
                           h4(strong(textOutput("nursery_add_conf_title"))),
                           hr(),
                           tableOutput("nursery_add_conf_data")%>% withSpinner(type=5),
                           br(),
                           actionButton(inputId = "Add_nursery_confirm", label="Confirm Adding Fish Stock"),
                           br(),
                           br(),
                           actionButton(inputId = "reset_nursery_addition", label="Reset form"),
                           br(),
                           br(),
                           strong(textOutput("nursery_add_success"))
                         )
                     )%>% shinyjs::hidden()
                 )),
        tabPanel("Add Adult Fish",
                 div(id="adult_addition",
                     wellPanel(
                       p(style=  "color:blue",strong("Please select the required fish room first from the drop down list below and then click load to update the 
                                                         current status of the respective fish room."
                       )),
                       
                       selectInput("fishroom_choices_adult_add", "Select the Fish room: ", choices = list(
                         "Johnson Fish Room",
                         "Streisinger Fish Room"
                       )),
                       actionButton(inputId = "upload_adult_room", label="Load"),
                       br(),
                       strong(textOutput("adult_room_load_success")%>% withSpinner(type=5)),
                       br(),
                       
                       selectizeInput(inputId = "Tank_name_adult", label = "Tank name* ", choices= NULL , options = list(create=F, placeholder = "Empty Tank location")),
                       #numericInput(inputId = "Stock_no_add_adult", label="Stock Number* :", value = 18000, min =18000),
                       selectizeInput(inputId = "Stock_no_add_adult",label = "Stock Number* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter stock number")),
                       dateInput(inputId = "DOB_add_adult",label = "Date of birth* :", max = Sys.Date(),format = "dd-MM-yyyy"),
                       selectizeInput(inputId = "Fish_name_add_adult",label = "Fish name* :", choices= NULL , options = list(create=T, placeholder = "Type Fish name")),
                       selectizeInput(inputId = "Fish_Genotype_add_adult",label = "Genotype* :", choices= NULL , options = list(create=T, placeholder = "Type Fish Genotype")),
                       numericInput(inputId = "nFish_add_adult", label="No of fish* :", value = 1 , min=1),
                       selectizeInput(inputId = "Fish_Responsible_add_adult",label = "Responsible* :", choices= NULL,options = list(create=T, placeholder = "Type Fish Owner")),
                       
                       selectizeInput(inputId = "Experiment_label_adult", label = "Experiment label*", choices= NULL ,multiple = T,options = list(create=T, placeholder = "Select Experiment label")),
                       
                       selectizeInput(inputId = "Food_label_adult", label = "Food label*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Available Food label")),
                       textInput(inputId = "Notes_add_adult",label = "Notes: "),
                       
                       p(style=  "color:red",strong("*Fields are mandatory."
                       )),
                       actionButton(inputId = "Add_adult_Fish_stock", label="Submit"),
                       h4(style=  "color:red",textOutput("error_add_Afish"))
                     ),
                     
                     br(),
                     div(id = "add_adult_cnfm_ui",
                         wellPanel(
                           useShinyjs(),
                           h4(strong(textOutput("adult_add_conf_title"))),
                           hr(),
                           tableOutput("adult_add_conf_data")%>% withSpinner(type=5),
                           br(),
                           actionButton(inputId = "Add_adult_confirm", label="Confirm Adding Fish Stock"),
                           br(),
                           br(),
                           actionButton(inputId = "reset_adult_addition", label="Reset form"),
                           br(),
                           br(),
                           div(id="success_message_n",
                               strong(textOutput("adult_add_success")%>% withSpinner(type=5)))
                         )
                     )%>% shinyjs::hidden()
                 )
                 
        )
      )
      
    )
  })
  ####################################################################################################################
  #########################Server part for adding larval stocks#############################################################
  
  nursery_add_choice<-eventReactive(input$nursery_add_select,{input$Nursery_choices_add})
  
  output$nursery_add_conf_title<- renderText({
    paste0("Confirm the new fish stock information")
  })
  
  observeEvent(input$nursery_add_select,{
    archive<-read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
    
    stockn_list<-unique(archive[,2])
    max_stockn<-max(stockn_list)
    new_stockn_list<- (max_stockn+1:(max_stockn+100))
    updateSelectizeInput(session,inputId = "Stock_no_add_n",choices = new_stockn_list,selected = new_stockn_list[1], server=T)
    
    fish_name_list<-unique(archive[,4])
    fish_name_list<-append(fish_name_list,"Enter fish name", after=0)
    updateSelectizeInput(session, inputId = "Fish_name_add_n",choices = fish_name_list,selected = fish_name_list[1], server=T)
    
    genotype_list<-unique(archive[,5])
    genotype_list<-append(genotype_list,"Enter genotype", after=0)
    updateSelectizeInput(session, inputId = "Fish_Genotype_add_n",choices = genotype_list,selected = genotype_list[1], server=T)
    
    fish_Resp_list<-unique(archive[,7])
    fish_Resp_list<-append(fish_Resp_list,"Enter owner name", after=0)
    updateSelectizeInput(session, inputId = "Fish_Responsible_add_n",choices = fish_Resp_list,selected = fish_Resp_list[1], server=T)
    
    fmale_parent_list<-unique(archive[,8]) 
    fmale_parent_list<-append(fmale_parent_list,"Enter female parent name", after=0)
    updateSelectizeInput(session, inputId = "Fish_fParent_add_n",choices = fmale_parent_list,selected = fmale_parent_list[1], server=T)
    
    male_parent_list<- unique(archive[,9])
    male_parent_list<-append(male_parent_list,"Enter male parent name", after=0)
    updateSelectizeInput(session, inputId = "Fish_MParent_add_n",choices = male_parent_list,selected = male_parent_list[1], server=T)
    
    
  })
  
  observeEvent(input$Add_nursery,{
    
    if (nursery_add_choice()=="Walker Nursery"){
      zebra1<- read.csv(file = "Walker nursery.csv",header = T, fill = T,encoding="UTF-8")
      
    }else if(nursery_add_choice()=="Johnson Nursery"){
      zebra1<- read.csv(file = "Johnson nursery .csv",header = T, fill = T,encoding="UTF-8")
      
    }
    
    user_input<-c(input$nTanks_add_n,input$Stock_no_add_n,as.character(input$DOB_add_n),input$Fish_name_add_n,input$Fish_Genotype_add_n,
                  input$nFish_add_n,input$Fish_Responsible_add_n,input$Stock_no_fparent_add_n,input$Fish_fParent_add_n,
                  input$Stock_no_Mparent_add_n,input$Fish_MParent_add_n)
    
    stock_list<- zebra1[,2]
    
    dup_check_stock<-dup_check(input$Stock_no_add_n,stock_list)
    stockn_check<- check_stockn(input$Stock_no_add_n)
    
    qc<-check_errors(user_input)
    if(qc==T){
      output$error_add_Nfish<- renderText(
        "ERROR !!! Please fill all the mandatory fields"
      )
    }else if (dup_check_stock==T){
      output$error_add_Nfish<- renderText(
        "ERROR !!! This Stock number is already present!!"
      )
      
    }else if (stockn_check==T){
      output$error_add_Nfish<- renderText(
        "ERROR !!! You can only enter numeric values as a stock number!!"
      )
      
    }else{
      shinyjs::show("add_nursery_cnfm_ui")
      output$error_add_Nfish<- renderText(
        ""
      )
      
    }
    
  })
  
  output$nursery_add_conf_data<-renderTable({
    features<-matrix(nrow = 13, ncol=2)
    features[1,]<-c("Nursery : ", input$Nursery_choices_add )
    features[2,]<-c("No. of tanks :", input$nTanks_add_n)
    features[3,]<-c("Stock No. :", input$Stock_no_add_n)
    features[4,]<-c("Date of Birth :", format(input$DOB_add_n,"%d-%b-%Y"))
    features[5,]<-c("Fish name : ",input$Fish_name_add_n)
    features[6,]<-c("Fish Genotype : ", input$Fish_Genotype_add_n)
    features[7,]<-c("No. of fish :", input$nFish_add_n)
    features[8,]<-c("Responsible person :", input$Fish_Responsible_add_n)
    features[9,]<-c("Female parent stock no. :", input$Stock_no_fparent_add_n)
    features[10,]<-c("Female parent fish Genotype :", input$Fish_fParent_add_n)
    features[11,]<-c("Male parent stock no. :", input$Stock_no_Mparent_add_n)
    features[12,]<-c("Male parent fish genotype :", input$Fish_MParent_add_n)
    features[13,]<-c("Notes :", input$Experiment_n)
    colnames(features)<- c("Stock Features","User Input" )
    return(features)
  })
  
  
  
  observeEvent(input$Add_nursery_confirm, {
    
    if (nursery_add_choice()=="Walker Nursery"){
      zebra1<- read.csv(file = "Walker nursery.csv",header = T, fill = T,encoding="UTF-8")
      filename<-"Walker nursery.csv"
    }else if(nursery_add_choice()=="Johnson Nursery"){
      zebra1<- read.csv(file = "Johnson nursery .csv",header = T, fill = T,encoding="UTF-8")
      filename<-"Johnson nursery .csv"
    }
    
    
    #WRITING DATA INTO THE EXCEL
    archive<-read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
    
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    archive_stock_list<- archive[,2]
    dup_check_archive<-dup_check(input$Stock_no_add_n,archive_stock_list)
    if(dup_check_archive==T){
      dup_loc<-which(input$Stock_no_add_n==archive_stock_list)
      
      output$modal_output_dupArchive<-renderTable(archive[dup_loc,2:6])
      showModal(modalDialog(
        tagList(
          p(style= "color:red",strong("WARNING : This stock number is already present in the archive !! ")),
          tableOutput("modal_output_dupArchive"),
          br(),
          p(strong("Stock number is unique to each fish. Therefore, do you wish to overwrite the old stock
                  number with the new information ?"))
        ),
        title = "Confirm Overwriting Stock number",
        footer = tagList(actionButton("Nursery_add_archive_dup","Yes"), modalButton("No"))
      ))
      
      
      
    }else{
      archive[nrow(archive)+1,]<-list(input$nTanks_add_n,input$Stock_no_add_n,format(input$DOB_add_n,"%d-%b-%Y"),input$Fish_name_add_n,
                                      input$Fish_Genotype_add_n,input$nFish_add_n,input$Fish_Responsible_add_n,
                                      input$Fish_fParent_add_n,input$Fish_MParent_add_n,input$Stock_no_fparent_add_n,input$Stock_no_Mparent_add_n,
                                      input$Experiment_n)
      
      log_data[nrow(log_data)+1,]<-list( format(Sys.time(), "%d-%b-%Y %H.%M"),input$Stock_no_add_n,format(input$DOB_add_n,"%d-%b-%Y"),input$Fish_name_add_n,
                                         input$Fish_Genotype_add_n,input$Fish_Responsible_add_n,input$nFish_add_n,
                                         paste0(" ADDED STOCK IN THE ", input$Nursery_choices_add))
      zebra1[nrow(zebra1)+1,]<-list(input$nTanks_add_n,input$Stock_no_add_n,format(input$DOB_add_n,"%d-%b-%Y"),input$Fish_name_add_n,
                                    input$Fish_Genotype_add_n,input$Fish_Responsible_add_n,input$nFish_add_n,
                                    input$Experiment_n)
      
      
      write.csv(archive,file="Archive.csv", row.names = F )
      
      write.csv(log_data, file = "log.csv",row.names = F)
      write.csv(zebra1,file = filename, row.names = F)
      
      output$nursery_add_success<- renderText({
        paste0("Your fish stock is been succesfully added to the ", input$Nursery_choices_add, " !!")
      })
    }
  })
  
  observeEvent(input$Nursery_add_archive_dup,{
    if (nursery_add_choice()=="Walker Nursery"){
      zebra1<- read.csv(file = "Walker nursery.csv",header = T, fill = T,encoding="UTF-8")
      filename<-"Walker nursery.csv"
    }else if(nursery_add_choice()=="Johnson Nursery"){
      zebra1<- read.csv(file = "Johnson nursery .csv",header = T, fill = T,encoding="UTF-8")
      filename<-"Johnson nursery .csv"
    }
    
    #WRITING DATA INTO THE EXCEL after removing old archive entry with the same stock
    archive<-read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
    
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    archive_stock_list<- archive[,2]
    
    dup_loc<-which(input$Stock_no_add_n==archive_stock_list)
    archive<-archive[-dup_loc,]
    archive[nrow(archive)+1,]<-list(input$nTanks_add_n,input$Stock_no_add_n,format(input$DOB_add_n,"%d-%b-%Y"),input$Fish_name_add_n,
                                    input$Fish_Genotype_add_n,input$nFish_add_n,input$Fish_Responsible_add_n,
                                    input$Fish_fParent_add_n,input$Fish_MParent_add_n,input$Stock_no_fparent_add_n,input$Stock_no_Mparent_add_n,
                                    input$Experiment_n)
    log_data[nrow(log_data)+1,]<-list( format(Sys.time(), "%d-%b-%Y %H.%M"),input$Stock_no_add_n,format(input$DOB_add_n,"%d-%b-%Y"),input$Fish_name_add_n,
                                       input$Fish_Genotype_add_n,input$Fish_Responsible_add_n,input$nFish_add_n,
                                       paste0(" ADDED STOCK IN THE ", input$Nursery_choices_add))
    zebra1[nrow(zebra1)+1,]<-list(input$nTanks_add_n,input$Stock_no_add_n,format(input$DOB_add_n,"%d-%b-%Y"),input$Fish_name_add_n,
                                  input$Fish_Genotype_add_n,input$Fish_Responsible_add_n,input$nFish_add_n,
                                  input$Experiment_n)
    
    
    write.csv(archive,file="Archive.csv", row.names = F )
    
    write.csv(log_data, file = "log.csv",row.names = F)
    write.csv(zebra1,file = filename, row.names = F)
    removeModal()
    output$nursery_add_success<- renderText({
      paste0("The old archive is replaced and your fish stock is been succesfully added to the ", input$Nursery_choices_add, " !!")
    })
    
  })
  
  
  observeEvent(input$reset_nursery_addition,{
    reset("larval_addition")
    shinyjs::hide("add_nursery_cnfm_ui")
  })
  
  observeEvent(input$reset_nursery_addition,{
    output$nursery_add_success<- renderText({
      paste0("Your form is been reset. Please confirm again to add stocks!!")
    })
  })
  
  
  
  ###########################################################################################################################
  ##########server part of adding adult fish
  adult_add_choice<-eventReactive(input$upload_adult_room,{input$fishroom_choices_adult_add})
  observeEvent(input$upload_adult_room,{
    if(adult_add_choice()=="Johnson Fish Room"){
      adult_file <- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
    }else if (adult_add_choice()=="Streisinger Fish Room") {
      adult_file <- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
    }
    
    empty<- find_empty_tanks(adult_file)
    updateSelectizeInput(session, inputId = "Tank_name_adult", selected = empty[[1]][1] ,choices = empty,server= T) 
    
    exp_label_std<-list("GENOTYPE","SURGERY","BREEDING STOCK","TRANSPLANT")
    exp_label<- unique(adult_file[,8])
    exp_label<-append(exp_label,exp_label_std,after = 0)
    updateSelectizeInput(session, inputId = "Experiment_label_adult", selected = exp_label[1] ,choices = exp_label,server= T)
    
    food_label<-list("JA","JB","JC","JD","JE","JF","JG","JH","JI","JJ","SA","SB","SC","SD","SE","SF","SG","SH","SI","SJ",
                     "AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ")
    updateSelectizeInput(session, inputId = "Food_label_adult", selected = food_label[14] ,choices = food_label,server= T)
    
    archive<-read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
    
    stockn_list<-unique(archive[,2])
    max_stockn<-max(stockn_list)
    new_stockn_list<- (max_stockn+1:(max_stockn+100))
    updateSelectizeInput(session, inputId = "Stock_no_add_adult",choices = new_stockn_list,selected = new_stockn_list[1], server=T)
    
    fish_name_list<-unique(archive[,4])
    fish_name_list<-append(fish_name_list,"Enter fish name", after=0)
    updateSelectizeInput(session, inputId = "Fish_name_add_adult",choices = fish_name_list,selected = fish_name_list[1], server=T)
    
    genotype_list<-unique(archive[,5])
    genotype_list<-append(genotype_list,"Enter genotype", after=0)
    updateSelectizeInput(session, inputId = "Fish_Genotype_add_adult",choices = genotype_list,selected = genotype_list[1], server=T)
    
    fish_Resp_list<-unique(archive[,7])
    fish_Resp_list<-append(fish_Resp_list,"Enter owner name", after=0)
    updateSelectizeInput(session, inputId = "Fish_Responsible_add_adult", choices = fish_Resp_list,selected = fish_Resp_list[1], server=T)
    
    
  })
  
  success_text_adult_upload<-eventReactive(input$upload_adult_room,{
    paste0("Fish room map of ",adult_add_choice()  ," is successfully loaded !")
  })
  output$adult_room_load_success<- renderText({
    success_text_adult_upload()
  })
  
  observeEvent(input$Add_adult_Fish_stock,{
    user_input<- c(input$Tank_name_adult,input$Stock_no_add_adult,as.character(input$DOB_add_adult),input$Fish_name_add_adult,
                   input$Fish_Genotype_add_adult,input$nFish_add_adult,input$nFish_add_adult, input$Fish_Responsible_add_adult,
                   paste(input$Experiment_label_adult,collapse = " , "),input$Food_label_adult
    )
    
    stockn_check<- check_stockn(input$Stock_no_add_adult)
    
    qc<-check_errors(user_input)
    if(qc==T){
      output$error_add_Afish<- renderText(
        "ERROR !!! Please fill all the mandatory fields"
      )
    }else if (stockn_check==T){
      output$error_add_Afish<- renderText(
        "ERROR !!! You can only enter numeric values as a stock number!!"
      )
      
    }else{
      shinyjs::show("add_adult_cnfm_ui")
      output$error_add_Afish<- renderText(
        ""
      )
    }
  })
  
  output$adult_add_conf_title<- renderText({
    paste0("Confirm the new fish stock information")
  })
  
  output$adult_add_conf_data<-renderTable({
    featuresa<-matrix(nrow = 11, ncol=2)
    featuresa[1,]<-c("Fish room : ", adult_add_choice() )
    featuresa[2,]<-c("Tank name :", input$Tank_name_adult)
    featuresa[3,]<-c("Stock No. :", input$Stock_no_add_adult)
    featuresa[4,]<-c("Date of Birth :", format(input$DOB_add_adult,"%d-%b-%Y"))
    featuresa[5,]<-c("Fish name : ",input$Fish_name_add_adult)
    featuresa[6,]<-c("Fish Genotype : ", input$Fish_Genotype_add_adult)
    featuresa[7,]<-c("No. of fish :", input$nFish_add_adult)
    featuresa[8,]<-c("Responsible person :", input$Fish_Responsible_add_adult)
    featuresa[9,]<-c("Experimental Label :", paste(input$Experiment_label_adult,collapse = " , "))
    featuresa[10,]<-c("Food Label :", input$Food_label_adult)
    featuresa[11,]<-c("Notes :", input$Notes_add_adult)
    colnames(featuresa)<- c("Stock Features","User Input" )
    return(featuresa)
  })
  
  
  observeEvent(input$Add_adult_confirm,{
    if(adult_add_choice()=="Johnson Fish Room"){
      adult_file <- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_Ad<-"Johnson_room.csv"
    }else if (adult_add_choice()=="Streisinger Fish Room") {
      adult_file <- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_Ad<-"Streisinger_room.csv"
    }
    
    adult_file[locate_tank(adult_file,input$Tank_name_adult),]<-list(input$Tank_name_adult, input$Stock_no_add_adult, format(input$DOB_add_adult,"%d-%b-%Y"),
                                                                     input$Fish_name_add_adult, input$Fish_Genotype_add_adult, input$Fish_Responsible_add_adult,
                                                                     input$nFish_add_adult,paste(input$Experiment_label_adult,collapse = " , "),input$Food_label_adult,
                                                                     input$Notes_add_adult)
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),input$Stock_no_add_adult,format(input$DOB_add_adult,"%d-%b-%Y"),
                                      input$Fish_name_add_adult,input$Fish_Genotype_add_adult,input$Fish_Responsible_add_adult,
                                      input$nFish_add_adult,
                                      paste0(" ADDED STOCK IN THE ", adult_add_choice()))
    write.csv(log_data, file = "log.csv",row.names = F)
    write.csv(adult_file,file = filename_Ad, row.names = F)
  })
  
  
  observe({
    output$adult_add_success<- renderText({
      ""
    })
  })
  
  observeEvent(input$Add_adult_confirm,{
    output$adult_add_success<- renderText({
      paste0("Your fish stock is been succesfully added to the ", adult_add_choice(), " !!")
    })
  })
  
  
  observeEvent(input$reset_adult_addition,{
    reset("adult_addition")
    shinyjs::hide("add_adult_cnfm_ui")
  })
  
  observeEvent(input$reset_adult_addition,{
    output$adult_add_success<- renderText({
      paste0("Your form is been reset. Please confirm again to add stocks!!")
    })
  })
  
  ######################################################################################################################################
  ### UI codes for transfering fish stocks ############################################################################################
  
  output$transfer_stocks_ui<-renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = strong("Instructions:"), "
            Here, you will be able to transfer fish from one room to another. First you have to load the Fish room from which the stock 
           needs to be transferred. Separate tabs are given for transferring fish from nursery and transfer within the adult fish rooms.
           Then, you need to load the fish room to where the stock that needs to be transferred.
           Then, you have to select the stock number - for nursery fish transfer or tank location - for adult fish transfer to select the stock.  You can change the fish number, Genotype, Experiment label and Food label
           at the location where it is transferred. Once you submit the form, you will be asked to verify your action and finally,  click the confirm button. 
           Once you confirm, you actions will be logged. You can transfer another fish by hitting the reset button. 
           ")),
      br(),
      br(),
      tabsetPanel(
        tabPanel("From Nursery to Fish Rooms",
                 div(id="transfer fish_nursery",
                     wellPanel(
                       useShinyjs(),
                       p(style=  "color:blue",strong("Please select the required nursery first from the drop down list below and then click Upload to update the 
                                                         current status of the respective fish room."
                       )),
                       selectInput("nursery_choices_transfer", "Select the Nursery room: ", choices = list(
                         "Walker Nursery",
                         "Johnson Nursery"
                       )),
                       actionButton(inputId = "nursery_transfer_upload", label="Upload"),
                       br(),
                       div(fluidRow(column(width=12, dataTableOutput("nursery_stock_transfer_quickview"),
                                           style = "width: 100%; overflow-x: auto;"))),
                       br(),
                       br(),
                       h3(strong("FROM :-")),
                       hr(),
                       selectizeInput(inputId = "Select_nursery_stock_transfer", label = "Select Stock number to transfer*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type Stock number")),
                       actionButton(inputId = "nursery_transfer_select", label="Select"),
                       br(),
                       h5(strong(textOutput("nursery_transfer_room_selection"))),
                       hr(),
                       div(id="nursery_transfer",
                           h3(strong("TO :-")),
                           hr(),
                           
                           selectizeInput(inputId = "Select_destination_tank_nursery_transfer", label = "Destination Tank name*", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                           
                           #numericInput(inputId = "nursery_transfer_n", label="No of fish to transfer* :", value = 1 , min=1),
                           selectizeInput(inputId = "nursery_transfer_n", label = "No of fish to transfer*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Fish Number to transfer")),
                           selectizeInput(inputId = "Experiment_label_nursery_transfer", label = "Experiment label*", choices= NULL ,multiple = T,options = list(create=T, placeholder = "Select Experiment label")),
                           selectizeInput(inputId = "Food_label_adult_nursery_transfer", label = "Food label*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Available Food label")),
                           selectizeInput(inputId = "Notes_nursery_trasnfer", label="Notes*: ", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter your notes")),
                           p(style=  "color:red",strong("*All fields mandatory."
                           )),
                           
                           actionButton(inputId = "nursery_Fish_transfer_submit", label="Submit"),
                           br(),
                           h4(style=  "color:red",textOutput("error_transfer_Nurseryfish")),
                           br(),
                           actionButton(inputId = "reset_nursery_transfer", label="Reset form"),
                           br(),
                           br(),
                           strong(textOutput("nursery_fish_transfer_success"))
                       )%>% shinyjs::hidden()
                     ))),
        
        tabPanel("Within Fish Rooms",
                 div(id="transfer fish_adult",
                     wellPanel(
                       useShinyjs(),
                       p(style=  "color:blue",strong("Please select the required fish room first from the drop down list below and then click upload to update the 
                                                         current status of the respective fish room."
                       )),
                       selectInput("adult_choices_transfer", "Select the Fish room: ", choices = list(
                         "Johnson Fish Room",
                         "Streisinger Fish Room"
                       )),
                       
                       actionButton(inputId = "adultroom_transfer_upload", label="Upload"),
                       hr(),
                       div(fluidRow(column(width=12, dataTableOutput("adult_stock_transfer_quickview"),
                                           style = "width: 100%; overflow-x: auto;"))),
                       br(),
                       h3(strong("FROM :-")),
                       hr(),
                       selectizeInput(inputId = "adult_stock_transfer_source_name", label = "Select source Tank name*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                       actionButton(inputId = "adultroom_transfer_select", label="Select"),
                       br(),
                       div(id="adult_transfer",
                           h3(strong("TO :-")),
                           hr(),
                           selectizeInput(inputId = "adult_stock_transfer_destination_name", label = "Select destination Tank name*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                           #numericInput(inputId = "adult_transfer_n", label="No of fish to transfer*:", value = 1 , min=1),
                           selectizeInput(inputId = "adult_transfer_n", label = "No of fish to transfer*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Fish Number to transfer")),
                           selectizeInput(inputId = "genotype_edit_adult_transfer", label = "Genotype*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Select genotype")),
                           selectizeInput(inputId = "Experiment_label_adult_transfer", label = "Experiment label*", choices= NULL ,multiple = T,options = list(create=T, placeholder = "Select Experiment label")),
                           selectizeInput(inputId = "Food_label_adult_adult_transfer", label = "Food label* ", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Available Food label")),
                           selectizeInput(inputId = "Notes_adult_transfer", label="Notes* : ", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter your notes")),
                           
                           p(style=  "color:red",strong("*All fields mandatory."
                           )),
                           actionButton(inputId = "adult_stock_transfer_source_submit", label="Submit"),
                           br(),
                           h4(style=  "color:red",textOutput("error_transfer_Adultfish")),
                           br(),
                           actionButton(inputId = "reset_adult_transfer", label="Reset form"),
                           br(),
                           br(),
                           strong(textOutput("adult_fish_transfer_success"))
                       )%>% shinyjs::hidden()
                     )
                 ))
      )
    )
    
  })
  
  #####Server side code for nursery Fish transfer################################################################################################
  
  nursery_transfer_room<- eventReactive(input$nursery_transfer_upload,{input$nursery_choices_transfer})
  
  observeEvent(input$nursery_transfer_upload,{
    if (nursery_transfer_room()=="Walker Nursery"){
      zebra_n<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      zebra_ad<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      # output$nursery_transfer_room_selection<- renderText({
      #   "Your Fish will be transferred to the Streisinger room !!"
      # })
    }else if(nursery_transfer_room()=="Johnson Nursery"){
      
      zebra_n<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      zebra_ad<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      # output$nursery_transfer_room_selection<- renderText({
      #   "Your Fish will be transferred to the Johnson room !!" 
      # })
    }
    
    
    nursery_stock_n<- list(zebra_n[,2])
    updateSelectizeInput(session, inputId = "Select_nursery_stock_transfer" ,choices = nursery_stock_n,server= T)
    output$nursery_stock_transfer_quickview<-DT::renderDataTable(
      zebra_n,option=list(pageLength=5,lengthChange = FALSE)
    )
  })
  
  observeEvent(input$nursery_transfer_select,{
    if (nursery_transfer_room()=="Walker Nursery"){
      zebra_n<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      zebra_ad<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      output$nursery_transfer_room_selection<- renderText({
        "Your Fish will be transferred to the Streisinger room !!"
      })
    }else if(nursery_transfer_room()=="Johnson Nursery"){
      
      zebra_n<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      zebra_ad<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      output$nursery_transfer_room_selection<- renderText({
        "Your Fish will be transferred to the Johnson room !!" 
      })
    }
    
    shinyjs::show("nursery_transfer")
    
    from_stock_loc <- locate_stockn(zebra_n, input$Select_nursery_stock_transfer)
    destination_free_tanks<- list(find_empty_tanks(zebra_ad))
    updateSelectizeInput(session, inputId = "Select_destination_tank_nursery_transfer" ,choices = destination_free_tanks,server= T)
    
    fishn<- seq(1:zebra_n[from_stock_loc,7])
    ov_geno<-orig_val(zebra_n[from_stock_loc,7],fishn)
    updateSelectizeInput(session, inputId = "nursery_transfer_n",choices = fishn,selected = fishn[ov_geno], server=T)
    
    exp_label_std<-list("GENOTYPE","SURGERY","BREEDING STOCK","TRANSPLANT")
    exp_label<- unique(zebra_ad[,8])
    exp_label<-append(exp_label,exp_label_std,after = 0)
    updateSelectizeInput(session, inputId = "Experiment_label_nursery_transfer", selected = exp_label[1] ,choices = exp_label,server= T)
    
    food_label<-list("JA","JB","JC","JD","JE","JF","JG","JH","JI","JJ","SA","SB","SC","SD","SE","SF","SG","SH","SI","SJ",
                     "AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ")
    updateSelectizeInput(session, inputId = "Food_label_adult_nursery_transfer", selected = food_label[14] ,choices = food_label,server= T)
    notes_list<-unique(zebra_n[,8])
    notes_list<- append(notes_list, "Enter notes", after = 0)
    notes_orig<- orig_val( zebra_n[from_stock_loc,8],notes_list)
    updateSelectizeInput(session, inputId = "Notes_nursery_trasnfer", selected = notes_list[notes_orig] ,choices = notes_list,server= T)
    
    
    
  })
  
  
  observeEvent(input$nursery_Fish_transfer_submit,{
    if (nursery_transfer_room()=="Walker Nursery"){
      zebra_n <-read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      dest_room<- "Streisinger_room"
    }else if(nursery_transfer_room()=="Johnson Nursery"){
      zebra_n<-read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      dest_room<- "Johnson_room"
    }
    
    from_stock_loc <- locate_stockn(zebra_n, input$Select_nursery_stock_transfer)
    transfer_nursery_name<-zebra_n[from_stock_loc,4]
    user_input<- c(input$Select_nursery_stock_transfer, input$Select_destination_tank_nursery_transfer, input$nursery_transfer_n,
                   paste(input$Experiment_label_nursery_transfer, collapse = " , "), input$Food_label_adult_nursery_transfer, input$Notes_nursery_trasnfer
    )
    qc<-check_errors(user_input)
    
    
    if(qc==T){
      output$error_transfer_Nurseryfish<- renderText(
        "ERROR !!! Please fill all the mandatory fields."
        
      )
    }else{
      output$error_transfer_Nurseryfish<- renderText(
        ""
      )
      showModal(modalDialog(
        tagList(
          paste0("Please confirm the transfer of ",input$nursery_transfer_n ," fish of ",input$Select_nursery_stock_transfer, " - ", transfer_nursery_name ),
          paste0(" from ", nursery_transfer_room(), " to the tank location ", input$Select_destination_tank_nursery_transfer),
          paste0(" in the ", dest_room,".")
        ),
        title = "Confirm Transfer",
        footer = tagList(actionButton("nursery_Fish_transfer_confirm","Confirm"), modalButton("Cancel"))
      ))
      
    }
    
    
  })
  
  observeEvent(input$nursery_Fish_transfer_confirm,{
    if (nursery_transfer_room()=="Walker Nursery"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      zebra_n <-read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<-"Streisinger_room.csv" 
      filename_nurs<- "Walker nursery.csv"
      dest_room<- "Streisinger Room"
    }else if(nursery_transfer_room()=="Johnson Nursery"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      zebra_n<-read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<-"Johnson_room.csv"
      filename_nurs<- "Johnson nursery .csv"
      dest_room<- "Johnson Room"
    }
    from_stock_loc <- locate_stockn(zebra_n, input$Select_nursery_stock_transfer)
    zebra_ad[locate_tank(zebra_ad,input$Select_destination_tank_nursery_transfer),]<- list(
      input$Select_destination_tank_nursery_transfer, input$Select_nursery_stock_transfer, as.character(zebra_n[from_stock_loc,3]),zebra_n[from_stock_loc,4],
      zebra_n[from_stock_loc,5],zebra_n[from_stock_loc,6], input$nursery_transfer_n,
      paste(input$Experiment_label_nursery_transfer, collapse = " , "), input$Food_label_adult_nursery_transfer, input$Notes_nursery_trasnfer
    )
    
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),input$Select_nursery_stock_transfer,as.character(zebra_n[from_stock_loc,3]),
                                      zebra_n[from_stock_loc,4],zebra_n[from_stock_loc,5],zebra_n[from_stock_loc,6],
                                      input$nursery_transfer_n,
                                      paste0(" TRANSFERRED STOCK FROM ", nursery_transfer_room(), " TO ",input$Select_destination_tank_nursery_transfer,
                                             " IN ", dest_room))
    
    write.csv(log_data, file = "log.csv",row.names = F)
    write.csv(zebra_ad,file = filename_ad, row.names = F)
    
    if(zebra_n[from_stock_loc,7]<=input$nursery_transfer_n){
      zebra_n<-zebra_n[-from_stock_loc,]
    }else{
      zebra_n[from_stock_loc,7]<- zebra_n[from_stock_loc,7]-input$nursery_transfer_n
    }
    
    write.csv(zebra_n,file = filename_nurs,row.names = F)
    
    removeModal()
    
    output$nursery_fish_transfer_success<-renderText({
      paste0("Your stock is successfully transferred !!")
    })
    
  })
  
  observeEvent(input$reset_nursery_transfer,{
    reset("transfer fish_nursery")
    shinyjs::hide("nursery_transfer")
  })
  
  observeEvent(input$reset_nursery_transfer,{
    output$nursery_fish_transfer_success<- renderText({
      paste0("Your form is been reset. Please submit again to transfer stocks!!")
    })
    output$nursery_transfer_room_selection<- renderText({
      "" 
    })
  })
  
  ##################################################################################################################################################
  #####Served side code for adult fish transfer ####################################################################################################
  adult_transfer_roomchoice<- eventReactive(input$adultroom_transfer_upload,{input$adult_choices_transfer})
  observeEvent(input$adultroom_transfer_upload,{
    if (adult_transfer_roomchoice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      
    }else if(adult_transfer_roomchoice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
    }
    
    output$adult_stock_transfer_quickview<-DT::renderDataTable(
      zebra_ad,option=list(pageLength=5,lengthChange = TRUE)
    )
    
    occupiedtanks_at<-find_occupied_tanks(zebra_ad)
    updateSelectizeInput(session, inputId = "adult_stock_transfer_source_name" ,choices = occupiedtanks_at,server= T)
    
    
  })
  
  observeEvent(input$adultroom_transfer_select, {
    
    if (adult_transfer_roomchoice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8" )
      filename_ad<- "Streisinger_room.csv"
      
    }else if(adult_transfer_roomchoice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    
    shinyjs::show("adult_transfer")
    
    emptytanks_at<- find_empty_tanks(zebra_ad)
    updateSelectizeInput(session, inputId = "adult_stock_transfer_destination_name" ,choices = emptytanks_at,server= T)
    
    adt_location<- locate_tank(zebra_ad, input$adult_stock_transfer_source_name)
    
    fishn<- seq(1:zebra_ad[adt_location,7])
    ov_geno<-orig_val(zebra_ad[adt_location,7],fishn)
    updateSelectizeInput(session, inputId = "adult_transfer_n",choices = fishn,selected = fishn[ov_geno], server=T)
    
    genotype_list<- unique(zebra_ad[,5])
    genotype_list<-append(genotype_list,"Enter genotype", after=0)
    ov_geno<-orig_val(zebra_ad[adt_location,5],genotype_list)
    updateSelectizeInput(session, inputId = "genotype_edit_adult_transfer",choices = genotype_list,selected = genotype_list[ov_geno], server=T)
    
    exp_label_std<-list("GENOTYPE","SURGERY","BREEDING STOCK","TRANSPLANT")
    exp_label<- unique(zebra_ad[,8])
    exp_label<-append(exp_label,exp_label_std,after = 0)
    ov_explab<- orig_val(zebra_ad[adt_location,8],exp_label)
    updateSelectizeInput(session, inputId = "Experiment_label_adult_transfer", selected = exp_label[ov_explab] ,choices = exp_label,server= T)
    
    food_label<-list("JA","JB","JC","JD","JE","JF","JG","JH","JI","JJ","SA","SB","SC","SD","SE","SF","SG","SH","SI","SJ",
                     "AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ")
    ov_foodlab<- orig_val(zebra_ad[adt_location,9],food_label)
    updateSelectizeInput(session, inputId = "Food_label_adult_adult_transfer", selected = food_label[ov_foodlab],choices = food_label,server= T)
    
    notes_list<-unique(zebra_ad[,10]) 
    notes_list<-append(notes_list,"Enter notes", after = 0)
    ov_notes<- orig_val_notes(zebra_ad[adt_location, 10], notes_list)
    updateSelectizeInput(session, inputId = "Notes_adult_transfer", selected = notes_list[ov_notes], choices = notes_list, server = T)
    
  })
  
  observeEvent(input$adult_stock_transfer_source_submit,{
    if (adult_transfer_roomchoice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
      
    }else if(adult_transfer_roomchoice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    
    from_stock_loc <- locate_tank(zebra_ad, input$adult_stock_transfer_source_name)
    user_input<-c(input$adult_stock_transfer_source_name, input$adult_stock_transfer_destination_name, input$adult_transfer_n,
                  input$genotype_edit_adult_transfer, paste(input$Experiment_label_adult_transfer, collapse = " , "), input$Food_label_adult_adult_transfer,
                  input$Notes_adult_transfer
    )
    
    qc<-check_errors(user_input)
    if(qc==T){
      output$error_transfer_Adultfish<- renderText(
        "ERROR !!! Please fill all the mandatory fields"
        
      )
    }else{
      output$error_transfer_Adultfish<- renderText(
        ""
      )
      showModal(modalDialog(
        tagList(
          paste0("Please confirm the transfer of ",input$adult_transfer_n, " fish of " ,zebra_ad[from_stock_loc,2], " - ", zebra_ad[from_stock_loc,4] ),
          paste0(" from ", input$adult_stock_transfer_source_name, " to the tank location ", input$adult_stock_transfer_destination_name)
          
        ),
        title = "Confirm Transfer",
        footer = tagList(actionButton("adult_Fish_transfer_confirm","Confirm"), modalButton("Cancel"))
      ))
      
    }
    
    
  })
  
  
  observeEvent(input$adult_Fish_transfer_confirm,{
    if (adult_transfer_roomchoice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
      
    }else if(adult_transfer_roomchoice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    
    from_stock_loc <- locate_tank(zebra_ad, input$adult_stock_transfer_source_name)
    to_stock_loc<- locate_tank(zebra_ad, input$adult_stock_transfer_destination_name)
    
    zebra_ad[to_stock_loc, ]<-list(
      input$adult_stock_transfer_destination_name, zebra_ad[from_stock_loc,2],  as.character(zebra_ad[from_stock_loc,3]), zebra_ad[from_stock_loc,4],
      input$genotype_edit_adult_transfer, zebra_ad[from_stock_loc,6], input$adult_transfer_n, paste(input$Experiment_label_adult_transfer, collapse = " , "),
      input$Food_label_adult_adult_transfer, input$Notes_adult_transfer) 
    
    
    # 
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),zebra_ad[from_stock_loc,2], as.character(zebra_ad[from_stock_loc,3]),
                                      zebra_ad[from_stock_loc,4],input$genotype_edit_adult_transfer, zebra_ad[from_stock_loc,6],
                                      input$adult_transfer_n,
                                      paste0(" TRANSFERRED STOCK FROM ", adult_transfer_roomchoice()  ," TANK LOCATION- ", input$adult_stock_transfer_source_name, " TO ",input$adult_stock_transfer_destination_name
                                      ))
    
    write.csv(log_data, file = "log.csv",row.names = F)
    
    
    if(as.numeric(zebra_ad[from_stock_loc,7])<=input$adult_transfer_n){
      zebra_ad[from_stock_loc,2:10]<-NA
    }else{
      zebra_ad[from_stock_loc,7]<- as.numeric(zebra_ad[from_stock_loc,7])-input$adult_transfer_n
    }
    
    write.csv(zebra_ad,file = filename_ad,row.names = F)
    
    removeModal()
    
    output$adult_fish_transfer_success<-renderText({
      paste0("Your stock is successfully transferred !!")
    })
    
  })
  
  observeEvent(input$reset_adult_transfer,{
    reset("transfer fish_adult")
    shinyjs::hide("adult_transfer")
  })
  
  observeEvent(input$reset_adult_transfer,{
    output$adult_fish_transfer_success<- renderText({
      paste0("Your form is been reset. Please submit again to transfer stocks!!")
    })
  })
  
  ################################################################################################################################################
  #################UI side code for edit stocks ##############################################################################################
  output$edit_stock_ui<-renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = strong("Instructions:"), "
            Here, you will be able to edit or euthanize any stocks present in the four Fish rooms. First you have to load the Fish room where the stock 
           is present. Then, you have to type in the stock number (in case of nursery fish) or tank location (in case of adult fish) to select the stock that needs to be edited.
           You can change the fish number, Genotype, Experiment label, Food label
           and notes. Once you submit the form, you will be asked to verify your edits and finally,  click the confirm button. 
           Once you confirm, your actions will be logged. You can edit another fish by hitting the reset button. If you click on the Euthanize button, all the fish of the selected
           stock number/tank location
           will be removed from the fish room map.
           ")),
      br(),
      br(),
      wellPanel(
        useShinyjs(),
        div(id="edit_fish_room_options",
            
            p(style=  "color:blue",strong("Please select the required fish room first from the drop down list below and then click upload to update the 
                                                         current status of the respective fish room."
            )),
            selectInput("edit_fish_room_choices", "Select the Fish room: ", choices = list(
              "Johnson Fish Room",
              "Streisinger Fish Room",
              "Walker Nursery",
              "Johnson Nursery"
            )),
            actionButton(inputId = "edit_fish_room_upload", label="Upload"),
            hr(),
            div(dataTableOutput("edit_fish_room_quickview"),
                style = "width: 100%; overflow-x: auto;"),
            
        ),
        
        div(id= "edit_adult_room",
            
            h3(strong("Select Adult Fish :-")),
            hr(),
            selectizeInput(inputId = "edit_Afish_targets", label = "Available Tank names*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
            fluidRow(column(width = 3, actionButton(inputId = "edit_Afish", label="Edit")), column(width = 3,  actionButton(inputId = "Euthanise_AFish", label = "Euthanize All"))),
            strong(textOutput("ethanize_Afish_target_success")),
            hr(),
            div(id="edit_Adult_stocks",
                selectizeInput(inputId = "genotype_edit_Afish_target", label = "Genotype*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit Genotype")),
                selectizeInput(inputId = "Number_edit_Afish_target", label = "No. of fish*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Edit Fish Number")),
                selectizeInput(inputId = "ExpLabel_edit_Afish_target", label = "Experiment Label*", choices= NULL ,multiple = T,options = list(create=T, placeholder = "Edit experiment label")),
                selectizeInput(inputId = "FoodLabel_edit_Afish_target", label = "Food Label*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Edit Food Label")),
                selectizeInput(inputId = "Notes_edit_Afish_target", label = "Notes*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit notes")),
                p(style=  "color:red",strong("*All fields mandatory. Don't leave anything empty")),
                
                actionButton(inputId = "edit_Afish_target_submit", label="Submit"),
                br(),
                h4(style=  "color:red",textOutput("error_edit_Afish")),
                br(),
                actionButton(inputId = "reset_edit_Afish_target", label="Reset form"),
                br(),
                br(),
                strong(textOutput("edit_Afish_target_success"))
            )%>% shinyjs::hidden()
            
        )%>% shinyjs::hidden(),
        
        
        div(id= "edit_nursery_room",
            h3(strong("Select Nursery Fish :-")),
            hr(),
            selectizeInput(inputId = "edit_Nfish_targets", label = "Available Stock numbers*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type Stock")),
            fluidRow(column(width = 3, actionButton(inputId = "edit_Nfish", label="Edit")), column(width = 3,  actionButton(inputId = "Euthanise_NFish", label = "Euthanize All"))),
            strong(textOutput("ethanize_Nfish_target_success")),
            hr(),
            div(id="edit_nursery_stock",
                selectizeInput(inputId = "TankNumber_edit_Nfish_target", label = "No. of Tanks*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Edit Tank Number")),
                selectizeInput(inputId = "genotype_edit_Nfish_target", label = "Genotype*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit Genotype")),
                selectizeInput(inputId = "Number_edit_Nfish_target", label = "No. of fish*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Edit Fish Number")),
                selectizeInput(inputId = "Notes_edit_Nfish_target", label = "Notes*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit notes")),
                p(style=  "color:red",strong("*All fields mandatory")),
                
                actionButton(inputId = "edit_Nfish_target_submit", label="Submit"),
                br(),
                h4(style=  "color:red",textOutput("error_edit_Nfish")),
                br(),
                actionButton(inputId = "reset_edit_Nfish_target", label="Reset form"),
                br(),
                br(),
                strong(textOutput("edit_Nfish_target_success")))%>% shinyjs::hidden()
        )%>% shinyjs::hidden())
    )})
  
  
  #########################################Server side code for editing Adult Fish################################################################################################
  
  
  observeEvent(input$edit_fish_room_upload,{
    if(input$edit_fish_room_choices=="Johnson Fish Room"||input$edit_fish_room_choices=="Streisinger Fish Room"){
      shinyjs::show("edit_adult_room")
      shinyjs::hide("edit_nursery_room")
    }else if (input$edit_fish_room_choices=="Walker Nursery"||input$edit_fish_room_choices=="Johnson Nursery"){
      shinyjs::show("edit_nursery_room")
      shinyjs::hide("edit_adult_room")
    }})
  
  
  edit_fishroom_choice<-eventReactive(input$edit_fish_room_upload,{input$edit_fish_room_choices})
  observeEvent(input$edit_fish_room_upload,{
    if (edit_fishroom_choice()=="Johnson Fish Room"){
      zebra<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      tank_names <-find_occupied_tanks(zebra)
      updateSelectizeInput(session, inputId = "edit_Afish_targets" ,choices = tank_names,server= T)
    }else if(edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      tank_names <-find_occupied_tanks(zebra)
      updateSelectizeInput(session, inputId = "edit_Afish_targets" ,choices = tank_names,server= T)
    }else if(edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      stock_numbers<- zebra[,2]
      updateSelectizeInput(session, inputId = "edit_Nfish_targets" ,choices = stock_numbers,server= T)
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      stock_numbers<- zebra[,2]
      updateSelectizeInput(session, inputId = "edit_Nfish_targets" ,choices = stock_numbers,server= T)
    }
    
    output$edit_fish_room_quickview<-DT::renderDataTable(
      zebra,option=list(pageLength=5,lengthChange = TRUE)
    )
    
    output$ethanize_Afish_target_success<-renderText({
      ""
    })
    
  })
  
  
  ####################################Editing Adult fish ####################################
  
  
  observeEvent(input$edit_Afish,{
    if (edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
    }else if(edit_fishroom_choice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    shinyjs::show("edit_Adult_stocks")
    
    output$ethanize_Afish_target_success<-renderText({
      ""
    })
    
    
    adt_location<- locate_tank(zebra_ad, input$edit_Afish_targets)
    
    genotype_list<- unique(zebra_ad[,5])
    genotype_list<-append(genotype_list,"Enter genotype", after=0)
    ov_geno<-orig_val(zebra_ad[adt_location,5],genotype_list)
    updateSelectizeInput(session, inputId = "genotype_edit_Afish_target",choices = genotype_list,selected = genotype_list[ov_geno], server=T)
    
    fishn<- seq(1:zebra_ad[adt_location,7])
    ov_geno<-orig_val(zebra_ad[adt_location,7],fishn)
    updateSelectizeInput(session, inputId = "Number_edit_Afish_target",choices = fishn,selected = fishn[ov_geno], server=T)
    
    exp_label_std<-list("GENOTYPE","SURGERY","BREEDING STOCK","TRANSPLANT")
    exp_label<- unique(zebra_ad[,8])
    exp_label<-append(exp_label,exp_label_std,after = 0)
    ov_explab<- orig_val(zebra_ad[adt_location,8],exp_label)
    updateSelectizeInput(session, inputId = "ExpLabel_edit_Afish_target", selected = exp_label[ov_explab] ,choices = exp_label,server= T)
    
    food_label<-list("JA","JB","JC","JD","JE","JF","JG","JH","JI","JJ","SA","SB","SC","SD","SE","SF","SG","SH","SI","SJ",
                     "AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ")
    ov_foodlab<- orig_val(zebra_ad[adt_location,9],food_label)
    updateSelectizeInput(session, inputId = "FoodLabel_edit_Afish_target", selected = food_label[ov_foodlab],choices = food_label,server= T)
    
    notes_list<-unique(zebra_ad[,10]) 
    notes_list<-append(notes_list,"Enter notes", after = 0)
    ov_notes<- orig_val_notes(zebra_ad[adt_location, 10], notes_list)
    updateSelectizeInput(session, inputId = "Notes_edit_Afish_target", selected = notes_list[ov_notes], choices = notes_list, server = T)
    
  })
  
  observeEvent(input$edit_Afish_target_submit,{
    if (edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
    }else if(edit_fishroom_choice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    
    
    
    adt_location<- locate_tank(zebra_ad, input$edit_Afish_targets)
    track_change<-matrix(nrow = 5, ncol=2)
    colnames(track_change)<- c("Before editing","After editing" )
    row.names(track_change)<-c("Genotype","Number of fish","Experiment Label","Food Label","Notes")
    track_change[1,]<- c(zebra_ad[adt_location,5] ,input$genotype_edit_Afish_target)
    track_change[2,]<- c(zebra_ad[adt_location,7] ,input$Number_edit_Afish_target)
    track_change[3,]<- c(zebra_ad[adt_location,8] ,paste(input$ExpLabel_edit_Afish_target, collapse = " , "))
    track_change[4,]<- c(zebra_ad[adt_location,9] ,input$FoodLabel_edit_Afish_target)
    track_change[5,]<- c(zebra_ad[adt_location,10] ,input$Notes_edit_Afish_target)
    
    user_input_values<- c(input$genotype_edit_Afish_target,input$Number_edit_Afish_target,paste(input$ExpLabel_edit_Afish_target, collapse = " , "),input$FoodLabel_edit_Afish_target,
                          input$Notes_edit_Afish_target)
    qc<-check_errors(user_input_values)
    user_edit<- track_editing(track_change)
    if(qc==T){
      output$error_edit_Afish<- renderText(
        "ERROR !!! Please fill all the mandatory fields"
        
      )
    }else if (length(user_edit)==0){
      output$error_edit_Afish<- renderText(
        "You didn't make any changes to the stock!!"
      )
    }else if (length(user_edit)>0){
      output$error_edit_Afish<- renderText(
        ""
      )
      output$modal_output_Aedit<-renderText(user_edit)
      showModal(modalDialog(
        tagList(
          paste0("Please confirm the following edits for ",zebra_ad[adt_location,2], " - ",zebra_ad[adt_location,4], " : " ),
          textOutput("modal_output_Aedit")
        ),
        title = "Confirm Edits",
        footer = tagList(actionButton("AFish_edit_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
    
  })
  #### writing excel after confirming the edits
  observeEvent(input$AFish_edit_confirm,{
    if (edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
    }else if(edit_fishroom_choice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    
    adt_location<- locate_tank(zebra_ad, input$edit_Afish_targets)
    zebra_ad[adt_location,5]<- input$genotype_edit_Afish_target
    zebra_ad[adt_location,7] <- input$Number_edit_Afish_target
    zebra_ad[adt_location,8] <- paste(input$ExpLabel_edit_Afish_target, collapse = " , ")
    zebra_ad[adt_location,9] <- input$FoodLabel_edit_Afish_target
    zebra_ad[adt_location,10] <- input$Notes_edit_Afish_target
    
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),zebra_ad[adt_location,2], as.character(zebra_ad[adt_location,3]),
                                      zebra_ad[adt_location,4],input$genotype_edit_Afish_target, zebra_ad[adt_location,6],
                                      input$Number_edit_Afish_target,
                                      paste0("EDITED THE ADULT STOCK ")
    )
    ###saving all changes
    write.csv(zebra_ad,file = filename_ad,row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    removeModal()
    output$edit_Afish_target_success<-renderText({
      paste0("Your stock is successfully edited !!")
    })
    
  })
  
  observeEvent(input$reset_edit_Afish_target,{
    reset("edit_adult_room")
    shinyjs::hide("edit_Adult_stocks")
  })
  
  observeEvent(input$reset_edit_Afish_target,{
    output$edit_Afish_target_success<- renderText({
      paste0("Your form is been reset. Please submit again to edit stocks!!")
    })
  })
  
  ####Perform euthanize all action for the adult fish stock
  observeEvent(input$Euthanise_AFish,{
    if (edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
    }else if(edit_fishroom_choice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    adt_location<- locate_tank(zebra_ad, input$edit_Afish_targets)
    
    showModal(modalDialog(
      tagList(
        paste0("Please confirm euthanizing all fish of ",zebra_ad[adt_location,2], " - ",zebra_ad[adt_location,4], " - ",zebra_ad[adt_location,5])
      ),
      title = "Confirm Euthanize All",
      footer = tagList(actionButton("AFish_euthanize_all_confirm","Confirm"), modalButton("Cancel"))
    ))
    
  })
  
  #remove fish from the adult room after confirming
  observeEvent(input$AFish_euthanize_all_confirm,{
    if (edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
    }else if(edit_fishroom_choice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    
    adt_location<- locate_tank(zebra_ad, input$edit_Afish_targets)
    
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),zebra_ad[adt_location,2], as.character(zebra_ad[adt_location,3]),
                                      zebra_ad[adt_location,4],zebra_ad[adt_location,5], zebra_ad[adt_location,6],
                                      zebra_ad[adt_location,7],
                                      paste0("EUTHANIZED THE ADULT STOCK")
    )
    
    write.csv(log_data, file = "log.csv",row.names = F)
    zebra_ad[adt_location,2:10]<- NA
    write.csv(zebra_ad,file = filename_ad,row.names = F)
    
    removeModal()
    output$ethanize_Afish_target_success<-renderText({
      "You stock is removed from the fish room map !!"
    })
    
  })
  
  
  ################################################Server code Editing Nursery Fish#################################################################
  observeEvent(input$edit_Nfish,{
    if (edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      
    }
    
    shinyjs::show("edit_nursery_stock")
    
    output$ethanize_Nfish_target_success<-renderText({
      ""
    })
    
    Nstock_loc<-locate_stockn(zebra, input$edit_Nfish_targets)
    
    ntanks<- seq(1:zebra[Nstock_loc,1])
    ov_ntanks<-orig_val(zebra[Nstock_loc,1],ntanks)
    updateSelectizeInput(session, inputId = "TankNumber_edit_Nfish_target",choices = ntanks,selected = ntanks[ov_ntanks], server=T)
    
    genotype_list<- zebra[,5]
    genotype_list<-append(genotype_list,"Enter genotype", after=0)
    ov_geno<-orig_val(zebra[Nstock_loc,5],genotype_list)
    updateSelectizeInput(session, inputId = "genotype_edit_Nfish_target",choices = genotype_list,selected = genotype_list[ov_geno], server=T)
    
    fishn<- seq(1:zebra[Nstock_loc,7])
    ov_fishn<-orig_val(zebra[Nstock_loc,7],fishn)
    updateSelectizeInput(session, inputId = "Number_edit_Nfish_target",choices = fishn,selected = fishn[ov_fishn], server=T)
    
    notes_list<-unique(zebra[,8]) 
    notes_list<-append(notes_list,"Enter notes", after = 0)
    ov_notes<- orig_val_notes(zebra[Nstock_loc, 8], notes_list)
    updateSelectizeInput(session, inputId = "Notes_edit_Nfish_target", choices = notes_list, selected = notes_list[ov_notes],server = T)
    
  })
  
  
  observeEvent(input$edit_Nfish_target_submit,{
    if (edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      
    }
    
    Nstock_loc<-locate_stockn(zebra, input$edit_Nfish_targets)
    
    track_change<-matrix(nrow = 4, ncol=2)
    colnames(track_change)<- c("Before editing","After editing" )
    row.names(track_change)<-c("No. of tanks","Genotype","No. of fish","Notes")
    track_change[1,]<- c(zebra[Nstock_loc,1] ,input$TankNumber_edit_Nfish_target)
    track_change[2,]<- c(zebra[Nstock_loc,5] ,input$genotype_edit_Nfish_target)
    track_change[3,]<- c(zebra[Nstock_loc,7] ,input$Number_edit_Nfish_target)
    track_change[4,]<- c(zebra[Nstock_loc,8] ,input$Notes_edit_Nfish_target)
    
    user_input_values<- c(input$TankNumber_edit_Nfish_target,input$genotype_edit_Nfish_target,
                          input$Number_edit_Nfish_target,input$Notes_edit_Nfish_target)
    
    qc<-check_errors(user_input_values)
    user_edit<- track_editing(track_change)
    
    if(qc==T){
      output$error_edit_Nfish<- renderText(
        "ERROR !!! Please fill all the mandatory fields"
        
      )
    }else if (length(user_edit)==0){
      output$error_edit_Nfish<- renderText(
        "You didn't make any changes to the stock!!"
      )
    }else if (length(user_edit)>0){
      output$error_edit_Nfish<- renderText(
        ""
      )
      output$modal_output_Nedit<-renderText(user_edit)
      showModal(modalDialog(
        tagList(
          paste0("Please confirm the following edits for ",zebra[Nstock_loc,2], " - ",zebra[Nstock_loc,4], " : " ),
          textOutput("modal_output_Nedit")
        ),
        title = "Confirm Edits",
        footer = tagList(actionButton("NFish_edit_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
  })
  
  ### writing excel after confirming the edits
  observeEvent(input$NFish_edit_confirm,{
    if (edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      filename<- "Walker nursery.csv"
      
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      filename<- "Johnson nursery .csv"
      
    }
    
    Nstock_loc<-locate_stockn(zebra, input$edit_Nfish_targets)
    
    zebra[Nstock_loc,1]<- input$TankNumber_edit_Nfish_target
    zebra[Nstock_loc,5]<- input$genotype_edit_Nfish_target
    zebra[Nstock_loc,7]<- input$Number_edit_Nfish_target
    zebra[Nstock_loc,8]<- input$Notes_edit_Nfish_target
    
    
    
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),zebra[Nstock_loc,2], as.character(zebra[Nstock_loc,3]),
                                      zebra[Nstock_loc,4],input$genotype_edit_Nfish_target, zebra[Nstock_loc,6],
                                      input$Number_edit_Nfish_target,
                                      paste0("EDITED THE NURSERY STOCK "))
    
    ###saving all changes
    write.csv(zebra,file = filename,row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    removeModal()
    output$edit_Nfish_target_success<-renderText({
      paste0("Your stock is successfully edited !!")
    })
    
  })
  
  observeEvent(input$reset_edit_Nfish_target,{
    reset("edit_nursery_room")
    shinyjs::hide("edit_nursery_stock")
  })
  
  observeEvent(input$reset_edit_Nfish_target,{
    output$edit_Nfish_target_success<- renderText({
      paste0("Your form is been reset. Please submit again to edit stocks!!")
    })
  })
  
  ###############################Performing euthanize action of nursery stock###################################################################
  observeEvent(input$Euthanise_NFish,{
    
    if (edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      filename<- "Walker nursery.csv"
      
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      filename<- "Johnson nursery .csv"
      
    }
    Nstock_loc<-locate_stockn(zebra, input$edit_Nfish_targets)
    
    showModal(modalDialog(
      tagList(
        paste0("Please confirm euthanizing all fish of ",zebra[Nstock_loc,2], " - ",zebra[Nstock_loc,4], " - ",zebra[Nstock_loc,5])
      ),
      title = "Confirm Euthanize All",
      footer = tagList(actionButton("NFish_euthanize_all_confirm","Confirm"), modalButton("Cancel"))
    ))
    
    
  })
  
  #removing fish from the nursery after confirming the euthanize action
  observeEvent(input$NFish_euthanize_all_confirm,{
    
    if (edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      filename<- "Walker nursery.csv"
      
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      filename<- "Johnson nursery .csv"
      
    }
    
    Nstock_loc<-locate_stockn(zebra, input$edit_Nfish_targets)
    
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),zebra[Nstock_loc,2], as.character(zebra[Nstock_loc,3]),
                                      zebra[Nstock_loc,4],zebra[Nstock_loc,5], zebra[Nstock_loc,6],
                                      zebra[Nstock_loc,7],
                                      paste0("EUTHANIZED THE NURSERY STOCK")
    )
    
    write.csv(log_data, file = "log.csv",row.names = F)
    zebra<-zebra[-Nstock_loc,]
    write.csv(zebra,file = filename,row.names = F)
    
    removeModal()
    output$ethanize_Nfish_target_success<-renderText({
      "You stock is removed from the fish room map !!"
    })
    
  })
  
  ################################UI code for Archive section#####################################################################################
  ################################################################################################################################################
  
  output$archive_ui<- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        
        wellPanel(
          useShinyjs(),
          h2(strong("Mokalled Lab Fish Stocks Archive")),
          hr(),
          p(style = "text-align: justify", "
            Here, you will be able to visualize all fish stocks that are/were present in the Mokalled Lab. 
            Click Upload button to show or refresh the archive. You can download the archive by hitting download button."),
          br(),
          
          actionButton(inputId = "archive_upload", label="Show Fish Archive"),
          
          downloadButton("Download_archive","Download Archive (.xlsx)"),
          
          hr(),
          
          div(DT::dataTableOutput("fish_archive")%>% withSpinner(type=5),
              style = "width: 100%; overflow-x: auto;")
          
        )
      )
    )
    
  })
  
  #############################################Server side code of showing Archive##########################################################
  fishArchive<- eventReactive(input$archive_upload,{
    read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
  })
  
  
  output$fish_archive <- DT::renderDataTable(
    fishArchive(),
    options = list(
      pageLength = 80,
      initComplete = JS(
        "function(settings, json) {",
        "  var table = settings.oInstance.api();",
        "  table.columns.adjust().draw();",
        "  var totalPages = table.page.info().pages;",
        "  table.page(totalPages - 1).draw(false);",
        "}"
      )
    ),
    escape = FALSE
  )
  
  output$Download_archive<-downloadHandler(
    filename=function(){
      "MokalledLab_FishArchive.xlsx"
    },
    content=function(file){
      write.xlsx(fishArchive(),file, showNA = F,row.names = F)
    }
  )
  
  ##############################################UI Code for log data###################################################################
  
  output$logbook_ui<- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        
        wellPanel(
          useShinyjs(),
          h2(strong("Fish Database Log Book")),
          hr(),
          p(style = "text-align: justify", "
            Here, you will be able to visualize the log of all the changes made to this database by the user. 
            Click upload button to show or refresh the latest version of the log. You can download the log sheet by hitting download button."),
          br(),
          
          actionButton(inputId = "log_upload", label="Upload Log"),
          
          downloadButton("Download_log","Download Log (.xlsx)"),
          hr(),
          
          div(DT::dataTableOutput("fish_log")%>% withSpinner(type=5),
              style = "width: 100%; overflow-x: auto;")
          
        )
      )
    )
    
  })
  
  #################################Server side code for log data ##########################################################################
  fishlog<- eventReactive(input$log_upload,{
    read.csv(file = "log.csv", header = T, fill = T,encoding="UTF-8")
  })
  
  
  output$fish_log <- DT::renderDataTable(
    fishlog(),
    options = list(
      pageLength = 80,
      initComplete = JS(
        "function(settings, json) {",
        "  var table = settings.oInstance.api();",
        "  table.columns.adjust().draw();",
        "  var totalPages = table.page.info().pages;",
        "  table.page(totalPages - 1).draw(false);",
        "}"
      )
    ),
    escape = FALSE
  )
  
  output$Download_log<-downloadHandler(
    filename=function(){
      "MokalledLab_Fishdatabase_log.xlsx"
    },
    content=function(file){
      write.xlsx(fishlog(),file, showNA = F,row.names = F)
    }
  )
  
  #############################################################################################################################################
  #########################################UI Code for Genealogy ###################################################################
  output$familytree_ui<- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        
        wellPanel(
          
          h2(strong("Genealogy Tree")),
          hr(),
          p(style = "text-align: justify;color:blue", strong("Instructions: "),"
            Here, you will be able to identity the lineage tree of any stock present in the Archive. First, you need to upload
            the Archive file by hitting the upload button. This will provide a quick view of the fish stocks archive and then, you can search your stock number
            using the top left search box. Then, enter your required stock number in the field below the Archive table and hit Identify Genealogy. Once you
            generate the genealogy you download either the genealogy tree or table by hitting the respective download button given below."),
          br(),
          p(strong("Upload the Archive file: ")),
          actionButton(inputId = "Genealogy_Archive", label="Upload"),
          hr(),
          div(DT::dataTableOutput("genealogy_quickview"),
              style = "width: 100%; overflow-x: auto;"),
          hr(),
          
          selectizeInput(inputId = "Select_genealogy_stock", label = "Select Stock number*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type Stock number")),
          
          actionButton(inputId = "Genealogy_generate", label="Identify Genealogy"),
          
          downloadButton("Download_genealogy_tree","Download Family Tree (.pdf)"),
          downloadButton("Download_genealogy_table","Download Family Table (.xlsx)"),
          br(),
          br(),
          h2(strong(textOutput("genealogy_tree_title"))),
          hr(),
          plotOutput("genealogy_tree", width = "100%"),
          br(),
          br(),
          h2(strong(textOutput("genealogy_table_title"))),
          hr(),
          tableOutput("Genealogy_table")
        )
      )
    )
    
  })
  
  ###################################################################################################################################################
  ###########################################Server side code for Genealogy############################################################################
  
  
  observeEvent(input$Genealogy_Archive,{
    archive <- read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
    output$genealogy_quickview <- DT::renderDataTable(
      archive,
      options = list(
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "  var table = settings.oInstance.api();",
          "  table.columns.adjust().draw();",
          "  var totalPages = table.page.info().pages;",
          "  table.page(totalPages - 1).draw(false);",
          "}"
        )
      ),
      escape = FALSE
    )
    
    stock_list<- archive[,2]
    #print(stock_list)
    updateSelectizeInput(session, inputId = "Select_genealogy_stock",choices = stock_list,selected = stock_list[1729], server=T)
    
  })
  
  observeEvent(input$Genealogy_generate,{
    av<-read.csv("Archive.csv",header = T, fill = T,encoding="UTF-8")
    famtree<-as.data.frame(genealogy(input$Select_genealogy_stock, av), stringsAsFactors = F)
    
    d = data.frame(
      offspring = famtree$Offstock[!is.na(famtree$Offstock)],
      mom   = famtree$momstock[!is.na(famtree$momstock)],
      dad   = famtree$dadstock[!is.na(famtree$dadstock)],
      stringsAsFactors = F
    )
    
    d2 = data.frame(from=c(d$mom,d$dad), to=rep(d$offspring,2))
    g=graph_from_data_frame(d2)
    #co=layout.reingold.tilford(g, flip.y=T)
    co <- layout_as_tree(g, root = which(grepl("G1", V(g)$name)))
    
    stoc_loc<-locate_stockn(av,input$Select_genealogy_stock)
    
    output$genealogy_tree_title<-renderText(
      paste("Genealogy tree of ", input$Select_genealogy_stock, " - ", av[stoc_loc, 4] )
    )
    #pdf(file = NULL)
    output$genealogy_tree<- renderPlot(
      
      plot(g,layout=co, edge.arrow.size=1,edge.color="black",edge.width=1.5,vertex.size=25, vertex.frame.color="black", vertex.label.color="black", vertex.color="orange",
           vertex.label.cex=1)
    )
    
    output$genealogy_table_title<- renderText(
      paste("Genealogy table of ", input$Select_genealogy_stock, " - ", av[stoc_loc, 4])
    )
    
    output$Genealogy_table<-renderTable(famtree)
    
    output$Download_genealogy_tree<-downloadHandler(
      filename=function(){
        paste("Genealogy tree_", input$Select_genealogy_stock, " - ", av[stoc_loc, 4],".pdf")
      },
      content=function(file){
        
        cairo_pdf(filename = file, width = 14, height = 14, pointsize = 12, family = "sans", fallback_resolution = 300)
        plot(g,layout=co, edge.arrow.size=1,edge.color="black",edge.width=1.5,vertex.size=25, vertex.frame.color="black", vertex.label.color="black", vertex.color="orange",
             vertex.label.cex=1)
        dev.off()
        
      }
    )
    
    output$Download_genealogy_table<-downloadHandler(
      filename=function(){
        paste("Genealogy table_", input$Select_genealogy_stock, " - ", av[stoc_loc, 4],".xlsx")
      },
      content=function(file){
        write.xlsx(famtree,file, showNA = F, row.names = F)
      }
    )
    
    
    
    
    
  })
  
  #######################################################UI section for the about part##################################################################
  ######################################################################################################################################################
  output$about_ui<- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      wellPanel(
        h2(strong("Mokalled Lab Fish database")),
        hr(),
        p(style="text-align: justify;font-size:16px",("This database is dedicated to the Mokalled Lab. Mokalled lab has fish stocks in both
                                     Streisinger and Johnson Fish rooms at WashU Fish facility. The Johnson Fish room has an exclusive nursery for
                                     all the stocks that will be raised in the Johnson room. On the other hand, Walker nursery
                                     fish will be raised in Streisinger fish room only. Before, adding fish stock information
                                     into this database, one has to obtain a stock number by adding your fish stock to
                                     the fish facility excel file. Then, use the same stock number for adding information here. 
                                     Each stock number is unique to the fish stock added in the nurseries, however you can have
                                     multiple tanks in the adult room with the same stock number. Please try to add complete information
                                     everytime you add a new stock. This will help us to accurately trace the growth and lineage of 
                                     a fish line. One major restriction of this database is that it won't allow you to split the 
                                         tank location and add two separate stock numbers with a divider. In this situation, we 
                                         recommend you to keep the same stock numbers in the divided tank and mention the details in 
                                         the notes."))
      ),
      wellPanel(
        h2(strong("Fish room map")),
        hr(),
        p(style="text-align: justify;font-size:16px", ("Each adult Fish room has predesignated rack space for each user in the Mokalled Lab. Eventhough, 
                                      this is not a strict boundary, one will be able to locate most of the tanks of a user in 
                                      these designated racks. Following is the fish room map for Mokalled lab members :")),
        br(),
        br(),
        tags$img(src="fish_facility_map.png", height="100%", width="100%", alt="Something went wrong",deleteFile=F),
      ),
      wellPanel(
        h2(strong("Suggestions & Reports")),
        hr(),
        p(style="text-align: justify;font-size:16px", ("This website is currently at its very first versions (v1.0.1, last updated: 29-July-2023).
                                      It is possible that you will encounter bugs while using this database. If you find any 
                                      issues, please screenshot the error and send it to: "),em(strong("vishnu@wustl.edu")),
          (". You are also welcome to report any suggestions and improvements to this 
                                          database. Thank you all for using this database.") )
      ),
      br(),
      p(style= "text-align: center",(strong("Copyright(C)2023 Mokalled Lab.  This website is powered using Shiny and R")))
    )
  })
  
  session$onSessionEnded(function() {
    stopApp()
  }) 
  
  writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
  writeLines(.libPaths(), "library.txt")
  
  
  
  
}

shinyApp(ui, server)

