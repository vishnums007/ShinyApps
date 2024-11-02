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
  return(empty_tanks)
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

locate_room<-function(df, roomname){
  rooms<-df[,1]
  rooms<-rooms[!is.na(rooms)]
  room_loc<-which(roomname==rooms)
  return(room_loc)
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

days_to_ymd <- function(DOB) {
  date1<- as.Date(DOB, format = "%d-%b-%Y")
  current_date <- Sys.Date()
  days <- ifelse(!is.na(date1), as.numeric(difftime(current_date, date1, units = "days")), NA)
  years <- floor(days / 365.25)
  remaining_days <- days %% 365.25
  months <- floor(remaining_days / 30.4375)
  days <- floor(remaining_days %% 30.4375)
  return(paste(years, "years", months, "months", days, "days"))
}

cal_weeks<- function(DOB){
  date1<- as.Date(DOB, format = "%d-%b-%Y")
  current_date <- Sys.Date()
  nweeks <- ifelse(!is.na(date1), as.numeric(difftime(current_date, date1, units = "weeks")), NA)
  return(nweeks)
}

empty_selection_check<-function(selected_rows,file){
  empty_check=FALSE
  for( i in 1:length(selected_rows)){
    check<- is.na(file[selected_rows[i],2])
    #print(empty_check)
    if(check==T){
      empty_check=TRUE
    }
  }
  return(empty_check)
}

#function to create a label
create_label <- function(label_info) {
  # Set up a PNG device with specific dimensions
  png("label.png", width = 900, height = 300, res = 300)
  par(mar = c(0, 0, 0, 0))
  # Set up a blank plot
  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE)
  # Draw a border around the plot area
  rect(0, 0, 10, 10, border = "black", lwd = 1) # Adjust 'lwd' for border width
  
  # function to adjust the size dynamically
  fit_height<-function(name){
    cex_value <- 0.5
    text_height <- strheight(name)
    #print(text_width)
    maxheight=10
    if(text_height>maxheight){
      cex_value<-(maxheight/text_height)*cex_value
    }
    return(cex_value)
  }
  
  fit_text<-function(name){
    cex_value <- 1
    text_width <- strwidth(name, cex = cex_value)
    #print(text_width)
    maxwidth=7.5
    if(text_width>maxwidth){
      cex_value<-maxwidth/text_width
    }
    return(cex_value)
  }
  
  # Add the stock number to the label
  text(x = 1, y = 9, labels = label_info[1], cex = 1) # stock
  text(x = 8.2, y = 9, labels = "No.", cex = 1)
  text(x = 9.2, y = 9, labels = label_info[6], cex = 1) #number
  text(x = 3, y = 1, labels = label_info[2], cex = 1) #DOB
  text(x = 8, y = 1, labels = label_info[5], cex = 1) #Resp
  text(x = 3.85, y = 6, labels = label_info[3], cex = fit_text(label_info[3])) #Fish
  text(x = 3.85, y = 4, labels = label_info[4], cex = fit_text(label_info[3])) #Genotype
  
  wrapped_text <- strwrap(label_info[7], width = 13)
  wrapped_text<-paste(wrapped_text, collapse = "\n")
  height=fit_height(wrapped_text)
  text(x = 8.85, y = 5, labels = paste(wrapped_text, collapse = "\n"), cex = height) #Notes
  
  # Draw a horizontal line below the text
  lines(x = c(0, 10), y = c(8, 8), lwd = 1) # Adjust line position and width as needed
  lines(x = c(2, 2), y = c(10, 8), lwd = 1)
  lines(x = c(8.7,8.7), y = c(10, 8), lwd = 1)
  lines(x = c(7.7,7.7), y = c(10, 8), lwd = 1)
  lines(x = c(0, 10), y = c(2, 2), lwd = 1) 
  #lines(x = c(3, 3), y = c(2, 0), lwd = 1)
  lines(x = c(6, 6), y = c(2, 0), lwd = 1)
  lines(x = c(7.7,7.7), y = c(8, 2), lwd = 1)
  # Close the device to save the file
  dev.off()
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
  dashboardHeader(title = "Mokalled Lab Fish Database", titleWidth = 300 ,tags$li(class="dropdown",style="padding:8px;", shinyauthr::logoutUI("logout"))),
  
  
  
  #side bar 
  
  dashboardSidebar(width = 150,
                   collapsed = T,
                   div(htmlOutput("welcome"), style = "padding: 20px"),
                   sidebarMenu(id="tabs",style = "position: fixed; overflow: visible;",
                               menuItem("View Fish stocks",tabName = "view_stocks",icon = icon("search")),
                               menuItem("Add Fish Stocks",tabName = "add_stocks",icon = icon("plus-square")),
                               menuItem("Transfer Fish",tabName = "transfer_stocks", icon = icon("exchange-alt")),
                               menuItem("Edit Fish Stocks",tabName = "edit_stocks", icon = icon("pencil")),
                               menuItem("Genealogy",tabName = "familytree", icon = icon("timeline")),
                               menuItem("Print Label",tabName = "print_label", icon = icon("print")),
                               menuItem("Archive",tabName = "archive", icon = icon("box-archive")),
                               menuItem("User logs",tabName = "logbook", icon = icon("book")),
                               menuItem("About", tabName = "about", icon = icon("info-circle"))
                               
                   )
  ),
  
  #main body
  dashboardBody(
    tags$head(includeHTML("google-analytics.html")),
    #login body
    shinyauthr::loginUI("login",
                        cookie_expiry = cookie_expiry,
                        additional_ui = tagList(
                          tags$head(tags$style(".table{margin: 0 auto;}"),
                                    tags$script(src="loginwindow.js",type="text/javascript"),
                                    includeScript("returnClick.js"))
                        )),
    includeScript("scrolldown.js"),
    tags$script(HTML("$('body').addClass('fixed');")),
    tabItems(
      tabItem(tabName = "view_stocks", uiOutput("view_stocks_ui")),
      tabItem(tabName = "add_stocks", uiOutput("add_stocks_ui")),
      tabItem(tabName = "transfer_stocks",uiOutput("transfer_stocks_ui")),
      tabItem(tabName = "edit_stocks", uiOutput("edit_stock_ui")),
      tabItem(tabName = "print_label", uiOutput("print_label_ui")),
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
      useShinyjs(),
      fluidRow(
        box(width = 12, collapsible = TRUE, title = strong("Instructions:"), "
            Here, you will be able to visualize all the fish stocks currently present in the
            Johnson, Streisinger and Nursery rooms. First click the Update Fish Rooms button.Then, select the required fish room
            from the dropdown box and click view. You can also download the current sheet using 
              the download button.")),
      wellPanel(
        actionButton(inputId = "update_room_button", label="Update Fish Rooms"),
        
        selectizeInput(inputId = "Fishroom_map_choices", label = "Select the Fish Room: ", choices= NULL ,selected=NULL, multiple = F,options = list(create=F, placeholder = "Select Fish Room")),
        
        actionButton(inputId = "fishroom_upload", label="Show Fish Stocks"),
        
        
        downloadButton("Download_facility_map","Download Room map (.csv)"),
        h4(style=  "color:red",textOutput("error_upload_fishmap"))
      ),
      div(id="show_stats",
          
          wellPanel(
            strong(h3("STATS and COLOR CODE")),
            hr(),
            fluidRow(column(width = 2,strong("Occupied tanks: "), strong(textOutput("room_stats_occupied_n"))),
                     column(width = 2,offset = 1,strong("Unoccupied tanks: "), strong(textOutput("room_stats_unoccupied_n"))),  
                     column(width = 2,offset = 1,strong("Total fish no.: "), strong(textOutput("room_stats_totalfish_n"))),
                     column(width = 2,offset = 1,strong("Avg. fish age: "), strong(textOutput("room_stats_avg_fish_age"))),
            ),
            br(),
            fluidRow(
              tags$img(src="color_code.png", height="100%", width="100%", alt="Something went wrong",deleteFile=F)
            )
          ),
          
          fluidRow(
            wellPanel(
              div(h4(strong(textOutput("Fishroom_title"))),
                  dataTableOutput("fishroom_map"),
                  style = "width: 100%; overflow-x: auto;")
            )
          )
      )%>% shinyjs::hidden() 
    )
  })
  ###################################################################################  
  ##server side code for show fish stocks
  
  fishdb<-readRDS("fishdatabase.rds")
  rooms<-c(fishdb[["nursery_info"]][,1],fishdb[["adult_info"]][,1])
  
  observeEvent(input$tabs,{
    if(input$tabs=="view_stocks"){
      fishdb<-readRDS("fishdatabase.rds")
      rooms<-c(fishdb[["nursery_info"]][,1],fishdb[["adult_info"]][,1])
      updateSelectizeInput(session, inputId = "Fishroom_map_choices", choices = rooms, server = TRUE)
      shinyjs::hide("show_stats")
    }
  })
  
  observeEvent(input$update_room_button,{
    updateSelectizeInput(session, inputId = "Fishroom_map_choices", choices = rooms, server = TRUE)
    
  })
  
  fishroom_choice<-eventReactive(input$fishroom_upload,{input$Fishroom_map_choices})
  room_map<-eventReactive(input$fishroom_upload,{
    fishdb<-readRDS("fishdatabase.rds")
    combined_info<-rbind(fishdb[["nursery_info"]],fishdb[["adult_info"]])
    room_loc<-locate_room(combined_info, fishroom_choice())
    zebra<-fishdb[[combined_info$db_list_name[room_loc]]]
    age<- days_to_ymd(zebra$Date.of.Birth)
    week<- cal_weeks(zebra$Date.of.Birth)
    position <- which(names(zebra) == "Date.of.Birth") + 1
    zebra$"Age"<-age
    zebra$"Weeks"<- week
    return(zebra)
  })
  
  observeEvent(input$fishroom_upload,{
    user_input<- input$Fishroom_map_choices
    qc<-check_errors(user_input)
    if(qc==T){
      output$error_upload_fishmap<- renderText(
        "ERROR !!! First, you must click Update Fish Rooms and Then, select one fish room from the list!!! "
      )
    }else{
      n_occu<- length(unlist(find_occupied_tanks(room_map())))
      output$room_stats_occupied_n<-renderText({
        paste0(n_occu,", ",round((n_occu/320)*100), "%")
      })
      
      n_free<-length(unlist(find_empty_tanks(room_map())))
      output$room_stats_unoccupied_n<-renderText({
        paste0(n_free,", ",round((n_free/320)*100),"%")
      })
      
      room<-room_map()
      total_fish<- room$Number.of.fish
      total_fish<-sum(total_fish[!is.na(total_fish)])
      output$room_stats_totalfish_n<-renderText({
        paste(total_fish)
      })
      
      mean_weeks<- room$Weeks
      mean_weeks<-mean(mean_weeks[!is.na(mean_weeks)])
      avg_days<- mean_weeks*7
      years <- floor(avg_days / 365.25)
      remaining_days <- avg_days %% 365.25
      months <- floor(remaining_days / 30.4375)
      days <- floor(remaining_days %% 30.4375)
      #print(mean_weeks)
      
      output$room_stats_avg_fish_age<-renderText({
        paste(years, "years", months, "months", days, "days")
      })
      
      shinyjs::show("show_stats")
      
      output$Fishroom_title<-renderText({
        paste(fishroom_choice())
      })
      
      output$fishroom_map<-DT::renderDataTable(
        datatable(
          room_map(),selection = "single",option=list(pageLength=80,
                                                      rowCallback = JS(
                                                        "function(row, data, index) {",
                                                        "  var age = data[data.length - 1];",
                                                        "  var bgColor;",
                                                        "  if (age >= 6 && age <= 12.99) {",
                                                        "    bgColor = 'rgba(173, 216, 230, 0.50)';",  # Violet color with 45% opacity
                                                        "  } else if (age >= 13 && age <= 26) {",
                                                        "    bgColor = 'rgba(0,128,0,0.15)';",      # Green color with 45% opacity
                                                        "  } else if (age >= 52 && age <= 78) {",
                                                        "    bgColor = 'rgba(255,165,0,0.15)';",   # Orange color with 45% opacity
                                                        "  } else if (age > 78) {",
                                                        "    bgColor = 'rgba(255,0,0,0.15)';",     # Red color with 45% opacity
                                                        "  }",
                                                        "  $(row).css('background-color', bgColor);",
                                                        "}"
                                                      ),
                                                      columnDefs = list(list(targets = c(length(colnames(room_map()))), visible = FALSE))),escape=F
        )
      )
      
      output$Download_facility_map<-downloadHandler(
        filename=function(){
          paste("Fish stocks in_", fishroom_choice(), ".csv", sep="")
        },
        content=function(file){
          write.csv(room_map(),file,row.names = F)
        }
      )
      
      output$error_upload_fishmap<- renderText(
        ""
      )
      
    }
    
  })
  
  
  
  
  observeEvent(input$fishroom_upload,{
    
  })
  
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
                       selectizeInput(inputId = "Nursery_choices_add", label = "Select the Nursery Room: ", choices= NULL ,selected=NULL, multiple = F,options = list(create=F, placeholder = "Select Nursery Room")),
                       fluidRow(column(width = 3,
                                       actionButton(inputId = "nursery_add_select", label="Load"),
                       )),
                       div(id="add_larval_upload_hide",
                           strong(textOutput("nursery_room_load_success")),
                           
                           
                           br(),
                           numericInput(inputId = "nTanks_add_n", label="No. of tanks* :",value = 1 , min=1, max=20, step =1),
                           #numericInput(inputId = "Stock_no_add_n", label="Stock Number* :", value = 18000, min =18000),
                           selectizeInput(inputId = "Stock_no_add_n",label = "Stock Number* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter stock number")),
                           dateInput(inputId = "DOB_add_n",label = "Date of birth* :", max = Sys.Date(), format = "dd-MM-yyyy"),
                           selectizeInput(inputId = "Fish_name_add_n",label = "Fish name* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter your fish name")),
                           selectizeInput(inputId = "Fish_Genotype_add_n",label = "Genotype* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter your fish genotype")),
                           numericInput(inputId = "nFish_add_n", label="Total No. of fish (including all tanks) * :", value = 40 , min=1),
                           selectizeInput(inputId = "Fish_Responsible_add_n",label = "Responsible* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter owner name")),
                           numericInput(inputId = "Stock_no_fparent_add_n", label="Female Parent Stock Number* :", value = 18000, min =18000),
                           selectizeInput(inputId = "Fish_fParent_add_n",label = "Female Parent Genotype* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter female parent")),
                           numericInput(inputId = "Stock_no_Mparent_add_n", label="Male Parent Stock Number* :", value = 18000, min =18000),
                           selectizeInput(inputId = "Fish_MParent_add_n",label = "Male Parent Genotype* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter male parent")),
                           textInput(inputId = "Experiment_n",label = "Notes*: "),
                           
                           p(style=  "color:red",strong("*Fields are mandatory."
                           )),
                           actionButton(inputId = "Add_nursery", label="Submit"),
                           h4(style=  "color:red",textOutput("error_add_Nfish"))
                       )%>% shinyjs::hidden()),
                     
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
                       selectizeInput(inputId = "fishroom_choices_adult_add", label = "Select the Fish Room: ", choices= NULL ,selected=NULL, multiple = F,options = list(create=F, placeholder = "Select Fish Room")),
                       actionButton(inputId = "upload_adult_room", label="Load"),
                       br(),
                       div(id="add_adult_upload_hide",
                           strong(textOutput("adult_room_load_success")),
                           br(),
                           
                           selectizeInput(inputId = "Tank_name_adult", label = "Tank name* ", choices= NULL , options = list(create=F, placeholder = "Empty Tank location")),
                           #numericInput(inputId = "Stock_no_add_adult", label="Stock Number* :", value = 18000, min =18000),
                           selectizeInput(inputId = "Stock_no_add_adult",label = "Stock Number* :", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter stock number")),
                           dateInput(inputId = "DOB_add_adult",label = "Date of birth* :", max = Sys.Date(),format = "dd-MM-yyyy"),
                           selectizeInput(inputId = "Fish_name_add_adult",label = "Fish name* :", choices= NULL , options = list(create=T, placeholder = "Type Fish name")),
                           selectizeInput(inputId = "Fish_Genotype_add_adult",label = "Genotype* :", choices= NULL , options = list(create=T, placeholder = "Type Fish Genotype")),
                           numericInput(inputId = "nFish_add_adult", label="No of fish* :", value = 40 , min=1),
                           selectizeInput(inputId = "Fish_Responsible_add_adult",label = "Responsible* :", choices= NULL,options = list(create=T, placeholder = "Type Fish Owner")),
                           
                           selectizeInput(inputId = "Experiment_label_adult", label = "Experiment label*", choices= NULL ,multiple = T,options = list(create=T, placeholder = "Select Experiment label")),
                           
                           selectizeInput(inputId = "Food_label_adult", label = "Food label*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Available Food label")),
                           textInput(inputId = "Notes_add_adult",label = "Notes*: "),
                           
                           p(style=  "color:red",strong("*Fields are mandatory."
                           )),
                           actionButton(inputId = "Add_adult_Fish_stock", label="Submit"),
                           h4(style=  "color:red",textOutput("error_add_Afish"))
                       )%>% shinyjs::hidden()),
                     
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
                               strong(textOutput("adult_add_success")))
                         )
                     )%>% shinyjs::hidden()
                 )
                 
        )
      )
      
    )
  })
  ####################################################################################################################
  #########################Server part for adding larval stocks#############################################################
  nursery_rooms<-fishdb[["nursery_info"]][,1]
  adult_rooms<-fishdb[["adult_info"]][,1]
  
  observeEvent(input$tabs,{
    if(input$tabs=="add_stocks"){
      updateSelectizeInput(session, inputId = "Nursery_choices_add", choices = nursery_rooms, server = TRUE)
      updateSelectizeInput(session, inputId = "fishroom_choices_adult_add", choices = adult_rooms, server = TRUE)
    }
    
  })
  
  nursery_add_choice<-eventReactive(input$nursery_add_select,{input$Nursery_choices_add})
  
  output$nursery_add_conf_title<- renderText({
    paste0("Confirm the new fish stock information")
  })
  
  observeEvent(input$nursery_add_select,{
    #archive<-read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
    archive<-fishdb[["Archive"]]
    
    stockn_list<-as.numeric(unique(archive[,2]))
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
    
    output$nursery_room_load_success<-renderText(
      paste0("The room map of ",nursery_add_choice() ," is successfully loaded")
    )
    
    shinyjs::show("add_larval_upload_hide")
    
  })
  
  observeEvent(input$Add_nursery,{
    room_loc<-locate_room(fishdb[["nursery_info"]], nursery_add_choice())
    zebra1<-fishdb[[fishdb[["nursery_info"]]$db_list_name[room_loc]]]
    
    user_input<-c(input$nTanks_add_n,input$Stock_no_add_n,as.character(input$DOB_add_n),input$Fish_name_add_n,input$Fish_Genotype_add_n,
                  input$nFish_add_n,input$Fish_Responsible_add_n,input$Stock_no_fparent_add_n,input$Fish_fParent_add_n,
                  input$Stock_no_Mparent_add_n,input$Fish_MParent_add_n, input$Experiment_n)
    
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
        "ERROR !!! This Stock number is already present in this Nursery!!"
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
    room_loc<-locate_room(fishdb[["nursery_info"]], nursery_add_choice())
    zebra1<-fishdb[[fishdb[["nursery_info"]]$db_list_name[room_loc]]]
    
    
    #WRITING DATA INTO THE EXCEL
    log_data<-fishdb[["Database_logs"]]
    archive<-fishdb[["Archive"]]
    
    archive_stock_list<- archive[,2]
    dup_check_archive<-dup_check(input$Stock_no_add_n,archive_stock_list)
    if(dup_check_archive==T){
      dup_loc<-which(input$Stock_no_add_n==archive_stock_list)
      
      output$modal_output_dupArchive<-renderTable(archive[dup_loc,2:6])
      showModal(modalDialog(
        tagList(
          p(style= "color:red",strong("This stock number is already present in the archive !! ")),
          tableOutput("modal_output_dupArchive"),
          br(),
          p(strong("Stock number is unique to each fish. Therefore, do you wish to overwrite the old stock
                  number with the new information ?"))
        ),
        title = p(style="color:red",strong("WARNING: Confirm Overwriting Stock number")),
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
      
      
      
      fishdb$Archive<-archive
      fishdb$Database_logs<-log_data
      nursery_selected<-fishdb[["nursery_info"]]$db_list_name[room_loc]
      fishdb[[nursery_selected]]<-zebra1
      saveRDS(fishdb, "fishdatabase.rds")
      
      showModal(modalDialog(
        tagList(
          paste0("Your fish stock is been succesfully added to the ", input$Nursery_choices_add, " !!")
        ),
        title = p(style= "color:green",strong("SUCCESS !! ")),
        footer = tagList(actionButton("Nursery_add_modal_success","Close"))
      ))
      
    }
  })
  
  observeEvent(input$Nursery_add_modal_success,{
    reset("larval_addition")
    shinyjs::hide("add_nursery_cnfm_ui")
    shinyjs::hide("add_larval_upload_hide")
    removeModal()
    output$nursery_room_load_success<-renderText(
      paste0("Please load the nursery room map again!")
    )
  })
  
  observeEvent(input$Nursery_add_archive_dup,{
    room_loc<-locate_room(fishdb[["nursery_info"]], nursery_add_choice())
    zebra1<-fishdb[[fishdb[["nursery_info"]]$db_list_name[room_loc]]]
    
    #WRITING DATA INTO THE EXCEL after removing old archive entry with the same stock
    archive<-fishdb[["Archive"]]
    log_data<-fishdb[["Database_logs"]]
    
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
    
    
    
    fishdb$Archive<-archive
    fishdb$Database_logs<-log_data
    nursery_selected<-fishdb[["nursery_info"]]$db_list_name[room_loc]
    fishdb[[nursery_selected]]<-zebra1
    saveRDS(fishdb, "fishdatabase.rds")
    
    removeModal()
    
    showModal(modalDialog(
      tagList(
        
        paste0("The old archive is replaced and your fish stock is been succesfully added to the ", input$Nursery_choices_add, " !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("Nursery_dup_archive_add_modal_success","Close"))
    ))
  })
  
  observeEvent(input$Nursery_dup_archive_add_modal_success,{
    reset("larval_addition")
    shinyjs::hide("add_nursery_cnfm_ui")
    shinyjs::hide("add_larval_upload_hide")
    removeModal()
    output$nursery_room_load_success<-renderText(
      paste0("Please load the nursery room map again !!")
    )
  })
  
  
  observeEvent(input$reset_nursery_addition,{
    reset("larval_addition")
    shinyjs::hide("add_nursery_cnfm_ui")
    shinyjs::hide("add_larval_upload_hide")
  })
  
  observeEvent(input$reset_nursery_addition,{
    output$nursery_add_success<- renderText({
      paste0("Your form is been reset. Please confirm again to add stocks!!")
    })
    output$nursery_room_load_success<-renderText(
      paste0("Please load the nursery room map again !!")
    )
  })
  
  
  
  ###########################################################################################################################
  ##########server part of adding adult fish
  adult_add_choice<-eventReactive(input$upload_adult_room,{input$fishroom_choices_adult_add})
  observeEvent(input$upload_adult_room,{
    if(adult_add_choice()=="Johnson Fish Room"){
      adult_file <- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
    }else if (adult_add_choice()=="Streisinger Fish Room") {
      adult_file <- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
    }else if (adult_add_choice()=="Chien Fish Room") {
      adult_file <- read.csv(file = "Chien_room.csv", header = T, fill = T,encoding="UTF-8")
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
    
    success_text_adult_upload<-eventReactive(input$upload_adult_room,{
      paste0("The room map of ",adult_add_choice()  ," is successfully loaded !")
    })
    output$adult_room_load_success<- renderText({
      success_text_adult_upload()
    })
    
    shinyjs::show("add_adult_upload_hide")
    
    
  })
  
  observeEvent(input$Add_adult_Fish_stock,{
    user_input<- c(input$Tank_name_adult,input$Stock_no_add_adult,as.character(input$DOB_add_adult),input$Fish_name_add_adult,
                   input$Fish_Genotype_add_adult,input$nFish_add_adult,input$nFish_add_adult, input$Fish_Responsible_add_adult,
                   paste(input$Experiment_label_adult,collapse = " , "),input$Food_label_adult, input$Notes_add_adult
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
    }else if (adult_add_choice()=="Chien Fish Room") {
      adult_file <- read.csv(file = "Chien_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_Ad<-"Chien_room.csv"
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
    
    showModal(modalDialog(
      tagList(
        
        paste0("Your fish stock is been succesfully added to the ", adult_add_choice(), " !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_add_modal_success","Close"))
    ))
    updateSelectizeInput(session, inputId = "Tank_name_adult" ,choices = NULL,server= T)
  })
  
  observeEvent(input$adult_add_modal_success,{
    reset("adult_addition")
    shinyjs::hide("add_adult_cnfm_ui")
    shinyjs::hide("add_adult_upload_hide")
    removeModal()
    output$adult_room_load_success<- renderText({
      paste0("Please load the adult fishroom map again !! ")
    })
  })
  
  
  
  
  observeEvent(input$reset_adult_addition,{
    reset("adult_addition")
    shinyjs::hide("add_adult_cnfm_ui")
    shinyjs::hide("add_adult_upload_hide")
  })
  
  observeEvent(input$reset_adult_addition,{
    output$adult_add_success<- renderText({
      paste0("Your form is been reset. Please confirm again to add stocks!!")
    })
    
    output$adult_room_load_success<- renderText({
      paste0("Please load the adult fishroom map again !! ")
    })
  })
  
  ######################################################################################################################################
  ### UI codes for transferring fish stocks ############################################################################################
  
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
                       hr(),
                       div(id="nursery_transfer_upload_hide",
                           div(fluidRow(column(width=12, dataTableOutput("nursery_stock_transfer_quickview"),
                                               style = "width: 100%; overflow-x: auto;"))),
                           br(),
                           br(),
                           h3(strong("FROM :-")),
                           hr(),
                           br(),
                           verbatimTextOutput("selected_rows_nursery_stock_transfer_output"),
                           br(),
                           selectizeInput(inputId = "Select_nursery_stock_transfer", label = "Select Stock number to transfer*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type Stock number")),
                           actionButton(inputId = "nursery_transfer_select", label="Select"),
                           br(),
                           
                           hr(),
                           div(id="nursery_transfer_dest",
                               h3(strong("TO :-")),
                               hr(),
                               selectInput("nursery_choices_transfer_dest", "Select the Adult room: ", choices = list(
                                 "Johnson Fish Room",
                                 "Streisinger Fish Room",
                                 "Chien Fish Room"
                               )),
                               actionButton(inputId = "nursery_transfer_dest_select", label="Select"),
                               hr()
                           )%>% shinyjs::hidden(),
                           div(id="nursery_transfer",
                               
                               h5(strong(textOutput("nursery_transfer_room_selection"))),
                               br(),
                               selectizeInput(inputId = "Select_destination_tank_nursery_transfer", label = "Destination Tank name* (Tip: select multiple tanks to transfer simultaneously to several locations.)",multiple = T, selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                               
                               #numericInput(inputId = "nursery_transfer_n", label="No of fish to transfer* :", value = 1 , min=1),
                               selectizeInput(inputId = "nursery_transfer_n", label = "No of fish to transfer in each tank*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Fish Number to transfer")),
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
                           )%>% shinyjs::hidden())%>% shinyjs::hidden()
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
                         "Streisinger Fish Room",
                         "Chien Fish Room"
                       )),
                       
                       actionButton(inputId = "adultroom_transfer_upload", label="Upload"),
                       hr(),
                       div(id="adult_transfer_upload_hide",
                           div(fluidRow(column(width=12, dataTableOutput("adult_stock_transfer_quickview"),
                                               style = "width: 100%; overflow-x: auto;"))),
                           br(),
                           h3(strong("FROM :-")),
                           hr(),
                           br(),
                           verbatimTextOutput("selected_rows_adult_stock_transfer_output"),
                           br(),
                           selectizeInput(inputId = "adult_stock_transfer_source_name", label = "Select source Tank name*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                           actionButton(inputId = "adultroom_transfer_select", label="Select"),
                           h4(style=  "color:red",textOutput("error_adultroom_transfer_select")),
                           strong(textOutput("adult_transfer_room_selection")),
                           br(),
                           div(id="adult_transfer",
                               h3(strong("TO :-")),
                               hr(),
                               selectizeInput(inputId = "adult_stock_transfer_destination_name", label = "Select destination Tank name* : ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
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
                           )%>% shinyjs::hidden())%>% shinyjs::hidden()
                     )
                 )),
        tabPanel("Between Fish Rooms",
                 div(id="transfer_between_fish_adult",
                     wellPanel(
                       useShinyjs(),
                       p(style=  "color:blue",strong("Please select the required fish room first from the drop down list below and then click upload to update the 
                                                         current status of the respective fish room."
                       )),
                       selectInput("adult_choices_between_transfer", "Select the Fish room: ", choices = list(
                         "Johnson Fish Room",
                         "Streisinger Fish Room",
                         "Chien Fish Room"
                       )),
                       
                       actionButton(inputId = "adultroom_between_transfer_upload", label="Upload"),
                       hr(),
                       div(id="adult_transfer_between_upload_hide",
                           div(fluidRow(column(width=12, dataTableOutput("adult_stock_transfer_between_quickview"),
                                               style = "width: 100%; overflow-x: auto;"))),
                           br(),
                           h3(strong("FROM :-")),
                           hr(),
                           br(),
                           verbatimTextOutput("selected_rows_adult_stock_transfer_between_output"),
                           br(),
                           selectizeInput(inputId = "adult_stock_transfer_between_source_name", label = "Select source Tank name*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                           actionButton(inputId = "adultroom_transfer_between_select", label="Select"),
                           h4(style=  "color:red",textOutput("error_adultroom_transfer_between_select")),
                           strong(textOutput("adult_transfer_between_room_selection")),
                           br(),
                           div(id="adult_transfer_between_dest",
                               h3(strong("TO :-")),
                               hr(),
                               selectInput("adult_choices_transfer_between_dest", "Select the Adult room: ", choices = list(
                                 "Johnson Fish Room",
                                 "Streisinger Fish Room",
                                 "Chien Fish Room"
                               )),
                               actionButton(inputId = "adult_transfer_between_dest_select", label="Select"),
                               hr()
                           )%>% shinyjs::hidden(),
                           div(id="adult_transfer_between_tanks",
                               selectizeInput(inputId = "adult_stock_transfer_between_destination_name", label = "Select destination Tank name* : ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                               #numericInput(inputId = "adult_transfer_n", label="No of fish to transfer*:", value = 1 , min=1),
                               selectizeInput(inputId = "adult_transfer_between_n", label = "No of fish to transfer*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Fish Number to transfer")),
                               selectizeInput(inputId = "genotype_edit_adult_transfer_between", label = "Genotype*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Select genotype")),
                               selectizeInput(inputId = "Experiment_label_adult_transfer_between", label = "Experiment label*", choices= NULL ,multiple = T,options = list(create=T, placeholder = "Select Experiment label")),
                               selectizeInput(inputId = "Food_label_adult_adult_transfer_between", label = "Food label* ", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Available Food label")),
                               selectizeInput(inputId = "Notes_adult_transfer_between", label="Notes* : ", choices=NULL, selected = NULL ,options = list(create=T, placeholder = "Enter your notes")),
                               
                               p(style=  "color:red",strong("*All fields mandatory."
                               )),
                               actionButton(inputId = "adult_stock_transfer_between_source_submit", label="Submit"),
                               br(),
                               h4(style=  "color:red",textOutput("error_transfer_between_Adultfish")),
                               br(),
                               actionButton(inputId = "reset_adult_transfer_between", label="Reset form"),
                               br(),
                               br(),
                               strong(textOutput("adult_fish_transfer_between_success"))
                           )%>% shinyjs::hidden())%>% shinyjs::hidden()
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
    }else if(nursery_transfer_room()=="Johnson Nursery"){
      zebra_n<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
    }
    output$nursery_transfer_room_selection<- renderText({
      "" 
    })
    
    shinyjs::show("nursery_transfer_upload_hide")
    shinyjs::hide("nursery_transfer")
    shinyjs::hide("nursery_transfer_dest")
    
    nursery_stock_n<- list(zebra_n[,2])
    updateSelectizeInput(session, inputId = "Select_nursery_stock_transfer" ,choices = nursery_stock_n,server= T,selected = NULL)
    output$nursery_stock_transfer_quickview<-DT::renderDataTable(
      zebra_n,selection = "single",option=list(pageLength=5,lengthChange = TRUE,lengthMenu = c(5, 10, 20, 40, 80, 100) )
    )
    
    observeEvent(input$nursery_stock_transfer_quickview_rows_selected,{
      shinyjs::hide("nursery_transfer")
      shinyjs::hide("nursery_transfer_dest")
      selected_rows <- input$nursery_stock_transfer_quickview_rows_selected
      selected_data <- zebra_n[selected_rows, ]
      if(length(selected_rows)>0){
        updateSelectizeInput(session, inputId = "Select_nursery_stock_transfer" ,choices = nursery_stock_n,selected = selected_data[2],server= T)
        output$selected_rows_nursery_stock_transfer_output<- renderPrint(
          print(selected_data)
        )
      }
    })
  })
  
  observeEvent(input$nursery_transfer_select,{
    
    shinyjs::show("nursery_transfer_dest")
    shinyjs::hide("nursery_transfer")
    
  })
  
  nursery_transfer_dest_choice<- eventReactive(input$nursery_transfer_dest_select,{input$nursery_choices_transfer_dest})
  
  observeEvent(input$nursery_transfer_dest_select,{
    if (nursery_transfer_room()=="Walker Nursery"){
      zebra_n<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
    }else if(nursery_transfer_room()=="Johnson Nursery"){
      zebra_n<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
    }
    
    if(nursery_transfer_dest_choice()=="Streisinger Fish Room"){
      zebra_ad<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      output$nursery_transfer_room_selection<- renderText({
        "Your Fish will be transferred to the Streisinger room !!"
      })
    }else if (nursery_transfer_dest_choice()=="Johnson Fish Room"){
      zebra_ad<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      output$nursery_transfer_room_selection<- renderText({
        "Your Fish will be transferred to the Streisinger room !!"
      })
    } else if (nursery_transfer_dest_choice()=="Chien Fish Room"){
      zebra_ad<- read.csv(file = "Chien_room.csv", header = T, fill = T,encoding="UTF-8")
      output$nursery_transfer_room_selection<- renderText({
        "Your Fish will be transferred to the Chien room !!"
      })
    }
    
    shinyjs::show("nursery_transfer")
    #shinyjs::show("nursery_transfer_dest")
    
    from_stock_loc <- locate_stockn(zebra_n, input$Select_nursery_stock_transfer)
    destination_free_tanks<- list(find_empty_tanks(zebra_ad))
    updateSelectizeInput(session, inputId = "Select_destination_tank_nursery_transfer" ,choices = destination_free_tanks,server= T, selected = destination_free_tanks[1])
    
    fishn<- seq(1:(zebra_n[from_stock_loc,7]*2))
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
    
    # output$nursery_transfer_room_selection<- renderText({
    #   "" 
    # })
    # 
    output$selected_rows_nursery_stock_transfer_output<- renderPrint(
      print(zebra_n[from_stock_loc,])
    )
    
    
  })
  
  
  observeEvent(input$nursery_Fish_transfer_submit,{
    if (nursery_transfer_room()=="Walker Nursery"){
      zebra_n <-read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      #dest_room<- "Streisinger_room"
    }else if(nursery_transfer_room()=="Johnson Nursery"){
      zebra_n<-read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      # if(nursery_transfer_dest_choice()=="Streisinger Fish Room"){
      #   #dest_room<- "Streisinger_room"
      # }else{
      #   dest_room<- "Johnson_room"
      # }
    }
    
    if(nursery_transfer_dest_choice()=="Streisinger Fish Room"){
      dest_room<- "Streisinger_room"
    }else if (nursery_transfer_dest_choice()=="Johnson Fish Room"){
      dest_room<- "Johnson_room"
    } else if (nursery_transfer_dest_choice()=="Chien Fish Room"){
      dest_room<- "Chien_room"
    }
    
    from_stock_loc <- locate_stockn(zebra_n, input$Select_nursery_stock_transfer)
    transfer_nursery_name<-zebra_n[from_stock_loc,4]
    user_input<- c(input$Select_nursery_stock_transfer, paste(input$Select_destination_tank_nursery_transfer, collapse = " , "), input$nursery_transfer_n,
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
          paste0(" from ", nursery_transfer_room(), " to the tank location/s ", paste(input$Select_destination_tank_nursery_transfer, collapse = " , ")),
          paste0(" in the ", dest_room,".")
        ),
        title = strong("Confirm Transfer"),
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
      zebra_n<-read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      if(nursery_transfer_dest_choice()=="Streisinger Fish Room"){
        zebra_ad<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
        filename_ad<-"Streisinger_room.csv" 
        filename_nurs<- "Johnson nursery .csv"
        dest_room<- "Streisinger Room"
      }else{
        zebra_ad<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
        filename_ad<-"Johnson_room.csv"
        filename_nurs<- "Johnson nursery .csv"
        dest_room<- "Johnson Room"
        
      }
      
    }
    from_stock_loc <- locate_stockn(zebra_n, input$Select_nursery_stock_transfer)
    
    dest_loc<-input$Select_destination_tank_nursery_transfer
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    if (length(dest_loc>1)) {
      
      for (i in 1:length(dest_loc)){
        
        zebra_ad[locate_tank(zebra_ad,dest_loc[i]),]<- list(
          dest_loc[i], input$Select_nursery_stock_transfer, as.character(zebra_n[from_stock_loc,3]),zebra_n[from_stock_loc,4],
          zebra_n[from_stock_loc,5],zebra_n[from_stock_loc,6], input$nursery_transfer_n,
          paste(input$Experiment_label_nursery_transfer, collapse = " , "), input$Food_label_adult_nursery_transfer, input$Notes_nursery_trasnfer
        )
        
        log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),input$Select_nursery_stock_transfer,as.character(zebra_n[from_stock_loc,3]),
                                          zebra_n[from_stock_loc,4],zebra_n[from_stock_loc,5],zebra_n[from_stock_loc,6],
                                          input$nursery_transfer_n,
                                          paste0(" TRANSFERRED STOCK FROM ", nursery_transfer_room(), " TO ",dest_loc[i],
                                                 " IN ", dest_room))
        zebra_n[from_stock_loc,7]<- zebra_n[from_stock_loc,7]-as.numeric(input$nursery_transfer_n)
        
        
      }
      if(zebra_n[from_stock_loc,7]<0){
        zebra_n<-zebra_n[-from_stock_loc,]
      }
      
    }else{
      
      zebra_ad[locate_tank(zebra_ad,input$Select_destination_tank_nursery_transfer),]<- list(
        input$Select_destination_tank_nursery_transfer, input$Select_nursery_stock_transfer, as.character(zebra_n[from_stock_loc,3]),zebra_n[from_stock_loc,4],
        zebra_n[from_stock_loc,5],zebra_n[from_stock_loc,6], input$nursery_transfer_n,
        paste(input$Experiment_label_nursery_transfer, collapse = " , "), input$Food_label_adult_nursery_transfer, input$Notes_nursery_trasnfer
      )
      
      
      
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),input$Select_nursery_stock_transfer,as.character(zebra_n[from_stock_loc,3]),
                                        zebra_n[from_stock_loc,4],zebra_n[from_stock_loc,5],zebra_n[from_stock_loc,6],
                                        input$nursery_transfer_n,
                                        paste0(" TRANSFERRED STOCK FROM ", nursery_transfer_room(), " TO ",input$Select_destination_tank_nursery_transfer,
                                               " IN ", dest_room))
      
      
      
      if(zebra_n[from_stock_loc,7]<=as.numeric(input$nursery_transfer_n)){
        zebra_n<-zebra_n[-from_stock_loc,]
      }else{
        zebra_n[from_stock_loc,7]<- zebra_n[from_stock_loc,7]-as.numeric(input$nursery_transfer_n)
      }
      
      
      
    }
    write.csv(zebra_n,file = filename_nurs,row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    write.csv(zebra_ad,file = filename_ad, row.names = F)
    
    
    removeModal()
    
    showModal(modalDialog(
      tagList(
        
        paste0("Your stock is successfully transferred !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("nursery_transfer_modal_success","Close"))
    ))
    updateSelectizeInput(session, inputId = "Select_destination_tank_nursery_transfer" ,choices = NULL,server= T)
  })
  
  observeEvent(input$nursery_transfer_modal_success,{
    #reset("transfer fish_nursery")
    shinyjs::hide("nursery_transfer")
    shinyjs::hide("nursery_transfer_upload_hide")
    removeModal()
    output$nursery_transfer_room_selection<- renderText({
      "Please upload the nursery room again to make another transfer " 
    })
  })
  
  observeEvent(input$reset_nursery_transfer,{
    reset("transfer fish_nursery")
    shinyjs::hide("nursery_transfer")
    shinyjs::hide("nursery_transfer_upload_hide")
  })
  
  observeEvent(input$reset_nursery_transfer,{
    output$nursery_fish_transfer_success<- renderText({
      paste0("Your form is been reset. Please submit again to transfer stocks!!")
    })
    output$nursery_transfer_room_selection<- renderText({
      "Please upload the nursery room again to make another transfer !!" 
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
      zebra_ad, selection = "single",option=list(pageLength=10,lengthChange = TRUE,lengthMenu = c(5, 10, 20, 40, 80, 100))
    )
    
    output$adult_transfer_room_selection<- renderText({
      "" 
    })
    
    shinyjs::show("adult_transfer_upload_hide")
    
    
    occupiedtanks_at<-find_occupied_tanks(zebra_ad)
    updateSelectizeInput(session, inputId = "adult_stock_transfer_source_name" ,choices = occupiedtanks_at,server= T,selected = NULL)
    
    observeEvent(input$adult_stock_transfer_quickview_rows_selected,{
      shinyjs::hide("adult_transfer")
      selected_rows <- input$adult_stock_transfer_quickview_rows_selected
      selected_data <- zebra_ad[selected_rows, ]
      if(length(selected_rows)>0){
        updateSelectizeInput(session, inputId = "adult_stock_transfer_source_name" ,choices = occupiedtanks_at,server= T, selected =selected_data[1])
        output$selected_rows_adult_stock_transfer_output<- renderPrint(
          print(selected_data)
        )
      }
    })
    
  })
  
  
  
  observeEvent(input$adultroom_transfer_select, {
    
    if (adult_transfer_roomchoice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8" )
      filename_ad<- "Streisinger_room.csv"
      
    }else if(adult_transfer_roomchoice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    
    qc<- check_errors(input$adult_stock_transfer_source_name)
    
    if(qc==T){
      output$error_adultroom_transfer_select<-renderText(
        "ERROR !!! You must select a non-empty tank for transfer!!"
      )
      shinyjs::hide("adult_transfer")
      
    }else{
      output$error_adultroom_transfer_select<-renderText(
        ""
      )
      isolate({
        shinyjs::show("adult_transfer")
        emptytanks_at<- find_empty_tanks(zebra_ad)
        updateSelectizeInput(session, inputId = "adult_stock_transfer_destination_name" ,choices = emptytanks_at,server= T,selected = NULL)
        
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
        
        output$selected_rows_adult_stock_transfer_output<- renderPrint(
          print(zebra_ad[adt_location,])
        )
      })
      
    }
    
    
    output$adult_transfer_room_selection<- renderText({
      "" 
    })
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
        title = strong("Confirm Transfer"),
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
    
    
    if(as.numeric(zebra_ad[from_stock_loc,7])<=as.numeric(input$adult_transfer_n)){
      zebra_ad[from_stock_loc,2:10]<-NA
    }else{
      zebra_ad[from_stock_loc,7]<- as.numeric(zebra_ad[from_stock_loc,7])-as.numeric(input$adult_transfer_n)
    }
    
    write.csv(zebra_ad,file = filename_ad,row.names = F)
    
    removeModal()
    
    showModal(modalDialog(
      tagList(
        
        paste0("Your stock is successfully transferred !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_transfer_modal_success","Close"))
    ))
    updateSelectizeInput(session, inputId = "adult_stock_transfer_destination_name" ,choices = NULL,server= T)
    
  })
  
  observeEvent(input$adult_transfer_modal_success,{
    #reset("transfer fish_nursery")
    shinyjs::hide("adult_transfer")
    shinyjs::hide("adult_transfer_upload_hide")
    removeModal()
    output$adult_transfer_room_selection<- renderText({
      "Please upload the fish room map again to make another transfer !!!" 
    })
  })
  
  observeEvent(input$reset_adult_transfer,{
    reset("transfer fish_adult")
    shinyjs::hide("adult_transfer")
    shinyjs::hide("adult_transfer_upload_hide")
    output$adult_transfer_room_selection<- renderText({
      "Please upload the fish room map again to make another transfer !!!" 
    })
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
           You can change the fish number, Genotype, Experiment label, Food label, resposible
           and notes. Once you submit the form, you will be asked to verify your edits and finally,  click the confirm button. 
           Once you confirm, your actions will be logged. You can edit another fish by hitting the reset button. If you click on the Euthanize button, all the fish of the selected
           stock number/tank location
           will be removed from the fish room map.There are two separate tabs available for making individual tanks as well as bulk edits.
           ")),
      br(),
      br(),
      tabsetPanel(
        tabPanel("Individual Tank Editing",
                 wellPanel(
                   useShinyjs(),
                   div(id="edit_fish_room_options",
                       
                       p(style=  "color:blue",strong("Please select the required fish room first from the drop down list below and then click upload to update the 
                                                         current status of the respective fish room. Then click on any fish that you wish to edit."
                       )),
                       selectInput("edit_fish_room_choices", "Select the Fish room: ", choices = list(
                         "Johnson Fish Room",
                         "Streisinger Fish Room",
                         "Walker Nursery",
                         "Johnson Nursery"
                       )),
                       actionButton(inputId = "edit_fish_room_upload", label="Upload"),
                       div(id="edit_fish_room_upload_hide",
                           hr(),
                           div(dataTableOutput("edit_fish_room_quickview"),
                               style = "width: 100%; overflow-x: auto;"),
                           
                       )%>% shinyjs::hidden()),
                   
                   div(id= "edit_adult_room",
                       
                       h3(strong("Select Adult Fish :-")),
                       hr(),
                       br(),
                       verbatimTextOutput("selected_rows_edit_Afish_targets_output"),
                       br(),
                       selectizeInput(inputId = "edit_Afish_targets", label = "Available Tank names*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type tank name")),
                       fluidRow(column(width = 3, actionButton(inputId = "edit_Afish", label="Edit")), column(width = 3,  actionButton(inputId = "Euthanise_AFish", label = "Euthanize All"))),
                       h4(style=  "color:red",textOutput("error_edit_Afish_select")),
                       strong(textOutput("edit_adult_fish_message")),
                       strong(textOutput("ethanize_Afish_target_success")),
                       hr(),
                       div(id="edit_Adult_stocks",
                           selectizeInput(inputId = "genotype_edit_Afish_target", label = "Genotype*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit Genotype")),
                           selectizeInput(inputId = "responsible_edit_Afish_target", label = "Responsible*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit Owner")),
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
                       br(),
                       verbatimTextOutput("selected_rows_edit_Nfish_targets_output"),
                       br(),
                       selectizeInput(inputId = "edit_Nfish_targets", label = "Available Stock numbers*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type Stock")),
                       fluidRow(column(width = 3, actionButton(inputId = "edit_Nfish", label="Edit")), column(width = 3,  actionButton(inputId = "Euthanise_NFish", label = "Euthanize All"))),
                       strong(textOutput("edit_nursery_fish_message")),
                       strong(textOutput("ethanize_Nfish_target_success")),
                       hr(),
                       div(id="edit_nursery_stock",
                           selectizeInput(inputId = "TankNumber_edit_Nfish_target", label = "No. of Tanks*", choices= NULL ,multiple = F,options = list(create=F, placeholder = "Edit Tank Number")),
                           selectizeInput(inputId = "genotype_edit_Nfish_target", label = "Genotype*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit Genotype")),
                           selectizeInput(inputId = "responsible_edit_Nfish_target", label = "Responsible*", choices= NULL ,multiple = F,options = list(create=T, placeholder = "Edit Owner")),
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
        ),
        tabPanel("Bulk Editing",
                 wellPanel(
                   useShinyjs(),
                   div(id="bulk_edit_fish_room_options",
                       
                       p(style=  "color:blue",strong("Please select the required fish room first from the drop down list below and then click upload to update the 
                                                         current status of the respective fish room. Here, you can select multiple tanks - in the adult room or
                                                     multiple stock numbers -in the nursery room to make bulk edit. Selection can be done byselecting the rows in the table below."
                       )),
                       selectInput("bulk_edit_fish_room_choices", "Select the Fish room: ", choices = list(
                         "Johnson Fish Room",
                         "Streisinger Fish Room",
                         "Walker Nursery",
                         "Johnson Nursery"
                       )),
                       actionButton(inputId = "bulk_edit_fish_room_upload", label="Upload"),
                       div(id="bulk_edit_fish_room_upload_hide",
                           hr(),
                           div(dataTableOutput("bulk_edit_fish_room_quickview"),
                               style = "width: 100%; overflow-x: auto;"),
                           
                       )%>% shinyjs::hidden()),
                   div(id= "bulk_edit_adult_room",
                       
                       h3(strong("Select Adult Fish :-")),
                       hr(),
                       
                       verbatimTextOutput("selected_rows_bulk_edit_Afish_targets_output"),
                       br(),
                       fluidRow(
                         actionButton(inputId = "bulkedit_Resp", label="Edit Responsible"),
                         actionButton(inputId = "bulkedit_Explabel", label="Edit Experiment Label"),
                         actionButton(inputId = "bulkedit_Fdlabel", label="Edit Food Label"),
                         actionButton(inputId = "bulkedit_Notes", label="Edit Notes"),
                         actionButton(inputId = "bulkedit_Euthanize", label="Euthanize All"),
                       ),
                       br(),
                       div(id="bulkedit_resp_ad",
                           selectizeInput(inputId = "bulk_edit_resp_Ad_choice", label = "Reponsible*: ", selected=NULL,choices= NULL ,multiple=F,options = list(create=T, placeholder = "Owner Name")),
                           actionButton(inputId = "bulk_edit_resp_Ad_choice_submit", label="Submit")
                       )%>% shinyjs::hidden(),
                       div(id="bulkedit_explabel_ad",
                           selectizeInput(inputId = "bulk_edit_explabel_Ad_choice", label = "Experiment Label*: ", selected=NULL,choices= NULL ,multiple=F,options = list(create=T, placeholder = "Experiment Label")),
                           actionButton(inputId = "bulk_edit_explabel_Ad_choice_submit", label="Submit")
                       )%>% shinyjs::hidden(),
                       div(id="bulkedit_foodlabel_ad",
                           selectizeInput(inputId = "bulk_edit_foodlabel_Ad_choice", label = "Food Label*: ", selected=NULL,choices= NULL ,multiple=F,options = list(create=T, placeholder = "Food Label")),
                           actionButton(inputId = "bulk_edit_foodlabel_Ad_choice_submit", label="Submit")
                       )%>% shinyjs::hidden(),
                       div(id="bulkedit_Notes_ad",
                           selectizeInput(inputId = "bulk_edit_notes_Ad_choice", label = "Notes*: ", selected=NULL,choices= NULL ,multiple=F,options = list(create=T, placeholder = "Notes")),
                           actionButton(inputId = "bulk_edit_notes_Ad_choice_submit", label="Submit")
                       )%>% shinyjs::hidden(),
                       h4(style=  "color:red",textOutput("bulk_edit_adult_fish_message"))
                       
                   )%>% shinyjs::hidden(),
                   
                   div(id= "bulk_edit_nursery_room",
                       h3(strong("Select Nursery Fish :-")),
                       hr(),
                       verbatimTextOutput("selected_rows_bulk_edit_Nfish_targets_output"),
                       br(),
                       fluidRow(
                         actionButton(inputId = "bulkedit_Resp_N", label="Edit Responsible"),
                         actionButton(inputId = "bulkedit_Notes_N", label="Edit Notes"),
                         actionButton(inputId = "bulkedit_Euthanize_N", label="Euthanize All"),
                       ),
                       br(),
                       br(),
                       div(id="bulkedit_resp_N",
                           selectizeInput(inputId = "bulk_edit_resp_N_choice", label = "Reponsible*: ", selected=NULL,choices= NULL ,multiple=F,options = list(create=T, placeholder = "Owner Name")),
                           actionButton(inputId = "bulk_edit_resp_N_choice_submit", label="Submit")
                       )%>% shinyjs::hidden(),
                       div(id="bulkedit_notes_N",
                           selectizeInput(inputId = "bulk_edit_notes_N_choice", label = "Notes*: ", selected=NULL,choices= NULL ,multiple=F,options = list(create=T, placeholder = "Notes")),
                           actionButton(inputId = "bulk_edit_notes_N_choice_submit", label="Submit")
                       )%>% shinyjs::hidden(),
                       h4(style=  "color:red",textOutput("bulk_edit_N_fish_message"))
                       
                   )%>% shinyjs::hidden()
                   
                   
                 )
        )
      )
    )})
  ###################################################Server side code for bulk edit adult fish#############################################################################
  
  
  
  
  bulk_edit_fishroom_choice<-eventReactive(input$bulk_edit_fish_room_upload,{input$bulk_edit_fish_room_choices})
  bulk_edit_file<-eventReactive(input$bulk_edit_fish_room_upload,{
    if (bulk_edit_fishroom_choice()=="Johnson Fish Room"){
      zebra<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
    }else if(bulk_edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
    }else if(bulk_edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      
    }else if(bulk_edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
    }
  })
  
  bulk_edit_file_name<-eventReactive(input$bulk_edit_fish_room_upload,{
    if (bulk_edit_fishroom_choice()=="Johnson Fish Room"){
      zebra<- "Johnson_room.csv"
    }else if(bulk_edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra<- "Streisinger_room.csv"
    }else if(bulk_edit_fishroom_choice()=="Walker Nursery"){
      zebra<- "Walker nursery.csv"
    }else if(bulk_edit_fishroom_choice()=="Johnson Nursery"){
      zebra<-"Johnson nursery .csv"
    }
  })
  
  observeEvent(input$bulk_edit_fish_room_upload,{
    if (bulk_edit_fishroom_choice()=="Johnson Fish Room"){
      zebra<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      exp_label_list<-unique(zebra[,8])
      notes_list<-unique(zebra[,10])
      
    }else if(bulk_edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      tank_names <-find_occupied_tanks(zebra)
      exp_label_list<-unique(zebra[,8])
      notes_list<-unique(zebra[,10])
      
    }else if(bulk_edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      stock_numbers<- zebra[,2]
      notes_list<-unique(zebra[,8])
      exp_label_list<-""
      
      
    }else if(bulk_edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      stock_numbers<- zebra[,2]
      notes_list<-unique(zebra[,8])
      exp_label_list<-""
    }
    
    output$bulk_edit_fish_room_quickview<-DT::renderDataTable(
      zebra,option=list(pageLength=5,lengthChange = TRUE,lengthMenu = c(5, 10, 20, 40, 80, 100))
    )
    
    resp_list<-unique(zebra[,6])
    resp_list<-append(resp_list,"MHM",after = 0)
    updateSelectizeInput(session, inputId = "bulk_edit_resp_Ad_choice" ,choices = resp_list,server= T,selected = resp_list[1])
    updateSelectizeInput(session, inputId = "bulk_edit_resp_N_choice" ,choices = resp_list,server= T,selected = resp_list[1])
    
    exp_label_list<-append(exp_label_list,"Enter Experiment Label",after = 0)
    updateSelectizeInput(session, inputId = "bulk_edit_explabel_Ad_choice" ,choices = exp_label_list,server= T,selected = exp_label_list[1])
    
    food_label<-list("JA","JB","JC","JD","JE","JF","JG","JH","JI","JJ","SA","SB","SC","SD","SE","SF","SG","SH","SI","SJ",
                     "AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ")
    updateSelectizeInput(session, inputId = "bulk_edit_foodlabel_Ad_choice" ,choices = food_label,server= T,selected = food_label[1])
    
    notes_list<-append(notes_list,"Enter notes",after = 0)
    updateSelectizeInput(session, inputId = "bulk_edit_notes_Ad_choice" ,choices = notes_list,server= T,selected = notes_list[1])
    updateSelectizeInput(session, inputId = "bulk_edit_notes_N_choice" ,choices = notes_list,server= T,selected = notes_list[1])
    
    
    
    observeEvent(input$bulk_edit_fish_room_quickview_rows_selected,{
      selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
      if(length(selected_rows)>0){
        if(bulk_edit_fishroom_choice()=="Johnson Fish Room"||bulk_edit_fishroom_choice()=="Streisinger Fish Room"){
          selected_data1 <- zebra[selected_rows, ]
          tank_names <-find_occupied_tanks(zebra)
          #updateSelectizeInput(session, inputId = "bulk_edit_Afish_targets" ,choices = tank_names,server= T, selected = selected_data1[1])
          output$selected_rows_bulk_edit_Afish_targets_output<- renderPrint(
            print(selected_data1)
          )
          shinyjs::hide("bulk_edit_Adult_stocks")
          shinyjs::show("bulk_edit_adult_room")
        }else if(bulk_edit_fishroom_choice()=="Walker Nursery"||bulk_edit_fishroom_choice()=="Johnson Nursery"){
          selected_data2 <- zebra[selected_rows, ]
          stock_numbers<- zebra[,2]
          # updateSelectizeInput(session, inputId = "bulk_edit_Nfish_targets" ,choices = stock_numbers,server= T, selected = selected_data2[2])
          output$selected_rows_bulk_edit_Nfish_targets_output<- renderPrint(
            print(selected_data2)
          )
          shinyjs::hide("bulk_edit_nursery_stock")
          shinyjs::show("bulk_edit_nursery_room")
        }
        
      }
    })
    
  })
  
  observeEvent(input$bulk_edit_fish_room_upload,{
    shinyjs::show("bulk_edit_fish_room_upload_hide")
    if(input$bulk_edit_fish_room_choices=="Johnson Fish Room"||input$bulk_edit_fish_room_choices=="Streisinger Fish Room"){
      shinyjs::hide("bulk_edit_adult_room")
      shinyjs::hide("bulk_edit_nursery_room")
    }else if (input$bulk_edit_fish_room_choices=="Walker Nursery"||input$bulk_edit_fish_room_choices=="Johnson Nursery"){
      shinyjs::hide("bulk_edit_nursery_room")
      shinyjs::hide("bulk_edit_adult_room")
    }})
  
  
  #### Hiding and showing bulk edit menus
  
  
  observeEvent(input$bulkedit_Resp,{
    shinyjs::show("bulkedit_resp_ad")
    shinyjs::hide("bulkedit_explabel_ad")
    shinyjs::hide("bulkedit_foodlabel_ad")
    shinyjs::hide("bulkedit_Notes_ad")
  })
  
  observeEvent(input$bulkedit_Explabel,{
    shinyjs::show("bulkedit_explabel_ad")
    shinyjs::hide("bulkedit_foodlabel_ad")
    shinyjs::hide("bulkedit_Notes_ad")
    shinyjs::hide("bulkedit_resp_ad")
  })
  
  observeEvent(input$bulkedit_Fdlabel,{
    shinyjs::show("bulkedit_foodlabel_ad")
    shinyjs::hide("bulkedit_resp_ad")
    shinyjs::hide("bulkedit_explabel_ad")
    shinyjs::hide("bulkedit_Notes_ad")
  })
  
  observeEvent(input$bulkedit_Notes,{
    shinyjs::show("bulkedit_Notes_ad")
    shinyjs::hide("bulkedit_explabel_ad")
    shinyjs::hide("bulkedit_foodlabel_ad")
    shinyjs::hide("bulkedit_resp_ad")
  })
  
  observeEvent(input$bulkedit_Resp_N,{
    shinyjs::show("bulkedit_resp_N")
    shinyjs::hide("bulkedit_notes_N")
  })
  
  observeEvent(input$bulkedit_Notes_N,{
    shinyjs::hide("bulkedit_resp_N")
    shinyjs::show("bulkedit_notes_N")
  })
  
  
  
  
  
  ##making bulk edit responsible adult
  observeEvent(input$bulk_edit_resp_Ad_choice_submit,{
    qc<-check_errors(input$bulk_edit_resp_Ad_choice)
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    empty_check<- empty_selection_check(selected_rows,bulk_edit_file())
    
    if (qc==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You cannot leave this field empty"
      )}
    
    else if(empty_check==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You selected an empty tank to edit"
      )
      
    }else{
      output$bulk_edit_adult_fish_message<-renderText(
        ""
      )
      
      output$modal_output_bulk_resp_table<-renderTable(bulk_edit_file()[selected_rows, 1:4])
      showModal(modalDialog(
        tagList(
          paste0("Please confirm editing the resposible person for the following to ",input$bulk_edit_resp_Ad_choice),
          br(),
          hr(),
          tableOutput("modal_output_bulk_resp_table"),
        ),
        title = strong("CONFIRM EDITS"),
        footer = tagList(actionButton("AFish_bulkedit_resp_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
  })
  
  ##writing responsible edits to the excel
  observeEvent(input$AFish_bulkedit_resp_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      #print(bulk_edit_file()[selected_rows[i],6])
      data[selected_rows[i],6]<- input$bulk_edit_resp_Ad_choice
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], input$bulk_edit_resp_Ad_choice,
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EDITED THE RESPONSIBLE PERSON NAME "))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_bulkedit_resp_modal_success","Close"))
    ))
  })
  
  observeEvent(input$adult_bulkedit_resp_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  
  ##making bulk edit Experimental label adult
  observeEvent(input$bulk_edit_explabel_Ad_choice_submit,{
    qc<-check_errors(input$bulk_edit_explabel_Ad_choice)
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    empty_check<- empty_selection_check(selected_rows,bulk_edit_file())
    
    if (qc==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You cannot leave this field empty"
      )}
    
    else if(empty_check==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You selected an empty tank to edit"
      )
      
    }else{
      output$bulk_edit_adult_fish_message<-renderText(
        ""
      )
      
      output$modal_output_bulk_explabel_table<-renderTable(bulk_edit_file()[selected_rows, 1:4])
      showModal(modalDialog(
        tagList(
          paste0("Please confirm editing the experiment label for the following to ",input$bulk_edit_explabel_Ad_choice),
          br(),
          hr(),
          tableOutput("modal_output_bulk_explabel_table"),
        ),
        title = strong("CONFIRM EDITS"),
        footer = tagList(actionButton("AFish_bulkedit_explabel_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
  })
  
  ##writing to the excel experiment label
  observeEvent(input$AFish_bulkedit_explabel_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      #print(bulk_edit_file()[selected_rows[i],6])
      data[selected_rows[i],8]<- input$bulk_edit_explabel_Ad_choice
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], bulk_edit_file()[selected_rows[i],6],
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EDITED THE EXPERIMENT LABEL TO ", input$bulk_edit_explabel_Ad_choice))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_bulkedit_explabel_modal_success","Close"))
    ))
  })
  
  observeEvent(input$adult_bulkedit_explabel_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  ##making bulk edit Food label adult
  observeEvent(input$bulk_edit_foodlabel_Ad_choice_submit,{
    qc<-check_errors(input$bulk_edit_foodlabel_Ad_choice)
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    empty_check<- empty_selection_check(selected_rows,bulk_edit_file())
    
    if (qc==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You cannot leave this field empty"
      )}
    
    else if(empty_check==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You selected an empty tank to edit"
      )
      
    }else{
      output$bulk_edit_adult_fish_message<-renderText(
        ""
      )
      
      output$modal_output_bulk_foodlabel_table<-renderTable(bulk_edit_file()[selected_rows, 1:4])
      showModal(modalDialog(
        tagList(
          paste0("Please confirm editing the food label for the following to ",input$bulk_edit_foodlabel_Ad_choice),
          br(),
          hr(),
          tableOutput("modal_output_bulk_foodlabel_table"),
        ),
        title = strong("CONFIRM EDITS"),
        footer = tagList(actionButton("AFish_bulkedit_foodlabel_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
  })
  
  ##writing to the excel food label
  observeEvent(input$AFish_bulkedit_foodlabel_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      #print(bulk_edit_file()[selected_rows[i],6])
      data[selected_rows[i],9]<- input$bulk_edit_foodlabel_Ad_choice
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], bulk_edit_file()[selected_rows[i],6],
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EDITED THE FOOD LABEL TO ", input$bulk_edit_foodlabel_Ad_choice))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_bulkedit_foodlabel_modal_success","Close"))
    ))
  })
  
  observeEvent(input$adult_bulkedit_foodlabel_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  
  ##making bulk edit notes adult
  observeEvent(input$bulk_edit_notes_Ad_choice_submit,{
    qc<-check_errors(input$bulk_edit_notes_Ad_choice)
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    empty_check<- empty_selection_check(selected_rows,bulk_edit_file())
    
    if (qc==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You cannot leave this field empty"
      )}
    
    else if(empty_check==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You selected an empty tank to edit"
      )
      
    }else{
      output$bulk_edit_adult_fish_message<-renderText(
        ""
      )
      
      output$modal_output_bulk_notes_table<-renderTable(bulk_edit_file()[selected_rows, 1:4])
      showModal(modalDialog(
        tagList(
          paste0("Please confirm editing the food label for the following to ",input$bulk_edit_notes_Ad_choice),
          br(),
          hr(),
          tableOutput("modal_output_bulk_notes_table"),
        ),
        title = strong("CONFIRM EDITS"),
        footer = tagList(actionButton("AFish_bulkedit_notes_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
  })
  
  ##writing to the excel food label
  observeEvent(input$AFish_bulkedit_notes_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      #print(bulk_edit_file()[selected_rows[i],6])
      data[selected_rows[i],10]<- input$bulk_edit_notes_Ad_choice
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], bulk_edit_file()[selected_rows[i],6],
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EDITED THE NOTES TO ", input$bulk_edit_notes_Ad_choice))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_bulkedit_notes_modal_success","Close"))
    ))
  })
  
  observeEvent(input$adult_bulkedit_notes_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  #####Bulk euthanizing all the fish
  
  observeEvent(input$bulkedit_Euthanize,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    
    empty_check<- empty_selection_check(selected_rows,bulk_edit_file())
    
    if(empty_check==T){
      output$bulk_edit_adult_fish_message<-renderText(
        "ERROR !!! You selected an empty tank to edit"
      )
    }else{
      output$bulk_edit_adult_fish_message<-renderText(
        ""
      )
      
      output$modal_output_bulk_euthanize_table<-renderTable(bulk_edit_file()[selected_rows, 1:4])
      showModal(modalDialog(
        tagList(
          paste0("Please confirm EUTHANIZING all the following fish."),
          br(),
          hr(),
          tableOutput("modal_output_bulk_euthanize_table"),
        ),
        title = p(style="color:red",strong("CONFIRM EUTHANIZE ALL")),
        footer = tagList(actionButton("bulk_AFish_euthanize_all_confirm","Confirm"), modalButton("Cancel"))
      ))
      
    }
    
    
  })
  
  observeEvent(input$bulk_AFish_euthanize_all_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      #print(bulk_edit_file()[selected_rows[i],6])
      data[selected_rows[i],2:10]<- NA
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], bulk_edit_file()[selected_rows[i],6],
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EUTHANIZED THE STOCK "))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully EUTHANIZED !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_bulkedit_euthanize_modal_success","Close"))
    ))
    
  })
  
  observeEvent(input$adult_bulkedit_euthanize_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  
  ####Bulk editing nursery fish responsible
  
  observeEvent(input$bulk_edit_resp_N_choice_submit,{
    qc<-check_errors(input$bulk_edit_resp_N_choice)
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    if (qc==T){
      output$bulk_edit_N_fish_message<-renderText(
        "ERROR !!! You cannot leave this field empty"
      )}
    
    else{
      output$bulk_edit_N_fish_message<-renderText(
        ""
      )
      
      output$modal_output_bulk_resp_table_N<-renderTable(bulk_edit_file()[selected_rows, 1:4])
      showModal(modalDialog(
        tagList(
          paste0("Please confirm editing the resposible person for the following stocks to ",input$bulk_edit_resp_N_choice),
          br(),
          hr(),
          tableOutput("modal_output_bulk_resp_table_N"),
        ),
        title = strong("CONFIRM EDITS"),
        footer = tagList(actionButton("NFish_bulkedit_resp_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
  })
  
  ##writing bulk responsible edits Nursery to the excel
  observeEvent(input$NFish_bulkedit_resp_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      #print(bulk_edit_file()[selected_rows[i],6])
      data[selected_rows[i],6]<- input$bulk_edit_resp_N_choice
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], input$bulk_edit_resp_N_choice,
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EDITED THE RESPONSIBLE PERSON NAME "))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("N_bulkedit_resp_modal_success","Close"))
    ))
  })
  
  observeEvent(input$N_bulkedit_resp_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  ####Bulk editing nursery fish notes
  
  observeEvent(input$bulk_edit_notes_N_choice_submit,{
    qc<-check_errors(input$bulk_edit_notes_N_choice)
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    if (qc==T){
      output$bulk_edit_N_fish_message<-renderText(
        "ERROR !!! You cannot leave this field empty"
      )}
    
    else{
      output$bulk_edit_N_fish_message<-renderText(
        ""
      )
      
      output$modal_output_bulk_notes_table_N<-renderTable(bulk_edit_file()[selected_rows, 1:4])
      showModal(modalDialog(
        tagList(
          paste0("Please confirm editing the notes for the following stocks to ",input$bulk_edit_notes_N_choice),
          br(),
          hr(),
          tableOutput("modal_output_bulk_notes_table_N"),
        ),
        title = strong("CONFIRM EDITS"),
        footer = tagList(actionButton("NFish_bulkedit_notes_confirm","Confirm"), modalButton("Cancel"))
      ))
    }
    
  })
  
  ##writing bulk notes edits Nursery to the excel
  observeEvent(input$NFish_bulkedit_notes_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      #print(bulk_edit_file()[selected_rows[i],6])
      data[selected_rows[i],8]<- input$bulk_edit_notes_N_choice
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], bulk_edit_file()[selected_rows[i],6],
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EDITED THE NOTES to ", input$bulk_edit_notes_N_choice))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("N_bulkedit_notes_modal_success","Close"))
    ))
  })
  
  observeEvent(input$N_bulkedit_notes_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  
  #####Bulk euthanizing all the nursery fish
  
  observeEvent(input$bulkedit_Euthanize_N,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    
    output$modal_output_bulk_euthanize_table_N<-renderTable(bulk_edit_file()[selected_rows, 1:4])
    showModal(modalDialog(
      tagList(
        paste0("Please confirm EUTHANIZING all the following fish."),
        br(),
        hr(),
        tableOutput("modal_output_bulk_euthanize_table_N"),
      ),
      title = p(style="color:red",strong("CONFIRM EUTHANIZE ALL")),
      footer = tagList(actionButton("bulk_NFish_euthanize_all_confirm","Confirm"), modalButton("Cancel"))
    ))
  })
  
  observeEvent(input$bulk_NFish_euthanize_all_confirm,{
    selected_rows <- input$bulk_edit_fish_room_quickview_rows_selected
    data<-bulk_edit_file()
    for( i in 1:length(selected_rows)){
      selected_rows<-sort(selected_rows, decreasing = T)
      print(selected_rows)
      data<-data[-selected_rows[i],]
    }
    
    ##logging in the edit
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    for (i in 1:length(selected_rows)) {
      log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),bulk_edit_file()[selected_rows[i],2], as.character(bulk_edit_file()[selected_rows[i],3]),
                                        bulk_edit_file()[selected_rows[i],4],bulk_edit_file()[selected_rows[i],5], bulk_edit_file()[selected_rows[i],6],
                                        bulk_edit_file()[selected_rows[i],7],
                                        paste0("EUTHANIZED THE STOCK FROM NURSERY "))
      
    }
    
    ###saving all changes
    write.csv(data,file = bulk_edit_file_name(),row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    
    removeModal()
    showModal(modalDialog(
      tagList(
        
        paste0("Your stocks are successfully EUTHANIZED !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("N_bulkedit_euthanize_modal_success","Close"))
    ))
    
  })
  
  observeEvent(input$N_bulkedit_euthanize_modal_success,{
    removeModal()
    shinyjs::hide("bulk_edit_adult_room")
    shinyjs::hide("bulk_edit_nursery_room")
    shinyjs::hide("bulk_edit_fish_room_upload_hide")
  })
  
  
  
  
  
  #########################################Server side code for editing Adult Fish################################################################################################
  ###############################################################################################################################################################################
  
  observeEvent(input$edit_fish_room_upload,{
    shinyjs::show("edit_fish_room_upload_hide")
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
      updateSelectizeInput(session, inputId = "edit_Afish_targets" ,choices = tank_names,server= T,selected = NULL)
    }else if(edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      tank_names <-find_occupied_tanks(zebra)
      updateSelectizeInput(session, inputId = "edit_Afish_targets" ,choices = tank_names,server= T,selected = NULL)
    }else if(edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      stock_numbers<- zebra[,2]
      updateSelectizeInput(session, inputId = "edit_Nfish_targets" ,choices = stock_numbers,server= T,selected = NULL)
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      stock_numbers<- zebra[,2]
      updateSelectizeInput(session, inputId = "edit_Nfish_targets" ,choices = stock_numbers,server= T,selected = NULL)
    }
    
    output$edit_fish_room_quickview<-DT::renderDataTable(
      zebra,selection = "single",option=list(pageLength=5,lengthChange = TRUE,lengthMenu = c(5, 10, 20, 40, 80, 100))
    )
    
    output$ethanize_Afish_target_success<-renderText({
      ""
    })
    
    output$ethanize_Nfish_target_success<- renderText({
      ""
    })
    
    output$edit_adult_fish_message<-renderText({
      ""
    })
    
    output$edit_nursery_fish_message<-renderText({
      ""
    })
    
    observeEvent(input$edit_fish_room_quickview_rows_selected,{
      selected_rows <- input$edit_fish_room_quickview_rows_selected
      if(length(selected_rows)>0){
        if(edit_fishroom_choice()=="Johnson Fish Room"||edit_fishroom_choice()=="Streisinger Fish Room"){
          selected_data1 <- zebra[selected_rows, ]
          tank_names <-find_occupied_tanks(zebra)
          updateSelectizeInput(session, inputId = "edit_Afish_targets" ,choices = tank_names,server= T, selected = selected_data1[1])
          output$selected_rows_edit_Afish_targets_output<- renderPrint(
            print(selected_data1)
          )
          shinyjs::hide("edit_Adult_stocks")
        }else if(edit_fishroom_choice()=="Walker Nursery"||edit_fishroom_choice()=="Johnson Nursery"){
          selected_data2 <- zebra[selected_rows, ]
          stock_numbers<- zebra[,2]
          updateSelectizeInput(session, inputId = "edit_Nfish_targets" ,choices = stock_numbers,server= T, selected = selected_data2[2])
          output$selected_rows_edit_Nfish_targets_output<- renderPrint(
            print(selected_data2)
          )
          shinyjs::hide("edit_nursery_stock")
        }
        
      }
    })
    
  })
  
  ##############################################################################################################################
  ####################################Editing Adult fish ####################################
  
  
  observeEvent(input$edit_Afish,{
    if (edit_fishroom_choice()=="Streisinger Fish Room"){
      zebra_ad <-read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Streisinger_room.csv"
    }else if(edit_fishroom_choice()=="Johnson Fish Room"){
      zebra_ad<-read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      filename_ad<- "Johnson_room.csv"
    }
    qc<-check_errors(input$edit_Afish_targets)
    
    if(qc==T){
      output$error_edit_Afish_select<-renderText(
        "ERROR !!! You must select a non-empty tank for edit!!"
      )
      shinyjs::hide("edit_Adult_stocks")
    }else{
      shinyjs::show("edit_Adult_stocks")
      output$error_edit_Afish_select<-renderText(
        ""
      )
      adt_location<- locate_tank(zebra_ad, input$edit_Afish_targets)
      
      genotype_list<- unique(zebra_ad[,5])
      genotype_list<-append(genotype_list,"Enter genotype", after=0)
      ov_geno<-orig_val(zebra_ad[adt_location,5],genotype_list)
      updateSelectizeInput(session, inputId = "genotype_edit_Afish_target",choices = genotype_list,selected = genotype_list[ov_geno], server=T)
      
      fishn<- seq(1:(zebra_ad[adt_location,7]*2))
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
      
      resp_list<-unique(zebra_ad[,6])
      resp_list<-append(resp_list,"MHM",after = 0)
      ov_resp<- orig_val_notes(zebra_ad[adt_location,6],resp_list)
      updateSelectizeInput(session, inputId = "responsible_edit_Afish_target", selected = resp_list[ov_resp], choices = resp_list, server = T)
      
      output$selected_rows_edit_Afish_targets_output<- renderPrint(
        print(zebra_ad[adt_location,])
      )
      
    }
    
    
    
    output$ethanize_Afish_target_success<-renderText({
      ""
    })
    
    output$edit_adult_fish_message<-renderText({
      ""
    })
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
    track_change<-matrix(nrow = 6, ncol=2)
    colnames(track_change)<- c("Before editing","After editing" )
    row.names(track_change)<-c("Genotype","Number of fish","Experiment Label","Food Label","Notes","Responsible")
    track_change[1,]<- c(zebra_ad[adt_location,5] ,input$genotype_edit_Afish_target)
    track_change[2,]<- c(zebra_ad[adt_location,7] ,input$Number_edit_Afish_target)
    track_change[3,]<- c(zebra_ad[adt_location,8] ,paste(input$ExpLabel_edit_Afish_target, collapse = " , "))
    track_change[4,]<- c(zebra_ad[adt_location,9] ,input$FoodLabel_edit_Afish_target)
    track_change[5,]<- c(zebra_ad[adt_location,10] ,input$Notes_edit_Afish_target)
    track_change[6,]<-c(zebra_ad[adt_location,6], input$responsible_edit_Afish_target)
    
    user_input_values<- c(input$genotype_edit_Afish_target,input$Number_edit_Afish_target,paste(input$ExpLabel_edit_Afish_target, collapse = " , "),input$FoodLabel_edit_Afish_target,
                          input$Notes_edit_Afish_target,input$responsible_edit_Afish_target)
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
        title = strong("Confirm Edits"),
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
    zebra_ad[adt_location,6]<-input$responsible_edit_Afish_target
    
    
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
    
    showModal(modalDialog(
      tagList(
        
        paste0("Your stock is successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_edit_modal_success","Close"))
    ))
    
  })
  
  observeEvent(input$adult_edit_modal_success,{
    shinyjs::hide("edit_adult_room")
    shinyjs::hide("edit_Adult_stocks")
    shinyjs::hide("edit_fish_room_upload_hide")
    reset("edit_adult_room")
    removeModal()
    output$edit_adult_fish_message<-renderText({
      "Please upload the fish map again before making another edit !!!"
    })
  })
  
  observeEvent(input$reset_edit_Afish_target,{
    reset("edit_adult_room")
    shinyjs::hide("edit_adult_room")
    shinyjs::hide("edit_Adult_stocks")
    shinyjs::hide("edit_fish_room_upload_hide")
  })
  
  observeEvent(input$reset_edit_Afish_target,{
    output$edit_Afish_target_success<- renderText({
      paste0("Your form is been reset. Please submit again to edit stocks!!")
    })
    
    output$edit_adult_fish_message<-renderText({
      "Please upload the fish map again before making another edit !!!"
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
    qc<-check_errors(input$edit_Afish_targets)
    
    if(qc==T){
      output$error_edit_Afish_select<-renderText(
        "ERROR !!! There is nothing here to Euthanize!!"
      )
    }else{
      
      output$error_edit_Afish_select<-renderText(
        ""
      )
      adt_location<- locate_tank(zebra_ad, input$edit_Afish_targets)
      output$selected_rows_edit_Afish_targets_output<- renderPrint(
        print(zebra_ad[adt_location,])
      )
      
      showModal(modalDialog(
        tagList(
          paste0("Please confirm euthanizing all fish of ",zebra_ad[adt_location,2], " - ",zebra_ad[adt_location,4], " - ",zebra_ad[adt_location,5])
        ),
        title = p(style="color:red",strong("Confirm Euthanize All")),
        footer = tagList(actionButton("AFish_euthanize_all_confirm","Confirm"), modalButton("Cancel"))
      ))
      
    }
    
    
    
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
    
    showModal(modalDialog(
      tagList(
        
        paste0("You stock is removed from the fish room map !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("adult_euthanize_modal_success","Close"))
    ))
    
  })
  
  observeEvent(input$adult_euthanize_modal_success,{
    shinyjs::hide("edit_fish_room_upload_hide")
    shinyjs::hide("edit_Adult_stocks")
    shinyjs::hide("edit_adult_room")
    output$ethanize_Afish_target_success<-renderText({
      "Please upload the fish room map again before making another euthanization !!"
    })
    removeModal()
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
    
    output$edit_nursery_fish_message<-renderText({
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
    
    fishn<- seq(1:(zebra[Nstock_loc,7]*2))
    ov_fishn<-orig_val(zebra[Nstock_loc,7],fishn)
    updateSelectizeInput(session, inputId = "Number_edit_Nfish_target",choices = fishn,selected = fishn[ov_fishn], server=T)
    
    notes_list<-unique(zebra[,8]) 
    notes_list<-append(notes_list,"Enter notes", after = 0)
    ov_notes<- orig_val_notes(zebra[Nstock_loc, 8], notes_list)
    updateSelectizeInput(session, inputId = "Notes_edit_Nfish_target", choices = notes_list, selected = notes_list[ov_notes],server = T)
    
    resp_list<-unique(zebra[,6])
    resp_list<-append(resp_list,"MHM",after = 0)
    ov_resp<- orig_val_notes(zebra[Nstock_loc,6],resp_list)
    updateSelectizeInput(session, inputId = "responsible_edit_Nfish_target", selected = resp_list[ov_resp], choices = resp_list, server = T)
    
    output$selected_rows_edit_Nfish_targets_output<- renderPrint(
      print(zebra[Nstock_loc,])
    )
    
  })
  
  
  observeEvent(input$edit_Nfish_target_submit,{
    if (edit_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
      
    }else if(edit_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
      
    }
    
    Nstock_loc<-locate_stockn(zebra, input$edit_Nfish_targets)
    
    output$edit_nursery_fish_message<-renderText({
      ""
    })
    
    track_change<-matrix(nrow = 5, ncol=2)
    colnames(track_change)<- c("Before editing","After editing" )
    row.names(track_change)<-c("No. of tanks","Genotype","No. of fish","Notes","Responsible")
    track_change[1,]<- c(zebra[Nstock_loc,1] ,input$TankNumber_edit_Nfish_target)
    track_change[2,]<- c(zebra[Nstock_loc,5] ,input$genotype_edit_Nfish_target)
    track_change[3,]<- c(zebra[Nstock_loc,7] ,input$Number_edit_Nfish_target)
    track_change[4,]<- c(zebra[Nstock_loc,8] ,input$Notes_edit_Nfish_target)
    track_change[5,]<-c(zebra[Nstock_loc,6], input$responsible_edit_Nfish_target)
    
    user_input_values<- c(input$TankNumber_edit_Nfish_target,input$genotype_edit_Nfish_target,
                          input$Number_edit_Nfish_target,input$Notes_edit_Nfish_target,input$responsible_edit_Nfish_target)
    
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
        title = strong("Confirm Edits"),
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
    zebra[Nstock_loc,6]<-input$responsible_edit_Nfish_target
    
    
    
    log_data<-read.csv(file = "log.csv",header = T, fill = T,encoding="UTF-8")
    
    log_data[nrow(log_data)+1,]<-list(format(Sys.time(), "%d-%b-%Y %H.%M"),zebra[Nstock_loc,2], as.character(zebra[Nstock_loc,3]),
                                      zebra[Nstock_loc,4],input$genotype_edit_Nfish_target, zebra[Nstock_loc,6],
                                      input$Number_edit_Nfish_target,
                                      paste0("EDITED THE NURSERY STOCK "))
    
    ###saving all changes
    write.csv(zebra,file = filename,row.names = F)
    write.csv(log_data, file = "log.csv",row.names = F)
    removeModal()
    
    showModal(modalDialog(
      tagList(
        
        paste0("Your stock is successfully edited !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("nursery_edit_modal_success","Close"))
    ))
  })
  
  observeEvent(input$nursery_edit_modal_success,{
    reset("edit_nursery_room")
    shinyjs::hide("edit_nursery_room")
    shinyjs::hide("edit_nursery_stock")
    shinyjs::hide("edit_fish_room_upload_hide")
    removeModal()
    output$edit_nursery_fish_message<-renderText({
      "Please upload the fish map again before making another edit !!!"
    })
  })
  
  observeEvent(input$reset_edit_Nfish_target,{
    reset("edit_nursery_room")
    shinyjs::hide("edit_nursery_room")
    shinyjs::hide("edit_nursery_stock")
    shinyjs::hide("edit_fish_room_upload_hide")
  })
  
  observeEvent(input$reset_edit_Nfish_target,{
    output$edit_Nfish_target_success<- renderText({
      paste0("Your form is been reset. Please submit again to edit stocks!!")
    })
    
    output$edit_nursery_fish_message<-renderText({
      "Please upload the fish map again before making another edit !!!"
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
      title = p(style="color:red",strong("Confirm Euthanize All")),
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
    
    showModal(modalDialog(
      tagList(
        
        paste0("You stock is removed from the nursery room map !!")
      ),
      title = p(style= "color:green",strong("SUCCESS !! ")),
      footer = tagList(actionButton("nursery_euthanize_modal_success","Close"))
    ))
    
    
  })
  
  observeEvent(input$nursery_euthanize_modal_success,{
    shinyjs::hide("edit_nursery_room")
    shinyjs::hide("edit_fish_room_upload_hide")
    output$ethanize_Nfish_target_success<-renderText({
      "Please upload the fish room again before making another euthanization !!"
    })
    removeModal()
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
          
          downloadButton("Download_archive","Download Archive (.csv)"),
          
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
      "MokalledLab_FishArchive.csv"
    },
    content=function(file){
      write.csv(fishArchive(),file, row.names = F)
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
          
          downloadButton("Download_log","Download Log (.csv)"),
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
      "MokalledLab_Fishdatabase_log.csv"
    },
    content=function(file){
      write.csv(fishlog(),file, row.names = F)
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
          div(id="genealogy_tree_hide",
              div(DT::dataTableOutput("genealogy_quickview"),
                  style = "width: 100%; overflow-x: auto;"),
              hr(),
              br(),
              verbatimTextOutput("selected_rows_genealogy_quickview_output"),
              br(),
              
              selectizeInput(inputId = "Select_genealogy_stock", label = "Select Stock number*: ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Type Stock number")),
              
              actionButton(inputId = "Genealogy_generate", label="Identify Genealogy"),
              
              downloadButton("Download_genealogy_tree","Download Family Tree (.pdf)"),
              downloadButton("Download_genealogy_table","Download Family Table (.csv)"),
              br(),
              br(),
              div(id="genealogy_tree_output",
                  h2(strong(textOutput("genealogy_tree_title"))),
                  hr(),
                  plotOutput("genealogy_tree", width = "100%"),
                  br(),
                  br(),
                  h2(strong(textOutput("genealogy_table_title"))),
                  hr(),
                  tableOutput("Genealogy_table"))%>% shinyjs::hidden()
          )%>% shinyjs::hidden())
      )
    )
    
  })
  
  ###################################################################################################################################################
  ###########################################Server side code for Genealogy############################################################################
  
  
  observeEvent(input$Genealogy_Archive,{
    archive <- read.csv(file = "Archive.csv", header = T, fill = T,encoding="UTF-8")
    output$genealogy_quickview <- DT::renderDataTable(
      archive,selection = "single",
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
    updateSelectizeInput(session, inputId = "Select_genealogy_stock",choices = stock_list,selected = stock_list[length(stock_list)], server=T)
    
    observeEvent(input$genealogy_quickview_rows_selected,{
      
      selected_rows <- input$genealogy_quickview_rows_selected
      selected_data <- archive[selected_rows, ]
      if(length(selected_rows)>0){
        updateSelectizeInput(session, inputId = "Select_genealogy_stock",choices = stock_list,selected = selected_data[2], server=T)
        output$selected_rows_genealogy_quickview_output<- renderPrint(
          print(selected_data)
        )
        shinyjs::hide("genealogy_tree_output")
      }
    })
    
    shinyjs::show("genealogy_tree_hide")
    
  })
  
  observeEvent(input$Genealogy_generate,{
    shinyjs::show("genealogy_tree_output")
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
        paste("Genealogy table_", input$Select_genealogy_stock, " - ", av[stoc_loc, 4],".csv")
      },
      content=function(file){
        write.csv(famtree,file, row.names = F)
      }
    )
    
    
    
    
    
  })
  
  #######################################################UI Section for print label part###################################################################
  #######################################################################################################################################################
  output$print_label_ui<-renderUI({
    req(credentials()$user_auth)
    fluidPage(
      useShinyjs(),
      fluidRow(
        box(width = 12, collapsible = TRUE, title = strong("Instructions:"), "
            Here, you will be able to print label for your fish stock. First, please the select the required fish room
            from the dropdown box and click View FishFacility Map. Then, you can click on the required fish stock row in the fish facility map, which you would like to print the label.
            This should autopopulate details in white boxes that takes info for the label. You can edit these details if you wish, except for stock number and date of birth. 
            Once you finalize the details for the label, click Preview Label. Finally, click the Download Label button to save
            the label in your computer. Then, you can print these with your own printer.")),
      wellPanel(
        
        selectInput("print_label_Fishroom_choices", "Select the Fish room: ", choices = list("Johnson Fish Room",
                                                                                             "Streisinger Fish Room",
                                                                                             "Walker Nursery",
                                                                                             "Johnson Nursery"
        )),
        
        actionButton(inputId = "print_label_fishroom_upload", label="View FishFacilty Map"),
      ),
      div(id="print_label",
          
          wellPanel(
            
            strong(h3("PRINT Label")),
            hr(),
            fluidRow(column(width = 2, selectizeInput(inputId = "print_label_stockn", label = "Stock number : ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "Stock Number"))),
                     column(width = 2, offset=1, selectizeInput(inputId = "print_label_Fishn", label = "Fish number : ", selected=NULL,choices= NULL , options = list(create=T, placeholder = "Fish Number"))),
                     column(width = 2, offset=1, selectizeInput(inputId = "print_label_Fishname", label = "Fish Name : ", selected=NULL,choices= NULL , options = list(create=T, placeholder = "Fish Name"))),
                     column(width = 2, offset=1, selectizeInput(inputId = "print_label_Fishgenotype", label = "Fish Genotype : ", selected=NULL,choices= NULL , options = list(create=T, placeholder = "Fish Genotype"))),
            ),
            fluidRow(column(width = 2, selectizeInput(inputId = "print_label_DOB", label = "DOB : ", selected=NULL,choices= NULL , options = list(create=F, placeholder = "DOB"))),
                     column(width = 2, offset=1, selectizeInput(inputId = "print_label_Responsible", label = "Responsible : ", selected=NULL,choices= NULL , options = list(create=T, placeholder = "Resposible"))),
                     column(width = 6, offset=1, selectizeInput(inputId = "print_label_Notes", label = "Notes : ", selected=NULL,choices= NULL , options = list(create=T, placeholder = "Notes")))
            ),
            hr(),
            fluidRow(column(width = 2,actionButton(inputId = "preview_label_button", label="Preview Label")),
                     column(width = 2,offset=1, downloadButton(outputId = "Download_label_button", label="Download Label"))
            ),
            hr(),
            div(id="preview_label",
                
                
                strong(("Preview")),
                hr(),
                imageOutput("labelImage")
                
            )%>% shinyjs::hidden() 
            
            
            
          ),
          
          fluidRow(
            wellPanel(
              div(h4(strong(textOutput("print_label_Fishroom_title"))),
                  dataTableOutput("print_label_fishroom_map"),
                  style = "width: 100%; overflow-x: auto;")
            )
          )
      )%>% shinyjs::hidden() 
    )
  })
  
  #####################################################Server Section for  Print labeL####################################################################
  ########################################################################################################################################################
  
  print_label_fishroom_choice<-eventReactive(input$print_label_fishroom_upload,{input$print_label_Fishroom_choices})
  print_label_room_map<-eventReactive(input$print_label_fishroom_upload,{
    if (print_label_fishroom_choice()=="Johnson Fish Room"){
      zebra<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
      zebra<-zebra[,-8:-9]
    }else if(print_label_fishroom_choice()=="Streisinger Fish Room"){
      zebra<- read.csv(file = "Streisinger_room.csv", header = T, fill = T,encoding="UTF-8")
      zebra<-zebra[,-8:-9]
    }else if(print_label_fishroom_choice()=="Walker Nursery"){
      zebra<- read.csv(file = "Walker nursery.csv", header = T, fill = T,encoding="UTF-8")
    }else if(print_label_fishroom_choice()=="Johnson Nursery"){
      zebra<- read.csv(file = "Johnson nursery .csv", header = T, fill = T,encoding="UTF-8")
    }
    return(zebra)
  })
  
  observeEvent(input$print_label_fishroom_upload,{
    
    output$print_label_Fishroom_title<-renderText({
      paste(print_label_fishroom_choice())
    })
    output$print_label_fishroom_map<-DT::renderDataTable(
      print_label_room_map(),selection = "single",option=list(pageLength=80,lengthChange = TRUE,lengthMenu = c(5, 10, 20, 40, 80, 100) )
    )
    
    shinyjs::show("print_label")
  })
  
  
  
  observeEvent(input$print_label_fishroom_map_rows_selected,{
    selected_rows <- input$print_label_fishroom_map_rows_selected
    #print(selected_rows)
    
    if(length(selected_rows)>0){
      updateSelectizeInput(session, inputId = "print_label_stockn",choices = list(print_label_room_map()[selected_rows,2]), server=T)
      updateSelectizeInput(session, inputId = "print_label_DOB",choices = list(print_label_room_map()[selected_rows,3]), server=T)
      updateSelectizeInput(session, inputId = "print_label_Fishname",choices = list(print_label_room_map()[selected_rows,4]), server=T)
      updateSelectizeInput(session, inputId = "print_label_Fishgenotype",choices = list(print_label_room_map()[selected_rows,5]), server=T)
      updateSelectizeInput(session, inputId = "print_label_Responsible",choices = list(print_label_room_map()[selected_rows,6]), server=T)
      updateSelectizeInput(session, inputId = "print_label_Fishn",choices = list(print_label_room_map()[selected_rows,7]), server=T)
      updateSelectizeInput(session, inputId = "print_label_Notes",choices = list(print_label_room_map()[selected_rows,8]), server=T)
    }
    
    shinyjs::hide("preview_label")
  })
  
  observeEvent(input$preview_label_button,{
    label_info <- c(input$print_label_stockn, input$print_label_DOB, input$print_label_Fishname, input$print_label_Fishgenotype,
                    input$print_label_Responsible, input$print_label_Fishn, input$print_label_Notes)
    create_label(label_info)
    output$labelImage <- renderImage({
      list(src = "label.png", width = "100%", height = "100%", alt = "Generated Label")
    }, deleteFile = F)
    
    #print(label_info)
    output$Download_label_button<-downloadHandler(
      filename=function(){
        paste0("Print_Label_for_Stock_ ",input$print_label_stockn,".png")
      },
      content=function(file){
        file.copy("label.png",file)
        
      }
    )
    
    shinyjs::show("preview_label")
  })
  
  
  
  
  
  #######################################################UI section for the About part##################################################################
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
        p(style="text-align: justify;font-size:16px", ("This website is currently at its very first versions.
                                      It is possible that you will encounter bugs while using this database. If you find any 
                                      issues, please screenshot the error and send it to: "),em(strong("vishnu@wustl.edu")),
          (". You are also welcome to report any suggestions and improvements to this 
                                          database. Thank you all for using this database.") )
      ),
      wellPanel(
        h2(strong("Update Report")),
        hr(),
        p(style="text-align: justify;font-size:16px", ("Following are the updates added to this database: ")),
        p(strong(tags$u("v1.0.0"))),
        tags$ul(
          tags$li("081123: As Mayssa wanted, adding notes is mandatory while adding fish to the nursery and adult room"),
          tags$li("081623: To edit, transfer and make genealogy, you can directly select the required stock from the table.
                  This is will autopopulate the stock number or tank name."),
          tags$li("082723: Added color code and stats to the veiw stocks table"),
          tags$li("083123: Added option to transfer fish from Johnson nursery to both Streisinger and Johnson room."),
          
        ),
        p(strong(tags$u("v2.0.0"))),
        tags$ul(
          tags$li("092423: Added bulk editing option for both adult and nursery room"),
          tags$li("092523: Added simultaneous transfer of a nursery stock to multiple tanks in the adult room")
        ),
        p(strong(tags$u("v2.0.1"))),
        tags$ul(
          tags$li("102724: Fixed a bug on the bulk transfer from the nursery. Updated the fish facilty map."),
          tags$li("102924: Fixed a bug on the default memory update of empty tank list")
        ),
        p(strong(tags$u("v2.0.2"))),
        tags$ul(
          tags$li("103124: Added print label function")
        )
      ),
      br(),
      p(style= "text-align: center",(strong(HTML("&copy; Mokalled Lab. "),"This website is powered using Shiny and R"))),
      p(style= "text-align:center", em("v2.0.2, last updated: 31-October-2024"))
    )
  })
  
  session$onSessionEnded(function() {
    stopApp()
  }) 
  
  writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
  writeLines(.libPaths(), "library.txt")
  
  
  
  
}

shinyApp(ui, server)

