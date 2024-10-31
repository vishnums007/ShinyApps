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


Stock<-29298
DOB<-"15-Aug-24"
Fish<-"Tg(ctgfa-R-NTR-L13) RFP+"
Geno<-"RFP/+"
Resp<-"VMS/MHM"
N<-40
Note<-"Stocks"


# Function to create an image with text and a line
create_label <- function(filename, label_info) {
  # Set up a PNG device with specific dimensions
  png(filename, width = 900, height = 300, res = 300)
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
  text(x = 1.5, y = 1, labels = label_info[2], cex = 1) #DOB
  text(x = 8, y = 1, labels = label_info[5], cex = 1) #Resp
  text(x = 3.85, y = 6, labels = label_info[3], cex = fit_text(label_info[3])) #Fish
  text(x = 3.85, y = 4, labels = label_info[4], cex = fit_text(label_info[4])) #Genotype
  
  wrapped_text <- strwrap(label_info[7], width = 15)
  wrapped_text<-paste(wrapped_text, collapse = "\n")
  height=fit_height(wrapped_text)
  text(x = 8.85, y = 5, labels = paste(wrapped_text, collapse = "\n"), cex = height) #Notes

  # Draw a horizontal line below the text
  lines(x = c(0, 10), y = c(8, 8), lwd = 1) # Adjust line position and width as needed
  lines(x = c(2, 2), y = c(10, 8), lwd = 1)
  lines(x = c(8.7,8.7), y = c(10, 8), lwd = 1)
  lines(x = c(7.7,7.7), y = c(10, 8), lwd = 1)
  lines(x = c(0, 10), y = c(2, 2), lwd = 1) 
  lines(x = c(3, 3), y = c(2, 0), lwd = 1)
  lines(x = c(6, 6), y = c(2, 0), lwd = 1)
  lines(x = c(7.7,7.7), y = c(8, 2), lwd = 1)
  # Close the device to save the file
  dev.off()
}

# Example usage


labelname<- paste0("PrintLabel_", Stock,".png")

Fish<-"Tg(ctgfa-R-NTR-L13) RFP+"
Note <- "Adjust line position and width as needed.screen for GFP."
Geno<-"RFP+"
label_info<-c(Stock, DOB, Fish, Geno, Resp, N, Note)
create_label(labelname, label_info)




wrapped_text <- strwrap(notes, width = 15)  # Adjust width as needed
print(wrapped_text)
text(x = 5, y = 5, labels = paste(wrapped_text, collapse = "\n"))
