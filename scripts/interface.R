# Load libraries
library(shiny)
library(shinydashboard)
library(stringr)
library(data.tree)
library(magick)

# Read the data
aadr_data <- read.csv("D:/BINP29/Population genetics/Data/AADR_test", sep = '\t', header = TRUE)
tree <- read.csv("D:/BINP29/Population genetics/Data/chrY_hGrpTree_isogg2016.tsv", sep = "\t", header = TRUE)
colnames(tree)[1] <- "Child"
y_tree <- FromDataFrameNetwork(tree)

# UI
ui <- dashboardPage(
  skin = "blue",  
  dashboardHeader(
    title = "Haplogroup story teller",
    titleWidth = 250
  ),
  dashboardSidebar(
    selectInput("input_variable", label = "Select Input:", choices = aadr_data$Y.haplogroup.ISOGG),
    tags$hr()
  ),
  dashboardBody(
    box(
      title = "Results", width = 600,
      verbatimTextOutput("result_text"),
      imageOutput("result_image"),  # New image output
      tags$style(HTML("#result_text { height: 300px; width: 950px; overflow-y: auto; }"))
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "input_variable", choices = aadr_data$Y.haplogroup.ISOGG)
  })
  
  output$result_text <- renderText({
    req(input$input_variable)
    
    haplogroup <- input$input_variable
    
    result_str <- character(0)
    matching_rows <- aadr_data[aadr_data$Y.haplogroup.ISOGG == haplogroup, ]
    
    max_date <- max(matching_rows$Date)
    max_date_index <- which.max(matching_rows$Date)
    country <- matching_rows$Country[max_date_index]
    result_str <- c(result_str, paste("The Y haplogroup provides information on direct paternal linkage, shared male ancestry, and origin of family lines." ),"\n")
    
    if (max_date > 0) {
      result_str <- c(result_str, paste("The most recent occurrence of", haplogroup, "is estimated to have been around", max_date, "CE, origin:", country))
    } else {
      result_str <- c(result_str, paste("The most recent occurrence of", haplogroup, "is estimated to have been around", abs(max_date), "BCE, origin:", country))
    }
    
    path <- FindNode(y_tree, haplogroup)$path
    # if the path is found
    if (!is.null(path)) {
      for (j in rev(path[-length(path)])) { # to check immediate ancestor
        if (j %in% unique(aadr_data$Y.haplogroup.ISOGG)) {
          rows <- aadr_data[aadr_data$Y.haplogroup.ISOGG == j, ]
          
          min_date <- min(rows$Date)
          min_date_index <- which.min(rows$Date)
          country2 <- rows$Country[min_date_index]
          hap_2 <- rows$Y.haplogroup.ISOGG[min_date_index]
          path_df <-  data.frame(Child = haplogroup, Parent = hap_2)
          if (min_date > 0) {
            result_str <- c(result_str, paste("The haplogroup", haplogroup, "'s paternal line was formed when it branched off from the ancestor", hap_2, "around", min_date, "CE, origin:", country2))
          } else {
            result_str <- c(result_str, paste("The haplogroup", haplogroup, "'s paternal line was formed when it branched off from the ancestor", hap_2, "around", abs(min_date), "BCE, origin:", country2))
          }
          result_str <- c(result_str, paste("Path information:", as.character(path_df$Child), "->", as.character(path_df$Parent)))
          
          output$result_image <- renderImage({
            
            img_path <- "D:/BINP29/Population genetics/Data/image.jpg"
            
            # image object
            img <- image_read(img_path)
            
            # According to BCE and CE
            if (max_date < 0 || min_date < 0) {
              img <- image_annotate(img, text = paste("Haplogroup:", haplogroup, "\nDate:", abs(max_date), "BCE"), location = "+275+250", size = 11, color = "black")
              img <- image_annotate(img, text = paste("Ancestor:", hap_2, "\n Date:", abs(min_date), "BCE"), location ="+20+10" , size = 11, color = "black")
            } else {
              img <- image_annotate(img, text = paste("Haplogroup:", haplogroup, "\nDate:", abs(max_date), "CE"), location = "+275+250", size = 11, color = "black")
              img <- image_annotate(img, text = paste("Ancestor:", hap_2, "\n Date:", abs(min_date), "CE"), location = "+20+10", size = 11, color = "black")
            }
            
            annotated_img_path <- tempfile(fileext = ".png")
            image_write(img, path = annotated_img_path)
            
            # Return a list containing the filename and alt text
            list(src = annotated_img_path, alt = "Annotated Image")
          }, deleteFile = TRUE)
          
          break
        } else {
          result_str <- c(result_str, "No ancestry found")
          output$result_image <- renderImage({
            # No ancestry found
            list(src = NULL, alt = "No Image")
          }, deleteFile = TRUE)
          break
        }
      }
    }
    
    # Return the result as a character vector
    HTML(paste(result_str, collapse = "\n\n"))
  })
}

shinyApp(ui, server)
