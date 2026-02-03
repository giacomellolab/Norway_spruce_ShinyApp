#library(renv)
#renv::snapshot()
#renv::dependencies()
library(shiny)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
#library(rsconnect)
library(shinydashboard)
library(shinythemes)
library(flexdashboard)
library(bslib)
library(stringr)

gene_list <- read.csv(file = "./uniq_genes.csv", header = TRUE, sep = ",")
#sample.table = read.table(file = "./samplenames.csv", header = TRUE, sep = ";")
#sample.name <- read.table(file = "./sample_name.csv", header = TRUE, sep = ";")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Norway spruce project", titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
  tags$head(
      tags$style("h4 {font-family:Calibri;font-size:25px}",
                 "p {font-family:Calibri;font-size:25px}"),
      # Add Darkly theme CSS from Bootswatch 3.x
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootswatch@3.4.1/darkly/bootstrap.min.css"
      )
  ),
  
  # Global CSS to control sizing and responsive behavior
  # Also adding darkly themes
  tags$style(HTML("
  
    #darkly theme over shiny dashboard
    .skin-blue .main-sidebar, .skin-blue .left-side { background-color: #222 !important; }
      .skin-blue .sidebar-menu > li > a { color: #ddd !important; }
      .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li > a:hover { background-color: #1a1a1a !important; color: #fff !important; }
      .skin-blue .main-header .logo { background-color: #111 !important; color: #fff !important; font-size: 22px !important; }
      .skin-blue .main-header .navbar { background-color: #111 !important; }
      .content-wrapper, .right-side { background-color: #121212 !important; color: #eee !important; }
      .box { background-color: #1e1e1e !important; border: 1px solid #333 !important; }
      .box-header { color: #fff !important; border-bottom: 1px solid #444 !important; }
      .form-control, .selectize-input { background-color: #2b2b2b !important; color: #eee !important; border: 1px solid #555 !important; }
      .selectize-dropdown { background-color: #2b2b2b !important; color: #eee !important; }
      .btn { background-color: #444 !important; color: #fff !important; border: 1px solid #666 !important; }
      .btn:hover { background-color: #555 !important; }
      .nav-tabs-custom > .nav-tabs > li.active > a { background-color: #333 !important; color: #fff !important; }
      .nav-tabs-custom > .nav-tabs > li > a { color: #aaa !important; }
      h1, h2, h3, h4, h5, h6, p, label { color: #ddd !important; }
  
    /* Change font size and style for dashboard header title */
    .main-header .logo {
      font-size: 40px !important;  /* adjust this value */
      font-weight: bold;            /* optional */
      line-height: 50px;            /* vertically centers the text */
      text-align: left;
      font-family: Calibri;
      font-weight: 600;
      color: #fff !important;
      background-color: #222 !important;
    }
    
    /* Change font size and style for dashboard header title */
    .main-header .navbar {
      font-size: 40px !important;  /* adjust this value */
      font-weight: bold;            /* optional */
      line-height: 50px;            /* vertically centers the text */
      text-align: left;
      font-family: Calibri;
      font-weight: 600;
      color: #fff !important;
      background-color: #222 !important;
    }
    
    /* ensure the body uses full height so calc(100vh - ...) works reliably */
    .content-wrapper, .right-side {
        min-height: calc(100vh - 50px);
    }

      /* column holding the image - allow scrolling if content is taller */
      .image-col {
        padding: 10px 15px;
        overflow: auto;
      }

      /* Responsive image rules:
         - max-width: 100% so it never overflows horizontally
         - max-height: calc(100vh - Xpx) so it can grow vertically but not overflow the viewport
         - width: auto and height: auto allow the image to preserve aspect ratio
         - object-fit: contain keeps the full image visible
      */
      
      .responsive-img {
        display: block;
        margin auto;
        max-width: 100%;
        width: auto;
        height: auto;
        /*max-height: calc(100vh - 180px); */
        /* tweak 180px as needed for header + paddings + controls */
        object-fit: contain;
      }

      /* On very small screens, let sidebar stack above content */
      @media (max-width: 767px) {
        .image-col {
          padding-top: 0.5rem;
        }
        .responsive-img {
          max-height: calc(100vh - 260px); /* more room for stacked sidebar */
        }
      }
      
      /* for pdf files */
      .responsive-pdf {
      width: 100%;
      height: calc(100vh - 150px);
      border: none;
      }
    
    /* Increase tab title font size */
  .nav-tabs > li > a {
    font-size: 25px !important;  /* adjust as needed */
    font-weight: 500;            /* optional: make it a bit bolder */
  }

  /* Active tab styling (highlighted one) */
  .nav-tabs > li.active > a {
    font-size: 25px !important;
    font-weight: 600;
  }
  ")),
  
  # contents of body start here  
  fluidRow(
        
        column(width = 10,
               #add helper text
               tags$br(),
               
               tags$p("This is a shiny app to visualise the results from ST data analysis of Norway spruce consisting of a total of 88 bud sections."), 
               tags$p("The data was collected from 25 Norway spruce buds across three budtypes - Vegetative, Female and Acrocona."),
               tags$p("These buds were collected at three different timepoints (August, September and October) reflecting the reproductive as well as the developmental changes occuring in the Norway spruce."),
               tags$br(),
               tags$p("Note: Go to 'Brightfield Images' tab for stained bud tissue brightfield images. Sections are labelled in the same order as they appear here."),
               
               tags$br())),#end of first fluidrow
  
  fluidRow(
        div(
        style = "padding-left: 15px; padding-right: 10px;",
        tabsetPanel(
          
        #Tab1
        tabPanel(
            title = "Spots Information",
            tags$br(),
            
            fluidRow(
            column(width = 9,
            tags$p("This tab gives a general overview of the spatial transcriptomics (ST) data from the Norway spruce bud sections analyzed in this study."),
            tags$br(),
            tags$p("Select a criteria to view spots distribution across the bud tissue sections."),
            )),
            
            tags$br(),

            sidebarLayout(
              sidebarPanel(
                selectInput("criteria", label = "Criteria:",
                            choices  = list("Clusters" = "seurat_clusters",
                                            "Genes per spot" = "gene_count",
                                            "UMI" = "UMI_count",
                                            "UMAP - clusters" = "umap_clusters"))),
                mainPanel(
                  fluidRow(
                    column(width = 9, class = "image-col",
                           uiOutput("criteria_img"))
                    )))
                ),#end of tab1
                                     
        #Tab2
        tabPanel(
            title = "Gene Expression",
            tags$br(),
            tags$p("You can select a gene from the drop-down below to visualise it's spatial gene expression across the 88 bud sections."), 
            tags$br(),
                                       
            #grid layout for this tab to display gene expression and corresponding images with pfam values
            sidebarLayout(
              sidebarPanel(
                selectizeInput("genes_1", label = "Select Gene:", choices = NULL)),
              mainPanel(
                fluidRow(column(4,
                                h5("PFAM value (for annotated genes) for selected Gene is "),
                                verbatimTextOutput("pfam", placeholder = TRUE))),
                fluidRow(column(width = 8, class = "image-col",
                                uiOutput("gene_img")))
              ))
        ),#end of tab2
            
        #tab3
        tabPanel(
             title = "Brightfield Images",
             tags$br(),
             tags$p("Here you can take a closer look at the brightfield bud tissue sections. Simply select the budtype. Sections are also labelled in the order they appear in the spatial plots."), 
             tags$br(),
                                       
             sidebarLayout(
                  sidebarPanel(
                        
                        h4("Select the sample to view high resolution image for."),
                        tags$br(),
                        selectInput("select2", label = "Select budtype",
                                               choices  = list("1. August Vegetative 1/August Vegetative 2" = "AugVeg1_2",
                                                               "2. August Vegetative 3" = "AugVeg3",
                                                               "3. August Vegetative 4 (Section 29-32 only)" = "AugVeg4",
                                                               "4. August Female 1" = "AugFem1",
                                                               "5. August Female 2" = "AugFem2",
                                                               "6. August Female 3" = "AugFem3",
                                                               "7. August Acrocona 1/August Acrocona 2" = "AugAcro1_2",
                                                               "8. August Acrocona 3" = "AugAcro3",
                                                               "9. September Vegetative 1/September Vegetative 2" = "SeptVeg1_2",
                                                               "10. September Vegetative 3" = "SeptVeg3",
                                                               "11. September Vegetative 4 (Section 85-88 only)" = "SeptVeg4",
                                                               "12. September Female 1" = "SeptFem1",
                                                               "13. September Female 2" = "SeptFem2",
                                                               "14. September Female 3" = "SeptFem3",
                                                               "15. September Acrocona 1" = "SeptAcro1",
                                                               "16. September Acrocona 2" = "SeptAcro2",
                                                               "17. September Acrocona 3" = "SeptAcro3",
                                                               "18. October Vegetative 1" = "OctVeg1",
                                                               "19. October Vegetative 2" = "OctVeg2",
                                                               "20. October Vegetative 3" = "OctVeg3",
                                                               "21. October Female 1" = "OctFem1",
                                                               "22. October Female 2" = "OctFem2",
                                                               "23. October Female 3" = "OctFem3",
                                                               "24. October Acrocona 1" = "OctAcro1",
                                                               "25. October Acrocona 2" = "OctAcro2",
                                                               "26. October Acrocona 3" = "OctAcro3")
                                           )),
                  mainPanel(
                       column(width = 9, class = "image-col", uiOutput("bf_img"))) #end of mainpanel
                
                ) #end of sidebarlayout for tab3
         ), #end of tab 3
                                     
         #Tab4
         tabPanel(title = "ST deconvolve",
                  fluidRow(
                    column(
                      width = 3,
                      wellPanel(
                        tags$br(),
                        h4("Select a bud category below which will show celltype options to select further"),
                        tags$br(),
                        selectInput("category", "Bud category:", 
                                    choices = c("AcroconaOctober", "FemaleOctober", "VegetativeOctober")),
                        uiOutput("subcat_ui")  # dynamically rendered 2nd dropdown
                      )
                    ),
                    
                    column(
                      width = 9,
                      uiOutput("image_ui")  # image displayed here
                    )
                  )
        
         )#end of tab4
              )#end of tabsetpanel
        )#end of div offset for tabsetpanel
       )#end of main fluidrow in dashboard body
    )#end of dashboardbody
) #end of dashboardpage

# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  #update selectize for allgenes values
  updateSelectizeInput(session, 'genes_1', choices = sort(c(gene_list$genenames)), 
                       options = list(maxOptions = 2500), 
                       server = TRUE, selected = "MA-10011940g0010")
  
  #function for search button
  gene_image <- reactive({
    filename <- normalizePath(file.path('./genes_exp',
                                        paste(input$genes_1,".png" , sep="")))
    tags$img(src = filename,
             class = "responsive-img",
             alt = "Image 3 - responsive")
  })
  
  #output for tab2 with gene expression data
  output$gene_img <- renderUI({
    gene_image()
  })
  
  output$pfam <- renderText(
    { index <- which(gene_list$genenames == input$genes_1)
      print(gene_list[index,2]) }
  )
  
  output$samples_2 <- renderTable(sample.table)
  
  #output for tab1 with spots criteria information
  #output$sample_name_table <- renderTable(sample.name)
  
  output$criteria_img <- renderUI({
    filename <- normalizePath(file.path(paste(input$criteria, ".png" , sep="")))
    
    tags$img(src = filename,
         class = "responsive-img",
         alt = "Image 1 - responsive")
  })
  
  #output for bright field images on tab3
  output$bf_img <- renderUI({
    filename <- normalizePath(file.path('./hires_images',
                                        paste(input$select2, ".jpg" , sep="")))
    tags$img(src = filename,
             class = "responsive-img",
             alt = "Image 2 - responsive")
  })
  
  #for tab4
  # --- Dynamic 2nd dropdown based on 1st selection ---
  output$subcat_ui <- renderUI({
    req(input$category)
    
    if (input$category == "AcroconaOctober") {
      selectInput("subcategory", "Select sample id to view groups:",
                  choices = c("Sample 46", "Sample 47", "Sample 48", "Sample 39", "Sample 40", "Sample 41", "Sample 42"))
    } else if (input$category == "FemaleOctober") {
      selectInput("subcategory", "Select sample id to view groups:",
                  choices = c("Sample 49", "Sample 50", "Sample 51", "Sample 52", "Sample 53", "Sample 54", "Sample 55", "Sample 56"))
    } else if (input$category == "VegetativeOctober") {
      selectInput("subcategory", "Select sample id to view groups:",
                  choices = c("Sample 43", "Sample 44", "Sample 45", "Sample 33", "Sample 34", "Sample 35", "Sample 36", "Sample 37", "Sample 38"))
    }
  })
  
  # --- Show image based on both dropdowns ---
  output$image_ui <- renderUI({
    req(input$category, input$subcategory)
    
    # Build the path based on selections
    folder <- tolower(input$category)
    file_name <- input$subcategory %>% str_remove_all("\\s") %>% tolower()
    filename <- normalizePath(file.path('./stdeconvolve',folder,
                                        paste(file_name, ".pdf" , sep="")))
    
    tags$embed(
      src = filename,
      type = "application/pdf",
      class = "responsive-pdf"
    )
  })
}

shinyApp(ui = ui, server = server)