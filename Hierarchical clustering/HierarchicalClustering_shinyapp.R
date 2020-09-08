if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinythemes, dplyr, factoextra)

# Max upload file size is 30MB
options(shiny.maxRequestSize = 30*1024^2)

# Define UI ----
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$style(HTML("hr {border-top: 1px solid #000000;}")),
  titlePanel(
    HTML(
      paste(
        h2("Toolbox for", strong("Hierarchical clustering")),
        h6("by Bastian Gschwendtner, Florian Duss, Jonas Widmer & Julian Zihlmann")
      )
    )),
  navbarPage("",
             tabPanel("Introduction",
                      sidebarLayout(
                        position = "right",
                        sidebarPanel(
                          h5(strong("Questions or issues?")),
                          p("If there are any questions on how to use this tool or some issues arose while using it, contact us by E-Mail."),
                          a("Write us!", href = "mailto:bastian.gschwendtner@hslu.ch? ,&subject=Hierarchical clustering Toolbox")
                        ),
                        mainPanel(
                          h3("Hierarchical clustering"),
                          
                          p("Hierarchical cluster analysis refers to a specific family of distance-based methods for cluster analysis (structure discovery in data sets). 
                            Clusters consist of objects which are closer to each other (or vice versa: more similar) than objects of other clusters. 
                            The methods in this family can be differentiated according to the distance or proximity measures used 
                            (between objects, but also between entire clusters) and according to their calculation rules."),
                          
                          p("If one subdivides according to the calculation regulation, then one differentiates between two important types of procedures:"),
                          tags$ul(
                            tags$li("the divisive cluster methods, in which first all objects are regarded as belonging to a cluster
                                    and then the clusters already formed are divided step by step into smaller and smaller clusters, 
                                    until each cluster consists of only one object. (Also referred to as \"top-down procedure\")"),
                            tags$li("the agglomerative cluster procedures, in which each object first forms a cluster and then the clusters
                                    already formed are gradually combined into ever larger ones until all objects belong to a cluster. 
                                    (Also referred to as \"bottom-up process\")")
                          ),
                          
                          h3("Distance Measures"),
                          
                          p("The choice of distance measures is a critical step in clustering. It defines how the similarity of two elements (x, y)
                            is calculated and will influence the shape of the clusters."),
                          
                          h4("Classical methods"),
                          tags$ul(
                            tags$li("Euclidean distance"),
                            tags$li("Manhattan Distance")
                          ),
                          
                          h4("Correlation-based distances"),
                          tags$ul(
                            tags$li("Pearson correlation distance (measures the degree of a linear relationship between two profiles).
                                    This is the most commonly used method."),
                            tags$li("Spearman correlation distance (computes the correlation between the rank of x and the rank of y variables) >
                                    non-parametric correlations, used to perform rank-based correlation analysis"),
                            tags$li("Kendall correlation distance (measures the correspondence between the ranking of x and y variables) >
                                    non-parametric correlations, used to perform rank-based correlation analysis")
                          ),

                          tags$a(href="https://www.datanovia.com/en/lessons/clustering-distance-measures", "Further information on Distance Measures!"),



                          h3("Linkage Types"),
                          h4("Single Linkage"),
                          p("In single linkage hierarchical clustering, the distance between two clusters is defined as the shortest distance between two points
                            in each cluster. For example, the distance between clusters \"r\" and \"s\" to the left is equal to the length of the arrow between their
                            two closest points."),

                          h4("Complete Linkage"),
                          p("In complete linkage hierarchical clustering, the distance between two clusters is defined as the longest distance between
                            two points in each cluster. For example, the distance between clusters \"r\" and \"s\" to the left is equal to the length of the
                            arrow between their two furthest points."),

                          h4("Average Linkage"),
                          p("In average linkage hierarchical clustering, the distance between two clusters is defined as the average distance between each point
                            in one cluster to every point in the other cluster. For example, the distance between clusters \"r\" and \"s\" to the left is equal to the
                            average length each arrow between connecting the points of one cluster to the other."),

                          h4("Centroid linkage"),
                          p("In this linkage method the distance between two clusters is defined as the distance between the centroid for cluster 1
                            (a mean vector of length p variables) and the centroid for cluster 2."),

                          h4("Ward's minimum variance method"),
                          p("This method minimizes the total within-cluster variance. At each step the pair of clusters with minimum between-cluster distance
                            are merged."),

                          p("For more information on linkage methods visit:"),
                          tags$a(href="https://www.datanovia.com/en/lessons/agglomerative-hierarchical-clustering/#linkage", "Datanovia"),
                          tags$a(href="https://www.saedsayad.com/clustering_hierarchical.htm", " or Dr. Saed Sayad")
                          
                        ))),

             
             tabPanel("Chose your Dataset",
                      tags$hr(),
                      
                      h4("Comment:"),
                      p("Uploading your data may take some time. The larger the file, the longer it takes."),
                      p("You can use one of the two example datasets for testing and explaining the tool or the hierarchical clustering method."),
                      
                      # Horizontal line ----
                      tags$hr(),
                      
                      sidebarPanel(
                        
                        
                        
                        # Input: Select data set ----
                        radioButtons("dataset", "Dataset",
                                     choices = c("Motor Trend Car Road Tests (mtcars)" = "mtcars", 
                                                 "Flower Species (iris)" = "iris",
                                                 "Own dataset" = "own"),
                                     selected = "mtcars"),
                        
                        # Horizontal line ----
                        tags$hr(),                                
                        
                        # Input: Select a file ----
                        fileInput("file1", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        
                        
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),
                        
                        # Input: Select separator ----
                        radioButtons("sep", "Separator",
                                     choices = c(Comma = ",",
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = ","),
                        
                        # Input: Select quotes ----
                        radioButtons("quote", "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = '"'),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        
                        # Input: Specify the number of observations to view ----
                        numericInput("obs", "Number of observations to view:", 10),
                        
                        
                        # Include clarifying text ----
                        helpText("Note: while the data view will show only the specified",
                                 "number of observations, the summary will still be based",
                                 "on the full dataset.")
                        
                      ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        
                        # Output: Header + summary of distribution ----
                        h4("Summary"),
                        verbatimTextOutput("summary"),
                        
                        # Output: Header + table of distribution ----
                        h4("Observations"),
                        tableOutput("view")
                        
                      )
                      
             ),
             
             tabPanel("Hierarchical Clustering",
                      
                      # Horizontal line ----
                      tags$hr(),
                      
                      h4("Comment:"),
                      p("The hierarchical clustering algorithm calculates distances between points. Therefore 
                          all categorical observations have already been omited and you can only choose from your",
                        strong("numerical"),"observations."),
                      p("Creating the plots may take some time."),
                      
                      # Horizontal line ----
                      tags$hr(),
                      
                      sidebarPanel(
                        selectInput('metric', 'Choose a distance method:', 
                                    choices =list(
                                      'Euclidean'='euclidean',
                                      'Manhattan'='manhatten',
                                      'Pearson correlation'='pearson',
                                      'Maximum' = 'maximum',
                                      'Binary' = 'binary',
                                      'Minkowski' = 'minkowski',
                                      'Spearman' = 'spearman',
                                      'Kendall' = 'kendall')),
                        
                        selectInput('linkage', 'Choose a linkage method:', 
                                    choices = list(
                                      'Single'='single',
                                      'Complete'='complete',
                                      'Average'='average',
                                      'Mcquitty'='mcquitty',
                                      'Median'='median',
                                      'Centroid'='centroid',
                                      'Ward.D'='ward.D',
                                      'Ward.D2'='ward.D2'),
                                    selected='single'),
                        
                        sliderInput('cut', 'Number of Groups', 3, min = 1, max = 20),
                        
                        
                        # Input: Select normalisation of data ----
                        radioButtons("scale1", "Scale your data?",
                                     choices = 
                                       c("Yes" = "yes",
                                         "No" = "no"),
                                     selected = "yes")
                        
                      ),
                      mainPanel(
                        plotOutput('plot1'),
                        plotOutput('plot2')
                      ),
                      
                      
                      verbatimTextOutput("selection1"),
                      verbatimTextOutput("selection2"),
                      verbatimTextOutput("selection3")
                      
                      
                      
                      
             ) # End of tabPanel
             
  ) # End of navbar
) # End of UI



#################################################################################################################
#################################################################################################################


# Define server logic ----
server <- function(input, output, session) {
  
  dataset <- reactive({ 
    
    if (input$dataset =="mtcars") {
      df <- mtcars
    } 
    else if (input$dataset == "iris") {
      df <- iris
    } 
    else {
      req(input$file1)
      
      inFile <- input$file1
      
      df <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    }
    
    return(df)
    
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- dataset()
    summary(dataset)
  })
  
  # Show n-observations
  output$view <- renderTable({
    head(dataset(), n = input$obs)
  })
  
  # Only use numerical values
  num_data <- reactive({
    select_if(dataset(), is.numeric)
  })
  
  # Hierarchical Clustering
  
  # Compute the dissimilarity matrix with different distance types
  
  res.dist <- reactive({
    get_dist(
      (if(input$scale1 =="yes") {
        scale(num_data())
      }
      else {
        num_data()
      }),
      method = input$metric)
  })
  
  # Visualize the dissimilarity matrix:
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    
    fviz_dist(res.dist(), lab_size = 8)
    
  })
  
  # Compute hierarchical clustering with different linkage types:
  res.hc <- reactive({
    hclust(res.dist(), method = input$linkage)
  })
  
  # Visualize the tree:
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    
    fviz_dend(res.hc(), input$cut, cex = 0.5, color_labels_by_k = TRUE, rect = TRUE)
    
  })

  
}

# Run the app ----
shinyApp(ui = ui, server = server)