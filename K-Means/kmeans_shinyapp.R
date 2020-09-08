if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, dplyr, factoextra, datasets, shinythemes)

library(shiny)
library(shinythemes)
library(dplyr)
library(factoextra)
library(datasets)

# Max upload file size is 30MB
options(shiny.maxRequestSize = 30*1024^2)

# Define UI ----
ui <- fluidPage(
    theme = shinytheme("yeti"),
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    titlePanel(
        HTML(
            paste(
                h2("Toolbox for", strong("K-Means Clustering")),
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
                        a("Write us!", href = "mailto:florianduss@hotmail.com? ,&subject=K-Means Toolbox")
                        ),
                mainPanel(
                    h2("Introduction to Clustering"),
                    
                    p("This application helps you to cluster your data with the K-Means method.
                    Before we have a look at this method we shortly explain what clustering is."),
                    
                    p("Clustering When we cluster data we try to find subgroups (aka clusters) in a dataset.
                    Those groups should be distinct from each other while similar in itself. 
                    This process helps you for example to find subgroups within your customers or can be used for market segmentation. 
                    The different clusters enable us to target each group in a different, more personalized way. 
                    Think about specific advertisement strategies for each group instead of one vague advertisement strategy for everybody. 
                    Keep in mind that clustering methods can be used not only in business questions, the possible spectrum for application is nearly limitless."),
                    
                    h3("K-Means"),
                    
                    p("K-means clustering is one of the most basic types of unsupervised learning algorithm. 
                    It belongs with hierarchical clustering propabaly to the best-known clustering approaches. 
                    In K-Means clustering we partition our observations into a pre-specified number of clusters (K).
                    Those K-clusters should be distinct from each other and non-overlapping, 
                    meaning that the K-Means algorithm assigns each provided observation to exactly one of the K clusters."),

                    tags$br(),
                    p(strong("K-Means Process:")),
                    
                    tags$ol(
                        tags$li("Select K-random points as initial centroids"),
                        tags$li("Repeat until centroids do not change:"),
                            tags$ul(
                                ("2.1) Form K clusters by assigning each point to its closest centroid"),
                                tags$br(),
                                ("2.2) Recompute the centroid of each cluster")
                            )
                    ),
                    
                        
                    h3("Optimal Value for K"),
                    p("Evaluating clusters is a complex task for which several approaches have been created in the past. 
                    Most approaches measure the compactness of a cluster as well as the separation between the clusters. 
                    Below are three common used methods closer specified.  
                    Later you can chose from those three methods in this toolbox to find the optimal number of K for your Data."),
                    
                    h4("The Silhouette score"),
                    p("The Silhouette score (aka Silhouette index) is a very well-known clustering evaluation method. 
                    It introduces clustering quality scores for each individual point and calculates the final quality index as an average 
                      of the point-wise quality estimates. 
                      In other words, it measures the quality of the clustering."),
                    
                    h4("The Elbow method / WSS"),
                    p("The total WSS (Within-Sum-of-Squares) measures the compactness of the clustering and we want it to be as small as possible. 
                    This method looks at the total WSS as a function of the number of clusters: 
                    One should choose a number of clusters so that adding another cluster doesn't improve much better the total WSS."),
                    
                    p("The optimal value of K can be seen in the plot. If the curve would be an arm, the optimal K is at the place where 
                      the elbow would be (hence the elbow method.) This is not an exact method like the silhouette score or the gap statistic."),
                    
                    img(src ="elbow_method.png", height = 400, width = 400, alt="Plot explaing the elbow method"),
                    # Source Image: Elbow Method
                    tags$br(),
                        tags$i(tags$source("Source: https://www.edureka.co/blog/k-means-clustering-algorithm/")),


                    h4("The Gap statistic"),
                    p("The gap statistic compares the total within intra-cluster variation for different values of K
                    with their expected values under null reference distribution of the data.
                    The estimate of the optimal clusters will be a value that", strong("maximize the gap statistic"),".
                    This means that the clustering structure is far away from the random uniform distribution of points.")
              ))),
           
           
           tabPanel("Chose your Dataset",
                    tags$hr(),
                    
                    h4("Comment:"),
                    p("Uploading your data may take some time. The larger the file, the longer it takes."),
                    p("You can use one of the two example datasets for testing and explaining the tool or the K-Means clustering method."),
                    
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
           
           tabPanel("K-Means Model",
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    h4("Comment:"),
                    p("The K-Means algorithm calculates distances between points. Therefore 
                          all categorical observations have already been omited and you can only choose from your",
                      strong("numerical"),"observations."),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    sidebarPanel(
                        selectInput('xcol1', 'X Variable', ""),
                        selectInput('ycol1', 'Y Variable', "", selected=names(df)[[2]]),
                        sliderInput('clusters', 'Cluster count', 3, min = 1, max = 20),
                        
                        # Input: Select normalisation of data ----
                        radioButtons("scale1", "Scale your data?",
                                     choices = c("No" = "no",
                                                 "Yes" = "yes"),
                                     selected = "no")

                    ),
                    mainPanel(
                        plotOutput('plot1')
                    ),
                    
                    column(12,
                    tags$hr(),
                    p("Matrix of cluster centres:"),
                    verbatimTextOutput("selection1"),
                    p("Within-cluster sum of squares, one component per cluster:"),
                    verbatimTextOutput("selection2"),
                    p("Total within-cluster sum of squares:"),
                    verbatimTextOutput("selection3"),
                    p("Sum of squares (in %)"),
                    verbatimTextOutput("selection4")
                    )
                    
                    
           ),
           tabPanel("Optimal value for K",
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    h4("Comment:"),
                    p("The K-Means algorithm calculates distances between points. Therefore 
                          all categorical observations have already been omited and you can only choose from your",
                      strong("numerical"),"observations."),
                    p("Creating the plots may take some time. Especially the \"Gap statistic\" will need more time."),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    sidebarPanel(
                        selectInput('xcol2', 'X Variable', ""),
                        selectInput('ycol2', 'Y Variable', "", selected=names(df)[[2]]),
                        selectInput('method', 'Choose a method:', 
                                list('The Silhouette score'='silhouette',
                                     'The Elbow method / WSS'='wss',
                                     'The Gap statistic'='gap_stat')),
                        
                        # Input: Select normalisation of data ----
                        radioButtons("scale2", "Scale your data?",
                                     choices = c("No" = "no",
                                                 "Yes" = "yes"),
                                     selected = "no")
                    ),
                    mainPanel(
                        plotOutput('plot2')
                    )
           
           

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
        
        # Input for 'K-Means'
        updateSelectInput(session, inputId = 'xcol1', label = 'X Variable',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'ycol1', label = 'Y Variable',
                          choices = names(select_if(df, is.numeric)))
        
        # Input for 'Optimal value for K'
        updateSelectInput(session, inputId = 'xcol2', label = 'X Variable',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable',
                          choices = names(select_if(df, is.numeric)))
        
        
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

    # Available Variables for the "K-Means Model"
    selectedData1 <- reactive({
        (if(input$scale1 =="yes") {
            scale(dataset()[, c(input$xcol1, input$ycol1)])
        }
        else {
            dataset()[, c(input$xcol1, input$ycol1)]
        })
    })

    # Available Variables for the "Optimal Value of K" 
    selectedData2 <- reactive({
        (if(input$scale2 =="yes") {
            scale(dataset()[, c(input$xcol2, input$ycol2)])
        }
        else {
            dataset()[, c(input$xcol2, input$ycol2)]
        })
    })
    
    # K-Means algorithm
    clusters <- reactive({
        kmeans(selectedData1(), input$clusters, nstart = 50)
    })

    # Methods for "Optimal Value for K"
    method <- reactive({
        input$method
    })

    
    #K-Means
    output$plot1 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))

        plot(selectedData1(),
             col = clusters()$cluster,
             pch = 20, cex = 2)
    
    })
    
    #K-Means centers
    output$selection1 <- renderPrint({
        clusters()$centers
    })
    
    #K-Means withinss
    output$selection2 <- renderPrint({
        clusters()$withinss
    })
    
    
    #K-Means tot.withinss
    output$selection3 <- renderPrint({
        clusters()$tot.withinss
    })
    
    #K-Means percentage
    output$selection4 <- renderPrint({
        clusters()$betweenss/clusters()$totss*100
    })
    

    # Optimal Value for K
    output$plot2 <- renderPlot({
        fviz_nbclust(
            (if(input$scale2 =="yes") {
                scale(selectedData2())
            }
            else {
                selectedData2()
            }),
             kmeans, method = method(),
             k.max=20,
             linecolor = "black")
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)