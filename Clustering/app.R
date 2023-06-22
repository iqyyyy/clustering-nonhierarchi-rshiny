library(shiny)
library(shinydashboard)
library(cluster)
library(DT)
library(ggplot2)
library(dplyr)
library(png)

# UI
ui <- dashboardPage(
    dashboardHeader(title = "Clustering App"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home"),
            menuItem("Clustering", tabName = "clustering"),
            menuItem("Download", tabName = "download")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "home",
                tags$div(
                    class = "skin-blue",
                    tags$div(
                        class = "wrapper",
                        tags$div(
                            class = "content-wrapper",
                            tags$section(
                                class = "content",
                                tags$div(
                                    class = "container-fluid",
                                    tags$div(
                                        class = "row",
                                        tags$div(
                                            class = "col-md-12",
                                            align = "center",
                                            tags$h2("Kelompok 1 Sistem Informasi Manajemen"),
                                            tags$p("Introduce Our Team "),
                                            tags$h3("Member:"),
                                            tags$ul(
                                                tags$li("Ikhsania Hapsari Putri (M0720037)"),
                                                tags$li("Aufa Muthia Indhi (M0721010)"),
                                                tags$li("Aurelia Lintang Alis (M0721012)"),
                                                tags$li("Frencilia Paulina Agustin (M0721030)"),
                                                tags$li("Maghfironia Arma (M0721042)"),
                                                tags$li("Naufal Ulwan Arrifqy (M0721052)")
                                            ),
                                            tags$h3("Clustering dengan K-Means dan K-Medoids:"),
                                            tags$p("K-Means dan K-Medoids adalah metode-metode dalam analisis klaster atau clustering. Dalam analisis klaster, tujuannya adalah untuk mengelompokkan data menjadi kelompok-kelompok yang serupa berdasarkan kemiripan atau jarak antar data. K-Means menggunakan rata-rata centroid sebagai representasi kelompok, sementara K-Medoids menggunakan titik tengah yang merupakan data aktual dalam setiap kelompok. Kedua metode ini sering digunakan dalam pengelompokan data untuk mengidentifikasi pola atau struktur yang tersembunyi dalam data.")
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            tabItem(
                tabName = "clustering",
                h2("Clustering Methods"),
                fluidRow(
                    box(
                        title = "Data Input",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 4,
                        fileInput("file", "Choose a CSV file", accept = ".csv"),
                        br(),
                        selectInput("separator", "Separator", choices = c(",", ";", "\t"), selected = ","),
                        checkboxInput("header", "Header", value = TRUE)
                    ),
                    box(
                        title = "Clustering Options",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 4,
                        numericInput("k", "Number of Clusters (k):", value = 3, min = 2),
                        selectInput("method", "Clustering Method:", choices = c("K-Means", "K-Medoids"), selected = "K-Means"),
                        actionButton("clusterButton", "Cluster", class = "btn-primary")
                    )
                ),
                fluidRow(
                    box(
                        title = "Original Data",
                        status = "info",
                        solidHeader = TRUE,
                        width = 6,
                        DT::dataTableOutput("originalData")
                    ),
                    box(
                        title = "Clustered Data",
                        status = "success",
                        solidHeader = TRUE,
                        width = 6,
                        DT::dataTableOutput("clusteredData")
                    )
                ),
                fluidRow(
                    box(
                        title = "Clustering Plot",
                        status = "warning",
                        solidHeader = TRUE,
                        width = 12,
                        plotOutput("clusterPlot")
                    )
                )
            ),
            tabItem(
                tabName = "download",
                h2("download result"),
                fluidRow(
                    box(
                        title = "Download Cluster Plot",
                        status = "primary",
                        solidHeader = T,
                        width = 4,
                        downloadButton("downloadPlot", "Download Plot")
                    ),
                    box(
                        title = "Download Clustered Data",
                        status = "primary",
                        solidHeader = T,
                        width = 4,
                        downloadButton("downloadData", "Download Data")
                    )
                )
            )
        )
    )
)

# Server
server <- function(input, output) {
    
    # Read data from uploaded CSV file
    data <- reactive({
        req(input$file)
        df <- read.csv(input$file$datapath, sep = input$separator, header = input$header)
        
        # Convert data to integer or double
        df <- data.frame(lapply(df, function(x) if (is.numeric(x)) as.numeric(x) else x))
        
        df
    })
    
    # Display original data in a table
    output$originalData <- DT::renderDataTable({
        data()
    })
    
    # Perform clustering based on selected method and k value
    clusteredData <- eventReactive(input$clusterButton, {
        if (input$method == "K-Means") {
            kmeans(data(), centers = input$k)$cluster
        } else if (input$method == "K-Medoids") {
            pam(data(), k = input$k)$cluster
        }
    })
    
    # Combine clustered data with original data and display in a table
    output$clusteredData <- DT::renderDataTable({
        clustered <- data()
        clustered$Cluster <- rep(clusteredData(), each = nrow(data()) / length(clusteredData()))
        clustered
    })
    
    # Generate clustering plot
    output$clusterPlot <- renderPlot({
        if (input$method == "K-Means") {
            clusters <- kmeans(data(), centers = input$k)$cluster
        } else if (input$method == "K-Medoids") {
            clusters <- pam(data(), k = input$k)$cluster
        }
        
        # Create a new data frame with cluster information
        clustered <- data()
        clustered$Cluster <- as.factor(clusters)
        
        # Plot the clustering results
        ggplot(clustered, aes(x = clustered[, 1], y = clustered[, 2], color = factor(Cluster))) +
            geom_point(size = 3) +
            labs(x = "X", y = "Y", color = "Cluster") +
            theme_minimal()
    })
    
    # Download cluster plot as PNG
    output$downloadPlot <- downloadHandler(
        filename = function() {
            paste("cluster_plot", ".png", sep = "")
        },
        content = function(file) {
            if (input$method == "K-Means") {
                clusters <- kmeans(data(), centers = input$k)$cluster
            } else if (input$method == "K-Medoids") {
                clusters <- pam(data(), k = input$k)$cluster
            }
            
            # Create a new data frame with cluster information
            clustered <- data()
            clustered$Cluster <- clusters
            
            # Plot the clustering results
            p <- ggplot(clustered, aes(x = clustered[, 1], y = clustered[, 2], color = factor(Cluster))) +
                geom_point(size = 3) +
                labs(x = "X", y = "Y", color = "Cluster") +
                theme_minimal()
            
            ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        }
    )
    
    # Download clustered data as CSV
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("clustered_data", ".csv", sep = "")
        },
        content = function(file) {
            clustered <- data()
            clustered$Cluster <- clusteredData()
            write.csv(clustered, file, row.names = FALSE)
        }
    )
    
    
}

# Run the Shiny app
shinyApp(ui, server)
