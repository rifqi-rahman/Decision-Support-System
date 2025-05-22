library(shiny)
library(dplyr)
library(DT)

# Fungsi bantu untuk membuat form input matrix
matrixInput <- function(inputId, nrow, ncol, rowNames = NULL, colNames = NULL, defaultValue = 0, width = "60px") {
  tableOutput <- tagList()
  
  # Header kolom
  if (!is.null(colNames)) {
    header <- fluidRow(
      column(2),
      lapply(1:ncol, function(j) column(2, tags$div(strong(colNames[j]), style = "margin-bottom: 5px;")))
    )
    tableOutput <- tagAppendChildren(tableOutput, header)
  }
  
  # Baris input
  for (i in 1:nrow) {
    rowInputs <- tagList()
    for (j in 1:ncol) {
      id <- paste0(inputId, "_", i, "_", j)
      rowInputs <- tagAppendChildren(rowInputs,
                                     column(2, numericInput(id, NULL, value = defaultValue, min = 0, max = 5, step = 0.1, width = width)))
    }
    rowLabel <- if (!is.null(rowNames)) column(2, strong(rowNames[i])) else column(2)
    tableOutput <- tagAppendChildren(tableOutput, fluidRow(rowLabel, rowInputs))
  }
  return(tableOutput)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
        background-color: #f9f9f9;
      }
      h4 {
        margin-top: 30px;
        font-weight: 600;
      }
      .well {
        background-color: #ffffff;
        border: 1px solid #e0e0e0;
        border-radius: 10px;
        padding: 30px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
      }
    "))
  ),
  
  titlePanel("✨ Sistem Pendukung Keputusan - Metode MFEP"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alternatif", "Jumlah Alternatif:", 3, min = 1, max = 10),
      numericInput("kriteria", "Jumlah Kriteria:", 3, min = 1, max = 10),
      actionButton("generate", "Buat Form Penilaian", class = "btn btn-primary"),
      br(), br()
    ),
    
    mainPanel(
      div(class = "well",
          uiOutput("inputTables")
      ),
      br(),
      DTOutput("hasil")
    )
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues()
  
  # Validasi bobot
  bobotValid <- reactive({
    kri <- isolate(values$kri_count)
    if (is.null(kri)) return(FALSE)
    bobot <- numeric(kri)
    for (j in 1:kri) {
      id <- paste0("bobot_1_", j)
      val <- input[[id]]
      if (is.null(val)) return(FALSE)
      bobot[j] <- as.numeric(val)
    }
    sum(bobot) == 1
  })
  
  # Tombol hitung muncul hanya jika bobot valid
  output$showHitungBtn <- renderUI({
    req(values$kri_count)
    if (bobotValid()) {
      actionButton("hitung", "Hitung MFEP", class = "btn btn-success")
    }
  })
  
  # Buat form penilaian
  observeEvent(input$generate, {
    alt <- input$alternatif
    kri <- input$kriteria
    
    values$alternatif <- paste0("A", 1:alt)
    values$kriteria <- paste0("K", 1:kri)
    values$alt_count <- alt
    values$kri_count <- kri
    
    output$inputTables <- renderUI({
      tagList(
        h4("Masukkan Bobot Kriteria (total = 1.0)"),
        uiOutput("bobotWarning"),
        matrixInput("bobot", nrow = 1, ncol = kri, colNames = values$kriteria, defaultValue = 0, width = "80px"),
        
        h4("Masukkan Skor Alternatif terhadap Kriteria (1-5)"),
        matrixInput("skor", nrow = alt, ncol = kri, rowNames = values$alternatif, colNames = values$kriteria, defaultValue = 1, width = "80px"),
        div(style = "margin-top: 20px;", uiOutput("showHitungBtn"))
        )
    })
  })
  
  # Warning bobot
  output$bobotWarning <- renderUI({
    if (!isTRUE(bobotValid())) {
      tags$p("❗ Total bobot kriteria harus sama dengan 1.0", style = "color:red; font-weight:500; margin-top: -10px; margin-bottom: 10px;")
    }
  })
  
  # Kalkulasi MFEP
  observeEvent(input$hitung, {
    alt <- isolate(values$alt_count)
    kri <- isolate(values$kri_count)
    
    req(alt, kri)
    
    bobot <- numeric(kri)
    for (j in 1:kri) {
      id <- paste0("bobot_1_", j)
      bobot[j] <- as.numeric(input[[id]])
    }
    
    skor <- matrix(0, nrow = alt, ncol = kri)
    for (i in 1:alt) {
      for (j in 1:kri) {
        id <- paste0("skor_", i, "_", j)
        skor[i, j] <- as.numeric(input[[id]])
      }
    }
    
    hasil <- skor %*% bobot
    df <- data.frame(Alternatif = values$alternatif, Total_MFEP = round(hasil, 3))
    df <- df %>% arrange(desc(Total_MFEP))
    
    output$hasil <- renderDT(df, options = list(pageLength = 10))
  })
}

# Jalankan aplikasi
shinyApp(ui, server)
