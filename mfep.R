library(shiny)
library(dplyr)
library(DT)

# Fungsi bantu untuk membuat form input matrix
matrixInput <- function(inputId, nrow, ncol, rowNames = NULL, colNames = NULL, defaultValue = 0, width = "50px") {
  tableOutput <- tagList()
  
  # Header kolom
  if (!is.null(colNames)) {
    header <- fluidRow(
      column(2),
      lapply(1:ncol, function(j) column(2, strong(colNames[j])))
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
  titlePanel("Sistem Pendukung Keputusan - MFEP"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alternatif", "Jumlah Alternatif:", 3, min = 1, max = 10),
      numericInput("kriteria", "Jumlah Kriteria:", 3, min = 1, max = 10),
      actionButton("generate", "Buat Form Penilaian"),
      br(), br(),
      
    ),
    
    mainPanel(
      uiOutput("inputTables"),
      br(),
      uiOutput("bobotWarning"),  # <-- tempat pesan validasi
      br(),
      uiOutput("hitungButton"),  # <-- tombol akan tampil jika valid
      DTOutput("hasil")
    )
    
    
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues()
  
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
        matrixInput("bobot", nrow = 1, ncol = kri, colNames = values$kriteria, defaultValue = 0, width = "200px"),
        
        h4("Masukkan Skor Alternatif terhadap Kriteria (1-5)"),
        matrixInput("skor", nrow = alt, ncol = kri, rowNames = values$alternatif, colNames = values$kriteria, defaultValue = 1, width = "200px")
      )
    })
    
  })
  
  observe({
    req(values$kri_count)  # hanya lanjut kalau form sudah digenerate
    
    total_bobot <- 0
    valid <- TRUE
    
    for (j in 1:values$kri_count) {
      id <- paste0("bobot_1_", j)
      val <- input[[id]]
      if (is.null(val) || is.na(val)) {
        valid <- FALSE
      } else {
        total_bobot <- total_bobot + as.numeric(val)
      }
    }
    
    # Buat output validasi
    if (!valid || abs(total_bobot - 1.0) > 0.001) {
      output$bobotWarning <- renderUI({
        div(style = "color: red;", "❗ Total bobot kriteria harus sama dengan 1.0")
      })
      output$hitungButton <- renderUI({ NULL })
    } else {
      output$bobotWarning <- renderUI({ NULL })
      output$hitungButton <- renderUI({
        actionButton("hitung", "Hitung MFEP")
      })
    }
  })
  
  
  # Kalkulasi MFEP
  observeEvent(input$hitung, {
    alt <- isolate(values$alt_count)
    kri <- isolate(values$kri_count)
    
    req(alt, kri)  # pastikan form sudah digenerate
    
    # Ambil bobot
    bobot <- numeric(kri)
    for (j in 1:kri) {
      id <- paste0("bobot_1_", j)
      bobot[j] <- as.numeric(input[[id]])
    }
    
    # Ambil skor
    skor <- matrix(0, nrow = alt, ncol = kri)
    for (i in 1:alt) {
      for (j in 1:kri) {
        id <- paste0("skor_", i, "_", j)
        skor[i, j] <- as.numeric(input[[id]])
      }
    }
    
    # Kalkulasi MFEP
    hasil <- skor %*% bobot
    df <- data.frame(Alternatif = values$alternatif, Total_MFEP = round(hasil, 3))
    df <- df %>% arrange(desc(Total_MFEP))
    
    output$hasil <- renderDT(df, options = list(pageLength = 10))
  })
}

# Jalankan aplikasi
shinyApp(ui, server)
