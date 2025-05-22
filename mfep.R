# app.R
library(shiny)
library(dplyr)
library(DT)
library(plotly)

### Helper: Create matrix input UI
createMatrixInput <- function(inputId, nrow, ncol, rowNames = NULL, colNames = NULL, defaultValue = 0, width = "60px") {
  tableOutput <- tagList()
  
  if (!is.null(colNames)) {
    header <- fluidRow(
      column(2),
      lapply(1:ncol, function(j) column(2, tags$div(strong(colNames[j]), style = "margin-bottom: 5px;")))
    )
    tableOutput <- tagAppendChildren(tableOutput, header)
  }
  
  for (i in 1:nrow) {
    rowInputs <- tagList()
    for (j in 1:ncol) {
      id <- paste0(inputId, "_", i, "_", j)
      rowInputs <- tagAppendChildren(rowInputs,
                                     column(2, numericInput(id, NULL, value = defaultValue, min = 0, max = 5, step = 0.1, width = width)))
    }
    rowLabel <- if (!is.null(rowNames)) column(2, textInput(paste0("alt_name_", i), NULL, value = rowNames[i], width = "100px")) else column(2)
    tableOutput <- tagAppendChildren(tableOutput, fluidRow(rowLabel, rowInputs))
  }
  return(tableOutput)
}

### UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("body {font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto;} .well {background: #fff; padding: 25px; border-radius: 10px;}"))
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
      div(class = "well", uiOutput("inputTables")),
      br(),
      DTOutput("hasil"),
      br(),
      plotlyOutput("hasilPlot")
    )
  )
)

### Server
server <- function(input, output, session) {
  values <- reactiveValues()
  
  ### Ambil nama alternatif
  getAltNames <- reactive({
    alt <- input$alternatif
    sapply(1:alt, function(i) {
      name <- input[[paste0("alt_name_", i)]]
      if (is.null(name) || name == "") paste0("A", i) else name
    })
  })
  
  ### Validasi jumlah bobot = 1
  bobotValid <- reactive({
    kri <- input$kriteria
    if (is.null(kri)) return(FALSE)
    bobot <- numeric(kri)
    for (j in 1:kri) {
      id <- paste0("bobot_1_", j)
      val <- input[[id]]
      if (is.null(val)) return(FALSE)
      bobot[j] <- as.numeric(val)
    }
    round(sum(bobot), 3) == 1
  })
  
  ### Form Penilaian
  observeEvent(input$generate, {
    alt <- input$alternatif
    kri <- input$kriteria
    
    values$alt_count <- alt
    values$kri_count <- kri
    values$form_generated <- TRUE
    
    output$inputTables <- renderUI({
      tagList(
        h4("Masukkan Bobot Kriteria (total = 1.0)"),
        uiOutput("bobotWarning"),
        createMatrixInput("bobot", 1, kri, colNames = paste0("K", 1:kri), defaultValue = 0, width = "80px"),
        
        h4("Masukkan Skor Alternatif terhadap Kriteria (1-5)"),
        createMatrixInput("skor", alt, kri, rowNames = paste0("A", 1:alt), colNames = paste0("K", 1:kri), defaultValue = 1, width = "80px"),
        
        div(style = "margin-top: 20px;", uiOutput("showButtons"))
      )
    })
  })
  
  ### Warning Bobot
  output$bobotWarning <- renderUI({
    if (!bobotValid()) {
      tags$p("❗ Total bobot kriteria harus sama dengan 1.0", style = "color:red; font-weight:500;")
    }
  })
  
  ### Tombol Hitung dan Reset muncul jika bobot valid
  observe({
    if (!is.null(values$form_generated) && values$form_generated && bobotValid()) {
      output$showButtons <- renderUI({
        div(style = "display: flex; gap: 10px;",
            actionButton("hitung", "Hitung MFEP", class = "btn btn-success"),
            actionButton("reset", "Reset", class = "btn btn-danger"))
      })
    } else {
      output$showButtons <- renderUI({})
    }
  })
  
  ### RESET
  observeEvent(input$reset, {
    alt <- values$alt_count
    kri <- values$kri_count
    
    if (!is.null(kri)) for (j in 1:kri) updateNumericInput(session, paste0("bobot_1_", j), value = 0)
    if (!is.null(alt) && !is.null(kri)) {
      for (i in 1:alt) {
        for (j in 1:kri) updateNumericInput(session, paste0("skor_", i, "_", j), value = 1)
      }
    }
    
    values$alt_count <- NULL
    values$kri_count <- NULL
    values$hasil <- NULL
    values$form_generated <- FALSE
    
    output$inputTables <- renderUI({})
    output$hasil <- renderDT({})
    output$hasilPlot <- renderPlotly({})
    output$showButtons <- renderUI({})
    
    showNotification("Form berhasil di-reset. Silakan klik 'Buat Form Penilaian' ulang.", type = "message")
  })
  
  ### Hitung MFEP
  observeEvent(input$hitung, {
    if (is.null(values$form_generated) || !values$form_generated) {
      showModal(modalDialog(
        title = "Form Belum Dibuat",
        "Silakan klik 'Buat Form Penilaian' terlebih dahulu sebelum menghitung.",
        easyClose = TRUE
      ))
      return()
    }
    
    alt <- input$alternatif
    kri <- input$kriteria
    req(alt, kri)
    
    # Validasi input tersedia
    if (any(sapply(1:kri, function(j) is.null(input[[paste0("bobot_1_", j)]]))) ||
        any(sapply(1:alt, function(i) any(sapply(1:kri, function(j) is.null(input[[paste0("skor_", i, "_", j)]])))))) {
      showModal(modalDialog(
        title = "Form Tidak Lengkap",
        "Beberapa input skor atau bobot belum dimasukkan. Pastikan semua kolom telah terisi.",
        easyClose = TRUE
      ))
      return()
    }
    
    bobot <- sapply(1:kri, function(j) as.numeric(input[[paste0("bobot_1_", j)]]))
    skor <- matrix(0, nrow = alt, ncol = kri)
    for (i in 1:alt) for (j in 1:kri) skor[i, j] <- as.numeric(input[[paste0("skor_", i, "_", j)]])
    
    hasil <- skor %*% bobot
    df <- data.frame(Alternatif = getAltNames(), Total_MFEP = round(hasil, 3)) %>%
      arrange(desc(Total_MFEP)) %>%
      mutate(Ranking = row_number())
    
    values$hasil <- df
    
    output$hasil <- renderDT({
      datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
        formatStyle('Ranking', target = 'row', backgroundColor = styleEqual(1, '#e6f2ff'))
    })
    
    output$hasilPlot <- renderPlotly({
      plot_ly(df, x = ~Total_MFEP, y = ~reorder(Alternatif, Total_MFEP), type = 'bar', orientation = 'h',
              marker = list(color = 'rgba(0,123,255,0.7)', line = list(color = 'rgba(0,123,255,1.0)', width = 1))) %>%
        layout(title = 'Visualisasi Hasil MFEP',
               xaxis = list(title = 'Skor Total MFEP'),
               yaxis = list(title = 'Alternatif'),
               margin = list(l = 100))
    })
  })
}

### Run App
shinyApp(ui, server)
