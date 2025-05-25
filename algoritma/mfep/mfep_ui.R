# /algoritma/mfep/mfep_ui.R

# Fungsi createMatrixInput ada di utils.R.
# Jika utils.R tidak di-source secara global di dashboard.R, Anda perlu source di sini:
# source("../../utils/utils.R", local = TRUE) # Path relatif

# Modifikasi createMatrixInput khusus untuk MFEP agar ID nama alternatifnya sesuai
createMfepMatrixInput <- function(inputId, nrow, ncol, rowNames = NULL, colNames = NULL, defaultValue = 0, width = "60px") {
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
      id_input <- paste0(inputId, "_", i, "_", j) 
      rowInputs <- tagAppendChildren(rowInputs,
                                     column(2, numericInput(id_input, NULL, value = defaultValue, min = 0, max = 5, step = 0.1, width = width)))
    }
    # ID nama alternatif spesifik untuk MFEP server logic
    rowLabel <- if (!is.null(rowNames)) column(2, textInput(paste0("alt_name_mfep_", i), NULL, value = rowNames[i], width = "100px")) else column(2)
    tableOutput <- tagAppendChildren(tableOutput, fluidRow(rowLabel, rowInputs))
  }
  return(tableOutput)
}


getMfepPageUI <- function() {
  tagList(
    div(class="back-button-container", actionButton("back_to_home_mfep", "Kembali ke Dashboard", icon = shiny::icon("arrow-left"), class="btn btn-light")),
    titlePanel("✨ Metode Multi-Factor Evaluation Process (MFEP)"),
    sidebarLayout(
      sidebarPanel(
        numericInput("alternatif_mfep", "Jumlah Alternatif:", 3, min = 1, max = 10),
        numericInput("kriteria_mfep", "Jumlah Kriteria:", 3, min = 1, max = 10),
        actionButton("generate_mfep", "Buat Form Penilaian", class = "btn btn-primary"),
        br(), br(),
        uiOutput("mfep_action_buttons_ui") # Tombol Hitung & Reset akan muncul di sini
      ),
      mainPanel(
        div(class = "well", uiOutput("inputTables_mfep")),
        br(),
        DTOutput("hasil_mfep"),
        br(),
        plotlyOutput("hasilPlot_mfep")
      )
    )
  )
}