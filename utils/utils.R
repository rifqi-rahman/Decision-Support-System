# /utils/utils.R

# Helper Function untuk Membuat Kartu Algoritma
createAlgorithmCard <- function(id, title, description, icon_name = "cogs", status = "coming_soon") {
  button_label <- if (status == "active") "Pilih Metode" else "Lihat Detail"
  button_class <- if (status == "active") "btn btn-primary" else "btn btn-outline-secondary"
  
  div(class = "algorithm-card",
      id = paste0("card_", id), # ID untuk kartu, bisa digunakan untuk JS/CSS spesifik jika perlu
      div(class="card-icon", shiny::icon(icon_name)),
      tags$h4(title),
      tags$p(description),
      actionButton(paste0("btn_", id), button_label, class = button_class)
  )
}

# Helper: Create matrix input UI (digunakan oleh MFEP)
createMatrixInput <- function(inputId, nrow, ncol, rowNames = NULL, colNames = NULL, defaultValue = 0, width = "60px") {
  tableOutput <- tagList()
  
  if (!is.null(colNames)) {
    header <- fluidRow(
      column(2), # Placeholder untuk row names
      lapply(1:ncol, function(j) column(2, tags$div(strong(colNames[j]), style = "margin-bottom: 5px;")))
    )
    tableOutput <- tagAppendChildren(tableOutput, header)
  }
  
  for (i in 1:nrow) {
    rowInputs <- tagList()
    for (j in 1:ncol) {
      # ID input harus unik dalam konteks penggunaannya (misal, MFEP akan menggunakan prefix sendiri)
      id_input <- paste0(inputId, "_", i, "_", j)
      rowInputs <- tagAppendChildren(rowInputs,
                                     column(2, numericInput(id_input, NULL, value = defaultValue, min = 0, max = 5, step = 0.1, width = width)))
    }
    # Input nama alternatif, ID juga perlu spesifik jika digunakan (misal, alt_name_mfep_)
    # Untuk MFEP, ID nama alternatif akan di-handle di mfep_ui.R
    rowLabelInputId <- if (!is.null(rowNames)) paste0("alt_name_", inputId, "_", i) else paste0("placeholder_alt_name_", inputId, "_", i)
    rowLabelValue <- if(!is.null(rowNames)) rowNames[i] else ""
    
    rowLabel <- if (!is.null(rowNames)) column(2, textInput(rowLabelInputId, NULL, value = rowLabelValue, width = "100px")) else column(2)
    tableOutput <- tagAppendChildren(tableOutput, fluidRow(rowLabel, rowInputs))
  }
  return(tableOutput)
}