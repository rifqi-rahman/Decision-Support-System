# /algoritma/mfep/mfep_server.R

initializeMfepServerLogic <- function(input, output, session) {
  # Fungsi createMfepMatrixInput ada di mfep_ui.R, yang di-source oleh dashboard.R
  # atau bisa juga utils.R di-source di sini jika createMatrixInput umum dipakai.
  # Untuk kasus ini, createMfepMatrixInput ada di mfep_ui.R, jadi tidak perlu source lagi di sini.
  
  values_mfep <- reactiveValues(
    form_generated = FALSE, 
    hasil = NULL, 
    alt_count = 3, # Nilai default bisa disesuaikan atau diambil dari input
    kri_count = 3
  )
  
  # Ambil nama alternatif MFEP
  getAltNames_mfep <- reactive({
    req(input$alternatif_mfep) # Memastikan input$alternatif_mfep sudah ada
    sapply(1:input$alternatif_mfep, function(i) {
      name <- input[[paste0("alt_name_mfep_", i)]] # ID yang sesuai dengan mfep_ui.R
      if (is.null(name) || name == "") paste0("A", i) else name
    })
  })
  
  # Validasi jumlah bobot = 1 untuk MFEP
  bobotValid_mfep <- reactive({
    # Hanya validasi jika form sudah digenerate dan ada kriteria
    req(values_mfep$form_generated, input$kriteria_mfep)
    if (input$kriteria_mfep <= 0) return(FALSE)
    
    kri <- input$kriteria_mfep
    bobot_values <- sapply(1:kri, function(j) input[[paste0("bobot_1_", j)]])
    
    if (any(sapply(bobot_values, is.null))) return(FALSE) # Jika ada bobot yang belum diisi
    
    round(sum(as.numeric(bobot_values), na.rm = TRUE), 3) == 1.0
  })
  
  # Form Penilaian MFEP
  observeEvent(input$generate_mfep, {
    alt <- input$alternatif_mfep
    kri <- input$kriteria_mfep
    req(alt, kri) # Pastikan alt dan kri ada nilainya
    
    values_mfep$alt_count <- alt
    values_mfep$kri_count <- kri
    values_mfep$form_generated <- TRUE
    
    output$inputTables_mfep <- renderUI({
      tagList(
        h4("Masukkan Bobot Kriteria (total = 1.0)"),
        uiOutput("bobotWarning_mfep"), # Warning bobot
        # Menggunakan createMfepMatrixInput dari mfep_ui.R
        createMfepMatrixInput("bobot", 1, kri, colNames = paste0("K", 1:kri), defaultValue = 0, width = "80px"),
        h4("Masukkan Skor Alternatif terhadap Kriteria"),
        createMfepMatrixInput("skor", alt, kri, rowNames = paste0("A", 1:alt), colNames = paste0("K", 1:kri), defaultValue = 1, width = "80px")
      )
    })
    # Reset hasil jika form digenerate ulang
    output$hasil_mfep <- renderDT({})
    output$hasilPlot_mfep <- renderPlotly({})
    values_mfep$hasil <- NULL
  })
  
  # Warning Bobot MFEP
  output$bobotWarning_mfep <- renderUI({
    if (values_mfep$form_generated && !bobotValid_mfep()) {
      tags$p("❗ Total bobot kriteria harus sama dengan 1.0", style = "color:red; font-weight:500;")
    }
  })
  
  # Tombol Hitung dan Reset untuk MFEP
  output$mfep_action_buttons_ui <- renderUI({
    if (values_mfep$form_generated) {
      div(style = "margin-top: 20px; display: flex; gap: 10px;",
          if (bobotValid_mfep()) {
            actionButton("hitung_mfep", "Hitung MFEP", class = "btn btn-success")
          } else {
            actionButton("hitung_mfep_disabled", "Hitung MFEP", class = "btn btn-success shiny-bound-input", disabled = TRUE, title = "Total bobot kriteria harus sama dengan 1.0")
          },
          actionButton("reset_mfep", "Reset Form MFEP", class = "btn btn-danger")
      )
    }
  })
  
  # RESET MFEP
  observeEvent(input$reset_mfep, {
    alt <- values_mfep$alt_count
    kri <- values_mfep$kri_count
    
    if (!is.null(kri) && kri > 0) {
      for (j in 1:kri) updateNumericInput(session, paste0("bobot_1_", j), value = 0)
    }
    if (!is.null(alt) && alt > 0 && !is.null(kri) && kri > 0) {
      for (i in 1:alt) {
        updateTextInput(session, paste0("alt_name_mfep_", i), value = paste0("A",i))
        for (j in 1:kri) updateNumericInput(session, paste0("skor_", i, "_", j), value = 1)
      }
    }
    
    values_mfep$form_generated <- FALSE
    values_mfep$hasil <- NULL
    
    output$inputTables_mfep <- renderUI({}) # Kosongkan form
    # output$mfep_action_buttons_ui juga akan hilang karena values_mfep$form_generated = FALSE
    
    showNotification("Form MFEP berhasil di-reset. Klik 'Buat Form Penilaian' untuk memulai lagi.", type = "message", duration = 5)
  })
  
  # Hitung MFEP
  observeEvent(input$hitung_mfep, {
    if (is.null(values_mfep$form_generated) || !values_mfep$form_generated) {
      showModal(modalDialog(title = "Form Belum Dibuat", "Silakan klik 'Buat Form Penilaian' terlebih dahulu.", easyClose = TRUE)); return()
    }
    if (!bobotValid_mfep()) {
      showModal(modalDialog(title = "Bobot Tidak Valid", "Total bobot kriteria harus sama dengan 1.0.", easyClose = TRUE)); return()
    }
    
    alt <- values_mfep$alt_count
    kri <- values_mfep$kri_count
    req(alt, kri)
    
    # Validasi input tersedia (skor dan bobot)
    bobot_inputs_valid <- all(sapply(1:kri, function(j) !is.null(input[[paste0("bobot_1_", j)]])))
    skor_inputs_valid <- all(sapply(1:alt, function(i) all(sapply(1:kri, function(j) !is.null(input[[paste0("skor_", i, "_", j)]])))))
    
    if (!bobot_inputs_valid || !skor_inputs_valid) {
      showModal(modalDialog(title = "Form Tidak Lengkap", "Beberapa input skor atau bobot belum dimasukkan atau form belum sepenuhnya ter-render. Pastikan semua kolom telah terisi.", easyClose = TRUE)); return()
    }
    
    bobot_data <- sapply(1:kri, function(j) as.numeric(input[[paste0("bobot_1_", j)]]))
    skor_data <- matrix(0, nrow = alt, ncol = kri)
    for (i in 1:alt) for (j in 1:kri) skor_data[i, j] <- as.numeric(input[[paste0("skor_", i, "_", j)]])
    
    hasil_calc <- skor_data %*% bobot_data
    df <- data.frame(Alternatif = getAltNames_mfep(), Total_MFEP = round(hasil_calc, 3)) %>%
      dplyr::arrange(dplyr::desc(Total_MFEP)) %>%
      dplyr::mutate(Ranking = dplyr::row_number())
    
    values_mfep$hasil <- df
    
    output$hasil_mfep <- DT::renderDT({
      DT::datatable(df, options = list(pageLength = 10, scrollX=TRUE), rownames = FALSE,
                    caption = tags$caption(style = "caption-side: top; text-align: center; font-size: 1.2em; color:black;", "Tabel Hasil Perhitungan dan Perankingan MFEP")) %>%
        DT::formatStyle('Ranking', target = 'row', backgroundColor = DT::styleEqual(1, '#e6f2ff'))
    })
    
    output$hasilPlot_mfep <- plotly::renderPlotly({
      p <- plotly::plot_ly(df, x = ~Total_MFEP, y = ~reorder(Alternatif, Total_MFEP), type = 'bar', orientation = 'h',
                           marker = list(color = 'rgba(0,123,255,0.7)', line = list(color = 'rgba(0,123,255,1.0)', width = 1))) %>%
        plotly::layout(title = list(text='Visualisasi Hasil MFEP', x = 0.5), # tengahkan judul
                       xaxis = list(title = 'Skor Total MFEP'),
                       yaxis = list(title = 'Alternatif', automargin = TRUE), # automargin untuk nama alternatif panjang
                       margin = list(l = 120)) # margin kiri untuk yaxis
      p
    })
  })
}