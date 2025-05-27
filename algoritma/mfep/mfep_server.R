# /algoritma/mfep/mfep_server.R

initializeMfepServerLogic <- function(input, output, session) {
  
  values_mfep <- reactiveValues(
    form_generated = FALSE, 
    hasil = NULL, 
    alt_count = 3,
    kri_count = 3,
    detail_perhitungan = NULL # ---- TAMBAHAN BARU: Untuk menyimpan detail perhitungan ----
  )
  
  getAltNames_mfep <- reactive({
    req(input$alternatif_mfep)
    sapply(1:input$alternatif_mfep, function(i) {
      name <- input[[paste0("alt_name_mfep_", i)]]
      if (is.null(name) || name == "") paste0("A", i) else name
    })
  })
  
  bobotValid_mfep <- reactive({
    req(values_mfep$form_generated, input$kriteria_mfep)
    if (input$kriteria_mfep <= 0) return(FALSE)
    
    kri <- input$kriteria_mfep
    bobot_values <- sapply(1:kri, function(j) input[[paste0("bobot_1_", j)]])
    
    if (any(sapply(bobot_values, is.null))) return(FALSE)
    
    round(sum(as.numeric(bobot_values), na.rm = TRUE), 3) == 1.0
  })
  
  observeEvent(input$generate_mfep, {
    alt <- input$alternatif_mfep
    kri <- input$kriteria_mfep
    req(alt, kri)
    
    values_mfep$alt_count <- alt
    values_mfep$kri_count <- kri
    values_mfep$form_generated <- TRUE
    
    output$inputTables_mfep <- renderUI({
      tagList(
        h4("Masukkan Bobot Kriteria (total = 1.0)"),
        uiOutput("bobotWarning_mfep"),
        createMfepMatrixInput("bobot", 1, kri, colNames = paste0("K", 1:kri), defaultValue = 0, width = "80px"),
        h4("Masukkan Skor Alternatif terhadap Kriteria (1-5)"), # Teks diperbarui
        createMfepMatrixInput("skor", alt, kri, rowNames = paste0("A", 1:alt), colNames = paste0("K", 1:kri), defaultValue = 1, width = "80px")
      )
    })
    # Reset hasil dan detail jika form digenerate ulang
    output$hasil_mfep <- renderDT({})
    output$hasilPlot_mfep <- renderPlotly({})
    output$mfep_detail_tables_ui <- renderUI({}) # ---- TAMBAHAN BARU: Reset UI detail ----
    values_mfep$hasil <- NULL
    values_mfep$detail_perhitungan <- NULL # ---- TAMBAHAN BARU: Reset data detail ----
  })
  
  output$bobotWarning_mfep <- renderUI({
    if (values_mfep$form_generated && !bobotValid_mfep()) {
      tags$p("❗ Total bobot kriteria harus sama dengan 1.0", style = "color:red; font-weight:500;")
    }
  })
  
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
    values_mfep$detail_perhitungan <- NULL # ---- TAMBAHAN BARU: Reset data detail ----
    
    output$inputTables_mfep <- renderUI({})
    output$hasil_mfep <- renderDT({}) # Kosongkan tabel hasil utama
    output$hasilPlot_mfep <- renderPlotly({}) # Kosongkan plot
    output$mfep_detail_tables_ui <- renderUI({}) # ---- TAMBAHAN BARU: Reset UI detail ----
    
    showNotification("Form MFEP berhasil di-reset. Klik 'Buat Form Penilaian' untuk memulai lagi.", type = "message", duration = 5)
  })
  
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
    
    bobot_inputs_valid <- all(sapply(1:kri, function(j) !is.null(input[[paste0("bobot_1_", j)]])))
    skor_inputs_valid <- all(sapply(1:alt, function(i) all(sapply(1:kri, function(j) !is.null(input[[paste0("skor_", i, "_", j)]])))))
    
    if (!bobot_inputs_valid || !skor_inputs_valid) {
      showModal(modalDialog(title = "Form Tidak Lengkap", "Beberapa input skor atau bobot belum dimasukkan atau form belum sepenuhnya ter-render. Pastikan semua kolom telah terisi.", easyClose = TRUE)); return()
    }
    
    bobot_data <- sapply(1:kri, function(j) as.numeric(input[[paste0("bobot_1_", j)]]))
    skor_data <- matrix(0, nrow = alt, ncol = kri)
    for (i in 1:alt) for (j in 1:kri) skor_data[i, j] <- as.numeric(input[[paste0("skor_", i, "_", j)]])
    
    hasil_calc <- skor_data %*% bobot_data
    df_hasil_utama <- data.frame(Alternatif = getAltNames_mfep(), Total_MFEP = round(hasil_calc, 3)) %>%
      dplyr::arrange(dplyr::desc(Total_MFEP)) %>%
      dplyr::mutate(Ranking = dplyr::row_number())
    
    values_mfep$hasil <- df_hasil_utama
    
    # ---- Blok BARU: Membuat dan Menyimpan Detail Perhitungan ----
    detail_perhitungan_list <- list()
    nama_alternatif_lengkap <- getAltNames_mfep() 
    
    for (i_alt in 1:alt) {
      detail_df_alt <- data.frame(
        No = 1:kri, # Nomor urut kriteria
        Kriteria = paste0("K", 1:kri),
        `Bobot Kriteria` = bobot_data,
        `Nilai Evaluasi` = skor_data[i_alt, ],
        `Bobot X Nilai` = round(bobot_data * skor_data[i_alt, ], 3),
        check.names = FALSE # Mencegah R mengubah spasi di nama kolom menjadi titik
      )
      
      total_weighted_evaluation_alt <- sum(detail_df_alt$`Bobot X Nilai`)
      
      detail_perhitungan_list[[nama_alternatif_lengkap[i_alt]]] <- list(
        data = detail_df_alt,
        total = round(total_weighted_evaluation_alt, 3)
      )
    }
    values_mfep$detail_perhitungan <- detail_perhitungan_list
    # ---- Akhir Blok BARU ----
    
    # Render tabel hasil utama
    output$hasil_mfep <- DT::renderDT({
      DT::datatable(df_hasil_utama, options = list(pageLength = 10, scrollX=TRUE), rownames = FALSE,
                    caption = tags$caption(style = "caption-side: top; text-align: center; font-size: 1.2em; color:var(--text-color-primary);", "Tabel Hasil Perhitungan dan Perankingan MFEP")) %>% # Menggunakan var(--text-color-primary)
        DT::formatStyle('Ranking', target = 'row', backgroundColor = DT::styleEqual(1, '#e6f2ff'))
    })
    
    # Render plot hasil utama
    output$hasilPlot_mfep <- plotly::renderPlotly({
      p <- plotly::plot_ly(df_hasil_utama, x = ~Total_MFEP, y = ~reorder(Alternatif, Total_MFEP), type = 'bar', orientation = 'h',
                           marker = list(color = 'rgba(0,123,255,0.7)', line = list(color = 'rgba(0,123,255,1.0)', width = 1))) %>%
        plotly::layout(title = list(text='Visualisasi Hasil MFEP', x = 0.5, font = list(color="var(--text-color-primary)")), # Menggunakan var(--text-color-primary)
                       xaxis = list(title = 'Skor Total MFEP', color="var(--text-color-secondary)", gridcolor="var(--border-color-soft)"), # Menggunakan var(--text-color-secondary)
                       yaxis = list(title = 'Alternatif', automargin = TRUE, color="var(--text-color-secondary)", gridcolor="var(--border-color-soft)"), # Menggunakan var(--text-color-secondary)
                       margin = list(l = 120),
                       paper_bgcolor='rgba(0,0,0,0)', # Transparan agar mengikuti bg body
                       plot_bgcolor='rgba(0,0,0,0)')   # Transparan
      p
    })
    
    # ---- Blok BARU: Merender UI untuk tabel-tabel detail ----
    output$mfep_detail_tables_ui <- renderUI({
      req(values_mfep$detail_perhitungan)
      
      detail_tables_output_list <- lapply(names(values_mfep$detail_perhitungan), function(alt_name) {
        table_output_id <- paste0("detail_table_mfep_", gsub("[^A-Za-z0-9_]", "", alt_name)) # ID valid
        
        # Render tabel DT spesifik untuk alternatif ini
        # Perlu `local({})` agar `alt_name` di-capture dengan benar untuk setiap iterasi
        local({
          current_alt_name <- alt_name
          output[[table_output_id]] <- DT::renderDT({
            DT::datatable(values_mfep$detail_perhitungan[[current_alt_name]]$data,
                          options = list(dom = 't', paging = FALSE, ordering = FALSE, searching = FALSE,
                                         columnDefs = list(list(className = 'dt-center', targets = '_all'))), # Center align semua kolom
                          rownames = FALSE,
                          caption = tags$caption(
                            style = "caption-side: bottom; text-align: right; color:var(--text-color-secondary); padding-top:10px; padding-bottom:15px;", # Menggunakan var(--text-color-secondary)
                            tags$strong("Total Weighted Evaluation: ", values_mfep$detail_perhitungan[[current_alt_name]]$total)
                          ),
                          style = "bootstrap4", # Atau "bootstrap5" jika Anda sudah upgrade Bootstrap
                          class = "compact hover stripe" # Kelas DT untuk styling
            )
          })
        }) # Akhir local scope
        
        # UI untuk satu blok detail alternatif
        tagList(
          tags$h4(paste("Proses Evaluasi untuk Alternatif:", alt_name), style="margin-top: 25px; color:var(--text-color-primary);"), # Menggunakan var(--text-color-primary)
          DT::DTOutput(table_output_id),
          hr(style="border-top: 1px solid var(--border-color-soft);") # Menggunakan var(--border-color-soft)
        )
      }) # Akhir lapply
      
      if(length(values_mfep$detail_perhitungan) > 0){
        return(tagList(
          tags$h3("Rincian Proses Perhitungan per Alternatif", style="margin-top:40px; margin-bottom:20px; text-align:center; color:var(--text-color-primary);"), # Menggunakan var(--text-color-primary)
          detail_tables_output_list
        ))
      } else {
        return(NULL)
      }
    })
    # ---- Akhir Blok BARU ----
  })
}
