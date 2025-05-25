library(shiny)
library(shinyjs) # Untuk show/hide atau aksi JS lainnya jika diperlukan
library(dplyr)
library(DT)
library(plotly)

# --- Helper Function untuk Membuat Kartu Algoritma ---
createAlgorithmCard <- function(id, title, description, icon_name = "cogs", status = "coming_soon") {
  button_label <- if (status == "active") "Pilih Metode" else "Lihat Detail"
  button_class <- if (status == "active") "btn btn-primary" else "btn btn-outline-secondary"
  
  div(class = "algorithm-card",
      id = paste0("card_", id),
      div(class="card-icon", icon(icon_name)), # Menggunakan ikon Font Awesome
      tags$h4(title),
      tags$p(description),
      actionButton(paste0("btn_", id), button_label, class = button_class)
  )
}

# --- UI untuk Modul MFEP (Diambil dari kode Anda dan disederhanakan) ---
uiMfep <- function() {
  fluidPage(
    div(class="back-button-container", actionButton("back_to_home_mfep", "Kembali ke Dashboard", icon = icon("arrow-left"), class="btn btn-light")),
    titlePanel("✨ Metode Multi-Factor Evaluation Process (MFEP)"),
    sidebarLayout(
      sidebarPanel(
        numericInput("alternatif_mfep", "Jumlah Alternatif:", 3, min = 1, max = 10),
        numericInput("kriteria_mfep", "Jumlah Kriteria:", 3, min = 1, max = 10),
        actionButton("generate_mfep", "Buat Form Penilaian", class = "btn btn-primary"),
        br(), br(),
        # Tombol Hitung & Reset akan muncul di sini melalui uiOutput
        uiOutput("mfep_action_buttons_ui")
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

### Helper: Create matrix input UI (digunakan oleh MFEP)
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
      id <- paste0(inputId, "_", i, "_", j) # ID unik untuk MFEP
      rowInputs <- tagAppendChildren(rowInputs,
                                     column(2, numericInput(id, NULL, value = defaultValue, min = 0, max = 5, step = 0.1, width = width)))
    }
    # Input nama alternatif untuk MFEP
    rowLabel <- if (!is.null(rowNames)) column(2, textInput(paste0("alt_name_mfep_", i), NULL, value = rowNames[i], width = "100px")) else column(2)
    tableOutput <- tagAppendChildren(tableOutput, fluidRow(rowLabel, rowInputs))
  }
  return(tableOutput)
}


# --- UI Utama Aplikasi ---
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), # Memanggil CSS kustom
    tags$head(
      # ... link CSS Anda ...
      tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
      tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA), # crossorigin butuh NA di R
      tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap", rel="stylesheet"),
      tags$title("Dashboard Sistem Pendukung Keputusan")
    ),
  ),
  useShinyjs(), # Inisialisasi shinyjs
  
  # Container utama yang kontennya akan diubah secara dinamis
  uiOutput("main_content")
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  current_view <- reactiveVal("home") # Default view adalah home
  
  # Navigasi
  observeEvent(input$btn_mfep, { current_view("mfep") })
  observeEvent(input$btn_topsis, { current_view("coming_soon_topsis") })
  observeEvent(input$btn_ahp, { current_view("coming_soon_ahp") })
  observeEvent(input$btn_saw, { current_view("coming_soon_saw") })
  observeEvent(input$btn_wp, { current_view("coming_soon_wp") })
  observeEvent(input$btn_fc, { current_view("coming_soon_fc") })
  observeEvent(input$btn_cf, { current_view("coming_soon_cf") })
  observeEvent(input$btn_nb, { current_view("coming_soon_nb") })
  observeEvent(input$btn_ds, { current_view("coming_soon_ds") })
  observeEvent(input$btn_fuzzy, { current_view("coming_soon_fuzzy") })
  
  # Tombol kembali
  observeEvent(input$back_to_home_mfep, { current_view("home") })
  observeEvent(input$back_to_home_coming_soon, { current_view("home") })
  
  # Render UI berdasarkan current_view
  output$main_content <- renderUI({
    if (current_view() == "home") {
      tagList(
        # 1. Bagian Landing Page / Intro DSS
        div(class = "landing-section",
            tags$h1("Decision Support System (DSS)"),
            tags$p("Sistem Pendukung Keputusan (DSS) adalah sistem berbasis komputer yang membantu proses pengambilan keputusan dengan menganalisis sejumlah besar data dan menyajikan informasi yang relevan secara terstruktur. DSS dirancang untuk membantu manajer membuat keputusan yang lebih baik dengan menyediakan alat untuk analisis data, pemodelan, dan simulasi."),
            tags$p("Tujuan utama DSS adalah untuk meningkatkan kualitas keputusan, bukan untuk menggantikan peran pengambil keputusan. Dengan DSS, Anda dapat mengeksplorasi berbagai skenario, mengevaluasi alternatif, dan memahami potensi dampak dari keputusan Anda."),
            # tags$img(src="placeholder_dss_image.png", class="landing-image-placeholder", alt="Ilustrasi DSS"), # Ganti dengan path gambar Anda jika ada
            tags$div(class="landing-image-placeholder", style="height: 200px; background: #eee; display:flex; align-items:center; justify-content:center;", "Tempat Ilustrasi DSS")
        ),
        # 2. Bagian Kartu Algoritma
        tags$h2("Pilih Alat Analisis:", style="text-align:center; margin-bottom:30px;"),
        div(class = "algorithms-grid",
            createAlgorithmCard("mfep", "MFEP", "Multi-Factor Evaluation Process untuk evaluasi multi-kriteria.", icon_name="calculator", status="active"),
            createAlgorithmCard("topsis", "TOPSIS", "Technique for Order of Preference by Similarity to Ideal Solution.", icon_name="bullseye"),
            createAlgorithmCard("ahp", "AHP", "Analytic Hierarchy Process untuk pembobotan kriteria dan pemilihan alternatif.", icon_name="sitemap"),
            createAlgorithmCard("saw", "SAW", "Simple Additive Weighting untuk penilaian berdasarkan bobot.", icon_name="balance-scale"),
            createAlgorithmCard("wp", "Weighted Product", "Metode Produk Terbobot untuk masalah MADM.", icon_name="weight-hanging"),
            createAlgorithmCard("fc", "Forward Chaining", "Metode inferensi berbasis aturan (Rule-Based System).", icon_name="project-diagram"),
            createAlgorithmCard("cf", "Certainty Factor", "Mengukur tingkat keyakinan dalam sistem pakar.", icon_name="check-circle"),
            createAlgorithmCard("nb", "Naive Bayes", "Klasifikasi probabilistik berdasarkan teorema Bayes.", icon_name="brain"),
            createAlgorithmCard("ds", "Dempster-Shafer", "Teori pembuktian untuk menggabungkan bukti dari sumber berbeda.", icon_name="puzzle-piece"),
            createAlgorithmCard("fuzzy", "Logika Fuzzy", "Mengatasi ketidakpastian dan ketidakjelasan dalam data.", icon_name="wave-square")
        )
      )
    } else if (current_view() == "mfep") {
      div(class="mfep-container", uiMfep())
    } else if (startsWith(current_view(), "coming_soon")) {
      # Ekstrak nama metode dari current_view()
      method_name_raw <- sub("coming_soon_", "", current_view())
      method_name <- switch(method_name_raw,
                            "topsis" = "TOPSIS",
                            "ahp" = "AHP",
                            "saw" = "SAW",
                            "wp" = "Weighted Product",
                            "fc" = "Forward Chaining",
                            "cf" = "Certainty Factor",
                            "nb" = "Naive Bayes",
                            "ds" = "Dempster-Shafer",
                            "fuzzy" = "Logika Fuzzy",
                            "Metode Ini")
      
      
      div(class="coming-soon-container",
          tags$h2(paste(method_name, "Segera Hadir!")),
          tags$p("Fitur ini sedang dalam pengembangan dan akan tersedia secepatnya."),
          tags$p("Terima kasih atas kesabaran Anda."),
          icon("tools", class="fa-3x text-muted mb-3"),
          br(),
          actionButton("back_to_home_coming_soon", "Kembali ke Dashboard", icon=icon("arrow-left"), class="btn btn-primary btn-lg")
      )
    }
  })
  
  # --- Logika Server untuk MFEP ---
  # (PENTING: Pastikan input ID unik jika Anda memiliki banyak modul nanti, atau gunakan Shiny Modules)
  # Untuk sekarang, saya tambahkan suffix "_mfep" pada input MFEP
  
  values_mfep <- reactiveValues(form_generated = FALSE, hasil = NULL)
  
  ### Ambil nama alternatif MFEP
  getAltNames_mfep <- reactive({
    req(input$alternatif_mfep)
    sapply(1:input$alternatif_mfep, function(i) {
      name <- input[[paste0("alt_name_mfep_", i)]] # ID unik
      if (is.null(name) || name == "") paste0("A", i) else name
    })
  })
  
  ### Validasi jumlah bobot = 1 untuk MFEP
  bobotValid_mfep <- reactive({
    req(input$kriteria_mfep)
    kri <- input$kriteria_mfep
    bobot <- numeric(kri)
    valid_sum <- TRUE
    if (kri > 0 && values_mfep$form_generated) { # Hanya validasi jika form sudah digenerate
      for (j in 1:kri) {
        id <- paste0("bobot_1_", j) # ID ini harus konsisten dengan createMatrixInput
        val <- input[[id]]
        if (is.null(val)) {
          valid_sum <- FALSE
          break
        }
        bobot[j] <- as.numeric(val)
      }
      if(valid_sum) round(sum(bobot), 3) == 1 else FALSE
    } else {
      FALSE # Jika belum ada kriteria atau form belum generate, anggap tidak valid untuk tombol
    }
  })
  
  ### Form Penilaian MFEP
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
        uiOutput("bobotWarning_mfep"),
        createMatrixInput("bobot", 1, kri, colNames = paste0("K", 1:kri), defaultValue = 0, width = "80px"),
        
        h4("Masukkan Skor Alternatif terhadap Kriteria (1-5)"),
        createMatrixInput("skor", alt, kri, rowNames = paste0("A", 1:alt), colNames = paste0("K", 1:kri), defaultValue = 1, width = "80px")
      )
    })
    # Reset hasil jika form digenerate ulang
    output$hasil_mfep <- renderDT({})
    output$hasilPlot_mfep <- renderPlotly({})
    values_mfep$hasil <- NULL
  })
  
  ### Warning Bobot MFEP
  output$bobotWarning_mfep <- renderUI({
    # Hanya tampilkan warning jika form sudah digenerate dan bobot tidak valid
    if (values_mfep$form_generated && !bobotValid_mfep()) {
      tags$p("❗ Total bobot kriteria harus sama dengan 1.0", style = "color:red; font-weight:500;")
    }
  })
  
  ### Tombol Hitung dan Reset untuk MFEP
  output$mfep_action_buttons_ui <- renderUI({
    if (values_mfep$form_generated) {
      div(style = "margin-top: 20px; display: flex; gap: 10px;",
          # Tombol hitung hanya aktif jika bobot valid
          if (bobotValid_mfep()) {
            actionButton("hitung_mfep", "Hitung MFEP", class = "btn btn-success")
          } else {
            actionButton("hitung_mfep_disabled", "Hitung MFEP (Bobot Belum Valid)", class = "btn btn-success", disabled = TRUE)
          },
          actionButton("reset_mfep", "Reset Form MFEP", class = "btn btn-danger")
      )
    }
  })
  
  ### RESET MFEP
  observeEvent(input$reset_mfep, {
    alt <- values_mfep$alt_count
    kri <- values_mfep$kri_count
    
    if (!is.null(kri)) {
      for (j in 1:kri) updateNumericInput(session, paste0("bobot_1_", j), value = 0)
    }
    if (!is.null(alt) && !is.null(kri)) {
      for (i in 1:alt) {
        updateTextInput(session, paste0("alt_name_mfep_", i), value = paste0("A",i)) # Reset nama alternatif juga
        for (j in 1:kri) updateNumericInput(session, paste0("skor_", i, "_", j), value = 1)
      }
    }
    
    # Tidak mereset input$alternatif_mfep dan input$kriteria_mfep agar pengguna tidak perlu input ulang jumlah
    # Jika ingin reset total, uncomment baris di bawah dan set nilai default untuk alternatif/kriteria
    # updateNumericInput(session, "alternatif_mfep", value = 3)
    # updateNumericInput(session, "kriteria_mfep", value = 3)
    
    values_mfep$form_generated <- FALSE # Ini akan menyembunyikan form dan tombol reset/hitung
    values_mfep$hasil <- NULL
    
    output$inputTables_mfep <- renderUI({})
    output$hasil_mfep <- renderDT({})
    output$hasilPlot_mfep <- renderPlotly({})
    
    showNotification("Form MFEP berhasil di-reset. Klik 'Buat Form Penilaian' untuk memulai lagi.", type = "message")
  })
  
  ### Hitung MFEP
  observeEvent(input$hitung_mfep, {
    # Pastikan form sudah digenerate sebelum menghitung
    if (is.null(values_mfep$form_generated) || !values_mfep$form_generated) {
      showModal(modalDialog(
        title = "Form Belum Dibuat",
        "Silakan klik 'Buat Form Penilaian' terlebih dahulu sebelum menghitung.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Pastikan bobot valid sebelum menghitung
    if (!bobotValid_mfep()) {
      showModal(modalDialog(
        title = "Bobot Tidak Valid",
        "Total bobot kriteria harus sama dengan 1.0.",
        easyClose = TRUE
      ))
      return()
    }
    
    alt <- values_mfep$alt_count
    kri <- values_mfep$kri_count
    req(alt, kri)
    
    # Validasi input tersedia
    bobot_inputs_valid <- all(sapply(1:kri, function(j) !is.null(input[[paste0("bobot_1_", j)]])))
    skor_inputs_valid <- all(sapply(1:alt, function(i) all(sapply(1:kri, function(j) !is.null(input[[paste0("skor_", i, "_", j)]])))))
    
    if (!bobot_inputs_valid || !skor_inputs_valid) {
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
    
    hasil_calc <- skor %*% bobot
    df <- data.frame(Alternatif = getAltNames_mfep(), Total_MFEP = round(hasil_calc, 3)) %>%
      arrange(desc(Total_MFEP)) %>%
      mutate(Ranking = row_number())
    
    values_mfep$hasil <- df
    
    output$hasil_mfep <- renderDT({
      datatable(df, options = list(pageLength = 10, scrollX=TRUE), rownames = FALSE,
                caption = "Tabel Hasil Perhitungan dan Perankingan MFEP") %>%
        formatStyle('Ranking', target = 'row', backgroundColor = styleEqual(1, '#e6f2ff'))
    })
    
    output$hasilPlot_mfep <- renderPlotly({
      plot_ly(df, x = ~Total_MFEP, y = ~reorder(Alternatif, Total_MFEP), type = 'bar', orientation = 'h',
              marker = list(color = 'rgba(0,123,255,0.7)', line = list(color = 'rgba(0,123,255,1.0)', width = 1))) %>%
        layout(title = 'Visualisasi Hasil MFEP',
               xaxis = list(title = 'Skor Total MFEP'),
               yaxis = list(title = 'Alternatif'),
               margin = list(l = 100))
    })
  })
  
}

# --- Run App ---
shinyApp(ui, server)