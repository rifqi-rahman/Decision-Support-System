# /dashboard.R (File Shiny App utama - Orkestrator)

# Memuat library yang dibutuhkan di scope global aplikasi
library(shiny)
library(shinyjs) # Jika Anda memutuskan untuk menggunakannya secara eksplisit
library(dplyr)
library(DT)
library(plotly)

# Memuat komponen UI dan Server dari file terpisah di subdirektori
# source() akan mengeksekusi kode dalam file tersebut seolah-olah ada di sini.
# Fungsi-fungsi yang didefinisikan di dalamnya akan tersedia.
source("utils/utils.R", local = TRUE)
source("home/home_ui.R", local = TRUE)
# source("home/home_server.R", local = TRUE) # Belum ada, tapi siapkan jika perlu

source("algoritma/mfep/mfep_ui.R", local = TRUE)
source("algoritma/mfep/mfep_server.R", local = TRUE)
# Nanti jika ada algoritma lain:
# source("algoritma/topsis/topsis_ui.R", local = TRUE)
# source("algoritma/topsis/topsis_server.R", local = TRUE)

source("coming_soon/coming_soon_ui.R", local = TRUE)

# --- UI Utama Aplikasi ---
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), # Path ke CSS
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA), #NA untuk R
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap", rel="stylesheet"),
    tags$title("Dashboard Sistem Pendukung Keputusan")
  ),
  useShinyjs(), # Inisialisasi shinyjs (jika digunakan)
  uiOutput("main_content") # Container utama yang kontennya akan diubah secara dinamis
)

# --- Server Logic Utama (Orkestrator) ---
server <- function(input, output, session) {
  
  current_view <- reactiveVal("home") # Default view adalah home
  
  # Navigasi dari Home ke modul/halaman lain
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
  
  # Tombol kembali ke Home
  observeEvent(input$back_to_home_mfep, { current_view("home") })
  observeEvent(input$back_to_home_coming_soon, { current_view("home") })
  
  # Render UI dinamis berdasarkan current_view
  output$main_content <- renderUI({
    if (current_view() == "home") {
      getHomePageUI() # Fungsi dari home/home_ui.R
    } else if (current_view() == "mfep") {
      # Untuk MFEP, kita panggil UI-nya. Server logicnya sudah diinisialisasi di bawah.
      div(class="mfep-container", getMfepPageUI()) # Fungsi dari algoritma/mfep/mfep_ui.R
    } else if (startsWith(current_view(), "coming_soon")) {
      method_name_raw <- sub("coming_soon_", "", current_view())
      method_name_display <- switch(method_name_raw,
                                    "topsis" = "TOPSIS", "ahp" = "AHP", "saw" = "SAW",
                                    "wp" = "Weighted Product", "fc" = "Forward Chaining",
                                    "cf" = "Certainty Factor", "nb" = "Naive Bayes",
                                    "ds" = "Dempster-Shafer", "fuzzy" = "Logika Fuzzy",
                                    "Metode Ini") # Default jika tidak cocok
      getComingSoonPageUI(method_name_display) # Fungsi dari coming_soon/coming_soon_ui.R
    }
  })
  
  # Inisialisasi server logic untuk MFEP
  # Fungsi ini didefinisikan di algoritma/mfep/mfep_server.R
  # dan akan menyiapkan semua observer dan reactive yang dibutuhkan oleh MFEP.
  initializeMfepServerLogic(input, output, session)
  
  # Jika ada algoritma lain yang fungsional, inisialisasi server logicnya di sini juga:
  # initializeTopsisServerLogic(input, output, session)
  # ...dan seterusnya...
}

# --- Run App ---
shinyApp(ui, server)