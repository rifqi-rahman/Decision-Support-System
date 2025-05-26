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

# --- UI Utama Aplikasi ---# dashboard.R

# ... (library dan source file seperti sebelumnya) ...

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap", rel="stylesheet"),
    # Bootstrap JS Bundle (Popper.js disertakan) - Diperlukan untuk fungsionalitas collapse navbar
    tags$script(src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js", integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p", crossorigin="anonymous"),
    tags$title("Dashboard Sistem Pendukung Keputusan")
  ),
  useShinyjs(), # Jika masih dipakai untuk hal lain
  uiOutput("main_content"),
  
  # JavaScript untuk Dark Mode dan Smooth Scroll
  tags$script(HTML('
  $(document).ready(function(){
    console.log("Document ready. Initializing scripts...");

    // Dark Mode Toggle
    const darkModeToggle = document.getElementById("darkModeToggle");

    if (darkModeToggle) {
      console.log("Dark mode toggle switch found.");

      // Function to apply theme
      function applyTheme(theme) {
        if (theme === "dark") {
          document.body.classList.add("dark-mode");
          darkModeToggle.checked = true;
          console.log("Applied dark theme.");
        } else {
          document.body.classList.remove("dark-mode");
          darkModeToggle.checked = false;
          console.log("Applied light theme.");
        }
      }

      // Load saved theme
      const savedTheme = localStorage.getItem("theme");
      if (savedTheme) {
        console.log("Saved theme found:", savedTheme);
        applyTheme(savedTheme);
      } else {
        // Default to light theme if no preference or based on system pref
        // For simplicity, defaulting to light here
        console.log("No saved theme, defaulting to light.");
        applyTheme("light"); 
      }

      // Event listener for toggle change
      darkModeToggle.addEventListener("change", function() {
        if (this.checked) {
          applyTheme("dark");
          localStorage.setItem("theme", "dark");
        } else {
          applyTheme("light");
          localStorage.setItem("theme", "light");
        }
      });
    } else {
      console.error("Dark mode toggle switch (darkModeToggle) NOT found!");
    }

    // Smooth scrolling untuk link navbar (tetap sama)
    $(".nav-link").on("click", function(event) {
      if (this.hash !== "") {
        var targetHref = $(this).attr("href");
        if(targetHref.startsWith("#")){
           event.preventDefault();
           var hash = this.hash;
           var offsetTop = $(hash).offset() ? $(hash).offset().top : 0; // Cek jika elemen ada
           $("html, body").animate({
             scrollTop: offsetTop - 70 // -70 untuk offset tinggi navbar
           }, 800);
        }
      }
    });
    console.log("Smooth scroll initialized.");
  });
'))
)

# ... (Server logic tetap sama) ...
server <- function(input, output, session) {
  # ... (server logic dari tahap sebelumnya) ...
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
    # Penting: Karena navbar sekarang ada di UI utama (fluidPage),
    # getHomePageUI() tidak perlu lagi membuat navbar.
    # Fungsi UI lainnya juga tidak perlu membuat navbar.
    
    current_ui <- NULL
    if (current_view() == "home") {
      current_ui <- getHomePageUI() # Fungsi dari home/home_ui.R
    } else if (current_view() == "mfep") {
      current_ui <- div(class="mfep-container", getMfepPageUI()) # Fungsi dari algoritma/mfep/mfep_ui.R
    } else if (startsWith(current_view(), "coming_soon")) {
      method_name_raw <- sub("coming_soon_", "", current_view())
      method_name_display <- switch(method_name_raw,
                                    "topsis" = "TOPSIS", "ahp" = "AHP", "saw" = "SAW",
                                    "wp" = "Weighted Product", "fc" = "Forward Chaining",
                                    "cf" = "Certainty Factor", "nb" = "Naive Bayes",
                                    "ds" = "Dempster-Shafer", "fuzzy" = "Logika Fuzzy",
                                    "Metode Ini")
      current_ui <- getComingSoonPageUI(method_name_display) # Fungsi dari coming_soon/coming_soon_ui.R
    }
    # Scroll ke atas halaman setiap kali view berubah
    # shinyjs::runjs("window.scrollTo(0, 0);") # Uncomment jika useShinyjs() aktif dan diperlukan
    return(current_ui)
  })
  
  # Inisialisasi server logic untuk MFEP
  initializeMfepServerLogic(input, output, session)
}

shinyApp(ui, server)