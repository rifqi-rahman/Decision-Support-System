# /coming_soon/coming_soon_ui.R

getComingSoonPageUI <- function(method_name_display) {
  div(class="coming-soon-container",
      tags$h2(paste(method_name_display, "Segera Hadir!")),
      tags$p("Fitur ini sedang dalam pengembangan dan akan tersedia secepatnya."),
      tags$p("Terima kasih atas kesabaran Anda."),
      shiny::icon("tools", class="fa-3x text-muted mb-3"), # Menggunakan Font Awesome
      br(),
      actionButton("back_to_home_coming_soon", "Kembali ke Dashboard", icon=shiny::icon("arrow-left"), class="btn btn-primary btn-lg")
  )
}