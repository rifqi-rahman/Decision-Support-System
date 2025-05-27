# /home/home_ui.R

getHomePageUI <- function() {
  # Fungsi createAlgorithmCard ada di utils.R, yang akan di-source oleh dashboard.R
  # source("../utils/utils.R", local = TRUE) # Uncomment jika dashboard.R tidak source utils.R secara global
  
  tagList(
    # --------------------------------------------------------------------
    # Navbar
    # --------------------------------------------------------------------
    tags$nav(class = "navbar navbar-expand-lg navbar-light bg-light fixed-top custom-navbar",
             div(class = "container-fluid", # Menggunakan container-fluid untuk lebar penuh
                 tags$a(class = "navbar-brand", href = "#", "DSS"), # Ganti dengan ID page top jika perlu
                 tags$button(class = "navbar-toggler", type = "button",
                             `data-bs-toggle` = "collapse", `data-bs-target` = "#navbarNav",
                             `aria-controls` = "navbarNav", `aria-expanded` = "false", `aria-label` = "Toggle navigation",
                             tags$span(class = "navbar-toggler-icon")
                 ),
                 div(class = "collapse navbar-collapse", id = "navbarNav",
                     tags$ul(class = "navbar-nav ms-auto mb-2 mb-lg-0", # ms-auto untuk align ke kanan
                             tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#home-hero", "Home")),
                             tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#toolkit", "Toolkit")),
                             tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#about", "About")),
                             tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#contact", "Contact")),
                             tags$li(class = "nav-item", # Dark Mode Toggle
                                     div(class = "dark-mode-toggle-container",
                                         tags$label(class = "switch",
                                                    tags$input(type = "checkbox", id = "darkModeToggle"),
                                                    tags$span(class = "slider round")
                                         )
                                     )
                             )
                     )
                 )
             )
    ),
    
    # Placeholder untuk mengatasi konten yang tertutup navbar fixed-top
    div(style = "padding-top: 70px;"), # Sesuaikan tinggi padding ini dengan tinggi navbar Anda
    
    # --------------------------------------------------------------------
    # Hero Section (Landing Page Intro) - Beri ID untuk navigasi
    # --------------------------------------------------------------------
    div(id = "home-hero", class = "landing-section",
        tags$h1("Unlock Your Best Decisions"),
        tags$p(class="lead-dss", "The Intelligent DSS Dashboard navigating complexity to achieve clarity in every choice."),
        tags$p(class="secondary-dss", "More than just data, this is insight. Our DSS transforms numbers into strategy, empowering you to make smarter, faster, and more confident decisions. From deep analysis to accurate predictions, discover the power behind every option."),
        tags$img(src="banner.png", class="img-fluid rounded shadow-sm", alt="About illustration placeholder", style="max-height: 300px; margin: auto; display: block; background-color: #ccc;") # Placeholder gambar
    ),
    
    # --------------------------------------------------------------------
    # Toolkit Section - Beri ID untuk navigasi
    # --------------------------------------------------------------------
    div(id = "toolkit",
        tags$h2("Explore Our Decision Toolkit", class="algorithms-section-title"),
        div(class = "algorithms-grid",
            createAlgorithmCard("mfep", "MFEP: Precision Evaluation", "Quantify every factor, identify the true champion. Optimal results through multi-criteria insight.", icon_name="calculator", status="active"),
            createAlgorithmCard("topsis", "TOPSIS: Ideal Solution Mapping", "Navigate towards the ideal, steer clear of compromise. Find your optimal path.", icon_name="bullseye"),
            # ... (kartu algoritma lainnya tetap sama) ...
            createAlgorithmCard("fuzzy", "Fuzzy Logic: Embrace Ambiguity", "Navigate uncertainty and imprecise data with flexible logic.", icon_name="wave-square")
        )
    ),
    
    # --------------------------------------------------------------------
    # About Section - Beri ID untuk navigasi
    # --------------------------------------------------------------------
    div(id = "about", class = "page-section about-section bg-light-shade", # bg-light-shade untuk variasi latar
        div(class="container text-center", # Menggunakan container agar tidak full-width
            tags$h2("About This Project", class="section-title"),
            tags$p(class="section-subtitle",
                   "Empowering strategic foresight through advanced decision science."),
            div(class="row mt-5",
                div(class="col-md-6 text-start", # text-start untuk align kiri
                    tags$h4("Our Mission"),
                    tags$p("To provide accessible, intuitive, and powerful decision support tools that enable users from various backgrounds to make data-driven choices with confidence. We believe that the right tools can demystify complex decisions and unlock new potentials."),
                    tags$h4("The Technology"),
                    tags$p("Built with R and Shiny, this dashboard leverages robust analytical methods to deliver interactive and insightful decision-making experiences. We are committed to open-source principles and continuous improvement.")
                ),
                div(class="col-md-6",
                    tags$img(src="placeholder_about_image.png", class="img-fluid rounded shadow-sm", alt="About illustration placeholder", style="max-height: 300px; margin: auto; display: block; background-color: #ccc;") # Placeholder gambar
                )
            )
        )
    ),
    
    # --------------------------------------------------------------------
    # Testimonials Section - Beri ID untuk navigasi (Placeholder)
    # --------------------------------------------------------------------
    div(id = "testimonials", class = "page-section testimonials-section",
        div(class="container text-center",
            tags$h2("Loved by Decision Makers", class="section-title"),
            tags$p(class="section-subtitle","See what our users are saying about their experience."),
            div(class="row mt-5 gx-4 gy-4", # gx-4 untuk gutter x, gy-4 untuk gutter y
                # Testimonial Card 1 (Placeholder)
                div(class="col-md-4",
                    div(class="testimonial-card",
                        tags$img(src="placeholder_avatar1.png", alt="User Avatar", class="testimonial-avatar"),
                        tags$p(class="testimonial-text",tags$i("“This DSS has revolutionized how we approach strategic planning. The insights are invaluable!”")),
                        tags$p(class="testimonial-author", "- Alex P., Project Manager")
                    )
                ),
                # Testimonial Card 2 (Placeholder)
                div(class="col-md-4",
                    div(class="testimonial-card",
                        tags$img(src="placeholder_avatar2.png", alt="User Avatar", class="testimonial-avatar"),
                        tags$p(class="testimonial-text",tags$i("“Finally, a tool that simplifies complex decision matrices. Highly recommended for analysts.”")),
                        tags$p(class="testimonial-author", "- Dr. Sarah K., Lead Analyst")
                    )
                ),
                # Testimonial Card 3 (Placeholder)
                div(class="col-md-4",
                    div(class="testimonial-card",
                        tags$img(src="placeholder_avatar3.png", alt="User Avatar", class="testimonial-avatar"),
                        tags$p(class="testimonial-text",tags$i("“The clarity and speed of analysis are outstanding. A game-changer for our team.”")),
                        tags$p(class="testimonial-author", "- Michael B., Operations Head")
                    )
                )
            )
        )
    ),
    
    # --------------------------------------------------------------------
    # Brands Section - Beri ID untuk navigasi (Placeholder)
    # --------------------------------------------------------------------
    div(id = "brands", class = "page-section brands-section bg-light-shade",
        div(class="container text-center",
            tags$h2("Trusted by Innovators", class="section-title"),
            tags$p(class="section-subtitle","Powering decisions for forward-thinking organizations."),
            div(class="brand-logos-grid mt-5",
                # Placeholder untuk 5 logo brand
                lapply(1:5, function(i) {
                  tags$img(src=paste0("placeholder_brand_logo", i, ".png"), alt=paste("Brand Partner", i), class="brand-logo")
                })
            )
        )
    ),
    
    # --------------------------------------------------------------------
    # Contact Section - Beri ID untuk navigasi
    # --------------------------------------------------------------------
    div(id = "contact", class = "page-section contact-section",
        div(class="container text-center",
            tags$h2("Get in Touch", class="section-title"),
            tags$p(class="section-subtitle","Have questions or want to collaborate? We’d love to hear from you."),
            div(class="contact-links mt-5",
                tags$a(href = "https://wa.me/62xxxxxxxxxx", target = "_blank", class = "contact-link-btn btn btn-success btn-lg me-3", # Ganti dengan nomor WhatsApp Anda
                       shiny::icon("whatsapp"), " Chat on WhatsApp"
                ),
                tags$a(href = "https://github.com/yourusername/yourrepository", target = "_blank", class = "contact-link-btn btn btn-dark btn-lg", # Ganti dengan link GitHub Anda
                       shiny::icon("github"), " View on GitHub"
                )
            )
        )
    ),
    
    # --------------------------------------------------------------------
    # Footer
    # --------------------------------------------------------------------
    tags$footer(class = "text-center custom-footer",
                div(class = "container",
                    tags$p(paste0("© ", format(Sys.Date(), "%Y"), " DSS Dashboard Project. All Rights Reserved.")),
                    tags$p(
                      "Crafted by MFEP R Team : Rifqi & Najma ", shiny::icon("flame"), " using R & Shiny."
                      # Anda bisa menambahkan link sosial media di sini jika mau
                    )
                )
    )
  ) # Akhir dari tagList utama
}

