# /home/home_ui.R

getHomePageUI <- function() {
  # Fungsi createAlgorithmCard ada di utils.R, yang akan di-source oleh dashboard.R
  # Jika utils.R tidak di-source secara global di dashboard.R, Anda perlu source di sini:
  # source("../utils/utils.R", local = TRUE) # Path relatif jika dashboard.R ada di root
  
  tagList(
    # Bagian Landing Page / Intro DSS
    div(class = "landing-section",
        tags$h1("Unlock Your Best Decisions"),
        tags$p(class="lead-dss", "The Intelligent DSS Dashboard navigating complexity to achieve clarity in every choice."),
        tags$p(class="secondary-dss", "More than just data, this is insight. Our DSS transforms numbers into strategy, empowering you to make smarter, faster, and more confident decisions. From deep analysis to accurate predictions, discover the power behind every option."),
        tags$div(class="landing-image-placeholder", "Future Home of DSS Illustration")
    ),
    # Bagian Judul Kartu Algoritma
    tags$h2("Explore Our Decision Toolkit", class="algorithms-section-title"),
    # Bagian Kartu Algoritma
    div(class = "algorithms-grid",
        createAlgorithmCard("mfep", "MFEP: Precision Evaluation", "Quantify every factor, identify the true champion. Optimal results through multi-criteria insight.", icon_name="calculator", status="active"),
        createAlgorithmCard("topsis", "TOPSIS: Ideal Solution Mapping", "Navigate towards the ideal, steer clear of compromise. Find your optimal path.", icon_name="bullseye"),
        createAlgorithmCard("ahp", "AHP: Hierarchy & Prioritization", "Structure priorities, simplify complex choices with analytical precision.", icon_name="sitemap"),
        createAlgorithmCard("saw", "SAW: Weighted Simplicity", "Simple Additive Weighting for clear, criteria-based ranking.", icon_name="balance-scale"),
        createAlgorithmCard("wp", "Weighted Product: Multiplicative Power", "Effective multi-attribute decision making using product-based weighting.", icon_name="weight-hanging"),
        createAlgorithmCard("fc", "Forward Chaining: Logical Inference", "Rule-based reasoning to deduce expert conclusions.", icon_name="project-diagram"),
        createAlgorithmCard("cf", "Certainty Factor: Confidence Measured", "Quantify belief in expert system conclusions.", icon_name="check-circle"),
        createAlgorithmCard("nb", "Naive Bayes: Probabilistic Insight", "Classify outcomes with the power of Bayesian probability.", icon_name="brain"),
        createAlgorithmCard("ds", "Dempster-Shafer: Evidence Fusion", "Combine diverse evidence for robust belief assessment.", icon_name="puzzle-piece"),
        createAlgorithmCard("fuzzy", "Fuzzy Logic: Embrace Ambiguity", "Navigate uncertainty and imprecise data with flexible logic.", icon_name="wave-square")
    )
  )
}