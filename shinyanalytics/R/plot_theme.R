# R/plot_theme.R
ggplot2::theme_set(
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 14),
      axis.title       = ggplot2::element_text(face = "bold", size = 12),
      axis.text        = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      strip.text       = ggplot2::element_text(face = "bold", size = 12),
      legend.position  = "none"
    )
)
