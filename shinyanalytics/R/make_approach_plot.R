make_approach_plot <- function(df, view, group_var = NULL, facet_var = NULL) {
  
  # canonical: overall GIR curve
  if (view == "Overall GIR Curve") {
    df2 <- df |> dplyr::filter(!is.na(yds_to_target), !is.na(approach_gir))
    
    p <- ggplot2::ggplot(
      df2,
      ggplot2::aes(x = yds_to_target, y = approach_gir)
    ) +
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::labs(title = 'GIR Curve (GIR Prob.)', x = "Distance (yds)", y = "GIR %") +
      ggplot2::theme(aspect.ratio = NULL,
                     plot.margin = ggplot2::margin(5,5,5,5),
                     axis.text.x = ggplot2::element_text(hjust = 0.5, vjust = 0.5, angle = 270))
    
    return(plotly::ggplotly(p))
  }
  
  # canonical: GIR curve by lie
  if (view == "GIR Curve by Lie") {
    df2 <- df |> dplyr::filter(!is.na(yds_to_target), !is.na(approach_gir))
    
    p <- ggplot2::ggplot(
      df2,
      ggplot2::aes(x = yds_to_target, y = approach_gir)
    ) +
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::facet_wrap(~ lie) +
      ggplot2::labs(x = "Distance (yds)", y = "GIR %", title = 'GIR Curve (GIR Prob.) by Lie') +
      ggplot2::theme(aspect.ratio = NULL,
                     plot.margin = ggplot2::margin(5,5,5,5),
                     axis.text.x = ggplot2::element_text(hjust = 0.5, vjust = 0.5, angle = 270))
    
    return(plotly::ggplotly(p))
  }
  
  # canonical: par-3 GIR curve by tee
  if (view == "Par-3 GIR Curve by Tee") {
    df2 <- df |> dplyr::filter(par == 3)
    
    p <- ggplot2::ggplot(
      df2,
      ggplot2::aes(x = yds_to_target, y = approach_gir)
    ) +
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::facet_wrap(~ tees) +
      ggplot2::labs(x = "Distance (yds)", y = "Par-3 GIR %", title = 'Par-3 GIR Curve\n(GIR Prob.) by Tee') +
      ggplot2::theme(aspect.ratio = NULL,
                     plot.margin = ggplot2::margin(5,5,5,5),
                     axis.text.x = ggplot2::element_text(hjust = 0.5, vjust = 0.5, angle = 270))
    
    return(plotly::ggplotly(p))
  }
  
  # canonical: par-3 GIR curve by course
  if (view == "Par-3 GIR Curve by Course") {
    df2 <- df |> dplyr::filter(par == 3)
    
    p <- ggplot2::ggplot(
      df2,
      ggplot2::aes(x = yds_to_target, y = approach_gir)
    ) +
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::facet_wrap(~ course_name) +
      ggplot2::labs(x = "Distance (yds)", y = "GIR %", title = 'Par-3 GIR Curve\n(GIR Prob.) by Course') +
      ggplot2::theme(aspect.ratio = NULL,
                     plot.margin = ggplot2::margin(5,5,5,5))
    
    return(plotly::ggplotly(p))
  }
  
  # custom GIR analysis
  if (view == "Custom GIR Analysis") {
    
    df2 <- df |> dplyr::filter(!is.na(yds_to_target), !is.na(approach_gir))
    
    # grouping
    if (!is.null(group_var) && group_var != "") {
      aes_map <- ggplot2::aes(
        x = yds_to_target,
        y = approach_gir,
        color = .data[[group_var]]
      )
    } else {
      aes_map <- ggplot2::aes(
        x = yds_to_target,
        y = approach_gir
      )
      group_var <- NULL
    }
    
    # faceting
    if (!is.null(facet_var) && facet_var != "") {
      facet_formula <- stats::as.formula(paste("~", facet_var))
    } else {
      facet_formula <- NULL
    }
    
    # interpretability constraints
    if (!is.null(group_var)) {
      n_groups <- dplyr::n_distinct(df2[[group_var]])
      if (n_groups > 6) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Too many groups to display", size = 6) +
            ggplot2::theme_void()
        ))
      }
    }
    
    if (!is.null(facet_formula)) {
      n_facets <- dplyr::n_distinct(df2[[facet_var]])
      if (n_facets > 12) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Too many facets to display", size = 6) +
            ggplot2::theme_void()
        ))
      }
    }
    
    # build plot
    p <- ggplot2::ggplot(df2, aes_map) +
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::labs(x = "Distance (yds)", y = "GIR %") +
      ggplot2::theme(
        aspect.ratio = NULL,
        plot.margin = ggplot2::margin(5,5,5,5),
        axis.text.x = ggplot2::element_text(hjust = 0.5, vjust = 0.5, angle = 270)
      )
    
    if (!is.null(facet_formula)) {
      p <- p + ggplot2::facet_wrap(facet_formula)
    }
    
    return(plotly::ggplotly(p))
  }
  
  stop("Unknown view: ", view)
}
