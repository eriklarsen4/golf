# time series plots ----
make_ts_plot <- function(df, metric, label) {

  if (metric == "skill_est") {
    
    df2 <- df |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        skill_est      = round(skill_est, 1),
        skill_ci_lower = round(skill_ci_lower, 1),
        skill_ci_upper = round(skill_ci_upper, 1),
        fir           = ifelse(is.na(fir), 0, fir),
        gir           = ifelse(is.na(gir), 0, gir),
        tot_putts     = ifelse(is.na(tot_putts), 0, tot_putts),
        tot_penalties = ifelse(is.na(tot_penalties), 0, tot_penalties)
      )
    
    main_series <- df2 |>
      dplyr::mutate(
        course_name     = as.character(course_name),
        tot_gross       = as.numeric(tot_gross),
        tot_net         = as.numeric(tot_net),
        handicap_index  = as.numeric(handicap_index),
        fir             = as.numeric(fir),
        gir             = as.numeric(gir),
        tot_putts       = as.numeric(tot_putts),
        tot_penalties   = as.numeric(tot_penalties)
      ) |>
      dplyr::transmute(
        x = date_js,
        y = skill_est,
        meta = purrr::pmap(
          list(date_js, course_name, tot_gross, tot_net, handicap_index,
               fir, gir, tot_putts, tot_penalties),
          function(date_js, course_name, tot_gross, tot_net, handicap_index,
                   fir, gir, tot_putts, tot_penalties) {
            list(
              date_js        = date_js,
              course_name    = course_name,
              tot_gross      = tot_gross,
              tot_net        = tot_net,
              handicap_index = handicap_index,
              fir            = fir,
              gir            = gir,
              tot_putts      = tot_putts,
              tot_penalties  = tot_penalties
            )
          }
        )
      ) |>
      purrr::transpose()
    
    ci_series <- df2 |>
      dplyr::transmute(
        x = date_js,
        y = purrr::map2(skill_ci_lower, skill_ci_upper, ~ c(.x, .y))
      ) |>
      purrr::transpose()
    
    p <- apexcharter::apexchart() |>
      apexcharter::ax_chart(type = "line", height = 300) |>
      apexcharter::ax_series(
        list(
          name = "Skill Estimate",
          type = "line",
          data = main_series
        ),
        list(
          name = "CI",
          type = "rangeArea",
          data = ci_series
        )
      ) |>
      apexcharter::ax_stroke(curve = "smooth") |>
      apexcharter::ax_markers(size = 4) |>
      apexcharter::ax_fill(opacity = c(1, 0.25)) |>
      apexcharter::ax_title(text = "Skill Estimate") |>
      apexcharter::ax_xaxis(type = "datetime", title = list(text = "Date")) |>
      apexcharter::ax_yaxis(
        title = list(text = "Skill Estimate (relative to average handicap index)"),
        decimalsInFloat = 1,
        labels = list(formatter = htmlwidgets::JS("function(val){ return val.toFixed(1); }"))
      ) |>
      apexcharter::ax_tooltip(
        shared = T,
        y = list(
          title = list(formatter = htmlwidgets::JS("function(){ return ''; }")),
          formatter = htmlwidgets::JS("
      function(val, opts) {
        // Return empty string so Apex prints only a dash in the white box.
        return '';
      }
    ")
        ),
        x = list(
          formatter = htmlwidgets::JS("
      function(value, opts) {
        try {
          var dIdx  = opts.dataPointIndex;
          var point = opts.w.config.series[0].data[dIdx];
          var m     = point.meta;

          // --- META BLOCK ---
          var html =
            '<b>Skill Estimate:</b> ' + point.y.toFixed(1) +
            '<br>Date: ' + new Date(m.date_js).toLocaleDateString('en-US') +
            '<br>Course: ' + m.course_name +
            '<br>Gross Score: ' + m.tot_gross +
            '<br>Net Score: ' + m.tot_net +
            '<br>Handicap Index: ' + m.handicap_index +
            '<br>FIR %: ' + m.fir +
            '<br>GIR %: ' + m.gir +
            '<br>Total Putts: ' + m.tot_putts +
            '<br>Total Penalties: ' + m.tot_penalties +
            '<br><br>';

          // --- MANUAL SERIES NAME ROWS ---
          for (var i = 0; i < opts.w.globals.seriesNames.length; i++) {
            html +=
              '<div style=\"display:flex;align-items:center;margin:2px 0;\">' +
              '<span style=\"display:inline-block;width:10px;height:10px;border-radius:50%;background:' +
              opts.w.globals.colors[i] +
              ';margin-right:6px;\"></span>' +
              opts.w.globals.seriesNames[i] +
              '</div>';
          }

          return html;

        } catch (e) {
          return '';
        }
      }
    ")
        )
      )
    
    return(p)
  }
  
  if (!metric %in% names(df)) {
    stop("Column not found: ", metric)
  }

  df2 <- df |>
    dplyr::arrange(date) |>
    dplyr::transmute(
      x = date_js,
      y = .data[[metric]],
      date_js,
      course_name,
      tot_gross,
      tot_net,
      handicap_index,
      fir,
      gir,
      tot_putts,
      tot_penalties
    ) |>
    purrr::transpose()

  p <- apexcharter::apexchart() |>
    apexcharter::ax_series(list(name = label, data = df2)) |>
    apexcharter::ax_chart(type = "line", id = paste0('metric_', metric)) |>
    apexcharter::ax_markers(size = 4) |>
    apexcharter::ax_title(text = label) |>
    apexcharter::ax_xaxis(type = "datetime", title = list(text = "Date")) |>
    apexcharter::ax_yaxis(title = list(text = label)) |>
    apexcharter::ax_tooltip(
      shared = F,
      y = list(
        title = list(formatter = htmlwidgets::JS("function(){return ''}")),
        formatter = htmlwidgets::JS(
          "function(val, opts) {
             const p = opts.w.config.series[0].data[opts.dataPointIndex];
             return (
               '<b>' + opts.w.globals.seriesNames[0] + ':</b> ' + val +
               '<br>Date: ' + new Date(p.date_js).toLocaleDateString('en-US') +
               '<br>Course: ' + p.course_name +
               '<br>Gross Score: ' + p.tot_gross +
               '<br>Net Score: ' + p.tot_net +
               '<br>Handicap Index: ' + p.handicap_index +
               '<br>FIR %: ' + p.fir +
               '<br>GIR %: ' + p.gir +
               '<br>Total Putts: ' + p.tot_putts +
               '<br>Total Penalties: ' + p.tot_penalties
             );
           }"
        )
      )
    )

  return(p)
}