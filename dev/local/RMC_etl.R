library(tidyverse)
library(pdftools)
library(zip)

# define the years
years <- c(2020:2022,2024:2026)

# delineate each .zip by its year with an object name
for (i in 1:length(years)) {
  assign(paste0("RMC_", years[i]), value = paste0('C:/Users/Erik/Desktop/Programming/R/Sports/', years[i], '.zip.zip'))
}

# compile all the objects
RMC_zips <- mget(ls(pattern = 'RMC')) |> unlist() |> as.character()

# get the .pdf's from within
filtered_rounds <- lapply(RMC_zips, function(x){
  utils::unzip(
    x,
    exdir = tempdir()
  )
}) |> 
  unlist() |> 
  as.data.frame() |>  
  dplyr::rename(file = 1) |> 
  dplyr::filter(!grepl(file, pattern = 'Cup|Classic|Tourn|Rolling_Hills|ABCD|Del_Lago|Rained_Out|(Championship|CC)')) |> 
  dplyr::pull(file)

clubchampionship_rounds <- lapply(RMC_zips, function(x){
  utils::unzip(
    x,
    exdir = tempdir()
  )
}) |> 
  unlist() |> 
  as.data.frame() |>  
  dplyr::rename(file = 1) |> 
  dplyr::filter(grepl(file, pattern = '(Championship|CC)')) |> 
  dplyr::pull(file)

all_rounds <- c(filtered_rounds, clubchampionship_rounds)

# alternative
# RMC_zips2 <- dir(path = paste0(getwd(), '/../'), pattern = '.zip', full.names = T)
# lapply(RMC_zips2, function(x){utils::unzip(x, list = T)})

# parse the score line of each pdf ----
parse_score_line <- function(page_line) {
  # page_line is a tibble with columns x, text for one y-group --
  # MUST sort by x first; token order in the raw list is not
  # guaranteed to match reading order on multi-column-block pages
  page_line <- dplyr::arrange(page_line, x)
  texts <- page_line$text
  xs    <- page_line$x
  
  is_score_tok <- grepl(x = texts, pattern = "^[Xx]?[0-9]+$")
  
  # exclude ANY parenthetical token (handicap, qualifying score, table
  # header) and any tie-designator token ("T6", "T12", ...) from the name
  name_tokens <- texts[
    !is_score_tok &
      !grepl(x = texts, pattern = "^\\(.*\\)$") &
      !grepl(x = texts, pattern = "^T[0-9]+$") &
      grepl(x = texts, pattern = "[A-Za-z]")
  ]
  
  # handicap: digit or +digit inside parens, nothing else
  hcp_token <- texts[grepl(x = texts, pattern = "^\\([+]?\\d+\\)$")]
  
  score_texts <- texts[is_score_tok]
  score_xs    <- xs[is_score_tok]
  score_vals  <- as.integer(gsub(x = score_texts, pattern = "^[Xx]", replacement = ""))
  score_isx   <- grepl(x = score_texts, pattern = "^[Xx]")
  
  if (length(name_tokens) == 0 || length(score_texts) == 0) {
    return(NULL)
  }
  
  # hole scores are always <=15; subtotals (OUT/IN/TOT/NET) are always >15
  is_hole    <- score_vals <= 15
  hole_texts <- score_texts[is_hole]
  hole_xs    <- score_xs[is_hole]
  hole_isx   <- score_isx[is_hole]
  
  sub_xs   <- score_xs[!is_hole]
  sub_vals <- score_vals[!is_hole]
  
  # a pickup/DQ blanks the subtotal cell, not the individual hole cells --
  # require all 18 holes before trusting the row at all
  if (length(hole_texts) != 18) {
    return(NULL)
  }
  
  # OUT sits between hole 9 and hole 10 on the x-axis; everything else
  # (IN, TOT, NET) sits after hole 18, in x-order
  out_idx  <- which(sub_xs > hole_xs[9] & sub_xs < hole_xs[10])
  rest_idx <- setdiff(seq_along(sub_xs), out_idx)
  rest_ord <- rest_idx[order(sub_xs[rest_idx])]
  
  if (length(out_idx) == 1) {
    OUT <- sub_vals[out_idx]
  } else {
    OUT <- NA_integer_
  }
  
  if (length(rest_ord) >= 1) {
    IN <- sub_vals[rest_ord[1]]
  } else {
    IN <- NA_integer_
  }
  
  if (length(rest_ord) >= 2) {
    TOT <- sub_vals[rest_ord[2]]
  } else {
    TOT <- NA_integer_
  }
  
  if (length(rest_ord) >= 3) {
    NET_total <- sub_vals[rest_ord[3]]
  } else {
    NET_total <- NA_integer_
  }
  
  if (!is.na(NET_total)) {
    score_type <- "net"
  } else {
    score_type <- "gross"
  }
  
  last_tokens  <- name_tokens[grepl(x = name_tokens, pattern = ",")]
  first_tokens <- name_tokens[!grepl(x = name_tokens, pattern = ",")]
  last_clean   <- gsub(x = last_tokens, pattern = ",", replacement = "")
  first_clean  <- first_tokens |> purrr::map_chr(stringr::str_squish) |> paste(collapse = " ")
  
  if (length(last_clean) > 0) {
    last_name <- last_clean[1]
  } else {
    last_name <- ""
  }
  
  name <- paste(first_clean, last_name) |> stringr::str_squish() |> stringr::str_to_title()
  
  if (length(hcp_token) > 0) {
    raw <- gsub(x = hcp_token[1], pattern = "[()]", replacement = "")
    if (grepl(x = raw, pattern = "^\\+")) {
      course_hcp <- as.integer(gsub(x = raw, pattern = '\\+', replacement = ''))
    } else {
      course_hcp <- -as.integer(raw)
    }
  } else {
    course_hcp <- NA_integer_
  }
  
  dplyr::tibble(
    player_name = name,
    course_hcp  = course_hcp,
    hole        = 1:18,
    score       = hole_texts,
    is_x        = hole_isx,
    OUT         = OUT,
    IN          = IN,
    TOT         = TOT,
    NET_total   = NET_total,
    score_type  = score_type
  )
}

# extract hole-by-hole scores ----
extract_element_scores <- function(pdf, element) {
  page <- pdf[[element]] |> dplyr::as_tibble()
  
  # points/differential tables use the same 18-numeric-token shape as a
  # scorecard but aren't hole scores -- exclude by header text
  if (any(grepl(x = page$text[1:15], pattern = "^Points$"))) {
    return(tibble::tibble())
  }
  # if (any(grepl(x = page$text[1:15], pattern = "Points|Leaderboard|Skins", ignore.case = T))) {
  #   return(tibble::tibble())
  # }
  
  page <- page |>
    dplyr::arrange(y) |>
    dplyr::mutate(y_group = cumsum(dplyr::coalesce(y - dplyr::lag(y), 0) > 5))
  
  groups <- page |>
    dplyr::group_by(y_group) |>
    dplyr::group_split()
  
  # a line with no digits at all, but with a "LAST," style token, is an
  # orphaned last name printed on its own line above the score row --
  # stitch it onto the following group before parsing
  is_orphan_last_name <- purrr::map_lgl(groups, function(g) {
    !any(grepl(x = g$text, pattern = "[0-9]")) &
      any(grepl(x = g$text, pattern = ",$"))
  })
  
  merged <- list()
  i <- 1
  while (i <= length(groups)) {
    if (is_orphan_last_name[i] && i < length(groups)) {
      merged[[length(merged) + 1]] <- dplyr::bind_rows(groups[[i]], groups[[i + 1]])
      i <- i + 2
    } else {
      merged[[length(merged) + 1]] <- groups[[i]]
      i <- i + 1
    }
  }
  
  merged |>
    purrr::map_dfr(parse_score_line) |>
    dplyr::mutate(source_element = element, .before = 1)
}

# get gross scores ----
extract_round_scores <- function(pdf) {
  parsed <- purrr::map_dfr(seq_along(pdf), function(element) {
    extract_element_scores(pdf = pdf, element = element)
  })
  
  if (nrow(parsed) == 0) {
    return(tibble::tibble())
  }
  
  # if a player still appears in more than one *valid* element for the
  # same score_type (shouldn't happen post-filter, but cheap insurance
  # against an unseen format), keep first
  parsed |>
    dplyr::distinct(player_name, hole, score_type, .keep_all = TRUE)
}

# qc's to link deduplication, missing gross scores, and last name only players ----
qc <- function(df) attr(df, "qc")

set_qc <- function(df, name, value) {
  existing <- attr(df, "qc")
  if (is.null(existing)) existing <- list()
  existing[[name]] <- value
  attr(df, "qc") <- existing
  df
}

# de-duplicate round dates ----
dedupe_score_duplicates <- function(df) {
  qc_in <- attr(df, "qc")
  
  instances <- df |>
    dplyr::group_by(source_file, source_element, player_name, score_type, date, course_name) |>
    dplyr::summarize(
      score_sig    = paste(score[order(hole)], collapse = ","),
      has_hcp      = !all(is.na(course_hcp)),
      n_tokens     = length(stringr::str_split(player_name[1], "\\s+")[[1]]),
      surname      = tolower(utils::tail(stringr::str_split(player_name[1], "\\s+")[[1]], 1)),
      name_quality = dplyr::if_else(n_tokens >= 2, 1, 0),
      .groups = "drop"
    )
  
  resolve_group <- function(g) {
    if (nrow(g) == 1) {
      g$status <- "unique"
      return(g)
    }
    full <- g |> dplyr::filter(name_quality == 1)
    stub <- g |> dplyr::filter(name_quality == 0)
    full_surnames <- unique(full$surname)
    
    if (nrow(full) == 0) {
      distinct_surnames <- unique(g$surname)
      distinct_files    <- unique(g$source_file)
      distinct_dates    <- unique(g$date)
      distinct_courses  <- unique(g$course_name)
      if (length(distinct_surnames) == 1 && length(distinct_files) >= 2 &&
          length(distinct_dates) == 1 && length(distinct_courses) == 1) {
        best <- g |>
          dplyr::slice_max(order_by = has_hcp, n = 1, with_ties = FALSE) |>
          dplyr::mutate(status = "kept_cross_file_dup")
        dropped <- g |>
          dplyr::anti_join(best, by = c("source_file", "source_element", "player_name")) |>
          dplyr::mutate(status = "merged_cross_file_dup")
        return(dplyr::bind_rows(best, dropped))
      }
      g$status <- "ambiguous_no_full_name"
      return(g)
    }
    if (length(full_surnames) > 1) {
      match_counts <- sapply(stub$surname, function(s) sum(full_surnames == s))
      stub$status <- dplyr::if_else(
        stub$surname %in% full_surnames & match_counts == 1,
        "merge_candidate", "ambiguous_surname_conflict"
      )
      full$status <- "keep_distinct"
      return(dplyr::bind_rows(full, stub))
    }
    match_ok <- stub$surname == full_surnames[1]
    keepable <- dplyr::bind_rows(full, stub[match_ok, ])
    best <- keepable |>
      dplyr::slice_max(order_by = name_quality + has_hcp, n = 1, with_ties = FALSE) |>
      dplyr::mutate(status = "kept")
    dplyr::bind_rows(best, stub[!match_ok, ] |> dplyr::mutate(status = "ambiguous_surname_mismatch"))
  }
  
  resolved <- instances |>
    dplyr::group_by(score_type, score_sig) |>
    dplyr::group_modify(~ resolve_group(.x)) |>
    dplyr::ungroup()
  
  kept <- df |>
    dplyr::semi_join(
      resolved |> dplyr::filter(status %in% c("unique", "kept", "keep_distinct", "kept_cross_file_dup")),
      by = c("source_file", "source_element", "player_name", "score_type")
    )
  
  flagged <- resolved |>
    dplyr::filter(!status %in% c("unique", "kept", "keep_distinct", "kept_cross_file_dup", "merged_cross_file_dup")) |>
    dplyr::select(source_file, source_element, player_name, score_type, status)
  
  auto_resolved <- resolved |>
    dplyr::filter(status %in% c("kept_cross_file_dup", "merged_cross_file_dup")) |>
    dplyr::select(source_file, source_element, player_name, score_type, status)
  
  attr(kept, "qc") <- qc_in
  kept |>
    set_qc("dedupe_flagged", flagged) |>
    set_qc("dedupe_auto_resolved", auto_resolved)
}

# fill gross scores where missing ----
fill_missing_gross_from_net <- function(df) {
  qc_in <- attr(df, "qc")
  
  coverage <- df |>
    dplyr::distinct(date, course_name, source_file, player_name, score_type) |>
    dplyr::group_by(date, course_name, source_file, player_name) |>
    dplyr::summarize(
      has_gross = "gross" %in% score_type,
      has_net   = "net" %in% score_type,
      .groups = "drop"
    )
  
  net_only <- coverage |> dplyr::filter(has_net, !has_gross)
  
  synthesized_gross <- df |>
    dplyr::inner_join(
      net_only |> dplyr::select(date, course_name, source_file, player_name),
      by = c("date", "course_name", "source_file", "player_name")
    ) |>
    dplyr::filter(score_type == "net") |>
    dplyr::mutate(score_type = "gross", NET_total = NA_integer_, is_synthesized_gross = TRUE)
  
  out <- dplyr::bind_rows(
    df |> dplyr::mutate(is_synthesized_gross = FALSE),
    synthesized_gross
  )
  
  attr(out, "qc") <- qc_in
  out |> set_qc("fill_gross_net_only_summary", net_only)
}

# function to replace last-name-only names ----
fill_stub_player_names <- function(df) {
  qc_in <- attr(df, "qc")
  
  instances <- df |>
    dplyr::filter(hole <= 18) |>
    dplyr::group_by(source_file, source_element, player_name, score_type, date, course_name) |>
    dplyr::summarize(
      score_sig    = paste(score[order(hole)][1:18], collapse = ","),
      n_tokens     = length(stringr::str_split(player_name[1], "\\s+")[[1]]),
      surname      = tolower(utils::tail(stringr::str_split(player_name[1], "\\s+")[[1]], 1)),
      name_quality = dplyr::if_else(n_tokens >= 2, 1, 0),
      .groups = "drop"
    )
  
  resolve_name <- function(g) {
    full <- g |> dplyr::filter(name_quality == 1)
    stub <- g |> dplyr::filter(name_quality == 0)
    if (nrow(stub) == 0) {
      return(stub |> dplyr::mutate(new_player_name = character(0), rename_status = character(0)))
    }
    resolve_stub <- function(s_surname) {
      cand_names <- unique(full$player_name[full$surname == s_surname])
      if (length(cand_names) == 1) {
        list(name = cand_names, status = "renamed")
      } else if (length(cand_names) > 1) {
        list(name = NA_character_, status = "ambiguous_multiple_full_names")
      } else {
        list(name = NA_character_, status = "no_matching_full_name")
      }
    }
    res <- lapply(stub$surname, resolve_stub)
    stub$new_player_name <- vapply(res, function(x) x$name, character(1))
    stub$rename_status    <- vapply(res, function(x) x$status, character(1))
    stub
  }
  
  renames <- instances |>
    dplyr::group_by(date, course_name, score_sig) |>
    dplyr::group_modify(~ resolve_name(.x)) |>
    dplyr::ungroup()
  
  applied <- renames |> dplyr::filter(rename_status == "renamed")
  review  <- renames |>
    dplyr::filter(rename_status != "renamed") |>
    dplyr::select(source_file, source_element, player_name, score_type, date, course_name, rename_status)
  
  df_renamed <- df |>
    dplyr::left_join(
      applied |> dplyr::select(source_file, source_element, score_type, player_name, new_player_name),
      by = c("source_file", "source_element", "score_type", "player_name")
    ) |>
    dplyr::mutate(player_name = dplyr::coalesce(new_player_name, player_name)) |>
    dplyr::select(-new_player_name)
  
  attr(df_renamed, "qc") <- qc_in
  df_renamed |> set_qc("fill_stub_review", review)
}

# normalize the rounds ----
normalize_rounds <- function(raw_rounds) {
  raw_rounds |>
    dplyr::mutate(
      player_name = dplyr::case_when(
        grepl(x = player_name, pattern = '\\s\\(') ~ gsub(x = player_name, pattern = '\\s\\(.+?\\)', replacement = ''),
        TRUE ~ player_name
      )
    ) |>
    dplyr::group_by(player_name, date, course_name) |>
    tidyr::fill(c(course_hcp, NET_total), .direction = 'updown') |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # net-table TOT reprints the gross total, not the real net total --
      # NET_total holds the real one; swap it in before pivoting
      TOT = dplyr::case_when(
        score_type == 'net' & !is.na(NET_total) ~ NET_total,
        TRUE ~ TOT
      )
    ) |>
    dplyr::distinct(date, course_name, player_name, course_hcp, hole, score, OUT, IN, TOT, NET_total, score_type) |>
    tidyr::pivot_wider(
      id_cols = c(date, course_name, player_name, course_hcp, hole),
      names_from = score_type,
      values_from = c(score, OUT, IN, TOT),
      names_glue = "{.value}_{score_type}"
    ) |>
    dplyr::rename(gross = score_gross, net = score_net, tot_gross = TOT_gross, tot_net = TOT_net, course_handicap = course_hcp)
}

# verify ----

library(DBI)
con <- golf::get_db_connection()

filtered_rounds[which(filtered_rounds %>% stringr::str_detect(., pattern = '\\d{2}-\\d{2}-\\d{2}[a-z]') == T)]
dupes <- filtered_rounds[which(filtered_rounds %>% stringr::str_detect(., 
                                                                       pattern = '(2020/05-03-20)|(2020/08-09-20)|(2022/05-01-22)|(2022/08-14-22)|(2024/04-07-24)|(2024/07-21-24)|(2024/08-11-24)|(2025/04-06-25)|(2025/08-17-25)|(2025/11-16-25)') == T)]

"%notin%" <- Negate("%in%")

purrr::map(dupes, ~ extract_round_scores(pdf = pdftools::pdf_data(.x)) |> 
             dplyr::pull(player_name) |> unique()) |>
  purrr::set_names(basename(dupes))

# for filtered rounds
filtered_scores <- purrr::map_dfr(filtered_rounds, function(path) {
  pdf <- pdftools::pdf_data(path)
  filename <- basename(path)
  
  m <- stringr::str_match(
    filename,
    "^([0-9]{2}-[0-9]{2}-[0-9]{2})([a-z]|-[0-9]+)?_(.+?)(-[0-9]+)?\\.pdf$"
  )
  
  extract_round_scores(pdf = pdf) |>
    dplyr::mutate(
      date = m[,2] |>
        gsub(pattern = '([0-9]{1,})-([0-9]{1,})-([0-9]{1,})', replacement = '20\\3-\\1-\\2'),
      file_suffix = dplyr::coalesce(m[,3], m[,5]),
      course_name = m[,4] |>
        stringr::str_remove("_Results$|_RD[12]_Club_Championship$|_CC_Rd[12]$|_Club_Championship_Rd[12]$|_Club_Championship$") |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_trim(),
      .before = 1
    ) |>
    dplyr::mutate(course_name = dplyr::case_when(course_name == 'Silvberbell' ~ 'Silverbell',
                                                 grepl(course_name, pattern = 'AZ National') ~ 'Arizona National',
                                                 TRUE ~ course_name)) |> 
    dedupe_score_duplicates() |>
    purrr::keep_at('kept') %>% 
    purrr::map_df(., .f = as.data.frame) |> 
    fill_stub_player_names() %>%
    purrr::map_df(., .f = as.data.frame) |> 
    normalize_rounds()
}) |>
  dplyr::left_join(
    DBI::dbGetQuery(conn = con, statement = "SELECT DISTINCT course_name, hole, par, hole_handicap FROM courses ORDER BY course_name, hole;") |>
      dplyr::mutate(
        course_name = dplyr::case_when(
          grepl(x = course_name, pattern = 'Ventana', ignore.case = T) ~ 'Ventana Canyon-Mountain',
          grepl(x = course_name, pattern = 'Tucson National', ignore.case = T) ~ 'Tucson National',
          TRUE ~ course_name
        )
      ),
    by = c('course_name', 'hole')
  ) |>
  dplyr::relocate(par, .after = hole) |>
  # tidyr::unnest(cols = c(gross:tot_net)) |> 
  dplyr::mutate(
    gross = as.numeric(gross),
    net   = as.numeric(net)
  ) |>
  dplyr::group_by(player_name, date) |>
  dplyr::mutate(
    handicap_stroke = dplyr::case_when(
      is.na(course_handicap) ~ NA_real_,
      between(course_handicap + hole_handicap, -17, 0) ~ -1,
      course_handicap + hole_handicap <= -18 ~ -2,
      course_handicap + hole_handicap > 18 & course_handicap > 0 ~ 1,
      TRUE ~ 0
    ),
    # per-hole net table digits equal gross digits in the source PDFs --
    # net must be computed from handicap_stroke, never trusted as parsed
    net_computed = handicap_stroke + gross,
    net = net_computed,
    OUT_net = dplyr::if_else(all(is.na(net_computed[c(1:9)])), NA_real_, sum(net_computed[c(1:9)], na.rm = T)),
    IN_net  = dplyr::if_else(all(is.na(net_computed[c(10:18)])), NA_real_, sum(net_computed[c(10:18)], na.rm = T)),
    # tot_net falls back to the value already parsed from NET_total
    # (real round total) when gross is missing for the whole round
    tot_net = dplyr::if_else(all(is.na(net_computed[c(1:18)])), dplyr::first(tot_net), sum(net_computed[c(1:18)], na.rm = T))
  ) |>
  dplyr::select(-net_computed) |>
  dplyr::ungroup() |>
  dplyr::relocate(c(hole_handicap, handicap_stroke), .after = net) |>
  dplyr::mutate(
    is_gross_birdie       = dplyr::case_when(gross - par == -1 ~ TRUE, TRUE ~ FALSE),
    is_gross_eagle_better = dplyr::case_when(gross - par < -1 ~ TRUE, TRUE ~ FALSE),
    is_gross_par          = dplyr::case_when(gross - par == 0 ~ TRUE, TRUE ~ FALSE),
    is_gross_bogey        = dplyr::case_when(gross - par == 1 ~ TRUE, TRUE ~ FALSE),
    is_gross_bogey_worse  = dplyr::case_when(gross - par > 1 ~ TRUE, TRUE ~ FALSE),
    is_net_birdie         = dplyr::case_when(net - par == -1 ~ TRUE, TRUE ~ FALSE),
    is_net_eagle_better   = dplyr::case_when(net - par < -1 ~ TRUE, TRUE ~ FALSE),
    is_net_par            = dplyr::case_when(net - par == 0 ~ TRUE, TRUE ~ FALSE),
    is_net_bogey          = dplyr::case_when(net - par == 1 ~ TRUE, TRUE ~ FALSE),
    is_net_bogey_worse    = dplyr::case_when(net - par > 1 ~ TRUE, TRUE ~ FALSE)
  )

dupes_scores <- purrr::map_dfr(dupes, function(path) {
  pdf <- pdftools::pdf_data(path)
  filename <- basename(path)
  
  m <- stringr::str_match(
    filename,
    "^([0-9]{2}-[0-9]{2}-[0-9]{2})([a-z]|-[0-9]+)?_(.+?)(-[0-9]+)?\\.pdf$"
  )
  
  extract_round_scores(pdf = pdf) |>
    dplyr::mutate(
      date = m[,2] |>
        gsub(pattern = '([0-9]{1,})-([0-9]{1,})-([0-9]{1,})', replacement = '20\\3-\\1-\\2'),
      file_suffix = dplyr::coalesce(m[,3], m[,5]),
      course_name = m[,4] |>
        stringr::str_remove("_Results$|_RD[12]_Club_Championship$|_CC_Rd[12]$|_Club_Championship_Rd[12]$|_Club_Championship$") |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_trim(),
      .before = 1
    ) |>
    dplyr::mutate(course_name = dplyr::case_when(
      course_name == 'Silvberbell' ~ 'Silverbell',
      grepl(course_name, pattern = 'AZ National') ~ 'Arizona National',
      TRUE ~ course_name
    )) |>
    normalize_rounds()
}) |>
  dplyr::left_join(
    DBI::dbGetQuery(conn = con, statement = "SELECT DISTINCT course_name, hole, par, hole_handicap FROM courses ORDER BY course_name, hole;") |>
      dplyr::mutate(
        course_name = dplyr::case_when(
          grepl(x = course_name, pattern = 'Ventana', ignore.case = T) ~ 'Ventana Canyon-Mountain',
          grepl(x = course_name, pattern = 'Tucson National', ignore.case = T) ~ 'Tucson National',
          TRUE ~ course_name
        )
      ),
    by = c('course_name', 'hole')
  ) |>
  dplyr::relocate(par, .after = hole) |>
  # tidyr::unnest(cols = c(gross:tot_net)) |> 
  dplyr::mutate(
    gross = as.numeric(gross),
    net   = as.numeric(net)
  ) |>
  dplyr::group_by(player_name, date) |>
  dplyr::mutate(
    handicap_stroke = dplyr::case_when(
      is.na(course_handicap) ~ NA_real_,
      between(course_handicap + hole_handicap, -17, 0) ~ -1,
      course_handicap + hole_handicap <= -18 ~ -2,
      course_handicap + hole_handicap > 18 & course_handicap > 0 ~ 1,
      TRUE ~ 0
    ),
    # per-hole net table digits equal gross digits in the source PDFs --
    # net must be computed from handicap_stroke, never trusted as parsed
    net_computed = handicap_stroke + gross,
    net = net_computed,
    OUT_net = dplyr::if_else(all(is.na(net_computed[c(1:9)])), NA_real_, sum(net_computed[c(1:9)], na.rm = T)),
    IN_net  = dplyr::if_else(all(is.na(net_computed[c(10:18)])), NA_real_, sum(net_computed[c(10:18)], na.rm = T)),
    # tot_net falls back to the value already parsed from NET_total
    # (real round total) when gross is missing for the whole round
    tot_net = dplyr::if_else(all(is.na(net_computed[c(1:18)])), dplyr::first(tot_net), sum(net_computed[c(1:18)], na.rm = T))
  ) |>
  dplyr::select(-net_computed) |>
  dplyr::ungroup() |>
  dplyr::relocate(c(hole_handicap, handicap_stroke), .after = net) |>
  dplyr::mutate(
    is_gross_birdie       = dplyr::case_when(gross - par == -1 ~ TRUE, TRUE ~ FALSE),
    is_gross_eagle_better = dplyr::case_when(gross - par < -1 ~ TRUE, TRUE ~ FALSE),
    is_gross_par          = dplyr::case_when(gross - par == 0 ~ TRUE, TRUE ~ FALSE),
    is_gross_bogey        = dplyr::case_when(gross - par == 1 ~ TRUE, TRUE ~ FALSE),
    is_gross_bogey_worse  = dplyr::case_when(gross - par > 1 ~ TRUE, TRUE ~ FALSE),
    is_net_birdie         = dplyr::case_when(net - par == -1 ~ TRUE, TRUE ~ FALSE),
    is_net_eagle_better   = dplyr::case_when(net - par < -1 ~ TRUE, TRUE ~ FALSE),
    is_net_par            = dplyr::case_when(net - par == 0 ~ TRUE, TRUE ~ FALSE),
    is_net_bogey          = dplyr::case_when(net - par == 1 ~ TRUE, TRUE ~ FALSE),
    is_net_bogey_worse    = dplyr::case_when(net - par > 1 ~ TRUE, TRUE ~ FALSE)
  )

# entire chain for filtered rounds
purrr::map_dfr(filtered_rounds, function(path) {
  pdf <- pdftools::pdf_data(path)
  filename <- basename(path)
  
  m <- stringr::str_match(
    filename,
    "^([0-9]{2}-[0-9]{2}-[0-9]{2})([a-z]|-[0-9]+)?_(.+?)(-[0-9]+)?\\.pdf$"
  )
  
  extract_round_scores(pdf = pdf) |>
    dplyr::mutate(
      date = m[,2] |>
        gsub(pattern = '([0-9]{1,})-([0-9]{1,})-([0-9]{1,})', replacement = '20\\3-\\1-\\2'),
      file_suffix = dplyr::coalesce(m[,3], m[,5]),
      source_file = filename,
      course_name = m[,4] |>
        stringr::str_remove("_Results$|_RD[12]_Club_Championship$|_CC_Rd[12]$|_Club_Championship_Rd[12]$|_Club_Championship$") |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_trim(),
      .before = 1
    ) |>
    dplyr::mutate(course_name = dplyr::case_when(
      course_name == 'Silvberbell' ~ 'Silverbell',
      grepl(course_name, pattern = 'AZ National') ~ 'Arizona National',
      TRUE ~ course_name
    ))
}) |> 
  dedupe_score_duplicates() |>
  purrr::keep_at('kept') %>% 
  purrr::map_df(., .f = as.data.frame) |>
  dplyr::filter(!grepl(player_name, pattern = '(Par)', ignore.case = T)) |> 
  fill_stub_player_names() |> 
  purrr::keep_at('df') %>%
  purrr::map_df(., .f = as.data.frame) |> 
  fill_missing_gross_from_net() |> 
  purrr::keep_at('df') %>%
  purrr::map_df(., .f = as.data.frame) |> 
  normalize_rounds()

# sussing out playername issues
purrr::map_dfr(filtered_rounds, function(path) {
  pdf <- pdftools::pdf_data(path)
  filename <- basename(path)
  
  m <- stringr::str_match(
    filename,
    "^([0-9]{2}-[0-9]{2}-[0-9]{2})([a-z]|-[0-9]+)?_(.+?)(-[0-9]+)?\\.pdf$"
  )
  
  extract_round_scores(pdf = pdf) |>
    dplyr::mutate(
      date = m[,2] |>
        gsub(pattern = '([0-9]{1,})-([0-9]{1,})-([0-9]{1,})', replacement = '20\\3-\\1-\\2'),
      file_suffix = dplyr::coalesce(m[,3], m[,5]),
      source_file = filename,
      course_name = m[,4] |>
        stringr::str_remove("_Results$|_RD[12]_Club_Championship$|_CC_Rd[12]$|_Club_Championship_Rd[12]$|_Club_Championship$") |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_trim(),
      .before = 1
    ) |>
    dplyr::mutate(course_name = dplyr::case_when(
      course_name == 'Silvberbell' ~ 'Silverbell',
      grepl(course_name, pattern = 'AZ National') ~ 'Arizona National',
      TRUE ~ course_name
    ))
}) |> 
  dedupe_score_duplicates() |>
  purrr::keep_at('kept') %>% 
  purrr::map_df(., .f = as.data.frame) |>
  dplyr::filter(!grepl(player_name, pattern = '(Par)', ignore.case = T)) |> 
  fill_stub_player_names() |> 
  purrr::keep_at('review') %>%
  purrr::map_df(., .f = as.data.frame) |> 
  fill_missing_gross_from_net() |> 
  purrr::keep_at('df') %>%
  purrr::map_df(., .f = as.data.frame) |> 
  normalize_rounds()

all_scores |> 
  # dplyr::filter(is.na(par)) |> 
  # dplyr::distinct(course_name)
  # dplyr::arrange(desc(course_handicap)) |> 
  # print(n = 100)
  dplyr::filter(grepl(player_name, pattern = 'Epp', ignore.case = T)) |> 
  dplyr::distinct(player_name)
