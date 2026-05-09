# Rebuild all articles in inst/articles
#
# This regenerates every .Rmd file in inst/articles into its
# corresponding output formats (HTML + MD by default).
# 
# @import rmarkdown
#
# @export
rebuild_articles <- function() {
  # Resolve the article directory relative to the package root
  article_dir <- file.path("inst", "articles")
  
  if (!dir.exists(article_dir)) {
    stop("Directory inst/articles does not exist.")
  }
  
  # Find all article .Rmd files
  files <- list.files(
    article_dir,
    pattern = "\\.Rmd$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    message("No article .Rmd files found in inst/articles.")
    return(invisible(NULL))
  }
  
  message("Rebuilding ", length(files), " article(s)...")
  
  for (f in files) {
    message("  - Rendering: ", basename(f))
    rmarkdown::render(
      f,
      output_format = "all",
      quiet = FALSE,
      envir = new.env(parent = globalenv())
    )
  }
  
  message("Done.")
  invisible(NULL)
}