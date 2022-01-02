#' That's a custom R Markdown html format for a custom template "raport-html".
#'
#' @details
#' It uses by default the `downcute` template from `rmdformats` library, has my
#' custom Google Analytics tag and has some preloaded libraries.
#'
#' The parameters are copied from the `downcute` options from `juba`'s Github.
#'
#' @return An custom R Markdown html output format.
#'
#' @param custom_GA if TRUE, template should look for a custom `GA_header.html` file, instead if using my own with my own GA tag
#' @param use_GA if TRUE includes a Google Analytics header, FALSE by default
#'
#' @param fig_width Default width (in inches) for figures
#' @param fig_height Default width (in inches) for figures
#' @param fig_caption \code{TRUE} to render figures with captions
#' @param lightbox if TRUE, add lightbox effect to content images
#' @param thumbnails if TRUE display content images as thumbnails
#' @param gallery if TRUE and lightbox is TRUE, add a gallery navigation between images in lightbox display
#' @param pandoc_args arguments passed to the pandoc_args argument of rmarkdown \code{\link[rmarkdown]{html_document}}
#' @param md_extensions arguments passed to the md_extensions argument of rmarkdown \code{\link[rmarkdown]{html_document}}
#' @param toc_depth adjust table of contents depth
#' @param embed_fonts if TRUE, use local files for fonts used in the template. This leads to bigger files but ensures that these fonts are available. If FALSE they are downloaded from Google Web Fonts.
#' @param use_bookdown if TRUE, uses \code{\link[bookdown]{html_document2}} instead of \code{\link[rmarkdown]{html_document}}, thus providing numbered sections and cross references
#' @param default_style specify default display style, "light" or "dark"
#' @param downcute_theme document template theme
#' @param mathjax set to NULL to disable Mathjax insertion
#' @param highlight syntax highlighting, forced to NULL as highlighting is done via prism.js
#' @param ... Additional function arguments passed to R Markdown \code{\link[rmarkdown]{html_document}}
#'
#' @import rmdformats
#' @import pkgload
#' @import rlang
#' @import rmarkdown
#' @export
raporthtml <- function(custom_GA = FALSE,
                       use_GA = FALSE,
                       fig_width = 8,
                       fig_height = 5,
                       fig_caption = TRUE,
                       lightbox = FALSE,
                       thumbnails = FALSE,
                       gallery = FALSE,
                       toc_depth = 3,
                       embed_fonts = TRUE,
                       use_bookdown = FALSE,
                       pandoc_args = NULL,
                       md_extensions = NULL,
                       mathjax = "rmdformats",
                       highlight = NULL,
                       default_style = c("light", "dark"),
                       downcute_theme = c("default", "chaos"),
                       ...) {
  header = pkgload::package_file("inst/rmarkdown/templates/raport-html/resources/GA_header.html")
  if (custom_GA) {
    if (file.exists("GA_header.html")) {
      header = "GA_header.html"
    } else{
      rlang::abort("No GA_header.html file found while custom_GA set to TRUE.")
    }
  }


  rmdformats::downcute(pandoc_args = pandoc_args,
                       fig_width = fig_width,
                       fig_height = fig_height,
                       fig_caption = fig_caption,
                       lightbox = lightbox,
                       thumbnails = thumbnails,
                       gallery = gallery,
                       toc_depth = toc_depth,
                       use_bookdown = use_bookdown,
                       md_extensions = md_extensions,
                       mathjax = mathjax,
                       default_style = default_style,
                       downcute_theme = downcute_theme,

                       lightbox = lightbox,
                       embed_fonts = embed_fonts,
                       use_bookdown = use_bookdown,
                       mathjax = "rmdformats",
                       highlight = highlight,
                       default_style = default_style,
                       downcute_theme = downcute_theme,
                       includes = rmarkdown::includes(in_header = header),
                       ...)
}
