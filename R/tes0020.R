#' tes0020 PDF Template
#'
#' @param logo File path to institution logo file. The logo will be printed on
#' the top right corner of the first page and the size will be adjusted to
#' `logo_height`. The default is the TalTech logo.
#' @param institution Information about the institution as the page footnote. You can input a list of
#' values.
#' @param short_title A shorter version of the title to be printed on the
#' top-right corner of every page, except the front page.
#' @param watermark Watermark text to be printed on the page. You can change
#' its color via `watermark_color`.
#' @param libertine T/F. Libertine is a collection of open fonts for western
#' languages. Default is `TRUE`.
#' @param logo_height Height of the logo image. This logo image will be scaled
#' to height and the default height is 4cm.
#' @param watermark_color Color for the watermark text. Default is "gray".
#' @param footer_on_first_page T/F value for whether the institution info footer 
#' will be displayed on the front page.
#' @param toc T/F value for table of contents. See ?rmarkdown::pdf_document
#' for details
#' @param lot T/F value for list of tables.
#' @param lof T/F value for list of figures.
#' @param number_sections T/F value for whether sections should be numbered.
#' See ?rmarkdown::pdf_document for details
#' @param fancy_captions T/F value for whether to apply special formatting to
#' captions.
#' @param latex_engine LaTeX engine. See ?rmarkdown::pdf_document for details
#' @param ... other options to be passed to rmarkdown::pdf_document. See
#' ?rmarkdown::pdf_document for details
#'
#' @importFrom rmarkdown pdf_document pandoc_variable_arg
#' @import knitr
#' 
#' @export
pdf_worksheet <- function(logo = system.file("rmarkdown/templates/tes0020/resources/taltech.pdf",
                                         package = "tes0020"),
                          institution = NULL, short_title = NULL,
                          watermark = NULL, libertine = TRUE,
                          logo_height = "4cm", watermark_color = "gray",
                          footer_on_first_page = TRUE,
                          toc = FALSE, lot = FALSE, lof = FALSE,
                          fancy_captions = TRUE, number_sections = TRUE,
                          latex_engine = "xelatex", ...) {
  tes0020_args <- c()
  
  if (!is.null(logo)) {
    tes0020_args <- c(tes0020_args, pandoc_variable_arg("logo", logo), 
                    pandoc_variable_arg("logo_height", logo_height))
  }
  
  if (!is.null(institution)) {
    if (is.list(institution) && length(institution) > 1) {
      institution <- paste(unlist(institution), 
                       collapse = " \\hspace{.025 in} $\\cdot$ \\hspace{.05 in} ")
    }
    tes0020_args <- c(tes0020_args, pandoc_variable_arg("institution", institution))
  }
  
  if (!is.null(short_title)) {
    tes0020_args <- c(tes0020_args, pandoc_variable_arg("short_title", short_title))
  }
  
  if (footer_on_first_page) {
    tes0020_args <- c(tes0020_args, 
                    pandoc_variable_arg("footer_on_first_page", "yes"))
  }
  
  if (libertine) {
    tes0020_args <- c(tes0020_args, pandoc_variable_arg("libertine", "yes"))
  }

  if (!is.null(watermark)) {
    tes0020_args <- c(tes0020_args, pandoc_variable_arg("watermark", watermark),
                    pandoc_variable_arg("watermark_color", watermark_color))
  }
  
  if (lot) tes0020_args <- c(tes0020_args, pandoc_variable_arg("lot", "yes"))
  if (lof) tes0020_args <- c(tes0020_args, pandoc_variable_arg("lof", "yes"))
  
  if (fancy_captions) {
    tes0020_args <- c(tes0020_args, pandoc_variable_arg("fancy_captions", "yes"))
  }
  
  template <- system.file("rmarkdown/templates/tes0020/resources/tes0020.tex",
                          package = "tes0020")
  config <- rmarkdown::pdf_document(
    latex_engine = latex_engine,
    template = template,
    number_sections = number_sections,
    toc = toc,
    ...
  )
  
  pre_pandoc <- config$pandoc$args
  config$pandoc$args <- c(pre_pandoc, tes0020_args)
  
  return(config)
}


