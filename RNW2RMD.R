#' Convert Rnw to Rmd
#' @inheritParams fs::file_move
#' @export

# inspiration
# https://github.com/dtkaplan/detex/blob/master/R/replace_tex.R
# https://github.com/ajrgodfrey/BrailleR/blob/master/R/Rnw2Rmd.R

RNW2RMD <- function(path, new_path = NULL) {
  if (is.null(new_path)) {
    new_path <- path %>%
      gsub(".Rnw", ".Rmd", .) %>%
      gsub(".tex", ".Rmd", .) %>%
      fs::fs_path()
  }
  x <- readLines(path)
  # deal with latex quotes first
  x <- gsub("``",'"', x)
  x <- gsub("''",'"', x)
  # chunks
  x <- gsub("(<<)(.*)(>>=)", "```{r \\2}", x)
  x <- gsub("^@", "```", x)
  x <- gsub("(\\\\Sexpr\\{)([^\\}]+)(\\})", "`r \\2`", x)
  # sections
  x <- gsub("(\\\\chapter\\{)([^\\}]+)(\\})", "# \\2", x)
  x <- gsub("(\\\\section\\{)([^\\}]+)(\\})", "## \\2", x)
  x <- gsub("(\\\\subsection\\{)([^\\}]+)(\\})", "### \\2", x)
  x <- gsub("(\\\\subsubsection\\{)([^\\}]+)(\\})", "#### \\2", x)
  # references
  x <- gsub("(\\\\citep\\{)([^\\}]+)(\\})", "[@\\2]", x)
  x <- gsub("(\\\\cite\\{)([^\\}]+)(\\})", "@\\2", x)
  x <- gsub("(\\\\ref\\{)([^\\}]+)(\\})", "\\\\@ref(\\2)", x)
  x <- gsub("(\\\\label\\{)([^\\}]+)(\\})", "{#\\2}", x)
  x <- gsub("(\\\\index\\{)([^\\}]+)(\\})(\\{)([^\\}]+)(\\})\\%", "\\\\index{\\2}{\\5}", x)
  # LaTeX
  x <- gsub("\\\\item", "- ", x)
  x <- gsub("(\\\\emph\\{)([^\\}]+)(\\})", "*\\2*", x)
  x <- gsub("(\\\\textit\\{)([^\\}]+)(\\})", "*\\2*", x)
  x <- gsub("(\\\\textbf\\{)([^\\}]+)(\\})", "**\\2**", x)
  x <- gsub("(\\\\href\\{)([^\\}]+)(\\})(\\{)([^\\}]+)(\\})", "[\\5](\\2)", x)
  x <- gsub("(\\\\url\\{)([^\\}]+)(\\})", "(\\2)", x)

  # Derek specific
  x <- gsub("(\\\\footnote\\{)([^\\}]+)(\\})", "^[\\2]", x)
  x <- gsub("(\\\\figref\\{)([^\\}]+)(\\})", "Figure \\\\@ref(\\2)", x)
  x <- gsub("(\\\\figrefp\\{)([^\\}]+)(\\})", "(Figure \\\\@ref(\\2))", x)
  x <- gsub("(\\\\tabref\\{)([^\\}]+)(\\})", "Table \\\\@ref(\\2)", x)
  x <- gsub("(\\\\tabrefp\\{)([^\\}]+)(\\})", "(Table \\\\@ref(\\2))", x)
  x <- gsub("(\\\\modref\\{)([^\\}]+)(\\})", "Module \\\\@ref(\\2)", x)
  x <- gsub("(\\\\sectref\\{)([^\\}]+)(\\})", "Section \\\\@ref(\\2)", x)
  x <- gsub("(\\\\sectrefp\\{)([^\\}]+)(\\})", "(Section \\\\@ref(\\2))", x)
  x <- gsub("(\\\\R\\{)([^\\}]+)(\\})", "`\\2`", x)
  x <- gsub("(\\\\warn\\{)([^\\}]+)(\\})", "::: \\{.tip data-latex=''\\}\n\\2\n:::", x)
  x <- gsub("(\\\\defn\\{)([^\\}]+)(\\})", "::: \\{.defn data-latex=''\\}\n\\2\n:::", x)
  x <- gsub("(\\\\index\\{)([^\\}]+)(\\})", "", x)
  x <- gsub("(\\\\vspace\\{)([^\\}]+)(\\})", "", x)
  x <- gsub("\\\\begin\\{quote\\}", "\n>", x)
  x <- gsub("\\\\end\\{quote\\}", "", x)
  x <- gsub("\\\\begin\\{Enumerate\\}", "\n* ", x)
  x <- gsub("\\\\end\\{Enumerate\\}", "", x)
  x <- gsub("\\\\begin\\{Itemize\\}", "\n1. ", x)
  x <- gsub("\\\\end\\{Itemize\\}", "", x)
  x <- gsub("\\\\minitoc", "", x)
  x <- gsub("chap:", "", x)

  x <- gsub("{\\\\tt ([a-zA-Z0-9. _()=]*)} ", "`\\1` ", x, perl = TRUE)  # need to clean up {`

  writeLines(x, new_path)
}
