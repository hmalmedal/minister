factor_no <- function(x, aa = NULL) {
  if (is.character(x)) {
    x <- factor(x)
  }
  if (!is.factor(x)) {
    return(factor(x))
  }
  l <- levels(x)
  i <- l %in% aa
  l[i] <- stringr::str_replace_all(l[i], "(?<=[Aa])(?=[Aa])", "\u200c")
  o <- stringr::str_order(l, locale = "no")
  factor(x, levels(x)[o])
}
