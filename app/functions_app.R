cleaning_bairros <- function(x) {
  a <- stringi::stri_trans_general(str = x,
                                   id = "Latin-ASCII")
  b <- str_to_lower(a)
  d <- str_squish(b)
  return(d)
}