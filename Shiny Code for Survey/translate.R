# function to retrieve correct phrase

translate <- function(field, language, lk){
  unname(unlist(lk[lk$Field == field,language]))
}
