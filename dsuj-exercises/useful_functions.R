
use_packages <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[req == FALSE]
  if(length(need) > 0){
    install.packages(need, repos = "https://cloud.r-project.org/")
    lapply(need, require, character.only = TRUE)
  }
}


use_dev_pack <- function(name, path){
  use_packages("devtools")
  
  if(require(name, character.only = TRUE) == FALSE){
    devtools::install_github(path)
  }
}

strip_data <- function(string){
  require(stringr)
  return(str_replace_all(string, "\\s+", ", "))
}


report_par_tbl <- function(metaobject, digits = 2, p_digits = 3, pred_labels){
  fmt <- paste0("%.", digits, "f")
  
  broom::tidy(metaobject, conf.int = T) |>
    mutate(
      p = metafor::fmtp(p.value, 3),
      across(where(is.numeric), \(x) sprintf(fmt = fmt, x)),
      ci = paste0("[", conf.low, ", ", conf.high, "]"),
      predictor = pred_labels
    ) |> 
    dplyr::select(predictor, estimate, ci, statistic, p)
}


