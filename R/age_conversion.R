#' Age conversion
#'
#' Calculates age in months based on yy;mm format, commonly used in language acquisition studies
#' @param age Age in yy;mm format
#' @param sep Separator for age. Default is ";"
#' @return Age in months
#' @export
#' @examples
#' monthsAge("02;06")
#' monthsAge("05:03", sep = ":")

monthsAge = function(age, sep = ";"){
  pattern1 = stringr::str_c("\\d", sep)
  pattern2 = stringr::str_c(sep, "\\d+")
  y = stringr::str_extract(age, pattern = pattern1) |>
    stringr::str_remove(sep) |>
    as.numeric()
  m = stringr::str_extract(age, pattern = pattern2) |>
    stringr::str_remove(sep) |>
    as.numeric()

  total = y * 12 + m

  return(total)
}


#' Mean age calculator
#'
#' Calculates mean age based on yy;mm format, commonly used in language acquisition studies. `NA`s are ignored
#' @param age Age in yy;mm format
#' @param sep Separator for age. Default is ";"
#' @return Mean age in yy;mm format using the separator specified in `sep`
#' @export
#' @examples
#' meanAge(age = c("02;06", "03;04", NA))
#' meanAge(age = c("05:03", "04:07"), sep = ":")

meanAge = function(age, sep = ";"){
  age_in_months = monthsAge(age, sep)
  mean_age = mean(age_in_months, na.rm = TRUE)

  ye = mean_age %/% 12
  mo = mean_age %% 12 |> round(0)

  if(mo < 10){
    mo = stringr::str_c("0", mo)
  } else {
    mo = as.character(mo)
  }
  result = stringr::str_c(ye, sep, mo)

  return(result)

}
