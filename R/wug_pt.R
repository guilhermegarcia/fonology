
#' Word generator for Portuguese
#'
#' Returns IPA phonemic transcription for a nonce word given a specific weight profile
#' @param profile The weight profile of the desired string using Ls or Hs
#' @param n Number of words to be generated
#' @param palatalization Whether t and d should palatalize before i (default is FALSE)
#' @return The IPA transcription of said string
#' @examples
#' wug_pt(profile = "HLL", n = 2)
#' @importFrom magrittr %>%
#' @export

wug_pt = function(profile = "LLL", n = 1, palatalization = F){

  purrr::map(seq_len(n), ~gen_pt(profile = profile, palatalization = palatalization)) %>%
    unlist() %>%
    return()
}


