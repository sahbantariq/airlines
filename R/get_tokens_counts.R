#' importFrom magrittr "%>%"
#'
#' Token frequency
#'
#' This function calculates the frequency of each token
#'
#' @param page
#' @param posts
#'
#' @return A dataframe with some variables
#'
#' @export
get_tokens_count <- function(page, posts) {

  tokens <- get_tokens(page, posts)

  tokens_count <- tokens %>%
    dplyr::ungroup() %>%
    dplyr::count(word, sort = TRUE)

  tokens_count
}
