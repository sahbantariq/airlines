#' Compare word frequencies
#'
#' This function compares frequencies of different words used in the comments
#' of two different pages.
#'
#' @param page_one
#' @param page_two
#' @param posts
#'
#' @return A dataframe with some variables
#'
#' @export
compare_frequencies <- function(..., posts) {

  pages <- list(...)

  assertthat::assert_that(length(pages) > 1)

  pages_tokens_count <- lapply(pages, get_tokens_count, posts)

  for (i in 1:length(pages)) {
    pages_tokens_count[i] <- pages_tokens_count[i] %>%
      lapply(dplyr::mutate, page = unlist(pages[i]))
  }

  ifelse(length(pages) = 2,
         dplyr::bind_rows(pages_tokens_count[[1]], pages_tokens_count[[2]]),
         ifelse)
}
