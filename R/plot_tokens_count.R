#' Plot tokens count
#'
#' This function plots the frequency of tokens count
#'
#' @param page
#' @param posts
#' @param minimum_count
#'
#' @return A plot
#'
#' @export
plot_tokens_count <- function(page, posts, minimum_count = 10) {

  get_tokens_count(page, posts) %>%
    dplyr::filter(n > minimum_count) %>%
    dplyr::mutate(word = reorder(word, n)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(word, n)) +
    ggplot2::geom_col() +
    ggplot2::xlab(NULL) +
    ggplot2::coord_flip()

}
