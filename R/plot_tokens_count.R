#' Plot tokens count
#'
#' This function plots the frequency of tokens count
#'
#' @param page
#' @param posts
#'
#' @return A plot
#'
#' @export
plot_tokens_count <- function(page, posts) {

  get_tokens_count(page, posts) %>%
    dplyr::filter(n > 25) %>%
    dplyr::mutate(word = reorder(word, n)) %>%
    ggplot2::ggplot(mapping = aes(word, n)) +
    ggplot2::geom_col() +
    ggplot2::xlab(NULL) +
    ggplot2::coord_flip()

}
