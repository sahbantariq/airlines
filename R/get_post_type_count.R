#' Post type frequency
#'
#' This function calculates the frequency of the types of post. There are three
#' main types of post, i.e. photos, video and status.
#'
#' @param page
#' @param posts
#'
#' @return A dataframe with some variables
#'
#' @export
get_post_type_count <- function(page, posts) {

  reactions_count <- get_reactions_count(page, posts)

  post_type_count <- reactions_count %>%
    dplyr::count(type, sort = TRUE)

  reactions_count %>%
    dplyr::group_by(type) %>%
    dplyr::summarise_at(
      c("likes_count", "comments_count", "shares_count"), sum) %>%
    dplyr::left_join(post_type_count, by = "type") %>%
    dplyr::select(type, n, likes_count, comments_count, shares_count)
}
