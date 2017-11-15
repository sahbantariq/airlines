#' Post type efficiency
#'
#' This function calculates the efficiency of the types of post. There are three
#' main types of post, i.e. photos, video and status.
#'
#' @param page
#' @param posts
#'
#' @return A dataframe with some variables
#'
#' @export
get_post_type_efficiency <- function(page, posts) {

  get_post_type_count(page, posts) %>%
    dplyr::mutate(likes_per_type = as.integer(likes_count / n),
                  comments_per_type = as.integer(comments_count / n),
                  shares_per_type = as.integer(shares_count / n)) %>%
    dplyr::mutate(type_efficiency = likes_per_type / sum(likes_per_type) +
                    comments_per_type / sum(comments_per_type) +
                    shares_per_type / sum(shares_per_type)) %>%
    dplyr::arrange(type_efficiency) %>%
    dplyr::select(type, n, likes_per_type, comments_per_type,
                  shares_per_type, type_efficiency)
}
