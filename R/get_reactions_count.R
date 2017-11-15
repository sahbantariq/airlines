#' Token frequency
#'
#' This function calculates the frequency of reactions on each post
#'
#' @param page
#' @param posts
#'
#' @return A dataframe with some variables
#'
#' @export
get_reactions_count <- function(page, posts) {
  load("fb_oauth")

  # Extract posts from the page
  get_posts <- Rfacebook::getPage(page = page, token = fb_oauth, n = posts)

  get_posts %>%
    dplyr::mutate(post_number = row_number()) %>%
    dplyr::mutate(date = format(as.Date(created_time), "%d/%m/%Y")) %>%
    dplyr::select(post_number, from_name, date, type,
                  id, likes_count, comments_count, shares_count)
}
