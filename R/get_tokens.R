#' importFrom magrittr "%>%"
#'
#' Convert comments into tokens
#'
#' This function converts comments into tokens also it excludes the stopwords
#'
#' @param page
#' @param posts
#'
#' @return A dataframe with some variables
#'
#' @export
get_tokens <- function(page, posts) {
  custom_stop_words <- data_frame(word = c("miss"),
                                  lexicon = c("custom")) %>%
    bind_rows(tidytext::stop_words)

  comments <- get_comments(page, posts)

  mutated_comments <- comments %>%
    tibble::as_tibble() %>%
    dplyr::mutate(comments = iconv(.$comments,
                                   from = "UTF-8", to = "Latin1")) %>%
    dplyr::mutate(post_number = as.numeric(factor(id))) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(comment_number = row_number()) %>%
    dplyr::select(comments, id, post_number, comment_number,
                  created_time, type, likes_count,
                  comments_count, shares_count)

  tokens <- mutated_comments %>%
    tidytext::unnest_tokens(word, comments) %>%
    dplyr::anti_join(custom_stop_words, by = "word") %>%
    dplyr::filter(!is.na(word))

  tokens
}
