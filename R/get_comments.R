#' Get comments from facebook
#'
#' This function takes page name and number of posts as two arguments.
#' As a result, it provides all the comments in the respective number of posts
#' of respective page.
#'
#' @param page
#' @param posts
#'
#' @return A dataframe with 12 variables
#'
#' @export
get_comments <- function(page, posts) {
  load("fb_oauth")

  # Extract posts from the page
  get_posts <- Rfacebook::getPage(page = page,
                             token = fb_oauth, n = posts)

  # Storing the post ids in a new variable
  get_posts_id <- get_posts$id

  # Extracting comments and reactions from the extracted posts
  # The comments are extracted using post ids. Therefore, id variable
  # is used.
  all_posts <- lapply(get_posts_id, Rfacebook::getPost,
                      token=fb_oauth, comments = TRUE, reactions = TRUE)


  # Saving comments for first post in new variable
  full_comments <- setNames(as.data.frame(all_posts[[1]]$comments$message),
                            "comments") %>%
    dplyr::mutate(id = get_posts_id[1])

  # Exanding full_comments by adding comments of remaining posts.
  for (i in 1:posts-1) {
    comment <- setNames(as.data.frame(all_posts[[i+1]]$comments$message),
                        "comments") %>%
      dplyr::mutate(id = get_posts_id[i+1])
    full_comments = rbind(full_comments, comment)
  }

  # Assigning the post ids to its comments. The posts ids are repeated when there
  # are more than one comments.
  full_comment_post <- dplyr::left_join(full_comments,
                                        get_posts, by = "id")

  full_comment_post
}
