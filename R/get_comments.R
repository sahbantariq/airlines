#' Get comments from facebook

#' This function takes airlines name and number of posts as two arguments.
#' As a result, it provides all the comments in the respective number of posts
#' of respective airlines.
#'
#' @param airlines
#' @param posts
#'
#' @return A dataframe with 12 variables
#'
#' @export
get_comments <- function(airlines, posts) {

  # Make connection with facebook
  fb_oauth <- Rfacebook::fbOAuth(app_id="1380496555352781",
                                 app_secret="eb3abc42d1e00536e6f4e37e58fc0b5d",
                                 extended_permissions = TRUE)

  # Saving variable fb_oauth in a file and loading it
  save(fb_oauth, file="fb_oauth")
  load("fb_oauth")

  # Extract posts from turkish airlines page
  data_turkishairlines <- Rfacebook::getPage(page = airlines,
                             token = fb_oauth, n = posts)

  # Storing the post ids in a new variable
  data_turkishairlines_id <- data_turkishairlines$id

  # Extracting comments and reactions from the extracted posts
  # The comments are extracted using post ids. Therefore, id variable
  # is used.
  all_posts <- lapply(data_turkishairlines_id, Rfacebook::getPost,
                      token=fb_oauth, comments = TRUE, reactions = TRUE)


  # Saving comments for first post in new variable
  full_comments <- setNames(as.data.frame(all_posts[[1]]$comments$message),
                            "comments") %>%
    dplyr::mutate(id = data_turkishairlines_id[1])

  # Exanding full_comments by adding comments of remaining posts.
  for (i in 1:posts-1) {
    comment <- setNames(as.data.frame(all_posts[[i+1]]$comments$message),
                        "comments") %>%
      dplyr::mutate(id = data_turkishairlines_id[i+1])
    full_comments = rbind(full_comments, comment)
  }

  # Assigning the post ids to its comments. The posts ids are repeated when there
  # are more than one comments.
  full_comment_post <- dplyr::left_join(full_comments,
                                        data_turkishairlines, by = "id")

  full_comment_post
}
