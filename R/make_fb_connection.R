#' Make Facebook Connection
#'
#' This fucntion connects local server with fb and save a file in the server
#' containing connection details.
#'
#' @export
make_fb_connection <- function() {
  # Make connection with facebook
  fb_oauth <- Rfacebook::fbOAuth(app_id="1380496555352781",
                                 app_secret="eb3abc42d1e00536e6f4e37e58fc0b5d",
                                 extended_permissions = TRUE)

  # Saving variable fb_oauth in a file and loading it
  save(fb_oauth, file="fb_oauth")
}
