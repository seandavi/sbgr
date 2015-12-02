#' List user resources (V2)
#'
#' List user resources
#'
#' This call returns a list of the resources, such as projects,
#' billing groups, and organizations, that are accessible to you.
#'
#' The call will only return a successful response if {username} is
#' replaced with your own username, unless you are an
#' administrator. If you are an administrator, you can replace
#' {username} with the username of any CGC user, to return information
#' on their resources.
#'
#' @param auth_token auth token.
#' @param username NULL or character. 
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export user_list
#' @examples
#' \donttest{
#' req <- user_list(token)
#' req <- user_list(token, username = "test")
#' }
user_list <- function(auth_token = NULL, username = NULL, ...){
    req <- sbgapi(auth_token = auth_token, path = paste0("users/", username), method = "GET", ...)
    return(status_check(req))
}

