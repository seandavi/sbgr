#' Rate Limit (V2)
#'
#' Rate Limit
#'
#' The following API call allows you to get your rate limit. This call
#' returns information about your current rate limit. This is the
#' number of API calls you can make in one hour. Note: that making
#' this API call does not count against your rate limit.
#'
#' @param auth_token auth token.
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export rate_limit
#' @examples
#' \donttest{
#' req <- rate_limit(token)
#' }
rate_limit <- function(auth_token = NULL, ...){
    req <- sbgapi(auth_token = auth_token, path = "rate_limit", method = "GET", ...)
    return(status_check(req))
}



