#' wrapper of http logic for SBG API
#'
#' wrapper of http logic for SBG API
#'
#' Used for advanced users and the core method for higher level API in
#' this package, please refer to the easy api manual and the two
#' vignettes pages for more convenient usage.
#'
#' @param auth_token authenticate token string.
#' @param version API version number, default 1.1.
#' @param path path connected with base_url.
#' @param method one of 'GET', 'POST', 'PUT', 'Delete'
#' @param query Passed to httr package GET/POST call.
#' @param body Passed to httr package GET/POST/PUT/DELETE call.
#' @param base_url defeault is 'https://api.sbgenomics.com/1.1'
#'
#' @return returned request list of httr
#'
#' @references
#' \url{https://docs.sbgenomics.com/display/developerhub/API}
#'
#' @export sbgapi
#' @examples
#' token <- "fake_token"
#' \donttest{
#' ## list projects
#' sbgapi(auth_token = token, path = 'project', method = "GET")
#' }
sbgapi = function (auth_token = NULL, version = '1.1', path,
    method = c('GET', 'POST', 'PUT', 'DELETE'),
    query = NULL, body = list(),
    base_url = paste0("https://api.sbgenomics.com/", version, "/")) {

    if (is.null(auth_token))
        stop('auth_token must be provided')

    method <- match.arg(method)

    headers = c(
        'X-SBG-Auth-Token' = auth_token,
        'Accept' = 'application/json',
        'Content-type' = 'application/json'
    )

    switch(method,
           GET = {
               GET(paste0(base_url, path),
                   add_headers(.headers = headers), query = query)
           },
           POST = {
               stopifnot(is.list(body))
               body_json = toJSON(body, auto_unbox = TRUE)
               POST(paste0(base_url, path),
                    add_headers(.headers = headers), query = query,
                    body = body_json)
           },
           PUT = {
               stopifnot(is.list(body))
               body_json = toJSON(body, auto_unbox = TRUE)
               PUT(paste0(base_url, path),
                   add_headers(.headers = headers), body = body_json)
           },
           DELETE = {
               DELETE(paste0(base_url, path),
                      add_headers(.headers = headers))
           })
}


#' check request status
#'
#' check request status
#'
#' @return request content or the message
#'
#' @keywords internal
status_check = function (req) {

    if (status_code(req) %in% c('200', '201', '204')) {
        res <- content(req, "parsed")
        if(!is.null(res))
            attr(res, "response") <- req
        return(res)
    } else if (status_code(req) %in% c('401', '403', '404', '503')) {

        msg = content(req, 'parsed')$message       
        stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)            
        

    } else {
        msg = content(req, 'parsed')$message
        if(is.null(msg)){
            if(status_code(req) %in% names(.codes)){
                msg <- .codes[[status_code(req)]]                
            }
            if(is.null(msg)){
                stop(paste('Error of unknown type occured', status_code(req)))
            }else{
                stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)       
            }
        }
        
    }

}



## well this code is from specification v2
.codes = list(
    ## 0xxx: Platform maintenance errors
    "0" = "Seven Bridges Genomics platform is currently under maintenance.",

    ## 1xxx: General errors
    "1000"= "Allowed Rate limit exceeded! Check response headers.",

    ## 2xxx: Users
    "2000" = "User service is currently unavailable", 
    "2001" = "Not enough privileges to access requested user info.", 
    "2002" = "Requested user does not exist.", 
    "2003" = "Requested user already exists.", 

    ## "3xxx" = "Projects
    "3000" = "Project service is currently unavailable", 
    "3001" = "Not enough privileges to access requested project/member", 
    "3002" = "Requested project or member does not exist", 
    "3003" = "Requested project/member already exists", 
    "3004" = "Owner's username must not be null or empty string", 
    "3005" = "Member username must not be null or empty string", 
    "3006" = "Project id must not be null or an empty string", 
    "3007" = "Project name must not be null or empty string", 
    "3008" = "Billing group id must not be null or empty string", 
    "3009" = "Project type must not be null or empty string",
    "3010" = "Project type can be either CWL for developer projects or LEGACY for old style projects", 
    "3011" = "Project permissions must not null or an empty value",
    "3012" = "Malformed project id. Expecting owner/project", 

    ## 4xxx" = "Billing"
    "4000" = "Billing service is currently unavailable", 
    "4001" = "Insufficient privileges to access the requested billing group/invoice", 
    "4002" = "Requested billing group/invoice does not exist", 
    "4003" = "Requested billing group/invoice already exist", 
    "4004" = "Billing group id must not be null or an empty string", 
    "4005" = "Billing group id must be a valid UUID", 
    "4006" = "You are not a member of this billing group", 
    "4007" = "Invoice id must not be null or an empty string", 

    ## "5xxx" = "Files
    "5000" = "File service is currently unavailable", 
    "5001" = "Insufficient privileges to access the requested file", 
    "5002" = "Requested file does not exist",
    "5004" = "Requested file already exists", 
    "5005" = "Malformed project query parameter. Expecting ?project=owner/project", 
    "5006" = "Metadata validation failed" ,
    "5007" = "File copy failed", 
    "5008" = "File renaming not allowed", 

    ## "6xxx" = "Apps"
    "6000" = "App service is currently unavailable", 
    "6001" = "Insufficient privileges to access the requested app/revision", 
    "6002" = "Requested app/revision does not exist", 
    "6003" = "Requested app/revision already exists", 
    "6004" = "App name must not be null or an empty string", 
    "6006" = "Project owner must not be null or an empty string", 
    "6007" = "Project must not be null or an empty string", 
    "6008" = "App revision must not be null or an empty string", 
    "6009" = "App revision must be a valid integer",
    "6009" = "Source project must not be null or an empty string", 
    "6010" = "Source app must not be null or an empty string.", 
    "6011" = "Malformed app id. Expecting owner/project/app_name/revision",

    ## "7xxx" = "Tasks
    "7000" = "Task service is currently unavailable.",
    "7001" = "Insufficient privileges to access the requested task.",
    "7002" = "Requested task does not exist",
    "7003" = "Requested task already exists",
    "7004" = "Task id must not be empty or null or an empty string", 
    "7005" = "Task id must be a valid UUID", 
    "7006" = "Invalid task status. Allowed values = [QUEUED, DRAFT, RUNNING, COMPLETED, ABORTED, FAILED]", 
    "7007" = "This action is only available for DRAFT tasks", 
    "7008" = "This action is only available for RUNNING tasks", 
    "7009" = "Invalid task action. Action can be performed only on DRAFT or RUNNING tasks.", 
    "7010" = "Invalid task action. Action can be performed on DRAFT tasks", 
    "7011" = "Invalid task action. Action can be performed on RUNNING tasks", 
    "7012" = "Missing inputs.", 
    "7013" = "Invalid task action.", 
    "7014" = "Action parameter must not be null or an empty string.", 
    "7015" = "App Id must not be null or an empty string.", 

    ## "9xxx" = "General validation errors
    "9000" = "Bad request.", 
    "9001" = "Unauthorized.", 
    "9002" = "Forbidden.", 
    "9003" = "Not Found.", 
    "9004" = "Unexpected error happened.", 
    "9005" = "Service Unavailable.", 
    "9006" = "Method Not Allowed.", 
    "9007" = "Conflict.", 
    "9008" = "Unsupported Media Type.", 
    "9009" = "An Error occurred during the decoding of the request content.")




