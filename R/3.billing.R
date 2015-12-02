# 3. Billing

#' Returns the list of all billing groups you have access to
#'
#' Returns the list of all billing groups you have access to.
#' This is an utility method used only create projects with
#' appropriate billing group. Full access to billing data is not
#' available via the SBG public API yet.
#'
#' @param auth_token auth token
#' @param ... parameters passed to sbgapi function
#'
#' @return parsed list of the returned json
#'
#' @export billing
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = billing(token)}
billing = function (auth_token = NULL, ...) {

    req = sbgapi(auth_token = auth_token, path = 'billing', method = 'GET', ...)

    return(status_check(req))

}

#' [v2] Get a single billing group
#'
#' This call list all your billing groups without id provided, and
#' will retrieves a single billing group, specified by {id}.
#'
#' Billing periods:The spending will be shown for the current billing
#' period, unless the billing group specified is a free billing group
#' (i.e. pilot funds that you are using the CGC with), in which case
#' all spending on the account will be shown.
#'
#' @param auth_token auth token
#' @param id billing group id
#' @param breakdown This call returns a breakdown of spending
#' per-project for the billing group specified by specified billing
#' group id.
#' @export billing_groups
#' @examples
#' \donttest{
#' ## list all groups, to check id and details
#' billing_groups(token, base_url = url)
#' ## check specific billing group by provide id
#' billing_groups(token, base_url = url, id = "2b255c92-2b54-4fc8-something")
#' ## breakdown specific group
#' billing_groups(token, base_url = url, id = "2b255c92-2b54-4fc8-something", breakdown = TRUE)
#' }
billing_groups <- function(auth_token = NULL, id = NULL, breakdown = FALSE, ...){

    if(is.null(id)){
        req = sbgapi(auth_token = auth_token, path = 'billing/groups', method = 'GET', ...)
    }else{
        if(breakdown){
            req = sbgapi(auth_token = auth_token,
                path = paste0('billing/groups/', id, "/breakdown"), method = 'GET', ...)
        }else{
            req = sbgapi(auth_token = auth_token,
                path = paste0('billing/groups/', id), method = 'GET', ...)
        }
    }

    status_check(req)
    
}

#' List invoices
#'
#' This call returns a list of invoices, with information about each,
#' including whether or not the invoice is pending and the billing
#' period it covers. The call returns information about all available
#' invoices, unless you use the query parameter bg_id to specify the
#' ID of specific invoices.
#'
#' @param auth_token auth token.
#' @param id invoices id. by default is NULL, will list all invoices
#' with ID. if id is provided, This call retrieves information about a
#' selected invoice, including the costs for analysis and storage, and
#' the invoice period.
#' @export billing_invoices
#' @examples
#' \donttest{
#' ## list all invoices
#' billing_invoices(token, base_url = url)
#' ## check single invoice
#' billing_invoices(token, base_url = url, id = "some_id")
#' }
billing_invoices <- function(auth_token = NULL, id = NULL, ...){
    if(is.null(id)){
        req = sbgapi(auth_token = auth_token,
            path = 'billing/invoices', method = 'GET', ...)    
    }else{
        req = sbgapi(auth_token = auth_token,
            path = paste0('billing/invoices/', id), method = 'GET', ...)    
    }

    status_check(req)
}
