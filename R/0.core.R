library('httr')
library('jsonlite')

# wrapper for http logic
sbgapi = function (auth_token = NULL, version = '1.1', path,
                   method = c('GET', 'POST', 'PUT', 'DELETE'),
                   query = NULL, data = NULL) {
  
  if (is.null(auth_token)) stop('auth_token must be provided')
  
  headers = c(
    'X-SBG-Auth-Token' = auth_token,
    'Accept' = 'application/json',
    'Content-type' = 'application/json'
  )
  
  base_url = paste0('https://api.sbgenomics.com/', version, '/')
  
  if (method == 'GET') {
    req = GET(paste0(base_url, path), add_headers(headers), query = query)
  }
  
  if (method == 'POST') {
    
  }
  
  if (method == 'PUT') {
    
  }
  
  if (method == 'DELETE') {
    
  }
  
  return(req)
  
}

status_check = function (req) {

  if (status_code(req) == '200') {
    return(content(req, 'parsed'))
  } else if (status_code(req) %in% c('401', '403', '404', '503')) {
    msg = content(req, 'parsed')$message
    stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)
  } else {
    stop('Error of unknown type occured')
  }

}
