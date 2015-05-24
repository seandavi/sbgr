# 3. Billing

# 3.1 Returns the list of all billing groups you have access to. This is an utility method used only create projects with appropriate billing group. Full access to billing data is not available via the SBG public API yet.
billing = function (auth_token = NULL) {
  
  req = sbgapi(auth_token = auth_token, path = 'billing', method = 'GET')
  
  if (status_code(req) == '401') {
    stop('Status 401: Failed to authenticate user for operation')
  } else if (status_code(req) == '503') {
    stop('Status 503: Service is currently not available')
  } else if (status_code(req) == '200') {
    return(content(req, 'parsed'))
  } else {
    stop('Unknown error occured')
  }
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = billing(token)
