# 3. Billing

# 3.1 Returns the list of all billing groups you have access to.
# This is an utility method used only create projects with appropriate
# billing group. Full access to billing data is not available via the SBG
# public API yet.
billing = function (auth_token = NULL) {

  req = sbgapi(auth_token = auth_token, path = 'billing', method = 'GET')

  return(status_check(req))

}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = billing(token)
