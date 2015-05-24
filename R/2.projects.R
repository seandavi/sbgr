# 2. Projects

# 2.1 Returns the list of all projects you have access to.
project_list = function (auth_token = NULL) {

  req = sbgapi(auth_token = auth_token, path = 'project', method = 'GET')

  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = project_list(token)

# 2.2 Returns the details of the project with ID specified by the project_id path parameter.
project_details = function (auth_token = NULL, project_id = NULL) {
  
  if (is.null(project_id)) stop('project_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id), method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = project_details(token, project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')

# 2.3 Returns a list of all users invited to the project and their privileges. Project ID is specified as path parameter. Call returns ID and username of the user with privileges.
project_members = function (auth_token = NULL, project_id = NULL) {
  
  if (is.null(project_id)) stop('project_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/members'),
               method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = project_members(token, project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')
