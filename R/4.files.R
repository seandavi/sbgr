# 4. Files

# 4.1 Returns the list of all project files for a project with ID specified
# by the project_id path parameter. If user specifies string "public"
# as project_id, call returns a list of public files.
file_list = function (auth_token = NULL, project_id = NULL) {
  
  if (is.null(project_id)) stop('project_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/file'), method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = file_list(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e')

# 4.2 Returns detailed information about project's file with ID specified by the file_id path parameter.
file_details = function (auth_token = NULL, project_id = NULL, file_id = NULL) {
  
  if (is.null(project_id) | is.null(file_id))
    stop('project_id and file_id must be both provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/file/', file_id),
               method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = file_details(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
                   file_id = '530854e2e4b036506b803c7e')

# 4.6 Returns a direct download URL for a project's file with ID specified
# by the file_id path parameter. You can use any HTTP client, or library
# to access or download the content once you get the URL from API request.
file_download_url = function (auth_token = NULL, 
                              project_id = NULL, file_id = NULL) {
  
  if (is.null(project_id) | is.null(file_id))
    stop('project_id and file_id must be both provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id,
                             '/file/', file_id, '/download'),
               method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = file_download_url(token,
                        project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
                        file_id = '530854e2e4b036506b803c7e')
