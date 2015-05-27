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

# 2.4 Create new project.
# You can use this call to create a project. All details, including
# project name, description and funding source are specified as part
# of the JSON, sent as the body of the request. This call returns
# details of the project.
project_new = function (auth_token = NULL, name = NULL,
                        description = NULL, billing_group_id = NULL) {
  
  if (is.null(name) | is.null(description) | is.null(billing_group_id))
    stop('name, description, and billing_group_id must be provided')
  
  body = list('name' = name,
              'description' = description,
              'billing_group_id' = billing_group_id)
  
  req = sbgapi(auth_token = auth_token, 
               path = 'project', body = body,
               method = 'POST')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = project_new(token, name = 'Test API project',
                  description = 'My first API project',
                  billing_group_id = '5b6d5e71-dff8-42fc-8583-500d858f1093')

# 2.5 Add a user to the project with appropriate permissions.
# You can use this call to add specific users to a project and set their
# privileges. Note that you need to specify user's SBG platform username
# when adding to the project.
project_member_add = function (auth_token = NULL, project_id = NULL,
                               username = NULL, copy = FALSE, write = FALSE,
                               execute = FALSE, admin = FALSE) {
  
  if (is.null(project_id) | is.null(username))
    stop('project_id and username must be both provided')
  
  body = list('username' = username,
              'permissions' = list(
                'copy' = copy, 'write' = write,
                'execute' = execute, 'admin' = admin))
  
  req = sbgapi(auth_token = auth_token, 
               path = paste0('project/', project_id, '/members'),
               body = body, method = 'POST')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = project_member_add(token,
                         project_id = '88fc89c1-cfcd-46ed-a830-6a2fc110c628',
                         username = 'testuser',
                         write = TRUE)

# 2.6 Set permissions for a user specified as user_id path parameter
# to a project specified as project_id path parameter.
# This call will set project's member privileges.
# Privileges you do not explicitly set to "true" will be automatically
# set to "false". Project ID and user ID are specified in path parameters.
# Note that you must get the user IDs by performing the project_members()
# call and gathering id of the user with a specific permission. 
project_member_update = function (auth_token = NULL,
                                  project_id = NULL, user_id = NULL,
                                  write = FALSE, copy = FALSE,
                                  execute = FALSE, admin = FALSE) {
  
  if (is.null(project_id) | is.null(user_id))
    stop('project_id and user_id must be both provided')
  
  body = list('write' = write,
              'copy' = copy,
              'execute' = execute,
              'admin' = admin)
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/members/', user_id),
               body = body, method = 'PUT')
  
  return(status_check(req))
  
}

token = '58aeb140-1970-0130-6386-001f5b34aa78'
req = project_member_update(token,
                            project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
                            user_id = '08890148-6d9e-4a10-b284-924228d3f99a')

# 2.7 Delete a project. Note that this deletes all files, tasks which belong
# to a project.
# Removes a project with ID specified by the project_id path parameter
# from a project with ID specified by the project_id path parameter.
project_delete = function (auth_token = NULL, project_id = NULL) {
  
  if (is.null(project_id)) stop('project_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id), method = 'DELETE')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = project_delete(token, project_id = '3a21ade8-ef3e-41f8-8ac2-1dc3b434ac77')

# 2.8 Removes a member from a project, specified by project_id parameter.
# Note that user_id parameter is not username, but user ID parameter
# that you can receive from GET members call.
project_member_delete = function (auth_token = NULL,
                                  project_id = NULL, user_id = NULL) {
  
  if (is.null(project_id) | is.null(user_id))
    stop('project_id and user_id must be both provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/members/', user_id),
               method = 'DELETE')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = project_member_delete(token,
                            project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
                            user_id = '08890148-6d9e-4a10-b284-924228d3f99a')
