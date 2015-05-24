# 6. Tasks

# 6.1 Returns the list of all the tasks for a project with ID specified
# by the project_id path parameter. This call will return general information
# and status of a task, in case you want to get a details, including the
# inputs, outputs and parameters set for that task, you will have to use task
# details resource referencing the task_id of a task that you want to get info about.
task_list = function (auth_token = NULL, project_id = NULL) {
  
  if (is.null(project_id)) stop('project_id must be both provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/task'), method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = task_list(token, '1c1d06d2-5862-48f6-b595-e0099b20937e')

# 6.3 Returns information about the task with ID specified by the task_id path parameter.
task_details = function (auth_token = NULL,
                         project_id = NULL, task_id = NULL,
                         download.url = FALSE) {

  if (is.null(project_id) | is.null(task_id))
    stop('project_id and task_id must be both provided')

  if (download.url == FALSE) {
    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/task/', task_id),
                 method = 'GET')
  } else {
    req = sbgapi(auth_token = auth_token,
                 path = paste0('project/', project_id, '/task/', task_id),
                 query = list('action' = 'download'), method = 'GET')
  }

  return(status_check(req))

}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req1 = task_details(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
                   task_id = '22237')
req2 = task_details(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
                    task_id = '22237', download.url = TRUE)
