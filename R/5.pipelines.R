# 5. Pipelines

# 5.1 Returns the list of all public pipelines
pipeline_list_pub = function (auth_token = NULL) {
  
  req = sbgapi(auth_token = auth_token,
               path = 'pipeline/public', method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = pipeline_list_pub(token)

# 5.2 Returns the list of pipelines in users "My Pipelines" section
pipeline_list_my = function (auth_token = NULL) {
  
  req = sbgapi(auth_token = auth_token, path = 'pipeline/my', method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = pipeline_list_my(token)

# 5.3 Returns a list of all the pipelines in project with ID specified by the project_id path parameter.
pipeline_list_project = function (auth_token = NULL, project_id = NULL) {
  
  if (is.null(project_id)) stop('project_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/pipeline'),
               method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = pipeline_list_project(token, project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')

# 5.4 Returns the details of a pipeline (runtime and regular parameters, 
# description etc.) with ID specified by the pipeline_id path parameter
# for a project with ID specified by the project_id path parameter.
# When using the API to run a task, user needs to set input files for all
# input nodes. To facilitate this, some pipeline input nodes may contain
# field "suggested files", that contains files which may be used as default
# input (reference genomes, SNP database, etc.).
pipeline_details = function (auth_token = NULL, 
                             project_id = NULL, pipeline_id = NULL) {
  
  if (is.null(project_id) | is.null(pipeline_id))
    stop('project_id and pipeline_id must be both provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id, '/pipeline/', pipeline_id),
               method = 'GET')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = pipeline_details(token, project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858',
                       pipeline_id = '55606ad4896a5d524656afd0')
