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

# 5.5 Add a pipeline to a specified project.
# Use this call to add a pipeline from your other project or a public
# pipeline to a project.
pipeline_add = function (auth_token = NULL, project_id_to = NULL,
                         project_id_from = NULL, pipeline_id = NULL,
                         revision = NULL) {
  
  if (is.null(project_id_to) | is.null(project_id_from) | is.null(pipeline_id))
    stop('project_id_to, project_id_from and pipeline_id must be provided')
  
  body = list('project_id' = project_id_from,
              'pipeline_id' = pipeline_id)
  
  if (!is.null(revision)) body = c(body, 'revision' = as.character(revision))
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('project/', project_id_to, '/pipeline'),
               body = body, method = 'POST')
  
  return(status_check(req))
  
}

token = '58aeb140-1970-0130-6386-001f5b34aa78'
req = pipeline_add(token,
                   project_id_to = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
                   project_id_from = 'f0eb447f-3511-4b28-9253-eba96191d432',
                   pipeline_id = '53452130d79f0049c0c94441')
