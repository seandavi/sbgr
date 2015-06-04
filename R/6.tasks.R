# # 6. Tasks
# 
# # 6.1 Returns the list of all the tasks for a project with ID specified
# # by the project_id path parameter. This call will return general information
# # and status of a task, in case you want to get a details, including the
# # inputs, outputs and parameters set for that task, you will have to use task
# # details resource referencing the task_id of a task that you want to get info about.
# task_list = function (auth_token = NULL, project_id = NULL) {
#   
#   if (is.null(project_id)) stop('project_id must be both provided')
#   
#   req = sbgapi(auth_token = auth_token,
#                path = paste0('project/', project_id, '/task'), method = 'GET')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = task_list(token, '1c1d06d2-5862-48f6-b595-e0099b20937e')
# 
# # 6.2 Runs a task as a part of a project with ID specified by project_id.
# # All the details, including the pipeline ID and runtime parameters,
# # are part of JSON being sent within a body of the request.
# task_run = function (auth_token = NULL,
#                      project_id = NULL, task_details = NULL) {
#   
#   if (is.null(project_id) | is.null(task_details))
#     stop('project_id and task_details must be both provided')
#   
#   body = task_details
#   
#   req = sbgapi(auth_token = auth_token, 
#                path = paste0('project/', project_id, '/task'),
#                body = body, method = 'POST')
#   
#   return(status_check(req))
#   
# }
# 
# token = '58aeb140-1970-0130-6386-001f5b34aa78'
# details = list(
#   'name' = 'Test 2 of C. Elegans VC',
#   'description' = 'Testing Caenorhabditis elegans Exome Variant Calling',
#   'pipeline_id' = '422',
#   'inputs' = list('309485' = 13645,
#                   '317344' = 13646,
#                   '318662' = 13645,
#                   '699018' = 13647),
#   'parameters' = list('393463' = list('read_trimming_qual' = 30,
#                                       'rg_seq_tech' = 'Illumina'),
#                       '677492' = list()))
# 
# req = task_run(token,
#                project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#                task_details = details)
# 
# # 6.3 Returns information about the task with ID specified by the task_id path parameter.
# task_details = function (auth_token = NULL,
#                          project_id = NULL, task_id = NULL,
#                          download.url = FALSE) {
# 
#   if (is.null(project_id) | is.null(task_id))
#     stop('project_id and task_id must be both provided')
# 
#   if (download.url == FALSE) {
#     req = sbgapi(auth_token = auth_token,
#                  path = paste0('project/', project_id, '/task/', task_id),
#                  method = 'GET')
#   } else {
#     req = sbgapi(auth_token = auth_token,
#                  path = paste0('project/', project_id, '/task/', task_id),
#                  query = list('action' = 'download'), method = 'GET')
#   }
# 
#   return(status_check(req))
# 
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req1 = task_details(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#                    task_id = '22237')
# req2 = task_details(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#                     task_id = '22237', download.url = TRUE)
# 
# # 6.4 Performs action specified with 'action' parameter on the task with
# # ID specified by the task_id path parameter. Currently, only supported
# # action is 'abort'.
# task_action = function (auth_token = NULL, project_id = NULL,
#                         task_id = NULL, action = 'abort') {
#   
#   if (is.null(project_id) | is.null(task_id))
#     stop('project_id and task_id must be both provided')
#   
#   req = sbgapi(auth_token = auth_token, 
#                path = paste0('project/', project_id, '/task/', task_id),
#                query = list('action' = action), method = 'POST')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = task_action(token,
#                   project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#                   task_id = '5506a44ae4b04a4ab3ae7250',
#                   action = 'abort')
