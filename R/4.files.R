# # 4. Files
# 
# # 4.1 Returns the list of all project files for a project with ID specified
# # by the project_id path parameter. If user specifies string "public"
# # as project_id, call returns a list of public files.
# file_list = function (auth_token = NULL, project_id = NULL) {
#   
#   if (is.null(project_id)) stop('project_id must be provided')
#   
#   req = sbgapi(auth_token = auth_token,
#                path = paste0('project/', project_id, '/file'), method = 'GET')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = file_list(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e')
# 
# # 4.2 Returns detailed information about project's file with ID specified by the file_id path parameter.
# file_details = function (auth_token = NULL, project_id = NULL, file_id = NULL) {
#   
#   if (is.null(project_id) | is.null(file_id))
#     stop('project_id and file_id must be both provided')
#   
#   req = sbgapi(auth_token = auth_token,
#                path = paste0('project/', project_id, '/file/', file_id),
#                method = 'GET')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = file_details(token, project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#                    file_id = '530854e2e4b036506b803c7e')
# 
# # 4.3 This call will copy the specified file(s) to the specified project.
# # You can use this call to copy a group of uploaded files to a project
# # with ID specified by the project_id path parameter. In order to perform
# # copy operation you need to provide list of file IDs you wish to copy in
# # the body of the request.
# file_copy = function (auth_token = NULL, project_id = NULL, file_id = NULL) {
#   
#   if (is.null(project_id) | is.null(file_id))
#     stop('project_id and file_id must be both provided')
#   
#   body = list('file_id' = as.character(file_id))
#   
#   req = sbgapi(auth_token = auth_token, 
#                path = paste0('project/', project_id, '/file'),
#                body = body, method = 'POST')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = file_copy(token,
#                 project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#                 file_id = c('5506a44ae4b04a4ab3ae7250',
#                             '5506a44ae4b04a4ab3ae7254',
#                             '5506a44ae4b04a4ab3ae7252'))
# 
# # 4.4 This call will update project's file metadata where file ID is
# # specified by the file_id path parameter. File's metadata is being
# # replaced with the content of a PUT call. You can also use this call
# # to change the filename in case you supply "name" variable in PUT JSON.
# # For more information about file metadata, please check the
# # File Metadata Documentation.
# # https://developer.sbgenomics.com/platform/metadata
# file_meta_update = function (auth_token = NULL,
#                              project_id = NULL, file_id = NULL,
#                              name = NULL,
#                              file_type = c('text', 'binary', 'fasta', 'csfasta',
#                                            'fastq', 'qual', 'xsq', 'sff', 'bam',
#                                            'bam_index', 'illumina_export', 'vcf',
#                                            'sam', 'bed', 'archive', 'juncs',
#                                            'gtf','gff', 'enlis_genome'),
#                              qual_scale = c('sanger', 'illumina13', 'illumina15',
#                                             'illumina18', 'solexa'),
#                              seq_tech = c('454', 'Helicos', 'Illumina', 'Solid',
#                                           'IonTorrent'),
#                              sample = NULL, library = NULL,
#                              platform_unit = NULL, paired_end = NULL) {
#   
#   if (is.null(project_id) | is.null(file_id))
#     stop('project_id and file_id must be both provided')
#   
#   body = list(list('file_type' = file_type,
#                    'qual_scale' = qual_scale,
#                    'seq_tech' = seq_tech))
#   names(body) = 'metadata'
#   
#   if (!is.null(sample)) body$'metadata'$'sample' = as.character(sample)
#   if (!is.null(library)) body$'metadata'$'library' = as.character(library)
#   if (!is.null(platform_unit)) body$'metadata'$'platform_unit' = as.character(platform_unit)
#   if (!is.null(paired_end)) body$'metadata'$'paired_end' = as.character(paired_end)
#   
#   if (!is.null(name)) body = c(list('name' = name), body)
#   
#   req = sbgapi(auth_token = auth_token,
#                path = paste0('project/', project_id, '/file/', file_id),
#                body = body, method = 'PUT')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = file_meta_update(token,
#                        project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#                        file_id = '530854e2e4b036506b803c7e',
#                        name = 'c.elegans_chr2_test.fastq',
#                        file_type = 'fastq', qual_scale = 'illumina13',
#                        seq_tech = 'Illumina')
# 
# # 4.5 Removes a file with ID specified by the file_id path parameter
# # from a project with ID specified by the project_id path parameter.
# file_delete = function (auth_token = NULL,
#                         project_id = NULL, file_id = NULL) {
#   
#   if (is.null(project_id) | is.null(file_id))
#     stop('project_id and file_id must be both provided')
#   
#   req = sbgapi(auth_token = auth_token,
#                path = paste0('project/', project_id, '/file/', file_id),
#                method = 'DELETE')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = file_delete(token,
#                   project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#                   file_id = '530854e2e4b036506b803c7e')
# 
# # 4.6 Returns a direct download URL for a project's file with ID specified
# # by the file_id path parameter. You can use any HTTP client, or library
# # to access or download the content once you get the URL from API request.
# file_download_url = function (auth_token = NULL, 
#                               project_id = NULL, file_id = NULL) {
#   
#   if (is.null(project_id) | is.null(file_id))
#     stop('project_id and file_id must be both provided')
#   
#   req = sbgapi(auth_token = auth_token,
#                path = paste0('project/', project_id,
#                              '/file/', file_id, '/download'),
#                method = 'GET')
#   
#   return(status_check(req))
#   
# }
# 
# token = '410b4672ebfc43bab48dd0d18a32fb6f'
# req = file_download_url(token,
#                         project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#                         file_id = '530854e2e4b036506b803c7e')
