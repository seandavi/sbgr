# 1. Upload files

# 1.1 Returns the upload information for the ongoing upload.
upload_info = function (auth_token = NULL, upload_id = NULL) {
  
  if (is.null(upload_id)) stop('upload_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('upload/multipart/', upload_id),
               method = 'GET')
  
  return(status_check(req))
  
}

# 1.2 Gets the signed URL for the upload of the specified part.
# Note that URLs are valid for 60 seconds only and that you should initiate
# upload to the signed URL in this time frame.
upload_info_part = function (auth_token = NULL, 
                             upload_id = NULL, part_number = NULL) {
  
  if (is.null(upload_id) | is.null(part_number))
    stop('upload_id and part_number must be both provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('upload/multipart/', upload_id, '/', part_number),
               method = 'GET')
  
  return(status_check(req))
  
}

# 1.3 Initializes the upload of the specified file.
# This is the first operation performed when you wish to upload a file.
# Operation is initialized by providing file name, project id where you
# wish the file to be uploaded to (if not specified, defaults to user's stash)
# and optionally by providing wanted part size. You may wish to set your
# part size to a low value if you experience problems with uploading large
# file parts, although default value of 5MB should be good enough for most users.
# Limits:
# Maximum number of parts is 10000
# Maximum file size is 5TB
# Maximum part size is 5GB
# Default part size is 5MB
upload_init = function (auth_token = NULL, project_id = NULL,
                        name = NULL, size = NULL, part_size = NULL) {
  
  if (is.null(project_id) | is.null(name))
    stop('project_id and name must be both provided')
  
  body = list('project_id' = project_id, 'name' = name)
  
  if (!is.null(size)) body$'size' = size
  if (!is.null(part_size)) body$'part_size' = part_size
  
  req = sbgapi(auth_token = auth_token,
               path = 'upload/multipart', body = body, method = 'POST')
  
  return(status_check(req))
  
}

token = '58aeb140-1970-0130-6386-001f5b34aa78'
req = upload_init(token,
                  project_id = 'f0eb447f-3511-4b28-9253-eba96191d432',
                  name = 'Sample1_RNASeq_chr20.pe_1.fastq', size = 5242880)

# 1.4 Reports the completion of the part upload.
# The ETag is provided for the correctness check upon completion of the
# whole upload. Value for the ETag is provided by AWS S3 service when
# uploading the file in the ETag header.
upload_complete_part = function (auth_token = NULL, upload_id = NULL,
                                 part_number = NULL, e_tag = NULL) {
  
  if (is.null(upload_id) | is.null(part_number) | is.null(e_tag))
    stop('upload_id, part_number and e_tag must be provided')
  
  body = list('part_number' = as.character(part_number),
              'e_tag' = as.character(e_tag))
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('upload/multipart/', upload_id),
               body = body, method = 'POST')
  
  return(status_check(req))
  
}

token = '58aeb140-1970-0130-6386-001f5b34aa78'
req = upload_complete_part(token,
                           upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT',
                           part_number = '1',
                           e_tag = 'd41d8cd98f00b204e9800998ecf8427e')

# 1.5 Reports the complete file upload.
# If the whole parts are uploaded, and the provided ETags are correct,
# then the file is assembled and made available on the SBG platform.
upload_complete_all = function (auth_token = NULL, upload_id = NULL) {
  
  if (is.null(upload_id)) stop('upload_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('upload/multipart/', upload_id, '/complete'),
               method = 'POST')
  
  return(status_check(req))
  
}

token = '58aeb140-1970-0130-6386-001f5b34aa78'
req = upload_complete_all(token,
                          upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT')

# 1.6 Aborts the upload. All upload records and the file are deleted.
upload_delete = function (auth_token = NULL, upload_id = NULL) {
  
  if (is.null(upload_id)) stop('upload_id must be provided')
  
  req = sbgapi(auth_token = auth_token,
               path = paste0('upload/multipart/', upload_id), method = 'DELETE')
  
  return(status_check(req))
  
}

token = '410b4672ebfc43bab48dd0d18a32fb6f'
req = upload_delete(token, upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT')
