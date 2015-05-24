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
