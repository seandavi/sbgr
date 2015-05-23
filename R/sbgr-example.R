library('httr')
library('jsonlite')

sbgapi = function (auth_token = NULL, version = '1.1', path,
                   method = c('GET', 'POST', 'PUT', 'DELETE'),
                   query = NULL, data = NULL) {
  
  if (is.null(auth_token)) stop('auth_token must be provided')
  
  headers = c(
    'X-SBG-Auth-Token' = auth_token,
    'Accept' = 'application/json',
    'Content-type' = 'application/json'
  )
  
  base_url = paste0('https://api.sbgenomics.com/', version, '/')
  
  if (method == 'GET') {
    tmp = GET(paste0(base_url, 'project'), add_headers(headers))
  }
  
  if (method == 'POST') {
    
  }
  
  # exception handling ...
  
}

# choose the parameters of the file metadata and return a character vector or write to a file
make_metadata = function (output = c('metafile', 'vector'),
                          file_type = c('text', 'binary', 'fasta', 'csfasta',
                                        'fastq', 'qual', 'xsq', 'sff', 'bam',
                                        'bam_index', 'illumina_export', 'vcf',
                                        'sam', 'bed', 'archive', 'juncs',
                                        'gtf','gff', 'enlis_genome'),
                          qual_scale = c('sanger', 'illumina13', 'illumina15',
                                         'illumina18', 'solexa'),
                          seq_tech = c('454', 'Helicos', 'Illumina', 'Solid',
                                       'IonTorrent'),
                          sample = NULL, library = NULL,
                          platform_unit = NULL, paired_end = NULL) {
  
  
  
}

get_uploader = function () {
  
  
  
}

# Click 'Generate Token' button, copy and paste the generated token string to the R console
get_auth_token = function () {
  
  browseURL('https://igor.sbgenomics.com/account/?current=developer')
  cat("\nEnter generated auth token:")
  auth_token = scan(what = character(), nlines = 1L, quiet = TRUE)
  
  if (nchar(auth_token) != 32L)
    stop('Character length of the auth token must be 32, please check')
  
  return(auth_token)
  
}

# paste this: 410b4672ebfc43bab48dd0d18a32fb6f
# auth_token = get_auth_token()
# auth_token = '410b4672ebfc43bab48dd0d18a32fb6f'
