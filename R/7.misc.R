# # 7. Misc
# 
# # Click 'Generate Token' button, copy and paste the generated token
# # string to the R console
# misc_get_auth_token = function () {
# 
#   browseURL('https://igor.sbgenomics.com/account/?current=developer')
#   cat("\nEnter generated auth token:")
#   auth_token = scan(what = character(), nlines = 1L, quiet = TRUE)
# 
#   return(auth_token)
# 
# }
# 
# # download SBG uploader and uncompress it to a specified directory
# misc_get_uploader = function (destdir = NULL) {
# 
#   if (is.null(destdir)) stop('destdir must be provided')
# 
#   tmpfile = tempfile()
# 
#   download.file(url = 'https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz',
#                 method = 'libcurl', destfile = tmpfile)
# 
#   untar(tarfile = tmpfile, exdir = path.expand(destdir))
# 
# }
# 
# # choose the parameters of the file metadata and return a list, JSON string,
# # or write to a file
# misc_make_metadata = function (output = c('list', 'json', 'metafile'),
#                                file_type = c('text', 'binary', 'fasta', 'csfasta',
#                                              'fastq', 'qual', 'xsq', 'sff', 'bam',
#                                              'bam_index', 'illumina_export', 'vcf',
#                                              'sam', 'bed', 'archive', 'juncs',
#                                              'gtf','gff', 'enlis_genome'),
#                                qual_scale = c('sanger', 'illumina13', 'illumina15',
#                                               'illumina18', 'solexa'),
#                                seq_tech = c('454', 'Helicos', 'Illumina', 'Solid',
#                                             'IonTorrent'),
#                                sample = NULL, library = NULL,
#                                platform_unit = NULL, paired_end = NULL) {
#   
#   
#   
# }
# 
# # upload files using SBG uploader
# misc_upload_cli = function () {
#   
#   
#   
# }
