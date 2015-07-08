# 7. Misc

#' Opens browser to copy the auth token
#'
#' Click 'Generate Token' button, copy and paste the generated token
#' string to the R console. The function will return the token string.
#'
#' @return auth token
#'
#' @export misc_get_auth_token
#'
#' @examples
#' \donttest{misc_get_auth_token()}
misc_get_auth_token = function () {
  
  browseURL('https://igor.sbgenomics.com/account/?current=developer#developer')
  cat("\nEnter the generated authentication token:")
  auth_token = scan(what = character(), nlines = 1L, quiet = TRUE)
  
  return(auth_token)
  
}

#' Download SBG uploader and extract to a specified directory
#'
#' Download SBG uploader and extract to a specified directory.
#'
#' @param destdir The directory to extract SBG uploader to.
#' If not present, it will be created automatically.
#'
#' @export misc_get_uploader
#'
#' @examples
#' \donttest{misc_get_uploader('~/sbg-uploader/')}
misc_get_uploader = function (destdir = NULL) {
  
  if (is.null(destdir)) stop('destdir must be provided')
  
  tmpfile = tempfile()
  
  download.file(url = 'https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz',
                method = 'libcurl', destfile = tmpfile)
  
  untar(tarfile = tmpfile, exdir = path.expand(destdir))
  
}

#' Specify the parameters of the file metadata and return a list,
#' JSON string, or write to a file
#'
#' Specify the parameters of the file metadata and return a list,
#' JSON string, or write to a file.
#'
#' For more information about file metadata, please check the
#' File Metadata Documentation:
#' \url{https://developer.sbgenomics.com/platform/metadata}.
#'
#' @param output Output format,
#' could be \code{'list'}, \code{'json'}, or \code{'metafile'}.
#' @param destfile Filename to write to.
#' Must be specified when \code{output = 'metafile'}.
#' @param name File name.
#' @param file_type File type. This metadata parameter is mandatory
#' for each file.
#' @param qual_scale Quality scale encoding. For FASTQ files, you must
#' either specify the quality score encoding sch which contains the
#' FASTQ quality scale detector wrapper. In that case, you can
#' specify the quality score encoding scheme by setting
#' \code{qual_scale} inside the pipeline. For BAM files, this value
#' should always be \code{'sanger'}.
#' @param seq_tech Sequencing technology. The \code{seq_tech} parameter allows you to specify
#' the sequencing technology used. This metadata parameter is only
#' required by some the tools and pipelines; however, it is strongly
#' recommended that you set it whenever possible, unless you are certain
#' that your pipeline will work without it.
#' @param sample Sample ID. You can use the \code{sample} parameter to specify
#' the sample identifier. The value supplied in this field will be written
#' to the read group tag (\code{@@RG:SM}) in SAM/BAM files generated from reads
#' with the specified Sample ID. AddOrReplaceReadGroups will use this
#' parameter as the value for the read group tag in a SAM/BAM file.
#' @param library Library. You can set the library for the read using the
#' \code{library} parameter. The value supplied in this field will be written
#' to the read group tag (\code{@@RG:LB}) in SAM/BAM files generated from
#' reads with the specified Library ID. AddOrReplaceReadGroups will use
#' this parameter as the value for the read group tag in a SAM/BAM file.
#' @param platform_unit Platform unit. You can set the platform unit
#' (e.g. lane for Illumina, or slide for SOLiD) using the \code{platform_unit}
#' parameter. The value supplied in this field will be written to the read
#' group tag (\code{@@RG:PU}) in SAM/BAM files generated from the reads with
#' the specified Platform Unit. AddOrReplaceReadGroups will use this parameter
#' as the value for the read group tag of a SAM/BAM file.
#' @param paired_end Paired end. With paired-end reads, this parameter
#' indicates if the read file is left end (1) or right end (2).
#' For SOLiD CSFASTA files, paired end files 1 and 2 correspond to R3
#' and F3 files, respectively.
#'
#' @return list, JSON string, or a file.
#'
#' @export misc_make_metadata
#' 
#' @references
#' \url{https://developer.sbgenomics.com/platform/metadata}
#'
#' @examples
#' \donttest{misc_make_metadata(output = 'metafile',
#'             destfile = '~/c.elegans_chr2_test.fastq.meta',
#'             name = 'c.elegans_chr2_test.fastq',
#'             file_type = 'fastq', qual_scale = 'illumina13',
#'             seq_tech = 'Illumina')}
misc_make_metadata = function (output = c('list', 'json', 'metafile'),
                               destfile = NULL,
                               name = NULL,
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
  
  body = list(list('file_type' = file_type,
                   'qual_scale' = qual_scale,
                   'seq_tech' = seq_tech))
  names(body) = 'metadata'
  
  if (!is.null(sample)) body$'metadata'$'sample' = as.character(sample)
  if (!is.null(library)) body$'metadata'$'library' = as.character(library)
  if (!is.null(platform_unit)) body$'metadata'$'platform_unit' = as.character(platform_unit)
  if (!is.null(paired_end)) body$'metadata'$'paired_end' = as.character(paired_end)
  
  if (!is.null(name)) body = c(list('name' = name), body)
  
  if (output == 'metafile') {
    if (is.null(destfile)) stop('destfile must be provided')
    body = toJSON(body, auto_unbox = TRUE)
    writeLines(body, con = destfile)
  } else if (output == 'json') {
    body = toJSON(body, auto_unbox = TRUE)
    return(body)
  } else if (output == 'list') {
    return(body)
  }
  
}

#' Upload files using SBG uploader
#'
#' Upload files using SBG uploader.
#'
#' @param auth_token auth token
#' @param uploader The directory where the SBG uploader is located
#' (the directory that contains the bin/ directory).
#' @param file The location of the file to upload.
#' @param project_id The project ID to upload the files to.
#' If you do not supply this, then the uploader will place the
#' incoming files in your "My Files" section.
#' @param proxy Allows you to specify a proxy server through which
#' the uploader should connect. About the details the proxy parameter format,
#' see \url{https://developer.sbgenomics.com/tools/uploader/documentation}.
#'
#' @export misc_upload_cli
#'
#' @references
#' \url{https://developer.sbgenomics.com/tools/uploader/documentation}
#'
#' @examples
#' \donttest{misc_upload_cli(auth_token = 'your token',
#'                           uploader = '~/sbg-uploader/',
#'                           file = '~/test.fastq', project_id = '1234')}
misc_upload_cli = function (auth_token = NULL, uploader = NULL,
                            file = NULL, project_id = NULL,
                            proxy = NULL) {
  
  if (is.null(auth_token)) stop('auth_token must be provided')
  if (is.null(uploader)) stop('SBG uploader location must be provided')
  if (is.null(file)) stop('File location must be provided')
  
  auth_token = paste('-t', auth_token)
  uploader = file.path(paste0(uploader, '/bin/sbg-uploader.sh'))
  file = file.path(file)
  
  if (!is.null(project_id)) project_id = paste('-p', project_id)
  if (!is.null(proxy)) proxy = paste('-x', proxy)
  
  cmd = paste(uploader, auth_token, project_id, proxy, file)
  res = system(command = cmd, intern = TRUE)
  fid = strsplit(res, '\t')[[1]][1]
  return(fid)
  
}
