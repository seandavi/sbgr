setClassUnion("characterORNULL", c("character", "NULL"))
setClassUnion("numericORNULL", c("numeric", "NULL"))
setClassUnion("listORNULL", c("list", "NULL"))

## define meta data
.file_type <- c('text', 'binary', 'fasta', 'csfasta',
                'fastq', 'qual', 'xsq', 'sff', 'bam',
                'bam_index', 'illumina_export',
                'vcf', 'sam', 'bed', 'archive',
                'juncs', 'gtf','gff',
                'enlis_genome', NA)

.qual_scale <- c('sanger', 'illumina13',
                 'illumina15', 'illumina18',
                 'solexa', NA)

.seq_tech <- c('454', 'Helicos', 'Illumina', 'Solid', 'IonTorrent', NA)
.paired_end <- c(NA, "1", "2")




FileTypeSingleEnum <- setSingleEnum("FileType", .file_type)
QualScaleSingleEnum <- setSingleEnum("QualScale", .qual_scale)
SeqTechSingleEnum <- setSingleEnum("SeqTech", .seq_tech)
PairedEndSingleEnum <- setSingleEnum("PairedEnd", .paired_end)

#' Metadata class
#'
#' Metadata class
#'
#' This funcion will help you create a Metadata object, what it does it to accept 
#' a named list or just pass meta as argument one by one. Then it first match SBG's 
#' build-in meta field which will be shown in the graphic interface on the platform,
#' then save extra meta infomation in extra field, but not visible on the platform yet, 
#' you can view it via the API.  
#' 
#' There are four pre-defined field with pre-defined levels, they are file_type, 
#' qual_scale, seq_tech, and paired_end, to check there levels, you can simply
#' create a empty Metadata object and access the field levels. Please see examples.
#' 
#' Metadata is in progress, we will keep it updated. 
#' 
#' 
#'
#' @export Metadata
#' @exportClass Metadata
#' @importFrom objectProperties setSingleEnum
#' @examples
#' Metadata()
#' ## to check levels
#' Metadata()$file_type 
#' levels(Metadata()$file_type)
Metadata <- setRefClass("Metadata",
                        fields = list(
                            file_type = "FileTypeSingleEnum",
                            qual_scale = "QualScaleSingleEnum",
                            seq_tech = "SeqTechSingleEnum",
                            sample = "characterORNULL",
                            library = "characterORNULL",
                            platform_unit = "characterORNULL",
                            paired_end = "PairedEndSingleEnum",
                            extra = "listORNULL"
                        ),
                        methods = list(
                            initialize = function(file_type = NA,
                                                  qual_scale = NA,
                                                  seq_tech = NA,
                                                  sample = NULL,
                                                  library = NULL,
                                                  platform_unit = NULL,
                                                  paired_end = NA, ...){
                                
                                .l <- list(...)
                                if(length(.l))
                                    extra <<- .l
                                
                                file_type <<- FileTypeSingleEnum(tolower(file_type))
                                qual_scale <<- QualScaleSingleEnum(tolower(qual_scale))
                                seq_tech <<- SeqTechSingleEnum(seq_tech)
                                sample <<- sample
                                library <<- library
                                platform_unit <<- platform_unit
                                paired_end <<- PairedEndSingleEnum(tolower(paired_end))
                                
                            },
                            show = function(){
                                .showFields(.self, "-- Metadata --",
                                            c("file_type",
                                              "qual_scale",
                                              "seq_tech",
                                              "sample",
                                              "library",
                                              "platform_unit",
                                              "paired_end"))
                                .showList(extra)
                            },
                            asList = function(){
                                lst <- .getFields(.self, c("file_type",
                                                           "qual_scale",
                                                           "seq_tech",
                                                           "sample",
                                                           "library",
                                                           "platform_unit",
                                                           "paired_end"))
                                res <- list(c(lst, extra))
                                names(res) <- "metadata"
                                res
                            }
                        ))

setClassUnion("MetadataORNULL", c("Metadata", "NULL"))





#' Class Auth
#'
#' Auth token object
#'
#' Every object coud be requested from this Auth object and any action
#' could start from this object using cascading style. Please check vignette
#' 'easy-api' for more information.
#'
#' @field auth_token [character] your auth token.
#' @field url [character] basic url used for API, by default it's \url{https://api.sbgenomics.com/1.1/}
#'
#' @param auth_token [character] your auth token.
#' @param api [character %in% 'sbg-us', 'cgc'] which api you are
#' using, by default it is sbg us platform.
#' @param url [chracter] a URL for the API, default is \code{NULL},
#' will use \code{api} parameter to switch to the right one.
#' @param version [character] default: 1.1 version used for api.
#'
#' 
#'
#' @export Auth
#' @exportClass Auth
#' @examples
#' a <- Auth("fake_token")
Auth <- setRefClass("Auth", fields = list(auth_token = "character",
                                url = "character"),
                    methods = list(
                        initialize = function(
                            auth_token = NULL,
                            api = c("sbg-us", "cgc"),
                            url = NULL,
                            version = "1.1"){

                            api <- match.arg(api)

                            if(is.null(auth_token)){
                                stop("missing token")
                            }
                            auth_token <<- auth_token
                            
                            stopifnot(is.null(url) | is.character(url))
                            
                            if(is.null(url)){
                                url <<- switch(api,
                                               'sbg-us' = 
                                                   paste0('https://api.sbgenomics.com/', version, '/'),
                                               'cgc' = 
                                                   paste0('https://cgc-api.sbgenomics.com/', version, '/')
                                               )
                            }else{
                                url <<- url                                    
                            }

                        },
                        project_list = function(){
                            res <- sbgr::project_list(auth_token)
                            .asProjectList(res[[1]])
                        },
                        project_new = function(name = NULL,
                            description = NULL,
                            billing_group_id = NULL){
                            res <- sbgr::project_new(auth_token, name = name,
                                                     description = description,
                                                     billing_group_id = billing_group_id)

                            .asProject(res)
                        },
                        project_delete = function(id = NULL,
                            name = NULL){
                            if(!is.null(id)){
                                sbgr::project_delete(auth_token,
                                                     project_id = id)
                            }else{
                                ## match name
                                message("Matching by names")
                                p <- .self$project(name = name)
                                message("Following projects will be deleted")
                                print(p)
                                sbgr::project_delete(auth_token,
                                                     project_id = p$id)
                            }
                        },
                        project = function(
                            name = NULL,
                            id = NULL,
                            index = NULL,
                            ignore.case = TRUE,
                            exact = TRUE){
                            'find project'
                            
                            pl <- .self$project_list()
                            res <- m.match(pl, id = id, name = name, exact = exact,
                                           ignore.case = ignore.case)
                            if(is(res, "Project")){
                                res$auth <- .self
                            }else if(is.list(res) &&
                                     all(sapply(res, is, "Project"))){
                                res <- lapply(res, function(x){
                                    x$auth <- .self
                                    x
                                })
                            }
                            return(res)
                        },
                        pipeline = function(repos = c("public", "my", "project"), 
                                            project_name = NULL,
                                            project_id = NULL, 
                                            pipeline_name = NULL,
                                            pipeline_id = NULL,
                                            ignore.case = TRUE,
                                            exact = FALSE,
                                            detail = TRUE){
                            repos <- match.arg(repos)
                            
                            if(is.null(project_name) &
                                   is.null(project_id)){
                            if(repos == "public"){                                
                                res <- pipeline_list_pub()
                            }else if(repos == "my"){
                                res <- pipeline_list_my()
                            }else{
                                stop("Please provide project_name or project_id")  
                            }
                            }else{
                                repos <- "project"
                                res <- pipeline_list_project(project_name, 
                                                             project_id)
                            }
                            
                            if(!is.null(pipeline_name) | !is.null(pipeline_id)){
                                res <- m.match(res, 
                                               id = pipeline_id,
                                               name = pipeline_name, 
                                               ignore.case = ignore.case,
                                               exact = exact)
                            }
                    
                            if(is(res, "Pipeline")){
                                res$repos <- repos
                                res$auth <- .self
                    
                            }else if(all(sapply(res, is, "Pipeline"))){
                                res <- lapply(res, function(x){
                                    x$repos <- repos
                                    x$auth <- .self
                                    x
                                })   
                            }
                            
                            
                            return(res)
                        },
                        pipeline_list_pub = function(){
                            res <- sbgr::pipeline_list_pub(auth_token)
                            res <- .asPipelineList(res[[1]])
                            lapply(res, function(x) {
                                ## x$set_auth(auth_token)
                                #x$project_id <- id
                                x})
                        },
                        pipeline_list_my = function(){
                            res <- sbgr::pipeline_list_my(auth_token)
                            res <- .asPipelineList(res[[1]])
                            lapply(res, function(x) {
                                ## x$set_auth(auth_token)
                                #x$project_id <- id
                                x})
                        },
                        pipeline_list_project = function(name = NULL,
                            project_id = NULL,
                            id = project_id){

                            if(is.null(id)){
                                id <- project(name = name)$id
                            }
                            p <- project(name = name, id = id)
                            res <- p$pipeline()
                            ## add id
                            res
                        },
                        pipeline_add = function(project_name_to,
                            project_name_from = NULL,
                            pipeline_name = NULL,
                            project_id_from = NULL,
                            project_id_to = NULL,
                            pipeline_id = NULL,
                            revision = NULL,
                            exact = TRUE){
                            
                            if(is.null(project_id_from)){
                                if(is.null(project_name_from)){
                                    warning("omit project name or id, assume it's public")
                                }else{
                                    project_id_from <- project(project_name_from)$id
                                }
                            }

                            if(is.null(project_id_to)){
                                if(is.null(project_name_to)){
                                    stop("project(from) id or name must be provided")
                                }else{

                                    project_id_to <- project(project_name_to)$id
                                }
                            }
                            
                            if(is.null(pipeline_id)){
                                if(is.null(pipeline_name)){
                                    stop("Pipeline id or name must be provided")
                                }else{
                                    if(is.null(project_id_from)){
                                        ## this is a public project, then match pipe again
                                        pipes <- pipeline_list_pub()
                                        pipeline_id <- m.match(pipes, name = pipeline_name, id = pipeline_id, exact = exact)$id
                                    }else{
                                        p <- project(id = project_id_from)
                                        pipeline_id <- p$pipeline(pipeline_name, pipeline_id)$id
                                    }
                                }
                            }
                            res <- sbgr::pipeline_add(auth_token,
                                                      project_id_to = project_id_to, 
                                                      project_id_from = project_id_from,
                                                      pipeline_id = pipeline_id,
                                                      revision = revision)
                            .asPipeline(res)
                            
                        },
                        billing = function(){
                            res <- sbgr::billing(auth_token)
                            res <- .asBillingList(res[[1]])
                            lapply(res, function(x){
                                x$auth <- .self
                                x
                            })
                        },
                        
                        show = function(){
                            .showFields(.self, "== Auth ==",
                                        values = c("auth_token", "url"))
                        }
                    ))

setClassUnion("AuthORNULL", c("Auth", "NULL"))
#' Class Item
#'
#' Class Item
#'
#' To describe a set of objects, Project, Task, Pipeline, File etc.
#' 
#' @field response save the raw response from a request.
#' @field auth_token propogate the auth_token from parent.
Item <- setRefClass("Item", fields = list(response = "ANY",
                                          auth = "AuthORNULL")) ## to stored the called Auth parent


Permission <- setRefClass("Permission", contains = "Item",
                          fields = list(
                                            write = "logical",
                                            copy_permission = "logical", #cannot use copy
                                            execute = "logical",
                                            admin = "logical"),
                          methods = list(
                              initialize = function(write = FALSE,
                                  copy_permission = FALSE,
                                  execute = FALSE,
                                  admin = FALSE, ...){
                                  
                                  write <<- write
                                  copy_permission <<- copy_permission
                                  execute <<- execute
                                  admin <<- admin

                                  callSuper(...)
                              },
                              show = function(){
                                  ## message("== Permission == ")
                                  message("write: ", write)
                                  message("copy: ", copy_permission)
                                  message("execute: ", execute)
                                  message("admin: ", admin)
                              }
                          ))



Member <- setRefClass("Member", contains = "Item",
                      fields = list(
                          id = "character",
                          username = "character",
                          invitation_pending = "logical",
                          permissions = "Permission"
                      ),
                      methods = list(
                          show = function(){
                              .showFields(.self, "== Member ==",
                                          values = c("id", "username",
                                              "invitation_pending"))
                              .self$permissions$show()
                          }
                      ))


Project <- setRefClass("Project", contains = "Item",
                       fields = list(id = "character",
                           name = "character",
                           description = "character",
                           my_permission = "Permission"),
                       methods = list(
                           initialize = function(id, name = "",
                               description = "",
                               my_permission = Permission(), ...){

                               if(missing(id))
                                   stop("id is required")

                               id <<- id
                               name <<- name
                               description <<- description
                               my_permission <<- my_permission

                               callSuper(...)
                           },
                           details = function(){
                               res <- sbgr::project_details(auth$auth_token, id)
                               response <<- res
                               id <<- res$id
                               name <<- res$name
                               description <<- resdescription
                               my_permission <<- do.call(Permission,
                                                         res$my_permission)
                               .self
                               
                           },
                           members = function(){
                               res <- project_members(auth$auth_token, id)
                               .asMemberList(res[[1]])
                           },
                           member = function(username = NULL,
                               name = username,
                               id = NULL, 
                               ignore.case = TRUE,
                               exact = TRUE){

                               ms <- members()
                               m.match(ms, id = id, name = name,
                                       .name = "username", exact = exact)
                           },
                           member_add = function(
                               username = NULL,
                               name = username, 
                               copy = FALSE,
                               write = FALSE,
                               execute = FALSE,
                               admin = FALSE){
                               
                               res <- sbgr::project_member_add(auth$auth_token,
                                                               project_id = .self$id,
                                                               username = name,
                                                               copy = copy,
                                                               write = write,
                                                               execute = execute,
                                                               admin = admin)

                               .asMember(res)

                           },
                           member_update = function(
                               username = NULL,
                               name = username, 
                               user_id = NULL,
                               write = FALSE,
                               copy = FALSE,
                               execute = FALSE,
                               admin = FALSE){

                               if(is.null(user_id)){
                                   if(is.null(name)){
                                       stop("user id or name should be provided.")
                                   }else{
                                       user_id <- member(name = name, exact = TRUE)$id
                                   }
                               }
                               
                               res <- sbgr::project_member_update(auth$auth_token,
                                                                  project_id = .self$id,
                                                                  user_id = user_id,
                                                                  write = write,
                                                                  copy = copy,
                                                                  execute = execute,
                                                                  admin = admin)
                               .asMember(res)
                           },
                           member_delete = function(
                               username = NULL,
                               name = username,
                               user_id = NULL){

                               if(is.null(user_id)){
                                   if(is.null(name)){
                                       stop("user id or name should be provided.")
                                   }else{
                                       user_id <- member(name = name, exact = TRUE)$id
                                   }
                               }
                               sbgr::project_member_delete(auth$auth_token,
                                                           .self$id,
                                                           user_id = user_id)
                           },
                           pipeline = function(name = NULL, id = NULL,
                               detail = FALSE, exact = TRUE){
                               res <- sbgr::pipeline_list_project(auth$auth_token,
                                                                  .self$id)
                               res <- .asPipelineList(res[[1]])
                               res <- m.match(res, name = name, id = id,
                                              exact = exact)
                               if(detail){
                                   res<- lapply(res, function(x){
                                       x <- sbgr::pipeline_details(auth$auth_token,
                                                              project_id = .self$id,
                                                              pipeline_id = x$id)
                                       .asPipeline(x)
                                   })
                                   
                               }
                               
                               if(is(res, "Pipeline")){
                                   res$project_id <- .self$id
                                   res$repos <- "project"
                                   res$auth <- .self$auth
                               }else if(is.list(res) && 
                                            all(sapply(res, is, "Pipeline"))){
                                   res <- lapply(res, function(x) {
                                       x$project_id <- .self$id
                                       x$repos <- "project"
                                       x$auth <- .self$auth
                                       x
                                       })
                               }
                               return(res)
                           },
                           pipeline_add = function(project_name_from = NULL,
                               pipeline_name = NULL,
                               project_id_from = NULL,
                               pipeline_id = NULL,
                               revision = NULL, ...){

                             
                                                   
                               auth$pipeline_add(
                                   project_name_to = name, 
                                   project_name_from,
                                   pipeline_name,
                                   project_id_from, 
                                   project_id_to = id,
                                   pipeline_id, 
                                   revision,
                                 ...)
                           
                           },
                           file_list = function(){
                               res <- sbgr::file_list(auth$auth_token, id)
                               res <- .asFileList(res[[1]])
                               lapply(res, function(x) {
                                   ## x$set_auth(auth_token)
                                   x$project_id <- id
                                   x})
                           },
                           file = function(name = NULL, id = NULL, exact = FALSE){
                               fl <- file_list()
                               if(is.null(name) & is.null(id)){
                                   return(fl)
                               }
                               res <- m.match(fl, id = id,
                                              name = name, exact = exact)
                               if(is(res, "File")){
                                   res$project_id <- .self$id
                                   res$auth <- .self$auth
                               }else if(is.list(res) && 
                                            all(sapply(res, is, "File"))){
                                   res <- lapply(res, function(x) {
                                       x$project_id <- .self$id
                                       x$auth <- .self$auth
                                       x
                                       })
                               }
                               return(res)
                           },
                           file_delete = function(name = NULL, file_id = NULL,
                               exact = TRUE){
                               f <- file(name, file_id, exact = exact)
                               ## exact = FALSE is very dangerous operation
                               sapply(f$id, function(fid){
                                   sbgr::file_delete(auth$auth_token, id, fid)
                               })
                           },
                           file_copy = function(file_id = NULL){
                               res <- sbgr::file_copy(auth$auth_token,
                                                      id, file_id)
                           },
                           upload = function(file = NULL, metadata = list()){
                               u <- Upload(auth$auth_token,
                                           file,
                                           project_id = id,
                                           metadata = metadata)
                               u$upload()
                           },
                           ## task
                           task_list = function(){
                               req <- sbgr::task_list(auth$auth_token, id)
                               res <- .asTaskList(req[[1]])
                               lapply(res, function(x){
                                   x$response <- response(req)
                                   x$project_id <- id
                                   x$auth <- .self$auth
                                   x
                               })
                           },
                           task = function(name = NULL, id = NULL,
                                           ignore.case = TRUE,
                                           exact = FALSE){
                               tks <- task_list()
                               if(is.null(name) & is.null(id))
                                   return(tks)
                               
                               res <- m.match(tks, id = id, name = name,
                                              exact = exact,
                                              ignore.case = ignore.case)
                               
                               if(is(res, "Task")){
                                   res$project_id <- .self$id
                                   res$auth <- .self$auth
                               }else if(is.list(res) && 
                                        all(sapply(res, is, "Task"))){
                                   res <- lapply(res, function(x) {
                                       x$project_id <- .self$id
                                       x$auth <- .self$auth
                                       x
                                   })
                               }
                               return(res)
                               
                               
                           },
                           task_run = function(...){
                               task = Task(auth = .self$auth,
                                           project_id = id, ...) 
                               
                               task$run()
                           },
                           
                           delete = function(){
                               sbgr::project_delete(auth$auth_token,
                                                    project_id = id)
                           },
                           show = function(){
                               ## callSuper()
                               .showFields(.self, "== Project ==",
                                           c("id", "name",
                                             "description"))
                               .self$my_permission$show()
                               
                           }
                       ))


.asProject <- function(x){
    Project(id = x$id,
            name = x$name,
            description = x$description,
            my_permission = do.call(Permission, x$my_permission),
            response = response(x))
    
    
}

.asProjectList <- function(x){
    lapply(x, .asProject)
}

.asMember <- function(x){
    Member(id = x$id,
           username = x$username,
           invitation_pending = x$invitation_pending,
           permissions = do.call(Permission, x$permissions),
           response = response(x))
}

.asMemberList <- function(x){
    lapply(x, .asMember)
}



## Billing
Billing <- setRefClass("Billing", contains = "Item",
                       fields = c("id", "name"),
                       methods = list(
                           show = function(){
                               .showFields(.self, "== Billing ==",
                                           values = c("id", "name"))

                           }
                       ))

.asBilling <- function(x){
    Billing(id = x$id,
            name = x$name,
            response = response(x))
}
.asBillingList <- function(x){
    lapply(x, .asBilling)
}

## Pipeline
Pipeline <- setRefClass("Pipeline", contains = "Item",
                        fields = list(id = "characterORNULL",
                            revision = "characterORNULL",
                            name = "characterORNULL",
                            description = "characterORNULL",
                            inputs = "ANY", ## Fixme
                            nodes = "ANY", ## Fixme
                            outputs = "ANY", ## Fixme
                            project_id = "characterORNULL",
                            repos = "characterORNULL"),## Fixme
                        methods = list(
                            details = function(){
                                if(repos == "public"){
                                    req <- sbgapi(auth_token = auth$auth_token,
                                           path = paste0('pipeline/public/', id), method = 'GET')
                                    res <- status_check(req)
                                }
                                if(repos == "my"){
                                    req <- sbgapi(auth_token = auth$auth_token,
                                                  path = paste0('pipeline/my/', id), method = 'GET')
                                    res <- status_check(req) 
                                }
                                if(repos == "project"){
                                    res <- sbgr::pipeline_details(auth$auth_token,
                                                                  project_id = project_id,
                                                                  pipeline_id = id) 
                                }
                                
                               
                            
                                res <- .asPipeline(res)
                                revision <<- res$revision
                                inputs <<- res$inputs
                                outputs <<- res$outputs
                                nodes <<- res$nodes
                                
                                
                                
                                return(.self)
                            },
                            show = function(){
                                .showFields(.self, "== Pipeline ==",
                                            c("id", "revision",
                                              "name", "description",
                                              "inputs","nodes", "outputs" ))
                            }
                        )
                        )


.asPipeline <- function(x){
    Pipeline(id = x$id,
             revision = x$revision,
             name = x$name,
             description = x$description,
             inputs = x$inputs,
             nodes = x$nodes,
             outputs = x$outputs,
             response = response(x))
}
.asPipelineList <- function(x){
    lapply(x, .asPipeline)
}

## Upload: Tuesday
## Kind of complex, think about how to make it easier?
Part <- setRefClass("Part", contains = "Item",
                    fields = list(
                        part_number = "numericORNULL",
                        part_size = "numericORNULL",
                        uri = "characterORNULL",
                        etag = "characterORNULL"),
                    methods = list(
                        initialize = function(part_number = NULL,
                            part_size = NULL,
                            uri = NULL,
                            etag = NULL, ...){
                            
                            .part_number <- as.integer(as.character(part_number))
                            .part_size <- as.integer(as.character(part_size))
                            if(.part_number >  10000 | .part_number <1){
                                stop("par_number has to be a number in the range 1 – 10000.")
                            }
                            uri <<- uri
                            part_number <<- .part_number
                            part_size <<- .part_size
                            etag <<- etag

                            callSuper(...)
                        },
                        show = function(){
                            .showFields(.self, "== Part ==",
                                        c("part_number", "uri"))

                        }
                    ))



Upload <- setRefClass("Upload", contains = "Item",
                      fields = list(
                          ## filepath = "characterORNULL",
                          file = "characterORNULL",                          
                          auth_token = "characterORNULL",
                          project_id = "characterORNULL",
                          name = "characterORNULL",
                          size = "numericORNULL",
                          part_size = "numericORNULL",
                          upload_id = "characterORNULL",
                          part = "list",
                          part_length = "integer",
                          part_finished = "integer",
                          initialized = "logical",
                          metadata = "Metadata"
                      ),
                      methods = list(
                          initialize = function(
                              auth_token = NULL,
                              file = NULL,
                              project_id = NULL,
                              name = NULL,
                              size = NULL,
                              part_size = NULL,
                              part_finished = 0L, 
                              initialized = FALSE,
                              part_length = NULL,
                              metadata = list(), ...){

                              metadata <<- normalizeMeta(metadata)
                              
                              initialized <<- initialized
                              part_finished <<- part_finished 
                              ## validation
                              stopifnot_provided(!is.null(file))
                              
                              file <<- normalizePath(file)
                              
                              if(!file.exists(file)){
                                  stop("file doesn't exist, please provide relative or aboslution path to the file")
                              }

                              if(is.null(name)){
                                  name <<- basename(file)
                              }else{
                                  name <<- name
                              }

                              
                              if(is.null(size)){
                                  size <<- file.size(file)
                              }else{
                                  size <<- size
                              }
                              
                              stopifnot_provided(
                                  !is.null(auth_token),
                                  !is.null(project_id))


                              if(is.numeric(.self$size)){
                                  if(!(.self$size <= 5497558138880 &
                                           .self$size >= 0))
                                      stop("size must be between 0 – 5497558138880, inclusive")
                              }else{
                                  stop("size must be between 0 – 5497558138880, inclusive")                                  
                              }

                              auth <<- Auth(auth_token)
                              project_id <<- project_id
                              ## fixme: try manual part-size
                              if(is.null(part_size))

                                  if(is.null(part_length)){
                                      if(is.null(part_size)){
                                          part_size <<- as.integer(5 * 1024^2)
                                      }
                                      part_length <<- as.integer(ceiling(.self$size/.self$part_size))                                      
                                  }else{
                                      ## go with priority part_length
                                      ## let's reuire integer here
                                      part_size <<- as.integer(ceiling(.self$size/part_length))
                                      ## round the length number
                                      part_length <<- as.integer(ceiling(.self$size/.self$part_size))                                    

                                  }

                              .part_size <- rep(.self$part_size, .self$part_length)
                              ## last part
                              .part_size[.self$part_length] <- .self$size -
                                  .self$part_size * (.self$part_length - 1)

                              part <<- vector("list", .self$part_length)

                              part <<- lapply(1:.self$part_length, function(idx){
                                  Part(part_number = idx,
                                       part_size = .part_size[idx])
                              })
                              if(.self$part_length == 1){
                                  .self$part_size <<- .self$size
                              }
                              callSuper(...)
                          },
                          upload_init = function(){
                              res <- sbgr::upload_init(auth_token = auth$auth_token,
                                                       project_id = project_id,
                                                       name = name,
                                                       size = size)

                              upload_id <<- res$upload_id
                              initialized <<- TRUE
                              message("Initialized")
                              invisible(res)
                          },
                          upload_info = function(){
                              if(is.null(upload_id)){
                                  stop("Upload is not initialized yet")
                              }
                              res <- sbgr::upload_info(auth$auth_token, upload_id)
                              show()
                              invisible(res)
                          },
                          upload_info_part = function(part_number = NULL){
                              stopifnot_provided(!is.null(part_number))
                              if(part_number >  10000 | part_number <1){
                                  stop("par_number has to be a number in the range 1 – 10000.")
                              }
                              cl <- c("Content-Length" = as.character(part[[part_number]]$part_size))
                              res <- status_check(sbgapi(auth$auth_token,
                                             path = paste0("upload/multipart/", 
                                                 upload_id, "/", part_number),
                                             method = "GET"))


                              ## update that part
                              part[[part_number]]$uri <<- res$uri
                              part[[part_number]]$etag <<- res$etag
                              part[[part_number]]$response <<- response(res)
                              part[[part_number]]
                          },
                          upload_file = function(metadata = list()){
                              if(length(metadata)){
                                  metadata <<- list(metadata)
                                  names(metadata) <<- "metadata"
                              }
                              ## make this one easy to use
                              N <- part_length
                              res <- upload_init()
                              pb <- txtProgressBar(min = 0, max = N, style = 3)
                              con <- file(file, "rb")
                              for(i in 1:N){
                                  p <- upload_info_part(i)
                                  uri <- p$uri
                                  b <- readBin(con, "raw", part_size)
                                  res <- PUT(uri, body = b)
                                  rm(b)
                                  etag <- headers(res)$etag
                                  part[[i]]$etag <<- etag
                                  upload_complete_part(i, etag)
                                  part_finished <<- as.integer(i)
                                  setTxtProgressBar(pb, i)
                              }
                              close(pb)                              
                              res <- upload_complete_all()
                              close(con)
                              message("file uploading complete")
                             
                              ## when we complete we could add meta
                              meta <- .self$metadata$asList()
                              if(length(meta)){
                                  message("Adding metadata ...")
                                  req <- sbgapi(auth_token = auth$auth_token,
                                                       path = paste0('project/',
                                                           project_id,
                                                           '/file/', res$id),
                                                       body = meta,
                                                       method = 'PUT')
                                  res <- status_check(req)
                                  message("Metadata complete")
                              }
                              res <- .asFile(res)                              
                              res
                          },
                          upload_complete_part = function(part_number = NULL,
                              etag = NULL){
                              res <- sbgr::upload_complete_part(auth$auth_token,
                                                                upload_id,
                                                                part_number,
                                                                etag)

                          },
                          upload_complete_all = function(){
                              ## fixme:
                              req <- sbgapi(auth_token = auth$auth_token,
                                             path = paste0("upload/multipart/", 
                                                 upload_id, "/complete"),
                                             method = "POST")
                              status_check(req)
                              
                          },
                          upload_delete = function(){
                              sbgr::upload_delete(auth$auth_token, upload_id)
                          },
                          show = function(){
                              .showFields(.self, "== Upload ==",
                                          c("initialized", "part_length",
                                            "part_finished",
                                            "project_id", "name",
                                            "size", "part_size", "upload_id"))
                          }
                      ))
## define alias
um <- Upload@generator$def@refMethods
Upload$methods(list(
    init = um$upload_init,
    info = um$upload_info,
    info_part = um$upload_info_part,
    delete = um$upload_delete,
    upload = um$upload_file
))


.asUpload <- function(x){
    Upload(auth = Auth(x$auth_token),
           project_id = x$project_id,
           name = x$name,
           size = x$size,
           part_size = x$part_size,
           response = response(x))
}
.asUploadList <- function(x){
    lapply(x, .asUpload)
}



## Files: Wednesday

File <- setRefClass("File", contains = "Item",
                    fields = list(id = "characterORNULL",
                        name = "characterORNULL",
                        size = "numericORNULL",
                        metadata = "listORNULL",
                        project_id = "characterORNULL",
                        url = "characterORNULL"),
                    methods = list(
                        initialize = function(id = NULL,
                            name = NULL,
                            size = NULL,
                            metadata = NULL,
                            project_id = NULL,
                            url = NULL, ...){

                            
                            id <<- id
                            name <<- name
                            size <<- size
                            metadata <<- metadata
                            project_id <<- project_id
                            url <<- url

                            callSuper(...)
                        },
                        delete = function(){
                            sbgr::file_delete(auth$auth_token, project_id, id)
                        },
                        download_url = function(){
                            sbgr::file_download_url(auth$auth_token, project_id, id)
                        },
                        download = function(destfile, ..., method = "curl"){
                            'see help(download.file) for more options'
                            if(is.null(url))
                                url <<- download_url()$url
                            if(dir.exists(destfile)){
                                ## is directory
                                if(!is.null(name))
                                    destfile <- file.path(destfile, name)
                            }
                            download.file(url, destfile, ..., method = method)
                        },
                        set_metadata = function(metadata = list(),
                                                append = FALSE, clean = FALSE){
                            'when append = TRUE, keep original, overwrite with the new value;
                             if FALSE, clean meta and replace everything with new metadata'

                            o <- .self$metadata
                            if(length(metadata)){
                                if(append){
                                    new.meta <- list(.update_list(o = o, metadata))
                                    names(new.meta) <- "metadata"

                                }else{
                                    new.meta <- list(metadata)
                                    names(new.meta) <- "metadata"
                                }
                                metadata <<- new.meta$metadata
                                
                                req <- sbgapi( auth$auth_token,
                                                     path = paste0('project/',
                                                         project_id,
                                                         '/file/', id),
                                                     body = new.meta,
                                                     method = 'PUT')
                                res <- status_check(req)
                                res <- .asFile(res)
                            }else{
                                if(length(o)){
                                    if(clean){
                                        new.meta <- list(lapply(o, function(x) return(NA)))
                                        names(new.meta) <- "metadata"
                                        message("cleaning metadata ...")
                                        req <- sbgapi(auth$auth_token,
                                                             path = paste0('project/',
                                                                 project_id,
                                                                 '/file/', id),
                                                             body = new.meta,
                                                             method = 'PUT')
                                        req
                                        res <- status_check(req)
                                        res <- .asFile(res)

                                    }else{
                                        stop("Please provide the metadata to update, if you want to clean the meta, please use clean = TRUE")
                                    }
                                }else{
                                    stop("Metadata provided is empty")
                                }
                                
                            }
                            return(res)
                        },
                        show = function(){
                            .showFields(.self,
                                        "== File ==",
                                        c("id", "name",
                                          "size"))
                            message("-- metadata --")
                            .showList(metadata)
                        }
                    ))

.asFile <- function(x){
    File(id = x$id,
         name = x$name,
         size = x$size,
         metadata = x$metadata,
         response = response(x))
}
.asFileList <- function(x){
    lapply(x, .asFile)
}

#' Task class
#' 
#' Task class
#' 
#' @field id [characterORNULL] The task ID number, used when referring to the 
#' task in other Seven Bridges API calls
#' @field name [characterORNULL] Name of the task you wish to execute. If this 
#' is not specified, the task will be named automatically.
#' @field description [characterORNULL] Description of the task you wish to execute.
#' @field pipeline_id [characterORNULL] ID of the pipeline you wish to execute.
#' @field pipeline_revision [characterORNULL] Revision number of the pipeline 
#' you wish to execute. If this is not specified, the latest pipeline revision 
#' is used.
#' @field start_time [numericORNULL] start time.
#' @field status [characterORNULL] 1) active: task is currently running. 
#' 2) completed: task has finished successfully. 3) aborted: task was aborted by 
#' user. 4) failed: task has failed to finish due to either bad inputs and/or 
#' parameters, or because of the internal infrastructure failures.
#' @field message [characterORNULL] task message
#' @field jobs_completed [numericORNULL] completed jobs
#' @field jobs_total [numericORNULL] total jobs.
#' @field inputs [listORNULL] required for task execution. List of key-value 
#' pairs containing mappings of pipeline input node ID to file IDs.
#'  Note that you must supply an array of file IDs for each input nodes, 
#'  even if the array is empty.
#' @field parameters [listORNULL] required for task execution. List of key-value 
#' pairs containing mappings of node IDs to apps specific parameters. Note that 
#' you must supply some value for parameters, even if this an empty list of 
#' key-value pairs.
#' @field project_id [characterORNULL] required for task execution. 
#' ID of the project you want to execute the task in.
#' 
#' @exportClass Task
#' @export Task
Task <- setRefClass("Task", contains = "Item",
                    fields = list(id = "characterORNULL",
                        name = "characterORNULL",
                        description = "characterORNULL",
                        pipeline_id = "characterORNULL",
                        pipeline_revision = "characterORNULL",
                        start_time = "numericORNULL",
                        status = "characterORNULL",
                        message = "characterORNULL",
                        jobs_completed = "numericORNULL",
                        jobs_total = "numericORNULL",
                        inputs = "listORNULL",
                        outputs = "listORNULL",
                        parameters = "listORNULL",
                        project_id = "characterORNULL"),
                    methods = list(
                        initialize = function(status = NULL, ...){
                            if(length(status)){
                                status <<- status$status
                                message <<- status$message
                                jobs_completed <<- status$jobs_completed
                                jobs_total <<- status$jobs_total
                            }
                            callSuper(...)
                        },
                        action = function(action = NULL){
                            if(is.null(action)){
                                stop("please specify action: currently only support 'abort'")
                            }
                            req <- sbgr::task_action(auth$auth_token, project_id,
                                id, action)
                            .asTask(req)
                        },
                        abort = function(){
                            action(action = "abort")
                        },
                        details = function(){
                            req <- sbgr::task_details(auth$auth_token,
                                               project_id,
                                               id)
                            
                           ## update
                            start_time <<- req$start_time
                            status <<- req$status$status
                            message <<- req$status$message
                            jobs_completed <<- req$status$jobs_completed
                            jobs_total <<- req$status$jobs_total
                            outputs <<- req$outputs
                            .self
                          
                        },
                        monitor = function(time = 30){
                            t0 <- Sys.time()
                            message("Monitoring ...")
                            while(TRUE){
                                d <- details()
                                
                                if(d$status != "active"){
                                    message("Finished")
                                    break
                                }
                                
                            
                                Sys.sleep(time)
                            }
                        },
                        download = function(destfile, ..., method = "curl"){
                            if(is.null(outputs)){
                                details()
                            }
                            fids <- sapply(outputs, function(x) x[[1]])
                            p <- auth$project(id = project_id)
                            
                            for(fid in fids){
                                fl <- p$file(id = fid)
                                message("downloading: ", fl$name)
                                fl$download(destfile, ..., method = method)
                            }
                            
                        },
                        run = function(){
                            ## turn this into a list
                            .nms <- c("name", "description",
                                      "pipeline_id", "pipeline_revision",
                                      "inputs", "parameters")

                           
                            
                            body <- .getFields(.self, .nms)
                    
                            req <- sbgr::task_run(auth$auth_token,
                                                  project_id,
                                                  body)
                            
                            ## update task object
                            id <<- req$id
                            name <<- req$name
                            description <<- req$description
                            pipeline_id <<- req$pipeline_id 
                            pipeline_revision <<- req$pipeline_revision
                            start_time <<- req$start_time
                            status <<- req$status$status
                            message <<- req$status$message
                            jobs_completed <<- req$status$jobs_completed
                            jobs_total <<- req$status$jobs_total
                            inputs <<- req$inputs
                            outputs <<- req$outputs
                            parameters <<- req$parameteres
                          
                            .self
                            
                        },
                        show = function(){
                            .nms <- c("id", "name", "description",
                                      "pipeline_id", "pipeline_revision",
                                      "start_time", "status",
                                      "message", "jobs_completed",
                                      "jobs_total", "parameters", 
                                      "inputs", "outputs")
                            .showFields(.self, "== Task ==", .nms)
                        }
                    ))

.asTask <- function(x){
    res <- do.call(Task, x)
    res$response <- response(x)
    res
}


.asTaskList <- function(x){
    lapply(x, .asTask)
}





## MISC
response <- function(x){
    attr(x, "response")
}

.getFields <- function(x, values) {
                           ## from Martin's code
                           flds = names(x$getRefClass()$fields())
                           if (!missing(values))
                               flds = flds[flds %in% values]
                           result = setNames(vector("list", length(flds)), flds)
                           for (fld in flds)
                               result[[fld]] = x[[fld]]
                           result
                       }

stopifnot_provided <- function(..., msg = "is not provided"){
    n <- length(ll <- list(...))
    if(n == 0)
        return(invisible())
    mc <- match.call()
    x = NULL
    for(i in 1:n){
        if(!(is.logical(r <- eval(ll[[i]])) && all(r))){
            l <- mc[[i+1]][[2]]
            x <- c(x, deparse(l[[length(l)]]))
        }
    }
    if(length(x))
        stop(paste(paste(x, collapse = ","), msg), call. = FALSE)
}




m.fun <- function(x, y, exact = TRUE, ignore.case = TRUE, ...){
    if(exact){
        pmatch(x, y, ...)
    }else{
        grep(x, y, ignore.case = ignore.case, ...)
    }
}

## match by id and name
m.match <- function(obj, id = NULL, name = NULL,
                    .id = "id",
                    .name = "name",
                    exact = TRUE, ignore.case = TRUE){
    ## if no match, return whole list
    if(is.null(id)){
        if(is.null(name)){
            return(obj)
        }else{
            ## id is null, so use username
            nms <- sapply(obj, function(x) x[[.name]])
            if(ignore.case){
                name <- tolower(name)
                nms <- tolower(nms)
            }
            index <- m.fun(name, nms,
                           exact = exact,
                           ignore.case = ignore.case)
        }
    }else{
        ## id is not NULL
        ids <- sapply(obj, function(x) x[[.id]])
        index <- m.fun(id, ids,
                       exact = exact,
                       ignore.case = ignore.case)

    }
    if(length(index) == 1 && is.na(index)){
        stop("no matching")
    }else{
        if(length(index) ==1){
            obj[[index]]
        }else{
            obj[index]
        }
    }
}


.showFields <- function(x, title = NULL, values = NULL){
    if (missing(values)){
        flds = names(x$getRefClass()$fields())
    }else{
        flds = values
    }

    ## if(is.null(title))
    ##     title <- class(x)
    if(!is.null(title)){
        message(title)        
    }
    
    for (fld in flds)
        message(fld, " : ", x[[fld]])
    
}

.showList <- function(x){
    if(length(x)){
        x <- x[!sapply(x, is.null)]
        for (fld in names(x))
            message(fld, " : ", x[[fld]])
    }
}


.update_list <- function(o, n){
    o.nm <- names(o)
    n.nm <- names(n)
    i.nm <- intersect(o.nm, n.nm)
    
    if(length(i.nm)){
        o.nm <- setdiff(o.nm, i.nm)
        return(c(o[o.nm], n))
    }else{
        return(c(o, n))
    }
}

normalizeMeta <- function(x){
    ## normalize it
    if(is.list(x)){
        if(length(x) > 1){
            res <- do.call(Metadata, x)
        }else if(length(x) == 1){
            res <- do.call(Metadata, x[[1]])
        }else{
            res <- Metadata()
        }
    }else if(is(x, "Metadata")){
        res <- x
    }else{
        stop("metadata has to be a list or Metadata object")
    }
    return(res)
}

