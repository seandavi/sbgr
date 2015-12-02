##' Class Auth
##'
##' Auth token object
##'
##' Every object could be requested from this Auth object and any action
##' could start from this object using cascading style. Please check vignette
##' 'easy-api' for more information.
##'
##' @field auth_token [character] your auth token.
##' @field url [character] basic url used for API, by default
##' it's \url{https://api.sbgenomics.com/1.1/}
##'
##' @param auth_token [character] your auth token.
##' @param platform [character %in% 'sbg-us', 'cgc'] which platform you are
##'  using, by default it is sbg us platform.
##' @param url [chracter] a URL for the API, default is \code{NULL},
##'  will use \code{api} parameter to switch to the right one.
##' @param version [character] default: 1.1 version used for api.
##'
##' @importFrom stringr str_match 
##'
##' @export Auth
##' @exportClass Auth
##' @examples
##' ## replace it with real token
##' token <- "aef7e9e3f6c54fb1b338ac4ecddf1a56"
##' a <- Auth(token)
##' \donttest{
##' ## get billing info
##' b <- a$billing()
##' ## create project
##' a$project_new(name = "API", description = "API tutorial",
##'               billing_group_id = b[[1]]$id)
##' p <- a$project("API")
##' ## get data
##' fl <- system.file("extdata", "sample1.fastq", package = "sbgr")
##' ## create meta data
##' fl.meta <- list(file_type = "fastq",
##'                 seq_tech = "Illumina",
##'                 sample = "sample1",
##'                 author = "tengfei")
##' ## upload data with metadata
##' p$upload(fl, metadata = fl.meta)
##' ## check uploading success
##' f.file <- p$file(basename(fl))
##' ## get the pipeline from public repos
##' f.pipe <- a$pipeline(pipeline_name = "FastQC")
##' ## copy the pipeline to your project
##' p$pipeline_add(pipeline_name = f.pipe$name)
##' ## get the pipeline from your project not public one
##' f.pipe <- p$pipeline(name = "FastQC")
##' ## check the inputs needed for running tasks
##' f.pipe$details()
##' ## Ready to run a task? go
##' f.task <- p$task_run(name = "my task",
##'                       description = "A text description",
##'                       pipeline_id = f.pipe$id,
##'                       inputs = list(
##'                           "177252" = list(f.file$id)
##'                           ))
##' f.task$run()
##' ## or you can just run with Task constructor
##' f.task <- Task(auth = Auth(token),
##'                name = "my task",
##'                description = "A text description",
##'                pipeline_id = f.pipe$id,
##'                project_id = p$id,
##'                inputs = list(
##'                    "177252" = list(f.file$id)
##'                    ))
##' ## Monitor you task
##' f.task$monitor(30)
##'
##' ## download a task output files
##' f.task <- p$task("my task")
##' f.task$download("~/Desktop/")
##'
##' ## Abort the task
##' f.task$abort()
##' }
Auth <- setRefClass("Auth", fields = list(auth_token = "character",
                                url = "character",
                                version = "character"),
                    methods = list(
                        initialize = function(
                            auth_token = NULL,
                            platform = c("sbg-us", "cgc"),
                            url = NULL,
                            version = c("1.1", "v2"), ...){

                            platform <- match.arg(platform)
                            .v <- match.arg(version)
                        
                            if(is.null(auth_token)){
                                stop("missing token")
                            }
                            
                            auth_token <<- auth_token

                            stopifnot(is.null(url) | is.character(url))

                            if(is.null(url)){
                                url <<- switch(platform,
                                               'sbg-us' =
                                                   paste0('https://api.sbgenomics.com/', .v, '/'),
                                               'cgc' =
                                                   paste0('https://cgc-api.sbgenomics.com/', "v2", '/')
                                               )
                                ## if(platform %in% c("cgc")){
                                ##     ## V2 only platform?
                                ##     version <<- "v2"
                                ## }else{
                                ##     version <<- .v
                                ## }                                
                            }else{
                                url <<- url
                                ## platform 
                            }
                            version <<- .ver(url)

                        },
                        project_owner = function(owner = NULL, ...){
                            'List the projects owned by and accessible to a particular user.
                             Each project\'s ID and URL will be returned.'
                            
                            if(is.null(owner)){
                                stop("owner must be provided. For example, Nate. ")
                            }
                            v2Check(version)
                            req <- sbgapi(auth_token = auth_token,
                                          base_url = url, 
                                          path = paste0('projects/', owner),
                                          method = 'GET', ...)
                            res <- status_check(req)
                            if(hasItems(res)){
                                rp <- parseItem(res)
                                obj <- .asProjectList(rp)
                            }else{
                                message("not found")
                                obj <- res
                            }
                             obj <- setAuth(obj, .self, "Project")
                            
                        }, 
                        project_list = function(...){
                            if(version == "1.1"){
                                res <- sbgr::project_list(auth_token,
                                                          base_url = url, ...)
                                obj <- .asProjectList(res[[1]])
                            }
                            if(version == "v2"){
                                req <- sbgapi(auth_token = auth_token,
                                              base_url = url, 
                                              path = 'projects', method = 'GET', ...)
                                res <- status_check(req)
                                obj <- .asProjectList(res$items)
                                ## let's fill the permission for project
                            }
                            setAuth(obj, .self, "Project")
                        },
                        project_new = function(name = NULL,
                                               description = NULL,
                            billing_group_id = NULL, ...){
                            
                            'Create new projects'

                            if(version == "1.1"){
                                res <- sbgr::project_new(auth_token, name = name,
                                                         base_url = url,
                                                         description = description,
                                                         billing_group_id = billing_group_id)
                            }
                            if(version == "v2"){
                                if (is.null(name) || is.null(description) || is.null(billing_group_id))
                                    stop('name, description, and billing_group_id must be provided')

                                body = list('name' = name,
                                    'description' = description,
                                    'billing_group_id' = billing_group_id)

                                req = sbgapi(auth_token = auth_token,
                                    base_url = url,
                                    path = 'projects', body = body,
                                    method = 'POST', ...)

                                res <- status_check(req)
                            }
                            res <- .asProject(res)
                            res <- setAuth(res, .self, "Project")
                        },
                        project_delete = function(id = NULL,
                                                  name = NULL){
                            if(!is.null(id)){
                                sbgr::project_delete(auth_token,
                                                     base_url = url,
                                                     project_id = id)
                            }else{
                                ## match name
                                message("Matching by names")
                                p <- .self$project(name = name)
                                message("Following projects will be deleted")
                                print(p)
                                sbgr::project_delete(auth_token,
                                                     base_url = url,
                                                     project_id = p$id)
                            }
                        },
                        ## main project api function to support multiple versions                        
                        project = function(
                            name = NULL,
                            id = NULL,
                            index = NULL,
                            ignore.case = TRUE,
                            exact = TRUE, ...){
                            'find project'

                            ## if(version == "1.1"){
                            pl <- .self$project_list()
                            res <- m.match(pl, id = id, name = name, exact = exact,
                                           ignore.case = ignore.case)
                            res <- setAuth(res, .self, "Project")
                            res
                            
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


                            res
                        },
                        pipeline_list_pub = function(){
                            res <- sbgr::pipeline_list_pub(auth_token,
                                                           base_url = url)
                            res <- .asPipelineList(res[[1]])
                            lapply(res, function(x) {
                                ## x$set_auth(auth_token)
                                #x$project_id <- id
                                x})
                        },
                        pipeline_list_my = function(){
                            res <- sbgr::pipeline_list_my(auth_token,
                                                          base_url = url)
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
                            res <- sbgr::pipeline_add(auth_token, base_url = url,
                                                      project_id_to = project_id_to,
                                                      project_id_from = project_id_from,
                                                      pipeline_id = pipeline_id,
                                                      revision = revision)
                            .asPipeline(res)

                        },
                        billing = function(){
                            if(version == "1.1"){
                                res <- sbgr::billing(auth_token, base_url = url)
                                res <- .asBillingList(res[[1]])
                                lapply(res, function(x){
                                    x$auth <- .self
                                    x
                                })
                            }
                            if(version == "v2"){
                                ## show api
                                sbgr::billing(auth_token, base_url = url)
                            }
                        },

                        ## v2
                        billing_groups = function(id = NULL, breakdown = FALSE, ...){
                            v2Check(version)
                            res <- sbgr::billing_groups(auth_token, base_url = url,
                                                        id = id, breakdown = breakdown, ...)
                            ## .asBillingGroup(res)
                            res
                        },
                        billing_invoices = function(id = NULL, ...){
                            v2Check(version)
                            sbgr::billing_invoices(auth_token ,
                                                   base_url = url,
                                                   id = id, ...)
                        },
                        api = function(){
                            v2Check(version)
                            req <- sbgapi(auth_token, base_url = url, path = "")
                            status_check(req)
                        },
                        show = function(){
                            .showFields(.self, "== Auth ==",
                                        values = c("auth_token", "url"))
                        },
                        ## v2 only feature
                        rate_limit = function(...){
                            v2Check(version)
                            req <- sbgr::rate_limit(auth_token, base_url = url, ...)
                            .asRate(req)
                        },
                        user = function(username = NULL, ...){
                            v2Check(version)
                            req <- sbgr::user_list(auth_token, base_url = url, username = username, ...)
                            .asUser(req)
                        }
                    ))


setClassUnion("AuthORNULL", c("Auth", "NULL"))

##' Class Item
##'
##' Class Item
##'
##' To describe a set of objects, Project, Task, Pipeline, File etc.
##'
##' @field response save the raw response from a request.
##' @field auth_token propagate the auth_token from parent.
##' @field href api href
Item <- setRefClass("Item", fields = list(response = "ANY",
                                auth = "AuthORNULL",
                                href = "characterORNULL")) ## to stored the called Auth parent


Permission <- setRefClass("Permission", contains = "Item",
                          fields = list(
                              write = "logicalORNULL",
                              copy_permission = "logicalORNULL", #cannot use copy
                              execute = "logicalORNULL",
                              admin = "logicalORNULL",
                          read = "logicalORNULL"),
                          methods = list(
                              initialize = function(write = NULL,
                                  copy_permission = NULL,
                                  execute = NULL,
                                  admin = NULL,
                                  read = NULL, ...){

                                  write <<- write
                                  copy_permission <<- copy_permission
                                  execute <<- execute
                                  admin <<- admin
                                  read <<- read

                                  callSuper(...)
                              },
                              show = function(){
                                  ## message("== Permission == ")
                                  message("read: ", read)
                                  message("write: ", write)
                                  message("copy: ", copy_permission)
                                  message("execute: ", execute)
                                  message("admin: ", admin)
                              }
                          ))



Member <- setRefClass("Member", contains = "Item",
                      fields = list(
                          pid = "characterORNULL",
                          id = "characterORNULL",
                          username = "characterORNULL",
                          invitation_pending = "logicalORNULL",
                          permissions = "Permission"
                      ),
                      methods = list(
                          update = function(write = NULL,
                              copy = NULL,
                              execute = NULL,
                              admin = NULL, read = NULL, ...){
                              
                              v2Check(auth$version)
                              if(is.null(pid)){
                                  stop("cannot find project id")
                              }
                              body = list('write' = write,
                                  'copy' = copy,
                                  'execute' = execute,
                                  'read' = read, 
                                  'admin' = admin)

                              body <- body[!sapply(body, is.null)]
                              
                              if(length(body) == 0)
                                  stop("please provide updated information")

                              nms <- names(body)
                              lst <- body
                              names(lst)[names(lst) == "copy"] <- "copy_permission"
                              for(nm in nms){
                                  .self$permissions$field(nm, lst[[nm]])
                              }


                              req = sbgapi(auth_token = auth$auth_token,
                                  base_url = auth$url, 
                                  path = paste0('projects/', pid, '/members/', username, '/permissions'),
                                  body = body, method = 'PUT', ...)
                              
                              res <- status_check(req)

                              .asMember(res)
                          },
                          delete = function(...){
                              stopifnot(!is.null(auth$version))
                              
                              if(auth$version == "1.1"){
                                  stop("not supported for v1.1 yet, please use project$member_delete() instead")
                              }
                              
                              if(auth$version == "v2"){
                                  req = sbgapi(auth_token = auth$auth_token,
                                      base_url = auth$url,
                                      path = paste0('projects/', pid, '/members/', username),
                                      method = 'DELETE', ...)
                                  res <- status_check(req)
                              }
                          },
                          show = function(){
                              .showFields(.self, "== Member ==",
                                          values = c("id", "username",
                                                     "invitation_pending"))
                              .self$permissions$show()
                          }
                      ))


## this Project object should support both version
Project <- setRefClass("Project", contains = "Item",
                       fields = list(id = "characterORNULL",
                           name = "characterORNULL",
                           billing_group_id = "characterORNULL", 
                           description = "characterORNULL",
                           type = "characterORNULL", 
                           my_permission = "Permission",
                           owner = "characterORNULL",
                           tags = "listORNULL"),
                       methods = list(
                           initialize = function(id = NULL, name = NULL,
                               billing_group_id = NULL, 
                               description = NULL,
                               type = NULL,
                               my_permission = Permission(),
                               owner = NULL,
                               tags = list(), ...){

                               if(is.null(id))
                                   stop("id is required")

                               id <<- id
                               name <<- name
                               description <<- description
                               my_permission <<- my_permission
                               type <<- type
                               owner <<- owner
                               tags <<- tags
                               billing_group_id <<- billing_group_id
                               
                               callSuper(...)
                           },
                           update = function(name = NULL, description = NULL, billing_group_id = NULL, ... ){
                               'update name/description/billing group for a project'


                               body = list('name' = name,
                                   'description' = description,
                                   'billing_group_id' = billing_group_id)

                               body <- body[!sapply(body, is.null)]
                               if(length(body) == 0)
                                   stop("please provide updated information")

                               nms <- names(body)

                               for(nm in nms){
                                .self$field(nm, body[[nm]])
                               }

                               req <- sbgapi(auth_token = auth$auth_token,
                                   base_url = auth$url, 
                                   path = paste0('projects/', id),
                                   body = body, method = 'POST', ...)

                               res <- status_check(req)
                               res <- .asProject(res)
                               res$auth <- .self$auth
                               res
                           },
                           details = function(...){
                               'This call returns the details of a specified project.
                                The project id is the project\'s name, with any spaces replaced by hyphens.'

                               if(ptype(id) == "1.1"){
                                   res <- sbgr::project_details(auth$auth_token, id,
                                                                base_url = auth$url)
                               }
                               if(ptype(id) == "v2"){
                                   req <- sbgapi(auth_token = auth$auth_token,
                                                 base_url = auth$url,
                                                 path = paste0('projects/', id), method = 'GET', ...)
                                   res <- status_check(req)
                               }
                               ## update
                               response <<- response(res)
                               type <<- res$type
                               name <<- res$name
                               description <<- res$description
                               type <<- res$type
                               billing_group_id <<- res$billing_group_id
                               tags <<- res$tags
                               
                               obj <- .asProject(res)
                               obj$response <- response(res)
                               .showFields(.self, "== Project ==",
                                           c("href", "id", "name",
                                             "description",
                                             "type", "billing_group_id", "tags"))
                               invisible(obj)
                           },
                           ## suggested member API use members
                           members = function(username = NULL,
                               name = username,
                               ignore.case = TRUE,
                               exact = TRUE, ...){

                               if(is.null(id))
                                   stop("id must be provided")
                               ## depends on owner information to decide which version we use
                               if(ptype(id) == "1.1"){
                                   ## use V1.1
                                   res <- project_members(auth$auth_token, id)
                                   ms <- .asMemberList(res[[1]])                                   
                               }
                               if(ptype(id) == "v2"){
                                   ## use v2
                                   req = sbgapi(auth_token = auth$auth_token,
                                       base_url = auth$url, 
                                       path = paste0('projects/', id, '/members'),
                                       method = 'GET', ...)
                                   res <- status_check(req)
                                   ms <- .asMemberList(parseItem(res), pid = id)
                                   ms <- setAuth(ms, .self$auth, "Member")
                               }
                               
                               if(is.null(name)){
                                   return(ms)
                               }else{
                                   m <- m.match(ms, name = name,
                                                .name = "username", exact = exact)
                                   return(m)
                               }

                           },
                           member = function(username = NULL,
                                             name = username,
                                             id = NULL,
                                             ignore.case = TRUE,
                               exact = TRUE){
                               
                               .Deprecated("obj$members") 

                           },
                           member_add = function(
                               username = NULL,
                               name = username,
                               copy = FALSE,
                               write = FALSE,
                               execute = FALSE,
                               admin = FALSE,
                               read = FALSE,
                               ...){

                               if(ptype(id) == "1.1"){
                                   res <- sbgr::project_member_add(auth$auth_token,
                                                                   project_id = .self$id,
                                                                   username = name,
                                                                   copy = copy,
                                                                   write = write,
                                                                   execute = execute,
                                                                   admin = admin,
                                                                   read = read, 
                                                                   base_url = auth$url)

                                   .asMember(res)
                               }
                               if(ptype(id) == "v2"){
                                   body <- list('username' = name,
                                                'permissions' = list(
                                                    'copy' = copy, 'write' = write,
                                                    'read' = read,
                                                    'execute' = execute, 'admin' = admin))

                                   req = sbgapi(auth_token = auth$auth_token,
                                       base_url = auth$url,
                                       path = paste0('projects/', id, '/members'),
                                       body = body, method = 'POST', ...)
                                   
                                   res <- status_check(req)
                                   .asMember(res)
                               }

                           },
                           member_update = function(
                               ## onvly v1
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
                                                                  admin = admin,
                                                                  base_url = auth$url)
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
                                                           user_id = user_id,
                                                           base_url = auth$url)
                           },
                           pipeline = function(name = NULL, id = NULL,
                                               detail = FALSE, exact = TRUE){
                               res <- sbgr::pipeline_list_project(auth$auth_token,
                                                                  base_url = auth$url,
                                                                  .self$id)
                               res <- .asPipelineList(res[[1]])
                               res <- m.match(res, name = name, id = id,
                                              exact = exact)
                               if(detail){
                                   res<- lapply(res, function(x){
                                       x <- sbgr::pipeline_details(auth$auth_token,
                                                                   project_id = .self$id,
                                                                   pipeline_id = x$id,
                                                                   base_url = auth$url)
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
                               res
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
                               res <- sbgr::file_list(auth$auth_token, id,
                                                      base_url = auth$url)
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
                               res
                           },
                           file_delete = function(name = NULL, file_id = NULL,
                                                  exact = TRUE){
                               f <- file(name, file_id, exact = exact)
                               ## exact = FALSE is very dangerous operation
                               sapply(f$id, function(fid){
                                   sbgr::file_delete(auth$auth_token, id, fid, base_url = auth$url)
                               })
                           },
                           file_copy = function(file_id = NULL){
                               res <- sbgr::file_copy(auth$auth_token,
                                                      id, file_id, base_url = auth$url)
                           },
                           upload = function(file = NULL, metadata = list()){
                               u <- Upload(auth = auth,
                                           file,
                                           project_id = id,
                                           metadata = metadata)
                               u$upload()
                           },
                           ## task
                           task_list = function(){
                               req <- sbgr::task_list(auth$auth_token, id, base_url = auth$url)
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
                               res


                           },
                           task_run = function(...){
                               task = Task(auth = .self$auth,
                                           project_id = id, ...)

                               task$run()
                           },
                           delete = function(...){
                               if(ptype(id) == "1.1"){
                                   res <- sbgr::project_delete(auth$auth_token,
                                                        project_id = id,
                                                        base_url = auth$url, ...)
                               }
                               if(ptype(id) == "v2"){
                                   req = sbgapi(auth_token = auth$auth_token,
                                       base_url = auth$url, 
                                       path = paste0('projects/', id), method = 'DELETE', ...)

                                   res <- status_check(req)
                               }
                               res
                           },
                           show = function(){
                               .showFields(.self, "== Project ==",
                                           c("id", "name", "description"))
                           }
                       ))


.asProject <- function(x){
    if(is.null(x$my_permission)){
        Project(id = x$id,
                href = x$href,
                name = x$name,
                type = x$type,
                owner = x$owner,
                tags = x$tags,
                description = x$description, ## v1 only entry
                billing_group_id = x$billing_group_id, 
                response = response(x))
        
    }else{
        Project(id = x$id,
                href = x$href,
                name = x$name,
                type = x$type,
                owner = x$owner,
                tags = x$tags,
                description = x$description, ## v1 only entry
                billing_group_id = x$billing_group_id,                 
                my_permission = do.call(Permission, x$my_permission), ## v1 only entry
                response = response(x))
        
    }
}

.asProjectList <- function(x){
    lst <- lapply(x, .asProject)
    rp <- response(x)
    if(is.null(rp)){
        ## if not a list entry, searcy attr
        rp <- attr(x, "response")
    }
    attr(lst, "response") <- rp
    attr(lst, "href") <- x$href
    lst
}

.asMember <- function(x, pid = NULL){
    Member(id = x$id,
           pid = pid, 
           username = x$username,
           invitation_pending = x$invitation_pending,
           permissions = do.call(Permission, x$permissions),
           response = response(x))
}

.asMemberList <- function(x, pid = NULL){
    lst <- lapply(x, .asMember, pid = pid)
    rp <- response(x)
    if(is.null(rp)){
        ## if not a list entry, searcy attr
        rp <- attr(x, "response")
    }
    attr(lst, "response") <- rp
    attr(lst, "href") <- x$href
    lst    
}


## TODO: when it's get stable, define billing object
## PrivilegesEnum <- setSingleEnum("Privileges", c(""))
## Billing
## to make it simple to update, return a list, not an object, because no action defined an this object
Billing <- setRefClass("Billing", contains = "Item",
                       fields = list(id = "characterORNULL",
                           href = "characterORNULL",
                           name = "characterORNULL",
                           owner = "characterORNULL",
                           privileges = "list",
                           type = "characterORNULL",
                           pending = "logicalORNULL",
                           disabled = "logicalORNULL",
                           active = "logicalORNULL",
                           balance = "listORNULL"), ## 1.1
                       
                       methods = list(
                           
                           initialize = function(id = NULL,
                               href = NULL, name = NULL,
                               owner = NULL, privileges = list(),
                               type = NULL, pending = NULL,
                               disabled = NULL, active = NULL, balance = list(), ...){

                               id <<- id
                               href <<- href
                               name <<- name
                               owner <<- owner
                               privileges <<- privileges
                               type <<- type
                               disabled <<- disabled
                               active <<- active
                               balance <<- balance
                               
                               
                           },
                           
                           show = function(){
                               .showFields(.self, "== Billing ==",
                                           values = c("id", "href", "name",
                                               "owner", "privileges", "type",
                                               "disabled", "active", "balance"))

                           }
                       ))

.asBilling <- function(x){
    Billing(id = x$id,
            href = x$href,
            name = x$name,
            owner = x$owner,
            privileges = x$privileges,
            type = x$type,
            disabled = x$disabled,
            active = x$active,
            balance = x$balance,
            response = response(x))
}

.asBillingList <- function(x){
    lapply(x, .asBilling)
}

.asBillingGroup <- function(x){
    ## x is a req
    bg <- .asBillingList(x$items)
    ## TODO: should make group a class?
    attr(bg, "href") <- x$href
    bg
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
                                                  base_url = auth$url,
                                                  path = paste0('pipeline/public/', id), method = 'GET')
                                    res <- status_check(req)
                                }
                                if(repos == "my"){
                                    req <- sbgapi(auth_token = auth$auth_token,
                                                  base_url = auth$url,
                                                  path = paste0('pipeline/my/', id), method = 'GET')
                                    res <- status_check(req)
                                }
                                if(repos == "project"){
                                    res <- sbgr::pipeline_details(auth$auth_token,
                                                                  project_id = project_id,
                                                                  pipeline_id = id,
                                                                  base_url = auth$url)
                                }



                                res <- .asPipeline(res)
                                revision <<- res$revision
                                inputs <<- res$inputs
                                outputs <<- res$outputs
                                nodes <<- res$nodes



                                .self
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
                                stop("par_number has to be a number in the range 1-10000.")
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
                                  ## file.zie is R 3.2
                                  ## to be compatible
                                  ## size <<- file.size(file)
                                  size <<- file.info(file)$size
                              }else{
                                  size <<- size
                              }

                              stopifnot_provided(!is.null(project_id))


                              if(is.numeric(.self$size)){
                                  if(!(.self$size <= 5497558138880 &
                                           .self$size >= 0))
                                      stop("size must be between 0 - 5497558138880, inclusive")
                              }else{
                                  stop("size must be between 0 - 5497558138880, inclusive")
                              }


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
                                                       size = size,
                                                       base_url = auth$url)

                              upload_id <<- res$upload_id
                              initialized <<- TRUE
                              message("Initialized")
                              invisible(res)
                          },
                          upload_info = function(){
                              if(is.null(upload_id)){
                                  stop("Upload is not initialized yet")
                              }
                              res <- sbgr::upload_info(auth$auth_token, upload_id,
                                                       base_url = auth$url)
                              show()
                              invisible(res)
                          },
                          upload_info_part = function(part_number = NULL){
                              stopifnot_provided(!is.null(part_number))
                              if(part_number >  10000 | part_number <1){
                                  stop("par_number has to be a number in the range 1- 10000.")
                              }
                              cl <- c("Content-Length" = as.character(part[[part_number]]$part_size))
                              res <- status_check(sbgapi(auth$auth_token,
                                                         base_url = auth$url,
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
                                                base_url = auth$url,
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
                                                                etag, base_url = auth$url)

                          },
                          upload_complete_all = function(){
                              ## fixme:
                              req <- sbgapi(auth_token = auth$auth_token,
                                            base_url = auth$url,
                                            path = paste0("upload/multipart/",
                                                          upload_id, "/complete"),
                                            method = "POST")
                              status_check(req)

                          },
                          upload_delete = function(){
                              sbgr::upload_delete(auth$auth_token, upload_id,
                                                  base_url = auth$url)
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
    Upload(
        ## auth = Auth(x$auth_token),
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
                            sbgr::file_delete(auth$auth_token, project_id, id,
                                              base_url = auth$url)
                        },
                        download_url = function(){
                            sbgr::file_download_url(auth$auth_token, project_id, id ,
                                                    base_url = auth$url)
                        },
                        download = function(destfile, ..., method = "curl"){
                            'see help(download.file) for more options'
                            if(is.null(url))
                                url <<- download_url()$url
                            ## for compatible reason, R 3.1 doesn't have dir.exists
                            ##
                            .dir.exists <- function(d) {
                                de <- file.info(d)$isdir
                                ifelse(is.na(de), FALSE, de)
                            }
                            if(.dir.exists(destfile)){
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
                                               base_url = auth$url,
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
                                                      base_url = auth$url,
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
                            res
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

##' Task class
##'
##' Task class
##'
##' A task execution require auth, project_id, pipeline_id and inputs
##' parameters, there are two ways to execute a task, the recommended
##' way is to use a cascading method to create a project object called
##' \code{p} then just call \code{p$task_run()} to pass your
##' parameters. This way you save your time passing auth and
##' project_id. The other way is to create a Task object with all
##' required fields and call \code{run} method. Please check example
##' in the end or tutorial for easy API.
##'
##' @field id [characterORNULL] The task ID number, used when referring to the
##' task in other Seven Bridges API calls
##' @field name [characterORNULL] Name of the task you wish to execute. If this
##' is not specified, the task will be named automatically.
##' @field description [characterORNULL] Description of the task you wish to execute.
##' @field pipeline_id [characterORNULL] ID of the pipeline you wish to execute.
##' @field pipeline_revision [characterORNULL] Revision number of the pipeline
##' you wish to execute. If this is not specified, the latest pipeline revision
##' is used.
##' @field start_time [numericORNULL] start time.
##' @field status [characterORNULL] 1) active: task is currently running.
##' 2) completed: task has finished successfully. 3) aborted: task was aborted by
##' user. 4) failed: task has failed to finish due to either bad inputs and/or
##' parameters, or because of the internal infrastructure failures.
##' @field message [characterORNULL] task message
##' @field jobs_completed [numericORNULL] completed jobs
##' @field jobs_total [numericORNULL] total jobs.
##' @field inputs [listORNULL] required for task execution. List of key-value
##' pairs containing mappings of pipeline input node ID to file IDs.
##'  Note that you must supply an array of file IDs for each input nodes,
##'  even if the array is empty.
##' @field parameters [listORNULL] required for task execution. List of key-value
##' pairs containing mappings of node IDs to apps specific parameters. Note that
##' you must supply some value for parameters, even if this an empty list of
##' key-value pairs.
##' @field project_id [characterORNULL] required for task execution.
##' ID of the project you want to execute the task in.
##'
##' @exportClass Task
##' @export Task
##' @examples
##' token <- "aef7e9e3f6c54fb1b338ac4ecddf1a56"
##' a <- Auth(token)
##' ## A task constructor
##' Task(auth = Auth(token),
##'               name = "my task",
##'               description = "A text description",
##'               pipeline_id = "fake_pipeline_id",
##'               project_id = "fake_project_id",
##'               inputs = list(
##'                   "177252" = list("fake_id")
##'                   ))
##' \donttest{
##' ## replace with real token then follow the examples here
##' ## get billing info
##' b <- a$billing()
##' p <- a$project("API")
##' ## get the pipeline from your project not public one
##' f.pipe <- p$pipeline(name = "FastQC")
##' ## check the inputs needed for running tasks
##' f.pipe$details()
##' ## Ready to run a task? go
##' f.task <- p$task_run(name = "my task",
##'                       description = "A text description",
##'                       pipeline_id = f.pipe$id,
##'                       inputs = list(
##'                           "177252" = list(f.file$id)
##'                           ))
##' f.task$run()
##' ## or you can just run with Task constructor
##' f.task <- Task(auth = Auth(token),
##'                name = "my task",
##'                description = "A text description",
##'                pipeline_id = f.pipe$id,
##'                project_id = p$id,
##'                inputs = list(
##'                    "177252" = list(f.file$id)
##'                    ))
##' ## Monitor you task
##' f.task$monitor(30)
##'
##' ## download a task output files
##' f.task <- p$task("my task")
##' f.task$download("~/Desktop/")
##'
##' ## Abort the task
##' f.task$abort()
##' }
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
                                                     id, action, base_url = auth$url)
                            req
                        },
                        abort = function(){
                            action(action = "abort")
                        },
                        details = function(){
                            req <- sbgr::task_details(auth$auth_token,
                                                      project_id,
                                                      id, base_url = auth$url)

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
                                                  body,
                                                  base_url = auth$url)

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
                            parameters <<- req$parameters

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

isv2 <- function(version){
    version == "v2"
}

v2Check <- function(version, msg = "This function only supported in API V2"){
    if(version != "v2")
        stop(msg,  call. = FALSE)
}

## Rate limit
Rate <- setRefClass("Rate", contains = "Item",
            fields = list(limit = "numeric",
                remaining = "numeric",
                reset = "numeric"),
            methods = list(
                show = function(){
                    .showFields(.self, "== Rate Limit ==",
                                values = c("limit", "remaining", "reset"))
                }
            ))

.asRate <- function(x){
    Rate(limit = x$rate$limit,
         remaining = x$rate$remaining,
         reset = x$rate$reset)
}

User <- setRefClass("User", contains = "Item",
                    fields = c("username", "email",
                        "first_name", "last_name", "affiliation",
                        "phone", "address", "city",
                        "state", "country", "zip_code",
                        "projects", "billing_groups", "tasks"),
                    methods = list(
                        initialize = function(href = "", 
                            username = "",
                            email = "",
                            first_name = "",
                            last_name = "",
                            affiliation = "",
                            phone = "",
                            address= "",
                            city = "",
                            state = "",
                            country = "",
                            zip_code = "",
                            projects = "",
                            billing_groups= "",
                            tasks= "", ...){

                            href <<- href
                            username <<- username 
                            email <<- email 
                            first_name <<- first_name
                            last_name <<- last_name
                            affiliation <<- affiliation 
                            phone <<- phone 
                            address <<- address
                            city <<- city 
                            state <<- state 
                            country <<- country 
                            zip_code <<- zip_code
                            projects <<- projects
                            billing_groups <<- billing_groups
                            tasks <<- tasks

                        },
                        show = function(){
                            .showFields(.self, "== Rate Limit ==",
                                        values = c("href",
                                            "username",
                                            "email",
                                            "first_name",
                                            "last_name",
                                            "affiliation",
                                            "phone",
                                            "address",
                                            "city",
                                            "state",
                                            "country",
                                            "zip_code",
                                            "projects",
                                            "billing_groups",
                                            "tasks"))
                            
                        }
                    ))


.asUser <- function(x){
    User(href = x$href, 
         username = x$username,
         email = x$email,
         first_name = x$first_name,
         last_name = x$last_name,
         affiliation = x$affiliation,
         phone = x$phone,
         address= x$addrss,
         city = x$city,
         state = x$state,
         country = x$country,
         zip_code = x$zip_code,
         projects = x$projects,
         billing_groups= x$billing_groups,
         tasks= x$tasks)
}
