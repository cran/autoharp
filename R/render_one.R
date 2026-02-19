#' Run a single Rmd file through autoharp.
#'
#' Renders the specified file, and collates run time, static and 
#' correctness checks.
#'
#' @param rmd_name The path to the file to be rendered and checked.
#' @param out_dir The directory to store all the html output, md output, and
#' figures.
#' @param knit_root_dir The working directory while knitting the file.
#' @param log_name A character string, denoting the log file name. It defaults to
#' "render_one.log". If this file is already present in the directory, this
#' function will append to it.
#' @param soln_stuff This is a list, with components env, test_fname, and
#' tt_list. This object is the output of \code{\link{populate_soln_env}}. Set
#' this to be NA if you wish to skip correctness checks, and only do rendering.
#' @param max_time_per_run The maximum time to wait before aborting the
#' rendering of a particular file.
#' @param permission_to_install If TRUE, then the function will try to install 
#' any packages needed. By default, this is FALSE.
#'
#' @details The log file contains a record of the libraries used by the student, and if
#' any new libraries needed to be installed. The status will be one of SUCCESS,
#' FAIL or UNKNOWN.
#'
#' @importFrom parallel makePSOCKcluster clusterEvalQ clusterCall stopCluster clusterExport
#' @importFrom rmarkdown render
#' @return A data frame with one row for each file in the input directory.
#' @seealso \code{\link{populate_soln_env}}, \code{\link{check_correctness}}
#' @export
#'
render_one <- function(rmd_name, out_dir, knit_root_dir, log_name, soln_stuff,
                       max_time_per_run = 120, permission_to_install=FALSE) {
    # check if out_dir exists, if not, create it.
    if(!dir.exists(out_dir)) {
        dir.create(out_dir)
    }
    
    # Initialise log file.
    if(missing(log_name)){
        log_name <- "render_one.log"
    }
    out_log <- file.path(out_dir, log_name)
    out_file <- file(out_log, 'a+b')
    time_stamp <- as.character(Sys.time())
    cat('---', file=out_file, append=TRUE)
    cat('render_one commenced at', time_stamp, file=out_file, append=TRUE)
    cat('---\n', file=out_file, append=TRUE)
    cat('\nStarting on', rmd_name, '\n', file=out_file, append=TRUE)

#   Spin up a worker.
    worker_proc <- makePSOCKcluster(1)

    # Clean up steps
    on.exit({
        close.connection(out_file)
        stopCluster(worker_proc)
    })

    # Check solution stuff
    if(length(soln_stuff) == 2) {
      if(!setequal(names(soln_stuff), c("env", "test_fname"))) {
          stop("Incorrect solution object.")
      }
      check_corr_flag <- TRUE
    } else if(length(soln_stuff) == 1 && is.na(soln_stuff)){
      check_corr_flag <- FALSE
    } else {
      stop("Incorrect solution object.")
    }

    # Remove related files from out_dir
    root_name <- xfun::sans_ext(basename(rmd_name))
    unlink(paste0(file.path(out_dir, root_name), "*"))
    
    # prepare out_df
    out_df <- dplyr::bind_cols(fname = rmd_name, time_stamp = time_stamp)

    # Check for libraries
    lib_used <- get_libraries(rmd_name)
    if("error" %in% class(lib_used)) {
      cat('Libraries used: \n', file=out_file, append=TRUE)
      status <- "FAIL"
      cat(rmd_name, ":", status, "\n", file=out_file, append=TRUE)
      cat(conditionMessage(lib_used), '\n', file=out_file, append=TRUE)
      out_df <- dplyr::bind_cols(out_df, run_status = status)
      return(out_df)
    }
    cat('Libraries used:', paste0(lib_used, collapse=","), '\n', file=out_file,
        append=TRUE)
    if(!is.null(lib_used)) {
      id <- !(sapply(lib_used, quietly=TRUE, requireNamespace))
      lib_needed <- lib_used[id]
      if(length(lib_needed) > 0) {
        cat('Need to install:', paste0(lib_needed, collapse=","), '\n',
            file=out_file, append=TRUE)
        if(permission_to_install){
          try_install <- tryCatch(utils::install.packages(lib_needed, dependencies = TRUE),
                                error = function(e) return(e))
        } else {
            try_install <- simpleError("do not install")
        }
        if("error" %in% class(try_install)) {
          status <- "FAIL"
          cat(rmd_name, ":", status, "\n", file=out_file, append=TRUE)
          cat(conditionMessage(try_install), '\n', file=out_file, append=TRUE)
          out_df <- dplyr::bind_cols(out_df, run_status = status)
          return(out_df)
        }
      }
    }
    
    # Perform pre-checks
    pre_check_status <- render_prechecks(rmd_name)
    if(!pre_check_status) {
      status <- "FAIL"
      cat(rmd_name, ":", status, "\n", file=out_file, append=TRUE)
      cat("Precheck failed.. system( ) calls present or purl failed.", '\n', file=out_file, append=TRUE)
      out_df <- dplyr::bind_cols(out_df, run_status = status)
      return(out_df)
    }

    .myenv <- new.env()
    .myenv$.myfilename <- normalizePath(rmd_name)
    start_time <- Sys.time()

    clusterExport(worker_proc, c(".myenv", "rmd_name", "out_dir", "soln_stuff",
                                 "knit_root_dir", "max_time_per_run"), 
                  sys.frame(sys.nframe()))

    # set the limit on the worker process
    try_setLimit <- clusterEvalQ(worker_proc, 
                                 setTimeLimit(cpu=max_time_per_run, elapsed=max_time_per_run))
    clusterEvalQ(worker_proc, 
        try_out <- tryCatch(rmarkdown::render(rmd_name, output_format = 'html_document', 
                            output_dir = out_dir, clean=FALSE, quiet=TRUE, 
                            intermediates_dir = out_dir, knit_root_dir = knit_root_dir, 
                            envir = .myenv), error = function(e) e))
    try_out <- clusterCall(worker_proc, get, "try_out")[[1]]
    end_time <- Sys.time()

    if(class(try_out)[1] == "character") {
      if(stringr::str_sub(try_out, -4) == "html") {
        status <- "SUCCESS"
        .myenv <- clusterCall(worker_proc, get, ".myenv")[[1]]
      }
      else
        status <- "UNKNOWN"
    } else if("error" %in% class(try_out)) {
      status <- "FAIL"
      cat(rmd_name, ":", status, "\n", file=out_file, append=TRUE)
      cat(conditionMessage(try_out), '\n', file=out_file, append=TRUE)
      out_df <- dplyr::bind_cols(out_df, run_status = status)
      return(out_df)
    } else {
      status <- "UNKNOWN"
    }
    cat(rmd_name, ":", status, "\n", file=out_file, append=TRUE)
    run_time <-  round(as.numeric(difftime(end_time, start_time, units="secs")), 4)
    run_mem <- env_size(.myenv)
    # run correctness check on the worker process.
    # - use clusterEvalQ, NOT clusterCall
    # - Need to load autoharp on the worker in order to call check_correctness.
    if(check_corr_flag) {
        try_setLimit <- clusterEvalQ(worker_proc, 
                                     setTimeLimit(cpu=max_time_per_run, elapsed=max_time_per_run))
        load_ah <- clusterEvalQ(worker_proc, library(autoharp))
        clusterEvalQ(worker_proc, 
                      corr_out <- check_correctness(.myenv, soln_stuff$env, 
                                                    soln_stuff$test_fname))
        
        corr_out <- clusterCall(worker_proc, get, "corr_out")[[1]]
        
        if("error" %in% class(corr_out)){
          status <- "FAIL"
          cat(rmd_name, ":", status, "\n", file=out_file, append=TRUE)
          cat(conditionMessage(corr_out), '\n', file=out_file, append=TRUE)
          out_df <- dplyr::bind_cols(out_df, run_status = status)
          return(out_df)
        }
    } else {
        corr_out <- NULL
    }

    out_df <- dplyr::bind_cols(out_df, run_status = status, run_time = run_time, 
                               run_mem = run_mem, corr_out)
    out_df
}
