#----------------------------------------------------------------------
# run the mdi pipelines command tool from within the apps framework
#----------------------------------------------------------------------

# the mdi utility associated with the same MDI installation as this server
mdiCommandTarget <- file.path(serverEnv$ACTIVE_MDI_DIR, 'mdi')

# use system2 to call mdiCommandTarget
runMdiCommand <- function(
    args = character(), 
    collapse = TRUE, 
    FAIL = TRUE,
    errorDialog = NULL,
    suite = NULL
){
    if(serverEnv$IS_DEVELOPER) args <- c('-d', args)
    Sys.setenv(IS_PIPELINE_RUNNER = TRUE)
    tryCatch({
        x <- suppressWarnings(system2(
            mdiCommandTarget, 
            args,
            stdout = TRUE,
            stderr = TRUE
        ))
        command <- paste(mdiCommandTarget, paste(args, collapse = " "))
        if(!FAIL || isMdiSuccess(x, command = command, suite = suite)) list(
            success = TRUE, 
            command = command,
            results = if(collapse) paste(x, collapse = "\n") else x
        ) else {
            if(!is.null(errorDialog)) errorDialog(command, x)
            list(
                success = FALSE, 
                command = command,
                results = x  
            )            
        }
    }, error = function(e){ # this capture system2 errors, NOT mdi errors (which return in stderr)
        list(
            success = FALSE, 
            command = command,
            results = e
        )
    })
}

# determine whether mdi (not system2) reported a usage/configuration error
# generally, we expect launcher.pl to not die, so routinely only check its messages
# some developers may need to temporarily check mdiSuiteIsLocked to debug mdi-pipeline-framework code
isMdiSuccess <- function(results, caller = NULL, command = NULL, suite = NULL){
    check <- paste(results, collapse = "\n")
    success <- !grepl('mdi error:', check) && 
               !grepl('!!!!!!!!!!', check) &&
               !grepl('WARNING!', check) 
            #    && 
            #    !mdiSuiteIsLocked(suite) # locks fail to clear when launcher.pl dies prematurely (not when it throws an error)
    if(!success) {
        border <- paste(rep("!", 80), collapse = "")
        error <- "the mdi utility returned an error"
        if(!is.null(caller)) error <- paste0(caller, error, ": ")
        message(border)
        message(error)
        if(!is.null(command)) message(command)
        message(results)
        message(border)
        mdiUnlockAll()
        stopSpinner(session)
    }
    success
}

# prevent errors due to mdi command target crashes from propagating as lock errors
mdiUnlockAll <- function(){
    sapply(c(
        file.path(serverEnv$ACTIVE_MDI_DIR, "suites", "*.lock"),
        file.path(serverEnv$ACTIVE_MDI_DIR, "frameworks", "*.lock")
    ), unlink, force = TRUE)
}
mdiSuiteIsLocked <- function(suite){
    if(is.null(suite)) return(FALSE)
    file.exists(file.path(serverEnv$ACTIVE_MDI_DIR, "suites", paste0(suite, ".lock")))
}
