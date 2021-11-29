#----------------------------------------------------------------------
# wrapper functions for shinyFiles access to server files, subject to authorization
#----------------------------------------------------------------------
# note: this is _not_ a module due to the implementation of the shinyFiles package
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# a button to find a file to load
#----------------------------------------------------------------------
serverFilesButtonUI <- function(id){
    shinyFilesButton(
        id,
        "Load from Server",
        "Select data package, bookmark, or other source file to import",
        multiple = FALSE,
        buttonType = "default",
        class = NULL,
        icon = NULL,
        style = "width: 100%",
        viewtype = "detail"
    )
}
serverFilesButtonServer <- function(id, input, session, 
                                    rw = "read", filetypes = NULL,
                                    loadFn = function(file) NULL){
    paths <- getAuthorizedServerPaths(rw)
    addServerFilesObserver(id, input, loadFn, paths)
    shinyFileChoose(
        input,
        id,
        session = session,
        defaultRoot = NULL,
        defaultPath = "",
        roots = paths,
        filetypes = filetypes
    )
}
addServerFilesObserver <- function(id, input, loadFn, paths){
    observeEvent(input[[id]], {
        file <- input[[id]]
        req(file)
        reportProgress('serverFilesObserver')
        loadFn( parseFilePaths(paths, file) )
    })
}

#----------------------------------------------------------------------
# enable bookmark saving
#----------------------------------------------------------------------
serverBookmarkButtonUI <- function(id, label, class){
    shinySaveButton(
        id,
        label,
        "Save bookmark to server",

        filename = "xxxxx",

        filetype = "mdi",
        buttonType = "default",
        class = class,
        icon = icon("download"),
        style = "margin: 0;",
        viewtype = "detail"
    )
}
serverBookmarkButtonServer <- function(id, input, session,
                                       saveFn = function(file) NULL){
    paths <- getAuthorizedServerPaths('write')
    addServerBookmarkObserver(id, input, saveFn, paths)
    shinyFileSave(
        input, 
        id, 
        session = session,
        defaultRoot = NULL,
        defaultPath = "shinyFileSave", ##################### 
        allowDirCreate = TRUE,
        roots = paths,
        filetypes = 'mdi'
    )
}
addServerBookmarkObserver <- function(id, input, saveFn, paths){
    observeEvent(input[[id]], {
        file <- input[[id]]
        req(file)
        reportProgress('serverBookmarkObserver')
        saveFn( parseSavePath(paths, file) )
    })
}

#----------------------------------------------------------------------
# from the shinyFile documentation for dirGetter and fileGetter, used by shinyFileChoose, etc.
#----------------------------------------------------------------------
# roots         A named vector of absolute filepaths or a function returning a named vector of
#               absolute filepaths (the latter is useful if the volumes should adapt to changes in
#               the filesystem).
# restrictions  A vector of directories within the root that should be filtered out of the results
# filetypes     A character vector of file extensions (without dot in front i.e. ’txt’ not ’.txt’) to
#               include in the output. Use the empty string to include files with no extension. If
#               not set all file types will be included
# pattern       A regular expression used to select files to show. See base::grepl() for additional 
#               discussion on how to construct a regular expression (e.g., "log.*\\.txt")
#               hidden A logical value specifying whether hidden files should be returned or not