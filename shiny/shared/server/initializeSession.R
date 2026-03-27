#----------------------------------------------------------------------
# session initialization
#----------------------------------------------------------------------

# initialize a user session and record at framework and server levels
MbRAM_beforeStart <- sum(gc()[, 2]) # RAM dedicated to R before app started running
sessionEnv <- environment()
sessionEnv$invalidateGitBranch <- reactiveVal(0)
sessionStartTime <- Sys.time()
sessionNumber <- sample(1:1e8, 1)
sessionId <- paste(sessionStartTime, sessionNumber) # user session ID, like shinyId and serverId at higher scopes
sessionHash <- digest(sessionId) # NB: this is per _page_load_, unlike sessionKey which is per user encounter
sessionUrlBase <- paste('sessions', sessionHash, sep = "/")
sessionDirectory <- file.path(serverEnv$SESSIONS_DIR, sessionHash)
dir.create(sessionDirectory, showWarnings = FALSE)
nServerSessions       <<- nServerSessions + 1
nActiveServerSessions <<- nActiveServerSessions + 1    
nShinySessions        <<- nShinySessions + 1
nActiveShinySessions  <<- nActiveShinySessions + 1
userIP <- NULL
dataDirs <- list() # subdirectories in serverEnv$DATA_DIR

# instantiate session variables
app <- list(NAME = CONSTANTS$apps$launchPage) # parameters of the specific application
manifestTypes <- list()      # parameters of manifest files for different data classes
stepModuleInfo <- list()     # metadata about appStep modules, set by module.yml scripts
appStepNamesByType <- list() # lookup to find an app step name by the type(s) of its module
#analysisTypes <- list()     # parameters defining different types of data analyses
locks <- list()              # key relationships for data/UI integrity
bookmark <- NULL             # for saving and restoring app state
serverBookmark <- NULL       
modalTmpFile <- NULL         # path to a file currently stored in www/tmp for loading into a modal
inlineScripts <- list()      # paths to scripts sourced by app step servers
authenticatedUserData <- list() # authenticated user info+token/key (session-specific, not always required)
resolveActiveMdiDir <<- function(dir){
    if(is.null(serverEnv$CALLER_MDI_DIR)) return(dir)
    sub("/srv/active/mdi", serverEnv$CALLER_MDI_DIR, dir)
}
headerStatusData <- reactiveValues( # for UI display
    userDisplayName = if(serverEnv$REQUIRES_AUTHENTICATION) "" 
                      else paste(Sys.getenv(c('USERNAME', 'USER')), collapse = ""),
    dataDirDisplay = resolveActiveMdiDir(R.utils::getAbsolutePath(serverEnv$DATA_DIR))
)
aceEditorCache <- list()
rConsoleCache <- list()
commandTerminalCache <- list()
dataPackagesCache <- list()
lastLoadedBookmark <- reactiveVal("")

# load support scripts required to run the framework and apps
# note that scripts are loaded at the session, not the global, level
getExternalSuiteFile <- function(suite, shinyPath){
    getPath <- function(fork) file.path(serverEnv$SUITES_DIR, fork, suite, "shiny", shinyPath)
    file <- if(serverEnv$IS_DEVELOPER) getPath("developer-forks") else "__XXX__"
    if(!file.exists(file)) file <- getPath("definitive")  
    file  
}
sourceExternalScript <- function(suite, shinyPath){
    file <- getExternalSuiteFile(suite, shinyPath)
    if(file.exists(file)) source(file)
}
loadExternalYml <- function(suite, shinyPath){
    file <- getExternalSuiteFile(suite, shinyPath)
    if(file.exists(file)) read_yaml(file) else NULL
}
onScriptSourceError <- function(script, local, error){ # catch script source errors
    sapply(c( # load just what is need to handle the offending script in the code editor
        "global/utilities/ui.R",
        "global/utilities/logging.R",
        "global/utilities/strings.R", 
        "session/ui/modal_popup.R",
        "session/ui/destroyModules.R",
        "session/modules/widgets/framework/aceEditor/aceEditor_ui.R",
        "session/modules/widgets/framework/aceEditor/aceEditor_server.R",
        "session/modules/widgets/framework/aceEditor/aceEditor_utilities.R",
        "session/modules/widgets/framework/reloadAppScripts/reloadAppScripts_server.R"
    ), source, local = local)
    showAceEditor(
        session, 
        showFile = file.path(script),
        editable = serverEnv$IS_DEVELOPER,
        sourceError = error,
        sourceErrorType = sessionEnv$sourceLoadType
    )
}
loadAllRScripts <- function(dir = ".", recursive = FALSE, local = NULL){
    if(is.null(dir) || !dir.exists(dir)) return(TRUE)
    scripts <- list.files(dir, '\\.R$', full.names = TRUE, recursive = recursive)
    if(is.null(local)) local <- sessionEnv
    sourceFailure <- FALSE
    for(script in scripts) {
        if(!endsWith(script, '/global.R') && 
           !(dirname(script) %>% basename %>% startsWith("_")) &&
           !grepl('INLINE_ONLY', script, fixed = TRUE)) { # scripts intended to be sourced inline into other scripts
            # message(script)
            tryCatch({
                source(script, local = local)
            }, error = function(e){
                print(e)
                onScriptSourceError(script, local, e)
                sourceFailure <<- TRUE
            })
            if(sourceFailure) return(FALSE)
        }
    }
    return(TRUE)
}
loadAppScriptDirectory <- function(dir, local=NULL){
    success <- loadAllRScripts(dir, recursive = FALSE, local = local)
    if(!success) return(FALSE)
    for(subDir in c('classes', 'modules', 'types', 'ui', 'utilities')) {
        success <- loadAllRScripts(paste(dir, subDir, sep = '/'), recursive = TRUE, local = local)
        if(!success) return(FALSE)
    }
    TRUE
}
sessionEnv$sourceLoadType <- "framework"
initializeSessionSuccess <- loadAllRScripts('global', recursive = TRUE)
if(initializeSessionSuccess) initializeSessionSuccess <- loadAppScriptDirectory('session')
sessionEnv$sourceLoadType <- ""

# initialize git repository tracking
gitStatusData <- reactiveValues(
    app   = list(name = NULL, version = NULL),
    suite = list(name = NULL, dir = NULL, head = NULL),
    dependencies = list() # like suite, above, for each dependency suite
)

# activate our custom page reset action; reloads the page as is, to update all code
observeEvent(input$resetPage, {
    remoteKey <- getRemoteKeyQueryString()
    if(remoteKey != "") remoteKey <- paste0("&", remoteKey)
    updateQueryString(
        paste0("?resetPage=1", remoteKey), 
        mode = "push"
    )
    refresh()
})

# activate the mdiSharedEventHandler
# a way of receiving events from javascript when standard observe(input$x) fails, e.g., in some modals
mdiSharedEventHandlers <- list()
observeEvent(input$mdiSharedEventHandler, {
    x <- input$mdiSharedEventHandler
    req(x, is.list(x), x$key)
    fn <- mdiSharedEventHandlers[[x$key]]
    req(is.function(fn))
    fn(x$val)
}, ignoreInit = TRUE)
addMdiSharedEventHandler <- function(key, fn){
    mdiSharedEventHandlers[[key]] <<- fn
}
removeMdiSharedEventHandler <- function(key){
    mdiSharedEventHandlers[[key]] <<- NULL
}
