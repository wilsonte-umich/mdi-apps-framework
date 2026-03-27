#----------------------------------------------------------------------
# launch the MDI Stage 2 apps server
# sourced by mdi::run()
#----------------------------------------------------------------------
# thus, this script is re-sourced when MDI_FORCE_RESTART is set, followed
# by a call to stopApp() (stopApp() alone is not sufficient)
# if MDI_FORCE_REINSTALLATION is also set, git repos are pulled and R packages installed
#----------------------------------------------------------------------
# to summarize:
#   soft ui+server refresh  = triggered by page (re)load, (re)runs ui() and server() functions
#   hard ui+server refresh  = triggered by page (re)load, re-sources ui.r+server.R if changed, but not global.R
#   soft server restart     = triggered by stopApp(), re-sources everything except run_server.R <<<<< IMPACTS OTHER USERS <<<<< # nolint
#   hard server restart     = triggered by MDI_FORCE_RESTART+stopApp(), re-sources everything in apps framework
#   re-installation restart = triggered by MDI_FORCE_RESTART+MDI_FORCE_REINSTALLATION+stopApp(), also updates repos+packages # nolint
#----------------------------------------------------------------------
# the web server runs in .../mdi-apps-framework/shiny/shared
#----------------------------------------------------------------------
# message('--------- SOURCING run_server.R ---------')

# load environment variables
serverEnv <- as.list(Sys.getenv()) # thus, can access values as serverEnv$VARIABLE_NAME
setServerEnv <- function(name, default = NULL, type = as.character){
    serverEnv[[name]] <<- if(is.null(serverEnv[[name]])) {
        default
    } else {
        type(serverEnv[[name]])
    }
}

# adjust selected environment variables to logical
for(name in c('DEBUG', 'IS_DEVELOPER', 'IS_HOSTED', 'LAUNCH_BROWSER')) {
    serverEnv[[name]] <- as.logical(serverEnv[[name]])
}
if(serverEnv$IS_DEVELOPER) serverEnv$DEBUG <- TRUE

# set structured environment variables based on mode
serverEnv$IS_WINDOWS  <- .Platform$OS.type != "unix"
serverEnv$IS_LOCAL    <- serverEnv$SERVER_MODE == 'local'
serverEnv$IS_REMOTE   <- serverEnv$SERVER_MODE == 'remote'
serverEnv$IS_NODE     <- serverEnv$SERVER_MODE == 'node'
serverEnv$IS_ONDEMAND <- serverEnv$SERVER_MODE == 'ondemand'
serverEnv$IS_SERVER   <- serverEnv$SERVER_MODE == 'server'
serverEnv$IS_LOCAL_BROWSER <- !serverEnv$IS_ONDEMAND # here, the browser runs on the server
serverEnv$REQUIRES_AUTHENTICATION <- serverEnv$IS_SERVER # other users already validated themselves via SSH, etc.
checkMdiRemoteKey <- function(queryString){ # enforce single-user access when running remotely on a shared resource
    is.null(serverEnv$MDI_REMOTE_KEY) || # not a remote server
    (!is.null(queryString$mdiRemoteKey) && queryString$mdiRemoteKey == serverEnv$MDI_REMOTE_KEY)
}
getRemoteKeyQueryString <- function(){ # help assemble page reload URLs with remote keys
    if(is.null(serverEnv$MDI_REMOTE_KEY)) ""
    else paste0("mdiRemoteKey=", serverEnv$MDI_REMOTE_KEY)
}

# set the interface the server listens to; only select cases listen beyond localhost
if(!is.null(serverEnv$SERVER_PORT)) serverEnv$SERVER_PORT <- as.integer(serverEnv$SERVER_PORT)
setServerPort <- function(port) serverEnv$SERVER_PORT <<- port
serverEnv$HOST <- if(serverEnv$IS_LOCAL || serverEnv$IS_REMOTE || serverEnv$IS_ONDEMAND){
    "127.0.0.1"
} else if(serverEnv$IS_NODE || serverEnv$IS_SERVER) {
    "0.0.0.0"
} else {
    stop(paste('unknown server mode:', serverEnv$SERVER_MODE))    
}

# set properties based on whether server is publicly accessible or restricted access
if(serverEnv$IS_SERVER) { # public web server mode
    serverEnv$LAUNCH_BROWSER <- FALSE
    setServerEnv('MAX_MB_RAM_BEFORE_START', 6e3, as.integer)
    setServerEnv('MAX_MB_RAM_AFTER_END', 6e3, as.integer)
    serverEnv$CALLBACK_URL <- serverEnv$SERVER_URL
} else { # web server has highly restricted (often single-user) access
    setServerEnv('MAX_MB_RAM_BEFORE_START', 1e6, as.integer) # i.e. don't limit local start RAM
    setServerEnv('MAX_MB_RAM_AFTER_END', 6e3, as.integer)
    # serverEnv$SERVER_URL   <- paste0("http://localhost:", serverEnv$SERVER_PORT, "/") # cannot be 127.0.0.1 for Globus OAuth2 callback # nolint
    # serverEnv$CALLBACK_URL <- paste0("http://127.0.0.1:", serverEnv$SERVER_PORT, "/") # cannot be localhost for endpoint helper page action # nolint
} 

# set top-level directories based on whether we are running in a container and in developer mode
if(!dir.exists(serverEnv$MDI_DIR)) stop(paste('unknown directory:', serverEnv$MDI_DIR))
serverEnv$MDI_IS_CONTAINER <- !is.null(serverEnv$MDI_IS_CONTAINER) && serverEnv$MDI_IS_CONTAINER == "TRUE"
if(!serverEnv$MDI_IS_CONTAINER){
    serverEnv$ACTIVE_MDI_DIR <- serverEnv$MDI_DIR # values already set for containers
    serverEnv$STATIC_MDI_DIR <- serverEnv$MDI_DIR
}
serverEnv$APPS_FRAMEWORK_DIR <- if(serverEnv$IS_DEVELOPER){
    file.path(serverEnv$ACTIVE_MDI_DIR, 'frameworks', 'developer-forks', 'mdi-apps-framework')
} else { # use active directory even if not developer since might be more recent than a container static code copy
    file.path(serverEnv$ACTIVE_MDI_DIR, 'frameworks', 'definitive',      'mdi-apps-framework')
}
if(!dir.exists(serverEnv$APPS_FRAMEWORK_DIR)){ # in case developer doesn't have a fork of mdi-apps-framework
    serverEnv$APPS_FRAMEWORK_DIR <- file.path(serverEnv$ACTIVE_MDI_DIR, 'frameworks', 'definitive', 'mdi-apps-framework')
}

# set directories (framework runs from 'shared' directory that carries ui.R and server.R)
setServerDir <- function(name, parentDir, ..., check = TRUE, create = FALSE){
    serverEnv[[name]] <<- file.path(parentDir, ...)
    if(check  && !dir.exists(serverEnv[[name]])) stop(paste('missing directory:', serverEnv[[name]]))
    if(create && !dir.exists(serverEnv[[name]])) dir.create(serverEnv[[name]])
}
setServerDir('SHINY_DIR',       serverEnv$APPS_FRAMEWORK_DIR,   'shiny')
setServerDir('SHARED_DIR',      serverEnv$SHINY_DIR,            'shared')
setServerDir('BIN_DIR',         serverEnv$ACTIVE_MDI_DIR,       'bin')
setServerDir('CONFIG_DIR',      serverEnv$ACTIVE_MDI_DIR,       'config')
setServerDir('CONTAINERS_DIR',  serverEnv$ACTIVE_MDI_DIR,       'containers')
setServerDir('FRAMEWORKS_DIR',  serverEnv$ACTIVE_MDI_DIR,       'frameworks')
setServerDir('RESOURCES_DIR',   serverEnv$ACTIVE_MDI_DIR,       'resources')
setServerDir('SESSIONS_DIR',    serverEnv$ACTIVE_MDI_DIR,       'sessions')
setServerDir('SUITES_DIR',      serverEnv$ACTIVE_MDI_DIR,       'suites')
setServerDir('STORR_DIR',       serverEnv$DATA_DIR, 'storr',   check = FALSE, create = TRUE)
setServerDir('CACHE_DIR',       serverEnv$DATA_DIR, 'cache',   check = FALSE, create = TRUE)
setServerDir('UPLOADS_DIR',     serverEnv$DATA_DIR, 'uploads', check = FALSE, create = TRUE)
setwd(serverEnv$SHARED_DIR)

# declare version-specific R library(s) from which all packages are loaded
getLibPath <- function(lib) if(!is.null(lib) && lib != "") lib else NULL
.libPaths(c(
    serverEnv$LIBRARY_DIR_SHORT,
    serverEnv$LIBRARY_DIR,
    getLibPath(serverEnv$MDI_SYSTEM_R_LIBRARY)
))
if(serverEnv$DEBUG) message(paste(".libPaths =", .libPaths(), collapse = "\n"))

# set counters for sessions over the lifetime of this server
nServerSessions <- 0 # all sessions ever served since startup
nActiveServerSessions <- 0 # those sesssions that are currently running
serverId <- paste(Sys.time(), sample(1:1e8, 1)) # for keeping track of server instances in a database
if(serverEnv$DEBUG) message(paste('serverId', serverId))

# declare that we are the parent process ('future' child processes override this to FALSE)
isParentProcess <- TRUE

#----------------------------------------------------------------------
# server auto-restart loop when stopApp is called at session end, or upon config change
#----------------------------------------------------------------------
source(file.path('global', 'packages', 'packages.R'))
unloadMdiManagerPackages()
loadFrameworkPackages(runServerInitPackages, isInit = TRUE)
while(TRUE){
#----------------------------------------------------------------------

# load the Stage 2 apps config
serverConfig <- read_yaml(file.path(serverEnv$ACTIVE_MDI_DIR, 'config', 'stage2-apps.yml'))
if(is.null(serverConfig$site_name)) serverConfig$site_name <- 'MDI'

# determine whether the Pipeline Runner app is allowed
serverEnv$SUPPRESS_PIPELINE_RUNNER <- 
    serverEnv$IS_WINDOWS || # fail conditions that suppress Pipeline Runner
    serverEnv$IS_LOCAL || 
    is.null(serverConfig$pipeline_runner) ||         
    (is.logical(serverConfig$pipeline_runner) && !serverConfig$pipeline_runner) ||
    (is.character(serverConfig$pipeline_runner) && serverConfig$pipeline_runner != "auto") ||
    (is.character(serverConfig$pipeline_runner) && serverEnv$IS_SERVER)

# set max file upload size
if(is.null(serverConfig$max_upload_mb) || serverConfig$max_upload_mb == "auto"){
    serverConfig$max_upload_mb <- if(serverEnv$IS_SERVER) 5 else 200
}
options(shiny.maxRequestSize = serverConfig$max_upload_mb * 1024^2)

# create the persistent cache object shared across all sessions
# not for sensitive data in public server modes, etc.
assign("persistentCache", list(), .GlobalEnv)
if(is.null(serverConfig$default_ttl))     serverConfig$default_ttl <- 60 * 60 * 24 # i.e., 1 day
if(is.null(serverConfig$max_ttl))         serverConfig$max_ttl     <- 60 * 60 * 24
if(is.null(serverConfig$max_cache_bytes)) serverConfig$max_cache_bytes <- 1e9
serverConfig$default_ttl     <- eval(parse(text = serverConfig$default_ttl))
serverConfig$max_ttl         <- eval(parse(text = serverConfig$max_ttl))
serverConfig$max_cache_bytes <- eval(parse(text = serverConfig$max_cache_bytes))

# ensure that we have required server-level information for user authentication
serverEnv$IS_GLOBUS <- FALSE
serverEnv$IS_GOOGLE <- FALSE
serverEnv$IS_KEYED  <- FALSE
source(file.path('global', 'authentication', 'utilities.R')) 
source(file.path('global', 'authentication', 'sessionCache.R')) 
if(serverEnv$REQUIRES_AUTHENTICATION){
    if(is.null(serverConfig$access_control))
        stop("publicly addressable servers require an access_control declaration in config/stage2-apps.yml")
    else if(serverConfig$access_control == 'oauth2'){
        if(is.null(serverConfig$oauth2$host))
            stop("access_control mode 'oauth2' requires an oauth2$host declaration in config/stage2-apps.yml")
        if(!(serverConfig$oauth2$host %in% c('globus', 'google')))
            stop("unknown oauth2$host declaration in config/stage2-apps.yml; must be 'google' or 'globus'")
        if(is.null(serverConfig$oauth2$client$key) || 
           is.null(serverConfig$oauth2$client$secret))
            stop("invalid oauth2$client declaration in config/stage2-apps.yml; expect oauth2$client$key and oauth2$client$secret") # nolint
        serverEnv$IS_GLOBUS <- serverConfig$oauth2$host == 'globus'
        serverEnv$IS_GOOGLE <- serverConfig$oauth2$host == 'google'
    } else if(serverConfig$access_control == 'keys'){
        if(is.null(serverConfig$keys))
            stop("access_control mode 'keys' requires key declarations in config/stage2-apps.yml")
        serverEnv$IS_KEYED <- TRUE
    } else
        stop(paste("unknown access_control declaration:", serverConfig$access_control))
}
if(serverEnv$IS_GLOBUS || serverEnv$IS_GOOGLE) source(file.path('global', 'authentication', 'oauth2.R'))
if(serverEnv$IS_GLOBUS) source(file.path('global', 'authentication', 'globusAPI.R'))
if(serverEnv$IS_GOOGLE) source(file.path('global', 'authentication', 'googleAPI.R'))
if(serverEnv$IS_KEYED)  source(file.path('global', 'authentication', 'accessKey.R'))

# initialize storr key-value on-disk storage
serverEnv$STORR <- storr::storr_rds(serverEnv$STORR_DIR)

# initialize the server-level async monitor (persists between sessions and page reloads)
asyncTaskCounter <- 0
asyncTasks <- list()

# declare sessions directory and clear any prior user sessions
addResourcePath('sessions', serverEnv$SESSIONS_DIR) # for temporary session files
invisible(unlink(
    list.files(serverEnv$SESSIONS_DIR, full.names = TRUE, include.dirs = TRUE),
    recursive = TRUE,
    force = TRUE
))

# initialize git repository tracking
source(file.path('global', 'utilities', 'git.R'))
frameworkDir_ <- R.utils::getAbsolutePath(serverEnv$APPS_FRAMEWORK_DIR)
gitFrameworkStatus <- list(
    name = 'mdi-apps-framework',
    dir  = frameworkDir_,
    versions = getAllVersions(frameworkDir_)
)
gitFrameworkStatus$head <- getGitHead(gitFrameworkStatus)

# set the list of known apps (let Pipeline Runner load pipeline suites)
source(file.path('global', 'utilities', 'suites.R'))
appSuiteDirs <- getAppSuiteDirs()
appDirs <- getAppDirs(appSuiteDirs)
appUploadTypes <- getAppUploadTypes(appDirs) # uploadTypes recognized by installed apps; required prior to app load

# launch the Shiny app, a blocking action until/unless stopApp() is called
Sys.setenv(SHINY_SERVER_VERSION = '999.999.999') # suppress a baseless Shiny Server upgrade warning: https://rdrr.io/cran/shiny/src/R/runapp.R # nolint
# message('--------- CALLING run_server.R::runApp() ---------')
options(shiny.error = browser) # in non-interactive mode, prints extended error descriptions to console
runApp(
    appDir = '.',
    host = serverEnv$HOST,   
    port = serverEnv$SERVER_PORT, # on _first_ call, could be NULL for port auto-selection by Shiny
    launch.browser = serverEnv$LAUNCH_BROWSER
)

# check if framework has requested a hard server restart/reinstallation
# if not, loop will perform a soft restart by recalling runApp(), but not mdi::run()
if(Sys.getenv('MDI_FORCE_RESTART') != ""){
    install <- Sys.getenv('MDI_FORCE_REINSTALLATION') != ""
    suppressCheckout <- Sys.getenv('MDI_SUPPRESS_CHECKOUT') != ""
    Sys.setenv(MDI_FORCE_RESTART = "")
    Sys.setenv(MDI_FORCE_REINSTALLATION = "")
    Sys.setenv(MDI_SUPPRESS_CHECKOUT = "")
    mdi::run( # reinstalls and relaunches shiny server entirely anew (NB: can't use parallel::mcfork on Windows)
        mdiDir  = serverEnv$MDI_DIR,
        dataDir = serverEnv$DATA_DIR,
        hostDir = if(is.null(serverEnv$HOST_DIR) || 
                     serverEnv$HOST_DIR == "NULL" || 
                     serverEnv$HOST_DIR == "") NULL else serverEnv$HOST_DIR,  
        mode = serverEnv$SERVER_MODE,   
        install = install, 
        url = serverEnv$SERVER_URL,
        port = if(is.null(serverEnv$SERVER_PORT)) NULL # expect this to be set by server.R
               else as.integer(serverEnv$SERVER_PORT),
        browser = as.logical(serverEnv$LAUNCH_BROWSER),
        debug = as.logical(serverEnv$DEBUG),
        developer = as.logical(serverEnv$IS_DEVELOPER),
        checkout = if(suppressCheckout) FALSE else NULL # MDI_SUPPRESS_CHECKOUT set by gitManager Checkout
    )
    stop("no")
}

#----------------------------------------------------------------------
# end auto-restart loop
}
#----------------------------------------------------------------------
