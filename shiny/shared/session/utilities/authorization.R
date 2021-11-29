#----------------------------------------------------------------------
# enforce the app and file authorizations declared in 'config/stage2-app.yml'
# based on the session-level object 'authenticatedUserData'
#----------------------------------------------------------------------

# determine whether a user is allowed to load a specific app
isAuthorizedApp <- function(appName){
    auth <- authenticatedUserData$authorization
    if(is.null(auth) || is.null(auth$apps)) return( FALSE )
    if(length(auth$apps) == 1 && auth$apps == "all") return( TRUE )
    appName %in% auth$apps
}

# get the server file paths authorized to an authenticated user
getAuthorizedServerPaths <- function(rw = "read"){
    auth <- authenticatedUserData$authorization
    if(is.null(auth) || is.null(auth$paths) || is.null(auth$paths[[rw]])) return( character() )
    paths <- auth$paths[[rw]]
    if(length(paths) == 1 && paths == "all") paths <- names(serverConfig$paths)
    unlist(serverConfig$paths[paths])
}