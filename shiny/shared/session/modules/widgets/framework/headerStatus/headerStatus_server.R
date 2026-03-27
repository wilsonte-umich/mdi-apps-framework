#----------------------------------------------------------------------
# reactive components for feedback on current user and dataDir
# also OAuth2 logout when relevant
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
headerStatusServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
# output text
userDisplayName <- reactive({
    name <- headerStatusData$userDisplayName
    domain <- serverEnv$MDI_REMOTE_DOMAIN
    if(is.null(domain)) name else paste(name, domain, sep = "@")    
})
output$userDisplayName <- renderText({ 
    userDisplayName()
})
output$dataDir <- renderText({ 
    req(headerStatusData$userDisplayName)
    headerStatusData$dataDirDisplay
})    

# allow all users to view the site's code, and developers to edit
observeEvent(input$aceEditor, {
    showAceEditor(
        session,
        baseDirs = c(app$DIRECTORY, serverEnv$SHARED_DIR),
        editable = serverEnv$IS_DEVELOPER
    )  
})

# allow local or remote user to execute arbitrary commands in an R consol
# obviously must never be exposed on a public server
if(!serverEnv$IS_SERVER) observeEvent(input$rConsole, {
    req(!serverEnv$IS_SERVER)
    req(headerStatusData$userDisplayName)
    showRConsole(session)  
})

# allow local or remote user to execute arbitrary commands on the system
# obviously must never be exposed on a public server
if(!serverEnv$IS_SERVER) observeEvent(input$commandTerminal, {
    req(!serverEnv$IS_SERVER)
    req(headerStatusData$userDisplayName)
    showCommandTerminal(
        session,
        dir = serverEnv$ACTIVE_MDI_DIR
    )  
})

# allow local or remote user to unlock the MDI installation, i.e., all frameworks and suites
observeEvent(input$unlockAllRepos, {
    req(headerStatusData$userDisplayName)
    showUserDialog(
        "Unlock MDI Installation", 
        tags$p(paste("Please click OK to confirm that you wish to remove all framework and suite lock files from your local or remote MDI installation.")), # nolint
        tags$p(resolveActiveMdiDir(serverEnv$ACTIVE_MDI_DIR), style = "margin-left: 2em;"),
        tags$p("This action is usually only required if you experienced a fatal error during execution of an MDI command, e.g., in Pipeline Runner."), # nolint
        callback = function(...) {
            sapply(c(
                file.path(serverEnv$ACTIVE_MDI_DIR, "suites", "*.lock"),
                file.path(serverEnv$ACTIVE_MDI_DIR, "frameworks", "*.lock")
            ), unlink, force = TRUE)
        },
        size = "m", 
        type = 'okCancel'
    )   
})

# allow user to log out
observeEvent(input$logout, {
    req(headerStatusData$userDisplayName)
    config <- getOauth2Config()
    url <- parse_url(config$urls$logout)
    file.remove(sessionFile) # make sure we forget them too
    url$query <- list(
        client_id = serverConfig$oauth2$client$key, # don't redirect back to app, since it requires logging in again!
        redirect_uri = serverEnv$SERVER_URL,
        redirect_name = paste("Michigan Data Interface at", serverEnv$SERVER_URL)
    )
    runjs(paste0("window.location.href = '", build_url(url), "'"))
})

# allow user to change the dataDir; necessarily restarts the server
serverChooseDirIconServer(
    'changeDataDir', 
    input, 
    session,
    chooseFn = function(dir){
        req(dir)
        dir <- dir$dir
        req(dir)
        if(dir == serverEnv$DATA_DIR) return(NULL)
        if(endsWith(dir, 'mdi/data')){
            showUserDialog(
                "Server Restart Required", 
                tags$p(paste("The server must restart to change data directories.")),
                tags$p("Please reload a fresh web page to start a new session once the server restarts."),
                callback = function(...) {
                    serverEnv$DATA_DIR <<- dir
                    Sys.setenv(MDI_FORCE_RESTART = "TRUE")
                    stopApp()
                },
                size = "s", 
                type = 'okOnlyCallback', 
                footer = NULL, 
                easyClose = TRUE
            )            
        } else {
            showUserDialog(
                "Invalid Data Directory", 
                tags$p("Invalid data directory."), 
                tags$p(dir),
                tags$p("Please select a subdirectory named 'data' within a valid MDI installation directory."), 
                type = 'okOnly',
                size = "m"
            )
        }
    }
)

# allow authorized users to clean up old files from the data directory
if(getAuthorizationFlag('serverCleanup')) observeEvent(input$cleanDataDir, {
    showServerCleanup(session)  
})

# maintain a visual display of one or more asynchrous processes launched by user
createAsyncIcon <- function(taskCounter, class, link, style = ""){
    icon <- tags$i(
        class = paste(class, "fas header-large-icon header-async-monitor"),
        style = paste(style, "cursor: pointer; margin-right: 0.25em; font-size: 1.35em; color: #eee; padding: 0 5px;")
    )
    actionLink(
        session$ns(paste(link, taskCounter, sep = "_")), 
        icon,
        onclick = paste0("Shiny.setInputValue('headerStatus-asyncClick', '",  taskCounter, "', {priority: 'event'})")
    )
}
updateAsyncMonitor <- reactiveVal(0)
output$asyncMonitor <- renderUI({
    updateAsyncMonitor()
    lapply(names(asyncTasks), function(taskCounter){
        task <- asyncTasks[[taskCounter]]
        if(task$user != headerStatusData$userDisplayName) return("")        
        data <- task$reactiveVal()
             if(data$pending) createAsyncIcon(taskCounter, "fa-spinner fa-spin", "pending")
        else if(data$success) createAsyncIcon(taskCounter, "fa-check", "succeeded", "color: #0a0 !important;")
        else                  createAsyncIcon(taskCounter, "fa-times", "failed",    "color: #c00 !important;")
    })
}) 
workingAsyncId <- reactiveVal()
observeEvent(input$asyncClick, {
    workingAsyncId(input$asyncClick)
    task <- asyncTasks[[input$asyncClick]]
    data <- task$reactiveVal()
    showUserDialog(
        title = "Asynchronous Task Report",
        tags$p(tags$strong("Task:"), task$name),
        tags$p(
            tags$strong("Status:"),
                 if(data$pending) "Pending"
            else if(data$success) "Succeeded"
            else                  "Failed"
        ),
        if(!data$pending) tags$p(
            tags$strong("Results:"),
            tags$pre(if(data$success) data$value else data$message, style = "max-height: 500px;")
        ) else "",
        size = if(data$pending) "s" else "l",
        type = "custom",
        footer = tagList(
            bsButton(session$ns("dismissAsync"), "Keep Header Icon", style = "default"),            
            if(data$pending) "" else bsButton(session$ns("clearAsync"), "Clear Header Icon", style = "primary")
        )
    )
})
closeAsyncDialog <- function(clear, taskId = NULL){
    removeModal()
    if(clear){ # eventually, the user clears the icon from their header and from the server log
        if(is.null(taskId)) taskId <- workingAsyncId()
        asyncTasks[[taskId]] <<- NULL      
        updateAsyncMonitor(updateAsyncMonitor() + 1)
    }
}
observeEvent(input$dismissAsync, { closeAsyncDialog(FALSE) })
observeEvent(input$clearAsync,   { closeAsyncDialog(TRUE) })

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    initalizeAsyncTask = function(name, reactiveVal, autoClear = NULL){
        asyncTaskCounter <<- asyncTaskCounter + 1
        taskObserver <- observeEvent(reactiveVal(), {
            task <- reactiveVal()
            req(task)
            updateAsyncMonitor(updateAsyncMonitor() + 1)
            if(!task$pending) {
                if(!is.null(autoClear) && task$success) setTimeout(function(...){
                    closeAsyncDialog(TRUE, as.character(asyncTaskCounter)) 
                }, delay = autoClear)               
                taskObserver$destroy()
            }
        })
        asyncTasks[[as.character(asyncTaskCounter)]] <<- list(
            user = headerStatusData$userDisplayName,
            name = name,
            reactiveVal = reactiveVal
        )
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
