#----------------------------------------------------------------------
# reactive components for populating a command terminal emulator dialog
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
commandTerminalServer <- function( # generally, you do not call commandTerminalServer directly
    id,                            # see showCommandTerminal()
    host = NULL,        # the host to ssh into when running terminal commands
    pipeline = NULL,    # as used in 'mdi <pipeline> shell --action <action> --runtime <runtime>''
    action = NULL,      #   to execute commands in a pipeline action's environment
    runtime = NULL,
    dir = NULL,         # the directory in which to open the terminal
    results = "",       # starting contents of the command results pane
    tall = FALSE,       # whether the dialog is currently extra-large (xl)
    wide = FALSE,
    onExit = NULL    
) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------
if(serverEnv$IS_SERVER) return(NULL)
user <- headerStatusData$userDisplayName
req(user)

#----------------------------------------------------------------------
# initialize the terminal
#----------------------------------------------------------------------
module <- "commandTerminal"
chooseDirId <- "chooseDir"
spinnerSelector <- "#commandTerminalSpinner"
prefix <- session$ns("") # for passing to javascript
observers <- list() # for module self-destruction
workingDir <- reactiveVal(dir)
defaultTimeout <- 10
timeout_ <- reactive({ # since we have no way to pass SIGINT to synchronous system()
    x <- input$timeout
    if(is.null(x)) return(defaultTimeout)
    x <- trimws(x)
    if(x == "" || grepl("\\S", x)) return(defaultTimeout)
    as.integer(x)
})

#----------------------------------------------------------------------
# initialize the command prompt
#----------------------------------------------------------------------
output$prompt <- renderUI({
    domain <- if(is.null(host)) serverEnv$MDI_REMOTE_DOMAIN else host
    dir <- workingDir()
    prompt <- tags$span(if(is.null(domain)) user else paste(user, domain, sep = "@"), style = "color: #500;")
    prompt <- if(is.null(dir)) prompt else paste(prompt, tags$span(resolveActiveMdiDir(dir), style = "color: #00a;"), sep = " ")
    tagList(
        HTML(paste0("[", prompt, "]", "$")),
        serverChooseDirIconUI(session$ns(chooseDirId), class = "mdi-dir-icon")
    )
})
serverChooseDirIconServer(
    chooseDirId, 
    input, 
    session,
    chooseFn = function(dir) changeTerminalDirectory(dir$dir, workingDir, prefix)
)

#----------------------------------------------------------------------
# initialize the command runtime
#----------------------------------------------------------------------
isRuntime <- !is.null(pipeline) && !is.null(action) && !is.null(runtime)
runtimeCommand <- " "  
runtimePrompt <- "$"
if(isRuntime) observers$runtime <- observeEvent(input$runtime, {
    if(input$runtime){
        runtimeCommand <<- {
            mdiCommandTarget <- file.path(serverEnv$ACTIVE_MDI_DIR, 'mdi')
            developerFlag <- if(serverEnv$IS_DEVELOPER) '-d' else ''
            paste(mdiCommandTarget, developerFlag, pipeline, "shell", "--action", action, "--runtime", runtime, '')
        } 
        runtimePrompt <<- paste0("[", pipeline, " ", action, "]", "$")
    } else {
        runtimeCommand <<- " "  
        runtimePrompt <<- "$"
    }
})

#----------------------------------------------------------------------
# execute the command in response to enter key or Execute button click
#----------------------------------------------------------------------
doCommand <- reactiveVal(0)
observers$commandEnterKey <- observeEvent(input$commandEnterKey, {
    doCommand(doCommand() + 1) 
})
observers$execute <- observeEvent(input$execute, {
    doCommand(doCommand() + 1) 
})
observers$doCommand <- observeEvent(doCommand(), {
    req(doCommand() > 0)
    req(input$command)
    dir <- workingDir()
    req(dir)
    command <- interceptTerminalCommands(input$command, workingDir = workingDir, 
                                         prefix = prefix, onExit = onExit)
    req(command)
    shinyjs::show(selector = spinnerSelector)  
    systemCommand <- if(is.null(host)) { # enable terminal to work on login host (the default) or a node
        paste0(                "cd '", dir, "'; ", runtimeCommand, command, " 2>&1")
    } else {
        paste0("ssh ", host, " 'cd ",  dir, "; ",  runtimeCommand, command, " 2>&1", "'")
    }
    if(serverEnv$IS_WINDOWS) { # convert Windows to Linux compatible; requires Git Bash
        drive <- strsplit(dir, "")[[1]][1]
        systemCommand <- gsub(
            paste0(drive, ":/"), # convert "C:/" to "/mnt/c/", et.
            paste0("/mnt/", tolower(drive), "/"),
            paste0('bash -c "', systemCommand, '"')
        )
    }
    x <- c( # execute and collect the command's output
        results(), 
        paste0('__LT__span style="color: #00a;">', paste(runtimePrompt, command), '__LT__/span>'),
        tryCatch({
            system(systemCommand, intern = TRUE, timeout = timeout_())
        }, warning = function(w){ # when command executes but it reports an error
            if(grepl("timed out after", w)) paste("command timed out after", timeout_(), "seconds")
            else system(paste(systemCommand), intern = TRUE)
        }, error = function(e){ # when the command could not be executed and the system reports an error
            reportProgress(e$message)
            paste(
                e$message,
                "unrecognized or malformed command",
                "could not be executed on the server operating system",
                sep = "\n"
            )
        }) 
    )
    shinyjs::hide(selector = spinnerSelector)
    addCommandToHistory(prefix)
    results(x)
})

#----------------------------------------------------------------------
# display the command results in a concatenated pseudo-stream
#----------------------------------------------------------------------
results <- reactiveVal(results)
observers$results <- observeEvent(results(), {
    results <- results()
    results <- if(length(results) == 1) NULL 
    else paste(results[2:length(results)], collapse = "\n")
    html("results", html = gsub("__LT__", "<", gsub("<", "&lt;", results)))
    scrollCommandTerminalResults(prefix)
})
activateObserver <- observe({ # runs once after UI elements initialize
    results()
    runjs(paste0("activateCommandTerminalKeys('", prefix, "')"))
    scrollCommandTerminalResults(prefix) # unfortunately, this executes but something else steals focus after
    activateObserver$destroy()
})

#----------------------------------------------------------------------
# toggle the terminal dimensions
#----------------------------------------------------------------------
toggleSize <- function(){
    toggleClass(selector = ".modal-dialog", class = "modal-xl", condition = wide)
    toggleClass(selector = ".command-terminal", class = "command-terminal-xl", condition = tall)
}
observers$toggleWidth <- observeEvent(input$toggleWidth, { 
    wide <<- !wide
    toggleSize()
})
observers$toggleHeight <- observeEvent(input$toggleHeight, { 
    tall <<- !tall    
    toggleSize()
})

#----------------------------------------------------------------------
# clear the results window
#----------------------------------------------------------------------
observers$clear <- observeEvent(input$clear, { results("") })

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
list(
    observers = observers, # for use by destroyModuleObservers
    onDestroy = function() {
        list( # return the module's cached state object
            dir = workingDir(),
            results = results(),
            tall = tall,
            wide = wide
        )
    }
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
