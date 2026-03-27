#----------------------------------------------------------------------
# reactive components to launch and monitor pipeline jobs
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
runJobServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
        module <- 'runJob' # for reportProgress tracing
#----------------------------------------------------------------------
if(serverEnv$SUPPRESS_PIPELINE_RUNNER) return(NULL)

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
jobFiles <- selectJobFilesServer(
    id = 'jobFiles',
    parentId = id,
    parentOptions = options
)
addPRDocs('docs', "docs/server-deployment/pipeline-runner", "execute-and-monitor-jobs")

#----------------------------------------------------------------------
# conditional display elements dependent on an active job file selection
#----------------------------------------------------------------------
activeJobFile <- reactive({
    i <- jobFiles$selected()
    req(i)
    jobFiles$list[[i]]
})
observe({
    selectedRow <- jobFiles$selected()
    isSelection <- !is.null(selectedRow) && !is.na(selectedRow)
    shinyjs::toggle(
        selector = "span.requiresJobFile", 
        condition = isSelection
    )
    shinyjs::toggle(
        selector = "div.requiresJobFileMessage", 
        condition = !isSelection
    )
})

#----------------------------------------------------------------------
# generic handler for MDI job-manager commands
#----------------------------------------------------------------------
mdiOutput <- asyncDivServer('output', dataFn = doMdi, uiFn = fillPre, class = "command-output-wrapper",
                            async = TRUE, maskIds = session$ns("output-header"))
doMdi <- function(command, args, refreshable = TRUE, invalidateStatus = FALSE) {
    data <- runMdiCommand(args)
    list(
        command = if(is.null(data)) "" else command,
        args = args,
        data = data,
        refreshable = refreshable,
        invalidateStatus = invalidateStatus
    )
}
runJobManagerCommand <- function(command, jobId = NULL, 
                                 dryRun = TRUE, force = FALSE,
                                 errorString = 'mdi error:', options = "",
                                 refreshable = TRUE, invalidateStatus = FALSE){
    jobFile <- activeJobFile()
    req(jobFile)
    jobId  <- if(is.null(jobId)) ""  else c("--job", jobId)
    dryRun <- if(dryRun) "--dry-run" else ""
    force  <- if(force)  "--force"   else ""
    args <- c(command, jobId, dryRun, force, jobFile$path)
    mdiOutput$update(command = command, args = args, 
                     refreshable = refreshable, invalidateStatus = invalidateStatus)
}

#----------------------------------------------------------------------
# top-level action buttons for a selected job configuration file
#----------------------------------------------------------------------
# buttons to get ready to submit
observeEvent(input$inspect, {
    runJobManagerCommand('inspect', dryRun = FALSE) # inspect itself enforces --dry-run
})
observeEvent(input$mkdir, {
    runJobManagerCommand('mkdir', force = TRUE)
})
# dry-run job submission buttons (result panels reveal buttons for live execution)
observeEvent(input$submit, {
    runJobManagerCommand('submit', force = TRUE)
})
observeEvent(input$extend, {
    runJobManagerCommand('extend', force = TRUE)
})
# status history rollback and purging
observeEvent(input$rollback, {
    runJobManagerCommand('rollback', force = TRUE) 
})
observeEvent(input$purge, {
    runJobManagerCommand('purge', force = TRUE) 
})

#----------------------------------------------------------------------
# cascade to show the status of a selected job configuration file
#----------------------------------------------------------------------
nullStatusTable <- data.table(
    jobName = "no submitted jobs",
    jobID = "",
    array = "",
    start_time = "",
    exit_status = "",
    walltime = "",
    maxvmem = ""
)
invalidateStatus <- reactiveVal(0)
status <- asyncTableServer("status", function(jobFile){

    # call 'mdi status' to update status files, but use the files, not the mdi return value
    x <- runMdiCommand(c('status', jobFile$path), collapse = FALSE)
    if(!x$success) return(nullStatusTable)

    # recover the tab-delimited status text from disk
    dataDir <- paste0(".", jobFile$name, ".data")
    statusFile <- paste0(jobFile$name, ".status")
    statusFile <- file.path(jobFile$directory, dataDir, statusFile)
    if(!file.exists(statusFile)) return(nullStatusTable) 

    # parse the status file into a tab delimited table
    x <- strsplit(slurpFile(statusFile), "\n")[[1]]
    i1 <- which(startsWith(x, "jobName"))
    x <- paste(x[i1:length(x)], collapse = "\n")
    x <- fread(text = x)
    x[, array := sapply(array, function(d) if(is.na(d) || d == "") NA else {
        paste(range(as.integer(strsplit(d, ",")[[1]])), collapse = "-")
    })]
    x <- x[, .(
        jobName,
        jobID,
        array,
        start_time,
        exit_status,
        walltime,
        maxvmem
    )]  
    y <- data.table(
        delete = tableActionLinks(
            session$ns(deleteLinkId), 
            nrow(x), 
            'Delete', 
            allow = !isTerminated(x$exit_status)
        )  
    )  
    cbind(y, x)
}, options = list(
    paging = FALSE,      
    searching = FALSE            
))
statusTableObserver <- observeEvent({
    input$refreshStatus
    invalidateStatus()
    activeJobFile()
}, {
    jobFile <- activeJobFile()
    req(jobFile) 
    status$update(jobFile = jobFile, default = nullStatusTable)
})

#----------------------------------------------------------------------
# enable job deletion from within the status table
#----------------------------------------------------------------------
deleteLinkId <- 'deleteLink'
observeEvent(input[[deleteLinkId]], {
    statusTable <- status$tableData()
    req(statusTable)
    req(!isTerminated(statusTable$exit_status))
    row <- getTableActionLinkRow(input, deleteLinkId)
    jobName <- statusTable[row, jobName]
    jobId   <- statusTable[row, jobID]
    showUserDialog(
        "Confirm Job Deletion", 
        tags$p("Delete / kill / cancel this job from the cluster job queue?"),
        tags$p(
            tags$strong("Job Name: "), 
            jobName,
            style = "margin-left: 0.5em;"
        ),
        tags$p(
            tags$strong("Job ID: "), 
            jobId,
            style = "margin-left: 0.5em;"
        ),
        callback = function(parentInput) {
            runJobManagerCommand('delete', jobId = jobId, 
                                  dryRun = FALSE, force = TRUE,
                                  refreshable = FALSE, invalidateStatus = TRUE)
        },
        size = "s", 
        type = 'deleteCancel'
    )
})

#----------------------------------------------------------------------
# cascade to show the initial, complete log report of a selected job
#----------------------------------------------------------------------
selectedJobI <- status$table$selectionObserver
selectedJob <- reactive({
    rowI <- selectedJobI()
    req(rowI)
    status$tableData()[rowI, ]
})
selectedJobId <- reactive({
    job <- selectedJob()
    req(job)
    job[, jobID]
})
isTerminated <- Vectorize(function(exit_status) exit_status == "deleted" || check.numeric(exit_status))
isTerminatedJob <- reactive({
    job <- selectedJob()
    req(job)
    job[, isTerminated(exit_status)]
})
observeEvent(selectedJobI(), { 
    jobId <- selectedJobId()
    req(jobId)
    runJobManagerCommand('report', jobId = jobId, dryRun = FALSE)        
})
observeEvent(selectedJobI(), {
    jobI <- selectedJobI()
    html(
        id = session$ns("status-table-titleSuffix"), 
        asis = TRUE, 
        html = if(is.na(jobI)) "" else paste0(" - ", status$tableData()[jobI, jobName])
    )
})

#----------------------------------------------------------------------
# inputs panel to enable enhanced, task-level reporting
#----------------------------------------------------------------------
allTasks <- "all tasks"
taskSelector <- reactive({
    tasks <- selectedJob()[, array]
    if(is.na(tasks) || tasks == "") "" else tagList(
        column(
            width = 1,
            style = "padding-right: 0;",
            tags$strong("Task #", style = "float: right; margin-top: 6px;")
        ), 
        column(
            width = 2,
            selectInput(
                session$ns("taskNumber"), 
                NULL, 
                choices = c(
                    allTasks, 
                    as.character(1:max(as.integer(strsplit(tasks, "-")[[1]])))
                )
            )
        )
    )
})
reportButton <- function(){
    column(
        width = 2,
        bsButton(session$ns("report"), "Log Report", style = "default", width = "100%")
    )      
}
enableTaskLevelButtons <- reactive({
    tasks <- selectedJob()[, array]
    if(is.na(tasks)) return(TRUE) # non-array, single-task job
    req(input$taskNumber)
    req(input$taskNumber != allTasks)    
})
output$ls_ <- renderUI({
    req(enableTaskLevelButtons())
    column(
        width = 2,
        bsButton(session$ns("ls"), "List Output Files", style = "default", width = "100%")
    )
})
output$top_ <- renderUI({
    req(enableTaskLevelButtons())
    req(!isTerminatedJob())
    column(
        width = 2,
        bsButton(session$ns("top"), "Process Metrics", style = "default", width = "100%")
    )
})
output$taskOptions <- renderUI({
    job <- selectedJob()
    req(job)
    req(job[, jobName != "no submitted jobs"])
    fluidRow(
        style = "margin-top: 0.5em;",
        box(
            width = 12,
            status = 'primary',
            solidHeader = FALSE,
            style = "padding: 10px 0 10px 15px;",
            fluidRow(
                taskSelector(),
                reportButton(),
                uiOutput(session$ns('ls_')),
                uiOutput(session$ns('top_'))
            )
        )
    )        
})

# ----------------------------------------------------------------------
# task-level reporting/monitoring actions
# ----------------------------------------------------------------------
expandedJobId <- reactiveVal(NULL)
taskNumber <- reactive({
    jobId <- selectedJobId()
    req(jobId)
    tasks <- selectedJob()[, array]
    taskNumber <- if(
        is.null(tasks) || 
        is.na(tasks) || 
        tasks == "" || 
        input$taskNumber == allTasks
    ) NA else as.integer(input$taskNumber)
})
runTaskLevelCommand <- function(command, fn = NULL){
    jobId <- selectedJobId()
    req(jobId)
    tasks <- selectedJob()[, array]
    taskNumber <- taskNumber()
    jobId <- paste0(jobId, if(is.null(taskNumber) || is.na(taskNumber)){
        ""
    } else {
        paste0("[", taskNumber, "]")
    })
    expandedJobId(jobId)
    runJobManagerCommand(command, jobId = jobId, dryRun = FALSE)  
}
observeEvent({ # update command output as selected task changes
    selectedJobId()
    input$taskNumber
}, {
    req(input$taskNumber)
    isAllTasks <- input$taskNumber == allTasks
    command <- mdiOutput$data()$command
    req(command)  
    command <- if(isAllTasks) "report" else command
    runTaskLevelCommand(command)
})
observeEvent(input$report, { runTaskLevelCommand('report') }) # respond to task button clicks
observeEvent(input$ls,     { runTaskLevelCommand('ls') })
observeEvent(input$top,    { runTaskLevelCommand('top') })

#----------------------------------------------------------------------
# render all command outputs
#----------------------------------------------------------------------
results <- reactive({ # command output as an array of lines
    data <- mdiOutput$data()$data
    req(data)
    req(data$results)
    strsplit(data$results, "\n")[[1]]
})
output$command <- renderText({ # show a simplified version of the command output being displayed
    x <- mdiOutput$data()$args
    req(x)  
    paste(sapply(c("mdi", x), basename), collapse = " ")
})
fillPre <- function(mdiOutputData){
    x <- mdiOutputData$data
    if(x$success && mdiOutputData$invalidateStatus) 
        invalidateStatus(isolate({ invalidateStatus() }) + 1)
    shinyjs::toggle(session$ns("refreshOutput"), asis = TRUE, condition = mdiOutputData$refreshable)
    tags$pre( 
        class = if(x$success) "" else "command-output-error",
        paste(x$results, collapse = "\n") 
    )
}

#----------------------------------------------------------------------
# enable output refresh icon link
#----------------------------------------------------------------------
observeEvent(input$refreshOutput, {
    x <- mdiOutput$data()
    req(x)  
    mdiOutput$update(command = x$command, args = x$args,
                     refreshable = x$refreshable, # must always be TRUE to get here
                     invalidateStatus = x$invalidateStatus)
})

#----------------------------------------------------------------------
# build all required conda environments and/or download Singularity containers
#----------------------------------------------------------------------
missingCondaPipelines <- reactiveVal(NULL)
missingImages <- reactiveVal(list())
buildEnvironmentButton <- function(){ # must come before executeButtonMetadata
    results <- results()
    req(results)
    blockStarts <- which(results == "---") # boundaries of YAML blocks, a.k.a documents
    blockEnds   <- which(results == "...")
    missingCondaPipelines_ <- list()
    missingImages_ <- list()
    for(i in seq_along(blockStarts)){
        yaml <- read_yaml(text = paste0(results[blockStarts[i]:blockEnds[i]], collapse = "\n"))
        action <- yaml$execute
        if(is.null(action)) next # false for task and other non-job-level YAML blocks
        pipeline <- getShortPipelineName(yaml$pipeline)
        singularity <- yaml[[action]]$singularity
        if(TRUE || is.null(singularity)){
            conda <- strsplit(yaml[[action]]$conda$prefix, "\\s+")[[1]][1]
            if(!dir.exists(conda)) missingCondaPipelines_[[pipeline]] <- pipeline
        } else { # both the host system and the pipeline support containers and runtime = auto or container
            image <- rev(strsplit(singularity$image, "/")[[1]])
            imageFile <- file.path(serverEnv$ACTIVE_MDI_DIR, "containers", image[2], pipeline, paste0(sub(":v", "-v", image[1]), ".sif"))
            if(!file.exists(imageFile)) missingImages_[[singularity$image]] <- pipeline
        }
    }       
    missingCondaPipelines(missingCondaPipelines_)
    missingImages(missingImages_)
    if(length(missingCondaPipelines_) > 0 || length(missingImages_) > 0){
        bsButton(session$ns('environments'), "Build/Download Environment(s)", style = "primary", width = "100%")
    } else ""
}
buildEnvironments <- function(jobFile, missingCondaPipelines, missingImages){
    results <- c()
    if(length(missingCondaPipelines) > 0) for(pipeline in unique(unlist(missingCondaPipelines))){
        results <- c(results, runMdiCommand(args = c(pipeline, "conda", "--create", "--force"))$results)
    }
    if(length(missingImages) > 0) for(pipeline in unique(unlist(missingImages))){
        results <- c(results, runMdiCommand(args = c(pipeline, "checkContainer", jobFile))$results)
    }
    paste(results, collapse = "\n\n")
}
observeEvent(input$environments, {
    jobFile <- activeJobFile()
    missingCondaPipelines <- missingCondaPipelines()
    missingImages <- missingImages()
    showUserDialog(
        title = "Confirm Asynchronous Build",
        tags$p("The following execution environments will be built or downloaded asynchronously.",
               "You may keep working until the process completes and then submit your jobs."),
        if(length(missingCondaPipelines) > 0) tagList(
            tags$p(tags$strong("Conda Environments"), style = "margin-bottom: 0;"),
            lapply(names(missingCondaPipelines), tags$p, style = "padding-left: 2em;")
        ) else "",
        if(length(missingImages) > 0) tagList(
            tags$p(tags$strong("Singularity Containers"), style = "margin-bottom: 0;"),
            lapply(names(missingImages), tags$p, style = "padding-left: 2em;")
        ) else "",
        callback = function(...) mdi_async(
            buildEnvironments,
            reactiveVal(""), # a new reactive for this task  
            name = "buildEnvironments",
            header = TRUE,
            jobFile = jobFile,
            missingCondaPipelines = missingCondaPipelines,
            missingImages = missingImages
        ),
        size = "m"
    )
})

#----------------------------------------------------------------------
# download a data package from a job for use in Stage 2 apps
#----------------------------------------------------------------------
packageFile <- reactiveVal(NULL)
downloadPackageButton <- function(){ # must come before executeButtonMetadata
    packageFile(NULL)
    results <- results()
    req(results)
    is <- which(grepl("writing Stage 2 package file", results))
    req(is)
    i <- max(is)
    req(i)
    i <- i + 1 # this line carries the name of the most recently written data package.zip
    packageFile(results[i])
    downloadButton(session$ns('download'), label = "Download Package", 
                   icon = NULL, width = "100%", # style mimics bsButton style=primary
                   style = "color: white; background-color: #3c8dbc; width: 100%; border-radius: 3px; float: right;")
}
output$download <- downloadHandler(
    filename = function() basename(packageFile()),
    content  = function(tmpFile) file.copy(packageFile(), tmpFile)
)

#----------------------------------------------------------------------
# open a terminal emulator in an output directory ...
#----------------------------------------------------------------------
# ... one the server/login node
showOutputDirTerminal <- function(ssh = FALSE){ # must come before executeButtonMetadata

    # use the job's report to learn how to load the terminal
    report <- parsePipelineJobReport(expandedJobId(), activeJobFile())   
    req(report)
    dir <- file.path(
        getTaskOptions(report, 'output', 'output-dir'), 
        getTaskOptions(report, 'output', 'data-name')
    )
    req(dir)
    req(dir.exists(dir))

    # set the remote host for jobs that are still running
    if(ssh){
        host <- report$jobManager$host
        req(host)
    } else {
        host <- NULL
    }

    # open the terminal with a runtime environment that is valid if job has finished
    showCommandTerminal(
        session,
        host = host,
        pipeline = getShortPipelineName(report$options$pipeline),
        action = report$options$execute,
        runtime = getTaskOptions(report, 'resources', 'runtime'),
        dir = dir,
        forceDir = TRUE
    )
}
# ... on the node running the task
showNodeDirTerminal <- function(){ # must come before executeButtonMetadata
    showOutputDirTerminal(ssh = TRUE)
}

#----------------------------------------------------------------------
# enable final execution, i.e., after (from within) a --dry-run display
#----------------------------------------------------------------------
executeButtonMetadata <- list(
    inspect = list(
        button = buildEnvironmentButton,
        execute = function(...) NULL
    ),
    mkdir = list(
        label = "Make Directory", 
        style = "primary", 
        suppressIf = "all output directories already exist",
        invalidateStatus = FALSE
    ),
    submit = list(
        label = "Execute Submit",         
        style = "success",
        invalidateStatus = TRUE
    ),
    extend = list(
        label = "Execute Extend",         
        style = "success",
        invalidateStatus = TRUE
    ),
    rollback = list(
        label = "Execute Rollback",       
        style = "danger",
        invalidateStatus = TRUE
    ),
    purge = list(
        label = "Execute Purge",          
        style = "danger",
        invalidateStatus = TRUE
    ),
    report = list(
        button = downloadPackageButton,
        execute = function(...) NULL
    ),
    ls = list(
        label = "Open in Terminal",          
        style = "default",
        execute = showOutputDirTerminal
    ),
    top = list(
        label = "Open Node in Terminal",          
        style = "default",
        execute = showNodeDirTerminal
    )
)
output$executeButton <- renderUI({ # render the Execute ... button
    outputData <- mdiOutput$data()
    req(outputData)
    command <- outputData$command
    req(command)
    button <- executeButtonMetadata[[command]] 
    req(button)
    if(!is.null(button$button)) return(button$button()) # handle button overrides
    if(!is.null(button$suppressIf)){ # handle conditional button display
        data <- outputData$data
        req(data)
        results <- data$results
        req(results)
        if(any(grepl(button$suppressIf, results))) return(NULL)
    }
    bsButton(session$ns('execute'), button$label, style = button$style, width = "100%")
})
observeEvent(input$execute, { # act on the Execute button click
    outputData <- mdiOutput$data()
    req(outputData)
    command <- outputData$command
    req(command)
    button <- executeButtonMetadata[[command]] 
    if(!is.null(button$execute)) return(button$execute()) # handle button overrides 
    args <- outputData$args
    req(args)
    args <- args[args != "--dry-run"]
    mdiOutput$update(command = command, args = args,
                     refreshable = FALSE, # Execute actions are terminal commands
                     invalidateStatus = button$invalidateStatus)
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
# observe({
#     bm <- getModuleBookmark(id, module, bookmark, locks)
#     req(bm)
# })

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    output = list()
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
