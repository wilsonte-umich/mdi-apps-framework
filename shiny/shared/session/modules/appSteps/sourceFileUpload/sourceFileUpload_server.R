#----------------------------------------------------------------------
# reactive components to upload additional sample data files and rename samples
# this is typically the first module of all apps
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sourceFileUploadServer <- function(id, options, bookmark, locks) { 
    moduleServer(id, function(input, output, session) {    
#----------------------------------------------------------------------
module <- 'sourceFileUpload' # for reportProgress tracing
if(serverEnv$IS_DEVELOPER) activateMdiHeaderLinks(
    session,
    url = getDocumentationUrl("shiny/shared/session/modules/appSteps/sourceFileUpload/README", 
                              framework = TRUE),
    baseDirs = getAppStepDir(module),
    envir = environment()
)

#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------
sourceFileInput  <- sourceFileInputServer('fileInput', appName = app$config$name)
output$lastLoadedBookmark <- renderText({ lastLoadedBookmark() })
cft <- CONSTANTS$contentFileTypes
manifestFileType <- cft$manifestFile
qcReportFileType <- cft$qcReport
statusFileType   <- cft$statusFile

# initialize the analysis set (parent table)
sourceSummaryTemplate <- data.frame(
    Remove      = character(),
    FileName    = character(),    
    Project     = character(),
    N_Samples   = integer(),
    Avg_Yield   = integer(),
    Avg_Quality = numeric(),
    QC_Report   = character(),
        stringsAsFactors = FALSE
)
sources <- summaryTableServer(
    id = 'sources', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sourceFileInput$sendFeedback,
    template = sourceSummaryTemplate,
    type = 'shortList',
    remove = list(
        message = "Remove this sample source and all of its samples?",
        name = 'fileName'
    )
)

# initialize samples (child table)
sampleTableTemplate <- data.frame(
    Source_ID   = character(),
    Project     = character(),
    Sample_ID   = character(),
    Description = character(),
    Yield       = integer(),
    Quality     = numeric(),    
        stringsAsFactors = FALSE
)
sampleSummaryTemplate <- data.frame(
    Name         = character(),
    Project      = character(), 
    Sample_ID    = character(),
    Description  = character(),
        stringsAsFactors = FALSE
)
samples <- summaryTableServer(
    id = 'samples', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sourceFileInput$sendFeedback,
    template = sampleSummaryTemplate,
    type = 'longList100',
    names = list(
        get = getSampleNames,
        source = id
    ),
    parent = list( # enable filtering based on selected sample source 
        keyColumn = "Source_ID",
        table = sources
    )
)

#----------------------------------------------------------------------
# load an incoming data source file (either via launch page or app step 1)
#----------------------------------------------------------------------
loadSourceFile <- function(incomingFile, suppressUnlink = FALSE){
    if(is.null(incomingFile) || length(incomingFile$name) == 0) return(NULL)
    reportProgress('loadSourceFile', module)
    reportProgress(incomingFile$path, module)
    startSpinner(session, 'loadSourceFile')
    sourceType <- incomingFile$type
    sft <- CONSTANTS$sourceFileTypes        
    sourceId <- if(sourceType == sft$priorPackage) incomingFile$sourceId
                else tools::md5sum(incomingFile$path) # treat md5 sums as "effectively unique" identifiers
    loaded <- if(sourceType == sft$package)      loadPackageFile (incomingFile$path, sourceId, incomingFile$name, suppressUnlink) # nolint
         else if(sourceType == sft$manifest)     loadManifestFile(incomingFile$path, sourceId, suppressUnlink)
         else if(sourceType == sft$dataTable)    loadDataTable   (incomingFile$path, sourceId, suppressUnlink) # nolint
         else if(sourceType == sft$priorPackage) loadPriorPackage(incomingFile$path, sourceId, incomingFile$name, suppressUnlink)
    if(is.null(suppressUnlink) || !suppressUnlink) unlink(incomingFile$path)    
    sources$list[[sourceId]] <- c(
        loaded,
        list(
            sourceType = sourceType,
            fileName = incomingFile$name
        )
    )
    stopSpinner(session, 'loadSourceFile')
    sourceFileInput$sendFeedback(paste(loaded$nSamples, "sample(s) loaded"))    
}
# validate and merge an _additional_ source data file uploaded by user via step 1 (not via the launch page)
observeEvent(sourceFileInput$file(), {
    x <- sourceFileInput$file()
    req(x)
    type <- x$type
    req(type)
    loadSourceFile(x, suppressUnlink = x$suppressUnlink)
})
badSourceFile <- function(filePath, msg="", suppressUnlink = FALSE){
    if(is.null(suppressUnlink) || !suppressUnlink) unlink(filePath)
    stopSpinner(session, '!!!! badSourceFile !!!!')    
    sourceFileInput$sendFeedback(paste("bad source file:", msg), isError = TRUE)    
}

#----------------------------------------------------------------------
# load an incoming Stage 1 pipeline output package file
#----------------------------------------------------------------------
getPackageManifest <- function(packagePath, packageId, packageFileName, suppressUnlink, packageConfig){
    manifestFile <- packageConfig$files[[manifestFileType]]
    manifest <- if(is.null(manifestFile)) {
        getNullManifest(packageFileName) # some pipeline outputs may not be sample-based
    } else {  
        if(is.null(manifestFile$manifestType))
            badSourceFile(packagePath, 'missing manifest type in package', suppressUnlink)
        if(is.null(manifestTypes[[manifestFile$manifestType]])) 
            badSourceFile(packagePath, 'unknown manifest type in package', suppressUnlink)
        manifestPath <- getPackageFileByName(packageId, manifestFile$file)
        parseManifestFile(manifestPath, manifestFile$manifestType, packagePath)
    }
}
loadPackageFile <- function(packagePath, packageId, packageFileName, suppressUnlink){ # packagePath validated upstream as package usable by app
    dataDir <- getPackageDir(packageId) # packageId is the package file md5sum

    # extract the contents declared to be in the package file
    packageConfig <- getPackageFileConfig(packagePath, sourceFileInput$sendFeedback) # i.e., package.yml
    if(is.null(packageConfig$uploadType)) badSourceFile(packagePath, msg = "missing upload type in package file", suppressUnlink)
    contentFileTypes <- app$config$uploadTypes[[ packageConfig$uploadType ]]$contentFileTypes
    # contentFileTypes[[manifestFileType]] <- list(required = TRUE)
    contentFileTypeNames <- names(contentFileTypes)
    packageFileTypeNames <- names(packageConfig$files)

    # check that all required package files are present (before extracting anything)
    for(contentFileTypeName in contentFileTypeNames){
        x <- contentFileTypes[[contentFileTypeName]]
        if(is.null(x$required) || !x$required) next # don't worry about optional files
        matchCount <- sum(contentFileTypeName == packageFileTypeNames)
        if(matchCount == 0) badSourceFile(packagePath, paste("missing file type in package:", contentFileTypeName), suppressUnlink)
    }

    # extract the files required by, or compatible with, the app
    unzip(packagePath, files = 'package.yml', exdir = dataDir)        
    for(contentFileTypeName in unique(c(contentFileTypeNames, statusFileType, qcReportFileType))){
        packageFile <- packageConfig$files[[contentFileTypeName]]
        if(is.null(packageFile)) next
        fileName <- packageFile$file
        tryCatch({
            unzip(packagePath, files = fileName, exdir = dataDir)        
        }, error = function(e) badSourceFile(packagePath, paste("could not extract file from package:", fileName), suppressUnlink) )
    } 

    # return our results
    c(
        getPackageManifest(packagePath, packageId, packageFileName, suppressUnlink, packageConfig), # load the sample manifest file, if present
        list(
            dataDir = dataDir,
            config = packageConfig 
        )
    )
}

#----------------------------------------------------------------------
# load an incoming sample manifest
#----------------------------------------------------------------------
loadManifestFile <- function(manifestPath, manifestId, suppressUnlink){
    # manifestType <- 'IlluminaDefault' # TODO: smarter manifest type declaration, guessing, user input?
    manifestType <- 'simple'
    manifest <- parseManifestFile(manifestPath, manifestType, suppressUnlink = suppressUnlink)
    c(
        manifest,
        list(
            config = {} # nolint
        )
    )
}
parseManifestFile <- function(manifestPath, manifestType, errorPath = NULL, suppressUnlink = FALSE){
    manifest <- tryCatch({
        x <- manifestTypes[[manifestType]]$load(manifestPath)
        manifestTypes[[manifestType]]$parse(x)
    }, error = function(e){
        print(e)
        if(is.null(errorPath)) errorPath <- manifestPath
        badSourceFile(errorPath, "could not parse manifest file", suppressUnlink)
    })
    for(col in c('Yield', 'Quality')) if(is.null(manifest$unique[[col]])) manifest$unique[[col]] <- NA
    list(
        manifestType = manifestType,        
        nSamples = nrow(manifest$unique),
        manifest = manifest$manifest,
        unique   = manifest$unique       
    )
}
getNullManifest <- function(fileName){
    manifest <- data.frame(
        Project = gsub(CONSTANTS$fileSuffixes$package, "", fileName), # required to parse project names list
        Sample_ID = NA,
        Description = NA,
        Yield = NA,
        Quality = NA
    )
    list(
        manifestType = NA,      
        nSamples = 0,
        manifest = manifest,
        unique   = manifest
    ) 
}

#----------------------------------------------------------------------
# load a previously loaded data package from priorPackages
#----------------------------------------------------------------------
loadPriorPackage <- function(packagePath, packageId, packageFileName, suppressUnlink){
    packageConfig <- read_yaml(packagePath) 
    c(
        getPackageManifest(packagePath, packageId, packageFileName, suppressUnlink, packageConfig),
        list(
            dataDir = dirname(packagePath),
            config = packageConfig
        )
    )
}

#----------------------------------------------------------------------
# load an incoming user-constructed data table
# TODO: implement this
#----------------------------------------------------------------------
loadDataTable <- function(dataTablePath, dataTableId, suppressUnlink){
    badSourceFile(dataTablePath, "data table loading not implemented yet", suppressUnlink)
}

#----------------------------------------------------------------------
# reactively update the aggregated sources and samples tables
#   the response to user action on archetypal pattern inputs
#----------------------------------------------------------------------
observe({
    reportProgress('observe sources$list', module)
    rs <- sourceSummaryTemplate
    ss <- sampleSummaryTemplate
    st <- sampleTableTemplate
    
    # fill the two tables by source
    nSources <- length(sources$list)
    if(nSources > 0) for(i in 1:nSources){ # whenever the active sources change
        sourceId <- names(sources$list)[i]
        reportProgress(sourceId)  
        startSpinner(session, message = "loading data sources")  
        source <- sources$list[[sourceId]]
        hasSamples <- source$nSamples > 0

        # save aggregated projects across the entire package
        qcReport <- source$config$files[[qcReportFileType]]
        qcReport <- if(!is.null(qcReport)) qcReport$file
        projectNames <- unique(source$unique$Project)
        rs <- rbind(rs, data.frame(
            Remove      = "",
            FileName    = source$fileName,
            Project     = if(length(projectNames) > 1) "various" else projectNames, 
            N_Samples   = source$nSamples,
            Avg_Yield   = if(hasSamples) round(mean(source$unique$Yield),   0) else "NA",
            Avq_Quality = if(hasSamples) round(mean(source$unique$Quality), 1) else "NA",
            QC_Report   = tableCellActionLinks(session$ns(qcReportParentId), i, qcReport),
                stringsAsFactors = FALSE
        ))

        # save samples twice, once for UI, once for sharing with other modules
        if(hasSamples) for(i in seq_len(nrow(source$unique))){ 
            sample <- source$unique[i, ]
            ss <- rbind(ss, data.frame(
                Name         = "",
                Project      = sample$Project,
                Sample_ID    = sample$Sample_ID,
                Description  = sample$Description,
                Yield        = sample$Yield,
                Quality      = sample$Quality,
                    stringsAsFactors = FALSE
            ))
            st <- rbind(st, data.frame(
                Source_ID   = sourceId,
                Project     = sample$Project,
                Sample_ID   = sample$Sample_ID,
                Description = sample$Description,
                    stringsAsFactors = FALSE
            ))
        }
    }    

    # update the UI reactives
    sources$summary <- rs
    samples$summary <- ss
    samples$list    <- st
    isolate({
        sources$ids <- names(sources$list)
        samples$ids <- apply(samples$list[, c('Project', 'Sample_ID')], 1, paste, collapse = ":") 
    })
    stopSpinner(session)
})

#----------------------------------------------------------------------
# show QC reports when available
#----------------------------------------------------------------------
qcReportParentId <- 'showQCReport'
observeEvent(input[[qcReportParentId]], {
    
    # get the target file
    startSpinner(session, 'input[[qcReportParentId]]')    
    reportProgress('input[[qcReportParentId]]', module)
    ij <- getTableActionLinkRowAndItem(input, qcReportParentId)
    source <- sources$list[[ ij[1] ]] # only package files have associated QC reports
    qcFile <- getPackageFileByType(source, qcReportFileType)

    # load into a large modal
    showHtmlModal(
        file  = qcFile$path,
        type  = qcReportFileType,
        title = paste(source$fileName, source$unique[1, 'Project'], sep = ' / ')
    )
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    req(checkBookmarkPackageExistence(bm$outcomes$sources)) # abort load if bookmark packages are missing
    updateTextInput(session, 'analysisSetName', value = bm$outcomes$analysisSetName)
    sources$list  <- bm$outcomes$sources
    samples$list  <- bm$outcomes$samples
    samples$names <- bm$outcomes$sampleNames
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
        analysisSetName = reactive(input$analysisSetName),
        sources         = reactive(sources$list),
        samples         = reactive(samples$list), # actually a data.frame
        sampleNames     = reactive(samples$names)        
    ),
    sourcesSummary = reactive(sources$summary),
    loadSourceFile = loadSourceFile,
    isReady = reactive({ getStepReadiness(options$source, samples$list) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
