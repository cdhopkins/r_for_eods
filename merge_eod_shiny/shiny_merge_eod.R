#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require("shiny")
require("shinyFiles")
require("shinyalert")
require("reader") # to have get_delim
require("stringr") # regular expressions library




#GLOBAL VARIABLES

file_array<-c()
output_folder<-""
l_frames<-list()
eod_cluster<-NULL


append_metadata_batch<-function(new_file, target_folder)
{
  eod_cluster$createMergedFile(new_file, target_folder)
}


manage_batch_files<-function(file_array, target_folder)
{
    print("LOAD")
  print(file_array)
    eod_cluster<<-EodCluster$new(file_array$datapath, true_filenames=file_array$name)
    print(eod_cluster$getSize())
    print("DONE")
    #print(l_frames)
    l_files<-eod_cluster$getFilenameForMergedData()
    print(l_files)
    invisible(lapply(
      l_files,
      function(x)
      {
        new_file<-paste0(target_folder,"/",x)
        print(new_file)
        #print(l_frames[new_file])
        if(file.exists(new_file))
        {
          print("EXISTS")
          shinyalert(
            type = "input",
            inputId="shiny_file_confirm",
            inputType = "number",
            size = "l",
            inputValue=1,
            title=paste0("File exists\r\n", str_replace(new_file,target_folder,""), ".csv",
                         "\r\n 1 append or create \r\n 2 recreate \r\n 3 cancel"),
            callbackR= function(y)
            {
              print("CALLBACK")
              
              if(y==1)
              {
                print("update_file")
                #print(l_frames[new_file])
                append_metadata_batch(x, target_folder)
              }
              else if(y==2)
              {
                print("recreate_file")
                file.create(new_file)
                #print(l_frames[new_file])
                #print(l_frames[new_file])
                append_metadata_batch(x, target_folder)
                
              }
              else
              {
                print("cancel")
              }
              
            }
          )
        }
        else
        {
          print("create_file")
          file.create(new_file)
          #print(l_frames[new_file])
          append_metadata_batch(x,target_folder)
          
        }
        print("after_alert")
      }
      
    ))
    
}

#SHINY GRAPHICAL INTERFACE



# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyalert(),
    # Application title
    titlePanel("EOD matadata app"),

    # main panel 
    mainPanel(
            fileInput("eod_files", label= "Choose Mormyroscope files",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
            ),
            shinyDirButton('output_folder', title="merged_files_folder", label='Select a folder for the merged files')
            ,
            verbatimTextOutput("output_folder", placeholder = TRUE),
            actionButton("trigger_merge", "Merge files")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ##BLOCK TO MANAGE OUTPUT FILES
    observeEvent(input$eod_files,{
        file_array<<- input$eod_files
        
        #if (is.null(file_array))
        #    return(NULL)
        
        # Your code
       #print(file_array)
      
        
    })
    ###BLOCK TO SELECT THE OUTPUT FOLDER
    ##https://community.rstudio.com/t/shiny-directory-input/29160/2
    volumes = getVolumes()() 
    shinyDirChoose(
        input,
        'output_folder',
        roots = volumes
    )
    output_folder<<- getwd() 
    global <- reactiveValues(datapath = getwd())
    
    dir <- reactive(input$output_folder)
    
    output$output_folder <- renderText({
        global$datapath
    })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$output_folder
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- normalizePath("~")
                     global$datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 
                     output_folder<<- global$datapath    
                })
    ###
    ### Block handing button click
    observeEvent(input$trigger_merge,
                 {
                    #print("click")
                    #print(file_array)
                    if(length(file_array)>0 && nchar(output_folder)>0)
                    {
                        #print("go")
                        manage_batch_files(file_array,output_folder)
                    }
                 }
                 )
    

   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
