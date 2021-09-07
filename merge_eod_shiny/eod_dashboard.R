#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
require("shinyalert")
source("eod_class.R")
library(ggplot2)
library(gridExtra)
library(DT)


file_array<-c()
output_folder<-""
l_frames<-list()
eod_cluster<-NULL
loaded<-FALSE
current_eod_idx<-NULL
updated_files<-c()

display_plots<-function()
{
    print("__DISPLAY__")
    max_size<-eod_cluster$getSize()
    print(max_size) 
    list_chart <- list()
    
            for(i in 1:max_size)
            {
                tmp_eod<-eod_cluster$getEODS(i)
                tmp_eod$getPossibleBaseline()
                list_chart[[i]]<-tmp_eod$getMainPlot()
            }
           
      list_chart  
    
}

append_metadata_batch<-function(new_file, target_folder)
{
    eod_cluster$createMergedFile(new_file, target_folder)
}

manage_cluster<-function(file_array, output,session)
{

  eod_cluster<<-EodCluster$new(cluster_files=file_array$datapath, true_filenames=file_array$name)
  output$summary<-renderText(paste0("1","/", eod_cluster$getSize(), " file(s)" ))
  
 
  i<-1
  tmp_choices<-list()
  lapply(eod_cluster$getOriginalFiles(),
                       function(x)
                       {
                         print("x=")
                         print(x)
                         tmp_choices[[i]]<<-paste0(i, " ", x)
                         i<<-i+1
                       })
  print(tmp_choices)
  updateSelectInput(session, "select_eod",
                   
                    choices = setNames(seq(1,length(eod_cluster$getOriginalFiles())),
                                      tmp_choices)
                    
  )
  loaded<<-TRUE
}

inverse_phase<-function( output,session)
{
  if(!is.null(eod_cluster)&&loaded)
  {
    print(current_eod_idx)
    current_eod<-eod_cluster$getAllEods()[[as.numeric(current_eod_idx)]]
    print(typeof(current_eod))
    current_eod$inversePhase()
    current_eod$getPossibleBaseline()
    current_chart<-current_eod$getMainPlot()
    output$plot_eods<-renderPlot(current_chart)
    output$summary_data<-renderTable(current_eod$getMetadata())
    modified_file<-eod_cluster$getUploadedFile(as.numeric(current_eod_idx))
    print("MOD")
    print(modified_file)
    if(! modified_file %in% updated_files)
    {
      updated_files<<-c(updated_files,modified_file)
    }
  }
}


save_update<-function(file_array, output_folder, output,session)
{
  if(!is.null(eod_cluster)&&loaded)
  {
    print(updated_files)
    sapply(updated_files, 
           function(x)
           {
             eod_cluster$updateCluster(output_folder, x)
           }
          )
    updated_files<<-c()
  }
}
redraw_plot<-function(i, output,session)
{
  if(!is.null(eod_cluster)&&loaded)
  {
    current_eod_idx<<-i
    tmp_eod<-eod_cluster$getEODS(as.numeric(i))
    tmp_eod$getPossibleBaseline()
    current_chart<-tmp_eod$getMainPlot()
    output$plot_eods<-renderPlot(current_chart)
    output$summary_data<-renderTable(tmp_eod$getMetadata())
  }
}

manage_batch_files<-function(file_array, target_folder, output)
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

# Define UI for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "EOD dashboard"),
    dashboardSidebar(
        
        fileInput("eod_files", label= "Choose Mormyroscope files",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        textInput("output_folder", label="output  folder", value=getwd()),
        actionButton("change_output_folder", "Change output folder"),
        actionButton("trigger_merge", "Merge files"),
        actionButton("load_cluster", "Load cluster")
        
    ),
    dashboardBody(useShinyalert(),
                  fluidRow(
                    textOutput("summary"),
                      box(
                        
                        plotOutput("plot_eods")
                      ),
                    actionButton("reverse_phase", "Revert phase"),
                    actionButton("save_update", "Save updates"),
                      selectInput("select_eod", label="Current EOD", choices=c())
                    ),
                    htmlOutput("summary_data"),
                  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ##BLOCK TO MANAGE OUTPUT FILES
    observeEvent(input$eod_files,{
        file_array<<- input$eod_files
        print(input$eod_files$datapath)
        #if (is.null(file_array))
        #    return(NULL)
        
        # Your code
        #print(file_array)
        
        
    })
    
    observeEvent(input$change_output_folder,
                 {
                     tmp<-choose.dir(default=getwd(), caption="Select folder for EODs")
                     updateTextInput(session, "output_folder", value=tmp)
                 }
    )
    
    ###
    ### Block handling button click
    observeEvent(input$trigger_merge,
                 {
                     #print("click")
                     #print(file_array)
                     
                     output_folder <<-input$output_folder
                     if(length(file_array)>0 && nchar(output_folder)>0)
                     {
                         #print("go")
                         manage_batch_files(file_array,output_folder, output)
                     }
                 }
    )
    
    observeEvent(input$load_cluster,
                 {
                    print("read merge")
                    manage_cluster(input$eod_files,output, session)
                 })
    
    observeEvent(input$select_eod,
                  {
                    redraw_plot(input$select_eod,output, session)
                  })
    observeEvent(input$reverse_phase,
                 {
                   inverse_phase(output, session)
                 })
    
    observeEvent(input$save_update,
                 {
                   output_folder <<-input$output_folder
                   if(length(file_array)>0 && nchar(output_folder)>0)
                   {
                    save_update(file_array, output_folder, output, session)
                   }
                 })
}

# Run the application 
shinyApp(ui = ui, server = server)
