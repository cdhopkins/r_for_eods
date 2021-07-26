require(reader)
require(ggplot2)
require(changepoint)
require("stringr") # regular expressions library



global_metadata_lines = 12
global_sampling_rate = 192000
global_bit_depth = 16
global_encoding="utf-8"
global_treshold <- 0.02
global_nb_peaks<-7
global_arbitrary_baseline <-40

global_baseline_done = FALSE
global_baseline_type="meanvar"

Eod <-setRefClass("Eod",
                  fields = list(
                    file = "character",
                    threshold= "numeric",
                    maxPeaks= "numeric",
                    specimenTag = "character",
                    provisionalId="character",
                    collectionEvent= "character",
                    recordingSoftware = "character",
                    recordingHardware = "character",
                    recordist = "character",
                    bitDepth = "numeric",
                    samplingRate = "numeric",
                    recordingDateTime="ANY",
                    recordingTemperature="numeric",
                    recordingConductivity="numeric",
                    projectName = "character", 
                    wave = "ANY",
                    metadata_lines = "numeric",
                    obj_creation_time="ANY",
                    file_metadata = "ANY",
                    encoding = "character",
                    file_wave="ANY",
                    decimal_sep = "character",
                    specimenIdentifier="character",
                    #SC treatements
                    o_changepoints_mean="ANY",
                    v_changepoints_mean = "vector",
                    o_changepoints_var="ANY",
                    v_changepoints_var = "vector",
                    o_changepoints_meanvar="ANY",
                    v_changepoints_meanvar = "vector",
                    mean_by_mean="numeric",
                    mean_by_var="numeric",
                    mean_by_meanvar="numeric",
                    mean_by_arbitrary_baseline="numeric",
                    last_by_mean="numeric",
                    last_by_var="numeric",
                    last_by_meanvar="numeric",
                    last_by_arbitrary_baseline="numeric",
                    arbitraryBaseline="numeric",
                    frame_baseline="ANY",
                    frame_absolute="ANY",
                    baselineType = "character",
                    chosen_baseline_index="numeric",
                    chosen_baseline="ANY",
                    chosen_baseline_position="numeric",
                    chosen_last_position="numeric",
                    normalized_wave= "ANY",
                    normalized_wave_amplitude_only= "ANY", 
                    v_frame_zero = "ANY",
                    frame_start_end ="ANY",
                    valleys_peaks = "ANY",
                    padded_normalized_wave_for_periodgram = "ANY",
                    periodgram_frame = "ANY",
                    periodgram = "ANY",
                    main_plot="ANY",
                    periodgram_plot = "ANY",
                    derivate_plot = "ANY", 
                    base_filename="character"
                  )
                  
)

atts <- attributes(Eod$fields())$names
Eod$accessors(atts)
Eod$methods()

Eod$methods(
  initialize=
    function(specimenTag="N/A",
             samplingRate =  global_sampling_rate ,
             metadata_lines = global_metadata_lines, 
             provisionalId="N/A",
             collectionEvent="N/A",
             recordingSoftware ="N/A",
             recordingHardware="N/A" ,
             recordist="N/A",
             bitDepth =global_bit_depth ,
             recordingDateTime = "N/A",
             recordingTemperature = 0,
             recordingConductivity = 0,
             projectName = "N/A", 
             encoding = global_encoding,
             threshold = global_treshold,
             maxPeaks = global_nb_peaks,
             arbitraryBaseline= global_arbitrary_baseline,
             baselineType=global_baseline_type,
             ...
    )
    {
      callSuper(...)
      specimenTag <<- specimenTag
      provisionalId <<- provisionalId
      collectionEvent <<- collectionEvent
      recordingSoftware <<- recordingSoftware
      recordingHardware <<- recordingHardware
      recordist <<- recordist
      bitDepth <<-  bitDepth
      samplingRate <<-  samplingRate
      recordingDateTime <<-  recordingDateTime
      recordingTemperature <<-  recordingTemperature
      recordingConductivity <<-  recordingConductivity
      threshold <<- threshold
      maxPeaks <<-maxPeaks
      #tech
      metadata_lines <<- metadata_lines
      projectName <<- projectName
      encoding <<-encoding
      obj_creation_time <<- Sys.time()
      #sc
      arbitraryBaseline<<-arbitraryBaseline
      v_changepoints_mean <<-c(0)
      v_changepoints_var <<-c(0)
      v_changepoints_meanvar <<-c(0)
      mean_by_mean<<-0
      mean_by_var<<-0
      mean_by_meanvar<<-0
      mean_by_arbitrary_baseline<<-0
      last_by_mean<<-0
      last_by_var<<-0
      last_by_meanvar<<-0
      last_by_arbitrary_baseline<<-0
      baselineType <<- baselineType 
      base_filename<<-basename(file)
      specimenIdentifier<<- paste(projectName, collectionEvent, specimenTag, sep="_")
      .self
    }
  ,
  readFile = function(file, encoding=global_encoding)
  {
    file <<- file
    encoding <<-encoding
    file_obj_tmp <- read.csv(file, 
                             header=FALSE, 
                             nrows=metadata_lines, 
                             sep="=", 
                             colClasses=c("character","character"),
                             fileEncoding = encoding)
    
    #remove extra columns
    if(ncol(file_obj_tmp)>2)
    {
      file_obj_tmp<-file_obj_tmp[,1:2]
    }
    
    colnames(file_obj_tmp) <- c("param", "value")
    file_metadata<<-file_obj_tmp
    initialize(
      specimenTag=file_metadata$value[1],
      provisionalId=file_metadata$value[2],
      collectionEvent=file_metadata$value[3],
      recordingSoftware=file_metadata$value[4],
      recordingHardware=file_metadata$value[5],
      recordist=file_metadata$value[6],
      bitDepth=as.numeric(file_metadata$value[7]),
      samplingRate = as.numeric(file_metadata$value[8]),
      recordingDateTime = file_metadata$value[9],
      recordingTemperature = as.numeric(file_metadata$value[10]),
      recordingConductivity =  as.numeric(file_metadata$value[11]),
      projectName =   file_metadata$value[12],
    )
    #read wave
    decimal_sep <<- get.delim(file,
                              skip=metadata_lines,
                              delims=c(".",".")
    )
    file_wave<<-read.csv(
      file,
      sep='\t',
      skip=metadata_lines, 
      header=TRUE,
      dec=decimal_sep,
      colClasses=c("numeric","numeric", "NULL")
    )
    if(ncol(file_wave)>2)
    {
      file_wave<<-file_wave[,1:2]
    }
    colnames(file_wave) <<- c("time", "amplitude")
  },
  #geters and setters
  getMetadata=function()
  {
    file_metadata
  },
  getBaseFilename=function()
  {
    base_filename
  },
  
  getWave=function()
  {
    file_wave
  },
  getNormalizedWave=function()
  {
    normalized_wave
  },
  getMainPlot = function()
  {
    main_plot
  }
  ,
  getDerivatePlot = function()
  {
    derivate_plot
  },
  getPeriodgramPlot = function()
  {
    periodgram
  },
  getT1T2= function()
  {
    frame_start_end
  },
  getLandmarks= function()
  {
    tmp<-normalized_wave[valleys_peaks,]
    tmp[order(tmp$time),]
  },
  
  getRecordingDateFormatted = function()
  {
    timestamp(recordingDateTime)
  },
  #sc functions
  defineBaseline=function()
  {
    
    
    o_changepoints_mean<<-cpt.mean(file_wave$amplitude, method="BinSeg",Q=5)
    v_changepoints_mean<<-cpts(o_changepoints_mean)
    
    if(length(v_changepoints_mean)>0)
    {  
      mean_by_mean <<- coalesce(mean(file_wave[1:v_changepoints_mean[1],1 ]),0L )
    }
    else
    {
      v_changepoints_mean<<-c(1,length(file_wave$time))
    }
    last_by_mean <<- tail(v_changepoints_mean, n=1)
    
    o_changepoints_var<<-cpt.var(file_wave$amplitude, method="BinSeg",Q=5)
    v_changepoints_var<<-cpts(o_changepoints_var)
    if(length(v_changepoints_var)>0)
    { 
      mean_by_var <<- mean(file_wave[1:v_changepoints_var[1],1 ])
    }
    else
    {
      v_changepoints_var<<-c(1,length(file_wave$time))      
    }
    last_by_var <<- tail(v_changepoints_var, n=1)
    
    o_changepoints_meanvar<<-cpt.meanvar(file_wave$amplitude, method="BinSeg",Q=5)
    v_changepoints_meanvar <<- cpts(o_changepoints_meanvar)
    if(length(v_changepoints_meanvar)>0)
    { 
      mean_by_meanvar <<- mean(file_wave[1:v_changepoints_meanvar[1],1 ])
    }
    else
    {
      v_changepoints_meanvar<<-c(1,length(file_wave$time))
    }
    last_by_meanvar <<- tail(v_changepoints_meanvar, n=1)
    mean_by_arbitrary_baseline <<- mean(file_wave[1:arbitraryBaseline, 1 ])
    last_by_arbitrary_baseline <<-nrow(file_wave)
    
    global_baseline_done = TRUE
    
  },
  getAbsoluteValues=function()
  {
    
    frame_absolute <<- data.frame(
      entry=c("duration_sec", "nb_lines", "min_amplitude", "max_amplitude", "pos_min", "pos_max", "time_min", "time_max"),
      value=c(tail(file_wave$time,1), 
              format(length(file_wave$time), scientific=FALSE), 
              min(file_wave$amplitude), max(file_wave$amplitude),
              format(which(file_wave$amplitude==min(file_wave$amplitude)), scientific=FALSE),
              format(which(file_wave$amplitude==max(file_wave$amplitude)), scientific=FALSE),
              file_wave$time[which(file_wave$amplitude==min(file_wave$amplitude))],
              file_wave$time[which(file_wave$amplitude==max(file_wave$amplitude))]
      )
      
    )
    frame_absolute
  }
  ,
  getPossibleBaseline=function(type="meanvar")
  {
    
    if(global_baseline_done==FALSE)
    {
      defineBaseline()
      
    }
    
    frame_baseline <<- data.frame(
      type=c("mean", "var", "meanvar","by_arbitrary_baseline" ),
      mean=c(mean_by_mean,mean_by_var,mean_by_meanvar,mean_by_arbitrary_baseline ), 
      first_position=c(v_changepoints_mean[1],v_changepoints_var[1], v_changepoints_meanvar[1],arbitraryBaseline),
      last_position=c(last_by_mean, last_by_var, last_by_meanvar,last_by_arbitrary_baseline),
      first_position_time=c(file_wave$time[ v_changepoints_mean[1]],
                            file_wave$time[v_changepoints_var[1]], 
                            file_wave$time[v_changepoints_meanvar[1]],
                            file_wave$time[arbitraryBaseline]),
      last_position_time=c(file_wave$time[last_by_mean], file_wave$time[last_by_var], file_wave$time[last_by_meanvar],file_wave$time[last_by_arbitrary_baseline]),
      inflexion_points=c(paste(v_changepoints_mean,collapse=" "),paste(v_changepoints_var,collapse=" "), paste(v_changepoints_meanvar,collapse=" "),paste(c(arbitraryBaseline,last_by_arbitrary_baseline),collapse=" "))
    )
    
    chooseBaselineAndNormalize(type)
    frame_baseline
  },
  #main sc function
  normalize = function(type=baselineType)
  {
    
    getPossibleBaseline(type)
  }
  ,
  chooseBaselineAndNormalize = function(type) #type = "mean, meanvar, var, arbitrary
  {
    print(type)
    baselineType<<-type
    if(baselineType=="mean")
    {
      chosen_baseline_index <<- 1
    }
    else if(baselineType=="var")
    {
      chosen_baseline_index <<- 2
    }
    else if(baselineType=="meanvar")
    {
      chosen_baseline_index <<- 3
    }
    else if(baselineType=="arbitrary")
    {
      chosen_baseline_index <<-4
    }
    
    chosen_baseline<<-frame_baseline$mean[chosen_baseline_index]
    chosen_baseline_position<<-frame_baseline$first_position[chosen_baseline_index]
    chosen_last_position<<-frame_baseline$last_position[chosen_baseline_index]
    
    #normalize amplitude
    normalized_wave <<- data.frame(file_wave)
    
    normalized_wave$amplitude <<- normalized_wave$amplitude - chosen_baseline
    
    normalized_wave$amplitude <<- normalized_wave$amplitude/(max(normalized_wave$amplitude) - min(normalized_wave$amplitude))
    normalized_wave_amplitude_only <<-normalized_wave
    #normalize time
    normalized_wave<<-normalize_and_center_time(normalized_wave)
    #View(normalized_wave)
    createPeriodgram()
    
    
    signal_limit = start_end_signal()
    print("SIGNAL LIMIT")
    print(signal_limit)
    v_start_time_signal = normalized_wave$time[signal_limit[1]]
    print(v_start_time_signal)
    v_end_time_signal = normalized_wave$time[signal_limit[2]]
    print(v_end_time_signal)
    print("signal duration=")
    print(v_end_time_signal - v_start_time_signal)
    frame_start_end  <<- normalized_wave[signal_limit, ]
    frame_start_end$labels <<- c("T1","T2")
    
    v_zero_point<-which(abs(normalized_wave$time)==min(abs(normalized_wave$time)))
    v_frame_zero <<- normalized_wave[v_zero_point,]
    
    valleys_peaks <<- find_peaks_and_valleys(normalized_wave,chosen_baseline_position,chosen_last_position-1, p_nb_peaks=maxPeaks )
    print(valleys_peaks)
    #main plot
    main_plot <<- save_plot(normalized_wave, paste("Wave and landmarks (normalized)",base_filename), valleys_peaks, v_frame_zero, frame_start_end, chosen_baseline_position )
    
    #deriv plot
    tmp_frame_deriv<-fct_diff_deriv(normalized_wave)
    v_frame_deriv_start_end  <- tmp_frame_deriv[signal_limit, ]
    v_frame_deriv_start_end$labels <- c("T1","T2")
    v_frame_deriv_zero <- tmp_frame_deriv[v_zero_point,]
    derivate_plot <<- save_plot(tmp_frame_deriv, paste("Derivate", base_filename), valleys_peaks, v_frame_deriv_zero, v_frame_deriv_start_end, chosen_baseline_position )
  },
  
  start_end_signal = function()
  {
    
    tmp_amplitude <- abs(normalized_wave$amplitude)
    signal <- which(tmp_amplitude > threshold)
    c(min(signal), max(signal))
  },
  peak_detection = function(p_frame, start_index,last_position, p_threshold=NULL, diff_threshold=NULL)
  {
    v_position=c()
    v_prominence=c()
    previous_peak=0
    #v_prominence <-c()
    for(i in start_index:last_position )
    {
      p_val=p_frame$amplitude[i]
      p_previous=p_frame$amplitude[i-1]
      p_next=p_frame$amplitude[i+1]
      if(p_val > p_previous && p_val == p_next)
      {
        i<-detect_peak_plateau(p_frame, i, nrow(p_frame)-1)
        p_next=p_frame$amplitude[i+1]
      }
      if(p_val > p_previous && p_val > p_next )
      {
        if(!is.null(p_threshold))
        {
          if(p_val<=p_threshold)
          {
            next
          }
        }
        if(!is.null(diff_threshold))
        {
          if((p_val-p_previous )<=diff_threshold)
          {
            next
          }
        }
        
        v_prominence<-c(v_prominence, abs(p_val-previous_peak)) 
        v_position=c(v_position, i)
        previous_peak=p_val
      }
      
    }
    v_returned<-data.frame(position=v_position, prominence=v_prominence)
    v_returned
  },
  
  find_peaks_and_valleys = function( p_frame, p_silent_index, p_last_position, p_diff_threshold=NULL, p_nb_peaks=global_nb_peaks)
  {
    
    
    o_peaks<-peak_detection(p_frame, p_silent_index,p_last_position, diff_threshold =p_diff_threshold )
    p_frame$amplitude=(p_frame$amplitude) * - 1
    o_peaks2<-peak_detection(p_frame, p_silent_index,p_last_position, p_threshold=0,diff_threshold =p_diff_threshold)
    
    v_all_submits=rbind(o_peaks, o_peaks2)
    v_all_submits<-v_all_submits[order(v_all_submits$prominence),]
    
    v_peaks=v_all_submits$position
    
    v_peaks<-unique(v_peaks)
    
    v_peaks<-tail(v_peaks, n=p_nb_peaks)
    
    v_peaks
  }
  ,
  detect_peak_plateau = function(p_frame, current_position, last_position)
  {
    returned<-current_position
    p_ref_val=p_frame$amplitude[current_position]
    for(i in current_position+1:last_position)
    {
      p_val<-p_frame$amplitude[i]
      if(p_val!=p_ref_val)
      {
        return(i-1)
      }
    }
    returned
  },
  fct_diff_deriv=function(p_frame)
  {
    v_returned=c()
    v_diff=diff(p_frame$amplitude)*(samplingRate/1000)
    v_returned=c(v_returned,0 )
    for(i in 2:length(v_diff))
    {
      v_returned=c(v_returned,v_diff[i]-v_diff[i-1] )
    }
    v_returned=c(v_returned,0 )
    
    p_frame$amplitude<-v_returned
    
    p_frame  
  }
  ,
  #FREQUENCY FCT
  eod_spectrum =function()
  {
    
    
    spec<-spectrum(padded_normalized_wave_for_periodgram$amplitude,fs= samplingRate , plot=FALSE)
    
    periodgram_frame <<-data.frame(freq=spec$freq,spec=spec$spec )
    periodgram_frame$freq <<- periodgram_frame$freq * samplingRate
    periodgram <<-ggplot(periodgram_frame, aes(x=periodgram_frame$freq, y=10*log10(periodgram_frame$spec/max(periodgram_frame$spec)) )) + geom_line() + scale_x_log10() +ggtitle(paste("Periodgram", base_filename))
    
    
  }
  ,
  pad_1_sec=function()
  {
    v_diff_1sec=1-max(normalized_wave_amplitude_only$time)
    print("diff 1 sec")
    print(v_diff_1sec)
    
    v_time_interv=diff(normalized_wave_amplitude_only$time)
    v_mean_interval=mean(v_time_interv)
    print("mean interv")
    print(v_mean_interval)
    #View(v_time_interv)
    v_pad=seq(0,v_diff_1sec/2, by=v_mean_interval )
    #View(pad)
    #copy
    v_1sec_frame=data.frame(normalized_wave_amplitude_only)
    v_1sec_frame$time<-v_1sec_frame$time + v_mean_interval+ (v_diff_1sec/2) 
    v_pad_frame=data.frame(time=v_pad, amplitude=rep(0, times=length(v_pad)))
    #View(v_pad_frame)
    v_1sec_frame<-rbind(v_pad_frame,v_1sec_frame )
    v_pad_frame$time=v_pad_frame$time + v_mean_interval +max(v_1sec_frame$time)
    v_1sec_frame<-rbind(v_1sec_frame, v_pad_frame)
    #plot(v_1sec_frame, type='l')
    print("Min padded")
    print(min(v_1sec_frame$time))
    print("Max padded")
    print(max(v_1sec_frame$time))
    padded_normalized_wave_for_periodgram <<-v_1sec_frame
    
    
  },
  createPeriodgram =function()
  {
    v_1sec_frame<-pad_1_sec()
    v_plot<-eod_spectrum()  
  },
  #normalize time(private)
  normalize_and_center_time=function(p_frame)
  {
    
    #normalize time
    p_frame$time<- p_frame$time / (max(p_frame$time) - min(p_frame$time))
    #find center
    max_pos = which(p_frame$amplitude ==(max(p_frame$amplitude)))
    min_pos = which(p_frame$amplitude ==(min(p_frame$amplitude)))
    interv<-c(max_pos, min_pos)
    
    print("Pos max and min")
    print(interv)
    
    #if min before amx
    interv_sorted<- sort(interv)
    
    print("Pos max and min - sorted")
    print(interv_sorted)
    print("p1 time")
    print(p_frame[interv_sorted[1],1])
    print("p2 time")
    print(p_frame[interv_sorted[2],1])
    
    time_center_value = (p_frame[interv_sorted[2],1]+p_frame[interv_sorted[1],1]) /2
    
    
    print("(p1 time + p2 time) / 2")
    print(time_center_value)
    
    #closest array point
    real_time_center_position = which(abs(p_frame$time - time_center_value)==min(abs(p_frame$time - time_center_value))) 
    print("REAL TIME CENTER POSITION")
    print(real_time_center_position)
    real_time_center_value = p_frame[real_time_center_position, 1]
    print("REAL TIME CENTER VALUE")
    print(real_time_center_value)
    
    #substract time baseline value
    p_frame$time <- p_frame$time - real_time_center_value
    #return
    print("return")
    p_frame
    
  },
  inversePhase = function()
  {
    
    global_baseline_done = FALSE
    file_wave$amplitude<<-file_wave$amplitude * -1
    getPossibleBaseline(baselineType)
    return("done")
  }
  ,
  #GGPLOT
  save_plot = function(p_frame, p_title, valleys_peaks, v_frame_zero, v_frame_start_end, v_chosen_baseline_position )
  {
    tmp_plot<-ggplot(p_frame,aes(x = time, y = amplitude)) + geom_line() + ggtitle(p_title)+
      geom_point(shape=8, data= p_frame[valleys_peaks,] , fill="black", color="black", size=3) + #display peaks
      
      geom_point(shape=16,   data= v_frame_zero ,  color="red", size=3) + #x=0 (halfway between peaks)
      geom_point(shape=1,   data= v_frame_start_end ,  color="red", size=3) + #display start and stop
      geom_text(data= v_frame_start_end, label=v_frame_start_end$labels,  hjust=0,vjust=0)+
      geom_vline(aes(xintercept=p_frame$time[chosen_baseline_position]),
                 color="red", linetype="dashed", size=1)
    
    tmp_plot
  }
  #file
  ,
  savePlots = function(folder)
  {
    
    
    name_main_plot=paste0(folder, "\\",base_filename ,"_main_plot", ".png")
    name_derivate_plot=paste0(folder,"\\", base_filename,"_derivate_plot", ".png")
    name_periodgram_plot=paste0(folder,"\\", base_filename,"_periodgram_plot", ".png")
    
    ggsave(name_main_plot,getMainPlot())
    ggsave(name_derivate_plot,getDerivatePlot())
    ggsave(name_periodgram_plot,getPeriodgramPlot())
  }
  
  
)

#cluster object

EodCluster <-setRefClass("EodClusters",
                         fields = list(
                           sourceFiles="vector",
                           eodObjects="ANY",
                           baselineType="character",
                           identifiers="ANY",
                           merged_index="ANY",
                           original_files="ANY"
                         )
)

atts_clusters <- attributes(EodCluster$fields())$names
EodCluster$accessors(atts_clusters)
EodCluster$methods()

EodCluster$methods(
  initialize=
    function(sourceFiles="N/A", baselineType=global_baseline_type, true_filenames=NULL, ...)
    {
      callSuper(...)
      original_files<<-c()
      sourceFiles<<-sourceFiles
      baselineType<<-baselineType
      objs=list()
      identifiers<<-data.frame(id=c(), count=c())
      i <- 1
      for(file in sourceFiles)
      {
        if(is.null(true_filenames))
        {
          original_files<<-c(true_filenames,file)
        }
        else
        {
          original_files<<-c(true_filenames,true_filenames[i])
        }
        
        eod <- Eod$new(specimenTag="init")
        eod$readFile(file)
        eod$getPossibleBaseline(baselineType)
        
        objs[[i]]<-eod
        if(eod$getSpecimenIdentifier() %in% identifiers$id )
        {
          identifiers$count[which(identifiers$id==eod$getSpecimenIdentifier())]<<-identifiers$count[which(identifiers$id==eod$getSpecimenIdentifier())]+1
        }
        else
        {
          line<-data.frame(id=c(eod$getSpecimenIdentifier()), count=c(1))
          identifiers<<-rbind(identifiers,line)
        }
        i <- i + 1
      }
      eodObjects <<- objs
      size <<- length(eodObjects)
      .self
    },
  getEODS=function(index)
  {
    eodObjects[[index]]
  },
  getSpecimenList = function()
  {
    identifiers
  },
  getSpecimenData=function(specimen_id)
  {
    tmp<-unlist(lapply(eodObjects, function(x) x$getSpecimenIdentifier()==specimen_id), use.names = FALSE)
    eodObjects[tmp==TRUE]
  },
  saveAllPlots = function(folder, specimen_id="")
  {
    if(specimen_id=="")
    {
      v_tmp<-eodObjects
    }
    else
    {
      tmp <- unlist(lapply(eodObjects, function(x) x$getSpecimenIdentifier()==specimen_id), use.names = FALSE)
      v_tmp <- eodObjects[tmp==TRUE]
    }
    for(eod in v_tmp)
    {
      eod$savePlots(folder)
    }
  },
  
  superimposePlots=function(folder, specimen_id="")
  {
    if(specimen_id=="")
    {
      file_name=""
      v_tmp<-eodObjects
    }
    else
    {
      file_name=specimen_id
      tmp <- unlist(lapply(eodObjects, function(x) x$getSpecimenIdentifier()==specimen_id), use.names = FALSE)
      v_tmp <- eodObjects[tmp==TRUE]
    }
    df=NULL
    i=1
    cols=c("time")
    for(eod in v_tmp)
    {
      if(i==1)
      {
        df<-data.frame(eod$getNormalizedWave())
      }
      else
      {
        df<-merge(df, eod$getNormalizedWave(), by="time", all=TRUE )
      } 
      cols<-c(cols, eod$getBaseFilename())
      i <- i+1
    }
    print(cols)
    tmp_plot<-ggplot(df, aes(x=time, y=amplitude))
    
    for(eod in v_tmp)
    {
      tmp_frame<-eod$getNormalizedWave() 
      tmp_frame$filename=eod$getBaseFilename()
      colnames(tmp_frame)<-c("time", "amplitude", "filename")
      tmp_plot<- tmp_plot + geom_line(data = tmp_frame, aes(x=time, y=amplitude, color=filename) ) 
    }
    name_merged_plot=paste0(folder, "\\",file_name,"merged_eod_plot", ".png")
    ggsave(name_merged_plot,tmp_plot)
  },
  
  getSize = function()
  {
    length(eodObjects)
  },
  
  getAllEods=function()
  {
    eodObjects
  },
  
  getFilenameForMergedData=function()
  {
    merged_index<<-list()
    l_files<-c()
    i=1
    invisible(lapply(eodObjects, 
          function(eod)
          {
            metadata<-eod$getMetadata()
            tagno = paste(metadata$value[1])
            project_name = paste(metadata$value[nrow(metadata)])
            tagno=tolower(str_replace(tagno, "[^[:alnum:]]",""))
            project_name=tolower(str_replace(project_name, "[^[:alnum:]]",""))
            new_file<-paste0("merge_mscope_", project_name, "_", tagno, ".csv")
            l_files<<-c(l_files,new_file)
            if(new_file %in% merged_index)
            {
              merged_index[[new_file]]<<-c(i)
            }
            else
            {
              merged_index[[new_file]]<<-c(merged_index[[new_file]],i)
            }
            i<<-i+1
          }
      ))
    #print("merged=")
    #print(merged_index)
    print("ORIGINAL")
    print(original_files)
    unique(l_files)
  }
  ,
  createMergedFile=function(new_file, target_folder)
  {
    print("NEW_PARAM")
    print(new_file)
    list_f<-merged_index[[new_file]]
    print(list_f)
    i<-1
    invisible(lapply(list_f,
                    function(x)
                    {
                      print(x)
                      original_file<-original_files[x]
                      print(original_file)
                      fn=paste0(target_folder, "/",new_file )
                      tmp_eod=eodObjects[[x]]
                      write(c("BEGIN",as.character(i), paste0("original_file","\t",original_file ),"="),fn, append = TRUE)
                      write.table(tmp_eod$getMetadata(),fn, sep="\t", append = TRUE, row.names=FALSE, quote = FALSE )
                      write("WAVE",fn, append = TRUE)
                      write.table(tmp_eod$getWave(),fn, sep="\t", append = TRUE, row.names=FALSE , quote = FALSE)
                      write("END",fn, append = TRUE)
                      i<<-i+1
                    }
                    ))
  }
  
)

