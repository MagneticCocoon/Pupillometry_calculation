library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)


## 010: Load data, and frame a result matrix

pathName = "xxx"
filenames <- paste(pathName, dir(pathName, pattern ="*xxx.txt"), sep = "/")
numDF = length(filenames)

#-- Frame result matix: store the dependent variable values for each participant and scenario, per row
header = c("Group", "ID", "Scenario", "Pupil_avrg", "Pupil_onlyBlock", "Pupil_noPanic_block", "Pupil_noPanic_pause", "Pupil_Panic_block", "Pupil_Panic_pause")
resultPupil <- data.frame(matrix(, nrow = numDF, ncol = 9, dimnames = list(c(),header)))


## 020: Calculate pupil increase

for (n in 1:numDF){
  
  print(n)
  
  filename_n <- filenames[n]
  df_n = read.table(filename_n, header=TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  # 0. Preprocessing
  #-- Convert pupil size in pixel to mm (ref:Pupil_size_scaling_rational.txt)
  if (is.na(df_n$PupilDiam[1])) {df_n <- mutate(df_n, PupilDiam = PupilDiamPx/3.07)} # ref: Pupil_size_scaling.txt
  df_n$PupilDiamPx <- NULL
  
  #-- Trim before/after a session & make blinks NA
  df_n <- filter(df_n, IntvNum > -1)
  df_n$PupilDiam[which(df_n$PupilDiam==0)] <- NA
  
  #-- Convert microseconds to seconds for timestamps
  df_n <- mutate(df_n, Time = Time/1000000)
  
  
  # 1. Pupil Forshortening Error (PFE) correction (ref: PFE_correction folder)
  # 1) Pysical parameters
  Cx = -30
  Cy = -165
  Cz = 640
  Sx = -300
  Sy = 160
  Sz = 650
  mm_per_px = 0.3527778
  df_n <- mutate(df_n, X = Sx + (X * mm_per_px), Y = Sy - (Y * mm_per_px), Z = Sz )
  
  # 2) cosineTheta function application
  cosTheta <- function(Cx,Cy,Cz,X,Y,Z) {
    dot <- Cx*X + Cy*Y + Cz*Z
    absC <- sqrt(Cx^2 + Cy^2 + Cz^2)
    absT <- sqrt(X^2 + Y^2 + Z^2)
    dot / (absC * absT)
  }
  
  df_n <- mutate(df_n, Pupil_cor = PupilDiam / sqrt(cosTheta(Cx,Cy,Cz,X,Y,Z)))
  df_n$PupilDiam <- NULL # delete columns after used
  df_n$X <- NULL 
  df_n$Y <- NULL
  df_n$Z <- NULL
  
  
  # 2. Pupil delay correction: remove delays when stimuli change
  # 1) Make flags when stimuli change, using "lag" function
  df_n <- df_n %>% mutate(Intervals = paste(IntvName, IntvNum))  %>%  
    mutate(flag = ifelse(Intervals == lag(Intervals), "", "flag"))
  
  # 2) Determine delays within each interval
  DELAYTIME = 200  # 800ms latency (ref: 250 frame/sec * 0.8)
  df_n$flag[1] <- "" # reset the first row
  
  framecounter = -1 # start the loop from -1 to flag the first row
  for (i in 1:nrow(df_n)){
    if (df_n$flag[i] == "flag") { framecounter = 0 } else { framecounter = framecounter + 1 }
    if (framecounter < DELAYTIME) {df_n$Intervals[i] <- "Delay" }
  }  
  df_n$flag <- NULL
  df_n <- filter(df_n, Intervals != "Delay")
  
  
  # 3. Light reflex correction: baselining
  # 1) Calculate baselines
  BlockBase = mean(filter(df_n, Intervals == "Block 0")$Pupil_cor, na.rm = TRUE)
  PauseBase = mean(filter(df_n, Intervals == "Pause 0")$Pupil_cor, na.rm = TRUE)
  df_n <- filter(df_n, IntvNum > 0) # delete baseline intervals after calculation
  
  # 2) Subtrct baselines
  df_n <- df_n %>% mutate(Pupil_inc = ifelse(IntvName == "Block", Pupil_cor - BlockBase, Pupil_cor)) %>%
    mutate(Pupil_inc = ifelse(IntvName == "Pause", Pupil_cor - PauseBase, Pupil_inc))
  df_n$Pupil_cor <- NULL
  
  
  # 4. Calculate means of pupil increase for each intervals & store them in the matrix
  # 1) Make intervals
  df_n <- mutate(df_n, IntvSeiz = paste(IntvName, seizure))
  nopauses <- filter(df_n, IntvName == "Block")
  blocks <- filter(df_n, IntvSeiz == "Block seizureoff")
  pauses <- filter(df_n, IntvSeiz == "Pause seizureoff")
  blocksPn <- filter(df_n, IntvSeiz == "Block seizureon") 
  pausesPn <- filter(df_n, IntvSeiz == "Pause seizureon")
  
  # 2) Calculate means & store variales in the matrix
  resultPupil[n,"Group"] = str_extract(filename_n, "[NP][PA]")
  resultPupil[n,"ID"] = str_extract(filename_n, "[0-9]+")
  resultPupil[n,"Scenario"] = str_extract(filename_n, "[GS]")
  
  resultPupil[n,"Pupil_avrg"] = mean(df_n$Pupil_inc, na.rm = TRUE)
  resultPupil[n,"Pupil_onlyBlock"] = mean(nopauses$Pupil_inc, na.rm = TRUE)
  resultPupil[n,"Pupil_noPanic_block"] = mean(blocks$Pupil_inc, na.rm = TRUE)
  resultPupil[n,"Pupil_noPanic_pause"] = mean(pauses$Pupil_inc, na.rm = TRUE)
  resultPupil[n,"Pupil_Panic_block"] = mean(blocksPn$Pupil_inc, na.rm = TRUE)
  resultPupil[n,"Pupil_Panic_pause"] = mean(pausesPn$Pupil_inc, na.rm = TRUE)
  
  resultPupil[resultPupil == "-Inf"] <- 0
  
  
  # 5. Plot time series of individual (time unit: second)
  # 1) Import data frame from df_n & make second-base time
  df <- mutate(df_n, TimeFloored = floor(Time), Second = TimeFloored - TimeFloored[1] +1) %>%  # make seconds counting from 1
    mutate(IntvSeiz = paste(seizure, Intervals))
  
  df <- df %>% group_by(Group, ID, Scenario, IntvName, seizure, IntvSeiz, Second) %>% 
    summarize(meanPupil_inc = mean(Pupil_inc, na.rm=TRUE), medPupil_inc = median(Pupil_inc, na.rm=TRUE),
              sdPupil_inc = sd(Pupil_inc, na.rm=TRUE),
              npoint = n()) %>% # calculate center values for each second
    ungroup(df) %>% arrange(Second) %>% distinct(Second, .keep_all = TRUE) # arrange by second and remove duplicated seconds
  
  # 2) Make window counter
  df <- mutate(df, flag = ifelse(IntvSeiz == lag(IntvSeiz), "", "flag")) # flag when current observation is different from the previous observ.
  df$windowcounter[1] = 1 # start the counter from 1
  for (i in 2:nrow(df)){
    if (df$flag[i] == "flag") { df$windowcounter[i] = lag(df$windowcounter)[i] + 1 } else { df$windowcounter[i] = lag(df$windowcounter)[i]}
  } # window number increases whenever there is a flag 
  df$flag <- NULL
  df <- mutate(df, IntvSeiz = paste(IntvSeiz, windowcounter))
  
  # 3) Make window index
  df_window <- df %>% group_by(seizure, IntvName, IntvSeiz) %>%
    summarize(start = min(Second), end = max(Second)) %>% # start/end points of each window
    ungroup() %>% mutate(windowtype = paste(seizure,IntvName))
  
  df_window <- df_window %>% arrange(start) %>%
    mutate(Mode = dplyr::recode(seizure,
                                "seizureoff" = "Non-panic",
                                "seizureon" = "Panic")) %>%
    mutate(Window = dplyr::recode(windowtype,
                                  "seizureoff Block" = "Non-panic block",
                                  "seizureoff Pause" = "Pause",
                                  "seizureon Block" = "Panic block",
                                  "seizureon Pause" = "Pause")) %>%
    dplyr::select(-seizure, -windowtype) # re-organize window index
  
  # 4) Plot
  ggplot(df) + 
    geom_rect(data=df_window, aes(xmin=start, xmax=end,  fill=Window), ymin = -Inf, ymax = Inf, alpha = .2) +
    geom_line(aes(x=Second, y=meanPupil_inc), color="grey70", size=.5) +
    geom_line(aes(x=Second, y=mean(df_n$Pupil_inc, na.rm = TRUE)), color="yellow", size=.5) +
    geom_smooth(aes(x=Second, y=meanPupil_inc), color="black", size=.5, span=.1, method= "loess", se=FALSE) +
    scale_fill_manual(values = c("blue","red","grey90")) +
    xlab("Second") +
    ylab("Pupil increase [mm]") +
    ggtitle(paste("Pupil increase trend for", substr(filename_n, 17,22)))
  
  # 6. Save the image file of df_n
  ggsave(paste("Plot_pupilTimeseries/",substr(filename_n, 17,22),"_TimeTrend.png"),
         dpi = 300)

}



