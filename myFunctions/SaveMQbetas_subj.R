


# read in subject level betas (from 1st level Limo)
list_of_files <- list.files(path = "/Users/natalia/Documents/orbit-eeg_nlw/Limo_output/subj_ERP_betas",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

# a little bit of clean-up
sub.mq <- readr::read_csv(list_of_files, id = "file_name")
colnames(sub.mq) <- sub.mq[1,]
colnames(sub.mq)[1] <- "subject"
colnames(sub.mq)[2] <- "channel"
sub.mq <- na.omit(sub.mq)
sub.mq$channel <- as.factor(as.character(sub.mq$channel))

# rename subjects to omit full file path
sub.mq$subject <- gsub("/Users/natalia/Documents/orbit-eeg_nlw/Limo_output/subj_ERP_betas/", "", sub.mq$subject)
sub.mq$subject <- gsub("_MQ_ERP_betas.csv", "", sub.mq$subject)
sub.mq$subject <- gsub("j_", "-", sub.mq$subject)
sub.mq$subject <- as.factor(as.character(sub.mq$subject))


# wide to long format
sub.mq <- sub.mq %>% 
  pivot_longer(
    cols = 3:643, 
    names_to = "time",
    values_to = "beta"
  )
sub.mq$time <- as.numeric(as.character(sub.mq$time))


write.csv(sub.mq, file = "AllData_EEG-MQ_ERP.csv", row.names = FALSE)


#############

library(readr)

list_of_files <- list.files(path = "/Users/natalia/Documents/orbit-eeg_nlw/Limo_output/subj_ERP_feature_betas",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

# a little bit of clean-up
sub.ft <- readr::read_csv(list_of_files, id = "file_name")
colnames(sub.ft) <- sub.ft[1,]
colnames(sub.ft)[1] <- "subject"
colnames(sub.ft)[2] <- "channel"
sub.ft <- na.omit(sub.ft)
sub.ft$channel <- as.factor(as.character(sub.ft$channel))

# rename subjects to omit full file path
sub.ft$subject <- gsub("/Users/natalia/Documents/orbit-eeg_nlw/Limo_output/subj_ERP_feature_betas/", "", sub.ft$subject)
sub.ft$subject <- gsub("_betas.csv", "", sub.ft$subject)
sub.ft$subject <- gsub("_ERP", "", sub.ft$subject)
sub.ft$subject <- gsub("j_", "-", sub.ft$subject)
sub.ft$subject <- as.factor(as.character(sub.ft$subject))
sub.ft$feature <- sub.ft$subject
sub.ft$subject <- gsub("\\_.*", "", sub.ft$subject)
sub.ft$feature <- gsub(".*_", "", sub.ft$feature)

# wide to long format
sub.ft <- sub.ft %>% 
  pivot_longer(
    cols = 3:643, 
    names_to = "time",
    values_to = "beta"
  )

sub.ft$time <- as.numeric(as.character(sub.ft$time))


write.csv(sub.ft, file = "AllData_EEG-FT_ERP.csv", row.names = FALSE)



#############

setwd("/Users/natalia/Documents/orbit-eeg_nlw/Limo_output/")


# load data
t.data <- read.csv('MQ_tvalue_TF.csv')
p.data <- read.csv('MQ_pvalue_TF.csv')
betamean.data <- read.csv('MQ_meanbetas_TF.csv')
betase.data <- read.csv('MQ_sebetas_TF.csv')
poscluster <- read.csv('MQ_posClusters_TF.csv') 
negcluster <- read.csv('MQ_negClusters_TF.csv')

# T Stats - format data (time x channel x t value x frequency)
tdata.clean <- data.frame()
for (i in 3:28) {
  test <- t.data %>% select(1,2,i) #only one freq column
  test$freq <- rep(test[1,3]) #duplicate freq value into new column
  names(test)[1] <- "time"
  names(test)[2] <- "channel"
  names(test)[3] <- "tvalue"
  names(test)[4] <- "freq"
  test <- test[-1,] #remove first row
  tdata.clean <- rbind(tdata.clean,test) }

# P Value - format data (time x channel x p value x frequency)
pdata.clean <- data.frame()
for (i in 3:28) {
  test <- p.data %>% select(1,2,i) #only one freq column
  test$freq <- rep(test[1,3]) #duplicate freq value into new column
  names(test)[1] <- "time"
  names(test)[2] <- "channel"
  names(test)[3] <- "pvalue"
  names(test)[4] <- "freq"
  test <- test[-1,] #remove first row
  pdata.clean <- rbind(pdata.clean,test) }

# Mean Beta - format data (time x channel x mean beta x frequency)
mbdata.clean <- data.frame()
for (i in 3:28) {
  test <- betamean.data %>% select(1,2,i) #only one freq column
  test$freq <- rep(test[1,3]) #duplicate freq value into new column
  names(test)[1] <- "time"
  names(test)[2] <- "channel"
  names(test)[3] <- "mean.beta"
  names(test)[4] <- "freq"
  test <- test[-1,] #remove first row
  mbdata.clean <- rbind(mbdata.clean,test) }

# SE Beta - format data (time x channel x se beta x frequency)
sebdata.clean <- data.frame()
for (i in 3:28) {
  test <- betase.data %>% select(1,2,i) #only one freq column
  test$freq <- rep(test[1,3]) #duplicate freq value into new column
  names(test)[1] <- "time"
  names(test)[2] <- "channel"
  names(test)[3] <- "se.beta"
  names(test)[4] <- "freq"
  test <- test[-1,] #remove first row
  sebdata.clean <- rbind(sebdata.clean,test) }

# Positive Clusters - format data (time x channel x corrected p value x frequency)
poscluster.clean <- data.frame()
for (i in 3:28) {
  test <- poscluster %>% select(1,2,i) #only one freq column
  test$freq <- rep(test[1,3]) #duplicate freq value into new column
  names(test)[1] <- "time"
  names(test)[2] <- "channel"
  names(test)[3] <- "correctedP.pos"
  names(test)[4] <- "freq"
  test <- test[-1,] #remove first row
  poscluster.clean <- rbind(poscluster.clean,test) }

# Negative Clusters - format data (time x channel x corrected p value x frequency)
negcluster.clean <- data.frame()
for (i in 3:28) {
  test <- negcluster %>% select(1,2,i) #only one freq column
  test$freq <- rep(test[1,3]) #duplicate freq value into new column
  names(test)[1] <- "time"
  names(test)[2] <- "channel"
  names(test)[3] <- "correctedP.neg"
  names(test)[4] <- "freq"
  test <- test[-1,] #remove first row
  negcluster.clean <- rbind(negcluster.clean,test) }

tfdata.clean <- merge(mbdata.clean, sebdata.clean, by = c("time", "channel", "freq"))
tfdata.clean <- merge(tfdata.clean, pdata.clean, by = c("time", "channel", "freq"))
tfdata.clean <- merge(tfdata.clean, tdata.clean, by = c("time", "channel", "freq"))
tfdata.clean <- merge(tfdata.clean, poscluster.clean, by = c("time", "channel", "freq"))
tfdata.clean <- merge(tfdata.clean, negcluster.clean, by = c("time", "channel", "freq"))


tfdata.clean$channel <- as.character(tfdata.clean$channel)
tfdata.clean$time <- as.numeric(as.character(tfdata.clean$time))



write.csv(tfdata.clean, file = "/Users/natalia/Desktop/orbit-EEG/data/AllData_EEG-MQ_TF.csv", row.names = FALSE)




########


# load data
color <- read.csv('Color_TF_allstats.csv')
names(color)[1] <- 'stat'
color$feature <- 'color'
scene <- read.csv('Scene_TF_allstats.csv')
names(scene)[1] <- 'stat'
scene$feature <- 'scene'
emotion <- read.csv('Emotion_TF_allstats.csv')
names(emotion)[1] <- 'stat'
emotion$feature <- 'emotion'

features <- rbind(color,scene,emotion)

# organize
features <- features %>%
  select(feature, everything())

# load cluster info
poscorP.color <- read.csv('Color_posClusters_TF.csv')
negcorP.color <- read.csv('Color_negClusters_TF.csv')
poscorP.color$feature <- 'color'
negcorP.color$feature <- 'color'
poscorP.color$stat <- 'correctedP.pos'
negcorP.color$stat <- 'correctedP.neg'

poscorP.scene <- read.csv('Scene_posClusters_TF.csv')
negcorP.scene <- read.csv('Scene_negClusters_TF.csv')
poscorP.scene$feature <- 'scene'
negcorP.scene$feature <- 'scene'
poscorP.scene$stat <- 'correctedP.pos'
negcorP.scene$stat <- 'correctedP.neg'

poscorP.emotion <- read.csv('Emotion_posClusters_TF.csv')
negcorP.emotion <- read.csv('Emotion_negClusters_TF.csv')
poscorP.emotion$feature <- 'emotion'
negcorP.emotion$feature <- 'emotion'
poscorP.emotion$stat <- 'correctedP.pos'
negcorP.emotion$stat <- 'correctedP.neg'

#maintain only one frequency row for all pos and neg cluster (corP.pos, color)
poscorP.scene <- poscorP.scene[-1,]
negcorP.scene <- negcorP.scene[-1,]
poscorP.emotion <- poscorP.emotion[-1,]
negcorP.emotion <- negcorP.emotion[-1,]

pos.features <- rbind(poscorP.color, poscorP.scene, poscorP.emotion)
neg.features <- rbind(negcorP.color, negcorP.scene, negcorP.emotion)

# organize
pos.features <- pos.features %>%
  select(feature, stat, everything())
pos.features <- pos.features[,-2] #remove stat label

neg.features <- neg.features %>%
  select(feature, stat, everything())
neg.features <- neg.features[,-2] #remove stat label

## MEAN BETA ##
# format data (feature x stattype x time x channel x stat x frequency)
meanbeta <- data.frame()
all.meanbeta <- data.frame()
feature.subset <- subset(features, stat == 'meanbeta')
for (f in 1:3) {
  if (f == 1) {feature.loop <- subset(feature.subset, feature == 'color')} else if (f == 2) {feature.loop <- subset(feature.subset, feature == 'scene')} else if (f == 3) {feature.loop <- subset(feature.subset, feature == 'emotion')}
  for (i in 5:30) {
    test <- feature.loop %>% select(1,2,3,4,i) #only one freq column
    test$freq <- rep(test[1,5]) #duplicate freq value into new column
    names(test)[1] <- "feature"
    names(test)[2] <- "stat"
    names(test)[3] <- "time"
    names(test)[4] <- "channel"
    names(test)[5] <- "meanbeta"
    test <- test[-1,] #remove first row
    meanbeta <- rbind(meanbeta,test) }
  all.meanbeta <- rbind(all.meanbeta, meanbeta)}
all.meanbeta <- all.meanbeta[,-2] # remove second column, redundant

## STANDARD ERROR BETA ##
# format data (feature x stattype x time x channel x stat x frequency)
sebeta <- data.frame()
all.sebeta <- data.frame()
feature.subset <- subset(features, stat == 'sebeta')
for (f in 1:3) {
  if (f == 1) {feature.loop <- subset(feature.subset, feature == 'color')} else if (f == 2) {feature.loop <- subset(feature.subset, feature == 'scene')} else if (f == 3) {feature.loop <- subset(feature.subset, feature == 'emotion')}
  feature.loop <- feature.loop[-1,]
  for (i in 5:30) {
    test <- feature.loop %>% select(1,2,3,4,i) #only one freq column
    test$freq <- rep(test[1,5]) #duplicate freq value into new column
    names(test)[1] <- "feature"
    names(test)[2] <- "stat"
    names(test)[3] <- "time"
    names(test)[4] <- "channel"
    names(test)[5] <- "sebeta"
    test <- test[-1,] #remove first row
    sebeta <- rbind(sebeta, test)}
  all.sebeta <- rbind(all.sebeta, sebeta)}
all.sebeta <- all.sebeta[,-2] # remove second column, redundant

## T STATISTIC ##
# format data (feature x stattype x time x channel x stat x frequency)
tvalue <- data.frame()
all.tvalue <- data.frame()
feature.subset <- subset(features, stat == 'tvalue')
for (f in 1:3) {
  if (f == 1) {feature.loop <- subset(feature.subset, feature == 'color')} else if (f == 2) {feature.loop <- subset(feature.subset, feature == 'scene')} else if (f == 3) {feature.loop <- subset(feature.subset, feature == 'emotion')}
  feature.loop <- feature.loop[-1,]
  for (i in 5:30) {
    test <- feature.loop %>% select(1,2,3,4,i) #only one freq column
    test$freq <- rep(test[1,5]) #duplicate freq value into new column
    names(test)[1] <- "feature"
    names(test)[2] <- "stat"
    names(test)[3] <- "time"
    names(test)[4] <- "channel"
    names(test)[5] <- "tvalue"
    test <- test[-1,] #remove first row
    tvalue <- rbind(tvalue, test)}
  all.tvalue <- rbind(all.tvalue, tvalue)}
all.tvalue <- all.tvalue[,-2] # remove second column, redundant

## P VALUE ##
# format data (feature x stattype x time x channel x stat x frequency)
pvalue <- data.frame()
all.pvalue <- data.frame()
feature.subset <- subset(features, stat == 'pvalue')
for (f in 1:3) {
  if (f == 1) {feature.loop <- subset(feature.subset, feature == 'color')} else if (f == 2) {feature.loop <- subset(feature.subset, feature == 'scene')} else if (f == 3) {feature.loop <- subset(feature.subset, feature == 'emotion')}
  feature.loop <- feature.loop[-1,]
  for (i in 5:30) {
    test <- feature.loop %>% select(1,2,3,4,i) #only one freq column
    test$freq <- rep(test[1,5]) #duplicate freq value into new column
    names(test)[1] <- "feature"
    names(test)[2] <- "stat"
    names(test)[3] <- "time"
    names(test)[4] <- "channel"
    names(test)[5] <- "pvalue"
    test <- test[-1,] #remove first row
    pvalue <- rbind(pvalue, test)}
  all.pvalue <- rbind(all.pvalue, pvalue)}
all.pvalue <- all.pvalue[,-2] # remove second column, redundant

## CORRECTED P VALUE - POSITIVE CLUSTERS ##
# format data (feature x time x channel x stat x frequency)
pos.pcorvalue <- data.frame()
for (i in 4:29) {
  test <- pos.features %>% select(1,2,3,i) #only one freq column
  test$freq <- rep(test[1,4]) #duplicate freq value into new column
  names(test)[1] <- "feature"
  names(test)[2] <- "time"
  names(test)[3] <- "channel"
  names(test)[4] <- "correctedP.pos"
  test <- test[-1,] #remove first row
  pos.pcorvalue <- rbind(pos.pcorvalue, test)} #bind all 

## CORRECTED P VALUE - NEGATIVE CLUSTERS ##
# format data (feature x time x channel x stat x frequency)
neg.pcorvalue <- data.frame()
for (i in 4:29) {
  test <- neg.features %>% select(1,2,3,i) #only one freq column
  test$freq <- rep(test[1,4]) #duplicate freq value into new column
  names(test)[1] <- "feature"
  names(test)[2] <- "time"
  names(test)[3] <- "channel"
  names(test)[4] <- "correctedP.neg"
  test <- test[-1,] #remove first row
  neg.pcorvalue <- rbind(neg.pcorvalue, test)} #bind all 

all.pcorvalue <- merge(pos.pcorvalue, neg.pcorvalue, by = c("feature", "time", "channel", "freq"))


# merge all formatted stats into one df - features.clean
features.clean <- merge(all.meanbeta, all.sebeta, by = c("feature", "time", "channel", "freq"))
features.clean <- merge(features.clean, all.pvalue, by = c("feature", "time", "channel", "freq"))
features.clean <- merge(features.clean, all.tvalue, by = c("feature", "time", "channel", "freq"))
features.clean <- merge(features.clean, all.pcorvalue, by = c("feature", "time", "channel", "freq"))

# delete duplicate rows
#features.clean <- unique(features.clean)


features.clean$channel <- as.character(features.clean$channel)
features.clean$time <- as.numeric(as.character(features.clean$time))

write.csv(features.clean, file = "/Users/natalia/Desktop/orbit-EEG/data/AllData_EEG-FT_TF.csv", row.names = FALSE)


