

# https://www.r-bloggers.com/2018/06/anomaly-detection-in-r-2/
# https://unit8.co/resources/a-guide-to-building-a-financial-transaction-anomaly-detector/
# https://rpubs.com/Joaquin_AR/614976
# https://www.r-bloggers.com/2020/09/time-series-in-5-minutes-part-5-anomaly-detection/

library(readxl)
library(plyr)
library(jsonlite)
library(purrr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(caret)
library(dplyr)
library(solitude)
library(timetk)

# Data reading

json <- jsonlite::fromJSON("https://data.birmingham.gov.uk/api/3/action/package_search?q=Purchase%20Card%20transactions")

dat <- json$result$results$resources[[1]][c("name","url")]

lapply(seq(length(dat$url)),function(w){
  download.file(dat$url[w],destfile=paste0("Data/",dat$name[w],".xls"), mode = "wb")}
)

full_dataset <- lapply(list.files("Data/",full.names = T),read_xls)

# Data pre processing

# sapply(lapply(full_dataset, names),function(x){x[length(x)]})

names(full_dataset[[2]])[11] <- "Directorate"

names(full_dataset[[14]])[11] <- "Directorate"

names(full_dataset[[42]])[13] <- "Directorate"

full_dataset <- full_dataset[-13]

full_ds <- do.call(rbind.fill,full_dataset)

names(full_ds) <- gsub(" ","_",names(full_ds))

full_ds$TRANS_DATE <- substring(full_ds$TRANS_DATE,1,10)

full_ds$TRANS_DATE <- as.Date(full_ds$TRANS_DATE)

full_ds <- full_ds %>% modify_if(is.character, as.factor) 

head(full_ds)

write.csv(full_ds,"consolidado.csv")


# Data processing



boxplot(full_ds$ORIGINAL_GROSS_AMT)

full_ds <- full_ds[full_ds$ORIGINAL_GROSS_AMT>0,]

full_ds <- full_ds[full_ds$ORIGINAL_GROSS_AMT<250000,]


# Se identificaron puntos atipicos

sort(full_ds$ORIGINAL_GROSS_AMT)


# Feature engineer


highFreq <- full_ds %>% group_by(TRANS_CAC_DESC_1) %>% summarise(n=n()) %>% filter(n>=50) 

full_ds$TRANS_CAC_DESC_1 <- as.character(full_ds$TRANS_CAC_DESC_1)

full_ds$TRANS_CAC_DESC_1[!(full_ds$TRANS_CAC_DESC_1 %in% highFreq$TRANS_CAC_DESC_1 )] <- "Others"

full_ds$TRANS_CAC_DESC_1 <- as.factor(full_ds$TRANS_CAC_DESC_1)

summary(full_ds)

full_ds %>% group_by(TRANS_DATE,TRANS_CAC_DESC_1 )  %>% summarise(n=n()) %>% arrange(desc(n))

full_ds <- full_ds %>% group_by(TRANS_DATE,TRANS_CAC_DESC_1 )  %>% summarise(n=n(),tot=sum(ORIGINAL_GROSS_AMT))



full_ds <- full_ds %>% pivot_wider( names_from = TRANS_CAC_DESC_1, values_from = n )

full_ds <- aggregate(full_ds[,-1], list(TRANS_DATE=as.character(full_ds$TRANS_DATE)),sum,na.rm=T)

full_ds$TRANS_DATE <- as.Date(full_ds$TRANS_DATE)

full_ds <- full_ds %>% mutate(day = day(TRANS_DATE), month = month(TRANS_DATE), wday = wday(TRANS_DATE))



# Explortory data analysis

ggplot(full_ds,aes(x=factor(wday),y=tot   ))+geom_boxplot()

ggplot(full_ds,aes(x=factor(day),y=tot   ))+geom_boxplot()

ggplot(full_ds,aes(x=factor(month),y=tot))+geom_boxplot()

ggplot(full_ds,aes(x=TRANS_DATE,y=tot ))+geom_line()

ggplot(full_ds,aes(x=TRANS_DATE,y=tot))+geom_line()+ylim(c(-5000,10000))

full_ds <- full_ds[,-nearZeroVar(full_ds)]

# Insolation model

iso = isolationForest$new()

iso$fit(full_ds)

scores_train = full_ds %>%
  iso$predict() %>%
  arrange(desc(anomaly_score))

ggplot(scores_train,aes(x=anomaly_score)) +geom_histogram()

join_scores_train <- data.frame(scores_train) %>%
  right_join(data.frame(ID = as.numeric(row.names(full_ds)),full_ds),by = c("id"="ID"))

full_ds %>%
  plot_anomaly_diagnostics(TRANS_DATE, tot,
                           .message = FALSE,
                          # .facet_ncol = 3,
                           .ribbon_alpha = 0.20,
                           .interactive = FALSE,.max_anomalies=5)

