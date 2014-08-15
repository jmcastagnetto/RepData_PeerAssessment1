
## ------------------------------------------------------------------------
# knitr configuration
library(knitr)
opts_knit$set(progress=FALSE)
opts_chunk$set(echo=TRUE, message=FALSE, tidy=TRUE, comment=NA,
               fig.path="figure/", fig.keep="high", fig.width=10, fig.height=6,
               fig.align="center")
# load required libs
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2, quietly=TRUE, warn.conflicts=FALSE)


## ------------------------------------------------------------------------
# read the data
activity  <- read.csv(unz("activity.zip", "activity.csv"),
                      colClasses=c("integer","Date","integer"))


## ------------------------------------------------------------------------
summary(activity)


## ------------------------------------------------------------------------
tohhmm <- function(interval) {
    s <- sprintf("%04d", interval)
    s <- paste0(substr(s,1,2),":",substr(s,3,4))
    return(s)
}
tmp <- tohhmm(activity$interval)
activity$interval  <- factor(tmp, levels=unique(tmp), ordered=TRUE)


## ------------------------------------------------------------------------
# remove the NA in steps
activity2 <- activity[!is.na(activity$steps),]
n1 <- nrow(activity)
n2 <- nrow(activity2)


## ------------------------------------------------------------------------
df1 <- activity2 %>% group_by(date) %>% summarise(nsteps=sum(steps))


## ----part2-histogram-----------------------------------------------------
ggplot(df1, aes(x=nsteps)) + geom_histogram(col=rgb(0,0,1,0.5), 
                                            fill=rgb(0,0,1,0.5), binwidth=500) +
    ggtitle("Distribution of total number of steps per day") +
    xlab("Number of steps") + ylab("Frequency") +
    theme_bw()


## ------------------------------------------------------------------------
dfmean = format(round(mean(df1$nsteps),2), nsmall=2)
dfmedian = format(round(median(df1$nsteps), 2), nsmall=2)


## ------------------------------------------------------------------------
df2 <- activity2 %>% group_by(interval) %>% summarise(avgsteps=mean(steps))


## ----part3-timeseries----------------------------------------------------
# prepare some vectors to label the x-axes
intbrk <- as.numeric(df2$interval)
intbrk <- intbrk[seq(1, length(intbrk), 12)]
intlab <- levels(df2$interval)
intlab <- intlab[seq(1, length(intlab), 12)]
# make the plot
ggplot(df2, aes(x=as.numeric(interval), y=avgsteps)) + 
    scale_x_continuous(breaks=intbrk, labels=intlab) +
    geom_line(color="blue") +
    ggtitle("Average daily pattern") + 
    ylab("Average number of steps") +
    xlab("Interval") + 
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1))


## ------------------------------------------------------------------------
df2[with(df2, avgsteps==max(avgsteps)),]$interval


## ------------------------------------------------------------------------
sum(is.na(activity$steps))


## ------------------------------------------------------------------------
# step 1
df3 <- activity %>% group_by(interval) %>% 
    summarise(median=median(steps, na.rm=TRUE))
# step 2
activity3 <- merge(activity, df3, by="interval")
# step 3
activity3$steps <- ifelse(is.na(activity3$steps), 
                          activity3$median, activity3$steps)
# step 4
activity3$median <- NULL


## ----part4-histogram-----------------------------------------------------
df4 <- activity3 %>% group_by(date) %>% summarise(nsteps=sum(steps))
ggplot(df4, aes(x=nsteps)) + geom_histogram(col=rgb(0,1,0,0.5), 
                                            fill=rgb(0,1,0,0.5), binwidth=500) +
    ggtitle("Distribution of total number of steps per day (imputed data)") +
    xlab("Number of steps") + ylab("Frequency") +
    theme_bw()


## ------------------------------------------------------------------------
dfmean2 = format(round(mean(df4$nsteps),2), nsmall=2)
dfmedian2 = format(round(median(df4$nsteps), 2), nsmall=2)


## ----results='asis'------------------------------------------------------
kable(data.frame("Statistic"=c("Mean", "Median"),
                  "Original"=c(dfmean, dfmedian),
                  "Imputed"=c(dfmean2, dfmedian2)),
      format="html", table.attr="cellpadding='3'")


## ------------------------------------------------------------------------
activity3$daytype <- ifelse(
    weekdays(activity3$date) %in% c("Saturday", "Sunday"),
    "Weekend", "Weekday")
activity3$daytype <- as.factor(activity3$daytype)


## ------------------------------------------------------------------------
df5 <- activity3 %>% group_by(daytype, interval) %>% summarise(mean=mean(steps))


## ----part5-timeseries, fig.height=10-------------------------------------
# prepare a couple of vectors to label de x-axis
intbrk <- unique(as.numeric(df5$interval))
intbrk <- intbrk[seq(1, length(intbrk), 12)]
intlab <- levels(df5$interval)
intlab <- intlab[seq(1, length(intlab), 12)]
# make the plot
ggplot(df5, aes(x=as.numeric(interval), y=mean, color=daytype)) +
    scale_x_continuous(breaks=intbrk, labels=intlab) +
    geom_line() + facet_wrap(~ daytype, ncol=1) + 
    xlab("Interval") + ylab("Average number of steps") +
    ggtitle("Comparison of activity by type of day") + 
    scale_color_discrete(guide="none") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1))


## ------------------------------------------------------------------------
sessionInfo()


