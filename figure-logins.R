source("packages.R")

load("logins.RData")

count.dt <- dcast(
  all.intervals, date.str ~ hours.past.midnight,
  value.var="logins")
count.mat.na <- as.matrix(count.dt[, -1, with=FALSE])
rownames(count.mat.na) <- count.dt$date.str
count.mat <- count.mat.na[!apply(is.na(count.mat.na), 1, any),]
stopifnot(!is.na(count.mat))
pc.fit <- princomp(count.mat)
component.mat <- pc.fit$scores[,1:2]
component.dt <- data.table(component.mat, date.str=rownames(component.mat))

days[, is.weekend := weekday %in% c("Saturday", "Sunday")]
all.intervals[, is.weekend := weekday %in% c("Saturday", "Sunday")]
setkey(days, date.str)
setkey(component.dt, date.str)
component.days <- days[component.dt]

set.seed(2)
kmeans.fit <- kmeans(count.mat, centers=2)
stopifnot(rownames(kmeans.fit$cluster) == component.days$date.str)
component.days$kmeans.cluster <- factor(kmeans.fit$cluster)
component.days[, table(kmeans.cluster, is.weekend)]

viz <- list(
  weekdays=ggplot()+
    theme_bw()+
    ggtitle("logins per day, select days")+
    coord_equal()+
    scale_fill_gradient(low="white", high="blue", limits=c(0, max(days$logins)))+
    geom_tile(aes(week, weekday, fill=logins, clickSelects=date.str,
                  tooltip=paste(
                    logins,
                    "logins on",
                    weekday,
                    date.str)),
              data=days),
  selectedDays=ggplot()+
    theme_bw()+
    ggtitle("selected days")+
    guides(color="none")+
    geom_line(aes(hours.past.midnight, logins, showSelected=date.str,
                  color=is.weekend,
                  group=date.str),
              data=all.intervals),
  selector.types=list(date.str="multiple"),
  first=list(date.str=c("1970-02-02", "1970-02-03")),
  pca=ggplot()+
    theme_bw()+
    ggtitle("PCA of counts in 15-minute intervals")+
    scale_color_manual(values=c("1"="black","2"="white"))+
    geom_point(aes(Comp.1, Comp.2,
                   fill=is.weekend,
                   color=kmeans.cluster,
                   clickSelects=date.str),
               alpha=0.7,
               size=4,
               data=component.days))
animint2dir(viz, "figure-logins")
