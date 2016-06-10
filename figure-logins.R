source("packages.R")

load("logins.RData")

viz <- list(
  weekdays=ggplot()+
    ggtitle("logins per day")+
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
    ggtitle("selected days")+
    geom_line(aes(hours.past.midnight, logins, showSelected=date.str,
                  group=date.str),
              data=all.intervals),
  selector.types=list(date.str="multiple"),
  first=list(date.str=c("1970-02-02", "1970-02-03")))
animint2dir(viz, "figure-logins")
