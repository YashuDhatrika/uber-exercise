source("packages.R")

logins.list <- fromJSON("logins.json")
logins.vec <- strptime(logins.list$login_time, "%Y-%m-%d %H:%M:%S")
logins.dt <- data.table(time=as.POSIXct(logins.vec))
logins.dt[, date.str := strftime(time, "%Y-%m-%d")]
weekday.levs <-
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
    "Saturday", "Sunday")
logins.dt[, weekday.str := strftime(time, "%A")]
logins.dt[, weekday := factor(weekday.str, weekday.levs)]
logins.dt[, minutes15 := as.numeric(strftime(time, "%M")) %/% 15]
logins.dt[, min.minute := minutes15*15]
logins.dt[, max.minute := (minutes15+1)*15-1]
logins.dt[, hour.str := strftime(time, "%Y-%m-%d %H:")]
logins.dt[, min.time := as.POSIXct(strptime(paste0(hour.str, min.minute), "%Y-%m-%d %H:%M"))]
logins.dt[, week := as.numeric(strftime(time, "%V"))]

some.intervals <- logins.dt[, list(
  logins=.N
  ), by=min.time]
ggplot()+
  geom_bar(aes(min.time, logins),
           stat="identity",
           data=some.intervals)

grid.intervals <- data.table(
  min.time=some.intervals[, seq(min(min.time), max(min.time), by=15*60)])
setkey(grid.intervals, min.time)
setkey(some.intervals, min.time)
all.intervals <- some.intervals[grid.intervals]
all.intervals[is.na(logins), logins := 0]
all.intervals[, date.str := strftime(min.time, "%Y-%m-%d")]
all.intervals[, hours.num := as.numeric(strftime(min.time, "%H"))]
all.intervals[, minutes.num := as.numeric(strftime(min.time, "%M"))]
all.intervals[, hours.past.midnight := hours.num + minutes.num/60]
all.intervals[, week := as.numeric(strftime(min.time, "%V"))]
all.intervals[, weekday.str := strftime(min.time, "%A")]
all.intervals[, weekday := factor(weekday.str, weekday.levs)]
ggplot()+
  geom_line(aes(min.time, logins),
            data=all.intervals)

days <- all.intervals[, list(
  logins=sum(logins),
  intervals=.N
  ), by=.(date.str, weekday, week)]
ggplot()+
  coord_equal()+
  scale_fill_gradient(low="white", high="blue")+
  geom_tile(aes(week, weekday, fill=logins),
            data=days)

save(logins.dt, all.intervals, days, file="logins.RData")
