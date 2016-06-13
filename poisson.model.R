## IDEA: train on Feb, test on March. use poisson loss to evaluate
## predictions.

## IDEA: use glmnet poisson regression to predict next 4 times, based
## on features such as: counts in the prev few bins, indicator for
## interval, indicator for hour, spline basis on hours past midnight, 

source("packages.R")

load("logins.RData")

uniq.list <- list()
for(var.name in c("hours.num", "minutes.num")){
  uniq.list[[var.name]] <- unique(all.intervals[[var.name]])
}
    
feature.mat.list <- list()
output.mat.list <- list()
feature.i.vec <- all.intervals[,10:(.N-3)]
for(train.row.i in feature.i.vec){
  cat(sprintf("%4d / %4d features computed\n",
              train.row.i, length(feature.i.vec)))
  output.i.vec <- train.row.i:(train.row.i+3)
  output.mat.list[[paste(train.row.i)]] <- all.intervals[output.i.vec, logins]
  train.row <- all.intervals[train.row.i,]
  feature.vec.list <- list(
    prev.count=all.intervals[train.row.i-1, logins],
    prev5.mean=all.intervals[(train.row.i-5):(train.row.i-1), mean(logins),],
    is.weekday=train.row$weekday %in% c("Sunday", "Saturday"))
  for(var.name in names(uniq.list)){
    u.vec <- uniq.list[[var.name]]
    feature.vec.list[paste(var.name, u.vec)] <- train.row[[var.name]]==u.vec
  }
  feature.mat.list[[paste(train.row.i)]] <- do.call(c, feature.vec.list)
}
feature.mat <- do.call(rbind, feature.mat.list)
output.mat <- do.call(rbind, output.mat.list)

is.train <- 1:nrow(feature.mat) < 5000
pred.dt.list <- list()
for(output.i in 1:ncol(output.mat)){
  train.labels <- output.mat[is.train, output.i]
  train.features <- feature.mat[is.train,]
  fit <- cv.glmnet(
    train.features, train.labels, family="poisson")
  pred.vec <- predict(fit, feature.mat, type="response")
  intervals <- all.intervals[feature.i.vec+output.i-1,]
  intervals$pred.lo <- qpois(0.25, pred.vec)
  intervals$pred.med <- qpois(0.5, pred.vec)
  intervals$pred.hi <- qpois(0.75, pred.vec)
  intervals$output.i <- output.i
  intervals$is.train <- is.train
  pred.dt.list[[output.i]] <- intervals
}

pred.dt <- do.call(rbind, pred.dt.list)
pred.dt[, set := ifelse(is.train, "train", "test")]
abline.dt <- data.table(slope=1, intercept=0)
ggplot()+
  geom_abline(aes(slope=slope, intercept=intercept),
              data=abline.dt,
              color="grey")+
  coord_equal()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ output.i)+
  geom_point(aes(logins, pred.med, color=is.train),
             shape=1,
             data=pred.dt)

ggplot()+
  geom_abline(aes(slope=slope, intercept=intercept),
              data=abline.dt,
              color="grey")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set ~ output.i)+
  geom_hex(aes(logins, pred.med),
           data=pred.dt)

bin.size <- 5
pred.dt[, pred.med.bin.lo := (pred.med %/% bin.size) * bin.size]
pred.dt[, pred.med.bin.hi := pred.med.bin.lo + bin.size]
pred.dt[, logins.bin.lo := (logins %/% bin.size) * bin.size]
pred.dt[, logins.bin.hi := logins.bin.lo + bin.size]
pred.dt[, rect.id := sprintf(
                    "set=%s output=%d logins[%d,%d] pred[%d,%d]",
                    set, output.i,
                    logins.bin.lo, logins.bin.hi,
                    pred.med.bin.hi, pred.med.bin.lo)]

rect.dt <- pred.dt[, list(
  count=.N
  ), by=.(rect.id, set, output.i, logins.bin.lo, logins.bin.hi, pred.med.bin.hi, pred.med.bin.lo)]

days[, is.weekend := weekday %in% c("Saturday", "Sunday")]
all.intervals[, is.weekend := weekday %in% c("Saturday", "Sunday")]
addY <- function(dt, y.value){
  dt$Y <- factor(y.value, c("residuals", "logins"))
  dt
}
tallrect.dt <- data.table(
  hours.past.midnight=unique(all.intervals$hours.past.midnight))
hline.dt <- data.table(yintercept=0)
viz <- list(
  residuals=ggplot()+
    ggtitle("Prediction error plot")+
    xlab("observed number of logins")+
    ylab("predicted number of logins")+             
    geom_abline(aes(slope=slope, intercept=intercept),
                data=abline.dt,
                color="grey")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(set ~ output.i, labeller=function(var, val){
      paste(var, val)
    })+
    geom_rect(aes(xmin=logins.bin.lo, xmax=logins.bin.hi,
                  ymin=pred.med.bin.lo, ymax=pred.med.bin.hi,
                  clickSelects=rect.id,
                  tooltip=paste0("count=", count, " ", rect.id),
                  fill=log10(count)),
              size=1,
              data=rect.dt)+
    geom_point(aes(logins, pred.med, showSelected=date.str),
               fill=NA,
               data=pred.dt)+
    theme_animint(height=800)+
    scale_fill_gradient(low="grey80", high="red"),
  weekdays=ggplot()+
    theme_bw()+
    ggtitle("logins per day, select day")+
    coord_equal()+
    scale_fill_gradient(
      low="white", high="blue", limits=c(0, max(days$logins)))+
    geom_tile(aes(week, weekday, fill=logins, clickSelects=date.str,
                  tooltip=paste(
                    logins,
                    "logins on",
                    weekday,
                    date.str)),
              data=days)+
    geom_point(aes(week, weekday, showSelected=rect.id),
               size=2,
               fill="white",
               data=pred.dt),
  selectedDays=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(height=800, width=800)+
    facet_grid(Y ~ ., scales="free")+
    ylab("")+
    ggtitle("selected day, select prediction")+
    geom_hline(aes(yintercept=yintercept),
               color="grey",
               data=addY(hline.dt, "residuals"))+
    geom_tallrect(aes(xmin=hours.past.midnight-1/8,
                      xmax=hours.past.midnight+1/8,
                      clickSelects=hours.past.midnight),
                  data=tallrect.dt,
                  alpha=0.5)+
    geom_ribbon(aes(hours.past.midnight+(output.i-1)/4,
                    ymax=pred.hi,
                    ymin=pred.lo,
                    showSelected=hours.past.midnight,
                    showSelected2=date.str),
                fill="grey",
                chunk_vars="date.str",
                color="green",
                alpha=0.5,
                size=0.5,
                data=addY(pred.dt, "logins"))+
    geom_line(aes(hours.past.midnight+(output.i-1)/4,
                  pred.med-logins,
                  group=output.i,
                  showSelected=date.str),
              data=addY(pred.dt, "residuals"))+
    geom_line(aes(hours.past.midnight, logins, showSelected=date.str,
                  group=date.str),
              color="blue",
              data=addY(all.intervals, "logins"))+
    xlab("hours past midnight")+
    coord_cartesian(ylim=c(-10,110))+
    geom_line(aes(hours.past.midnight+(output.i-1)/4,
                  pred.med,
                  showSelected=hours.past.midnight,
                  showSelected2=date.str),
              color="green",
              size=1,
              alpha=0.5,
              chunk_vars="date.str",
              data=addY(pred.dt, "logins")),
  selector.types=list(rect.id="multiple"),
  time=list(variable="hours.past.midnight", ms=1000),
  duration=list(hours.past.midnight=1000),
  first=list(rect.id=rect.dt[order(-pred.med.bin.lo), rect.id][1]))
animint2dir(viz, "figure-poisson-model")
