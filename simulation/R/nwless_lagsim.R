## simulates the end-to-end lag of a typical client-server-based online videogame
## with the following assumptions:
## network delay: NO NETWORK DELAY AT ALL.
## (This is trying to decompose the model components and show 
## only frame/tick rate-related effects).
## fixed frame and tickrates
## specific exponentially distributed user input rate

## DONE: more than one simulation run per parameter setting
## DONE partially: don't couple sending the input events to the framerate but instead to the client's command send rate
#### command rate is still set to the same value as the server's tickrate

## TODO: make the delay, and input and other distributions exchangeable
## TODO: pure cloud gaming e2e simulation variant
## TODO: give the option to add more sources of delay to the e2e-lag, 
####     e.g. display lag, input device lag, video driver pipeline lag, input events that take more than one turn to process, ...
## TODO: aggregate results early (inside the parallel loop) to increase performance and reduce memload

library(ggplot2)
library(scales) # for muted()
library(latticeExtra) # for cloud and 3dbars
library(foreach)
library(parallel)
library(doParallel) # for foreach %dopar%

##################################################################################
## parameters

net.delay.mean <- 0         # one-way network delay (ms)
net.delay.sd   <- 0         # owd standard deviation (ms)

# does the server have a delay or do the events just snap to the next update event?
# Simulate tick *intervals* from 5 to 100 ms (i.e. 200 down to 10 Hz);
# convert to rates so as to interface with the rest of the sim script.
###server.tick.rates <- rep(seq(10,200,10),20)
server.tick.rates <- rep(rev(1000 / seq(5, 100, length.out=20)), 20)
server.delay.mean <- 0       # mean game server delay (ms)
server.delay.sd   <- 0       # mean game server delay (ms)

# As with `server.tick.rates`, use 5-100 ms frame *intervals*.
###client.frame.rates <- sort(rep(seq(10,200,10),20))
client.frame.rates <- sort(rep(1000 / seq(5, 100, length.out=20), 20))
client.input.rate   <- 20    # user input / sec
client.input.events <- 1000  # number of simulated user inputs

# client.command.rate        # for this sim defined as the same value as the server's tickrate

## extra cloud parameters
encode.delay <- 10
decode.delay <- 5
server.frame.rates <- c(5, 10, 15, 30, 60, 120)


##################################################################################
## set the path first if needed or if calling this file manually
setwd("git/onlinegame-lag-sim/simulation/R/")
source("onlinegaming-lag.R", chdir = TRUE)


##################################################################################
## execute the sim function for our parameter vectors
# parallel execution with foreach %dopar%

sim.rounds <- 10

registerDoParallel(cores = detectCores())

results <- foreach(round = 1:sim.rounds, .combine = rbind) %dopar% {
  results <- foreach(client.frame.rate = client.frame.rates, server.tick.rate = server.tick.rates, .combine = rbind) %do% {
    onlinegame.lagsim(client.frame.rate, server.tick.rate, net.delay.mean, net.delay.sd,
           server.delay.mean, server.delay.sd, client.input.rate, client.input.events)
  }
  results$round <- round
  results
}





##################################################################################
## execute the sim function for our parameter vectors
# parallel execution with foreach %dopar%

source("cloudgaming-lag.R", chdir = TRUE)

sim.rounds <- 10

registerDoParallel(cores = detectCores())

results <- foreach(round = 1:sim.rounds, .combine = rbind) %dopar% {
  results <- foreach(server.frame.rate = server.frame.rates, .combine = rbind) %do% {
    cloudgaming.lagsim(server.frame.rate, encode.delay, decode.delay, net.delay.mean, net.delay.sd,
                      server.delay.mean, server.delay.sd, client.input.rate, client.input.events)
  }
  results$round <- round
  results
}
ggplot(results, aes(x = e2e.lag, color = framerate)) + stat_ecdf()
ggsave("cloudgaming-lag-cdf.pdf")



##################################################################################
## alternate version of the main loop that attempts to aggregate early
## in order to increase execution speed
# TODO: Use data.table functionality instead of aggregate to speed things up
# library(data.table)
registerDoParallel(cores = detectCores())

sim.rounds <- 1000

results <- foreach(round = 1:sim.rounds, .combine = rbind) %dopar% {
  results <- foreach(client.frame.rate = client.frame.rates, server.tick.rate = server.tick.rates, .combine = rbind) %do% {
    lagsim(client.frame.rate, server.tick.rate, args)
  }
  results <- aggregate(e2e.lag ~ framerate + tickrate, data = results, FUN="median")
  results$round <- round
  results
}

results.mean <- aggregate(e2e.lag ~ framerate + tickrate, data = results, FUN="mean")

at <- seq(1, 20, length.out = 6)
labels <- c(10, 30, 60, 120, 144, 200)
at <- labels / 200 * 20

pdf(file="e2e-lag-3dbars.pdf", width=13, height=13)
print(cloud(e2e.lag~framerate+tickrate, results.mean, panel.3d.cloud=panel.3dbars,
            col.facet='grey', zlim=c(0,300), R.mat=rot,
            par.settings = list(axis.line = list(col = "transparent")),
            scales=list(arrows=FALSE, col=1, x=list(at=at, labels=labels), y=list(at=at, labels=labels))))
dev.off()


##################################################################################
## plotting
# ggplots
# ggplot(results, aes(x=e2e.lag, color = framerate, lty = tickrate)) + stat_ecdf(lwd=1)
#ggplot(results, aes(x=framerate, y=tickrate, z=e2e.lag)) + stat_summary2d()+ scale_fill_gradient2("e2e lag", high=muted("green"), trans="log", space="Lab")

# lattice.extra 3d bar plot
results.mean <- aggregate(e2e.lag ~ framerate + tickrate, data = results, FUN="mean")
rot <- diag(4)
rot[1,1] <- -1
rot[2,2] <- -1

pdf(file="e2e-lag-3dbars.pdf", width=13, height=13)
print(cloud(e2e.lag~framerate+tickrate, results.mean, panel.3d.cloud=panel.3dbars, col.facet='grey', R.mat=rot, par.settings = list(axis.line = list(col = "transparent")), scales=list(arrows=FALSE, col=1)))
dev.off()


## mean lag of the median lag of each sim round
# no significant deviation from overall mean
results.median.round <- aggregate(e2e.lag ~ framerate + tickrate + round, data = results, FUN="median")
results.median.mean <- aggregate(e2e.lag ~ framerate + tickrate, data = results.median.round, FUN="mean")
print(cloud(e2e.lag~framerate+tickrate, results.median.mean, panel.3d.cloud=panel.3dbars, col.facet='grey', R.mat=rot, par.settings = list(axis.line = list(col = "transparent")), scales=list(arrows=FALSE, col=1)))


## SD and coefficient of variation graphs
results.sd <- aggregate(e2e.lag ~ framerate + tickrate, data = results, FUN="sd")
print(cloud(e2e.lag ~ framerate+tickrate, results.sd, panel.3d.cloud=panel.3dbars, col.facet='grey', R.mat=rot, par.settings = list(axis.line = list(col = "transparent")), scales=list(arrows=FALSE, col=1)))
results.sd$varcoff <- results.sd$e2e.lag / results.mean$e2e.lag
print(cloud(varcoff ~ framerate+tickrate, results.sd, panel.3d.cloud=panel.3dbars, col.facet='grey', R.mat=rot, par.settings = list(axis.line = list(col = "transparent")), scales=list(arrows=FALSE, col=1)))



## TODO: convert axis from rates to IATs
# broken: just displays one line of results
results.mean$frameduration <- as.factor(1000/as.numeric(levels(results.mean$framerate)))
results.mean$tickduration <- as.factor(1000/as.numeric(levels(results.mean$tickrate)))
#results.mean <- aggregate(e2e.lag ~ frameduration + tickduration, data = results, FUN="mean")
 
print(cloud(e2e.lag~frameduration+tickduration, results.mean, panel.3d.cloud=panel.3dbars, col.facet='grey', R.mat=rot, par.settings = list(axis.line = list(col = "transparent")), scales=list(arrows=FALSE, col=1)))
