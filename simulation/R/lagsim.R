## simulates the end-to-end lag of a typical client-server-based online videogame
## with the following assumptions:
## network delay: gaussian distribution 
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

net.delay.mean <- 20         # one-way network delay (ms)
net.delay.sd   <- 5          # owd standard deviation (ms)

# does the server have a delay or do the events just snap to the next update event?
server.tick.rates <- rep(seq(10,200,10),20)
server.delay.mean <- 3       # mean game server delay (ms)
server.delay.sd   <- 0.1     # mean game server delay (ms)

client.frame.rates <- sort(rep(seq(10,200,10),20))
client.input.rate   <- 20    # user input / sec
client.input.events <- 1000  # number of simulated user inputs

# client.command.rate        # for this sim defined as the same value as the server's tickrate

sim.rounds <- 100


args <- list(net.delay.mean = net.delay.mean, net.delay.sd = net.delay.sd, 
             server.delay.mean = server.delay.mean, server.delay.sd = server.delay.sd, 
             client.input.rate = client.input.rate, client.input.events = client.input.events)


##################################################################################
## main sim function definition
lagsim <- function(client.frame.rate, server.tick.rate, args){
  ## init sim
  net.delay.mean      <- args$net.delay.mean
  net.delay.sd        <- args$net.delay.sd
  server.delay.mean   <- args$server.delay.mean
  server.delay.sd     <- args$server.delay.sd
  client.input.rate   <- args$client.input.rate
  client.input.events <- args$client.input.events
  
  
  client.command.rate <- server.tick.rate
  
  server.tick.timedelta  <- 1 / server.tick.rate  * 1000  # time interval between game ticks (ms)
  client.frame.timedelta <- 1 / client.frame.rate * 1000  # time interval between frames (ms)
  client.command.timedelta <- 1 / client.command.rate * 1000 # time interval between command messages (ms)
  
  ## first ticks are uniformly distributed in intervals
  server.tick.firsttick  <- runif(1) * server.tick.timedelta  # (ms)
  client.frame.firsttick <- runif(1) * client.frame.timedelta # (ms)
  client.command.firstmessage <- runif(1) * client.command.timedelta
  
  ## derived variables
  arrival.time   <- cumsum(rexp(client.input.events, rate = client.input.rate) * 1000)                  # arrivals time (in ms)
  finish.time    <- array(dim = client.input.events)                                                    # initialize array for the complete interaction finish time (in ms)
  net.delay.up   <- pmax(0, rnorm(client.input.events, mean = net.delay.mean, sd = net.delay.sd))       # (ms) network delay up
  net.delay.down <- pmax(0, rnorm(client.input.events, mean = net.delay.mean, sd = net.delay.sd))       # (ms) network delay down
  server.delay   <- pmax(0, rnorm(client.input.events, mean = server.delay.mean, sd = server.delay.sd)) # (ms) processing delay
  
  
  ##################################################################################
  ## main sim loop
  # in its current form represents the case of a client-server online videogame
  for(i in 1:client.input.events){
    
    # Describes the time, the client sends the input to the server.
    # At the moment this is the next time the client renders a frame,
    # TODO: but it should be changed to the next command message send interval.
    # client.frame.time <- ceiling((arrival.time[i] -  client.frame.firsttick) / client.frame.timedelta) * client.frame.timedelta + client.frame.firsttick
    client.command.time <- ceiling((arrival.time[i] -  client.command.firstmessage) / client.command.timedelta) * client.command.timedelta + client.command.firstmessage
    
    # arrival time of the command message at the server
    server.input.arrival.time <- client.command.time + net.delay.up[i]
    
    # Calculates the next time the server 'ticks' after the command has arrived.
    # Determined by the tickrate and the time of the first tick
    server.tick.time <- ceiling((server.input.arrival.time - server.tick.firsttick) / server.tick.timedelta) * server.tick.timedelta + server.tick.firsttick 
    
    # time the processing of the game state after the server 'ticks' is completed
    server.tick.processing.end.time <- server.tick.time + server.delay[i]
    
    # time the update message arrives back at the client
    client.update.arrival.time <- server.tick.processing.end.time + net.delay.down[i]
    
    # The update needs to be available before the frame is being processed/renderen
    # We therefore take the time of the frame *after* the next (i + 1) to accommodate this.
    client.finish.frame.time <- (ceiling((client.update.arrival.time - client.frame.firsttick) / client.frame.timedelta) + 1) * client.frame.timedelta + client.frame.firsttick 
    
    # TODO: Add constant screen delay here
    
    # Write the finish time back into the array
    finish.time[i] <- client.finish.frame.time
  }
  
  # full end-to-end lag calculates from the difference between start and end time
  e2e.lag <- finish.time - arrival.time
  
  # Combine all data into a data.frame and return it
  results <- data.frame(e2e.lag = e2e.lag, framerate = as.factor(client.frame.rate), tickrate = as.factor(server.tick.rate))
  return(results)
}


##################################################################################
## execute the sim function for our parameter vectors
# parallel execution with foreach %dopar%

registerDoParallel(cores = detectCores())

results <- foreach(round = 1:sim.rounds, .combine = rbind) %dopar% {
  results <- foreach(client.frame.rate = client.frame.rates, server.tick.rate = server.tick.rates, .combine = rbind) %do% {
    lagsim(client.frame.rate, server.tick.rate, args)
  }
  results$round <- round
  results
}



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
print(cloud(e2e.lag~framerate+tickrate, results.mean, panel.3d.cloud=panel.3dbars, col.facet='grey', zlim=c(0,300),R.mat=rot, par.settings = list(axis.line = list(col = "transparent")), scales=list(arrows=FALSE, col=1, x=list(at=at, labels=labels), y=list(at=at, labels=labels))))
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
