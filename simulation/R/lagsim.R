## simulates the end-to-end lag of a typical client-server-based online videogame
## with the following assumptions:
## network delay: gaussian distribution 
## fixed frame and tickrates
## specific exponentially distributed user input rate

## TODO: make the delay, and input and other distributions exchangeable
## TODO: don't couple sending the input events to the framerate but instead to the client's command send rate
## TODO: incorporate the lag on an additonal game streaming path (i.e. simulate cloud gaming)
## TODO: more than one simulation run per parameter setting
## TODO: give the option to add more sources of delay to the e2e-lag, 
##       e.g. display lag, input device lag, video driver pipeline lag, input events that take more than one turn to process, ...

library(ggplot2)
library(scales) # for muted()
library(latticeExtra) # for cloud and 3dbars
library(parallel) # for clusterMap


##################################################################################
## parameters

net.delay.mean <- 20         # one-way network delay (ms)
net.delay.sd   <- 5          # owd standard deviation (ms)

# does the server have a delay or do the events just snap to the next update event?
server.tickrates <- rep(seq(10,200,10),20)
server.delay.mean <- 3       # mean game server delay (ms)
server.delay.sd   <- 0.1     # mean game server delay (ms)

client.framerates <- sort(rep(seq(10,200,10),20))
client.input.rate   <- 20    # user input / sec
client.input.events <- 1000 # number of simulated user inputs

sim.rounds <- 1000


args <- list(net.delay.mean = net.delay.mean, net.delay.sd = net.delay.sd, 
             server.delay.mean = server.delay.mean, server.delay.sd = server.delay.sd, 
             client.input.rate = client.input.rate, client.input.events = client.input.events)


##################################################################################
## main sim function definition
lagsim <- function(client.framerate, server.tickrate, args){
  ## init sim
  net.delay.mean      <- args$net.delay.mean
  net.delay.sd        <- args$net.delay.sd
  server.delay.mean   <- args$server.delay.mean
  server.delay.sd     <- args$server.delay.sd
  client.input.rate   <- args$client.input.rate
  client.input.events <- args$client.input.events
  
  server.tickrate.timedelta  <- 1 / server.tickrate  * 1e3 # time interval between game ticks (ms) (ts)
  client.framerate.timedelta <- 1 / client.framerate * 1e3 # time interval between frame ticks (ms) (tr)
  
  ## first ticks are uniformly distributed in intervals
  server.tickrate.firsttick  <- runif(1) * server.tickrate.timedelta  # (ms)
  client.framerate.firsttick <- runif(1) * client.framerate.timedelta # (ms)
  
  ## derived variables
  arrival.time   <- cumsum(rexp(client.input.events, rate = client.input.rate) * 1e3) # arrivals time points (ms)
  finish.time    <- cumsum(rexp(client.input.events, rate = client.input.rate))       # finish time points (ms)
  net.delay.up   <- pmax(0, rnorm(client.input.events, mean = net.delay.mean, sd = net.delay.sd)) # (ms) network delay up
  net.delay.down <- pmax(0, rnorm(client.input.events, mean = net.delay.mean, sd = net.delay.sd)) # (ms) network delay down
  server.delay   <- pmax(0, rnorm(client.input.events, mean = server.delay.mean, sd = server.delay.sd))        # (ms) processing delay
  
  
  ##################################################################################
  ## main sim loop
  for(i in 1:client.input.events){
    
    # waiting for input and processing in next tick
    client.frame.time <- ceiling((arrival.time[i] -  client.framerate.firsttick) / client.framerate.timedelta) * client.framerate.timedelta + client.framerate.firsttick 
    server.input.arrival.time <- client.frame.time + net.delay.up[1] # uplink
    
    # processing input at next game tick
    server.tick.time <- ceiling((server.input.arrival.time - server.tickrate.firsttick) / server.tickrate.timedelta) * server.tickrate.timedelta + server.tickrate.firsttick 
    
    client.update.arrival.time <- server.tick.time + server.delay[i] + net.delay.down[i]
    # render frame in next-next frame
    client.finish.frame.time <- (ceiling((client.update.arrival.time - client.framerate.firsttick) / client.framerate.timedelta) + 1) * client.framerate.timedelta + client.framerate.firsttick 
    
    finish.time[i] <- client.finish.frame.time
  }
  
  e2e.lag <- finish.time - arrival.time
  results <- data.frame(e2e.lag = e2e.lag, framerate = as.factor(client.framerate), tickrate = as.factor(server.tickrate))
  return(results)
}


##################################################################################
## execute the sim function for our parameter vectors
# parallel execution with clusterMap

df <- data.frame()

cluster <- makeCluster(detectCores())

for(i in sim.rounds){
  results <- clusterMap(cluster, lagsim, client.framerate = client.framerates, server.tickrate = server.tickrates, MoreArgs = list(args))
  results <- do.call('rbind', results)
  df <- rbind(df, results)
}

stopCluster(cluster)
results <- df

##################################################################################
## plotting
# ggplots
ggplot(results, aes(x=e2e.lag, color = framerate, lty = tickrate)) + stat_ecdf(lwd=1)
ggplot(results, aes(x=framerate, y=tickrate, z=e2e.lag)) + stat_summary2d()+ scale_fill_gradient2("e2e lag", high=muted("green"), trans="log", space="Lab")

# lattice.extra 3d bar plot
results.mean <- aggregate(e2e.lag ~ framerate + tickrate, data = results, FUN="mean")
rot <- diag(4)
rot[1,1] <- -1
rot[2,2] <- -1
cloud(e2e.lag~framerate+tickrate, results.mean, panel.3d.cloud=panel.3dbars, col.facet='grey', R.mat=rot, par.settings = list(axis.line = list(col = "transparent")), scales=list(arrows=FALSE, col=1))
