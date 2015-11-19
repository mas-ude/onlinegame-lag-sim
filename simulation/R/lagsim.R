## simulates the end-to-end lag of a typical client-server-based online videogame
## with the following assumptions:
## network delay: gaussian distribution 
## fixed frame and tickrates
## specific exponentially distributed user input rate

## TODO: make the delay, and input and other distributions exchangeable
## TODO: don't couple sending the input events to the framerate but instead to the client's command send rate
## TODO: incorporate the lag on an additonal game streaming path (i.e. simulate cloud gaming)
## TODO: multiple runs, plotted into one set of figures, e.g. also a boxplot, or heatmaps for 2d relations (server and client tickrates)
## TODO: give the option to add more sources of delay to the e2e-lag, 
##       e.g. display lag, input device lag, video driver pipeline lag, input events that take more than one turn to process, ...

library(ggplot2)
library(scales) # for muted()

## parameters

net.delay.mean <- 20         # one-way network delay (ms)
net.delay.sd   <- 5          # owd standard deviation (ms)

# does the server have a delay or do the events just snap to the next update event?
server.tickrates  <- seq(10,200,10) #<- c(10,20,30,60,120)      # Hz=1/s
server.delay.mean <- 3       # mean game server delay (ms)
server.delay.sd   <- 0.1     # mean game server delay (ms)

client.framerates <- seq(10,200,10)    # <- c(5, 15, 30, 60, 120, 144)    # Hz=1/s
client.input.rate   <- 20    # user input / sec
client.input.events <- 1000 # number of simulated user inputs

results <- data.frame()

for (server.tickrate in server.tickrates){
  for (client.framerate in client.framerates){
    ## init sim
    
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
    df <- data.frame(e2e.lag = e2e.lag, framerate = as.factor(client.framerate), tickrate = as.factor(server.tickrate))
    results <- rbind(results, df)
  }
  
}


## plotting
ggplot(results, aes(x=e2e.lag, color = framerate, lty = tickrate)) + stat_ecdf(lwd=1)
ggplot(results, aes(x=framerate, y=tickrate, z=e2e.lag)) + stat_summary2d()+ scale_fill_gradient2("e2e lag", high=muted("green"))

