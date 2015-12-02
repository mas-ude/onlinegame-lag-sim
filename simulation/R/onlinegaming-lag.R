## simulates the end-to-end lag of a typical client-server-based online videogame
## with the following assumptions:
## network delay: gaussian distribution 
## fixed frame and tickrates
## specific exponentially distributed user input rate

##################################################################################
## main sim function definition
onlinegame.lagsim <- function(client.frame.rate, server.tick.rate, 
                              net.delay.mean, net.delay.sd, 
                              server.delay.mean, server.delay.sd,
                              client.input.rate, client.input.events, 
                              client.command.rate = server.tick.rate){
  
  ## init sim
  server.tick.timedelta    <- 1 / server.tick.rate    * 1000 # time interval between game ticks (ms)
  client.frame.timedelta   <- 1 / client.frame.rate   * 1000 # time interval between frames (ms)
  client.command.timedelta <- 1 / client.command.rate * 1000 # time interval between command messages (ms)
  
  ## first ticks are uniformly distributed in intervals
  server.tick.firsttick  <- runif(1)      * server.tick.timedelta
  client.frame.firsttick <- runif(1)      * client.frame.timedelta
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
    
    # The update needs to be available before the frame is being processed/rendered
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