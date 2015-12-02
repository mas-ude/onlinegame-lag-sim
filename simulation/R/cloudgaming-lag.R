## simulates the end-to-end lag of a typical cloud gaming scenario
## with the following assumptions:
## network delay: gaussian distribution 
## fixed frame and tickrates
## specific exponentially distributed user input rate

##################################################################################
## main sim function definition
cloudgaming.lagsim <- function(server.frame.rate, encode.delay, decode.delay,
                              net.delay.mean, net.delay.sd, 
                              server.delay.mean, server.delay.sd,
                              client.input.rate, client.input.events, 
                              client.command.rate = server.tick.rate){
  
  ## init sim
  client.command.rate <- server.frame.rate
  
  client.command.timedelta <- 1 / client.command.rate * 1000 # time interval between command messages
  server.frame.timedelta   <- 1 / server.frame.rate   * 1000 # time interval between frames
  
  ## first ticks are uniformly distributed in intervals
  client.command.firstmessage <- runif(1) * client.command.timedelta
  server.frame.firsttick      <- runif(1) * server.frame.timedelta
  
  ## transmission time of each frame
  server.frame.transmission.time <- 1000 / server.frame.rate 
  
  ## derived variables
  arrival.time   <- cumsum(rexp(client.input.events, rate = client.input.rate) * 1000)                  # arrivals time (in ms)
  finish.time    <- array(dim = client.input.events)                                                    # initialize array for the complete interaction finish time (in ms)
  net.delay.up   <- pmax(0, rnorm(client.input.events, mean = net.delay.mean, sd = net.delay.sd))       # (ms) network delay up
  net.delay.down <- pmax(0, rnorm(client.input.events, mean = net.delay.mean, sd = net.delay.sd))       # (ms) network delay down
  server.delay   <- pmax(0, rnorm(client.input.events, mean = server.delay.mean, sd = server.delay.sd)) # (ms) processing delay
#  decode.delay   <- pmax(0, rnorm(client.input.events, mean = server.delay.mean, sd = server.delay.sd)) # (ms) processing delay
  
  ##################################################################################
  ## main sim loop
  # in its current form represents the case of a client-server online videogame
  for(i in 1:client.input.events){
    
    # Describes the time, the client sends the input to the server.
    # At the moment this is the next time the client renders a frame,
    # TODO: but it should be changed to the next command message send interval.
    client.command.time <- ceiling((arrival.time[i] -  client.command.firstmessage) / client.command.timedelta) * client.command.timedelta + client.command.firstmessage
    
    # arrival time of the command message at the server
    server.input.arrival.time <- client.command.time + net.delay.up[i]
    
    # time the processing of the input event is completed
    server.processing.end.time <- server.input.arrival.time + server.delay[i]

    # Calculates the next time the game on the cloud server renders a new frame
    server.frame.time <- ceiling((server.processing.end.time - server.frame.firsttick) / server.frame.timedelta) * server.frame.timedelta + server.frame.firsttick 
    
    # Add a constant overhead to account for th
    server.frame.encode.time <- server.frame.time + encode.delay
    
    # time the update message arrives back at the client
    # in addition to the propagation delay, we need to add the time it takes
    # to transmit the whole frame over the wire over multiple packets
    client.frame.arrival.time <- server.frame.time + server.frame.transmission.time + net.delay.down[i]
    
    # Assume that frames can be 'instantly' display at the client and do not
    # have to wait for another lockstep process
    client.display.time <- client.frame.arrival.time + decode.delay
    
    # TODO: Add constant screen delay here
    
    # Write the finish time back into the array
    finish.time[i] <- client.display.time
  }
  
  # full end-to-end lag calculates from the difference between start and end time
  e2e.lag <- finish.time - arrival.time
  
  # Combine all data into a data.frame and return it
  results <- data.frame(e2e.lag = e2e.lag, framerate = as.factor(server.frame.rate))
  return(results)
}