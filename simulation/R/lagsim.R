library(ggplot2)

## parameters

par.tickrate <- 60      # Hz=1/s
par.framerate <- 60     # Hz=1/s
par.lambda <- 20         # user input / sec
par.net.mean <- 20       # one-way network delay (ms)
par.net.sd <- 5          # owd standard deviation (ms)
par.server.mean <- 3     # mean game server delay (ms)
par.server.sd <- 0.1    # mean game server delay (ms)
par.userinput.events <- 100 # number of simulated user inputs



## init sim
# y <- rep(0, par.userinput.events)     # e2e delays per user input
tickrate.timedelta <- 1 / par.tickrate  * 1e3  # time interval between game ticks (ms) (ts)
framerate.timedelta <- 1 / par.framerate * 1e3 # time interval between frame ticks (ms) (tr)

## first ticks are uniformly distributed in intervals
tickrate.timedelta.0 <- runif(1) * tickrate.timedelta   # (ms)
framerate.timedelta.0 <- runif(1) * framerate.timedelta # (ms)

## run sim
A <- cumsum(rexp(par.userinput.events, rate = par.lambda) * 1e3)  # arrivals time points (ms)
F <- cumsum(rexp(par.userinput.events, rate = par.lambda))        # finish time points (ms)
net.delay.up   <- pmax(0, rnorm(par.userinput.events, mean = par.net.mean, sd = par.net.sd))             # (ms) network delay up D(:,1) and down D(:,2)
net.delay.down <- pmax(0, rnorm(par.userinput.events, mean = par.net.mean, sd = par.net.sd))             # (ms) network delay up D(:,1) and down D(:,2)
B <- pmax(0, rnorm(par.userinput.events, mean = par.server.mean, sd = par.server.sd))       # (ms) processing delay
W <- rep(0, times = 3 * par.userinput.events)                     # waiting times (ms) due to discretization
W <- matrix(W, ncol = 3)                                          # convert to matrix (alternatively use df)


for(i in 1:par.userinput.events){
  
  nextTickR <- ceiling((A[i] -   framerate.timedelta.0) / framerate.timedelta) * framerate.timedelta + framerate.timedelta.0 # waiting for input and processing in next tick
  W[i,1] <- nextTickR - A[i]           # (ms)
  arrivalAtServer <- nextTickR + net.delay.up[1] # uplink
  
  nextTickS <- ceiling((arrivalAtServer - tickrate.timedelta.0) / tickrate.timedelta) * tickrate.timedelta + tickrate.timedelta.0 # processing input at next game tick
  W[i,2] <- nextTickS - arrivalAtServer # (ms)
  
  arrivalAtClient <- nextTickS + B[i] + net.delay.down[i]
  displayTickR <- (ceiling((arrivalAtClient - framerate.timedelta.0) / framerate.timedelta) + 1) * framerate.timedelta + framerate.timedelta.0 # render frame in next-next frame
  W[i,3] <- displayTickR - arrivalAtClient
  
  F[i] <- displayTickR
}

e2e.lag <- F - A

df <- data.frame(e2e.lag = e2e.lag, framerate = par.framerate)
ggplot(df, aes(x=e2e.lag)) + stat_ecdf(lwd=1)





# res.F <- F
# res.A <- A
# res.B <- B
# res.D <- D
# res.W <- W
# res.tickrate.timedelta.0 <- tickrate.timedelta.0
# res.tickrate.timedelta <- tickrate.timedelta
# res.tickrate.totalticks <- ceiling((max(F) - tickrate.timedelta.0) / tickrate.timedelta) # number of ticks

# res.framerate.timedelta.0 <- framerate.timedelta.0 # first tick
# res.framerate.timedelta <- framerate.timedelta # tick interval
# res.framerate.totalticks <- ceiling((max(F) - framerate.timedelta.0) / framerate.timedelta) # number of ticks
