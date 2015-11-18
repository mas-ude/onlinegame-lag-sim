## parameters

par.tickrate <- 60      # Hz=1/s
par.framerate <- 60     # Hz=1/s
par.lambda <- 20         # user input / sec
par.net.mean <- 20       # one-way network delay (ms)
par.net.sd <- 5          # owd standard deviation (ms)
par.server.mean <- 3     # mean game server delay (ms)
par.server.std <- 0.1    # mean game server delay (ms)
par.userinput.events <- 100 # number of simulated user inputs





sim <- function(){

  ## init sim
  y <- rep(0, par.userinput.events)     # e2e delays per user input
  tickrate.timedelta <- 1 / par.tickrate  * 1e3  # time interval between game ticks (ms) (ts)
  framerate.timedelta <- 1 / par.framerate * 1e3 # time interval between frame ticks (ms) (tr)
  
  ## first ticks are uniformly distributed in intervals
  tickrate.timedelta.0 <- runif(1) * tickrate.timedelta   # (ms)
  framerate.timedelta.0 <- runif(1) * framerate.timedelta # (ms)
  
  ## run sim
  A <- cumsum(rexp(par.userinput.events, rate = par.lambda) * 1e3)  # arrivals time points (ms)
  F <- cumsum(rexp(par.userinput.events, rate = par.lambda))        # finish time points (ms)
  D.down <- max(0, rnorm(par.userinput.events, mean = par.net.mean, sd = par.net.sd))             # (ms) network delay up D(:,1) and down D(:,2)
  D.up   <- max(0, rnorm(par.userinput.events, mean = par.net.mean, sd = par.net.sd))             # (ms) network delay up D(:,1) and down D(:,2)
  B <- max(0, rnorm(par.userinput.events, mean = par.server.mean, sd = par.server.sd))       # (ms) processing delay
  W <- rep(0, times = 3 * par.userinput.events)                     # waiting times (ms) due to discretization
  W <- matrix(W, ncol = 3)                                          # convert to matrix (alternatively use df)
  
}




# %% run sim
# D=max(0,normrnd(par.net.mean,par.net.std,par.N,2)); % (ms) network delay up D(:,1) and down D(:,2)
# B=max(0,normrnd(par.server.mean,par.server.std,par.N,1)); % (ms) processing delay
# W=zeros(par.N,3); % waiting times (ms) due to discretization
# for i=1:par.N
# nextTickR = ceil((A(i)-tr0)/tr)*tr+tr0; % waiting for input and processing in next tick
# W(i,1)=nextTickR - A(i); % (ms)
# arrivalAtServer = nextTickR+D(i,1); % uplink
# 
# nextTickS = ceil((arrivalAtServer-ts0)/ts)*ts+ts0; % processing input at next game tick
# W(i,2)=nextTickS - arrivalAtServer; % (ms)
# 
# arrivalAtClient = nextTickS + B(i) + D(i,2);
# displayTickR = (ceil((arrivalAtClient-tr0)/tr)+1)*tr+tr0; % render frame in next-next frame
# W(i,3)=displayTickR-arrivalAtClient;
# 
# F(i)=displayTickR;
# end
# 
# res.y=F-A;
# res.F=F;
# res.A=A;
# res.B=B;
# res.D=D;
# res.W=W;
# res.ts0=ts0;
# res.ts=ts;
# res.tsi=ceil((max(F)-ts0)/ts); % number of ticks
# 
# res.tr0=tr0; % first tick
# res.tr=tr; % tick interval
# res.tri=ceil((max(F)-tr0)/tr); % number of ticks
