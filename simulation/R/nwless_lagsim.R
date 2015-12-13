## simulates the end-to-end lag of a simplified locally running game using the onlinegame.lag sim
## with the following assumptions:
## network delay: NO NETWORK DELAY AT ALL.
## (This is trying to decompose the model components and show 
## only frame/tick rate-related effects).
## fixed frame and tickrates
## specific exponentially distributed user input rate


library(ggplot2)
library(RColorBrewer)
library(scales) # for muted()
library(latticeExtra) # for cloud and 3dbars
library(foreach)
library(doParallel) # for foreach %dopar%
library(sysfonts)
library(Cairo)

##################################################################################
## parameters

number_of_repetitions <- 100  # number of repetitions of experiment 
                              # for each frame and tick rate

net.delay.mean <- 0         # one-way network delay (ms)
net.delay.sd   <- 0         # owd standard deviation (ms)

# does the server have a delay or do the events just snap to the next update event?
# Simulate tick *intervals* from 5 to 100 ms (i.e. 200 down to 10 Hz);
# convert to rates so as to interface with the rest of the sim script.
###server.tick.rates <- rep(seq(10,200,10),20)
server.tick.rates <- rep(rev(1000 / seq(5, 100, length.out=20)), number_of_repetitions)
server.delay.mean <- 0       # mean game server delay (ms)
server.delay.sd   <- 0       # mean game server delay (ms)

# As with `server.tick.rates`, use 5-100 ms frame *intervals*.
###client.frame.rates <- sort(rep(seq(10,200,10),20))
client.frame.rates <- sort(rep(1000 / seq(5, 100, length.out=20), number_of_repetitions))
client.input.rate   <- 20    # user input / sec
client.input.events <- 1000  # number of simulated user inputs

# client.command.rate        # for this sim defined as the same value as the server's tickrate



##################################################################################
## set the path first if needed or if calling this file manually
#setwd("git/onlinegame-lag-sim/simulation/R/")
source("onlinegaming-lag.R", chdir = TRUE)


##################################################################################
## execute the sim function for our parameter vectors
## alternate version of the main loop that attempts to aggregate early
## in order to increase execution speed
# TODO: Use data.table functionality instead of aggregate to speed things up
# library(data.table)
registerDoParallel(cores = detectCores())

sim.rounds <- 1000

results <- foreach(round = 1:sim.rounds, .combine = rbind, .packages="foreach") %dopar% {
  results <- foreach(client.frame.rate = client.frame.rates, server.tick.rate = server.tick.rates, .combine = rbind) %do% {
    onlinegame.lagsim(client.frame.rate, server.tick.rate, net.delay.mean, net.delay.sd,
                      server.delay.mean, server.delay.sd, client.input.rate, 
                      client.input.events, server.tick.rate)
                      # We take the client command rate to equal the server tick rate
  }
  results <- aggregate(e2e.lag ~ framerate + tickrate, data = results, FUN="median")
  results$round <- round
  results
}

## save/load the data 
saveRDS(results, "../data/nwless-lagsim-1000rounds.rds")
# results <- readRDS("../data/nwless-lagsim-1000rounds.rds")


##################################################################################
## plotting

#results.sampled <- sample_n(results, nrow(results)*0.1)
results.mean <- aggregate(e2e.lag ~ framerate + tickrate, data = results, FUN="mean")

results.mean$fr2 <- factor(results.mean$framerate, levels = as.character(sort(as.numeric(levels(results.mean$framerate)), decreasing = TRUE)))
results.mean$tr2 <- factor(results.mean$tickrate, levels = as.character(sort(as.numeric(levels(results.mean$tickrate)), decreasing = TRUE)))

colour.count = length(unique(results.mean$tickrate))
get.palette = colorRampPalette(brewer.pal(9, "Set1"))

p <- ggplot(results.mean, aes(x=fr2, y=e2e.lag, color=tr2)) + geom_point(size = 4)
p <- p + scale_x_discrete(name = "frame duration (ms)", labels = seq(5, 100, length.out=20)) + ylab("E2E lag (ms)")
# p <- p + scale_color_discrete(values = get.palette(colour.count), name = "tick duration (ms)", labels=seq(5, 100, length.out=20), guide = guide_legend(ncol=2))
# p <- p + theme(text = element_text(family="Linux Biolinum O", size=24))
p <- p + theme(text = element_text(size=24))
p <- p + scale_color_manual(values = get.palette(colour.count), name = "tick duration (ms)", labels=seq(5, 100, length.out=20), guide = guide_legend(ncol=2))
p
ggsave("../visualization/nwless-onlinegame-1000rounds.pdf", width=12, height=8)


# works with custom fonts, but produces a file 5 times larger
#ggsave("nwless-onlinegame-1000rounds.pdf", width=12, height=8, device=cairo_pdf)

