# from https://stackoverflow.com/questions/39353580/how-to-implement-q-learning-in-r
#
# Author: aichao  Date: Sep 6, 2016
#
# Input parameters are:
# R is the rewards matrix as defined in the blog
# N is the number of episodes to iterate
# alpha is the learning rate
# gamma is the discount factor
# tgt.state is the target state of the problem.

q.learn <- function(R, N, alpha, gamma, tgt.state) {
  ## initialize Q to be zero matrix, same size as R
  Q <- matrix(rep(0,length(R)), nrow=nrow(R))
  ## loop over episodes
  for (i in 1:N) {
    ## for each episode, choose an initial state at random
    cs <- sample(1:nrow(R), 1)
    ## iterate until we get to the tgt.state
    while (1) {
      ## choose next state from possible actions at current state
      ## Note: if only one possible action, then choose it;
      ## otherwise, choose one at random
      next.states <- which(R[cs,] > -1)
      if (length(next.states)==1)
        ns <- next.states
      else
        ns <- sample(next.states,1)
      ## this is the update
      Q[cs,ns] <- Q[cs,ns] + alpha*(R[cs,ns] + gamma*max(Q[ns, which(R[ns,] > -1)]) - Q[cs,ns])
      ## break out of while loop if target state is reached
      ## otherwise, set next.state as current.state and repeat      
      if (ns == tgt.state) break
      cs <- ns
    }
  }
  ## return resulting Q normalized by max value
  return(100*Q/max(Q))
}
