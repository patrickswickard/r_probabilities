# display contents of each urn starting with n urns and r balls 
# balls is a list of which cups balls 1..x i,e. abcd go into
display_urn_state <- function(number_of_urns,balls) {
  colors <- c('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  #a = balls[1]
  #b = balls[2]
  #c = balls[3]
  #d = balls[4]
  urns <- c(1:number_of_urns)
  allurn <- list()
  number_of_balls = length(balls)
  for (i in 1:number_of_urns) {
    allurn[[i]] <- list()
  }
  for (i in 1:number_of_balls) {
    for (j in 1:number_of_urns) {
      if (balls[i] == urns[j])  {
        allurn[[j]] <- append(allurn[[j]], colors[i])
      }
    }
  }
  # we do this to make values canonical so bac=abc
  for (i in 1:number_of_urns) {
    # all this to sort a list???
    allurn[[i]] <- as.list(sort(unlist(allurn[[i]])))
  }
  finalurn <- list()
  for (i in 1:number_of_urns) {
    finalurn[[i]] <- paste(allurn[[i]], collapse='')
  }
  dfinalurn = paste(finalurn, collapse='|')
  return(dfinalurn[[1]])
}

print_sample_space_balls_urns <- function(number_of_balls, number_of_urns) {
  # all_ball_combos so each unique ball abcd etc goes into one cup
  #all_ball_combos <- expand.grid(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4))

  expand_grid_argument = list()
  for (i in 1:number_of_balls) {
    expand_grid_argument = append(expand_grid_argument,list((1:number_of_urns)))
  }

  #all_ball_combos <- expand.grid(c(1:number_of_urns),c(1:number_of_urns),c(1:number_of_urns),c(1:number_of_urns))
  all_ball_combos <- expand.grid(expand_grid_argument)

  lastindex <- length(all_ball_combos[,1])
  print("Sample space:")
  sample_space <- list()
  for (i in 1:lastindex) {
    dfinalurn <- display_urn_state(number_of_urns,all_ball_combos[i,])
    sample_space <- append(sample_space,dfinalurn)
  }
  unique_sample_space <- unique(unlist(sample_space))
  print(unique_sample_space)
  print("Size of unique sample space:")
  print(length(unique_sample_space))
}

print_sample_space_balls_urns(5,7)
