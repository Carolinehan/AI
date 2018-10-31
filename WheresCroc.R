#' randomWC
#'
#' Control function for Where's Croc where moves are random.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export
randomWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)
  return(moveInfo)
}

#' manualWC
#'
#' Control function for Where's Croc that allows manual play using keyboard.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export
manualWC=function(moveInfo,readings,positions,edges,probs) {
  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) {
    options=getOptions(mv1,edges)
  }
  print("Move 2 options (plus 0 for search):")
  print(options)
  mv2=readline("Move 2: ")
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }
  moveInfo$moves=c(mv1,mv2)
  return(moveInfo)
}

myFunction = function(moveInfo,readings,positions,edges,probs) {
  reset = F
  if(!is.null(moveInfo$mem$status))
  {
    reset = (moveInfo$mem$status == 1)
    if(reset == T)
    {
      moveInfo$mem$status = 2
    }
  }
  
  
  initial_state = initialMatrix(positions, edges, moveInfo, reset)
  transtionMatrix = Transition_matrix(moveInfo, readings, positions, edges, probs, initial_state)
  o = observationFunction(moveInfo, readings, positions, edges, probs)
  
  maxo=which(o==max(o), arr.ind = T)
  nextT=initial_state %*% transtionMatrix
  f = initial_state %*% transtionMatrix * o
  state_without_observation = initial_state %*% transtionMatrix
  normalizeF= f/rowSums(f)
  
  maxValue = max(normalizeF)
  m = which(normalizeF==maxValue,arr.ind=T)
  goal = m[1,2]
  current_position = positions[3]
  m = AStar(current_position,edges, goal,normalizeF)
  
  orders = abs(rank(normalizeF)-41)
  #print(orders)
  
  if(length(which(positions==goal))>0 && initial_state[1, goal] == 0)
  {
    moveInfo$moves = c(current_position, 0)
  }
  else
  {
    if(length(m) ==0)
    {
      nextGoal = which(orders == 2)
      m = AStar(current_position,edges, nextGoal,normalizeF)
      moveInfo$moves = c(0, m[1])
    }
    else if(length(m) <2)
    {
        moveInfo$moves = c(m[1], 0)
    }
    else
    {
      if(orders[m[1]] <3)
      {
        moveInfo$moves = c(m[1], 0)
      }
      else
      {
        moveInfo$moves=c(m[1],m[2])
      }
    }
  }
  moveInfo$mem$goal = goal
  moveInfo$mem$initia = normalizeF
  return(moveInfo)
}

Transition_matrix = function(moveInfo,readings,positions,edges,probs, initial_state) {
  Transition = matrix(0, ncol=40, nrow=40)
  for(i in 1:40) {
    v = getOptions(i, edges)
    #print(v)
    l = length(v);
    count = l
    p = 1 / count
    while(l > 0) {
      Transition[i, v[l]] = p
      l = l - 1
    }
  }
  
  return(Transition)
}

observationFunction = function(moveInfo,readings,positions,edges,probs) {
  
  observe_state = matrix(0, nrow = 1, ncol = 40)
  
  for (i in 1:40) {
    s_avg = probs$salinity[i,1]
    s_dev = probs$salinity[i,2]
    p_s = dnorm(readings[1],s_avg, s_dev)
    p_avg = probs$phosphate[i,1]
    p_dev = probs$phosphate[i,2]
    p_p = dnorm(readings[2], p_avg, p_dev)
    n_avg = probs$nitrogen[i,1]
    n_dev = probs$nitrogen[i,2]
    p_n = dnorm(readings[3], n_avg, n_dev)
    p = p_s * p_p * p_n;
    observe_state[1,i] = p
  }
  
  return(observe_state)
}

initialMatrix=function(positions, edges, moveInfo, reset){
  
  if(is.null(moveInfo$mem) || is.null(moveInfo$mem$initia) || reset == T) {
    count = 40
    initial_state = matrix(1/count, ncol=40, nrow=1)
    initial_state[1, positions[1:3]] = 0
    return (initial_state)
  } 
  else {
    initial_state = moveInfo$mem$initia
  }
  
  if(!is.null(moveInfo$moves))
  {
    if(length(moveInfo$moves) == 2)
    {
      if(moveInfo$moves[2] == 0)
      {
        initial_state[1, moveInfo$moves[1]] = 0
      }
    }
  }
  
  for(i in 1:2)
  {
    backpackerPosition =positions[i]
    if(is.na(backpackerPosition ))
    {
      next
    }
    
    if(backpackerPosition > 0)
    {
      next
    }
    
    if(backpackerPosition < 0)
    {
      initial_state = matrix(0, ncol=40, nrow=1)
      neighbors = getOptions(abs(backpackerPosition), edges)
      len = length(neighbors)
      for(neighbor in neighbors)
      {
        initial_state[1, neighbor] = 1/len
      }
    }
  }
  
  return (initial_state)
}

huristic = function(normalizedF, node)
{
  return(0)
}

cost = function(normalizedF, node)
{
  orders = abs(rank(normalizedF)-41)
  h_values = orders[node]
  if(h_values < 4)
  {
   return (h_values/10)
  }

  return(0.6)
}

AStar = function(current,edges,goal,normalizedF)
{
  openset=list()
  closeSet =list()
  route = list()
  currentNode =as.character(current)
  fscore=list()
  gscore=list()
  openset[[currentNode]]=current
  fscore[[currentNode]] = 0
  gscore[[currentNode]] = 0
  emptyOpenset = length(openset)
  
  while (emptyOpenset > 0) {
    fscoreToVector = sapply(fscore, function(node)node)
    minNode = names(which.min(fscoreToVector))
    minNodeValue = as.numeric(minNode)
    currentRoute = route[[minNode]]
    
    if(minNodeValue== goal) {
      finalRoute= append(currentRoute, minNodeValue)[-1]
      return (finalRoute)
    }
    
    currentCost =  gscore[[minNode]] 
    fscore[[minNode]] = NULL
    openset[[minNode]] = NULL
    closeSet[[minNode]] = minNodeValue
    
    
    neighbors =  getOptions(minNodeValue, edges)
    
    for(i in neighbors)
    {
      newNode = as.character(i)
      if(!is.null(closeSet[[newNode]]))
      {
        next
      }
      
      
      if(is.null(fscore[[newNode]]))
      {
        gscore[[newNode]] = currentCost +cost(normalizedF,i)
        fscore[[newNode]] = gscore[[newNode]] + huristic(normalizedF,i)
        route[[newNode]] = append(currentRoute, minNodeValue)
      }
      else
      {
        if((currentCost + 1) < fscore[[newNode]])
        {
          gscore[[newNode]] = currentCost +cost(normalizedF,i)
          fscore[[newNode]] = gscore[[newNode]] + huristic(normalizedF,i)
          route[[newNode]] = append(currentRoute, minNodeValue)
        }
      }
      
      openset[[newNode]] = i
    }
    
    emptyOpenset = length(openset)
  }
  return (FALSE)
}

#' testWC
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' With the default seed of 21, the mean for the par function on this is 5.444, and the sd is approximately 3.853.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 30 seconds. Note that you will need to reuse objects that do not change
#' from game to game (such as the transition matrix and routing information) in order to achieve this sort
#' of speed.
#'
#' The par function takes approximately 3.85 seconds on my laptop. If it takes longer than 30 seconds on the
#' evaluation machine, the time limit will be increased so as to be 25% slower than the par function.
#'
#' @param myFunction Your function to be passed to runWheresCroc. See runWheresCroc documentation for details.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runWheresCroc output of the result of each game.
#' @param returnVec See return value.
#' @param seed The random seed to use. Pass NA to not set a random seed.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If your function is too slow, NA is returned. Otherwise if returnVec is TRUE, a vector containing
#' all results is returned. If returnVec is FALSE, the average performance is returned.
#' @export
testWC=function(myFunction,verbose=0,returnVec=FALSE,seed=21,timeLimit=300){
  set.seed(21)
  seeds=sample(1:25000,500)
  startTime=Sys.time()
  mem=NA
  hmm=c()
  for (s in seeds) {
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    
    res=runWheresCroc(myFunction,doPlot=F,pause=0,verbose=verbose==2,returnMem=T,mem=mem)
    mem=res$mem
    hmm=c(hmm,res$move)
    if (verbose==2)
    {
      if(res$move > 10)
      {
        cat("\nNew game, seed",s)
        cat("\n turns: ", res$move)
      }
    }
  }
  if (verbose>=1) {
    endTime=Sys.time()
    cat("\nMean moves:", mean(hmm))
    cat("\nSD moves:", sd(hmm))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return (hmm)
  else
    return (mean(hmm))
}

#' Run Where's Croc
#'
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park.
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game.
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function and enter the appropriate numbers to make moves.
#' Note that the croc will move randomly, with a uniform distribution over moving to any adjancent waterholes
#' or staying still.
#' @param makeMoves Your function that takes five arguments: (1) A list of information for the move.
#' This has two fields. The first is a vector of numbers called 'moves', where you will enter
#' the moves you want to make. You should
#' enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called
#' 'mem' that you can use to store information you want to remember from turn to turn. (2) A
#' vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current
#' location. (3) A vector giving the positions of the two tourists
#' (elements 1 and 2) and yourself (element 3). If a tourist
#' has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist
#' was eaten by Croc in a previous turn, then the position will be NA. (4) a two column matrix giving the
#' edges paths between waterholes (edges) present (the numbers are from and to numbers for
#' the waterholes). All edges can be crossed both ways, so are only given once.
#' (5) a list of three matrices giving the mean
#' and standard deviation of readings for salinity, phosphate and nitrogen respectively
#' at each waterhole.
#' Your function should return the first argument passed with an updated moves vector
#' and any changes to the 'mem' field you wish to access later on.
#' @param doPlot A Boolean stating if you want the gameboard to be plotted each turn
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Designed to give time to plot game.
#' @param verbose Set to FALSE to stop any print output
#' @param returnMem Should the info$mem field be returned? If so, the output is a list consisting of
#' the move field, giving the number of moves in the game, and the mem field consisting of the mem
#' object
#' @param mem If you returned a mem object from a previous run, it can be passed here. It's status
#' will be set to 1. Otherwise a new mem list will be created with status set to 0. The idea is
#' to speed up multiple runs, such as the evaluation run of 500 games, by avoiding redoing
#' expensive initial setups of the transition matrix and routing informing.
#' @return A string describing the outcome of the game.
#' @export
runWheresCroc=function(makeMoves,doPlot=T,showCroc=T,pause=1,verbose=T,returnMem=F,mem=NA) {
  #set.seed(17859)
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  points=getPoints()
  edges=getEdges()
  probs=getProbs()
  move=0
  moveInfo=list(moves=c(),mem=list(status=0))
  if (!all(is.na(mem)))
    moveInfo$mem=mem
  first=T
  while (!is.na(positions[1])) {
    move=move+1
    if (!first) {
      positions[1]=sample(getOptions(positions[1],edges),1)
      if (!is.na(positions[2])&&positions[2]>0) {
        positions[2]=sample(getOptions(positions[2],edges),1)
      } else if (!is.na(positions[2]) && positions[2]<0) {
        positions[2]=NA
      }
      if (!is.na(positions[3])&&positions[3]>0) {
        positions[3]=sample(getOptions(positions[3],edges),1)
      } else if (!is.na(positions[3]) && positions[3]<0) {
        positions[3]=NA
      }
      if (!is.na(positions[2]) && positions[2]==positions[1]) {
        positions[2]=-positions[2]
      }
      if (!is.na(positions[3]) && positions[3]==positions[1]) {
        positions[3]=-positions[3]
      }
    }
    else
      first=F
    
    if (doPlot)
      plotGameboard(points,edges,move,positions,showCroc)
    
    Sys.sleep(pause)
    
    readings=getReadings(positions[1],probs)
    moveInfo=makeMoves(moveInfo,readings,positions[2:4],edges,probs)
    if (length(moveInfo$moves)!=2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    for (m in moveInfo$moves) {
      if (m==0) {
        if (positions[1]==positions[4]) {
          if (verbose)
            cat("\nCongratualations - You got croc at move ",move)
          if (returnMem) {
            mem=moveInfo$mem
            mem$status=1
            return (list(move=move,mem=mem))
          }
          return (move)
        }
      } else {
        if (m%in%getOptions(positions[4],edges)) {
          positions[4]=m
        } else {
          warning("Invalid move.")
        }
      }
    }
  }
}
#' @keywords internal
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @keywords internal
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))
  
  return (edges)
}

#' @keywords internal
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @keywords internal
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}


#' @keywords internal
plotGameboard=function(points,edges,move,positions,showCroc) {
  plot(points,pch=18,col="blue",cex=2,xlab="X",ylab="Y",main=paste("Where's Croc - Move",move))
  xFrom=points[edges[,1],1]
  yFrom=points[edges[,1],2]
  xTo=points[edges[,2],1]
  yTo=points[edges[,2],2]
  segments(xFrom,yFrom,xTo,yTo)
  for (bp in 2:3)
    if (!is.na(positions[bp])) {
      if (positions[bp]>0) {
        points(points[as.numeric(positions[bp]),1],points[as.numeric(positions[bp]),2],col="orange",pch=17,cex=4)
      } else {
        points(points[-as.numeric(positions[bp]),1],points[-as.numeric(positions[bp]),2],col="red",pch=17,cex=4)
      }
    }
  points(points[as.numeric(positions[4]),1],points[as.numeric(positions[4]),2],col="green",pch=15,cex=4)
  if (showCroc) {
    points(points[as.numeric(positions[1]),1],points[as.numeric(positions[1]),2],col="red",pch=15,cex=4)
  }
  text(points[,1]+.4, points[,2], labels=as.character(1:40))
}

#' @keywords internal
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}