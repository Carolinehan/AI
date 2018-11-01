myFunction = function(roads, car, packages) {
  nextMove=0
  toGo=0
  offset=0
  car = astarAllPackage(roads, car, packages)
  toGo = car$mem$package
  offset = car$mem$offset
  
  x2 = packages[toGo, offset + 1]
  y2= packages[toGo, offset + 2]
  
  
  if(car$x == x2 && car$y == y2)
  {
    car$nextMove = 5
    car$mem = list()
    car$mem$prevNode = list(x=car$x,y=car$y)
    return(car)
  }
  
  nextPath = aStar(car$x, car$y, x2, y2, roads,car)
  path = nextPath$nextNode
  x= floor((path-1) / 10) + 1
  y= (path - 1) %% 10 + 1
  #2 down, 4 left, 6 right, 8 up, 5 stay still
  
  bottleNeck = 4
  if (car$x == x && car$y != y) {
    if(car$y < y) {
      nextMove = 8
    }
    else if(car$y > y) {
      nextMove = 2
      
    }
  }
  else if(car$y == y && car$x != x) {
    if(car$x < x) {
      
      nextMove = 6
    }
    else if(car$x > x) {
      
      nextMove = 4
    }
  }
  else
  {
    nextMove = 5;
    print(car$x, car$y, x,y)
    print("................")
  }
  car$nextMove=nextMove
  if(x == x2 && y ==y2)
  {
    car$mem=list()
  }
  
  car$mem$prevNode = list(x=car$x,y=car$y)
  return (car)
}

heuristic_estimate_of_distance = function(x1, y1, x2, y2, roads) {
  # minimum = 10000
  startx = x1
  starty = y1
  endx = x2
  endy = y2
  minHroad = 0;
  minVroad = 0;
  distance = abs(x1 - x2) + abs(y1 - y2)
  if (x1 > x2) {
    startx = x2
    endx = x1
  }
  if (y1 > y2) {
    starty = y2
    endy = y1
  }

  if(startx < endx)
  {
    minHroad = min(roads$hroads[startx:endx-1, starty:endy])
  }
  if(starty < endy)
  {
    minVroad = min(roads$vroads[startx:endx, starty:endy-1])
  }
  if(minHroad != 0 && minVroad != 0)
  {
    minimum = min(minHroad, minVroad)
    distance = minimum * distance
  }
  else if(minHroad != 0)
  {
    distance = minHroad * distance
  }
  else if(minVroad != 0)
  {
    distance = minVroad * distance
  }
  
  return (distance)
}


aStar = function(x1, y1, x2, y2, roads,car) {
  dim = 10
  openset = matrix(0, ncol=10, nrow=10)
  openset[x1,y1] = 1
  closedset = matrix(0, ncol=10, nrow=10)
  g_score = matrix(0, ncol=10, nrow=10)
  h_score = matrix(0, ncol=10, nrow=10)
  f_score = matrix(0, ncol=10, nrow=10)
  came_from = matrix(0, ncol=10, nrow=10)
  m = matrix(1:100, nrow = 10, byrow = TRUE)
  h_score[x1,y1] = heuristic_estimate_of_distance(x1, y1, x2, y2, roads)
  f_score[x1,y1] = g_score[x1,y1] + h_score[x1,y1]
  path=list()
  
  
  emptyOpenset = length(which(openset == 1))
  i =0
  while (emptyOpenset > 0) {
    i = i+1
    openIndices = which(openset == 1, arr.ind = TRUE);
    minimum = min(f_score[openIndices]);
    indices = which(f_score == minimum, arr.ind = TRUE);
    ff =which(openset[indices] ==1,arr.ind = T)
    if(length(ff) > 0)
    {
      index = 1
      x = indices[ff[index],][1]
      y=indices[ff[index],][2]
    }
    else
    {
      break
    }
    
    if(x == x2 && y == y2) {
      secondNode = came_from[x,y];
      return (list(nextNode = secondNode, fscore=f_score[x,y]))
    }
    
    
    
    openset[x,y] = 0
    closedset[x,y] = 1
  
    
    # right
    if (x + 1 <= dim) {
      if(closedset[x + 1,y] == 0) {
        tentative_g_score = g_score[x,y] + roads$hroads[x,y]
        tentative_is_better = FALSE
        if (openset[x +1,y] != 1) {
          tentative_is_better = TRUE
        }
        else if(tentative_g_score < g_score[x + 1,y]) {
          tentative_is_better = TRUE
        }
        if(tentative_is_better == TRUE) {
          if(i == 1)
          {
            came_from[x + 1,y] = m[x+1,y]
          }
          else if (i > 1)
          {
            came_from[x + 1,y] = came_from[x,y]
          }
          
          g_score[x + 1,y] = tentative_g_score;
          h_score[x + 1,y] = heuristic_estimate_of_distance(x + 1, y, x2, y2, roads)
          f_score[x + 1,y] = g_score[x + 1,y] + h_score[x + 1,y]
          openset[x + 1,y] = 1
        }
      }
    }
    #up
    if (y + 1 <= dim) {
      if(closedset[x,y + 1] == 0) {
        tentative_g_score = g_score[x,y] + roads$vroads[x,y]
        tentative_is_better = FALSE
        if (openset[x,y + 1] != 1) {
          tentative_is_better = TRUE
        }
        else if(tentative_g_score < g_score[x,y + 1]) {
          tentative_is_better = TRUE
        }

        if(tentative_is_better == TRUE) {

          if(i == 1)
          {
            came_from[x,y+1] = m[x,y+1]
          }
          else if (i > 1)
          {
            came_from[x,y+1] = came_from[x,y]
          }

          g_score[x,y + 1] = tentative_g_score;
          h_score[x,y + 1] = heuristic_estimate_of_distance(x, y + 1, x2, y2, roads)
          f_score[x,y + 1] = g_score[x,y + 1] + h_score[x,y + 1]
          openset[x,y + 1] = 1
        }
      }
    }
    #left
    if (x - 1 >= 1) {
      if(closedset[x - 1,y] == 0) {
        tentative_g_score = g_score[x,y] + roads$hroads[x-1, y]
        tentative_is_better = FALSE
        if (openset[x - 1,y] != 1) {
          tentative_is_better = TRUE
        }
        else if(tentative_g_score < g_score[x - 1,y]) {
          tentative_is_better = TRUE
        }

        if(tentative_is_better == TRUE) {
          if(i == 1)
          {
            came_from[x - 1,y] = m[x - 1,y]
          }
          else if (i > 1)
          {
            came_from[x - 1,y] = came_from[x,y]
          }
          g_score[x - 1,y] = tentative_g_score;
          h_score[x - 1,y] = heuristic_estimate_of_distance(x - 1, y, x2, y2, roads)
          f_score[x - 1,y] = g_score[x - 1,y] + h_score[x - 1,y]
          openset[x - 1,y] = 1
        }
      }
    }
    #down
    if (y - 1 >= 1) {
        if(closedset[x,y - 1] == 0) {
        tentative_g_score = g_score[x,y] + roads$vroads[x,y-1]
        tentative_is_better = FALSE
        if (openset[x,y - 1] != 1) {
          tentative_is_better = TRUE
        }
        else if(tentative_g_score < g_score[x,y - 1]) {
          tentative_is_better = TRUE
        }

        if(tentative_is_better == TRUE) {
          if(i == 1)
          {
            came_from[x,y - 1] = m[x,y - 1]
          }
          else if (i > 1)
          {
            came_from[x,y - 1] = came_from[x,y]
          }
          g_score[x,y - 1] = tentative_g_score;
          h_score[x,y - 1] = heuristic_estimate_of_distance(x, y - 1, x2, y2, roads)
          f_score[x,y - 1] = g_score[x,y - 1] + h_score[x,y - 1]
          openset[x,y - 1] = 1
        }
      }
    }
    emptyOpenset = length(which(openset == 1))
  }
  return (FALSE)
}

aStarForPackage = function(car, packages,roads) {
  openset=list()
  closeSet =list()
  currentNode ="0"
  fscore=list()
  openset[[currentNode]]=currentNode
  fscore[[currentNode]] = 0
  emptyOpenset = length(openset)
  undelivedPakages=which(packages[,5]==0);
  totalNumbers = length(undelivedPakages)
  singleScore=list()
  
  
  while (emptyOpenset > 0) {
    fscoreToVector = sapply(fscore, function(node)node)
    minNode = names(which.min(fscoreToVector))
    
    if(nchar(minNode)==length(undelivedPakages)+1) {
      num=as.numeric(minNode)
      last = num/(10^(totalNumbers-1))
      return (floor(last))
    }
    
    currentCost =  fscore[[minNode]] 
    fscore[[minNode]] = NULL
    openset[[minNode]] = NULL
    closeSet[[minNode]] = minNode
    
    nodeLength = nchar(minNode)
    lastPackage = substr(minNode, nodeLength, nodeLength)
    x1 = 0
    y1 =0
    if(lastPackage == "0")
    {
      x1 = car$x
      y1 =car$y
    }
    else
    {
      x1 = packages[as.numeric(lastPackage),3]
      y1 = packages[as.numeric(lastPackage),4]
    }
    
    for(i in undelivedPakages)
    {
      if(grepl(as.character(i), minNode))
      {
        next
      }
      
      neighborNode = paste(minNode, as.character(i), sep="")
      if(!is.null(closeSet[[neighborNode]]))
      {
        next
      }
    
      x2 = packages[i,1]
      y2 = packages[i,2]
      newNode= paste(lastPackage,as.character(i),sep="")
      fcost = 0
      if(is.null(singleScore[[newNode]]))
      {
        fcost = aStar(x1,y1,x2,y2, roads, car)
        singleScore[[newNode]] = fcost
      }
      else
      {
        fcost = singleScore[[newNode]]
      }
      openset[[neighborNode]] = neighborNode
      fscore[[neighborNode]] = currentCost + fcost$fscore
    }
   
    emptyOpenset = length(openset)
  }
  return (FALSE)
}


astarAllPackage = function(roads, car, packages) {
  if (car$load==0) {
    package = aStarForPackage(car, packages,roads)
    car$mem$package = package
    car$mem$offset = 0
    
  }
  else
  {
    car$mem$package = car$load
    car$mem$offset = 2
  }
  
  return (car)
}



#' dumbDM
#'
#' This control function just moves randomly, until all packages are picked up and delivered by accident!
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
#' basicDM
#'
#' This control function will pick up the closest package (using distance and ignoring traffic).
#' As a first step, you should make sure you do better than this.
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
#' manualDM
#'
#' If you have the urge to play the game manually (giving moves 2, 4, 5, 6, or 8 using the keyboard) you
#' can pass this control function to runDeliveryMan
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export
manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

#' testDM
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' The mean for the par function (with n=500) on this is 172.734, and the sd is approximately 39.065.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' This set of seeds is chosen so as to include a tricky game that has pick ups and deliveries on the same
#' spot. This will occur in the actual games you are evaluated on too.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 4 minutes (250 seconds). If the evaluation machine is slower than expected,
#' this will be altered so that the required time is 25% slower than the par function.
#'
#' The par function takes approximately 96 seconds on my laptop (with n=500 and verbose=0).
#'
#' @param myFunction The function you have created to control the Delivery Man game.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runDeliveryMan output of the result of each game.
#' @param returnVec Set to TRUE if you want the results of the games played returned as a vector.
#' @param n The number of games played. You will be evaluated on a set of 500 games, which is also the default here.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If returnVec is false, a scalar giving the mean of the results of the games played. If returnVec is TRUE
#' a vector giving the result of each game played. If the time limit is breached, a NA is returned.
#' @export
testDM=function(myFunction,verbose=0,returnVec=FALSE,n=500,seed=21,timeLimit=250){
  if (!is.na(seed))
    set.seed(seed)
  seeds=sample(1:25000,n)
  startTime=Sys.time()
  aStar=sapply(seeds,function(s){
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    if (verbose==2)
      cat("\nNew game, seed",s)
    runDeliveryMan(myFunction,doPlot=F,pause=0,verbose=verbose==2)
  })
  endTime=Sys.time()
  if (verbose>=1){
    cat("\nMean:",mean(aStar))
    cat("\nStd Dev:",sd(aStar))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return(aStar)
  else
    return (mean(aStar))
}

#' Run Delivery Man
#'
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic
#' conditional on the vertical roads. <1,1> is the bottom left, and <dim,dim> is the top right.
#'(2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not
#' delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10. Note that
#' this means you will have to remove duplicated nodes from your frontier to keep your AStar
#' computationally reasonable! There is a time limit for how long an average game can be run in, and
#' if your program takes too long, you will penalized or even fail.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5,verbose=T) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i)
      plotRoads(roads$hroads,roads$vroads)
      points(car$x,car$y,pch=16,col="blue",cex=3)
      plotPackages(packages)
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          if (verbose)
            cat("\nCongratulations! You suceeded in",i,"turns!")
          return (i)
        }
      }
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  cat("\nYou failed to complete the task. Try again.")
  return (NA)
}
#' @keywords internal
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  }
  return (0)
}
#' @keywords internal
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$x,car$y]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$x,car$y]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$x,car$y]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$x,car$y]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")
  }
  car$nextMove=NA
  return (car)
}

#' @keywords internal
plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0)
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

#' @keywords internal
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @keywords internal
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n-1)
  vroads=matrix(rep(1,(n-1)*n),nrow=n)
  list(hroads=hroads,vroads=vroads)
}

#' @keywords internal
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(row,row+1),c(col,col),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(row,row),c(col,col+1),col=vroads[row,col])
    }
  }
}
#' @keywords internal
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }
  }
  list (hroads=hroads,vroads=vroads)
}