makeController=function(maze){
 
  decideAction=function(cont,maze_) {
    old_position =maze_$wobbie[1]*10 + maze_$wobbie[2]
    cont$old_position = old_position
    cont$monster = maze_$monster1[1]*10+maze_$monster1[2]
    
    monster_index = as.character(cont$monster)
    if(cont$doRand)
    {
      if(is.null(cont$q_table))
      {
        cont$q_table = list()
      }
    }
    
    q_table = cont$q_table
    
    old_position_index = as.character(old_position)
    
    rand = runif(1, 0, 1)
    
    actions = supported_actions(maze_)
    if(cont$doRand)
    {
      move = sample(actions,1)
    }
    else
    {
      move = 5
    }

    if(cont$doRand ==F || rand > 0.2)
    {
      if(!is.null(q_table[[old_position_index]]))
      {
        if(!is.null(q_table[[old_position_index]][[monster_index]]))
        {
          data_source = q_table[[old_position_index]][[monster_index]]
          data = sapply(data_source, function(node)node["Q"])
          max_index =  which.max(data)
          node =names(data_source)[[max_index]]
          move =as.numeric(node)
        }
      }
    }
    
    
    list(move=move,control=cont)
  }
  
  update=function(cont,maze_) {
    if(cont$doRand)
    {
      discount = 1
      
      q_table = cont$q_table
      old_position =cont$old_position
      action = maze_$lastAction
      reward = maze_$reward
      new_position =maze_$wobbie[1]*10 + maze_$wobbie[2]
      monster = maze_$monster1[1]*10+maze_$monster1[2]
      goal = maze_$goal[1]*10 + maze_$goal[2]
      monster_index = as.character(monster)
      old_monster = cont$monster
      old_monster_index =as.character(old_monster)
      
      cum_reward = 0
      new_position_index = as.character(new_position)
      if(!is.null(q_table[[new_position_index]]))
      {
        if(!is.null(q_table[[new_position_index]][[monster_index]]))
        {
          new_nodes = q_table[[new_position_index]][[monster_index]]
          reward_data = sapply(new_nodes, function(node) node["Q"])
          max_index = which.max(reward_data)
          cum_reward = reward_data[[max_index]]
        }
      }
      
      example_q = reward
      
      if(new_position != goal && new_position != monster)
      {
        example_q = example_q + discount*cum_reward
      }
      else
      {
        a=1
       
      }
      
      expected_reward = 0
      old_position_index = as.character(old_position)
      action_index = as.character(action)
      visited = 60
      visitedCount = 1
      if(!is.null(q_table[[old_position_index]]))
      {
        if(!is.null(q_table[[old_position_index]][[old_monster_index]]))
        {
          if(!is.null(q_table[[old_position_index]][[old_monster_index]][[action_index]]))
          {
            node = q_table[[old_position_index]][[old_monster_index]][[action_index]]
            expected_reward = node[["Q"]]
            visited = visited + node[["VisitedCount"]]
            visitedCount = node[["VisitedCount"]] + 1
          }
        }
      }
      
      
      gama = 60/visited
      new_q= as.numeric(expected_reward) + as.numeric(gama)*(as.numeric(example_q)-as.numeric(expected_reward))
      
      if(is.null(q_table[[old_position_index]]))
      {
        q_table[[old_position_index]] = list()
      }
      
      current_node = q_table[[old_position_index]]
      
      
      if(is.null(current_node[[old_monster_index]]))
      {
        q_table[[old_position_index]][[old_monster_index]] = list()
      }
      
      action_node = q_table[[old_position_index]][[old_monster_index]]
      if(is.null(action_node[[action_index]]))
      {
        q_table[[old_position_index]][[old_monster_index]][[action_index]]=list()
      }
      
      current = q_table[[old_position_index]][[old_monster_index]][[action_index]]
      current[["VisitedCount"]]=visitedCount
      current[["Q"]] = new_q
      q_table[[old_position_index]][[old_monster_index]][[action_index]] = current
      
      cont$q_table = q_table
    }
    
    return (cont)
  }
  
  
  
  list(decideAction=decideAction,update=update,doRand=TRUE)
}

supported_actions = function(maze)
{
  actions = c()
  inMaze=function(loc){
    length(which(maze$maze$x==loc[1] & maze$maze$y==loc[2]))==1
  }
  oldX=wobbieX=maze$wobbie[1]
  oldY=wobbieY=maze$wobbie[2]
  if (inMaze(c(oldX-1,oldY)))
    actions = append(actions, 4)
  if (inMaze(c(oldX+1,oldY)))
    actions = append(actions, 6)
  if (inMaze(c(oldX,oldY-1)))
    actions = append(actions, 2)
  if (inMaze(c(oldX,oldY+1)))
    actions = append(actions, 8)
  
  return (actions)
}

#' runWW
#'
#' You will need to pass a function that creates a Q-learning RL controller.
#' This controller will then play 10000 games, learning to control
#' the Wobbie's World game. It will be tested on 500 games after it
#' is finished learning. This function should take the maze object
#' returned from the makeWobbiesWorld function as the only argument. (The idea
#' is you can use the maze object to help set up your controller.)
#'
#' Once created your controller will be passed to the learn function.
#'
#' Your controller should have a field called doRand. This will be
#' set to FALSE after learning is completed, and you should set things
#' up so that your controller will no longer make random moves when this
#' is FALSE.
#'
#' Once learning is complete, your controller will be passed to the test
#' function and performance over 500 test runs will be recorded.
#'
#' Assessment will be based on the performance of your function for
#' a particular seed. It is compared to a par table-based q-learning
#' controller, and your average over the 500 test games need to be
#' at least within 1 of the par. For seed 0, the par controller
#' got an average score of 985.74 with a standard deviation of approximately
#' 6.55.
#'
#' On my laptop, the par controller took approximately 56 seconds to complete
#' (with verbose and doPlot both FALSE). To pass your code will need to complete
#' no more than 25% slower than the par function, or 300 seconds, whichever is higher.
#'
#' @param makeController A function that will return a Q-learning controller. See above for
#' details.
#' @param verbose Passed to the learn function
#' @param doPlot Passed to the learn function
#' @param seed A random seed to be used before anything else is executed.
#' @return A vector containing (1) the mean performance of the test runs; and (2) the time taken in seconds.
#' @export
runWW=function(
  makeController=makeRandomController,
  verbose=FALSE,
  doPlot=FALSE,
  seed=NA
){
  startTime=Sys.time()
  if (!is.na(seed))
    set.seed(seed)
  maze=makeWobbiesWorld()
  control=learn(control=makeController(maze),verbose=verbose,doPlot=doPlot)$control
  control$doRand=FALSE
  scrs=test(control)
  endTime=Sys.time()
  timeTaken=as.numeric(endTime)-as.numeric(startTime)
  cat("\nTime taken:",timeTaken,"seconds.\n")
  c(mean(scrs),timeTaken)
}

#' runGame
#'
#' Runs a single game of Wobbie's World.
#'
#' You need to pass a controller to this function.This controller should be a list, containing two named fields:
#'
#'   decideAction: A function which takes as arguments the controller and
#'       the maze, and returns an action (2=down,4=left,6=right,8=up)
#'
#'   update: A function which takes as arguments the controller and the maze,
#'       and returns the (updated) controller.
#'
#' Each turn the decideAction function is called. The maze is then updated
#' based on the returned action, and then the update function is called with
#' the updated maze so the controller can learn the consequences of its
#' action.
#'
#' The game terminates when Wobbie exits the maze or when he is caught by the monster.
#' The game score is the number of turns taken, minus 1000 if Wobbie was caught by the monster.
#' @param maze The maze to use. You should keep the default (with a single monster), which is
#' what will be used for evaluation.
#' @param control The control system. See above for details.
#' @param seed The seed to use for this game. Ignored if NA
#' @param doPlot Should the game be plotted
#' @param pause If the game is plotted, the pause between moves. (Non-zero pause required for
#' plotting to work on most systems.)
#' @return A list containing (1) The control system (updated if it is learning), (2) The game score, (3) The turns taken.
#' @export
runGame=function(maze=makeWobbiesWorld(),
                 control=makeRandomController(maze),
                 seed=NA,
                 doPlot=F,
                 pause=1
){
  if (!is.na(seed))
    set.seed(seed)
  score=0
  turn=0
  if (doPlot)
    plot(maze,paste("Wobbie's World   Turn:",turn," Score:",score),pause)
  while (maze$alive && !maze$finished) {
    turnRes=takeTurn(maze,control)
    maze=turnRes$maze
    control=turnRes$control
    turn=turn+1
    score=score+maze$reward
    if (doPlot)
      plot(maze,paste("Wobbie's World   Turn:",turn," Score:",score),pause)
  }
  list (control=control,score=score,turn=turn)
}

#' learn
#'
#' The learn function used by the runWW function. It is exported in case you wish to call it directly.
#' @param n Number of runs/games to perform
#' @param maze The maze to use. You should keep the default (with a single monster), which is
#' what will be used for evaluation.
#' @param control The control system. See runGame for details about this object's required characteristics.
#' @param verbose Should additional output be printed in the console.
#' @param doPlot Should a plot be drawn showing the evolution in performance of the system being trained
#' @param every If doPlot is TRUE, a new point is added to the plot after this many runs/games, showing the
#' average performance by the control system over the games since the last point was plotted,
#' @param col If plot is TRUE, the color of the point plotted
#' @param ymin If plot is TRUE, the lower limit of the area plotted (change to zoom in on good performance, rather
#' than have the plot dominated by early terrible performance).
#' @return A list containing (1) The updated controller, (2) The vector of scores, (3) The vector of turns taken
#' @export
learn=function(
  n=10000,
  maze=makeWobbiesWorld(),
  control=makeRandomController(maze),
  verbose=TRUE,
  doPlot=TRUE,
  every=100,
  col=1,
  ymin=-1000
) {
  plotStarted=FALSE
  score=c()
  turns=c()
  for (i in 1:n) {
    res=runGame(maze,control)
    
    if (verbose)
      cat("\nRun:",i," Turn:",res$turn," Score:",res$score)
    score=c(score,res$score)
    turns=c(turns,res$turn)
    control=res$control
    #print(control$q_table)
    if (doPlot && i%%every == 0) {
      if (plotStarted) {
        points(i/every,mean(score[(i-every):i]),col=col)
      }
      else {
        plot(i/every,mean(score[(i-every):i]),xlim=c(0,n/every),ylim=c(ymin,1000),col=col,
             xlab=paste("Batch of ",every,"runs"),ylab="Average Score")
        plotStarted=T
      }
    }
  }
  list(control=control,score=score,turns=turns)
}

#' test
#'
#' The test function called by the runWW function. It is exported in case you wish to call it directly.
#'
#' This function evaluates the performance of a controller
#' on a set of games. This is intended to be used once training is
#' completed. It will be used in evaluation (with n=500 and a particular random seed set before the call).
#'
#' Remember to turn off any randomness in your controller!
#' @param control The control system. See runGame for details about this object's required characteristics.
#' @param n The number of run/games. Evaluation is over 500 runs.
#' @return A vector giving the score of each game played.
#' @export
test=function(control,n=500) {
  scrs=c()
  for (i in 1:n)
    scrs=c(scrs,runGame(control=control)$score)
  cat("\nScores Mean:",mean(scrs))
  cat("\nScore Std Dev:",sd(scrs))
  return (scrs)
}

#' makeRandomController
#'
#' This function returns a demo controller that just makes random moves (even when
#' doRand is set to FALSE).
#' @param The maze used
#' @return A random controller able to be used in runWW and runGame.
#' @export
makeRandomController=function(maze){
  decideAction=function(cont,maze_) {
    list(move=sample(c(2,4,6,8),1),control=cont)
  }
  update=function(cont,maze_) {
    return (cont)
  }
  
  print(maze)
  list(decideAction=decideAction,update=update,doRand=TRUE)
}
