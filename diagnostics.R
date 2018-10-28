# Pnuemonia [T,F] -> X_Ray, Temperature
# Temperature [Normal]

# Visited_TB_Spot [T,F] -> Tuberculosis
# Tuberculosis [T,F] -> X_Ray
#
# Smokes [T,F] -> Lung_Cancer, Bronchitis
# Lung_Cancer [T,F] -> X_Ray, Dyspnea
# Bronchitis [T,F] -> Dyspnea
#
# X_Ray : [T,F]
# Dyspnea (Shortness of Breath): [T,F]
learn = function(his) {
  #probability of pne
  probability = list()
  l = subset(his, Pn == 0, select=c(Pn))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 1, select=c(Pn))
  s2 = length(l[,1]) + 1
  Pn_F = s1 / (s1+s2)
  Pn_T = s2 / (s1+s2)
  
  PnProb=matrix(c(0, Pn_F,
                  1, Pn_T), nrow=2, byrow = T)
  colnames(PnProb)=c("Pn","P")
  
  probability$Pn = PnProb
  
  
  #' #'probability of VTB
  l = subset(his, VTB == 0, select=c(VTB))
  s1 = length(l[,1]) + 1
  l = subset(his, VTB == 1, select=c(VTB))
  s2 = length(l[,1]) + 1
  VTB_F = s1 / (s1+s2)
  VTB_T = s2 / (s1+s2)
  
  VTBProb=matrix(c(0, VTB_F,
                   1, VTB_T), nrow=2, byrow = T)
  colnames(VTBProb)=c("VTB","P")
  
  probability$VTB = VTBProb
  
  #' #probability of Sm
  l = subset(his, Sm == 0, select=c(Sm))
  s1 = length(l[,1]) + 1
  l = subset(his, Sm == 1, select=c(Sm))
  s2 = length(l[,1]) + 1
  Sm_F = s1 / (s1+s2)
  Sm_T = s2 / (s1+s2)
  
  SmProb=matrix(c(0, Sm_F,
                  1, Sm_T), nrow=2, byrow = T)
  colnames(SmProb)=c("Sm","P")
  
  probability$Sm = SmProb
  
  #probability of TB
  l = subset(his, VTB == 0 & TB == 0, select=c(VTB,TB))
  s1 = length(l[,2]) + 1
  l = subset(his, VTB == 0 & TB == 1, select=c(VTB,TB))
  s2 = length(l[,2]) + 1
  TB_F_F = s1 / (s1+s2)
  TB_F_T = s2 / (s1+s2)
  l = subset(his, VTB == 1 & TB == 0, select=c(VTB,TB))
  s1 = length(l[,2]) + 1
  l = subset(his, VTB == 1 & TB == 1, select=c(VTB,TB))
  s2 = length(l[,2]) + 1
  TB_T_F = s1 / (s1+s2)
  TB_T_T = s2 / (s1+s2)
  
  TBProb=matrix(c(0,0, TB_F_F,
                  0,1,TB_F_T,
                  1,0,TB_T_F,
                  1,1,TB_T_T), nrow=4, byrow = T)
  colnames(TBProb)=c("VTB","TB","P")
  
  probability$TB = TBProb
  
  #' #probability of Br
  l = subset(his, Sm == 0 & Br == 0, select=c(Sm,Br))
  s1 = length(l[,1]) + 1
  l = subset(his, Sm == 0 & Br == 1, select=c(Sm,Br))
  s2 = length(l[,1]) + 1
  Br_F_F = s1 / (s1+s2)
  Br_F_T = s2 / (s1+s2)
  l = subset(his, Sm == 1 & Br == 0, select=c(Sm,Br))
  s1 = length(l[,1]) + 1
  l = subset(his, Sm == 1 & Br == 1, select=c(Sm,Br))
  s2 = length(l[,1]) + 1
  Br_T_F = s1 / (s1+s2)
  Br_T_T = s2 / (s1+s2)
  
  BrProb=matrix(c(0,0, Br_F_F,
                  0,1,Br_F_T,
                  1,0,Br_T_F,
                  1,1,Br_T_T), nrow=4, byrow = T)
  colnames(BrProb)=c("Sm","Br","P")
  
  probability$Br = BrProb
  
  #probability of LC
  l = subset(his, Sm == 0 & LC == 0, select=c(Sm,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Sm == 0 & LC == 1, select=c(Sm,LC))
  s2 = length(l[,1]) + 1
  LC_F_F = s1 / (s1+s2)
  LC_F_T = s2 / (s1+s2)
  l = subset(his, Sm == 1 & LC == 0, select=c(Sm,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Sm == 1 & LC == 1, select=c(Sm,LC))
  s2 = length(l[,1]) + 1
  LC_T_F = s1 / (s1+s2)
  LC_T_T = s2 / (s1+s2)
  
  LCProb=matrix(c(0,0, LC_F_F,
                  0,1,LC_F_T,
                  1,0,LC_T_F,
                  1,1,LC_T_T), nrow=4, byrow = T)
  colnames(LCProb)=c("Sm","LC","P")
  
  probability$LC = LCProb
  
  #' # probability of dy
  l = subset(his, Br == 0 & LC == 0 & Dy == 0, select=c(LC,Br,Dy))
  s1 = length(l[,1]) + 1
  l = subset(his, Br == 0 & LC == 0 & Dy == 1, select=c(LC,Br,Dy))
  s2 = length(l[,1]) + 1
  Dy_F_F_F = s1 / (s1+s2)
  Dy_F_F_T = s2 / (s1+s2)
  l = subset(his, Br == 0 & LC == 1 & Dy == 0, select=c(LC,Br,Dy))
  s1 = length(l[,1]) + 1
  l = subset(his, Br == 0 & LC == 1 & Dy == 1, select=c(LC,Br,Dy))
  s2 = length(l[,1]) + 1
  Dy_F_T_F = s1 / (s1+s2)
  Dy_F_T_T = s2 / (s1+s2)
  l = subset(his, Br == 1 & LC == 0 & Dy == 0, select=c(LC,Br,Dy))
  s1 = length(l[,1]) + 1
  l = subset(his, Br == 1 & LC == 0 & Dy == 1, select=c(LC,Br,Dy))
  s2 = length(l[,1]) + 1
  Dy_T_F_F = s1 / (s1+s2)
  Dy_T_F_T = s2 / (s1+s2)
  l = subset(his, Br == 1 & LC == 1 & Dy == 0, select=c(LC,Br,Dy))
  s1 = length(l[,1]) + 1
  l = subset(his, Br == 1 & LC == 1 & Dy == 1, select=c(LC,Br,Dy))
  s2 = length(l[,1]) + 1
  Dy_T_T_F = s1 / (s1+s2)
  Dy_T_T_T = s2 / (s1+s2)
  p_dy = matrix(c(Dy_F_F_F, Dy_F_F_T, Dy_F_T_F, Dy_F_T_T, 
                  Dy_T_F_F, Dy_T_F_T, Dy_T_T_F, Dy_T_T_T), ncol = 2, byrow = TRUE)
  
  DyProb=matrix(c(0,0,0, Dy_F_F_F,
                  0,0,1,Dy_F_F_T,
                  0,1,0,Dy_F_T_F,
                  0,1,1,Dy_F_T_T,
                  1,0,0,Dy_T_F_F,
                  1,1,0,Dy_T_T_F,
                  1,0,1,Dy_T_F_T,
                  1,1,1,Dy_T_T_T), nrow=8, byrow = T)
  colnames(DyProb)=c("Br","LC","Dy","P")
  
  probability$Dy = DyProb
  
  #probability of XR
  l = subset(his, Pn == 0 & LC == 0 & TB == 0 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 0 & LC == 0 & TB == 0 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_F_F_F_F = s1 / (s1+s2)
  XR_F_F_F_T = s2 / (s1+s2)
  
  l = subset(his, Pn == 0 & LC == 0 & TB == 1 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 0 & LC == 0 & TB == 1 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_F_F_T_F = s1 / (s1+s2)
  XR_F_F_T_T = s2 / (s1+s2)
  
  l = subset(his, Pn == 0 & LC == 1 & TB == 0 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 0 & LC == 1 & TB == 0 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_F_T_F_F = s1 / (s1+s2)
  XR_F_T_F_T = s2 / (s1+s2)
  
  l = subset(his, Pn == 0 & LC == 1 & TB == 1 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 0 & LC == 1 & TB == 1 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_F_T_T_F = s1 / (s1+s2)
  XR_F_T_T_T = s2 / (s1+s2)
  
  l = subset(his, Pn == 1 & LC == 0 & TB == 0 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 1 & LC == 0 & TB == 0 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_T_F_F_F = s1 / (s1+s2)
  XR_T_F_F_T = s2 / (s1+s2)
  
  l = subset(his, Pn == 1 & LC == 1 & TB == 0 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 1 & LC == 1 & TB == 0 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_T_T_F_F = s1 / (s1+s2)
  XR_T_T_F_T = s2 / (s1+s2)
  
  l = subset(his, Pn == 1 & LC == 1 & TB == 1 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 1 & LC == 1 & TB == 1 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_T_T_T_F = s1 / (s1+s2)
  XR_T_T_T_T = s2 / (s1+s2)
  
  l = subset(his, Pn == 1 & LC == 0 & TB == 1 & XR == 0, select=c(Pn,TB,LC))
  s1 = length(l[,1]) + 1
  l = subset(his, Pn == 1 & LC == 0 & TB == 1 & XR == 1, select=c(Pn,TB,LC))
  s2 = length(l[,1]) + 1
  XR_T_F_T_F = s1 / (s1+s2)
  XR_T_F_T_T = s2 / (s1+s2)
  
  XRProb=matrix(c(0,0,0,0, XR_F_F_F_F,
                  0,0,0,1,XR_F_F_F_T,
                  0,0,1,0,XR_F_F_T_F,
                  0,0,1,1,XR_F_F_T_T,
                  0,1,0,0,XR_F_T_F_F,
                  0,1,1,0,XR_F_T_T_F,
                  0,1,0,1,XR_F_T_F_T,
                  0,1,1,1,XR_F_T_T_T,
                  1,0,0,0,XR_T_F_F_F,
                  1,0,0,1,XR_T_F_F_T,
                  1,0,1,0,XR_T_F_T_F,
                  1,0,1,1,XR_T_F_T_T,
                  1,1,0,0,XR_T_T_F_F,
                  1,1,1,0,XR_T_T_T_F,
                  1,1,0,1,XR_T_T_F_T,
                  1,1,1,1,XR_T_T_T_T), nrow=16, byrow = T)
  colnames(XRProb)=c("Pn","LC","TB","XR","P")
  
  probability$XR = XRProb
  
  #probability of Te
  l = subset(his, Pn == 0, select=c(Pn,Te))
  avg_F = mean(l[,2])
  std_dis_F = sd(l[,2])
  l = subset(his, Pn == 1, select=c(Pn,Te))
  avg_T = mean(l[,2])
  std_dis_T = sd(l[,2])
  
  TeProb=matrix(c(0, avg_F,std_dis_F,
                  1, avg_T,std_dis_T), nrow=2, byrow = T)
  colnames(TeProb)=c("Pn","Avg","std")
  
  probability$Te = TeProb

  
  probability = list(Pn=list("0" = Pn_F,"1" = Pn_T), VTB=list("0"=VTB_F,"1"=VTB_T), Sm=list("0"=Sm_F,"1"=Sm_T), Te=list("0"=list(avg=avg_F, sd=std_dis_F),"1"= list(avg=avg_T, sd=std_dis_T)),
                     TB=list("00"=TB_F_F,"01"=TB_F_T,"10"=TB_T_F,"11"=TB_T_T),LC= list("00"=LC_F_F,"01"=LC_F_T,"10"=LC_T_F,"11"=LC_T_T),
                     Br=list("00"=Br_F_F,"01"=Br_F_T,"10"=Br_T_F,"11"=Br_T_T),
                     XR=list("0000"=XR_F_F_F_F,"0001"=XR_F_F_F_T,"0010"=XR_F_F_T_F,"0011"=XR_F_F_T_T,"0100"=XR_F_T_F_F,"0101"=XR_F_T_F_T,
                             "0110"=XR_F_T_T_F,"0111"=XR_F_T_T_T,"1000"=XR_T_F_F_F,"1001"=XR_T_F_F_T,"1010"=XR_T_F_T_F,"1011"=XR_T_F_T_T,
                             "1100"=XR_T_T_F_F,"1101"=XR_T_T_F_T,"1110"=XR_T_T_T_F,"1111"=XR_T_T_T_T),
                     Dy=list("000"=Dy_F_F_F, "001"=Dy_F_F_T, "010"=Dy_F_T_F, "011"=Dy_F_T_T, "100"=Dy_T_F_F, "101"=Dy_T_F_T,"110" =Dy_T_T_F, "111"=Dy_T_T_T))
  names(probability) = c("Pn", "VTB", "Sm", "Te", "TB", "LC", "Br", "XR", "Dy")

  
  return(probability)
}


diagnose = function(network, cases){
  probabilities = list()
  
  model=list(Pn=c(), Te=c("Pn"), VTB=c(), TB=c("VTB"),Sm=c(),LC=c("Sm"),Br=c("Sm"),XR=c("Pn", "LC","TB"),Dy=c("Br","LC"))
  rows = nrow(cases)
  unknowVars=c("Pn","TB","LC","Br")
  
  diagMatrix = matrix(0, nrow=10, ncol=4)
  
  sample_no = 1000
  burn_no = 100
  
  randomNos=runif(40*sample_no, 0, 1)
  randomIndex = 1
  
  
  Pn=1
  TB=0
  LC=1
  Br=0
  
  for(i in 1:rows)
  {
    samples = NULL
    case=cases[i,]

    case$Pn = Pn
    case$TB = TB
    case$LC = LC
    case$Br = Br
    pOld = NULL
    for(j in 1:sample_no)
    {
      for(unknowVar in unknowVars)
      {
        oldValue= case[[unknowVar]]
        
        if(is.null(pOld))
        {
          pOld = getProbability(model, case,network)
        }
        
        newValue= (oldValue+1)%%2
        case[[unknowVar]]=newValue
        pNew =  getProbability(model, case,network)
        if(pNew > pOld)
        {
          case[[unknowVar]]=newValue
          pOld = pNew
        }
        else
        {
          prob = pNew / pOld
          randomPro =randomNos[randomIndex]
          randomIndex = randomIndex + 1
          if(randomPro < prob)
          {
            case[[unknowVar]] = newValue
            pOld = pNew
          }
          else
          {
            case[[unknowVar]] = oldValue
          }
        }
      }
      
      Pn=case$Pn
      TB=case$TB
      LC=case$LC
      Br=case$Br
      samples=rbind(samples,case)
    }
    
    randomSamples = sample(1:sample_no,burn_no)
    
    burnSample = samples[-randomSamples,]
    
    totalNum = sample_no - burn_no
    l = subset(burnSample, Pn == 1, select=c(Pn))
    s2 = length(l[,1])
    Pn_T = s2 / totalNum
    diagMatrix[i,1]=Pn_T
    
    
    l = subset(burnSample, VTB == case$VTB & TB == 1, select=c(VTB,TB))
    s2 = length(l[,2])
    
    TB_F_T = s2 / totalNum
    
    diagMatrix[i,2]=TB_F_T
    
    
    l = subset(burnSample, Sm == case$Sm & LC == 1, select=c(Sm,LC))
    s2 = length(l[,1])
    LC_T_T = s2 / totalNum
    diagMatrix[i,3]=LC_T_T
    
    l = subset(burnSample, Sm == case$Sm & Br == 1, select=c(Sm,Br))
    s2 = length(l[,1])
    
    Br_T_T = s2 / totalNum
    diagMatrix[i,4]=Br_T_T
  }
  
  colnames(diagMatrix)=c("Pn","TB","LC","Br")
  rownames(diagMatrix)=1:10
  return(diagMatrix)
}


getProbability=function(model, case, network){
  probability = 1
  
  for(name in names(model))
  {
    subProb=getColProb(name,model,case, network)
    probability = probability* subProb
  }
  
  return (probability)
}

getColProb =function(name, model, case,network)
{
  dependentCols= c(name)
  if(!is.null(model[[name]]))
  {
    dependentCols = append(model[[name]], dependentCols)
  }
  proName =""
  
  if(name == "Te")
  {
    proName = as.character(case$Pn)
    subProb = dnorm(case[[name]], network[[name]][[proName]]$avg, network[[name]][[proName]]$sd)
  }
  else
  {
    for(col in dependentCols)
    {
      proName=paste(proName, as.character(case[[col]]), sep="")
    }
    
    
    subProb = network[[name]][[proName]]
  }
  
  return (subProb)
}

#' runDiagnostics
#'
#' The run function for the diagnostics project. You need to pass the two functions indicated in the parameters.
#'
#' Evaluation will be performed on new, randomly generated cases. Like here, your will get a MAE between your
#' estimates of your model and the true probabilities from the generative model (the model used to generate
#' the data).
#'
#' We will perform 1000 runs using the par function, which performs Metropolis within Gibbs MCMC sampling
#' on a network trained on the historical data using count parameters that start at one for the
#' categorical/conditional-categorical distributions, and a maximum likelihood approach for the normal
#' distribution. To pass your performance (MAE) need to be better than or equal to the worst performing of
#' these 1000 runs.
#'
#' All groups will be tested on the sampling that begin after the random seed is again set to a specific
#' value.
#'
#' Your code will also need to complete within 60 seconds. The par function using 1000 samples are completes
#' in about 22 seconds. If code runs slowly on the evaluation machine, you will code will need to complete
#' quicker than the time taken by the slowest of the 1000 runs plus 25%.
#'
#' If you want to load the historical cases (training data) or test cases (test data) use data(hist) or data(cases). See information
#' about this data in the help documentation (?hist or ?Diagnostics::hist)
#'
#' @param learn Your function which will create the network. It should accept as a single
#' argument the historical cases as returned by the Get_Historical_Data function.
#' @param diagnose Your function which will use the network created by your learn function
#' to estimate probabilities of unknown variables in a set of cases.
#' This function should take two arguments: (1) your network, as returned by your
#' learn function; and (2) The cases, as returned by the Get_Cases function.
#' @param verbose Controls the amount of output printed to console. 0 prints nothing. 1 prints your
#' time taken. 2 prints the time take plus the mean absolute error (MAE) of your estimates compared with the
#' true probabilities of the generative model, as well as the MAE of analytics solutions given the historic
#' data (with Bayesian expectation and maximum likelihood calculations) and the MAE of one precalculated run of
#' the par function using 1000 samples (as it will in the evaluation) and
#' one precalculated run of the par model using 5000 samples (to give you an indication of how much
#' improvement additional samples will make).
#' @return The output from your diagnose function.
#' @export
runDiagnostics = function (learn,diagnose,verbose=0) {
  startTime=Sys.time()
  network=learn(hist)
  estimates=diagnose(network,cases)
  ground_truth_probabilities=Get_Cases_Analytic_Generative_Model_Probabilities()
  mae=compareEstimates(estimates,ground_truth_probabilities)
  endTime=Sys.time()
  if (verbose==2) {
    cat("\nYour mean absolute error (MAE):",mae)
    mae_hist_bayes=compareEstimates(Get_Cases_Analytic_Empirical_Probabilities_With_Bayesian(),ground_truth_probabilities)
    cat("\nThe MAE of an analytic solution using Bayesian expectation:",mae_hist_bayes)
    mae_hist_ml=compareEstimates(Get_Cases_Analytic_Empirical_Probabilities_Without_Bayesian(),ground_truth_probabilities)
    cat("\nThe MAE of an analytic solution using maximum likelihood:",mae_hist_ml)
    mae_mike_1000=compareEstimates(Get_Cases_Mikes_Model_1000(),ground_truth_probabilities)
    cat("\nThe MAE of (one run of) the par model:",mae_mike_1000)
    mae_mike_5000=compareEstimates(Get_Cases_Mikes_Model_5000(),ground_truth_probabilities)
    cat("\nThe MAE of (one run of) the par model using 5000 MCMC samples instead of 1000:",mae_mike_5000)
  }
  if (verbose>0)
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  return(mae)
}
#' @keywords internal
compareEstimates=function(e1,e2){
  mean(abs(e1-e2))
}
#' @keywords internal
Get_Cases_Analytic_Generative_Model_Probabilities=function(){
  out=matrix(c(
    0.000, 0.043, 0.924, 0.139,
    1.000, 0.014, 0.013, 0.028,
    0.000, 0.749, 0.419, 0.164,
    0.000, 0.146, 0.038, 0.184,
    0.001, 0.314, 0.278, 0.025,
    0.000, 0.020, 0.979, 0.469,
    1.000, 0.004, 0.002, 0.028,
    0.000, 0.003, 0.001, 0.028,
    0.000, 0.003, 0.036, 0.184,
    1.000, 0.453, 0.573, 0.565),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Analytic_Empirical_Probabilities_With_Bayesian=function(){
  out=matrix(c(
    0.000, 0.053, 0.899, 0.160,
    1.000, 0.012, 0.009, 0.026,
    0.000, 0.791, 0.350, 0.171,
    0.000, 0.120, 0.043, 0.177,
    0.002, 0.322, 0.240, 0.025,
    0.000, 0.020, 0.979, 0.368,
    1.000, 0.004, 0.002, 0.026,
    0.000, 0.002, 0.001, 0.026,
    0.000, 0.002, 0.033, 0.177,
    1.000, 0.414, 0.544, 0.520),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Analytic_Empirical_Probabilities_Without_Bayesian=function(){
  out=matrix(c(
    0.000, 0.053, 0.899, 0.159,
    1.000, 0.012, 0.009, 0.026,
    0.000, 0.793, 0.353, 0.171,
    0.000, 0.115, 0.039, 0.177,
    0.002, 0.323, 0.239, 0.025,
    0.000, 0.020, 0.979, 0.368,
    1.000, 0.002, 0.001, 0.026,
    0.000, 0.002, 0.001, 0.026,
    0.000, 0.002, 0.033, 0.177,
    1.000, 0.444, 0.565, 0.513),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Mikes_Model_1000=function(){
  out=matrix(c(
    0, 0.054, 0.902, 0.169,
    1, 0.017, 0.008, 0.032,
    0, 0.791, 0.346, 0.176,
    0, 0.115, 0.042, 0.180,
    0, 0.319, 0.246, 0.027,
    0, 0.017, 0.984, 0.365,
    1, 0.005, 0.006, 0.025,
    0, 0.004, 0.000, 0.030,
    0, 0.003, 0.038, 0.164,
    1, 0.427, 0.540, 0.513),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Mikes_Model_5000=function(){
  out=matrix(c(
    0.000, 0.051, 0.900, 0.165,
    1.000, 0.012, 0.007, 0.024,
    0.000, 0.794, 0.350, 0.166,
    0.000, 0.116, 0.047, 0.183,
    0.002, 0.328, 0.236, 0.027,
    0.000, 0.023, 0.976, 0.364,
    1.000, 0.009, 0.005, 0.021,
    0.000, 0.003, 0.001, 0.027,
    0.000, 0.002, 0.031, 0.183,
    1.000, 0.418, 0.538, 0.519),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
