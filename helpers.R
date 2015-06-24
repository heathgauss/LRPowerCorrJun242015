LRPowerCorr <- function (sampsize, nsims, p, 
                         alpha,
                         pcx1, pcx2,
                         org1, org2,
                         k1, k2)
{
  #Define some things that don't need to be re-defined every iteration
  
  #The data.frame reject= '1' or '0' is stored in @ the end of each iteration
  base <<- data.frame(Reject=numeric())
  
  f = .00001
  #b0 = log(avep/(1-avep))
  sampsizeg1 = sampsize * (1 - pcx2)
  sampsizeg2 = sampsize * pcx2
  sampsizeg1x1yes = sampsizeg1 * pcx1
  sampsizeg2x1yes = sampsizeg2 * pcx1
  sampsizeg1x1no = sampsizeg1 - sampsizeg1x1yes
  sampsizeg2x1no = sampsizeg2 - sampsizeg2x1yes
  b = sampsizeg1x1yes / (1 + k1)
  d = k1 * b
  c = (sampsizeg1x1no * k1) / (org1 + k1)
  a = (org1 * c) / k1
  
  B = sampsizeg2x1yes / (1 + k2)
  D = k2 * B
  C = (sampsizeg2x1no * k2) / (org2 + k2)
  A = (org2 * C) / k2
  
  or0 = c/a
  or1 = (a*d)/(b*c)
  or2 = (a*C)/(A*c)
  or3 = (A*D*b*c)/(B*C*a*d)
  #pcx1 = (b + d + B + D) / (a + b + c + d + A + B + C + D)
  #pcx2 = (A + B + C + D) / (a + b + c + d + A + B + C + D)
  #sampsize = a + b + c + d + A + B + C + D
  #Cholesky's Decomposition
  corr <-chol(matrix(c(1, p, p,
                       p, 1, p,
                       p, p, 1),
                     nrow = 3, ncol = 3))
  
  #This function generates numbers each iteration
  Generate <- function(n){
    
    #Generate Binomial Variables
    x1=rbinom(n,1,pcx1)
    x2=rbinom(n,1,pcx2)
    
    #Normalize the binomial variables
    cx1= (x1 - pcx1)/sqrt((pcx1)*(1-pcx1))
    cx2= (x2 - pcx2)/sqrt((pcx2)*(1-pcx2))
    
    x3=x1*x2
    meanx3=mean(x3)
    stdx3=sd(x3)
    cx3= (x3 - mean(x3)) / sd(x3)
    xy = data.frame(runif(n))
    #Pass Cx1-3 into a data.frame
    gen<-data.frame(cx1,cx2,cx3)
    sumx3<-data.frame(meanx3,stdx3)
    #Store ^^ Data.frame as a Global Matrix
    center<<-data.matrix(gen, rownames.force = NA)
    sumx3<<-data.matrix(sumx3, rownames.force = NA)
    xy<<-data.matrix(xy, rownames.force = NA)
  }
  
  #Begin looping through each iteration/simulation
  for(i in 1:nsims) {
    Generate(sampsize)
    cy = center %*% corr
    cx1 = cy[,1]
    cx2 = cy[,2]
    cx3 = cy[,3]
    cx1 = (cx1*sqrt((pcx1)*(1-pcx1)))+pcx1
    cx2 = (cx2*sqrt((pcx2)*(1-pcx2)))+pcx2
    cx3 = (cx3*sumx3[1,2]) + sumx3[1,1] 
    multiple3 <- data.frame(cx1, cx2, cx3, xy)
    multiple3$logit <- with(multiple3, 
                            log(or0) + 
                              (log(or1)+f)*(cx1) +
                              (log(or2)+f)*(cx2) +
                              (log(or3)+f)*(cx3)
    )
    multiple3$prob <- with(multiple3,
                           (exp(logit))/(1+exp(logit))
    )
    multiple3$y <- with(multiple3,
                        ifelse(xy<=prob,1,0)
    )
    
    
    #I may not be pulling the right number here
    fullLR <- deviance(glm(y ~ cx1+cx2+cx3, data=multiple3, family=binomial(logit)))
    reducLR <- deviance(glm(y ~ cx1+cx2, data=multiple3, family=binomial(logit)))
    likelihoodratio = reducLR - fullLR
    critval = qchisq(1-alpha, 1)
    both <- data.frame(likelihoodratio, critval)
    Reject <- with(both, ifelse(likelihoodratio >= critval, 1, 0))
    base <<- rbind(base, data.frame(Reject))
  }
  
  x = table(base)
  freq = cbind(x['1'])
  power <- freq/nsims
  se <- sqrt((power*(1-power))/nsims)
  LCL <- power - (1.96*se)
  UCL <- power + (1.96*se)
  
  #cat("\nx: ", x)
  #cat("\nfreq: ", freq)
  #   cat("\nPower: ", power)
  #   cat("\nse: ", se)
  #   cat("\nLCL: ", LCL)
  #   cat("\nUCL: ", UCL)
  
  
  
  #   cat("\n\nSample Size =", sampsize, "; Simulations =", nsims, "; Rho =", p, "; P(Y=1) =", or0,
  #       "\nOR1 =", or1, "; OR2 =", or2, "; OR3 =", or3, 
  #       "\nPower:", round(power*100,2), "%      LCL:", round(LCL*100,2), "%      UCL:", round(UCL*100,2),
  #       "%")
  
  # I may decide to use the following three calls of the cat function.
  
#   cat("\n\nCell Count a =", round(a, digits=2), "; Cell Count b =", round(b, digits=2), "; Cell Count c =",
#         round(c, digits=2), "; Cell Count d =", round(d, digits=2),
#       "\nMarginal Frequency for Group 1, X1=No =", sampsizeg1x1no, "; Marginal Frequency for Group 1, X1=Yes =",
#         sampsizeg1x1yes, "; Sample Size for Group 1 =", sampsizeg1)
#   cat("\n\nCell Count A =", round(A, digits=2), "; Cell Count B =", round(B, digits=2), "; Cell Count C =",
#       round(C, digits=2), "; Cell Count D =", round(D, digits=2),
#       "\nMarginal Frequency for Group 2, X1=No =", sampsizeg2x1no, "; Marginal Frequency for Group 2, X1=Yes =",
#       sampsizeg2x1yes, "; Sample Size for Group 2 =", sampsizeg2)
  
  #       "\nPower:", round(power*100,2), "%      LCL:", round(LCL*100,2), "%      UCL:", round(UCL*100,2),
  #       "%")
  
  #cat("\n\nPower:", round(power*100,2), "%      LCL:", round(LCL*100,2), "%      UCL:", round(UCL*100,2),
  #    "%")

   cat("\nCell Count a:", round(a, digits=2))
   cat("\nCell Count b:", round(b, digits=2))
   cat("\nCell Count c:", round(c, digits=2))
   cat("\nCell Count d:", round(d, digits=2))
   cat("\nMarginal Frequency for X2=0 Group, X1=0: a+c =", sampsizeg1x1no)
   cat("\nMarginal Frequency for X2=0 Group, X1=1: b+d =", sampsizeg1x1yes)
   cat("\nSample Size for X2=0 Group: n =", sampsizeg1)

   cat("\n\nCell Count A:", round(A, digits=2))
   cat("\nCell Count B:", round(B, digits=2))
   cat("\nCell Count C:", round(C, digits=2))
   cat("\nCell Count D:", round(D, digits=2))
   cat("\nMarginal Frequency for X2=1 Group, X1=0: A+C =", sampsizeg2x1no)
   cat("\nMarginal Frequency for X2=1 Group, X1=1: B+D =", sampsizeg2x1yes)
   cat("\nSample Size for X2=1 Group: N =", sampsizeg2)

   cat("\n\nPower:", round(power*100,2), "%     LCL:", round(LCL*100,2), "%     UCL:", round(UCL*100,2), "%")
  
}
