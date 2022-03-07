A <- 1.291 #Begining of initial interval
B <- 2.582 #End of initial interval
freedom <- 5 #Degrees of freedom
alpha <- 0.1 #Critical Threshold
while (B-A >= 0.0001){ #While loop continues until desired 10^-4 minimum interval distance is reached
  aPvalue <- pt(-abs(A), df = freedom) #Calculates p value associated with the begining of the interval. 
  bPvalue <- pt(-abs(B), df = freedom) #Calculates p value associated with the end of the interval
  cPvalue <- pt(-abs(A + ((B-A)/2)), df = freedom) #Calculates p value associated with midpoint of the interval
  if (cPvalue > alpha){A <- A + ((B-A)/2)}
  else {B <- A + ((B-A)/2)}
}
A #Lower bound.
B #Upper bound.
(A + B) / 2 #Point estimate.
