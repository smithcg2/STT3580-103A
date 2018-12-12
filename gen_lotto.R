# Generate lotto numbers
# Avoid a split pot
# Author: Jared Knowles
# Date: 11/26/2012

# All numbers are equally likely to be selected to win in PowerBall, 
# but what fun is winning if we have to share the pot?
# Therefore, we will only select numbers that minimize our 
# likelihood of sharing the pot. 

gen_lotto<-function(){
  white<-seq(1:69)
  red<-1:24
  
  probs<-white
  # Decrease probabilities for commonly chosen numbers
  probs[probs<=31]<-1/(2*59)
  probs[probs>=32]<-1/14
  
  # We need 5 white
  w<-sample(white,5,prob=probs)
  # We need 1 PowerBall
  r<-sample(red,1)
  # Print results
  cat(" White Balls:",w[order(w)],"\n","Powerball:",r)
  # Make a good warning
  cat("\n Remember, your odds of winning: \n","1 in 195,249,054")
}

gen_lotto()
