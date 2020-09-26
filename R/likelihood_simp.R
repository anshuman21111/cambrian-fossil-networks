#Likelihood functions

likSingle1 = function(parameters, x) dnorm(x, mean = parameters["mu1"],sd = parameters["sd1"])

llikAll1 = function(parameters, x){
  if (parameters["sd1"] < 0)
  {-Inf}
  else{sum(log(likSingle1(parameters, x)))}}

likSingle3 = function(parameters, x){dnorm(x, mean = parameters["mu1"], sd = parameters["sd1"]) * parameters["p1"] +dnorm(x, mean = parameters["mu2"], sd = parameters["sd2"]) *parameters["p2"] + dnorm(x, mean = parameters["mu3"],sd = parameters["sd3"]) * (1 - parameters["p1"] - parameters["p2"])}

llikAll3 = function(parameters, x){
  if (parameters["p1"] < 0 || parameters["p1"] > 1 || parameters["p2"] <0 || parameters["p2"] > 1 || parameters["p1"] + parameters["p2"] >1 || parameters["sd1"] < 0 || parameters["sd2"] < 0 || parameters["sd3"]<0){-Inf}
  else{sum(log(likSingle3(parameters, x)))}}