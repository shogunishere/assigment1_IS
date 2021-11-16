
library(GA)

# questions:
# - ali je misljeno da generiramo same validne enacbe? (z velikim popSize je resitev najti trivialno)
# - ali moramo porabiti vsa stevila?
# - kaksen je input? 

numbers_and_operators <- c("4","25","66","5","3", "13","/", "+","-","*")
goal <- 1000
num_numbers = 0
for (i in 1:length(numbers_and_operators)){
  if (!is.na(as.numeric(numbers_and_operators[i]))){
    num_numbers = num_numbers + 1
  }
}
num_operators = length(numbers_and_operators) - num_numbers
expression_length = 2*num_numbers-1
num_operators_in_exp = expression_length - num_numbers

myInitPopulation <- function(object) 
{
  populationSize <- object@popSize
  #n <- seq.int(object@lower, object@upper)
  #n <- length(n)
  population <- matrix(nrow=populationSize,ncol=expression_length)
  
  for(i in 1:populationSize) {
    # ustvarimo validno enacbo
    
    # oblika: S,O,S,O,S,O,S,O,S
    
    # premesamo stevila in vstavimo v enacbo
    chosenNum <- sample(c(1:num_numbers),num_numbers)
    p <- c(1:expression_length)
    #seq(1,expression_length, by=2)
    #p[c(1,3,5,7,9)] <- chosenNum
    p[seq(1, expression_length, by=2)] <- chosenNum
  
    j <- 2
    for(x in 1:num_operators_in_exp) {
      opIndex <- sample(c((num_numbers+1):length(numbers_and_operators)),1)
      
      p[j] <- opIndex
      j <- j + 2
    }
    
    # izpis
    #print(numbers_and_operators[p])
    population[i,] <- p
  }
  
  #print(numbers_and_operators[population])
  for (i in 1:length(population[,1])){
    print(numbers_and_operators[population[i,]])
  }
  return(population)
  
}


myFitness <- function(equation) 
{
  
  #print("Fitness eq")
  #print(numbers_and_operators[equation])
  str <- paste(numbers_and_operators[equation], collapse="")
  #print("STR")
  #print(str)
  evaluated <- eval(parse(text=str))

  #print("FITNESS")
  #print(equation)
  # print(str)
  #print(evaluated)
  #print("-------")
  difference <- abs(goal - evaluated)
  
  return(1/(difference + 1))
  #return(-difference)
}

cros <- function(object, parents)
{
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  #
  
  print("-------POP----------")
  for (i in 1:object@popSize){
    print(numbers_and_operators[object@population[i,]])
    nas = is.na(object@population[i,])
    ix_nas = which(nas)
  }
  print("---------------------")
  
  children <- matrix(nrow = 2, ncol = n)
  children[1,] <- parents[1,]
  children[2,] <- parents[2,]
  operators_ixs = seq(2,expression_length, by=2)
  
  # število zamenjav
  cross_iterations = sample(1:num_operators, 1)
  for (i in 1:cross_iterations) {
    # ix...indeks operatorja, ki se bo zamenjal
    ix = sample(1:length(operators_ixs), 1)
    children[1,operators_ixs[ix]] = parents[2,operators_ixs[ix]]
    children[2,operators_ixs[ix]] = parents[1,operators_ixs[ix]]
  }
  
  print("STARSI")
  print(numbers_and_operators[parents[1,]])
  print(numbers_and_operators[parents[2,]])
  #children[1,cxPoints] <- parents[2,cxPoints]
  #children[2,cxPoints] <- parents[1, cxPoints]
  #
  print("otroci")
  for(i in 1:2){
    print(numbers_and_operators[children[i,]])
  }
  #
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
}

myMutation <- function(object, parent) 
{
  mutate <- parent <- as.vector(object@population[parent,])
  numbers_ixs = seq(1, expression_length, by=2)
  shuffle_ixs = sample(numbers_ixs, num_numbers)
  
  mutate[numbers_ixs] = parent[shuffle_ixs]
  
  return(mutate)
}

myRandomSearch <- function() {
  closest = 100000
  result = 0
  equation = 0
  
  for (i in 1:1000) {
    # premesamo stevila in vstavimo v enacbo
    chosenNum <- sample(c(1:num_numbers),num_numbers)
    p <- c(1:expression_length)
    p[seq(1, expression_length, by=2)] <- chosenNum
    
    j <- 2
    for(x in 1:num_operators_in_exp) {
      opIndex <- sample(c((num_numbers+1):length(numbers_and_operators)),1)
      
      p[j] <- opIndex
      j <- j + 2
    }
    str <- paste(numbers_and_operators[p], collapse="")
    evaluated <- eval(parse(text=str))
    
    difference <- abs(goal - evaluated)
    
    if (difference < closest){
      closest = difference
      result = evaluated
      equation = p
    }
  }
  print("Najboljši rezultat")
  print(result)
  print("enačba")
  print(numbers_and_operators[equation])
}


# GA <- ga(type <- "permutation", fitness<- function(x){1}, lower <- c1, upper <- 2, popSize <- 50, maxiter <- 5000, run <- 500, pmutation <- 0.2)
GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = 30, maxiter = 200, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)

