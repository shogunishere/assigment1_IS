
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
  # for (i in 1:length(population[,1])){
  #  print(numbers_and_operators[population[i,]])
  #}
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
  
  # print("-------POP----------")
  #for (i in 1:object@popSize){
  #  print(numbers_and_operators[object@population[i,]])
  #  nas = is.na(object@population[i,])
  #  ix_nas = which(nas)
  #}
  #print("---------------------")
  
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
  
  # print("STARSI")
  #print(numbers_and_operators[parents[1,]])
  #print(numbers_and_operators[parents[2,]])
  #children[1,cxPoints] <- parents[2,cxPoints]
  #children[2,cxPoints] <- parents[1, cxPoints]
  #
  #print("otroci")
  #for(i in 1:2){
  #  print(numbers_and_operators[children[i,]])
  #}
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

myRandomSearch <- function(tolerance) {
  closest = 100000
  result = 0
  equation = 0
  difference = goal 
  
  while(difference > tolerance) {
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
  print("najboljsi rezultat")
  print(result)
  print("enacba")
  print(numbers_and_operators[equation])
  
  return(result)
}


# GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = 50, maxiter = 50, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)

# time compairison

# random search
start <- proc.time()
randomSolution = myRandomSearch(tolerance = 10)
end <- proc.time()

randomSearchTime = end - start

# GA
start <- proc.time()

GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = 50, maxiter = 50, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)
gaSolution = eval(parse(text=paste(numbers_and_operators[GA1@solution[1,]],collapse="")))

end <- proc.time()

gaTime = end - start


# various equation sizes
size3 <- c("4","25","66","/", "+","-","*")
size6 <- c("4","25","66","5","3", "13","/", "+","-","*")
size12 <- c("4","25","66","5","3", "13","4","25","66","5","3", "13","/", "+","-","*")
size16 <- c("4","25","66","5","3", "13","4","25","66","5","3", "13","13","4","25","66","/", "+","-","*")
size20 <- c("4","25","66","5","3", "13","4","25","66","5","3", "13","13","4","25","66","13","4","25","66","/", "+","-","*")

# parameters

# large population, large number of iterations
numOfIterations = 200
populationSize = 100


# medium sized population, medium number of iterations
numOfIterations = 50
populationSize = 50

# small population, small number of iterations
numOfIterations = 20
populationSize = 20
  
# size3
numbers_and_operators <- size3
goal <- 255
num_numbers = 0
for (i in 1:length(numbers_and_operators)){
  if (!is.na(as.numeric(numbers_and_operators[i]))){
    num_numbers = num_numbers + 1
  }
}
num_operators = length(numbers_and_operators) - num_numbers
expression_length = 2*num_numbers-1
num_operators_in_exp = expression_length - num_numbers

start <- proc.time()

randomSolution = myRandomSearch(tolerance = 50)

end <- proc.time()

randomSearchTime = end - start

start <- proc.time()

GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = populationSize, maxiter = numOfIterations, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)
gaSolution = eval(parse(text=paste(numbers_and_operators[GA1@solution[1,]],collapse="")))

end <- proc.time()

gaTime = end - start


# size6
numbers_and_operators <- size6
goal <- 1023
num_numbers = 0
for (i in 1:length(numbers_and_operators)){
  if (!is.na(as.numeric(numbers_and_operators[i]))){
    num_numbers = num_numbers + 1
  }
}
num_operators = length(numbers_and_operators) - num_numbers
expression_length = 2*num_numbers-1
num_operators_in_exp = expression_length - num_numbers

start <- proc.time()

randomSolution = myRandomSearch(tolerance = 50)

end <- proc.time()

randomSearchTime = end - start

start <- proc.time()

GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = populationSize, maxiter = numOfIterations, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)
gaSolution = eval(parse(text=paste(numbers_and_operators[GA1@solution[1,]],collapse="")))

end <- proc.time()

gaTime = end - start


# size12
numbers_and_operators <- size12
goal <- 5713
num_numbers = 0
for (i in 1:length(numbers_and_operators)){
  if (!is.na(as.numeric(numbers_and_operators[i]))){
    num_numbers = num_numbers + 1
  }
}
num_operators = length(numbers_and_operators) - num_numbers
expression_length = 2*num_numbers-1
num_operators_in_exp = expression_length - num_numbers

start <- proc.time()

randomSolution = myRandomSearch(tolerance = 50)

end <- proc.time()

randomSearchTime = end - start

start <- proc.time()

GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = populationSize, maxiter = numOfIterations, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)
gaSolution = eval(parse(text=paste(numbers_and_operators[GA1@solution[1,]],collapse="")))

end <- proc.time()

gaTime = end - start


# size16
numbers_and_operators <- size16
goal <- 10809
num_numbers = 0
for (i in 1:length(numbers_and_operators)){
  if (!is.na(as.numeric(numbers_and_operators[i]))){
    num_numbers = num_numbers + 1
  }
}
num_operators = length(numbers_and_operators) - num_numbers
expression_length = 2*num_numbers-1
num_operators_in_exp = expression_length - num_numbers

start <- proc.time()

randomSolution = myRandomSearch(tolerance = 50)

end <- proc.time()

randomSearchTime = end - start

start <- proc.time()

GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = populationSize, maxiter = numOfIterations, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)
gaSolution = eval(parse(text=paste(numbers_and_operators[GA1@solution[1,]],collapse="")))

end <- proc.time()

gaTime = end - start

# size20
numbers_and_operators <- size20
goal <- 50011
num_numbers = 0
for (i in 1:length(numbers_and_operators)){
  if (!is.na(as.numeric(numbers_and_operators[i]))){
    num_numbers = num_numbers + 1
  }
}
num_operators = length(numbers_and_operators) - num_numbers
expression_length = 2*num_numbers-1
num_operators_in_exp = expression_length - num_numbers

start <- proc.time()

randomSolution = myRandomSearch(tolerance = 50)

end <- proc.time()

randomSearchTime = end - start

start <- proc.time()

GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = expression_length, population=myInitPopulation, popSize = populationSize, maxiter = numOfIterations, pcrossover = 0.4, pmutation = 0.1, crossover = cros, mutation = myMutation)
gaSolution = eval(parse(text=paste(numbers_and_operators[GA1@solution[1,]],collapse="")))

end <- proc.time()

gaTime = end - start


# ==================UNFINISHED FROM HERE========================
# plot quality of GA solution ~ population size
cars <- c(1, 3, 6, 4, 9)
plot(cars, type="o", col="blue")
axis(1, at=1:5, lab=c("20","50","100"))
title(main="Goal = 1000, Equation Size = 6")

# plot quality of GA solution ~ number of iterations
plot(cars, type="o", col="blue")


# plot quality of random search solution ~ random search for loop iteration
plot(cars, type="o", col="blue")

# compare random search solution quality with GA (diff. from goal)

