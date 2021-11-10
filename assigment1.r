
library(GA)

# questions:
# - ali je misljeno da generiramo same validne enacbe? (z velikim popSize je resitev najti trivialno)
# - ali moramo porabiti vsa stevila?
# - kaksen je input? 

numbers_and_operators = c("10","25","100","5","3","+","???","/","???"," ")
goal = 2512


myInitPopulation <- function(object) 
{
  populationSize <- object@popSize
  n <- seq.int(object@lower, object@upper)
  n <- length(n)
  population <- matrix(nrow=populationSize,ncol=n)
  
  for(i in 1:populationSize) {
    # ustvarimo validno enacbo
    
    # oblika: S,O,S,O,S,O,S,O,S
    
    # premesamo stevila in vstavimo v enacbo
    chosenNum = sample(c(1:5),5)
    p = c(1:9)
    p[c(1,3,5,7,9)] = chosenNum
    
    j = 2
    for(x in 1:4) {
      opIndex = sample(c(6:9),1)
      p[j] = opIndex
      j = j + 2
    }
    
    # izpis
    print(numbers_and_operators[p])
    population[i,] <- p
  }
  
  # print(population)
  population
  
}


myFitness <- function(equation) 
{
  str = paste(numbers_and_operators[equation], collapse="")
  evaluated = eval(parse(text=str))

  # print(equation)
  # print(str)
  # print(evaluated)
  
  diff = abs(goal - evaluated)
  
  
  return(1/(diff + 1))
}



myCrossover <- function(object,parents)
{
  parents <- object@population[parents,,drop = FALSE]
  return(parents)
  
}

myMutation <- function(object, parent) 
{
  return(parent)
}



# GA <- ga(type = "permutation", fitness= function(x){1}, lower = c1, upper = 2, popSize = 50, maxiter = 5000, run = 500, pmutation = 0.2)
GA1 <- ga(type = "permutation", fitness=myFitness , lower = 1, upper = 9, population=myInitPopulation, popSize = 50, maxiter = 100, crossover = myCrossover, mutation = myMutation, run = 1)
