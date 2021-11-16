
library(GA)

# questions:
# - ali je misljeno da generiramo same validne enacbe? (z velikim popSize je resitev najti trivialno)
# - ali moramo porabiti vsa stevila?
# - kaksen je input? 

numbers_and_operators <- c("10","25","100","5","3","+","-","/","*")
goal <- 2512


myInitPopulation <- function(object) 
{
  populationSize <- object@popSize
  n <- seq.int(object@lower, object@upper)
  n <- length(n)
  print(n)
  n <- 9
  population <- matrix(nrow=populationSize,ncol=n)
  
  for(i in 1:populationSize) {
    # ustvarimo validno enacbo
    
    # oblika: S,O,S,O,S,O,S,O,S
    
    # premesamo stevila in vstavimo v enacbo
    chosenNum <- sample(c(1:5),5)
    p <- c(1:9)
    p[c(1,3,5,7,9)] <- chosenNum
    
    j <- 2
    for(x in 1:4) {
      opIndex <- sample(c(6:9),1)
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
  
  print("Fitness eq")
  print(numbers_and_operators[equation])
  str <- paste(numbers_and_operators[equation], collapse="")
  print("STR")
  print(str)
  evaluated <- eval(parse(text=str))

  # print(equation)
  # print(str)
  # print(evaluated)
  
  diff <- abs(goal - evaluated)
  
  
  return(1/(diff + 1))
}

count <- 0

myCrossover <- function(object,parents)
{
  print("ZACETEK CROSSOVERJA")
  print(numbers_and_operators[object@population])
  parents <- object@population[parents,,drop = FALSE]
  n <- length(parents[1,])
  # Izberemo iz katerega parenta se vzame prvi del in
  # iz katerega drugi.
  prvidelParent <- sample(c(1,2),1)
  drugidelParent <- 3 - prvidelParent
  ix <- sample(c(2:n-1), 1)
  child_one <- c(parents[prvidelParent, 1:ix], parents[drugidelParent, (ix+1):n])
  child_two <- c(parents[drugidelParent, 1:ix], parents[prvidelParent, (ix+1):n])
  #print("Starsa")
  #print(parents[1,])
  #print(parents[2,])
  #print("Otrok 1")
  #print(child_one)
  #print("Otrok 2")
  #print(child_two)
  #print("Part one")
  #print("Eq 1")
  #print(numbers_and_operators[child_one])
  #print("Eq 2")
  #print(numbers_and_operators[child_two])
  #print(prvidelParent)
  #print("part two")
  #print(drugidelParent)
  #print("ix")
  #rint(ix)
  children <- matrix(nrow = 2, ncol = n)
  children[1,] <- child_one
  children[2,] <- child_two
  children <- c(child_one, child_two)
  #fitness <- c(myFitness(child_one), myFitness(child_two))
  fitness<- c(1,1)
  count <- count + 1
  print("Count")
  print(count)
  print("KONEC CROSSOVERJA")
  #return(list(children=parents, fitness = fitness))
  return(list(children=children, fitness = rep(NA, 2)))
  
}

myCrossover2 <- function(object, parents){
  print("CROSSOVER")
  print("population")
  for (i in 1:object@popSize){
    print(numbers_and_operators[object@population[i,]])
  }
  
  print(numbers_and_operators[object@population])
  parents <- object@population[parents,, drop=FALSE]
  n <- ncol(parents)
  prvidelStarsa <- sample(c(1,2), 1)
  drugidelStarsa <- 3 - prvidelStarsa
  ix <- sample(c(2:n-1), 1)
  children <- matrix(as.double(NA), nrow=2, ncol=n)
  children[1,1:ix] <- parents[prvidelStarsa, 1:ix]
  children[1, (ix+1):n] <- parents[drugidelStarsa, (ix+1):n]
  children[2,1:ix] <- parents[drugidelStarsa, 1:ix]
  children[2, (ix+1):n] <- parents[prvidelStarsa, (ix+1):n]
  
  out <- list(children=children, fitness = rep(NA,2))
  print("KONEC CROSSOVERJA")
  return(out)
}

cros <- function(object, parents)
{
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  #
  
  print("-------POP----------")
  for (i in 1:object@popSize){
    print(numbers_and_operators[object@population[i,]])
    #nas = is.na()
  }
  print("---------------------")
  cxPoints <- sample(1:n, 1)
  children <- matrix(nrow = 2, ncol = n)
  children[1,] <- parents[1,]
  children[2,] <- parents[2,]
  print("STARSI")
  print(numbers_and_operators[parents[1,]])
  print(numbers_and_operators[parents[2,]])
  children[1,3] <- parents[2,3]
  children[2,3] <- parents[1,3]
  #
  print("otric")
  for(i in 1:2){
    print(numbers_and_operators[children[i,]])
  }
  #
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
}

myMutation <- function(object, parent) 
{
  return(parent)
}

sel <- function(object){
  s <- c(1:object@popSize)
  return (list(population=object@population[s,], fitness=object@fitness[s]))
}

fit <- function(eq){
  return (1)
}


# GA <- ga(type <- "permutation", fitness<- function(x){1}, lower <- c1, upper <- 2, popSize <- 50, maxiter <- 5000, run <- 500, pmutation <- 0.2)
GA1 <- ga(type = "permutation", fitness=fit , lower = 1, upper = 9, population=myInitPopulation, popSize = 20, maxiter = 4, pcrossover = 0.1, crossover = cros, mutation = myMutation)