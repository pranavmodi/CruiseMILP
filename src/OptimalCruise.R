library(lpSolveAPI)

tourObject <- read.csv("input/tourobject.csv",stringsAsFactors=FALSE)
routes <- read.csv("input/routes.csv",stringsAsFactors=FALSE)
classes <- read.csv("input/classes.csv",stringsAsFactors=FALSE)
tourComplex <- read.csv("input/tourComplex.csv",stringsAsFactors=FALSE)

#budget
K <- 500
num.route.is.included <- nrow(routes)
num.TO.on.route <- nrow(tourObject)*nrow(routes)
num.cap.increments <- nrow(tourComplex)
num.of.vars <- num.TO.on.route + num.route.is.included + num.cap.increments
lpmodel <- make.lp(0,num.of.vars,verbose="full")
#TO capacity constraints
for (i in 1:nrow(tourObject)){
  torep <- rep(0,nrow(tourObject))
  torep[i] <- 1
  tocons <- rep(torep,nrow(routes))
  cons <- c(rep(0,num.route.is.included),tocons,rep(0,num.cap.increments))
  add.constraint(lpmodel,cons, "<=", tourObject$Capacity[i])
}
#Tc capacity constraints
for (tc in 1:nrow(tourComplex)){
  closeStr <- tourComplex[tourComplex$Number==tc,"closeTO"]
  b <- strsplit(closeStr,";")[[1]]
  closeOb <- as.numeric(b)
  initcap <- tourComplex[tourComplex$Number==tc,"InitCap"]
  tocons <- rep(0,nrow(tourObject)*nrow(routes))
  addlTimes <- tourObject[tourObject$Number %in% closeOb,"AddlTime"]
  routeVec <- rep(0,nrow(tourObject))
  routeVec[closeOb] <- addlTimes
  tcVec <- rep(0,nrow(tourComplex))
  tcVec[tc] <- -1
  cons <- c(rep(0,num.route.is.included),rep(routeVec,nrow(routes)),tcVec)
  print(cons)
  print(initcap)
  add.constraint(lpmodel,cons, "<=", initcap)
}
#number of objects of class in route constraint
for (r in 1:nrow(routes)){
  for (c in 1:max(tourObject$Class)){
    nl <- classes[(classes$Number==c & classes$Route==r),"MinRoute"]
    nh <- classes[classes$Number==c & classes$Route==r,"MaxRoute"]
    to.in.class <- tourObject[tourObject$Class==c,"Number"]
    inds <- rep(0,nrow(tourObject))
    for (a in to.in.class){
      inds[a] <- 1
    }
    fin <- rep(0,nrow(tourObject)*nrow(routes))
    fin[((r-1)*nrow(tourObject)+1):(r*nrow(tourObject))] <- inds
    cons <- c(rep(0,num.route.is.included),fin,rep(0,num.cap.increments))
    add.constraint(lpmodel,cons, ">=", nl)
    add.constraint(lpmodel,cons, "<=", nh)
  }  
}
# Route duration
for (r in 1:nrow(routes)){
 minD <- routes[routes$Number==r,"minDuration"]
 maxD <- routes[routes$Number==r,"maxDuration"]
 tobs <- rep(0,nrow(routes)*nrow(tourObject))
 tobs[((r-1)*nrow(tourObject)+1):(r*nrow(tourObject))] <- tourObject$AddlTime
 cons <- c(rep(0,num.route.is.included),tobs,rep(0,num.cap.increments))
 print(cons)
 print(paste(minD,maxD))
 add.constraint(lpmodel,cons, ">=", minD)
 add.constraint(lpmodel,cons, "<=", maxD) 
}
#budget constraint
cons <- c(rep(0,num.route.is.included),rep(0,nrow(tourObject)*nrow(routes)),tourComplex$Cost)
add.constraint(lpmodel,cons, "<=", K)
#add obj fun
a <- tourObject$Attractiveness*tourObject$Time
obfn <- -1*c(rep(0,num.route.is.included),rep(a,nrow(routes)),rep(0,nrow(tourComplex)))
set.objfn(lpmodel, obfn)
#Add constraint types  
set.type(lpmodel, 1:num.route.is.included, "binary")
set.type(lpmodel, (num.route.is.included+1):(num.route.is.included + num.TO.on.route), "binary")
set.type(lpmodel, (num.route.is.included + num.TO.on.route + 1):num.of.vars, "integer")
solve(lpmodel)

decision.vars <- get.variables(lpmodel)
decision.vars <- decision.vars[(num.route.is.included+1):(length(decision.vars)-num.cap.increments)]
print(decision.vars)
objective <- get.objective(lpmodel)
print(paste("Optimized Objective: ",(-1*objective)))

for (i in 1:nrow(routes)){
  chosen <- decision.vars[(nrow(tourObject)*(i-1)+1):(i*nrow(tourObject))]
  print(paste("Port Calls in Cruise",i,"(Number of days:",routes$Days[i],")"))
  print(paste("Total visiting time: ",sum(tourObject[tourObject$Number %in% which(chosen==1),"AddlTime"])))
  print(tourObject[tourObject$Number %in% which(chosen==1),c("Name","TypeName","AddlTime")])
  cat("\n")
}
