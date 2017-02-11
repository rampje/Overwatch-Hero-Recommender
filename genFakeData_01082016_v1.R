library(reshape2)
library(gtools)
g <- dplyr::glimpse

SampleVariables <- function(n){
      SampleLineup <- function(){ # get 1 sample of data ...
        SampleComp <- function(){ # 1 sample's comp data
          heros <- function(){
            # create all heros and classify them
            o <- c("Genji","McCree","Pharah",
                   "Reaper","Soldier 76","Sombra","Tracer")
            d <- c("Bastion","Hanzo","Junkrat","Symmetra",
                   "Mei","Torbjorn","Widowmaker")
            t <- c("D.Va","Reinhardt","Roadhog",
                   "Winston","Zarya")
            s <- c("Ana","Lucio","Mercy","Zenyatta")
            
            
            df <- data.frame(
              "Hero" = c(o,d,t,s),
              "Type" = c(rep("Offense", length(o)), rep("Defense", length(d)),
                         rep("Tank", length(t)), rep("Support", length(s))),
              stringsAsFactors = FALSE
            )
            df
          } #table of heros and their types
          compSelection <- function(){
            
            
            tankCount <- sample(1:2, size = 1)
            healerCount <- sample(1:2, size =1)
            remainingCount <- 6 - tankCount - healerCount
            
            tanks <- sample(heros()$Hero[heros()$Type=="Tank"], size = tankCount)
            healers <- sample(heros()$Hero[heros()$Type=="Support"], size = healerCount)
            theRest <- sample(heros()$Hero[heros()$Type %in% c("Offense","Defense")],
                              size = remainingCount)
            
            comp <- c(tanks, healers, theRest)
            comp
          } # comps for both teams
          genSample <- function(){
            HeroData <- heros()
            HeroData$CompBlue <- as.numeric(HeroData$Hero %in% compSelection())
            
            HeroData$CompRed <- as.numeric(HeroData$Hero %in% compSelection())
            HeroData <- melt(HeroData, id.vars = c("Hero","Type"))
            HeroData
          } #generate 1 sample of both teams comps
          genSample() 
        } # ...containing both teams'comps
        SampleScenario <- function(){
          scenarios <- function(){
            
            #### map data
            m1 <- c("Ilios","Lijiang Tower", "Nepal","Oasis")
            m2 <- c("Hollywood","King's Row","Numbani","Eichenwalde")
            m3 <- c("Dorado","Route 66", "Watchpoint: Gibraltar")
            m4 <- c("Hanamura", "Temple of Anubis", "Volskaya Industries")
            
            Map <- c(m1,m2,m3,m4)
            MapType <- c(rep("Assault",length(m1)),
                         rep("Escort",length(m2)),
                         rep("Hybrid", length(m3)),
                         rep("Control", length(m4)))
            Side <- c("Defending","Attacking")
            Phases <- c(rep(1,4), rep(3, 7), rep(2,3))
            nums <- c(rep(1,4),rep(1:3,7), rep(1:2,3))
            
            
            s <- paste(rep(Map,Phases), "-", nums)
            
            paste(rep(Side, 2), rep(s, 2))
            
          } #map, side, and phase possibilities
          
          MapData <- data.frame(
            "Scenario" = scenarios(),
            "Selection" = rep(0, length(scenarios())),
            stringsAsFactors = FALSE)
          
          MapData$Selection[sample(1:nrow(MapData), size = 1)] <- 1
          MapData
        } # and map, phase, and side
        
        genSampleLineUp <- function(){
          varNames <<- c(paste(SampleComp()$Hero,
                              SampleComp()$Type,
                              SampleComp()$variable),
                        SampleScenario()$Scenario)
          varValues <- c(SampleComp()$value,
                         SampleScenario()$Selection)
          
          
          varValues
        } # line up all vars as vector
        genSampleLineUp()
        }
      t(replicate(n, SampleLineup())) #put samples on wide table
    }
SampleOutcome <- function(n){ # generate vector of Win/Loss outcomes of size n
      sample(c("W","L"), size = n, replace = TRUE)
} # sampling Win/Loss



FullCompTable 


healers <- c("Ana","Lucio","Mercy","Zenyatta")
healerComps <- permutations(n=4, r=2, v=healers, repeats.allowed=F)
healerComps <- data.frame(healerComps, stringsAsFactors = FALSE)

hC2 <- data.frame(
  "X1" = c(healerComps$X1, healerComps$X2),
  "X2" = c(healerComps$X2, healerComps$X1),
  stringsAsFactors = FALSE
)
hC2[!duplicated(hC2),]


healerComps <- expand.grid(healers, healers)


healerComps <- healerComps[!(healerComps$Var1==healerComps$Var2),]
healerComps <- paste0(healerComps$Var1,"/",healerComps$Var2)
healerComps <- sort(healerComps[1:6])

## do same for 
o <- c("Genji","McCree","Pharah",
       "Reaper","Soldier 76","Sombra","Tracer")
d <- c("Bastion","Hanzo","Junkrat","Symmetra",
       "Mei","Torbjorn","Widowmaker")


t <- c("D.Va","Reinhardt","Roadhog",
       "Winston","Zarya")



#create training data
x <- 5000 # number of games sampled
fullsample <- SampleVariables(x)
train <- data.frame(fullsample)
names(train) <- varNames
train$Outcome <- SampleOutcome(x)
train <- data.frame(sapply(train, as.character))

# create logistic regression model
wide <- c(names(train)[1:45], names(train)[length(names(train))])
train <- melt(train, id.vars = wide)
train <- train[train$value==1,]
train$value <- NULL

mod1 <- glm(formula= Outcome ~ ., 
            data = train,
            family = binomial)



# test data
test <- SampleVariables(10)
test <- data.frame(test)
names(test) <- varNames
test$Outcome <- SampleOutcome(10)
test <- data.frame(sapply(test, as.character))

test <- melt(test, id.vars = wide)
test <- test[test$value==1,]


predict.glm(mod1, newdata = test, type = "response")



