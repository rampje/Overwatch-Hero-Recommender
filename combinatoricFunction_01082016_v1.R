o <- c("Genji","McCree","Pharah",
       "Reaper","Soldier 76","Sombra","Tracer")
d <- c("Bastion","Hanzo","Junkrat","Symmetra",
       "Mei","Torbjorn","Widowmaker")


nondpsCompsTable <- function(){
  healers <- c("Ana","Lucio","Mercy","Zenyatta")
  healerComps <- data.frame(combinations(n = 4, r = 2, 
                                         repeats.allowed = FALSE,
                                         v = healers))
  healerComps <- paste(healerComps$X1, "/", healerComps$X2)
  healerComps <- c(healerComps, healers)
  
  tanks <- c("D.Va","Reinhardt","Roadhog","Winston","Zarya")
  tankComps <- data.frame(combinations(n = 5, r = 2,
                                       repeats.allowed = FALSE,
                                       v = tanks))
  tankComps <- paste(tankComps$X1, "/", tankComps$X2)
  tankComps <- c(tankComps, tanks)
  
  paste(healerComps, "--", tankComps)
  
  nondpsComps <- data.frame(
    "tankComps" = rep(tankComps, 10),
    "healerComps" = rep(healerComps, 15),
    "tankCount" = rep(c(rep(2,10), rep(1,5)), 10),
    "healerCount" = rep(c(rep(2,6),rep(1,4)), 15),
    stringsAsFactors = FALSE
  )
  nondpsComps 
}

dps <- c("Genji","McCree","Pharah",
         "Reaper","Soldier 76","Sombra","Tracer",
         "Bastion","Hanzo","Junkrat","Symmetra",
         "Mei","Torbjorn","Widowmaker")

dpsComps2 <- data.frame(combinations(n = length(dps), r = 2,
                                     repeats.allowed = FALSE,
                                     v = dps))
dpsComps3 <- data.frame(combinations(n = length(dps), r = 3,
                                     repeats.allowed = FALSE,
                                     v = dps))
dpsComps4 <- data.frame(combinations(n = length(dps), r = 4,
                                     repeats.allowed = FALSE,
                                     v = dps))

fulldpsComps <- c(paste0(dpsComps2$X1,"/",dpsComps2$X2),
                  paste0(dpsComps3$X1,"/",dpsComps3$X2,"/",
                         dpsComps3$X3),
                  paste0(dpsComps4$X1,"/",dpsComps4$X2,"/",
                         dpsComps4$X3,"/",dpsComps4$X4))

fulldpsComps <- data.frame(
  "dpsComp" = fulldpsComps,
  "dpsCount" = c(rep(2, nrow(dpsComps2)),
                 rep(3, nrow(dpsComps3)),
                 rep(4, nrow(dpsComps4))),
  stringsAsFactors = FALSE
)

AllCompsTable <- function(){
  
  deepsComp <- character()
  nondeepsComp <- character()
  deepsCount <- numeric()
  deepsNum <- 2:4
  for(x in deepsNum){
    
    if(x==2){
      dc <- fulldpsComps$dpsComp[fulldpsComps$dpsCount==x]
      hc1 <- nondpsComps$healerComps[nondpsComps$healerCount==2 &
                                     nondpsComps$tankCount==2]

      tc1 <- nondpsComps$tankComps[nondpsComps$tankCount==1 &
                                       nondpsComps$healerCount==2]

      ndc1 <- paste(hc1, "|", tc2)
      ndc2 <- paste(hc2, "|", tc1)
      
      NDC <- c(ndc1, ndc2)
      ldc <- length(dc)
      
      dc <- rep(dc, length(NDC))
      NDC <- rep(NDC, ldc)
      
      deepsComp <- c(deepsComp, dc)
      nondeepsComp <- c(nondeepsComp, NDC)
      deepsCount <- c(deepsCount, rep(deepsNum, length(dc)))
      
    }
    else if(x==1){
    dc <- fulldpsComps$dpsComp[fulldpsComps$dpsCount==deepsNum[x]]
    hc <- nondpsComps$healerComps[nondpsComps$tankCount+nondpsComps$healerCount==2]
    tc <- nondpsComps$tankComps[nondpsComps$tankCount+nondpsComps$healerCount==2]
    
    dc <- rep(dc, length(hc))
    ndc <- paste(hc, "/", tc)
    ndc <- rep(ndc, length(dc))
    
    deepsComp <- c(deepsComp, dc)
    nondeepsComp <- c(nondeepsComp, ndc)
    deepsCount <- c(deepsCount, rep(deepsNum, length(dc)))
    }
    else{
      dc <- fulldpsComps$dpsComp[fulldpsComps$dpsCount==deepsNum[x]]
      hc <- nondpsComps$healerComps[nondpsComps$tankCount+nondpsComps$healerCount==2]
      tc <- nondpsComps$tankComps[nondpsComps$tankCount+nondpsComps$healerCount==2]
      
      dc <- rep(dc, length(hc))
      ndc <- paste(hc, "/", tc)
      ndc <- rep(ndc, length(dc))
      
      deepsComp <- c(deepsComp, dc)
      nondeepsComp <- c(nondeepsComp, ndc)
      deepsCount <- c(deepsCount, rep(deepsNum, length(dc)))
    }
    
  }
  t <- data.frame(
    "dpsComp" = deepsComp,
    "nondpsComp" = nondeepsComp,
    "dpsCount" = deepsCount,
    stringsAsFactors = F
  )
  
}




