rm(list = ls())


##### create game geographic grid
map = vector(mode = "list", 22)
for(i in 1:22){
  map[[i]] = vector(mode = "list", 39)
  for(j in 1:39){
    map[[i]][[j]] = c(41.9957 - (i-3)*(41.978139 - 39.722766)/(7.29*3)*(22/20),41.9957 - (i-2)*(41.978139 - 39.722766)/(7.29*3)*(22/20),80.5194 - (j-1)*(80.518922-75.773825)/(11.63*3)*(22/20),80.5194 - j*(80.518922-75.773825)/(11.63*3)*(22/20),0,0,0)
  }
}

##### clean data
counties = read.csv("C:/Users/Untellable/Documents/School Stuff/2017-2018 1st Semester/Gerrymandering/county list.csv",header = FALSE,stringsAsFactors = FALSE)
counties = counties$V1
for(i in 1:(length(counties))){
  counties[[i]] = toupper(substr(counties[[i]],0,nchar(counties[[i]])-7))
}

for(i in 1:67){ ##### runs seperately for each county
mydata = read.table(paste("C:/Users/Untellable/Documents/School Stuff/2017-2018 1st Semester/Gerrymandering/Voter Export/Data/",counties[[i]], " FVE 20171113.txt", sep = ""),fill=TRUE,stringsAsFactors = FALSE)
zip = mydata$V20
vote = mydata$V12
shortdata = data.frame(zip,vote)
shortdata = shortdata[which(shortdata$zip != ""),]
shortdata$zip = substr(shortdata$zip,0,5)

zipData = read.table("C:/Users/Untellable/Documents/School Stuff/2017-2018 1st Semester/Gerrymandering/RStudio/US Zip Codes from 2013 Government Data",header = TRUE, sep = ",")
zipData = zipData[which(zipData$ZIP %in% shortdata$zip),]
for(i in 1:(length(zipData$ZIP))){
  zipData$ZIP[[i]] = toString(zipData$ZIP[[i]])
}

zipData$LNG = -zipData$LNG

un = sort(unique(zipData$ZIP)) ##### counting voters
dems = vector(mode = "list",length(un))
reps = vector(mode = "list",length(un))
pop = vector(mode = "list", length(un))
for(i in 1:(length(un))){
  dems[[i]] = length(unlist(shortdata[which(shortdata$zip == un[[i]] & shortdata$vote == "D"),]))/2
  reps[[i]] = length(unlist(shortdata[which(shortdata$zip == un[[i]] & shortdata$vote == "R"),]))/2
  pop[[i]] = (length(unlist(shortdata[which(shortdata$zip == un[[i]]),]))/2)
}


zipData$dems = dems
zipData$reps = reps
zipData$pop = pop

for(i in 1:22){ ##### splitting data into grid
  for(j in 1:39){
    box = map[[i]][[j]]
    map[[i]][[j]][[5]] = map[[i]][[j]][[5]] + Reduce("+",c(zipData$dems[which(zipData$LAT < box[[1]] & zipData$LAT > box[[2]] & zipData$LNG < box[[3]] & zipData$LNG > box[[4]])]),0)
    map[[i]][[j]][[6]] = map[[i]][[j]][[6]] + Reduce("+",c(zipData$reps[which(zipData$LAT < box[[1]] & zipData$LAT > box[[2]] & zipData$LNG < box[[3]] & zipData$LNG > box[[4]])]),0)
    map[[i]][[j]][[7]] = map[[i]][[j]][[7]] + Reduce("+",c(zipData$pop[which(zipData$LAT < box[[1]] & zipData$LAT > box[[2]] & zipData$LNG < box[[3]] & zipData$LNG > box[[4]])]),0)
  }
}
}

for(i in 1:22){##### diagnostic output
  line = ""
  for(j in 1:39){
    line = paste(line,map[[i]][[j]][[5]],sep='\t')
  }
  cat(line,"\n")
}

smoothed = vector(mode="list",22) ##### smoothing and shaping data

for(i in 1:22){
  smoothed[[i]] = vector(mode="list",39)
  for(j in 1:39){
    smoothed[[i]][[j]] = c(0,0,0)
  }
}
for(i in 2:21){
  for(j in 2:38){
    smoothed[[i]][[j]] = c((map[[i]][[j]][[5]] + map[[i+1]][[j]][[5]]+map[[i-1]][[j]][[5]]+map[[i]][[j+1]][[5]]+map[[i]][[j-1]][[5]])/5,(map[[i]][[j]][[6]] + map[[i+1]][[j]][[6]]+map[[i-1]][[j]][[6]]+map[[i]][[j+1]][[6]]+map[[i]][[j-1]][[6]])/5,(map[[i]][[j]][[7]] + map[[i+1]][[j]][[7]]+map[[i-1]][[j]][[7]]+map[[i]][[j+1]][[7]]+map[[i]][[j-1]][[7]])/5)
  }
}
for(j in 2:36){
  smoothed[[22]][[j]] = c((map[[22]][[j]][[5]]+map[[21]][[j]][[5]]+map[[22]][[j+1]][[5]]+map[[22]][[j-1]][[5]])/4,(map[[22]][[j]][[6]]+map[[21]][[j]][[6]]+map[[22]][[j+1]][[6]]+map[[22]][[j-1]][[6]])/4,(map[[22]][[j]][[7]]+map[[21]][[j]][[7]]+map[[22]][[j+1]][[7]]+map[[22]][[j-1]][[7]])/4)
}
j=1
for(i in 3:21){
  smoothed[[i]][[1]] = c((map[[i]][[j]][[5]] + map[[i+1]][[j]][[5]]+map[[i-1]][[j]][[5]]+map[[i]][[j+1]][[5]])/4,(map[[i]][[j]][[6]] + map[[i+1]][[j]][[6]]+map[[i-1]][[j]][[6]]+map[[i]][[j+1]][[6]])/4,(map[[i]][[j]][[7]] + map[[i+1]][[j]][[7]]+map[[i-1]][[j]][[7]]+map[[i]][[j+1]][[7]])/4)
}
i = 1
for(j in 3:6){
  smoothed[[1]][[j]] = c((map[[i]][[j]][[5]] + map[[i+1]][[j]][[5]]+map[[i]][[j+1]][[5]]+map[[i]][[j-1]][[5]])/4,(map[[i]][[j]][[6]] + map[[i+1]][[j]][[6]]+map[[i]][[j+1]][[6]]+map[[i]][[j-1]][[6]])/4,(map[[i]][[j]][[7]] + map[[i+1]][[j]][[7]]+map[[i]][[j+1]][[7]]+map[[i]][[j-1]][[7]])/4)
}
for(i in 1:22){
  smoothed[[i]][[39]] = c(map[[i]][[39]][[5]],map[[i]][[39]][[6]],map[[i]][[39]][[7]])
}
smoothed[[22]][[1]] = c(map[[22]][[1]][[5]],map[[22]][[1]][[6]],map[[22]][[1]][[7]])


for(j in 7:39){
  smoothed[[2]][[j]] = c(0,0,0)
}
for(j in 37:39){
  smoothed[[3]][[j]] = c(0,0,0)
}
for(i in 4:6){
  for(j in 38:39){
    smoothed[[i]][[j]] = c(0,0,0)
      }
}
for(j in 34:39){
  smoothed[[22]][[j]] = c(0,0,0)
}
for(i in 10:22){
  for(j in 36:39){
    if(map[[i]][[j]][[7]] == 0){ smoothed[[i]][[j]] = c(0,0,0)}
  }
}
smoothed[[12]][[39]] = c(0,0,0)
smoothed[[4]][[37]] = c(0,0,0)
smoothed[[3]][[36]] = c(0,0,0)


smoothed_store = smoothed
smoothed = smoothed_store

#--------------------MAP 1----------------------------------------------------------

totalPop = 0
totalDem = 0
totalRep = 0
for(i in 1:22){
  for(j in 1:39){
    totalPop = totalPop + smoothed[[i]][[j]][[3]]
    totalDem = totalDem + smoothed[[i]][[j]][[1]]
    totalRep = totalRep + smoothed[[i]][[j]][[2]]
  }
}

popMult = 12780000/totalPop ##### accounting for missing and non-registered voters


lineD = "" ##### diagnostic
lineR = ""
lineU = ""
xml = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><gameBoard>" ##### converting data into xml format
for(i in 1:22){
  for(j in 1:39){
    d = smoothed[[i]][[j]][[1]]
    r = smoothed[[i]][[j]][[2]]
    pop = smoothed[[i]][[j]][[3]]
    pDem = 0
    pRep = 0
    if(pop > 0) {
      pDem = 100*d/pop
      pRep = 100*r/pop
      extra = (100 - pDem - pRep)*(3/4)
      pd = pDem + (pDem/(pDem + pRep))*extra ##### calculating party percentages
      pr = pRep + (pRep/(pDem + pRep))*extra
      pDem = pd
      pRep = pr
      diff = pDem - pRep
      pDem = round(pDem + diff*.25) ##### artificially inflating party differences
      pRep = round(pRep - diff*.25)
    }
    lineD = paste(lineD,round(pDem*pop*popMult/100),sep = '\t')
    lineR = paste(lineR,round(pRep*pop*popMult/100),sep = '\t')
    lineU = paste(lineU,round((100-pDem-pRep)*pop*popMult/100),sep = '\t')
    xml = paste(xml, "<gamePiece xCoord=\"",round(j-1),"\" yCoord=\"",round(i-1),"\"><initDistrict>",sep = "")
    if(pop == 0){
      xml = paste(xml,"0</initDistrict><repHome>0</repHome>",sep = "")
    }
    else{
      if(i < 16){
        if(j < 24){
          if(j == 10 & i == 7){
            xml = paste(xml,"1</initDistrict><repHome>1</repHome>",sep = "")
          }
          else{
            xml = paste(xml,"1</initDistrict><repHome>0</repHome>",sep = "")
          }
        }
        else{
          if(j == 29 & i == 8){
            xml = paste(xml,"3</initDistrict><repHome>3</repHome>",sep = "")
          }
          else{
            xml = paste(xml,"3</initDistrict><repHome>0</repHome>",sep = "")
          }
        }
      }
      else{
        if(j < 4){
          xml = paste(xml,"1</initDistrict><repHome>0</repHome>",sep = "")
        }
        else if(j < 25){
          if(j == 12 & i == 21){
            xml = paste(xml,"2</initDistrict><repHome>2</repHome>",sep = "")
          }
          else{
            xml = paste(xml,"2</initDistrict><repHome>0</repHome>",sep = "")
          }
        }
        else{
          if(j == 29 & i == 19){
            xml = paste(xml,"4</initDistrict><repHome>4</repHome>",sep = "")
          }
          else{
            xml = paste(xml,"4</initDistrict><repHome>0</repHome>",sep = "")
          }
        }
      }
    }
    xml = paste(xml, "<population>",round(pop*popMult),"</population><republican>",pRep,"</republican><democrat>",pDem,"</democrat><caucasian>0</caucasian><africanAmerican>0</africanAmerican><hispanic>0</hispanic></gamePiece>", sep = "")
  }
  lineD = paste(lineD,'\n',sep = "")
  lineR = paste(lineR,'\n',sep = "")
  lineU = paste(lineU,'\n',sep = "")
}
xml = paste(xml, "</gameBoard>",sep = "")

write(xml, file = "C:/Users/Untellable/Documents/School Stuff/2017-2018 1st Semester/Gerrymandering/mission1.txt")

#cat(lineU)

#-----------------------MAP 5--------------------------------------------------------------
text = scan("C:/Users/Untellable/Downloads/map.txt",what="",sep = "~")
text = text[[1]]

dist = strsplit(text,"initDistrict>")
dist = dist[[1]]
dist = dist[which(substr(dist,1,1) != "<")]
dist = substr(dist,1,1)


home = strsplit(text,"repHome>")
home = home[[1]]
home = home[which(substr(home,1,1) != "<")]
home = substr(home,1,1)
which(home != "0")

totalPop = 0
totalDem = 0
totalRep = 0
for(i in 1:22){
  for(j in 1:39){
    totalPop = totalPop + smoothed[[i]][[j]][[3]]
    totalDem = totalDem + smoothed[[i]][[j]][[1]]
    totalRep = totalRep + smoothed[[i]][[j]][[2]]
  }
}

popMult = 12780000/totalPop

xml = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><gameBoard>"
line = ""
for(i in 1:22){
  for(j in 1:39){
    d = smoothed[[i]][[j]][[1]]
    r = smoothed[[i]][[j]][[2]]
    pop = smoothed[[i]][[j]][[3]]
    pDem = 0
    pRep = 0
    if(pop > 0) {
      pDem = 100*d/pop
      pRep = 100*r/pop
      extra = (100 - pDem - pRep)*(3/4)
      pd = pDem + (pDem/(pDem + pRep))*extra
      pr = pRep + (pRep/(pDem + pRep))*extra
      pDem = pd
      pRep = pr
      diff = pDem - pRep
      pDem = round(pDem + diff*.25)
      pRep = round(pRep - diff*.25)
    }
    xml = paste(xml, "<gamePiece xCoord=\"",round(j-1),"\" yCoord=\"",round(i-1),"\"><initDistrict>",sep = "")
    if(pop == 0){
      xml = paste(xml,"0</initDistrict><repHome>0</repHome>",sep = "")
      line = paste(line,0,sep='\t')
    }
    else{
      xml = paste(xml,dist[[j+1+(i+7)*40]],"</initDistrict><repHome>",home[[j+1+(i+7)*40]],"</repHome>",sep = "")
      line = paste(line,dist[[j+1+(i+7)*40]],sep='\t')
    }
    xml = paste(xml, "<population>",round(pop*popMult),"</population><republican>",pRep,"</republican><democrat>",pDem,"</democrat><caucasian>0</caucasian><africanAmerican>0</africanAmerican><hispanic>0</hispanic></gamePiece>", sep = "")
  }
  line = paste(line,'\n',sep="")
}
xml = paste(xml, "</gameBoard>",sep = "")

write(xml, file = "C:/Users/Untellable/Documents/School Stuff/2017-2018 1st Semester/Gerrymandering/mission5.txt")
cat(line)

#-----------------------MAP 2&3------------------------------------------------------------

for(i in 1:22){
  for(j in 1:39){
    smoothed[[i]][[j]][[1]] = smoothed[[i]][[j]][[1]]^(3/4) ##### artificially flattening population differences (moving people to rural areas)
    smoothed[[i]][[j]][[2]] = smoothed[[i]][[j]][[2]]^(3/4)
    smoothed[[i]][[j]][[3]] = (smoothed[[i]][[j]][[3]] - smoothed[[i]][[j]][[1]] - smoothed[[i]][[j]][[2]])^(3/4) + smoothed[[i]][[j]][[1]] + smoothed[[i]][[j]][[2]]
  }
}


totalPop = 0
totalDem = 0
totalRep = 0
for(i in 1:22){
  for(j in 1:39){
    totalPop = totalPop + smoothed[[i]][[j]][[3]]
    totalDem = totalDem + smoothed[[i]][[j]][[1]]
    totalRep = totalRep + smoothed[[i]][[j]][[2]]
  }
}

popMult = 12780000/totalPop

lineD = ""
lineR = ""
lineU = ""
xml = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><gameBoard>"
for(i in 1:22){
  for(j in 1:39){
    d = smoothed[[i]][[j]][[1]]
    r = smoothed[[i]][[j]][[2]]
    pop = smoothed[[i]][[j]][[3]]
    pDem = 0
    pRep = 0
    if(pop > 0) {
      pDem = 100*d/pop
      pRep = 100*r/pop
      extra = (100 - pDem - pRep)*(15/16)
      pd = pDem + (pDem/(pDem + pRep))*extra
      pr = pRep + (pRep/(pDem + pRep))*extra
      pDem = pd
      pRep = pr
      diff = pDem - pRep
      pDem = round(pDem + diff*.25)
      pRep = round(pRep - diff*.25)
    }
    lineD = paste(lineD,round(pDem*pop*popMult/100),sep = '\t')
    lineR = paste(lineR,round(pRep*pop*popMult/100),sep = '\t')
    lineU = paste(lineU,round((100-pDem-pRep)*pop*popMult/100),sep = '\t')
    xml = paste(xml, "<gamePiece xCoord=\"",round(j-1),"\" yCoord=\"",round(i-1),"\"><initDistrict>",sep = "")
    if(pop == 0){
      xml = paste(xml,"0</initDistrict><repHome>0</repHome>",sep = "")
    }
    else{
      if(i > 11 & j < 16){
        if(j == 11 & i == 17){
          xml = paste(xml,"2</initDistrict><repHome>2</repHome>",sep = "")
        }
        else{
          xml = paste(xml,"2</initDistrict><repHome>0</repHome>",sep = "")
        }
      }
      else if((i > 17 & j > 33) | ((i == 18 | i == 19) & j == 33)){
        if(j == 36 & i == 18){
          xml = paste(xml,"4</initDistrict><repHome>4</repHome>",sep = "")
        }
        else{
          xml = paste(xml,"4</initDistrict><repHome>0</repHome>",sep = "")
        }
      }
      else if(i > 12 & j > 24){
        if(j == 26 & i == 18){
          xml = paste(xml,"3</initDistrict><repHome>3</repHome>",sep = "")
        }
        else{
          xml = paste(xml,"3</initDistrict><repHome>0</repHome>",sep = "")
        }
      }
      else{
        if(j == 8 & i == 8){
          xml = paste(xml,"1</initDistrict><repHome>1</repHome>",sep = "")
        }
        else{
          xml = paste(xml,"1</initDistrict><repHome>0</repHome>",sep = "")
        }
      }
    }
    xml = paste(xml, "<population>",round(pop*popMult),"</population><republican>",pRep,"</republican><democrat>",pDem,"</democrat><caucasian>0</caucasian><africanAmerican>0</africanAmerican><hispanic>0</hispanic></gamePiece>", sep = "")
  }
  lineD = paste(lineD,'\n',sep = "")
  lineR = paste(lineR,'\n',sep = "")
  lineU = paste(lineU,'\n',sep = "")
}
xml = paste(xml, "</gameBoard>",sep = "")

write(xml, file = "C:/Users/Untellable/Documents/School Stuff/2017-2018 1st Semester/Gerrymandering/mission2&3.txt")

#cat(lineU)
