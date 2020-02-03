tes <- read.csv("~/tes.csv")
packages <- c("dplyr", "stringr", "MCMCglmm")
lapply(packages, library, character.only = TRUE)


#all of this is super memory inefficent and will need to be fixed with 
#larger data probably

#a function that labels each element
#currently only labels jumps, add extra cases in later
label.function <- function(df){
  df <- df %>%
      mutate(`label` = case_when(str_detect(elements, "Sp") ~ "spin",
                                 str_detect(elements, "St") ~"step",
                                 str_detect(elements, "Ch") ~"choero",
                                 TRUE ~ "jump")
      )
  df <- df %>%
    mutate(`type of element` = case_when(
      str_detect(elements, "A") & label =="jump"  ~ "axel",
      str_detect(elements, "T") & label =="jump"  ~ "toeloop",
      str_detect(elements, "S") & label =="jump"  ~ "salcow",
      str_detect(elements, "Lo") & label =="jump"  ~ "loop",
      str_detect(elements, "F") & label =="jump"  ~ "flip",
      str_detect(elements, "Lz") & label =="jump"  ~ "lutz",
      
      )
)
}
  

# a function to get the asoects of each jump
#falls still needs work
#to save memory can re-write so only runs the jumps?
jumps.function <- function(df){
  #for each skater make a dataframe of all jumps attempted in all competiions

  
  #catch +SEQ and +COMBO
  #if a jump is labled as this, prevent program from thinking its a jump
  df <- df %>% 
    mutate(`+seq` = ifelse(str_detect(elements,"\\+SEQ"),1,0)) 
    #should I remane these to make combo names clearer???
  df$elements <- str_replace(df$elements, "\\+SEQ", "")
  df$elements <- str_replace(df$elements, "\\+COMBO", "")
  
  df$numjudges <- rowSums(select(df,minusthree:plusthree))
  df <-df %>%
    rowwise()%>%
    mutate(avgGOE = (minusthree*(-3)+ minustwo*(-2)+minusone*(-1)+
                       plusone + plustwo*2 +plusthree*3)/numjudges)
  
  df$combo <- str_count(df$elements, "\\+")
  combos<-df[rep(seq_len(nrow(df)), (df$combo+1)),]
  #now for second jump in combo
  combos$jumpscount <- rownames(combos)
  combos$elements <- as.character(combos$elements)
  combos<- combos %>% 
     mutate(`2ndcombo` = apply(combos,1, function(x){
      x[['2ndcombo']] = as.numeric(unlist(strsplit(x[['jumpscount']],"\\.")[[1]][2]))+1
    }))
  #as these are second jumjps not the first one, remove from first combo jump coloumn
  combos <- combos %>% mutate(`2ndcombo`= ifelse(is.na(`2ndcombo`), 0, `2ndcombo`))
  combos <- combos %>% mutate(combo=ifelse(`2ndcombo`=="2", 0, combo))

  #for thrid jump in combo, now tidy up the rest
  combos <- combos %>% 
    mutate(`3rdcombo`=ifelse(`2ndcombo` == 3,1,0)) %>%
    mutate(combo=ifelse(`2ndcombo`==3, 0, combo))
  #and fix the notation so its the same as everything else
  combos<- combos%>% mutate(`2ndcombo` = ifelse(combos$`2ndcombo` == 2, 1, 0))
  combos <- select(combos, -jumpscount)
  
  #split combo jumps into indivdual jumps
  combos$fullcombo <- combos$elements
  combos <- combos %>%
    mutate(elements = ifelse(combo == 1,str_extract(elements, "(.+)(?=\\+)")
 ,elements))
  combos <- combos %>%
    mutate(elements = ifelse(`2ndcombo` == 1,str_extract(elements, "([^\\+]+$)")
                             ,elements))
  
  #note to self, half loops are picked up as indivdual jump, exclude later
  #also change to "Eu" under new system
  
  #identify number of rotations
  combos <- combos %>% 
    mutate(numrotations = ifelse(str_detect(elements,"[:digit:][:letter:]"),
                                 str_extract(elements,"[:digit:]"),0))
   combos<- combos %>%
    mutate(numrotations = ifelse(str_detect(elements,"^[:letter:]+$"),
                               1,numrotations)) 

  ######################
   #falls need ironing out
   ########################
  #for a fall the GOE is all -3 (or -5 new system)
  #the number of judges chsnges from each competition, so different number
  
  combos <- combos %>% 
    mutate(falls = ifelse(minusthree >0 & minustwo == 0 & minusone == 0 & zero == 0 &
                            plusone == 0 & plustwo == 0 & plusthree ==0, 1,0))
  #note if fall on combo, only fell on last jump, so be careful
  #also need to add in case when invalid because of lack of combo
  #might need to check 
  
  #edge call 

  return(combos)
}
 
# a function to get the levels on the spin and step sequences
spin.function



df <- jumps.function(tes)
df <- label.function(df)  
#
#how good is a skater at a certain element

skater.skills <- function(df, skatername, element){
  #make a dataframe of each element to get the sd and mean
  #if not combo
  dummydf <- df %>% 
    filter(name == skatername) %>%
    filter (`type of element`==element & `2ndcombo`==0 & `3rdcombo`==0)
 
  #figure out NAs in avgGOE column
  averageGOE <- sum(dummydf[,"avgGOE"])/nrow(dummydf)
  sdGOE <- sd(dummydf$avgGOE)
  #the predicted 
  predictedGOE <- rnorm(1, averageGOE, sdGOE)
  #take into account small amount of data points somehow??????

  #####################
  #base value
  ######################
  #base value depends on how many rotations
  #whether their is a downgrade/under call
  #whether it is in the second half
  #all of these use random uniform greater than rnorm
  
  
  
  #is random distrubution the right distrubution??????
  #to do figure out skaters who are more likely to re-arange their programs
  #figure out if their is a difference between bonus and non-bonus jumps
  #underrate <- sum(dummydf[,"avgGOE"])/nrow(dummydf)
  #downgraderate <- sum(dummydf[,"under"])/nrow(dummydf)
  #calculate error
  # if don't change sccraping code get downgrade from base value
  
   #write values to vector
  #later try adding in factor for first jump in the combination
  
  ####################
  #fall deduction
  ####################
  #calculate for whole program or jump by jump?
  fallrate <- sum(dummydf[,"falls"])/nrow(dummydf)
  fallerror <- ((fallrate*(1-fallrate))/nrow(dummydf))^0.5
  
  
  #edge call work in progress
  #number of rotations is work in progress
  #number.of.rotations

 
#now given the skills of the skater, predict based on averages what they could expect
#to recieve for each element, run simulations to predict the winner

#make a csv dataframe of skaters and planned layout
#input a list of skaters and planned layout  
  
  expected.calls<-function(df, skatername){
    num.jumps <-10  #the number of jumps to simulate 
    # currently this is set to 10 3 in the short, 7 in the long, later compare short 
    # and long to see if there is a difference
    
    #get the values from the dataframe
    #do I need to make better names?????
    sd <-filter(df,name==skatername & rotated == "no.penalty")$error
    mean <- filter(df,name==skatername & rotated == "no.penalty")$propotion
    random.chance <- runif(num.jumps)  
    rotated.chance <- rnorm(num.jumps, mean, sd) #chance skater has at rotating
    under.chance <- filter(df,name==skatername & rotated == "warning")$propotion
    #if the jump isn't fullly rotated this is the chance it is under rortated rather 
    #than a downgrade, don't simulate this, becuase thats introducing two lots of error
    #assume that it is all corrleated, ie the error is pushing it towards more rotated
    #in general or less rotated in general.
    #add them together because they should sum to zero
    
    #this is my current thinking, I'm still not sure about this.
    
    
    #make a tidyverse version of this, don't use table???
    out <- data.frame(result = ifelse(random.chance < rotated.chance,"rotated",
                                      ifelse(random.chance < rotated.chance + under.chance, "under", "downgrade")))
    count(out, result)
    #1 is fully rotated, 2 is under, 3 is downgrade
    
  }
  
  skaters.list <- c('a','b','c') #a list of skaters competing 
  #use the expected calls function to simulate a set of calls for skaters
  func2<-function(skaters.list){bind_rows(m<-lapply(skaters.list,expected.calls, df=df)) %>% 
      group_by(result) %>% summarise(n=sum(n))}
  #suppress warnings somehow?  
  
  
  
  
competition.effects <- function(df,compname){
  #tech caller on each jump
  dummydf <- df %>% 
    filter(competition == compname) %>%
    filter (label=="jump")
  
  #percentage jumps called
  underrotation
  #underrotation on fall???
  downgrade

  
  #given the field how many underrotations downgrades would we expect
  #gather the names of the skaters
  competing.skaters <- unique(df %>% 
    filter(competition == compname) %>%
    select(name))
  #now see how often they get called in other competitions
  
  #put in apply or actual loop????
  skating.skill.function <- function(df,skatername,compname){
    skaterjumps <-df %>%
      filter(name == skatername & label == "jump") %>%
      #look at all other competitions except for this one
      filter(competition != compname)
  
    numjumps <- nrow(skaterjumps)
    #now get the distrubution of fully rotated, underrotated downgrade
    #make a dataframe of 4 columns skater name, percentage fully rotated,
    #percentage under rotated percentage downgrade
    
    
    #what is the chance given these distrubitions that 
  }
  
  #be careful of lack of data, do I need to filter for only certain reuslts
  #model uncertainity
  
  
  
  #now do the same thing except for edge calls
  #exclamation mark gather data?
  
  #next up levels
}
  
  
  #gather data- filter by skater and then the element in question
  
  #df
  #lutz
  
  
}


# this is the main prediction function to predict how well a skater is going to do 
#a certain element
#I want this to be a general function that will accept, indivdual jumps total scores
#spins ect, and use lapply when running  
#eventually will need a dataframe of terms to serach by, and combine with PCS
  # if jump


#if spin
# predict the level
#predict the GOE

  #this only works for single jumps. combo jumps are split first
  jump2 <- lapply(jumps.list,df){
    #how many rotations
    rotation <- df$
    #idendify falls where all goe is -3
    num.falls <- 
    #sucess rate of falls = total jumps-falls/total jump
    sucess.rate <- (nrow(df) - num.falls)/nrow(df)
    
    #outconstruct the element data base
  }
  return(out)
  ###########################
  #next step competition effects
  ############################


  
  
  
  

  $edge <-  str_count(df$elements, 'e')/num.lutz
  out$under <-  str_count(df$elements, '<')/num.jumps
  out$downgrade <- str_count(df$elements, '<<')/num.jumps
  
  #get sd?
  return(out)
}
comp.list <- split(tes, tes$competition)
#get the average scores for each compeition
compavg.list <-lapply(comp.list, elements.function)
  
  df$edge.deduction <- str_count(df$elements, 'e')
  df$under <- str_count(df$elements, '<')

hit.rate <-function(df){
  #jumps first, seperate combinations
  df$combo <- str_count(df$elements, "\\+")
  #jumps that are repeated have to be in combination
  combos<-df[rep(seq_len(nrow(df)), (df$combo+1)),]
  #loop through each row in apply
  vec <- sapply(df$elements,function(x){ unlist(str_split(x,"\\+"))})
  vec <- unlist(vec)
  combos$elements <- vec
  # need to fix for combo jumps?????


  jumps.list <-list(salcow, toeloop,loop,flip,lutz,axel)

  
  

  
  
  
  
  
  
  
  
  avg.downgrade
  df$averageGOE<-sum(avg.vector)
  #put these into a list
  #for indivdual skaters run all types through the function, for comp, just jumps
  dist.function <- function(df){

    #find out the number of judges by summing the first element
    num.judges <-sum(df[1,6:12])
    #now get the distrubution for each element
    df$minusthree <- df$minusthree/num.judges
    df$minustwo <- df$minustwo/num.judges
    df$minusone <- df$minusone/num.judges
    df$zero <- df$zero/num.judges
    df$plusone <- df$plusone/num.judges
    df$plustwo <- df$plustwo/num.judges
    df$plusthree <- df$plusthree/num.judges
    #
  })
  #for comp
  dist.function(df)
  #for indivdual skaters
  lapply(df, dist.function)
  #add to the jumps dataframe
  #add in excalmation?
  df$edge.deduction <- str_count(df$elements, 'e')
  #make this a bit more spefisticated??
  df$invalid <- str_count(df$elements, '\\*')
  df$downgrade <- str_count(df$elements, '<')
  
  
  #now attributes, spin function
  df <- within(combos,{
    level <- substring(elements, nchar(elements))
    rotation <- substr(elements, 1, 1)
  })
  df$rotation <- as.numeric(df$rotation)
  df$level <- as.numeric(df$level)
})

comp.list <- split(tes, tes$competition)
#get the average scores for each compeition
compavg.list <-lapply(comp.list, function(df){
 #get average values, get sd
  df$edge.deduction <- str_count(df$elements, 'e')
  df$under <- str_count(df$elements, '<')
  df
  #split GOE into jumps and spins
  #for jump GOE, 
  
  spinGOE
  #add in spin levels later?
  stepGOE
  choeroGOE 
})
#unlist to a df where each row is a competion
#do the same thing for a skaters

#now, using the comp list calculate the difference from the skaters average to the comp
compeffects <- 
  skateravg$thing[skateravg$name == compavg$name]


#split by skater
skaters.list <- split(tes, tes$name)



#test to see if spectra or means are the same-
#split data into one year chunks for each elemtent
#compare all chunks together, mean and spectrum...
#page448
#do it in terms of elements, take the points earned of all elements and compare
#means of alll different elements 


#set them up so everyone has the same 

znorm(timeseries, )



comp <- data.frame(name = rep(c('a','b','c','d'),c(10,20,30,40)),
                   no.penalty = rep_len(c(0,0,1),100),
                   warning = rep_len(c(1,0,0),100),
                   penalty = rep_len(c(0,0,1),100))


list1 <- split(comp, f= comp$name)
skatersamples <- lapply(list1, function(df){
  df <- df[sample(nrow(df), 1000, replace = TRUE),]
  return(df)
})
ss<-bind_rows(skatersamples)



#select by number of jumps attempted, pass into a loop?????????

#make this into a function which samples from each skater and then combines
boot.fun <- function(df,resample){
  list1 <- split(comp, f= comp$name)

  
  colSums(df[resample,2:4]) #the number of samples, fix this
}
results<- boot(ss,boot.fun,R=1000)
boot.ci(results, type = "bca",index = 1) #fully rotated
boot.ci(results, index = 2)#under
boot.ci(results, index = 3) #downgrade

boot_mean <- function(original_vector, resample_vector) {
  mean(original_vector[resample_vector,1])
}

mean_results <- boot(x, boot_mean, R = 20)
x<-as.data.frame(x)


c$compID <-1
v<- comp%>% group_by(name) %>% 
  summarise(no.penalty=mean(no.penalty), warning = mean(warning), 
            penalty = mean(penalty),n=n()) %>%
  gather(key=rotated,value =propotion ,-c(name,n))
v$error <-((v$propotion*(1-v$propotion))/v$n)^0.5

#random number is bigger a skaters chance of landing a jump then landed, 
#if not not underrotatiion
#the chance of landing a jump is on the normal distrubtion, because it is unknown

#be careful because the uniform has min max, which norm doesn't, but this is okay 
#because the random is simulating the portion which should be between 0-1




#this calculates one set of expected calls for one skater
expected.calls<-function(df, skatername){
  num.jumps <-10  #the number of jumps to simulate 
  # currently this is set to 10 3 in the short, 7 in the long, later compare short 
  # and long to see if there is a difference
  
  #get the values from the dataframe
  #do I need to make better names?????
  sd <-filter(df,name==skatername & rotated == "no.penalty")$error
  mean <- filter(df,name==skatername & rotated == "no.penalty")$propotion
  random.chance <- runif(num.jumps)  
  rotated.chance <- rnorm(num.jumps, mean, sd) #chance skater has at rotating
  under.chance <- filter(df,name==skatername & rotated == "warning")$propotion
  #if the jump isn't fullly rotated this is the chance it is under rortated rather 
  #than a downgrade, don't simulate this, becuase thats introducing two lots of error
  #assume that it is all corrleated, ie the error is pushing it towards more rotated
  #in general or less rotated in general.
  #add them together because they should sum to zero
  
  #this is my current thinking, I'm still not sure about this.
  
  
  #make a tidyverse version of this, don't use table???
  out <- data.frame(result = ifelse(random.chance < rotated.chance,"rotated",
  ifelse(random.chance < rotated.chance + under.chance, "under", "downgrade")))
  count(out, result)
  #1 is fully rotated, 2 is under, 3 is downgrade
  
}

skaters.list <- c('a','b','c') #a list of skaters competing 
#use the expected calls function to simulate a set of calls for skaters
func2<-function(skaters.list){bind_rows(m<-lapply(skaters.list,expected.calls, df=df)) %>% 
                       group_by(result) %>% summarise(n=sum(n))}
  #suppress warnings somehow?

#now create multiple simulations to see the average result
#can change the number of simulations later if needed
comp.sims <-bind_rows(replicate(1000, func2(skaters.list), simplify = FALSE))  
x<-comp.sims %>% 
  group_by(result) %>%
  summarise(mean = mean(n), sd = sd(n)) 
#histogram of all the outcomes
hist(filter(comp.sims, result == "rotated")$n,
     main="expected number of jumps judged as fully rotated",
     xlab = "number of jumps") #rotated
hist(filter(comp.sims, result == "under")$n,
     main="expected number of jumps judged as underrotated",
     xlab = "number of jumps") #rotated
hist(filter(comp.sims, result == "downgrade")$n,
     main="expected number of jumps judged as downgrade",
     xlab = "number of jumps") #rotated


#plot of the mean with sd error bars
qplot(x= result, y=mean, data=x)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.25,data=x)
#plot indivdual competitions against expected simulations ect


#to do 
#write function comparing selected competion to historical data
#highlight the speficfed competion in the plots
#do a t test to see if statistically signifcant difference


#calculate competition effects (h test?)
#difference from the mean---how do I encorporated uncertainity in this



#shaprio test to see the effects of competition holding skaters constant 
#to get the odds ratio

#estimate how likely it is that points lie on the distrubition
#we use the log to avoid math problems
loglik <- function(par, y) {
  sum(dnorm(y, par[1], sqrt(par[2]), log = TRUE)) }

#this finds the best distrubution (ie what the mean and varience is)
#it is finding the lowest because we mulipled by -1, because R functions
 MLest <- optim(c(mean = 0, var = 1), fn = loglik,
                 y = Ndata$y, control = list(fnscale = -1,
                  reltol = 1e-16))$par

#probablity of the prior, this is the same as the above equation basically,
#here we are trying to find the distrubution which our priors fit on
#R is varience prior, B is mean prior
logprior <- function(par, priorR, priorB) {
 dnorm(par[1], mean = priorB$mu, sd = sqrt(priorB$V),
       log = TRUE) + log(dinvgamma(par[2], shape = priorR$nu/2,
                                   scale = (priorR$nu * priorR$V)/2))

}

prior <- list(R = list(V = 1, nu = 0.002), B = list(mu = 0, V = 1e+08))

#use this code to get individual results, eg whether an indivdual skater in a 
#specific competition was more or less likely
#to do this speficfy pl=TRUE when running mcmcglmm
> lat92 <- m2a.5$Liab[, 92]
> eta92 <- m2a.5$Sol[, "(Intercept)"] + m2a.5$Sol[,
                                                  +     "day"] * Traffic$day[92]
resid92 <- lat92 - eta92
mean(resid92)
#this is the chance it would be more than expected page 37
1 - ppois(exp(mean(eta92)), exp(mean(lat92)))


prior <- list(R = list(V = 1, nu = 0.002))



#need to exclude automatic minus threes or eg falls, combo from anylisis
newdf <- df %>% group_by(name) %>% filter(n()>100)
newdf <- newdf %>% group_by(competition) %>% filter(n()>100) %>% ungroup()

newnewdf <- newdf %>% group_by(competition) %>% filter(n()>150) %>% ungroup()
length(unique(newdf$name))

              
                 
#need G term in priors for random effects


prior = list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1,nu = 0.002),
                    G2 = list(V = 1,nu = 0.002), G3 = list(V = 1,nu = 0.002), 
                    G4 = list(V = 1,nu = 0.002), G5 = list(V = 1,nu = 0.002)))
             
newdf <-label.function(newdf)
testdf <- filter(newdf, label=="jump")
test3 <- MCMCglmm(numrotations ~ 1, random = ~name + combo +  +
                   `2ndcombo` + `3rdcombo`,
                 data= testdf, prior=prior, pr=TRUE)


prior = list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1,
               nu = 0.002)))

prior = list(R = list(V = diag(1), nu=0.002), G = list(G1 = list(V = diag(27),
                                                                 nu = 0.002)))






#use this to predict PCS
#I still don't know how putting the ~ before things works, figure this out
test <- MCMCglmm(avgGOE ~ 1, random = ~competition +name,
                 data= newdf, prior=prior, pr=TRUE)

summary(test)
plot(test$Sol) #sol is mean left is like time series, right is sort of historgram
plot(test$VCV) #VCV is varience

#this seems to be better, as some compeitions are dodgyb buy some look fine
test2 <- MCMCglmm(avgGOE ~ 1, random = ~idh(competition):name,
                 data= newdf, prior=prior)

###################################
#there is a major problem need to treat both name and compeition as catorgorical varibles
diag(autocorr(test$VCV)[2, , ]) #varience with compeition wayyy tp high
#is this because I have way more compeitions than skaters????
#not sampling enough from compeitions???
effectiveSize(test$VCV)
#need to fix this somehow?
#157 skater, only 27 comp?????
cor(test$VCV)
HPDinterval(test$VCV)

z<-predict(test, marginal= ~name, type = "response", interval = "confidence")
skater.predictions <-unique(data.frame(z,newdf$name ))
z<-predict(test, marginal= ~competition, type = "response", interval = "confidence")
comp.predictions <- unique(data.frame(z,newdf$competition))

#interval=conifidence tells us about the past data, 
#inerval= prediction is the prediction inverval

#need to think of a more sophifsitcated solution for jump
#whether jump is sucessful of not,



ordinal.prediction.function <-function(df){
  df <- df %>%
    mutate(proportion = prop.table(n), error = (proportion*(1-proportion)/n)^0.5,
           cumFreq= cumsum(n),cumProportion = cumsum(proportion), 
           cumError =(cumProportion*(1-cumProportion)/n)^0.5) 
  df[is.na(df)] <-0
  #add for infinity????
  
  random.chance <- runif(num.sim)  # is this the right distrubution??????
  #the chance of getting the highest ordinal
  bestchance <- rnorm(num.sim, df$cumProportion[1],df$cumError[1])
  percentiles <- pnorm(bestchance)
  #now get the chance of all the other levels
  chances.df <- apply(df, 1, function(df){
    qnorm(percentiles,as.numeric(df['cumProportion']),as.numeric(df['cumError']))
  })
  #this basically says take a random number, if level4 sim bigger level4,
  #if not, what is the proportion they get level 3 or 4? if bigger then level 3
  #if not what is the proportion they get level 2 or above, if bigger level 2 ect
  chances.df[chances.df>random.chance] <-1
  #finds the pattern, number of 1s mean definetly bigger 
  ordinals <- apply(chances.df, 1, function(x) {
    r <- rle(x)
    max(r$lengths[as.logical(r$values)])
  })
  #now give them their name
  #have to flip the order of the names
  out <- rev(df$names)[ordinals]
  return(out)
}
