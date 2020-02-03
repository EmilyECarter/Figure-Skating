######ideas- juniors look at age
###### juniors look at unknown skaters PCS
#####juniors skate order SP to LP
###### juniors look at tech content influence again SP to LP-dance as well






#formating function
#add extra data like what kind of element, num rotations and level

#for everything except ice dance, add in spin labels and pair stuff, not quiet finished
#also might change later and remove spin flags and stuff fi it gets too big
label.function <- function(df){
  df <- df %>%
    mutate(`label` = case_when(str_detect(aspect_desc, "Sp") ~ "spin",
                               str_detect(aspect_desc, "St") ~"step",
                               str_detect(aspect_desc, "Ch") ~"choero",
                               str_detect(aspect_desc, "Ds") ~"death spiral",
                               str_detect(aspect_desc, "PSp") ~"pair spin",
                               str_detect(aspect_desc, "PCo") ~"pair combo spin",
                               str_detect(aspect_desc, "Li") ~"lift",
                               str_detect(aspect_desc, "Tw") ~"twist",
                               TRUE ~ "jump")
    )
  
  #this probably isn't nessisry for jumps as I do it later
  df <- df %>%
    mutate(label = if_else(section == "components", "NA",label))
  df <- df %>%
    mutate(`type of element` = case_when(
      str_detect(aspect_desc, "A") & label =="jump"  ~ "axel",
      str_detect(aspect_desc, "T") & label =="jump"  ~ "toeloop",
      str_detect(aspect_desc, "S") & label =="jump"  ~ "salcow",
      str_detect(aspect_desc, "Lo") & label =="jump"  ~ "loop",
      str_detect(aspect_desc, "F") & label =="jump"  ~ "flip",
      str_detect(aspect_desc, "Lz") & label =="jump"  ~ "lutz",
      
    ))
  #now get the levels for everything except the jump/cheoro sequence
  df$aspect_desc <- as.character(df$aspect_desc)
  df <- df %>%
    mutate(`level`= if_else(
      !(label %in% c("jump", "choero")) & section == "elements",
        substr(aspect_desc, nchar(aspect_desc), nchar(aspect_desc)),"NA"))
  #now relabel throw jumps as throw jumps
  
  
  #mark if a spin is given a V
  #change the marker later?????
  df <- df %>% 
    mutate(`spin flag`= case_when(
      str_detect(aspect_desc,"V") & label == "spin" ~ "V"))
  
  
  #group of lifts
  #label death spiral inside/outside?  
}

#speficic labeling for jumps
#catch +seq/+rep/ect
#honestly need to look up the rule changes before messing with this
#later put this into label function ceebs

#if double downgrade currently also counts as underrotation, fix this
jumps.label.function <- function(df){
  #make a jumpsdf of every jump, seperating combos
  jumps.df <- df %>% 
    filter(label=="jump") %>%
    select(.,-c(level, section, factor, `spin flag`))
  
  #make column for combo/rep/seq and *, will do stuff with this later
  #might be useful for num rotations, planned/unplaned ect
  #remove these so don't get caugt up in combo
  jumps.df <- jumps.df %>% 
    mutate(`+seq` = ifelse(str_detect(aspect_desc,"\\+SEQ"),1,0),
           `+rep` = ifelse(str_detect(aspect_desc,"\\+REP"),1,0),
           `+combo` = ifelse(str_detect(aspect_desc,"\\+COMBO"),1,0)) 
  #should I remane these to make combo names clearer???
  jumps.df$aspect_desc <- str_replace(jumps.df$aspect_desc, "\\+SEQ", "")
  jumps.df$aspect_desc <- str_replace(jumps.df$aspect_desc, "\\+COMBO", "")
  jumps.df$aspect_desc <- str_replace(jumps.df$aspect_desc, "\\+REP", "")
  
  jumps.df$`1stjump` <- str_count(jumps.df$aspect_desc, "\\+")
  jumps.df$combo <- ifelse(jumps.df$`1stjump`>0,1,0)
  combos<-jumps.df[rep(seq_len(nrow(jumps.df)), (jumps.df$`1stjump`+1)),]
  #now for second jump in combo
  combos$jumpscount <- rownames(combos)
  combos$aspect_desc <- as.character(combos$aspect_desc)
  combos<- combos %>% 
    mutate(`2ndjump` = apply(combos,1, function(x){
      x[['2ndjump']] = as.numeric(unlist(strsplit(x[['jumpscount']],"\\.")[[1]][2]))+1
    }))
  #as these are second jumjps not the first one, remove from first combo jump coloumn
  combos <- combos %>% mutate(`2ndjump`= ifelse(is.na(`2ndjump`), 0, `2ndjump`))
  combos <- combos %>% mutate(`1stjump`=ifelse(`2ndjump`=="2", 0, `1stjump`))
  
  #for thrid jump in combo, now tidy up the rest
  combos <- combos %>% 
    mutate(`3rdjump`=ifelse(`2ndjump` == 3,1,0)) %>%
    mutate(`1stjump`=ifelse(`2ndjump`==3, 0, `1stjump`))
  #and fix the notation so its the same as everything else
  combos<- combos%>% mutate(`2ndjump` = ifelse(combos$`2ndjump` == 2, 1, 0))
  combos <- select(combos, -jumpscount)
  #i don't know why this can't be one line
  index <- combos$`1stjump` >1
  combos$`1stjump`[index] <-1
  
  #split combo jumps into indivdual jumps
  combos$fullcombo <- combos$aspect_desc
  combos <- combos %>%
    mutate(aspect_desc = ifelse(`1stjump` == 1,str_extract(aspect_desc, "(.+?)(?=\\+)")
                                ,aspect_desc))
  combos <- combos %>%
    mutate(aspect_desc = case_when(`2ndjump` == 1 & str_count(fullcombo, '\\+') ==2 ~ 
                                     str_extract(aspect_desc, "(?<=\\+)(.*?)(?=\\+)"),
                                   `2ndjump` == 1 & str_count(fullcombo, '\\+') ==1 ~
                                     str_extract(aspect_desc, "([^\\+]+$)"),
                                   TRUE ~ aspect_desc))
  combos <- combos %>%
    mutate(aspect_desc = ifelse(`3rdjump` == 1,str_extract(aspect_desc, "([^\\+]+$)")
                                ,aspect_desc))
  
  #identify features of a jump, rotated edge ect
  #can't rely on info flag column because does not contain all the information
  #eg if edge call and under rotation.
  #sometimes a edge call is listed in info, need to fix this to use info flag
  #test to see if any extra data with  | info_flag == '<<' or info etc
  combos <- combos %>%
    mutate(edge_warning = ifelse(str_detect(aspect_desc,'\\!') ,1,0),
           edge_call = ifelse(str_detect(aspect_desc,'e') ,1,0),
           under_rotated = ifelse(str_detect(aspect_desc,'<') ,1,0),
           downgrade = ifelse(str_detect(aspect_desc,'<<') ,1,0),
           invalid = ifelse(str_detect(aspect_desc,'\\*'),1,0),
           num_rotations = ifelse(str_detect(aspect_desc,"[:digit:][:letter:]"),
                                  str_extract(aspect_desc,"[:digit:]"),1)
    )
  
  combos <- combos %>%
    mutate(`type of element` = case_when(
      str_detect(aspect_desc, "A")   ~ "axel",
      str_detect(aspect_desc, "T")  ~ "toeloop",
      str_detect(aspect_desc, "S")  ~ "salcow",
      str_detect(aspect_desc, "Lo")   ~ "loop",
      str_detect(aspect_desc, "F") ~ "flip",
      str_detect(aspect_desc, "Lz")  ~ "lutz"
      
    ))
  #fall on last element 
  
  return(combos)
  #if wanted to could clean this here, to just have to jump label, but does it matter??
  
  #note to self, half loops are picked up as indivdual jump, exclude later
  #also change to "Eu" under new system
  
  #for jump goe make seperate dataframes for falls, edge calls, under/downgrade and combo
  #issue of too litttle data?? test later
  
  
  #predicts the base value of a jump
  
}


#predicting functions

#predict anything with an ordinal, make a dataframe put highest ordinal first
#change last line to instead of df$names to df[,1]????
ordinal.prediction.function <-function(odf){
  odf <- odf %>%
    mutate(proportion = prop.table(n), error = (proportion*(1-proportion)/n)^0.5,
           cumFreq= cumsum(n),cumProportion = cumsum(proportion), 
           cumError =(cumProportion*(1-cumProportion)/n)^0.5) 
  
  #Inf/NaN occur when there is no data or a skater has never gotten that, for that reason
  #assign it the value of the higher level, because we work down, it will never get assigned
  odf[odf== Inf]<-NaN
  for(i in 1:nrow(odf)){
   if(is.nan(odf$cumError[i])){
     odf$cumError[i] <- odf$cumError[i-1]
   }
  }

  
  #test different distrubutions
  percentiles <- pnorm(rnorm(num.sim))
  
  #now get the chance of all the other levels
  #apply across each level then for each sim
  chances.df <- apply(odf, 1, function(odf){
    qnorm(percentiles,as.numeric(odf['cumProportion']),as.numeric(odf['cumError']))
  })
  
  #this basically says take a random number, if level4 sim bigger level4,
  #if not, what is the proportion they get level 3 or 4? if bigger then level 3
  #if not what is the proportion they get level 2 or above, if bigger level 2 ect
  random.chance <- runif(num.sim)
  chances.df[chances.df>random.chance] <-1
  #finds the pattern, number of 1s mean definetly bigger 
  ordinals <- apply(chances.df, 1, function(x) {
    r <- rle(x)
    max(r$lengths[as.logical(r$values)])
  })
  #now give them their name (for B's) (bug fixed of flipped names)
  out <- rev(odf$names)[ordinals]
  return(out)
}

#####does not take into account the trimmed values, of ditching the highest and lowest
#####need to get the aspect ID and fix
#predicts the componets of the skater, returns vector
component.function <- function(skatername){
  
  ###########################
  #component prediction
  ###########################
  #later break into short/long program etc
  
  #get data points, get mean and sd then simulate
  # use table(filter(df, section=="components")$aspect_desc) to see what is being called
  #use table for name changes
  composition <- filter(judged.aspects, performance_id %in% comps[,1] & 
                          aspect_desc =="Composition") %>% select(.,scores_of_panel)
  performance <- filter(judged.aspects, performance_id %in% comps[,1] & 
                          aspect_desc =="Performance") %>% select(.,scores_of_panel)
  skating.skills <- filter(judged.aspects, performance_id %in% comps[,1] & 
                             aspect_desc =="Skating Skills") %>% select(.,scores_of_panel)
  transitions <- filter(judged.aspects, performance_id %in% comps[,1] & 
                          aspect_desc =="Transitions") %>% select(.,scores_of_panel)
  interpretation <- filter(judged.aspects, performance_id %in% comps[,1] & 
                             aspect_desc %in% c("Interpretation of the Music",
                                                "Interpretation of the Music / Timing",
                                                "Interpretation of the Music/Timing")) %>%
    select(.,scores_of_panel)
  
  #now for each component simulate, assume normal distrubution
  #change distrubution later????
  #might need to pass argument for num sim
  component_list <- list(composition,performance,skating.skills,transitions,interpretation)
  simulate.components <- sapply(component_list, function(x){
    mean = mean(x[,1])
    sd = sd(x[,1])
    #this assumes there is random error in each component and they aren't correlated,
    #which is unlikely I think
    #change the value depending on how many simulations you want
    prediction <- rnorm(num.sim, mean,sd)
    return(prediction)
  })
  #if need to break out
#component.df <- data.frame(simulate.components, colnames = c('composition','performance',
  #                                      'skating.skills','transitions', 'interpretation'))
  predicted.components <- rowSums(simulate.components)  
  return(predicted.components)
}

#predicts the level of an element (determines bv), return df (change)
#currently only works with label (eg step, spin, not detailed like type of spin etc)
#change to level.prediction( type of element) 

level.prediction <- function(x){
  #predict things, that have a level eg spins and steps
  #for this test starter function, assume all spins.step sequences are the same
  #later break out into type of spin, flying etc, season etc
  # eg could be something like df level.elements <- c("step", "spin","spin","spin")
  element <- df %>% filter(., performance_id %in% comps[,1] & 
                      label ==x) %>% select(.,level)
  #now we need to make an ordinal prediction, 
  #make a frequency table and get error estimates
  #I am sure there is a faster ie less computationlly expensive way to do this, fix later
  
  level.table <- data.frame(names = c("4", "3", "2", "1", "B"), 
                            n = c(sum(element == 4), sum(element == 3), sum(element == 2), 
                                  sum(element == 1),sum(element == "B")))
  out <- ordinal.prediction.function(level.table)
  return(out)
}

#predict the goe of an element
#where x is the type of element
#maybe later consider doing a general jump goe or spin goe or something, experiment
#need to predict for each judge, trim and average,
#in a way this works, because we are predicting one judges response, the mutliple trials
#simulate the multiple judges on the pannel

#input a filter df of the elements to get the goe of
goe.function <- function(df){
  #for this one we need to get the performance ID then the aspect ID
  elements <- df %>% filter(., performance_id %in% comps[,1]) %>%
    select(.,aspect_id)
  marks <- filter(judge.scores, aspect_id %in% elements[,1]) %>%
    select(.,score)
  #now do the same ordinal prediction as with level function
  #base = 1, plus 1 = 1.1 ect
  goe.table <- data.frame(names = c(1.3, 1.2, 1.1, 1, 0.9, 
                                   0.8, 0.7), 
                          n = c(sum(marks == 3), sum(marks == 2), sum(marks == 1),
                                sum(marks == 0), sum(marks == -1), sum(marks == -2), 
                                sum(marks == -3)))
  out <- ordinal.prediction.function(goe.table)
  return(out)
  
  
}

#predicts num rotations, edge, undertoated or not
#jumpdf is the dataframe, jump is the jump label, could make simplier later?

jump.prediction.function <- function(jumpdf){
  #element <- filter(jumpdf, performance_id %in% comps[,1] & 
   #                   `type of element` ==jump)
  numrotations <- data.frame(names = c("4", "3", "2", "1"), 
                             n = c(sum(jumpdf$num_rotations == 4), sum(jumpdf$num_rotations == 3), 
                                   sum(jumpdf$num_rotations == 2), sum(jumpdf$num_rotations == 1)))
  ordinal.prediction.function(numrotations)
  #do I want to add colnames to make more readable??
  rotated <- data.frame(names= c("full", "under", "downgrade"),
                        n= c(0,sum(jumpdf$under_rotated), sum(jumpdf$downgrade)))
  #fully rotated is every jump that is not under or downgrade
  rotated[1,2] <- nrow(jumpdf)-sum(rotated$n)
  ordinal.prediction.function(rotated) 
  #only do for flip and lutz if needed to speed up?
  #also could be a check to make sure only right jumps are being labeled as edge call etc
  edges <- data.frame(names= c("clean", "!", "e"),
                      n= c(0,sum(jumpdf$edge_warning), sum(jumpdf$edge_call)))
  #fully rotated is every jump that is not under or downgrade
  edges[1,2] <- nrow(jumpdf)-sum(edges$n)
  
  out <- data.frame(num_rotations = ordinal.prediction.function(numrotations),
                    edges = ordinal.prediction.function(edges),
                    rotated = ordinal.prediction.function(rotated))
  return(out)
  #jump goe is affected by calls
  #get the goe of a jump, remove the deductions due to under or whatever to get 'true' goe
  #then predict goe of a jump, and add in the deduction
  
}




###################
#data/variables
###################
#load the 4 csvs
#number of simulations to run
num.sim <- 20
basevalue <- read.csv("~/Code/basevalue.csv")
names(basevalue) <-c("aspect_desc","bv")

packages <- c("dplyr", "stringr", "MCMCglmm")
lapply(packages, library, character.only = TRUE)

df <- judged.aspects
df <- label.function(df)
combos <- jumps.label.function(df)
########


#make a list of people competing
#each element of the list contains a dataframe of planned componet
#run that component through the label function
#for each element of that dataframe select the approiate prediction function

#from the planned component sheets
program.list = split(planned, list(planned$name, planned$program))




#only works for singles right now
lapply(program.list, prediction.function)
prediction.function<- function(df1){
  skatername <- as.character(df1$name[1])
  comps <- filter(performances, name == skatername) %>% select(.,performance_id)
  
  #make more elegant later, really need to label spin or jump
  df1$section <- "elements"
  predictdf <- label.function(df1)
  predictdf <- select(df1, -c('level', 'spin_flag'))
  
  #need to fix for combo jumps somehow
  #for each element in the dataframe use approrate prediction function,
  #for each row in the df
  level <- apply(predictdf, 1, function(df2){
    #again reminder spins are not fixed yet
    #spin flag
    if(df2['label'] == "spin"){
      level <- level.prediction(df2['label'])
      #now we have the level get the base value
      name<- data.frame(aspect_desc = paste(df2['aspect_desc'],level, sep=""))
      
      #spin flag prediction
      
      #now get the goe
      goe <- goe.function(filter(df,label == df2['label']))
      #OR for a more precise goe
      #goe <- goe.function(filter(df, str_detect(aspect_desc, df2['aspect_desc]))
      
      
      #make function
      out<-inner_join(name, basevalue)
      #need to get bv for each level prediction, not just first one
      out$bv <- as.numeric(as.character(out$bv))
      #and combine for the score
      out$mark <- goe*out$bv
      out <- data.frame(out, goe, level)
      #drop name?/make goe more readable????
   }
    else if (df2['label']== "jump"){
      #if combo string find +
      
      
      
     #function to get the different types of jumps
     #for solo/first jump in combo
     #if dodgy jump skater may decide not to put combo on end, so combine together
      #for the first jump in combo don't worry about popping
      #to do write a pop function
      #get edge call for all lutzes and flips, not just same rotations
     jumps1 <- combos %>% 
       filter(.,`1stjump`==1 | combo == 0) %>%
       filter(., performance_id %in% comps[,1] &
                `type of element` == df2['type of element'] &
                num_rotations ==substr(df2['aspect_desc'],1,1))
     
     

     
     pred <- jump.prediction.function(jumps1)
     pred <- pred %>% 
       mutate(num_rotations=  ifelse(rotated=='downgrade', num_rotations - 1,
                                     num_rotations))
     #insert name function for pooped jumps
     pred$aspect_desc <- df2['aspect_desc']
     
     
     #need pop function here
     #for now just repeat 
     out <- inner_join(pred, basevalue)
     out$bv <- as.numeric(as.character(out$bv))
     out <- out %>%
       mutate(mark =  ifelse(rotated == 'under',bv*0.7,bv))
     out <- out %>%
       mutate(mark = ifelse(edges == 'e'& rotated == 'full', bv*0.7,
                            ifelse( edges == 'e' & rotated == 'under',bv*0.6,bv)))
   

     #for now combine all jumps in a combo
     #exclude 1/2 loop/Eu for now
     #combojumps <- combos %>% 
       #filter(.,`1stjump`==0 & combo == 1) %>%
       #filter(.,!(`type of element` =='loop' & num_rotations ==1 &`2ndjump` ==1))
       #now run both dataframes though jump prediction to predict UR edge
       #use this to get base value
    #   bv<-
      # for now just treat all goe as the same, later do correction factors
      
    }
    #out <- base +goe
    #return(out)
  })
}
lapply(seq_along(skaterlist), prediction.function)

#pop function
#for a skater see what elements they have attempted in competition
#in a competition grade each jump as full potential or pop
#a pop is a jump that is not a full potential ie if skater can do 4T but does 3T
#take into account repeat rules

#this simple function assumes that goe and base value aren't corleated,
#but of course they are, especially for jumps
#experiment with step sequence and spins, see if there is a difference in goe withdifferent
#levels, test overall and with different skaters. 
#possible there is a cutoff ie a fall on step sequence/could drop levels and affect goe
#but might not be a big difference in terms of level 3 vs level 2? if level is unusually
#low for a skater---test
  
                          
  #massively oversimplified, fall prediction??????
 #if jumps consider num roations and toeloops - jump function pass arguments???
#falls for jumps??????

#jump function
  #base value of a jump depends on under rotation, num rotations, combo ohh my
  #second half
  
#fall.prediction
#ice dance still a work in progress

#add date and season to performances
#date.function