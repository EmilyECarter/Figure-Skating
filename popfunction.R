#skater consitencygetModelInfo

#correlation between jumps in a program














better.jump.function <- function(skatername){
  #because of the repeat rules need to calculate for each skater indivdually instead of
  #one element at a time
  
  
  # in a combo jump the GOE is supposed to be averaged, so need to be careful
  #on the other hand if a jump is dodgy a skater might decide to save the combo for later
  #if a fall occurs on a combo, we do not know how the first jump went 
  
  #to do, update for quad rules 
  type <- 'flip'
  #count first jump of combo and solo jumps in the same catorgy for now
  #(to do a combo they would have had to complete the first jump)
  sjumps <- combos %>%
    filter(., performance_id %in% comps[,1] & `type of element` == type) %>%
    filter(., combo == 0 | `1stjump` == 1)
  mutate(sjumps, rep == )
  #now take out any with 3 repeats in a program as skaters can only repeat the same 
  
  
  
  
  
  
  #to do, account for second half jumps
  #skaters who change their program mid-way ?????
  
  
  #pop function is caluculated first, for all jumps attempted
  #can only repeat a jump 2 times, do not count as a pop if already repeated
  #some skaters only atempt a harder jump as a single jump and not to repeat it
  #do not count this as a pop
  
  
  max.rotations <- max(sjumps$num_rotations)
  num.pops <- filter(sjumps1, num_rotations < max.rotations)
  
  #repeat rule
  
  
  
  
  
  
  repeats <- sjumps %>% 
    group_by( performance_id) %>% 
    summarise( max = max(num_rotations), n = n() ,pops = sum(num_rotations < max)) %>%
    mutate(.,pops= ifelse(n>2,pops-n+2 ,pops))
  #check if they do the jump in a combo
  truepop <- filter(sjumps, combo == 0)
  
  #true pops don't count if they never repeat
  
  #don't count as pop
  
  
  


  
  #for competitions with a pop determine if it is a pop into 
  
  num.pops
  pop.chance <- num.pops/nrow(jumps1)
  
  
  stick.to.plan
  
  
  element <- jumps1$num_rotations
  #need to estimate how many rotations they will complete
  level.table <- data.frame(names = c("4", "3", "2", "1"), 
                            n = c(sum(element == 4), sum(element == 3), sum(element == 2), 
                                  sum(element == 1)))
  out <- ordinal.prediction.function(level.table)
  return(out)
  
  ########
  #calculate if they are able to get off a combo
  
  #deviate
  #any jump with an * is a failed combo
  invalid <- filter(df, info_flag =='*')
  i2 <-filter(df, `scores_of_panel`== 0 & label == 'jump')
  i3 <- filter(df, '')
  invalid <-merge(i2, invalid, all = T)
  invalid <- left_join(invalid, performances, by= "performance_id")
  invalid <- select(invalid, c(aspect_id:base_value,goe, `type of element`,program, name))
  #take out things that don't count in short program
}


inTraining <- createDataPartition(combos$goe, p = .75, list = FALSE)
training <- combos[ inTraining,]
testing  <- combos[-inTraining,]
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,

  repeats = 10)
gbmFit1 <- train(goe ~ aspect_num+info_flag+`type of element`+credit_flag +combo, 
                 data = training, 
                 method = "bayesglm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                na.action = na.pass)
predict(gbmFit1, testing)

gbmFit1
