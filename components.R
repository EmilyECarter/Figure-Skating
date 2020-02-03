#components function

########test.component.function #########
#I want to test for accuracy of how close it is to the real result,
#but also argualbly more importantly is the ranking of people in a competition
#asign a comp id
df1 <- performances %>%
  group_by(competition, program) %>%
  { mutate(ungroup(.), g = group_indices(.)) }
  
ranking.train <- subset(df1,g %in% sample(unique(df1$g),120))
ranking.test <- anti_join(df1, ranking.train, by = "g")

#using the data from the test competions get the comps and scores
#here we want take a competition from the training dataset 

#so we need to take a list of skater names run them through the component function
#turn the component function into a databse and rank, then compare ranking from actual compeition


#for each competition in the test set get the corasponding perfmonace ids from the training set 
comp.test.list <- split(ranking.test, ranking.test$g)


id.list <-lapply(comp.test.list,function(comp){
  #print(comp)
  compsid <- sapply(as.character(comp$name), function(skatername){
    comps <- filter(ranking.train, name == skatername) %>% select(.,performance_id)
    }) 
  print(comps)
 #take the comps ID and runs them through the component function- and assigns ranks based 
  #on each simulation 
  #print(compsid)
  comp.scores <- lapply(compsid, function(list){
   # print(list)
    out<- component.function1(list)
    #out<- lapply(list, component.function1)
    out<-as.data.frame(out)
  })})
  #print(comp.scores) })
    #rank each sim
    #ranks <- apply(out, 1, function(x) rank(-x) )
    #now compare each column of ranks to comp[6]
    #print(ranks)
    apply(ranks, 2, function(x){
      print(x)
      res <- cor.test(x, comp[6]$rank, 
                    method = "spearman")
      print(res)
    })
    #also get the mean distance from predicted to actual
  } )
#now get data
  #data-num times predict winner
  #percentage of time it gets it exactly correct
  apply(ranks, 2, function(x){
    res<- cor.test(x,comp.test.list[[1]][6]$rank)
    })
})
  



sult1 <- lapply(id.list, function(list){
  out<- sapply(list, component.function1)
  out<-as.data.frame(out)
  ranks <- apply(out, 1, function(x) rank(-x) )
  #I now need to compare this to the orginal ranks comp[6]<- so I need to do this in the same 
  #lapply loop as above 
  })

#need to remove people who there is no entry for in the training data


#now for the test dataset see how well it predicts the result
ranking.test <- ranking.test %>% group_by(g) %>% mutate(component_rank = rank(-total_component_score))


#rank corelaction
#spearman p

out1 <- lapply(out1, function(skatername){
  comps <- filter(performances, name == skatername) %>% select(.,performance_id)
})








########model 1#######
#this uses the mean and sd of a skaters past scores to create predictions for the competition
#only uses the skater name and the average of the trimmed component score




component.function1 <- function(comps){
  ###########################
  #component prediction
  ###########################
  #later break into short/long program etc
  
  #get data points, get mean and sd then simulate
  # use table(filter(df, section=="components")$aspect_desc) to see what is being called
  #use table for name changes
  composition <- filter(judged.aspects, performance_id %in% comps & 
                          aspect_desc =="Composition") %>% select(.,scores_of_panel)
  performance <- filter(judged.aspects, performance_id %in% comps & 
                          aspect_desc =="Performance") %>% select(.,scores_of_panel)
  skating.skills <- filter(judged.aspects, performance_id %in% comps & 
                             aspect_desc =="Skating Skills") %>% select(.,scores_of_panel)
  transitions <- filter(judged.aspects, performance_id %in% comps & 
                          aspect_desc =="Transitions") %>% select(.,scores_of_panel)
  interpretation <- filter(judged.aspects, performance_id %in% comps & 
                             aspect_desc %in% c("Interpretation of the Music",
                                                "Interpretation of the Music / Timing",
                                                "Interpretation of the Music/Timing")) %>%
    select(.,scores_of_panel)
  
  #now for each component simulate, assume normal distrubution
  #change distrubution later????
  #might need to pass argument for num sim
  component_list <- list(composition,performance,skating.skills,transitions,interpretation)
  simulate.components <- sapply(component_list, function(x){
    mean <- mean(x[,1])
    sd <- ifelse(length(x[,1])>1, sd(x[,1]),0)
    #sd <- sd(x[,1])
    
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




