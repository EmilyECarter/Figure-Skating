#pcs
programs<- filter(d,displine == "DANCE"&level== "senior" ) 
ids <- filter(performances,program %in% programs$label )
pcs <- filter(judged.aspects, performance_id %in% ids$performance_id & section == 'components')
pcs <- left_join(pcs, performances)


gbmFit1 <- train(scores_of_panel ~ total_deductions+name
                   starting_number +competition, 
                 data = filter(pcs, aspect_desc == "Transitions"), 
                 method = "bayesglm", 
                 trControl = fitControl,
                 na.action = na.pass)
RMSE      Rsquared   MAE      
0.307957  0.9360311  0.1940721
loess r-squared variable importance

Overall
starting_number   100.00
total_deductions   39.16
competition        15.00
name                0.00

#however when removing name
gbmFit1 <- train(scores_of_panel ~ total_deductions
                 starting_number +competition, 
                 data = filter(pcs, aspect_desc == "Transitions"), 
                 method = "bayesglm", 
                 trControl = fitControl,
                 na.action = na.pass)
RMSE       Rsquared   MAE      
0.9159392  0.4730577  0.7596864
Overall
starting_number   100.00
competition        82.95
total_deductions    0.00
#starting number is probably corelated with name though
#scatterplot of each skaters average start order, line over/under half


#test against tech scores

#starting number compare
skaterpcs <-performances %>% add_count(competition, program)
pcs <-pcs %>% add_count(competition, program)
pcs$n <- pcs$n/5 #fot the 5 coponent marks
pcs$reverse_start <- pcs$n - pcs$starting_number +1
#some reserve start at 0. people who dropped out after short???
gbmFit1 <- train(scores_of_panel ~ total_deductions
                 reverse_start +competition, 
                 data = filter(pcs, aspect_desc == "Transitions"), 
                 method = "bayesglm", 
                 trControl = fitControl,
                 na.action = na.pass)
RMSE       Rsquared   MAE      
0.8004215  0.6009158  0.6207357
#an improvement over starting number!
gbmFit1 <- train(scores_of_panel ~ total_deductions +name+
                 reverse_start +competition, 
                 data = filter(pcs, aspect_desc == "Transitions"), 
                 method = "bayesglm", 
                 trControl = fitControl,
                 na.action = na.pass)
RMSE       Rsquared   MAE      
0.2771231  0.9484941  0.1778573
#noow do groups
#for dance max 5 per warm up group
pcs$num_group <- ceiling(pcs$n/5)
pcs$group <- ceiling(pcs$reverse_start/5)
gbmFit1 <- train(scores_of_panel ~ total_deductions +
                   group +competition, 
                 data = filter(pcs, aspect_desc == "Transitions"), 
                 method = "bayesglm", 
                 trControl = fitControl,
                 na.action = na.pass)
RMSE       Rsquared   MAE      
0.8396261  0.5612627  0.6602727
#dropped, is this becasue of lack of speficity??? or is order withina group useful

gbmFit1 <- train(scores_of_panel ~ total_deductions + name+
                   group +competition, 
                 data = filter(pcs, aspect_desc == "Transitions"), 
                 method = "bayesglm", 
                 trControl = fitControl,
                 na.action = na.pass)
RMSE       Rsquared  MAE      
0.2758363  0.949243  0.1786563
#name remains a super important factor, 
#is this because of reputation or performance
#(test element scores- but rep judging hteir???? and remove name)

#add in for when people withdraw and their is less than a full roster (in GPs and stuff??)
#
#compare people who have moved groups in the two programs


#look at spread of componant scores

#can I do a group by cor????

all_pcs <- left_join(judged.aspects, performances) 
all_pcs <- all_pcs %>% filter(., section == 'components')
all_pcs$aspect_desc <- recode(all_pcs$aspect_desc,"Interpretation of the Music/Timing" = "Interpretation of the Music", )
all_pcs$aspect_desc <- recode(all_pcs$aspect_desc,"Interpretation of the Music / Timing" = "Interpretation of the Music", )

x<- all_pcs %>% group_by(program) %>% summarise(., cor= cor(total_element_score, total_component_score))
x<-left_join(x, d, by =(c("program"="label")))
#take into account different types of competitions
ggplot(aes(cor, level, colour = displine), data=x)+geom_point()
#in dance on seniors at least
#this could be because reputation (ie some skaters are judged given better tech scores
# programs skated well with bigger tech scores also mean fewer mistakes potentially
#or that skaters have roughly equal tech and performance capabilites)

#what happens when we seperate this out into the indivdual components???
all_pcs <- all_pcs %>% 
  mutate(aspect_desc = ifelse(aspect_desc %in% c("Interpretation of the Music",
                                  "Interpretation of the Music / Timing",
                                  "Interpretation of the Music"),
       "Interpretation of the Music", aspect_desc))

x2<- all_pcs %>% group_by(aspect_desc , program) %>% summarise(., cor= cor(total_element_score, scores_of_panel))
x2<-left_join(x2, d, by =(c("program"="label")))
ggplot(aes(cor, level, colour = displine), data=x2)+geom_point()
#not suprisingly they seem to be clustered
ggplot(aes(cor, aspect_desc, colour = displine), data=subset(x2, level=='senior'))+
         geom_point()
ggplot(aes(cor, program.y, colour = displine), data=subset(x2, level=='senior'))+
  geom_point()
#less corelation in the long?
#okay this is saying that all of the corelations are exactly the same, which seems wrong

ggplot(aes(cor, program.y, colour = aspect_desc), data=subset(x2, level=='senior'))+
  geom_point()
ggplot(aes(cor, displine, colour = aspect_desc), data=subset(x2, level == 'senior'))+
         geom_point()

ggplot(aes(cor, aspect_desc), data=subset(x2, level=='senior'))+geom_boxplot()



#now lets consider only clean programs
#ie programs without a fall/deduction

#need to add back in time vilotations

x3<- all_pcs %>% 
  filter(.,total_deductions == 0) %>%
  group_by(aspect_desc , program) %>% summarise(., cor= cor(total_element_score, scores_of_panel))
x3<-left_join(x3, d, by =(c("program"="label")))
#is there a difference?
x3$difference <- x3$cor -x2$cor
#plot
ggplot(x3, aes(program, difference)) +
  geom_bar(stat = "identity", aes(fill = aspect_desc), legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#interestingly the correlations didn't change much except in the
# junior ladies free?

#x3 is clean program correlations, and x2 is all programs so a positive value means
#cleaner programs are more correlated with components


#are certain components more dependant on deductions????
ggplot(x3, aes(aspect_desc, difference)) +
  geom_bar(stat = "identity", aes(fill = displine), legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#this plot tells us nothing really, I mean performance seems to be the most stable??
#dance barly in this, now lets superate out juniors, because they might be mucking it up

ggplot(subset(x3, level =="senior"), aes(program, difference)) +
  geom_bar(stat = "identity", aes(fill = aspect_desc), legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#interestingly it is the ladies free skate again with the biggest difference
#this time clean programs are less correlated with compoinets than all programs
#(again this doens't nessisarily tell us much)



ggplot(subset(x3, level=="senior"), aes(aspect_desc, difference)) +
  geom_bar(stat = "identity", aes(fill = displine), legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#so basically overall clean programs are not more correlated with tes than overall
#is this really suprising though, because whether a skater has fallen will affect the tes


#(?? to do plot with change in corelation and number of programs considered/change 
#in programs considered)


#so tes is a function of two things, base value and goe
#now lets see if base value or goe is corelated with pcs more
all_pcs2 <- left_join(judged.aspects, performances) 

all_pcs2$aspect_desc <- recode(all_pcs2$aspect_desc,"Interpretation of the Music/Timing" = "Interpretation of the Music", )
all_pcs2$aspect_desc <- recode(all_pcs2$aspect_desc,"Interpretation of the Music / Timing" = "Interpretation of the Music", )
#could I do this all in one statement using mutate???)
tech_test <- all_pcs2 %>% 
  group_by(., performance_id) %>%
  summarise(avg.goe=mean(goe, na.rm=T), bv = mean(base_value, na.rm=T) )
all_pcs2<- left_join(tech_test, all_pcs2)

goe_cor<- all_pcs2 %>% group_by(program) %>% summarise(., cor= cor(avg.goe, total_component_score))
goe_cor<-left_join(x4, d, by =(c("program"="label")))
ggplot(goe_cor, aes(program, cor)) +
  geom_bar(stat = "identity", aes(fill = displine)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bv_cor <- all_pcs2 %>% group_by(program) %>% summarise(., cor= cor(base_value, total_component_score))
bv_cor<-left_join(bv_cor, d, by =(c("program"="label")))
ggplot(bv_cor, aes(program, cor)) +
  geom_bar(stat = "identity", aes(fill = displine)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bv_cor$difference <- bv_cor$cor-goe_cor$cor
ggplot(bv_cor, aes(program, difference)) +
  geom_bar(stat = "identity", aes(fill = displine)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#bv - goe so bv is more correlated than avegerage goe
#except in men where it is about the same
#super interesitng dance has the biggest effect, while dance is known as a sport determined
#by goe, interesting that the reputation it has is that all the top teams are getting the
#same levels, what seperates them is the pcs/goe

#This however is flawed as avgerage goe is also afacted by the element, so is a bunk 
#lets calculate the avg judges score 
avg.goe <- judge.scores %>% 
  group_by(aspect_id) %>%
  summarise(avg.goe = mean(score))
all_pcs2 <- select(all_pcs2, -avg.goe)
all_pcs2 <- left_join(avg.goe, all_pcs2)
#filter out the elements
all_pcs2 <- filter(all_pcs2, section == "elements")

#now lets plot again
goe_cor2<- all_pcs2 %>% group_by(program) %>% summarise(., cor= cor(avg.goe, total_component_score))
goe_cor2<-left_join(goe_cor2, d, by =(c("program"="label")))
ggplot(goe_cor2, aes(program, cor)) +
  geom_bar(stat = "identity", aes(fill = displine)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bv_cor2 <- all_pcs2 %>% group_by(program) %>% summarise(., cor= cor(bv, total_component_score))
bv_cor2<-left_join(bv_cor2, d, by =(c("program"="label")))
ggplot(bv_cor2, aes(program, cor)) +
  geom_bar(stat = "identity", aes(fill = displine)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bv_cor2$difference <- bv_cor2$cor-goe_cor2$cor
ggplot(bv_cor2, aes(program, difference)) +
  geom_bar(stat = "identity", aes(fill = displine)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#now this is looking much more resonable, pretty similar  not as big a change
#interestingly base vaule contines to be more coreleated than goe in ice dance
#where in the other disclipnes it is the other way around
#(this would be super interesting to compare to this year since the judging 
#system change)

#do a simplar linear regression or something to see which one is the biggest factor?????

#plot both on same plot?????
ggplot() +
  geom_point(data = bv_cor2, aes(program, cor, colour = "base value correlations"),
    stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+

  geom_point(data = goe_cor2, aes(program, cor, colour = "goe correlations"), 
             stat = "identity")+
  scale_colour_manual("",
                      values=c('base value correlations'='blue','goe correlations'='red'))
  
  scale_fill_identity()+
  scale_colour_manual(name = 'legend', 
                      values =c('blue'='blue','red'='red'),
                      labels = c('base value correlations','goe correlations'))
#interestig so appart from ice dance this really doesn't appear to have that 
  #great a corellation
  #obviously this doesn't say anything, skaters who recive high pcs are also likely to 
  #be good skaters
  
#made a mistake
#so this is completly ignoring skaters, 
#just saying for each element how does this affect overall score
#recalcuate for base value
bv_cor3 <- all_pcs2 %>% group_by(program) %>% summarise(., cor= cor(base_value, total_component_score))
bv_cor3<-left_join(bv_cor3, d, by =(c("program"="label")))
#now of course the correlation drops
#probably because of spins/steps/repeat rule
#now group by program first
bv_cor4 <- all_pcs2 %>%
  group_by(performance_id) %>% 
  mutate(., total_base_value = sum(base_value))%>%
  ungroup() %>%
  group_by(program) %>%
  summarise(., cor= cor(total_base_value, total_component_score))
bv_cor4<-left_join(bv_cor4, d, by =(c("program"="label")))
goe_cor4 <- all_pcs2 %>%
  group_by(performance_id) %>% 
  mutate(., avg_total_goe = mean(avg.goe))%>%
  ungroup() %>%
  group_by(program) %>%
  summarise(., cor= cor(avg_total_goe, total_component_score))
goe_cor4<-left_join(goe_cor4, d, by =(c("program"="label")))
#wow okay way more correlated when it is the avaege over the program! good catch!!
ggplot() +
  geom_point(data = bv_cor4, aes(program, cor, colour = "bv"),
             stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point(data = goe_cor4, aes(program, cor, colour = "goe"), 
             stat = "identity")+
  scale_colour_manual("",
                      values=c('bv'='blue','goe'='red'))
#okay so average goe of the program is actually super highly correlated to components
#this is maybe not suprising as both are determined by the judges
# does this fufil the conventional wisdom that ice dance is the most politcal???
# I mean look at those corrleations
#if a program has messy elements with interuption to the program (falls) it should detract
#although all the senoir teams have at least a 0.6 correlation for bv

#okay so lets dig into this
goe_cor4 <- all_pcs2 %>%
  group_by(performance_id) %>% 
  mutate(., avg_total_goe = mean(avg.goe))%>%
  ungroup() %>%
  filter(., )
  group_by(program, aspect_desc) %>%
  summarise(., cor= cor(avg_total_goe, total_component_score))
goe_cor4<-left_join(goe_cor4, d, by =(c("program"="label")))
#lets look at which copments are affected most


#seperate out clean programs only, as not clean programs should be affacted by pcs
avg.goe <- judge.scores %>% 
  group_by(aspect_id) %>%
  summarise(avg.goe = mean(score))

goe_cor_base <- left_join(judged.aspects, performances) 
avg_total_goe <- left_join(avg.goe, goe_cor_base) %>%
  filter(., section=="elements")%>%
  group_by(performance_id) %>% 
  summarise( avg_total_goe = mean(avg.goe))
goe_cor_base$aspect_desc <- recode(goe_cor_base$aspect_desc,"Interpretation of the Music/Timing" = "Interpretation of the Music", )
goe_cor_base$aspect_desc <- recode(goe_cor_base$aspect_desc,"Interpretation of the Music / Timing" = "Interpretation of the Music", )
goe_cor_base <- left_join(goe_cor_base, avg_total_goe)

goe_cor5 <- goe_cor_base %>%
  filter(., section == "components")%>%
  group_by(program, aspect_desc) %>%
  summarise(., cor= cor(avg_total_goe, scores_of_panel))
goe_cor5<-left_join(goe_cor5, d, by =(c("program"="label")))
ggplot(goe_cor5, aes(aspect_desc, cor))+
  geom_boxplot()
#so on first blush performance seeams more affected by avg goe, which is good
#lets seperate out the juniors
ggplot(subset(goe_cor5, level=="senior"), aes(aspect_desc, cor))+
  geom_boxplot()
#so performance is definetly the most corelated, although not by a ton
#do I need to go by each discpline???  maybe later

# so now lets compare only clean programs with no deductions
nod_goe_cor <- left_join(goe_cor_base, avg_total_goe) %>%
  filter(., section == "components")%>%
  filter(., total_deductions == 0)%>%
  group_by(program, aspect_desc) %>%
  summarise(., cor= cor(avg_total_goe, scores_of_panel))
nod_goe_cor<-left_join(nod_goe_cor, d, by =(c("program"="label")))
ggplot(nod_goe_cor, aes(aspect_desc, cor))+
  geom_boxplot()
#man junior is really all over the place, wonder if this has to do with unoffical junior goe caps
#or the fact that junior has a wider range of skaters and abilities, to look into
ggplot(subset(nod_goe_cor, level=="senior"), aes(aspect_desc, cor))+
  geom_boxplot()
#looks similar, lets plot together to compare

ploting_data <- rbind(cbind(nod_goe_cor, group=rep('nod',80)), 
                      cbind(goe_cor5, group=rep('all',80)))
ggplot(subset(ploting_data, level=="senior"), aes(aspect_desc, cor))+
  geom_boxplot(aes(colour=group))
nod_goe_cor$cor-goe_cor5$cor
#so no deductions is acutally slightly more correlated, 
#on one hand this could be because we are removing outliers or absolute messes of programs
#where the skating might be good, but every element is messy
#still conventional wisdom would say they should be less correlated, if it was truly being judged 
#fairly, as we were just looking at programs with no falls... there should still be a range of 
#skating
#also super interesting to note that performance correlation barely changes
#composision/transistions really shouldn't change?????

#okay now lets compare programs with a avg goe that is possitive only 
#first lets look at the distrbution of avg goe in these programs
ggplot(goe_cor_base, aes(x=avg_total_goe))+geom_histogram()
quantile(goe_cor_base$avg_total_goe, probs = c(0, 0.25, 0.5, 0.75, 1))
goe_cor_base<-left_join(goe_cor_base, d, by =(c("program"="label")))
ggplot(goe_cor_base, aes(x=avg_total_goe, color=level)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
#honestly it looks like junior avg goe is similar to seniors
summary(subset(goe_cor_base, level=="junior")$avg_total_goe)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-0.8981  0.4286  0.9630  0.8907  1.4286  2.1270 
summary(subset(goe_cor_base, level=="senior")$avg_total_goe)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-1.4222  0.1481  0.6759  0.7017  1.2222  2.8272 
#interesting, so senior goe is slightly lower, honestly wouldn't have expected that
ggplot(goe_cor_base, aes(x=avg_total_goe, color=displine)) +
  geom_histogram(fill="white", alpha=0.1, position="identity")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=displine),
             linetype="dashed")
mu<-goe_cor_base %>%
  group_by(displine)%>%
  summarise(grp.mean=mean(avg_total_goe))
1    DANCE 1.3460515
2   LADIES 0.5967867
3      MEN 0.4638014
4    PAIRS 0.6024358
#so everything else is fairly clustered around 0.6/0.5, except for dance
goe_cor_base %>%
  filter(.,level=='senior')%>%
  group_by(displine)%>%
  summarise(grp.mean=mean(avg_total_goe))
1    DANCE 1.3278693
2   LADIES 0.5664988
3      MEN 0.4643967
4    PAIRS 0.6169672
#junior is basically the same as well

#does this change if we remove programs with deductions??
mu2<-goe_cor_base %>%
  filter(total_deductions==0)%>%
  group_by(displine)%>%
  summarise(grp.mean=mean(avg_total_goe))
ggplot(subset(goe_cor_base, total_deductions==0), aes(x=avg_total_goe, color=displine)) +
  geom_histogram(fill="white", alpha=0.1, position="identity")+
  geom_vline(data=mu2, aes(xintercept=grp.mean, color=displine),
             linetype="dashed")
#avg goe bumps up again by about 0.2
#dance doesn't change too much, because this is a pretty crude easure, as it is total dedcutions
#in dance deductions are more likely to be for extended lift or time or something
mu2$dif<-mu2$grp.mean-mu$grp.mean

#interestingly for juniors ladies goe shoots up

#okay now lets try the 

###to do, look up rules about limits on pcs for falls, and which components are supposed to be 
#affected
#okay now lets try the correlation for only programs with no deductions, and above average goe
#

######to do plot 
ggplot(goe_cor_base, aes(total_element_score, total_component_score, colour=displine))+geom_point()
ggplot(subset(goe_cor_base, level=='senior'), aes(total_element_score, total_component_score, 
                                                  colour=displine))+geom_point()
#make this a quadrent graph
pd <- select(goe_cor_base, total_component_score, total_element_score, total_segment_score,
            performance_id, level, displine, program) 
pd<- unique(pd)

x<-lapply(split(pd, pd$program), function(x){
  comp_dist <-ecdf(x$total_component_score)
  x$comp_dist <- comp_dist(x$total_component_score)
  tech_dist <-ecdf(x$total_element_score)
  x$tech_dist <- tech_dist(x$total_element_score)
  total_dist <-ecdf(x$total_segment_score)
  x$total_dist <- total_dist(x$total_segment_score)
  return(x)
})
pd<-do.call("rbind",x)  
  

#do a similar thing but with goe and base value later

#this is a plot of all programs,
ggplot(pd, aes(comp_dist, tech_dist, colour=displine))+geom_point()

ggplot(subset(pd, level=='senior'& displine=='DANCE'), 
              aes(comp_dist, tech_dist, colour=displine))+geom_point()
#dance seems to stick the most to the stright line
ggplot(subset(pd, level=='senior'& displine=='MEN'), 
       aes(comp_dist, tech_dist, colour=displine))+geom_point()

ggplot(subset(pd, level=='senior'& displine=='LADIES'), 
       aes(comp_dist, tech_dist, colour=displine))+geom_point()

ggplot(subset(pd, level=='senior'& displine=='PAIRS'), 
       aes(comp_dist, tech_dist, colour=displine))+geom_point()

#dance sticks the most to a striaght line, followed by pairs, then ladies, men is all over the place

##splot indivdial skaters average comp and tech



#plot total score vs percentage of score from tech

#plot total score vs comp/tech_dist
ggplot(subset(pd, program=="PAIRS FREE SKATING"), 
       aes(comp_dist, tech_dist, colour=total_score))+geom_point()
#this isn't super interesting, what we want is some measure of people who get good scores
#but are further appart from the middle, even
#ie looking for things/people that are specilists in one area
ggplot(subset(pd, program=="PAIRS FREE SKATING"), 
       aes(total_segment_score,tech_dist-comp_dist))+geom_point()
#now this is interesting, now to add in more displines????
ggplot(subset(pd, program=="PAIRS FREE SKATING"), 
       aes(total_dist,tech_dist-comp_dist))+geom_point()
#I think using qauntiles are okay, but visually it looks better/
#when using raw scores visually it lloks like it has longer tails?? is this a problem
#I don't think so, but keep in mind
ggplot(subset(pd, level=="senior"), 
       aes(total_dist,tech_dist-comp_dist, colour=displine))+geom_point()
pd <- pd %>%  
  group_by(program) %>%
  mutate(.,tag= case_when(
    "good & tech"
    "bad % tech"
  ))
  
#barchart quadrents
#some measure of how much in each quadrent it is???? different from center????
#densitry of some sort?????


#try aggregating by skater to see if certain skaters fit into a box???
pd$tech_art_dif <- pd$tech_dist-pd$comp_dist
clust_plot<- pd %>% 
  filter(level=='senior'&displine=="DANCE") %>%
  select(tech_dist, tech_art_dif)
wss <- (nrow(clust_plot)-1)*sum(apply(clust_plot,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(clust_plot,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="total score & tech focus")


clust_plot$clusters <- kmeans(clust_plot,5)$cluster
between_SS / total_SS =  86.6 %
Cluster means:
  tech_dist tech_art_dif
1 0.6679222  -0.03502998
2 0.3510329  -0.14410358
3 0.4639019   0.10731891
4 0.1302714  -0.00949412
5 0.8812838   0.04071645
#increaing the cluster count adds about 2% over 5
#try clustering everyone then try breaking up into indivdual displines
ggplot(clust_plot, aes(tech_dist, tech_art_dif, colour=as.factor(clusters)))+
  geom_point()+scale_color_brewer(palette="Spectral")
clust_plot$eightclusters <- kmeans(clust_plot,8)$cluster
ggplot(clust_plot, aes(tech_dist, tech_art_dif, colour=as.factor(eightclusters)))+
  geom_point()+scale_color_brewer()
#use this code to look into what it breaks up
#when clustering it breaks up the bigger clusters, instead of drawing new boundreis
#could be interesting to look at what it considers are the best clusters
#or is probably not going to go anywhere really
ggplot(clust_plot, aes(eightclusters, clusters))+
  geom_bar(stat="identity",aes(fill=as.factor(clusters)) )+scale_fill_brewer(palette="Spectral")

#do these clusters look very different when we seperate into displines first???
#seperate into each disclpine, 5 clusters and compare the group means 
dat<-pd %>% filter(level=='senior') %>% group_by(displine)%>%
  do(as.data.frame(kmeans(c(.$total_dist,.$tech_art_dif),5)$centers[,1]))
#try swapping the columns to see if that will work????
dat2<-pd %>% filter(level=='senior') %>% group_by(displine)%>%
  do(as.data.frame(kmeans(c(.$tech_art_dif,.$total_dist),5)$centers[,1]))
#this is a bit of a hack as I cna only get the center for tech_dist
ggplot(dat, aes(x=dat[,2],y=dat2[,2],colour= displine))+geom_point()

#this is such a cheat but it does look like spliting into different displines does change the clusters

#merge the clusters back onto the main plot and see if different displines dominate a cluster
kmeans(pd[,c('tech_art_dif','total_dist')],5)
kmeans(pd[,c('tech_art_dif','total_dist')],5)$betweenss/kmeans(pd[,c('tech_art_dif','total_dist')],5)$totss
pd$clusters <- kmeans(pd[,c('tech_art_dif','total_dist')],5)$cluster
ggplot(pd, aes(clusters, fill=displine))+geom_bar()

ggplot(pd, aes(x = displine,
               fill=as.factor(clusters)))+geom_bar(position = "fill")+scale_fill_brewer(palette="Spectral")
#as we can see, ladies and men are practically idential, pairs is very similar and dance has some 
#differences but also bery similar
#this indicates(to me at least) that in each displine there are still certain archtypes of skaters
displine   1   2   3   4   5
DANCE   43  78 105 130  60
LADIES  81  85 106 117  95
MEN     81  78 109 126  94
PAIRS   50  60  85  87  56
displine          1          2          3          4          5
DANCE  0.02491309 0.04519119 0.06083430 0.07531866 0.03476246
LADIES 0.04692932 0.04924681 0.06141367 0.06778679 0.05504056
MEN    0.04692932 0.04519119 0.06315180 0.07300116 0.05446118
PAIRS  0.02896871 0.03476246 0.04924681 0.05040556 0.03244496

#possible plot- plot each cluster with colour as each displine, to see if they
#are evenly distrubeted in the cluster, but I feel this might be unessiary


#okay so that was how indivdaul performances were arranged, now lets see if we can cluster
#skaters

add the skater names to df
skater_clust <- left_join(pd, performances)
skater_clust <- skater_clust %>% 
  group_by(name) %>%
  avg_tech_dist

#create a percetage of score from tech colum and compare to total score
pd$tech_perc <- pd$total_element_score/pd$total_segment_score
ggplot(pd, aes(tech_perc, total_dist, colour = displine))+geom_point()
#nothing super informative
#you can imagine bad performances in ladies and men really bombing and thier tech being much lower
#although I do find it interesting that at the higher end tech really doesn't seem so much further 
#out than at lower levels
#eg you don't see higher scores really being dominated by tech, at least if this very crude
#graph shows us anything
#could try doing a histogram of some sorts, plotting for each bucket of total_dist
#what part came from tech
ggplot(subset(pd,level=="senior" && displine == "DANCE"), aes(tech_perc, total_dist))+
  geom_raster(fill="density")

ggplot(subset(pd, level=='senior'), aes(tech_perc, total_dist, colour = displine))+geom_point()

#to do, arragne around 0
#fins better distrunutions
#add lines and labels for the quadrents
#find clusters
#percentage of total score from components on a histogram


ggplot(subset(goe_cor_base, level=='senior'),
       aes(total_element_score-total_component_score, total_component_score-total_element_score, 
                                                  colour=displine))+geom_point()
wss <- (nrow(pd[,7:8])-1)*sum(apply(pd[,7:8],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(pd[,7:8],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
######ideas
#bucket the bv, to seperate out top teams

#see if jump goe/spin+step goe has more/less of an effect
# does the presence of a high bv element (quad?) is corelated with pcs
#


#lets seperate out the juniors once again


#(this is avgerage bv not total, try that)  
  
  
#(reputation judging??? is a skaters avg pcs more of a factor than their tes or goe)

#(maybe we cluster skaters )






#is this a function of how many falls there are

#falls should effect pcs, lets see if they do

#for each skater, is there a difference in pcs if there is a fall

#calaculate the average goe of a skater in programs without a fall, and the average 


ggplot(aes(cor, level, colour = displine), data=x2)+geom_point()

#do the line thing


#is the competition a useful predictor because of the qaulity of everyone else, or 
#the type of competition???? comp efffects?





gbmgbmFit1 <- train(total_component_score ~ total_element_score+total_deductions+
                   starting_number +competition, 
                 data = performances, 
                 method = "bayesglm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 na.action = na.pass)
RMSE      Rsquared   MAE    
5.774878  0.9012133  4.26206




[7] "JUNIOR MEN FREE SKATING"      "JUNIOR MEN SHORT PROGRAM"    
[9] "JUNIOR PAIRS FREE SKATING"    "JUNIOR PAIRS SHORT PROGRAM"  
[13] "MEN FREE SKATING"             "MEN SHORT PROGRAM"           
[15] "PAIRS FREE SKATING"           "PAIRS SHORT PROGRAM" 

#split into displines
dance <-filter(performances, program == "ICE DANCE FREE DANCE" |"ICE DANCE SHORT DANCE" )
dancefit1 <- train(total_component_score ~ total_element_score+total_deductions+
                   starting_number +competition, 
                 data = dance, 
                 method = "bayesglm", 
                 trControl = fitControl,
                 na.action = na.pass)
RMSE      Rsquared   MAE    
2.411314  0.9497613  1.88962

loess r-squared variable importance

Overall
total_element_score 100.000
starting_number       6.449
competition           5.662
total_deductions      0.000

dancefit2 <- train(total_component_score ~ total_deductions+
                     starting_number +competition, 
                   data = dance, 
                   method = "bayesglm", 
                   trControl = fitControl,
                   na.action = na.pass)
RMSE      Rsquared   MAE    
2.411314  0.9497613  1.88962

RMSE      Rsquared    MAE     
10.42624  0.06716825  9.456223

loess r-squared variable importance

Overall
starting_number   100.00
competition        87.79
total_deductions    0.00

dancefit3 <- train(total_component_score ~ total_deductions+name+
                     starting_number +competition, 
                   data = dance, 
                   method = "bayesglm", 
                   trControl = fitControl,
                   na.action = na.pass)

competition       100.00
total_deductions   87.47
starting_number    66.43
name                0.00
#does this indicate that a more importanct factor is comparing skaters next to each other
#not absolute value??????

#is there a class of skater (ie well known skaters) who this does not apply to??
d <- data.frame(label = levels(performances$program),stringsAsFactors = F)
#check this and change with ice dance and name changes                
d <-d %>% mutate(program = ifelse(str_detect(label,"FREE"), "long", "short"),
            displine = case_when(str_detect(label,"DANCE") ~ "DANCE",
                                 str_detect(label, "MEN") ~ "MEN",
                                 str_detect(label,"LADIES") ~ "LADIES",
                                 str_detect(label, "PAIRS") ~ "PAIRS"),
            level = ifelse(str_detect(label,"JUNIOR"),"junior", "senior"))

skaterpcs <- mutate(performances, 
                    displine = case_when(program %in% ~ "DANCE",
                                         str_detect("MEN",as.character(program)) ~ "MEN",
                                         str_detect("LADIES",as.character(program)) ~ "LADIES",
                                         str_detect("PAIRS",as.character(program)) ~ "PAIRS"))
skaterpcs <- performances %>% 
  group_by(name) %>%
  summarise( avg = mean(total_component_score), sd = sd(total_component_score), ) %>%
  filter(., !is.na(sd))

hist(skaterpcs$sd)
#add in starting groups

#now try filtering out only clean performances to compare apples to apples
senior <- filter(performances, "JUNIOR")

jdance <-filter(performances, program == "JUNIOR ICE DANCE FREE DANCE" |
                  "JUNIOR ICE DANCE SHORT DANCE" )
ladies <- filter(performances, program == "LADIES FREE SKATING" | "LADIES SHORT PROGRAM")
jladies <- filter(performances, program == "JUNIOR LADIES FREE SKATING" | "JUNIOR LADIES SHORT PROGRAM")