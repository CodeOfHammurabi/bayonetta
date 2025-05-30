#musket calculator
#proudly written without chatgpt by krishna girish :)

library(stringr)
library(dplyr)
library(tidyr)

#module 0: read the questions tsv for the week
#this should include a quad_id column in the leftmost, answers, and a quad name category at the very least
new_qns <-  read.delim('/Users/krishnasg/Downloads/cql_s1_gw7.tsv',sep="\t") 
#remove spares
new_qns <- new_qns[-which(new_qns$roundNo=="Spare"),]

if(nrow(new_qns)>=60){
  new_qns <- new_qns[1:60,]
}

#module 1: load in URLs and get "question fates" for each question
urls_collection <- read.csv('/Users/krishnasg/Downloads/urls_sample_cql.csv', header=F)
#dataframe of all game scores
#urls_collection <- 'https://wikiquiz.org/Quiz_Scorer_App.html#/!/IQM/CQL/Zoom/Season%201/GW3/Adheesh%20Ghosh/Adheesh%20Ghosh/Adheesh%20Ghosh/4/Anshumani%20Ruddra/Aaran%20Mohan/Raunaq%20Vohra/Mikey%20Brown/////////////////////////////////////5/3/2023-08-06%2019:00//109/CCPCCPPPPPPPWCCCCPPPCPPCCPCPCCCCCCCCCPCCCPPPPPPPPCCCPPCPCCPPPPPPCPCCPPCCCCPCCPCPPPPCCPCCPPWPCPPWCCCCPPPWCPPCC'

#wrapper function to split strings
extra <- function(str){ 
  a <- gsub("(.{4})", "\\1 ", str)
  return(a)}

#names from url
name_extractor <- function(url){
  parts <- unlist(strsplit(url, "/"))
  idx_4 <- which(parts == "4")[1]
  names_vector <- parts[(idx_4 + 1):(idx_4 + 4)]
  names_vector <- URLdecode(names_vector)
  return(names_vector)
}

urls_collection <- as.data.frame(urls_collection)

question_fates <- list()
for(i in 1:nrow(urls_collection)){
  seq <- sub(".*/", "", urls_collection[i,])
  seq <- unlist(strsplit(seq, "(?<=[C])", perl=TRUE))
  qvec <- unlist(str_split((unname(sapply(seq, FUN=extra))), " "))
  qvec <- qvec[qvec!=""]
  #now need to sanitize input for weird readers who mark all guesses even on direct as wrong
  qvec <- gsub("^W", "P", qvec)
  qvec[grepl("PPPP", qvec)] <- "X" #generate X's

  #qvec[which(str_length(qvec)==4 & !grepl("C", qvec))] <- "X" don't do this because it ruins BA counts
  question_fates[[i]] <- qvec}

#now we want to use this to track BA sequences over time
#this will allow us to find out who got each answer right
nrounds <- 5
whose_direct <- rep(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)), nrounds)

temp_scores <- as.data.frame(matrix(0, nrow=length(qvec), ncol=4))
colnames(temp_scores) <- paste0("p", c(1:4))
temp_bas <- as.data.frame(matrix(0, nrow=length(qvec),ncol=4))
colnames(temp_bas) <- paste0("p", c(1:4))

game_scores <- rep(list(temp_scores), nrow(urls_collection))
ba_recs <- rep(list(temp_bas), nrow(urls_collection))

#function to get passing order from a row of BA counts
get_pass_order <- function(whosedirect, bacts){
  names(bacts) <- paste0('p', 1:4)
  current_bas <- bacts[-whosedirect]
  player_nums <- as.numeric(str_extract(names(current_bas), "\\d+"))
  cyclic_offset <- (player_nums - whosedirect) %% 4  # proper cyclic order
  sorted_names <- names(current_bas)[order(current_bas, cyclic_offset)]
  return(c(whosedirect, as.numeric(str_extract(sorted_names, "\\d+"))))
}

finalscores <- as.data.frame(matrix(0, nrow=nrow(urls_collection), ncol=5))
finalbas <- as.data.frame(matrix(0, nrow=nrow(urls_collection), ncol=5))

for(j in 1:length(game_scores)){
   current <- question_fates[[j]]
   pts_scored <- game_scores[[j]]
   ba_cts <- ba_recs[[j]]
   
   for(i in 1:length(current)){
     question_fate <- current[i]
     if(question_fate=="C"){
       pts_scored[i,whose_direct[i]] <- pts_scored[i,whose_direct[i]]+1 #1 point to direct, no BAs
       ba_cts[(i+1),] <- ba_cts[i,] #BAs remain static for the next question
     }
     if(question_fate=="X"){
       ba_cts[(i+1),] <- ba_cts[i,] #remains the same for the next question
     }
     if(nchar(question_fate)>1){ #this indicates some passing shenanigans have happened
       passorder <- get_pass_order(whose_direct[i], unlist(ba_cts[i,])) #read current BA counts 
       #split the sequence
       local_seq <- unlist(strsplit(question_fate, ''))
       if(any(local_seq=="W")){ #if there's any wrong answers
         wrongs <- which(local_seq=="W")
         ba_cts[i,passorder[wrongs]] <- ba_cts[i,passorder[wrongs]]+1
         ba_cts[(i+1),] <- ba_cts[i,]
       }
       if(any(local_seq=="C")){ #if someone got it right
         who_right <- passorder[which(local_seq=="C")] #there can be only one
         ba_cts[i, who_right] <- ba_cts[i, who_right]+1
         pts_scored[i, who_right] <- pts_scored[i, who_right]+1
         ba_cts[(i+1),] <- ba_cts[i,]
       }
     }
   }
   
   ba_cts <- ba_cts[-nrow(ba_cts),]
   game_scores[[j]] <- pts_scored
   ba_recs[[j]] <- ba_cts
   finalscores[j, ] <- c(j, colSums(pts_scored))
   finalbas[j,] <- c(j, unlist(ba_cts[nrow(ba_cts),]))
}

#great, this section is done - basically tells us who got what, and progression of BAs over time

B <- as.data.frame(cbind(1:nrow(game_scores[[1]]), apply(game_scores[[1]], 2, cumsum)))
colnames(B) <- c("qn", name_extractor(urls_collection[1,]))
B_long <- B %>%
  pivot_longer(cols = c(`Sarah Trevarthen`, `Samanth Subramanian`, `Benny Meyers`, `Achyuth Sanjay`),
               names_to = "player", values_to = "value")
ggplot(B_long, aes(x = qn, y = value, color = player)) +
  geom_line(size = 1) +
  labs(x = "Question Number", y = "Player") +
  theme_classic() +
  theme(legend.title = element_blank())

#who got it right?

conversions <- as.data.frame(matrix(nrow=nrow(urls_collection), ncol=nrow(pts_scored)+1))
colnames(conversions) <- c('game_no', paste0("q", c(1:60)))

for(j in 1:nrow(conversions)){
  pts <- game_scores[[j]]
  who_conv <- c()
  for(i in 1:nrow(pts)){
    if(any(pts[i,]==1)){
      who_conv <- c(who_conv, colnames(pts)[which(pts[i,]==1)])
    } else who_conv <- c(who_conv, "X")
  }
  conversions[j, ] <- c(j, who_conv)
}

#conversion stats on each question
id_direct <- paste0("p", whose_direct)

conversion_stats <- as.data.frame(matrix(nrow=60, ncol=3))
for(i in 2:ncol(conversions)){
  current_player_seq <- conversions[,i]
  overall_conversion <- length(which(current_player_seq!="X"))/length(current_player_seq)
  whose_direct <- id_direct[(i-1)]
  direct_conversion <- length(which(current_player_seq==whose_direct))/length(current_player_seq)
  conversion_stats[(i-1),] <- c((i-1), direct_conversion, overall_conversion)
}

colnames(conversion_stats) <- c("q", "direct_conv", "overall_conv")

conversion_stats$answer <- new_qns[order(new_qns$X),]$answerText
  
#now, we compute muskets and d'artagnans
quad_orders <- new_qns[,1] #quad orders!
names(quad_orders) <- rep(1:15, each = 4) #name vector to match
group_map <- rep(1:15, each = 4) #what to fill in
quadnames <- unique(new_qns$Quad)

all_muskets <- as.data.frame(matrix(nrow=0, ncol=3))
conversion_list <- list()
colnames(all_muskets) <- c("quad", "player", "count")
for(i in 1:nrow(conversions)){
  group_assignment <- rep(0, (length(conversions[i,])-1))
  group_assignment[quad_orders] <- group_map
  muscat <- group_assignment
  grouped <- split(unlist(conversions[i,-1]), as.factor(muscat))
  muskets_df <- data.frame(group = as.integer(names(grouped)), do.call(rbind, grouped),
    row.names = NULL)
  at_least_dart <- apply(muskets_df[,-1], 1, function(row) {tab <- table(row)
    tab[tab >= 3]})
  conversion_list[[i]] <- muskets_df
  clean <- which(sapply(at_least_dart, length) > 0)
  musket_or_dart <- lapply(clean, function(j) {
    list(quad = j, player = names(at_least_dart[[j]]), count = as.integer(at_least_dart[[j]]))
  })
  bigs_df <- do.call("rbind", musket_or_dart)
  if(!is.null(bigs_df)){
    bigs_df <- as.data.frame(cbind(game_id = rep(i, nrow(bigs_df)), bigs_df))
  }
  all_muskets <- rbind(all_muskets, bigs_df)
}

all_muskets <- subset(all_muskets, all_muskets$player!="X")

#replace these with names of players who got them, and quad names
for(i in 1:nrow(all_muskets)){
  current_game <- unlist(all_muskets[i,1])
  player_who_got <- as.numeric(as.numeric(gsub("\\D", "", all_muskets[i,3])))
  all_muskets[i,3] <- name_extractor(urls_collection[current_game,])[player_who_got]
  #now, which quad
  all_muskets[i,2] <- quadnames[unlist(all_muskets[i,2])]
}

#owns per seat and X's per seat
owns_seat <- as.data.frame(matrix(0, nrow=nrow(conversions), ncol=5))
xs_seat <- as.data.frame(matrix(0, nrow=nrow(conversions), ncol=5))
for(i in 1:nrow(conversions)){
  sequence_gets <- unlist(conversions[i,-1])
  match_mat <- as.data.frame(cbind(id_direct, sequence_gets))
  colnames(match_mat) <- c("who_direct", "who_got")
  seat1_owns <- length(which(match_mat$who_direct=="p1" & match_mat$who_got=="p1"))
  seat2_owns <- length(which(match_mat$who_direct=="p2" & match_mat$who_got=="p2"))
  seat3_owns <- length(which(match_mat$who_direct=="p3" & match_mat$who_got=="p3"))
  seat4_owns <- length(which(match_mat$who_direct=="p4" & match_mat$who_got=="p4"))
  seat1_x <- length(which(match_mat$who_direct=="p1" & match_mat$who_got=="X"))
  seat2_x <- length(which(match_mat$who_direct=="p2" & match_mat$who_got=="X"))
  seat3_x <- length(which(match_mat$who_direct=="p3" & match_mat$who_got=="X"))
  seat4_x <- length(which(match_mat$who_direct=="p4" & match_mat$who_got=="X"))
  owns_seat[i,] <- c(i, seat1_owns, seat2_owns, seat3_owns, seat4_owns)
  xs_seat[i,] <- c(i, seat1_x, seat2_x, seat3_x, seat4_x)
}

avg_owns <- colMeans(owns_seat[,-1])
names(avg_owns) <- c("seat1_owns", "seat2_owns", "seat3_owns", "seat4_owns")
avg_xs <- colMeans(xs_seat[,-1])
names(avg_xs) <- c('seat1_xs', 'seat2_xs', 'seat3_xs', 'seat4_xs')

#top owns per seat - who did best?
most_owns <- apply(owns_seat[,-1], 2, max)
whos_most_owns <- apply(owns_seat[,-1], 2, which.max)

names_tops <- c()
for(i in 1:4){
  game_number <- whos_most_owns[i]
  names_tops <- c(names_tops, name_extractor(urls_collection[game_number,])[i])
}

week_toppers <- as.data.frame(cbind(names_tops, most_owns))

#congrats-all
congrats_all <- which(conversion_stats$overall_conv==0)
congrats_ans <- new_qns$answerText[which(new_qns$X %in% congrats_all)]
#congrats-none
congrats_none <- which(conversion_stats$overall_conv==1)
congrats_none_ans <- new_qns$answerText[which(new_qns$X %in% congrats_none)]

#nohitters
nohits <- which((owns_seat[,-1]+xs_seat[,-1])==15, arr.ind=T)
nohitters <- c()
for(i in 1:nrow(nohits)){
  nohitters <- c(nohitters, name_extractor(urls_collection[unlist(nohits[i,1]),])[nohits[i,2]])
}

#jack of all quads - reconstruct muskets_df for each game
jacks <- c()
for(i in 1:length(conversion_list)){
  current <- conversion_list[[i]]
  for(j in 1:4){
    if(all(apply(current, 1, function(x) any(x %in% paste0("p", j))))==T){
      jacks <- c(jacks, name_extractor(urls_collection[i,])[j])
    }}}

#hail-maries: when did we have 3 passes or wrong and then one person swooping in to save an X

hailmaries_all <- as.data.frame(matrix(nrow=0, ncol=3))
colnames(hailmaries_all) <- c("game", "answer", "person")
for(i in 1:nrow(urls_collection)){
  fates <- question_fates[[i]]
  saved <- which(nchar(fates)==4 & substr(fates, 4, 4)=="C") 
  who_saved <- apply(game_scores[[i]][saved,], 1, function(x) which(x==1))
  hailmaries <- as.data.frame(cbind(rep(i, times=length(saved)), new_qns[new_qns$X %in% saved,]$answerText, name_extractor(urls_collection[i,])[who_saved]))
  colnames(hailmaries) <- colnames(hailmaries_all)
  hailmaries_all <- rbind(hailmaries_all, hailmaries)
}

table(hailmaries_all$person)

#quad difficulty

quads_conv <- as.data.frame(matrix(nrow=nrow(urls_collection), ncol=15))
for(i in 1:length(conversion_list)){
  current <- conversion_list[[i]][,-1]
  quads_conv[i,] <- apply(current, 1, function(x) length(which(x!="X")))
}

quad_conversions <- as.data.frame(cbind(quadnames, colMeans(quads_conv)))

#how many answered on direct?
#annoying to compute given what we have, actually
#but let's see

quads_owns_conv <- as.data.frame(matrix(nrow=nrow(urls_collection), ncol=15))
for(i in 1:nrow(conversions)){
  musket_quad <- as.data.frame(cbind(muscat, unlist(conversions[i,-1]), id_direct))
  musket_quad <- musket_quad[order(muscat),]
  musket_quad$muscat <- as.numeric(musket_quad$muscat)
  musket_out <- musket_quad %>%
    group_by(muscat) %>%
    summarise(count_equal = sum(V2 == id_direct, na.rm = TRUE))
  quads_owns_conv[i,] <- musket_out$count_equal
}

quad_owns_conversions <- as.data.frame(cbind(quadnames, colMeans(quads_owns_conv)))

#quad equity - how many distinct players answered anything in a quad
#will do later- hard to interpret

#JUDE: obscurity score

jude_all <- as.data.frame(matrix(nrow=0, ncol=2))
auden_all <- as.data.frame(matrix(nrow=0, ncol=2))
for(i in 1:length(conversion_list)){
  whomst <- game_scores[[i]]
  for(j in 1:4){
    converted_which <- which(whomst[,j]==1)
    who_dis <- name_extractor(urls_collection[i,])[j]
    jude <- sum(1-conversion_stats[converted_which,3])
    jude_all <- rbind(jude_all, c(who_dis, jude))
    auden <- jude/length(converted_which) 
    auden_all <- rbind(auden_all, c(who_dis, auden))
  }
}
colnames(jude_all) <- c("name", "jude")
colnames(auden_all) <- c('name', 'auden')

#average unanswered deficit extent, normalized (AUDEN)
#more questions answered, more score


##############################################
#things to compute-
#1. muskets (DONE)
#2. d'artagnans in topics (DONE)
#3. jack of all quads (DONE)
#4. conversion rates per question (DONE)
#5. JUDE (joint unanswered deficit extent) - based on conversion stats (DONE)
#6. owns per seat, X's per seat (DONE)
#7. top scorers per seat (DONE)
#8. quad difficulty (DONE) & quad equity - which quads were most scattered across all 4 players (DONE)
#9. topic-wise conversion stats (meh)
#10. score progression per game? see ggplot of B (meh)
#11. any no-hitters? (DONE)
#12. congrats-alls (DONE)
#13. hail-maries (DONE)
#other stats?

#future features-
#option to reconstruct muskets from a single URL
#and native reading of format - # of players, # of rounds ,etc







