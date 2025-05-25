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
  print(names_vector)
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

#now, we compute muskets and d'artagnans

quad_orders <- new_qns[,1] #quad orders!

#name the vector so we can match
names(quad_orders) <- rep(1:15, each = 4)
group_map <- rep(1:15, each = 4) #what to fill in

group_assignment <- rep(0, nrow(conversions[i,]))
group_assignment[quad_orders] <- group_map
muscat <- group_assignment
quadnames <- unique(new_qns$Quad)

grouped <- split(unlist(conversions[1,-1]), as.factor(muscat)) #per-quad qns & conversions

muskets_df <- data.frame(
  group = as.integer(names(grouped)),
  do.call(rbind, grouped),
  row.names = NULL
)
colnames(muskets_df) <- c('quad', paste0('q', 1:4))

muskets_df <- as.data.frame(cbind(muskets_df[,-1], quadnames))







