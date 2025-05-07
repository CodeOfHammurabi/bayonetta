#musket calculator
#proudly written without chatgpt by krishna girish :)

library(stringr)
extra <- function(str){ #small wrapper to split by 4's
  a<-gsub("(.{4})", "\\1 ", str)
  return(a)}

qns <- read.delim(file.choose(),sep="\t") #load in raw tsv of questions
qns <- qns[,c(1,2,5)]
qns <- head(qns, -3) #remove spares

raw_urls <- read.csv(file.choose(), header=FALSE) #load in csv of wikiquiz URLs

raw_urlsa <- 'https://wikiquiz.org/Quiz_Scorer_App.html#/!/IQM/CQL/Zoom/Season%201/GW3/Adheesh%20Ghosh/Adheesh%20Ghosh/Adheesh%20Ghosh/4/Anshumani%20Ruddra/Aaran%20Mohan/Raunaq%20Vohra/Mikey%20Brown/////////////////////////////////////5/3/2023-08-06%2019:00//109/CCPCCPPPPPPPWCCCCPPPCPPCCPCPCCCCCCCCCPCCCPPPPPPPPCCCPPCPCCPPPPPPCPCCPPCCCCPCCPCPPPPCCPCCPPWPCPPWCCCCPPPWCPPCC'
raw_urls <- data.frame(raw_urlsa)

for(i in 1:nrow(raw_urls)){
  seq <- sub(".*/", "", raw_urls[i,])
  seq <- unlist(strsplit(seq, "(?<=[C])", perl=TRUE))
  qvec <- unlist(str_split((unname(sapply(seq, FUN=extra))), " "))
  qvec <- qvec[qvec!=""]
  qvec[grepl("PPPP", qvec)] <- "X"
  qvec[which(str_length(qvec)==4 & !grepl("C", qvec))] <- "X"
  qns <- as.data.frame(cbind(qns, qvec))}

#now we want to use this to track BA sequences over time
#this will allow us to find out who got each answer right
nrounds <- 5
whose_direct <- rep(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)), nrounds)

pts_scored <- as.data.frame(matrix(0, nrow=length(qvec), ncol=4))
colnames(pts_scored) <- paste0("p", c(1:4))
ba_cts <- as.data.frame(matrix(0, nrow=length(qvec),ncol=4))
colnames(ba_cts) <- paste0("p", c(1:4))

#function to get passing order from a row of BA counts
get_pass_order <- function(whosedirect, bacts){
  names(bacts) <- paste0('p', 1:4)
  current_bas <- bacts[-whosedirect]
  player_nums <- as.numeric(str_extract(names(current_bas), "\\d+"))
  cyclic_offset <- (player_nums - whosedirect) %% 4  # proper cyclic order
  sorted_names <- names(current_bas)[order(current_bas, cyclic_offset)]
  return(c(whosedirect, as.numeric(str_extract(sorted_names, "\\d+"))))
}

for(i in 1:length(qvec)){
  question_fate <- qvec[i]
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

final_total <- colSums(pts_scored)
ba_cts <- ba_cts[-nrow(ba_cts),] #occupational hazard - writes an extra row

print(final_total)
print(ba_cts[nrow(ba_cts),])

#who got it right?
who_conv <- c()
for(i in 1:nrow(pts_scored)){
  if(any(pts_scored[i,]==1)){
    who_conv <- c(who_conv, colnames(pts_scored)[which(pts_scored[i,]==1)])
  } else who_conv <- c(who_conv, "X")
}

converted <- as.data.frame(cbind(qns, who_conv))

#now, muskets
new_qns <-  read.delim(file.choose(),sep="\t") 
new_qns <- new_qns[1:60,]

seqs <- new_qns[,1] #these are quad orders
names(seqs) <- rep(1:15, each = 4)
group_map <- rep(1:15, each = 4) #what to fill in

group_assignment <- rep(0, nrow(converted))
group_assignment[seqs] <- group_map
converted$quad <- group_assignment

library(dplyr)
library(tidyr)

grouped <- split(converted$who_conv, converted$quad)
muskets_df <- data.frame(
  group = as.integer(names(grouped)),
  do.call(rbind, grouped),
  row.names = NULL
)
colnames(muskets_df) <- c('quad', paste0('q', 1:4))

as.data.frame(cbind(muskets_df, unique(new_qns$Quad)))







