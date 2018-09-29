library(R6)

Agent <- R6Class("Agent",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   greeting = "Hi!",
                   id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   response = NULL,
                   
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$ response <-response 
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   # function to test whether opponent uses tit-tat-strategy
                   # if yes, return TRUE
                   # else returns FALSE
                   is_tit_tat = function() {
                     t = self$book
                     
                     rec1 = t[(t$id1 == self$opponent_id),]  # all rounds opponent played as id1
                     rec2 = t[(t$id2 == self$opponent_id),]   # same but opponent as id2
                     
                     names(rec1) = c("round", "op", "opop", "bid_op", "bid_opop")  # rec1 contains round no., opponent id, opponent's opponent's id, opponent's bid, and opponent's opponent's bid
                     names(rec2) = c("round", "opop", "op", "bid_opop", "bid_op")
                     rec2 = rec2[,c("round", "op", "opop", "bid_op", "bid_opop")]  # change order of coloumns so that it is the same as in rec1
                     all_rec = rbind(rec1,rec2)       # contains all encounters from opponent (op) with other agents (opop)
                     
                     # minimum number of encounters before we consider whether tit-tat-stratey is used (?)
                     # if it is below, return NULL
                     if (nrow(all_rec) < 5) {
                       return(NULL)
                     } else {
                       
                       count = 0  # to count the numbers of rows when op's next bid is equal to opop's previous bid
                       # check all encounters one by one 
					   # could probably be done easier using nrow()...
                       for (i in 1:length(all_rec)) {
                         if (all_rec[i+1,"bid_op"] == all_rec[i, "bid_opop"]) {
                           count = count + 1
                         }
                       }
                       
                       # if number of bids based on previous step is equal to total number of encounters, it means opponent uses tit-tat-strategy
                       if (count == max(all_rec$round)) {
                         return (TRUE)
                       }
                       else {
                         return (FALSE) 
                       }
                     }
                     
                   },
                   
                   
                   get_bid = function() {
                     if(self$response == "Lemon!"){
                       self$bid = "defect"
                     } else {
                       bid_vector <- c("cooperate","defect")
                       self$bid <- sample(bid_vector,1)
                     }
                     # if more than 700 rounds always "defect"
                     if (self$round > 700) {
                       self$bid = "defect"
                     }
                   }
                 )
)

book = read.table("data/tournament.csv",header=TRUE,sep=",")
first = Agent$new()
first$response
first$set_round(2)
first$round
first$set_book(book)
first$set_response("Lemon!")
first$get_bid()
first$bid
first$set_id(5)

# test is_tit_tat_function
for (i in 1:24) {
  first$set_opponent_id(i)
  print(first$is_tit_tat())
}


last_interact = function() {   #interaction between two agents self$id, self$opponent_id
  t = self$book  
  
  rec1 = t[(t$id1 == self$id & t$id2 == self$opponent_id), c("round","bid2")]  #rec1 is a vector that contains all the lines that (t$id1 == self$id & t$id2 == self$opponent_id) == TRUE, but has only the "round" and "bid2" columns
  print(rec1)
  rec2 = t[(t$id1 == self$opponent_id & t$id2 == self$id), c("round","bid1")]
  
  names(rec1) = c("round","bid")   # change names of variables because otherwise there is problem when you will try to combine the two vectors in one
  names(rec2) = c("round","bid")
  all_rec = rbind(rec1,rec2)       # this vector contains all the encounters of agents self$id, self$opponent_id
  
  if (nrow(all_rec) > 0) {         # if the two agents have met in the past
    round = all_rec$round          # a vector of all the rounds
    n = max(round)                 # the largest number in this vector is the last time the two agents met
    last = all_rec[all_rec$round == n,"bid"]
  } else{                          # if the agents haven't met yet
    last = NULL
  }
  return(last)
}

# gives all interactions between two agents self$id, self$opponent_id
# could be combined with last_interact...
all_interactions = function() {   
  t = self$book  
  
  rec1 = t[(t$id1 == self$id & t$id2 == self$opponent_id), c("round","bid2")]  #rec1 is a vector that contains all the lines that (t$id1 == self$id & t$id2 == self$opponent_id) == TRUE, but has only the "round" and "bid2" columns
  rec2 = t[(t$id1 == self$opponent_id & t$id2 == self$id), c("round","bid1")]
  
  names(rec1) = c("round","bid")   # change names of variables because otherwise there is problem when you will try to combine the two vectors in one
  names(rec2) = c("round","bid")
  all_rec = rbind(rec1,rec2)       # this vector contains all the encounters of agents self$id, self$opponent_id
  
  if (nrow(all_rec) > 0) {         # if the two agents have met in the past
    round = all_rec$round          # a vector of all the rounds
    interactions = all_rec
  } else{                          # if the agents haven't met yet
    interactions = NULL
  }
  return(interactions)
}

# gives all the past actions from opponent 
opponent_history = function() {
  t = self$book
  
  rec1 = t[(t$id1 == self$opponent_id), c("round", "bid1")]  # table with all rounds opponent was involved as id1: round no., bid1 
  rec2 = t[(t$id2 == self$opponent_id), c("round", "bid2")]   # same but opponent as id2
  
  names(rec1) = c("round","bid")   # change names of variables because otherwise there is problem when you will try to combine the two vectors in one
  names(rec2) = c("round","bid")
  all_rec = rbind(rec1,rec2)       # contains all the past actions from opponent and round number
  
  if (nrow(all_rec) > 0) {
    return(all_rec)
  } else {
    return(NULL)
  }
}


