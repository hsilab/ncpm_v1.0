#### data: operator set
oper_set <- read.csv(file="./inst/extdata/cpmr_test.csv", header=FALSE)
#### Skill
skill <- "Novice"
# skill <- "Expert"

# data frame: working memory
a<-c()
b<-c()
c<-c()
d<-c()
e<-c()
f<-c()
g<-c()
h<-c()

wm_Box <- data.frame(a, b, c, d, e, f, g, h)

wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))

# colnames(wm_Box)[1]<-"chunk_num"
# colnames(wm_Box)[2]<-"chunk_name"
# colnames(wm_Box)[3]<-"stack_depth"
# colnames(wm_Box)[4]<-"pushed_time(Global)"
# colnames(wm_Box)[5]<-"elapsed_time(Local)"
# colnames(wm_Box)[6]<-"rehearsal"
# colnames(wm_Box)[7]<-"activation"
# colnames(wm_Box)[8]<-"prob_recall"
colnames(wm_Box)[1]<-"Chunk_Number"
colnames(wm_Box)[2]<-"Chunk_Name"
colnames(wm_Box)[3]<-"Stack_Depth"
colnames(wm_Box)[4]<-"Chunk_Arrival_Time"
colnames(wm_Box)[5]<-"Chunk_Elapsed_Time"
colnames(wm_Box)[6]<-"Rehearsal"
colnames(wm_Box)[7]<-"Activation"
colnames(wm_Box)[8]<-"Probability_of_Recall"
wm_Box

# data frame: chunk lifecyle
cl_1 <- c()
cl_2 <- c()
cl_3 <- c()
cl_4 <- c()
cl_5 <- c()

chunk_Lifecyle <- data.frame(cl_1, cl_2, cl_3, cl_4, cl_5)
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))

colnames(chunk_Lifecyle)[1]<-"Chunk_name"
colnames(chunk_Lifecyle)[2]<-"Time_pushed"
colnames(chunk_Lifecyle)[3]<-"Time_decayed"
colnames(chunk_Lifecyle)[4]<-"Time_end"
colnames(chunk_Lifecyle)[5]<-"Time_total"
chunk_Lifecyle

# data frame: number of operators
num_Oper <- c()
no_1 <- c()
no_2 <- c()
no_3 <- c()

num_Oper <- data.frame(no_1, no_2, no_3)
num_Oper <- rbind(num_Oper, c(0,0,0))

colnames(num_Oper)[1]<-"Perceptual"
colnames(num_Oper)[2]<-"Congitive"
colnames(num_Oper)[3]<-"Motor"

#' Reset all variables
#'
#' @return Empty wm_Box, chunk_Lifecycle, acc_Time_1, acc_Time_2
#' @export
#'
#' @examples
#' ResetAll()
ResetAll <- function() {
  # reset wm_Box
  for (i in 1:7) {
    for (j in 1:8) {
      wm_Box[i,j] <- 0
    }
    wm_Box[i,1] <- i # assigning chunk number in each row
    wm_Box[i,6] <- 3 # all rehearsals are reset as 3
  }
  # reset chunk_Lifecyle
  for (i in 1:nrow(chunk_Lifecyle)) {
    for (j in 1:ncol(chunk_Lifecyle)) {
      chunk_Lifecyle[i,j] <- 0
    }
  }
  acc_Time_1 <- 0
  acc_Time_2 <- 0

  reseted <- list(wm_Box, chunk_Lifecyle, acc_Time_1, acc_Time_2)
  return(reseted)
}

#' Reset number of operators
#'
#' @return Empty num_Oper
#' @export
#'
#' @examples
#' ResetNumOp()
ResetNumOp <- function() {
  num_Oper[1,1] <- 0
  num_Oper[1,2] <- 0
  num_Oper[1,3] <- 0
  return (num_Oper)
}

#' Set scenario
#'
#' @param user_scn If a user send a scenario (CSV), then add it into the model's inner variable
#'
#' @return task_1 The scenario file that will be used in the model
#' @export
#'
#' @examples
#' DefineScenario(csv)
DefineScenario <- function(user_scn) {
  task_1 <- user_scn
  return(task_1)
}
