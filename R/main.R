#' RunMain()
#'
#' @description Run main function
#'
#' @param scenario Scenario in CSV format
#' @param skill Novice or Expert
#'
#' @return
#' @export
#'
#' @examples
#' RunMain(csv file, "Novice")
RunMain <- function(scenario, time_library, skill) {
  Reseted <- ResetAll()
  wm_Box <- Reseted[[1]]
  chunk_Lifecyle <- Reseted[[2]]
  acc_Time_1 <- Reseted[[3]]
  acc_Time_2 <- Reseted[[4]]
  num_Oper <- ResetNumOp()

  for (i in 2:nrow(scenario)) {
    oper_Name <- ExtOper(scenario[i, 1])
    oper_Time <- RetrievingOperTime(oper_Name, i, scenario, skill, time_library)
    acc_Time_1 <- acc_Time_1 + as.numeric(oper_Time)

    num_of_chunks <- MultipleChunks(oper_Time, oper_Name, scenario[i, 1])
    chunk_process <- ExtChunk(scenario[i, 1], oper_Name, acc_Time_1, acc_Time_2, wm_Box, oper_Time, num_of_chunks, chunk_Lifecyle)
    wm_Box <- chunk_process[[1]]
    chunk_Lifecyle <- chunk_process[[2]]
    acc_Time_2 <- acc_Time_1

    num_Oper <- GetNumOper(oper_Name, num_Oper, oper_Time, skill)
  }

  TCT <- round(acc_Time_1/1000, 2)

  MC <- GetMemoryChunk(chunk_Lifecyle, acc_Time_1)

  final_R <- list(wm_Box, TCT, MC[[1]], round(MC[[2]], 2), num_Oper)
  return(final_R)
}

#' WM_status()
#'
#' @description Return calculated working memory storage
#' @return
#' @export
#'
#' @examples
#' WM_status()
WM_status <- function() {
  return(wm_Box)
}
