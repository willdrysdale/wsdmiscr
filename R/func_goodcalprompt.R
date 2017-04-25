#' Good cal Prompt
#' 
#' Prompt used to assertain where cals are good or bad, reuires user input
#' 
#' @param Raw AQD NOx output


good_cal_prompt = function(d){
  i = readline(prompt = paste("Was the cal between, ",as.character(d$start)," and ",as.character(d$end)," good? [y/n] : ",sep = ""))
  if (i == "y" | i == "n"){
    if (i == "y")
      d$good = 1
    else{
      if (i == "n"){
        d$good = 0
      }
    }
  }else
    good_cal_prompt(d)
}