#' @title Create commutation table
#'
#' Returns the commutation table for life insurance.(Overview)
#'
#' @param qx vectors (qx is mortality)
#' @param i scalar (i is interest rate)
#'
#' @return Dx,Nx,Cx,Mx in commutation table
#'
#' @examples
#' qx <- c(0.00081,0.00056,0.00038)
#' i <- 0.05
#'

cal_comm <- function(qx,i){

  # 現価率の計算------------------------------
  v <- 1/(1+i)

  # 生存数を計算------------------------------
  small_px <-1-qx
  small_lx <- 100000 * cumprod(c(1, small_px))

  # 死亡数を計算------------------------------
  small_dx <- small_lx - c(small_lx,0)[2:(length(small_lx)+1)]

  # 計算基数を算出----------------------------
  large_dx <<- v^(0:length(small_px))*small_lx
  large_nx <<- rev(cumsum(rev(large_dx)))
  large_cx <<- v^(0:length(small_px)+0.5)*small_dx
  large_mx <<- rev(cumsum(rev(large_cx)))

  #return(list(large_dx,large_nx,large_cx,large_mx))
  comm_table <- data.frame(Dx = large_dx, Nx = large_nx, Cx = large_cx, Mx = large_mx)
  comm_table
}
