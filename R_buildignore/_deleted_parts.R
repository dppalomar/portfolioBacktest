# # compute ROT based on absolute dollars
# PnL <- diff(NAV); colnames(PnL) <- "PnL"
# this is wrong: delta_abs <- diff(as.vector(NAV) * w)
# delta_abs[-rebalance_indices, ] <- 0  # only keep the rebalanced ones
# turnover_abs <- xts(rowSums(abs(delta_abs)), index(delta_abs))
# ROT_bips <- sum(PnL, na.rm = TRUE)/sum(turnover_abs, na.rm = TRUE)*1e4

