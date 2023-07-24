# loading packages
library(ggplot2)


ICER_fxn <- function(death_gvn_overtreat_prob, death_gvn_undertreat_prob) {
  
  
  # defining all expected costs and all expected DALYs
  # WHO prob
  branch_a <- 0.06812325
  branch_b <- 0.23498925
  branch_c <- 0.0393875
  branch_d <- 0.05125032
  branch_e <- 0.30279966
  branch_f <- 0.07735002
  branch_g <- 0.00988057
  branch_h <- 0.13161281
  branch_i <- 0.08460662
  
  # NIRUDAK
  branch_j <- 0.10285635
  branch_k <- 0.28938165
  branch_l <- 0.032262
  branch_m <- 0.02196501
  branch_n <- 0.21924172
  branch_o <- 0.05049327
  branch_p <- 0.0046827
  branch_q <- 0.16037538
  branch_r <- 0.1187703
  
  # WHO costs
  branch_a_cost <- 97.04
  branch_b_cost <-100.36
  branch_c_cost <-102.37
  branch_d_cost <-27.91
  branch_e_cost <-26.65
  branch_f_cost <-17.19
  branch_g_cost <-9.92
  branch_h_cost <-12.87
  branch_i_cost <-14.37
  
  # NIRUDAK costs
  branch_j_cost <-96.82
  branch_k_cost <-93.91
  branch_l_cost <-96.02
  branch_m_cost <-26.09
  branch_n_cost <-27.27
  branch_o_cost <-28.92
  branch_p_cost <-16.53
  branch_q_cost <-14.17
  branch_r_cost <-6.88
  
  # listing mean DALYs lost conditional on dying; doesn't vary w/ sensitivity analysis
  # WHO
  # death from overtreatment
  mean_DALYs_lost_if_die_branch_b <- 44.97
  mean_DALYs_lost_if_die_branch_c <- 35.09
  # death from undertreatment
  mean_DALYs_lost_if_die_branch_d <- 58.99
  mean_DALYs_lost_if_die_branch_g <- 64.02
  
  # NIRUDAK
  # death from overtreatment
  mean_DALYs_lost_if_die_branch_k <- 52.41
  mean_DALYs_lost_if_die_branch_l <- 49.18
  # death from undertreatment
  mean_DALYs_lost_if_die_branch_m <- 48.32
  mean_DALYs_lost_if_die_branch_p <- 49.92
  
  # expected DALYs lost conditional on making it to that branch endpoint
  # WHO
  branch_a_DALY <- 0
  branch_b_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_b
  branch_c_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_c
  branch_d_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_d
  branch_e_DALY <- 0
  branch_f_DALY <- 0
  branch_g_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_g
  branch_h_DALY <- 0
  branch_i_DALY <- 0
  
  # NIRUDAK
  branch_j_DALY <- 0
  branch_k_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_k
  branch_l_DALY <- death_gvn_overtreat_prob*mean_DALYs_lost_if_die_branch_l
  branch_m_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_m
  branch_n_DALY <- 0
  branch_o_DALY <- 0
  branch_p_DALY <- death_gvn_undertreat_prob*mean_DALYs_lost_if_die_branch_p
  branch_q_DALY <- 0
  branch_r_DALY <- 0
  
  # calculating expected costs
  # WHO
  branch_a_expected_cost <- branch_a_cost*branch_a
  branch_b_expected_cost <- branch_b_cost*branch_b
  branch_c_expected_cost <- branch_c_cost*branch_c
  branch_d_expected_cost <- branch_d_cost*branch_d
  branch_e_expected_cost <- branch_e_cost*branch_e
  branch_f_expected_cost <- branch_f_cost*branch_f
  branch_g_expected_cost <- branch_g_cost*branch_g
  branch_h_expected_cost <- branch_h_cost*branch_h
  branch_i_expected_cost <- branch_i_cost*branch_i
  
  #NIRUDAK
  branch_j_expected_cost <- branch_j_cost*branch_j
  branch_k_expected_cost <- branch_k_cost*branch_k
  branch_l_expected_cost <- branch_l_cost*branch_l
  branch_m_expected_cost <- branch_m_cost*branch_m
  branch_n_expected_cost <- branch_n_cost*branch_n
  branch_o_expected_cost <- branch_o_cost*branch_o
  branch_p_expected_cost <- branch_p_cost*branch_p
  branch_q_expected_cost <- branch_q_cost*branch_q
  branch_r_expected_cost <- branch_r_cost*branch_r
  
  # calculating expected DALYs
  # WHO
  branch_a_expected_DALY <- branch_a_DALY*branch_a
  branch_b_expected_DALY <- branch_b_DALY*branch_b
  branch_c_expected_DALY <- branch_c_DALY*branch_c
  branch_d_expected_DALY <- branch_d_DALY*branch_d
  branch_e_expected_DALY <- branch_e_DALY*branch_e
  branch_f_expected_DALY <- branch_f_DALY*branch_f
  branch_g_expected_DALY <- branch_g_DALY*branch_g
  branch_h_expected_DALY <- branch_h_DALY*branch_h
  branch_i_expected_DALY <- branch_i_DALY*branch_i
  
  # NIRUDAK
  branch_j_expected_DALY <- branch_j_DALY*branch_j
  branch_k_expected_DALY <- branch_k_DALY*branch_k
  branch_l_expected_DALY <- branch_l_DALY*branch_l
  branch_m_expected_DALY <- branch_m_DALY*branch_m
  branch_n_expected_DALY <- branch_n_DALY*branch_n
  branch_o_expected_DALY <- branch_o_DALY*branch_o
  branch_p_expected_DALY <- branch_p_DALY*branch_p
  branch_q_expected_DALY <- branch_q_DALY*branch_q
  branch_r_expected_DALY <- branch_r_DALY*branch_r
  
  # calculating ICER
  # total expected costs
  WHO_total_expected_cost <- sum(branch_a_expected_cost, branch_b_expected_cost, branch_c_expected_cost, branch_d_expected_cost,
                                 branch_e_expected_cost, branch_f_expected_cost, branch_g_expected_cost, branch_h_expected_cost,
                                 branch_i_expected_cost)
  
  NIRUDAK_total_expected_cost <- sum(branch_j_expected_cost, branch_k_expected_cost, branch_l_expected_cost, branch_m_expected_cost,
                                     branch_n_expected_cost, branch_o_expected_cost, branch_p_expected_cost, branch_q_expected_cost,
                                     branch_r_expected_cost)
  
  # total expected DALYs
  WHO_total_expected_DALYs <- sum(branch_a_expected_DALY, branch_b_expected_DALY, branch_c_expected_DALY, branch_d_expected_DALY,
                                  branch_e_expected_DALY, branch_f_expected_DALY, branch_g_expected_DALY, branch_h_expected_DALY,
                                  branch_i_expected_DALY)
  
  NIRUDAK_total_expected_DALYs <-sum(branch_j_expected_DALY, branch_k_expected_DALY, branch_l_expected_DALY, branch_m_expected_DALY,
                                     branch_n_expected_DALY, branch_o_expected_DALY, branch_p_expected_DALY, branch_q_expected_DALY,
                                     branch_r_expected_DALY)
  # incremental costs and DALYs
  incremental_costs <- NIRUDAK_total_expected_cost-WHO_total_expected_cost
  incremental_DALYs <-NIRUDAK_total_expected_DALYs-WHO_total_expected_DALYs
  # negative sign preserves usual interpretation of ICER
  incremental_DALYs_averted <- -incremental_DALYs
  
  # ICER
  ICER = (incremental_costs)/(incremental_DALYs_averted)
  
  results_vec <- c(ICER, incremental_costs, incremental_DALYs_averted, incremental_DALYs, NIRUDAK_total_expected_cost, WHO_total_expected_cost, NIRUDAK_total_expected_DALYs, WHO_total_expected_DALYs)
  
  names(results_vec) <- c("ICER", "Incremental Costs", "Incremental DALYs Averted", "Incremental DALYs", "NIRUDAK Total Expected Costs", "WHO Total Expected Costs", "NIRUDAK Total Expected DALYs", "WHO Total Expected DALYs")
  
  return(results_vec)
  
}
# end of function

# not sure what we are using this for? ostensibly to input into the for loop
# results_iteration1 <- ICER_fxn(death_gvn_overtreat_prob = 0.02, death_gvn_undertreat_prob = 1)

# varying the probability of death from UNDERtreatment
prob_die_gvn_undertreat_grid <- seq(0, 1, 0.01)

# varying the probability of death from OVERtreatment
prob_die_gvn_overtreat_grid <- seq(0, 1, 0.01)

# UNDERtreat: creating empty matrix to store results (M)
ncol_used <- length(ICER_fxn(0, 1))
M_col_names <- names(ICER_fxn(0,1))
M <- matrix(NA, nrow=length(prob_die_gvn_undertreat_grid), ncol = ncol_used)
colnames(M) <- M_col_names

# OVERtreat: creating empty matrix to store results (M_overtreat)
M_overtreat <- matrix(NA, nrow=length(prob_die_gvn_overtreat_grid), ncol = ncol_used)
colnames(M_overtreat) <- M_col_names

# base case probabilities
death_gvn_overtreat_prob <- 0.02
death_gvn_undertreat_prob <- 1

# UNDERTREATMENT: creating loop (filling the matrix)
for (i in 1:length(prob_die_gvn_undertreat_grid)){
  M[i, ] = ICER_fxn(death_gvn_overtreat_prob = 0.02, death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])
}

# if the probability of death from undertreatment is greater than or equal to 0.05, NIRUDAK is more costly but more effective

# for a probability of death from undertreatment less than 0.05, NIRUDAK is dominated by WHO (i.e., NIRUDAK is more expensive
# but less effective)

# all of the above is conditional on keeping the probability of death from overtreatment constant at 0.02

# finding the threshold at which ICER changes from positive to negative (i.e., dominance changes)
nondominated_undertreat_index <- which(M[,"ICER"] > 0)

# generating plot
# y axis: ICER (first column of the matrix)
# x axis: varied probabilities of due to undertreatment
plot(prob_die_gvn_undertreat_grid[nondominated_undertreat_index], M[nondominated_undertreat_index, 1], 
     xlim = c(0,1), main = "One-Way Sensitivity: Prob. Death Given Undertreament
     Probability of Death Given Overtreatment = 0.02",
     pch=20, xlab = "Probability of Death Given Undertreatment", ylab = "Cost per Incremental DALY Averted (ICER)")

# minimum value for which NIRUDAK is not dominated; for any probabilities to the left, NIRUDAK is dominated
abline(v=prob_die_gvn_undertreat_grid[min(nondominated_undertreat_index)], lty = 1, col="red")


# NEW PLOT UNDERTREATMENT, Y-AXIS IS INCREMENTAL DALYs AVERTED (by NIRUDAK)
plot(prob_die_gvn_undertreat_grid, M[, "Incremental DALYs Averted"], 
     xlim = c(0,1), main = "One-Way Sensitivity of Prob. Death Given Undertreament
     Probability of Death Given Overtreatment = 0.02",
     pch=20, xlab = "Probability of Death Given Undertreatment", ylab = "Incremental DALYs Averted")

# minimum value for which NIRUDAK is not dominated; for any probabilities to the left, NIRUDAK is dominated
abline(v=prob_die_gvn_undertreat_grid[min(nondominated_undertreat_index)], lty = 1, col="red")

################################################################################
# OVERTREATMENT: creating loop (filling the matrix)
for (i in 1:length(prob_die_gvn_overtreat_grid)){
  M_overtreat[i, ] = ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[i], death_gvn_undertreat_prob = 1)
}

nondominated_overtreat_index <- which(M_overtreat[,"ICER"] > 0)

# generating plot
# y axis: ICER (first column of the matrix)
# x axis: varied probabilities of due to undertreatment
# dev.new()
plot(prob_die_gvn_overtreat_grid[nondominated_overtreat_index], M_overtreat[nondominated_overtreat_index, 1], 
     xlim = c(0,1), main = "One-Way Sensitivity: Prob. Death Given Overtreament
     Probability of Death Given Undertreatment = 1",
     pch=20, xlab = "Probability of Death Given Overtreatment", ylab = "Cost per Incremental DALY Averted (ICER)")

# minimum value for which NIRUDAK is not dominated; for any probabilities to the left, NIRUDAK is dominated
abline(v=prob_die_gvn_overtreat_grid[max(nondominated_overtreat_index)], lty = 1, col="red")

# NIRUDAK never dominates because it's more expensive
# but for most of the probability range, it is below the WTP threshold

# in the undertreatment graph, to the left of the red line, WHO dominates
# in the overtreatment graph, to the right of the red line, WHO dominates

# NEW PLOT OVERTREATMENT, Y-AXIS IS INCREMENTAL DALYs AVERTED (by NIRUDAK)
plot(prob_die_gvn_overtreat_grid, M_overtreat[, "Incremental DALYs Averted"], 
     xlim = c(0,1), main = "One-Way Sensitivity of Prob. Death Given Overtreament
     Probability of Death Given Undertreatment = 1",
     pch=20, xlab = "Probability of Death Given Overtreatment", ylab = "Incremental DALYs Averted")

# minimum value for which NIRUDAK is not dominated; for any probabilities to the left, NIRUDAK is dominated
abline(v=prob_die_gvn_overtreat_grid[max(nondominated_overtreat_index)], lty = 1, col="red")

################################################################################
# TWO WAY SENSITIVITY ANALYSIS (probability of death from overtreatment AND from undertreatment)

M_twoway_ICER <- M_twoway_inc_costs <- M_twoway_inc_DALYs_averted <-  M_twoway_WHO_costs <- 
  M_twoway_WHO_DALYs_averted <-
  M_twoway_NIRUDAK_costs <- 
  M_twoway_NIRUDAK_DALYs_averted <- matrix(data=NA, 
                                           nrow = length(prob_die_gvn_undertreat_grid),
                                           ncol = length(prob_die_gvn_overtreat_grid))

colnames(M_twoway_ICER) <- colnames(M_twoway_inc_costs) <- 
  colnames(M_twoway_WHO_costs) <- colnames(M_twoway_WHO_DALYs_averted) <- colnames(M_twoway_NIRUDAK_costs)<-
  colnames(M_twoway_NIRUDAK_DALYs_averted)<-colnames(M_twoway_inc_DALYs_averted) <-
  paste("prdieOvr=", prob_die_gvn_overtreat_grid, sep = "")

rownames(M_twoway_ICER) <- rownames(M_twoway_inc_costs) <- 
  rownames(M_twoway_WHO_costs) <- rownames(M_twoway_WHO_DALYs_averted) <-  rownames(M_twoway_NIRUDAK_costs)<-
  rownames(M_twoway_NIRUDAK_DALYs_averted) <-
  rownames(M_twoway_inc_DALYs_averted) <- paste("prdieUndr=", prob_die_gvn_undertreat_grid, sep = "")

for (i in 1:length(prob_die_gvn_undertreat_grid)) {
  for (j in 1:length(prob_die_gvn_overtreat_grid)) {
    M_twoway_ICER[i,j] = round(ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[j], death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])["ICER"], 2)
    M_twoway_inc_costs[i,j] = round(ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[j], death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])["Incremental Costs"], 2)
    M_twoway_inc_DALYs_averted[i,j] = round(ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[j], death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])["Incremental DALYs Averted"], 2)
    M_twoway_WHO_costs[i,j] = round(ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[j], death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])["WHO Costs"], 2)
    M_twoway_WHO_DALYs_averted[i,j] = round(ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[j], death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])["WHO DALYs Averted"], 2)
    M_twoway_NIRUDAK_costs[i,j] = round(ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[j], death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])["NIRUDAK Costs"], 2)
    M_twoway_NIRUDAK_DALYs_averted[i,j] = round(ICER_fxn(death_gvn_overtreat_prob = prob_die_gvn_overtreat_grid[j], death_gvn_undertreat_prob = prob_die_gvn_undertreat_grid[i])["NIRUDAK DALYs Averted"], 2)
  }
}

# M_twoway_ICER[1:10, 1:10]

# max(M_twoway_ICER)

# setting (as of Jul 24, arbitrary) WTP threshold
WTP = 10

# nondominated_overtreat_index <- which(M_twoway_ICER[30,] > 0)

# two_way_sense_fxn is a function that ultimately generates the two-way sensitivity plot
two_way_sense_fxn <- function (WTP, doplot=TRUE){
  
  threshold_probs <- matrix(NA, length(prob_die_gvn_undertreat_grid), 2)
  threshold_probs[,1] <- prob_die_gvn_undertreat_grid
  colnames(threshold_probs) <- c("prob_death_undertreat", "prob_death_overtreat_threshold")
  
  for (i in 1:dim(M_twoway_ICER)[1]) {
    if (any(M_twoway_ICER[i,]>0 & M_twoway_ICER[i,] < WTP)) {
      max(which(WTP > M_twoway_ICER[i,] & M_twoway_ICER[i,] > 0))
      temp_prob <- prob_die_gvn_overtreat_grid[max(which(WTP > M_twoway_ICER[i,] & M_twoway_ICER[i,] > 0))]
      threshold_probs[i,2] <- temp_prob
    } else {threshold_probs[i,2] = NA}
  }
  
  if (doplot){
    plot(threshold_probs, pch=20, xlim = c(0,1), ylim = c(0,1))
  }
  return(threshold_probs)
}

# example
WTP80_2waysense_result <- two_way_sense_fxn(80)

################################################################################
# probabilistic sensitivity analysis (average over input uncertainty; characterizes uncertainty in the output)
# need to put distributions on the inputs and sampling over the distribution
# this is done after the two way
################################################################################


