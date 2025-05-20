

problem areas:
treating those with severe dehyrdation as if they had none
treating those with severe dehydration as if they had some 

treating those with None as if they had severe
treating those with some as if they had severe

treating those with some as if they had no dehydration


####Assumptions

##Trt
 #patients treated according to who guidelines. So, none is no treatment is sent home, 
 #some is oral fluids and monitoring and severe is IV and hospitalization.

##DALY Penalties associated with Appropriate Treatment
 #no DALY penalty for treating accurately (if instrument classifies correctly)
   #this may be wrong: do some with severe dehydration die even if treated appropriately?
   #if they did, this would diminish the benefit of getting things right.
##DALY penalities associated with getting things wrong:
  #assume treating those with no dehydration as if they had some dehydration carries no DALY penalty, only a cost 


# define net heatlh benefit function
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