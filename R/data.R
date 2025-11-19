#' Bigeye Tuna base case operating model from recent SS3 Stock Assessment
#'
#' Run 6 of the 2021 ICCAT Stock Synthesis 3 assessment (Anonymous 2021)
#'
#' @format An openMSE object of class 'OM'
#' @source Anonymous. 2021. Report of the 2021 Bigeye Stock Assessment Meeting. SCRS/2021/011. Collect. Vol. Sci. Pap. ICCAT, 78(2): 335-485.
#' @examples
#' myMSE = runMSE(BET_1)
"BET_1"


#' Alternative, low resilience, Bigeye Tuna base case operating model from recent SS3 Stock Assessment
#'
#' Run 6 of the 2021 ICCAT Stock Synthesis 3 assessment (Anonymous 2021) but with steepness = 0.7
#'
#' @format An openMSE object of class 'OM'
#' @source Anonymous. 2021. Report of the 2021 Bigeye Stock Assessment Meeting. SCRS/2021/011. Collect. Vol. Sci. Pap. ICCAT, 78(2): 335-485.
#' @examples
#' myMSE = runMSE(BET_2)
"BET_2"


#' Blue Shark base case operating model from recent SS3 Stock Assessment
#'
#' Base case Stock Synthesis 3 assessment (Courtney 2016)
#'
#' @format An openMSE object of class 'OM'
#' @source Courtney, D. 2016. Preliminary Stock Synthesis (SS3) Model Runs conducted for North Atlantic blue shark. SCRS/2015/151. Collect. Vol. Sci. Pap. ICCAT, 72(5): 1186-1232.
#' @examples
#' myMSE = runMSE(BSH_1)
"BSH_1"


#' Alternative, low resilience, Blue Shark base case operating model from recent SS3 Stock Assessment
#'
#' Base case Stock Synthesis 3 assessment (Courtney 2016) but with steepness = 0.7
#'
#' @format An openMSE object of class 'OM'
#' @source Courtney, D. 2016. Preliminary Stock Synthesis (SS3) Model Runs conducted for North Atlantic blue shark. SCRS/2015/151. Collect. Vol. Sci. Pap. ICCAT, 72(5): 1186-1232.
#' @examples
#' myMSE = runMSE(BSH_2)
"BSH_2"


#' Example Climate Test Data object
#'
#' The output of the function CT_3_test()
#'
#' @format A list two positions long, the first is the hierarchical list of MSE objects (openMSE class 'MSE') test x increment, the second is a matrix of the levels of each test.
#' @examples
#' CT_4_summary(CT_data)
"CT_data"
