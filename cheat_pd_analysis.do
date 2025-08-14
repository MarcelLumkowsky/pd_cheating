***Data Analysis Cheating Repeated PD*******************************************
********************************************************************************
*Fill in Paths and Save Data in MY_PATH_IN**************************************

clear
set scheme s2color
graph set window fontface "Arial"
global MY_PATH_IN   ""
global MY_PATH_OUT  ""
global OUT_TABLES  ""
global MY_LOG_FILE  ${MY_PATH_OUT}log.log
capture log close
log using "${MY_LOG_FILE}", text replace
set more off
version 15

********************************************************************************

use "${MY_PATH_IN}data_ready_for_analysis.dta"

drop if session == 6
drop if session == 7
drop if session == 8
drop if session == 10
drop if session == 13

********************************************************************************
*Section 5.1 - In Text - Significance levels************************************

tab cooperation if treatment == 1
tab cooperation if treatment == 5
tab cooperation if treatment == 3

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1), cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1), cluster (id group_2)

tab cooperation if treatment == 4
tab cooperation if treatment == 2

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1), cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1), cluster (id group_2)

vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_3==1), cluster (id group_2)
vcemway probit cooperation treatment_2 if (treatment_2==1|treatment_3==1), cluster (id group_2)

vcemway probit cooperation treatment_2 if (treatment_2==1|treatment_5==1), cluster (id group_2)
vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_5==1), cluster (id group_2)

vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_5==1), cluster (id group_2)
vcemway probit cooperation treatment_2 if (treatment_2==1|treatment_4==1), cluster (id group_2)

********************************************************************************
*Figure 1 - Bar graph CR rates *************************************************

grstyle init
grstyle set color edkblue "189 30 36*.8" "0 114 86*.8" "189 30 36*.5" "0 114 86*.5"
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno draw_major_hgrid no
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 8pt: subheading axis_title
grstyle set size 8pt: heading
grstyle set size 8pt: tick_label
grstyle set size 8pt: key_label
grstyle set size 8pt: legend

gen order = 1 if treatment == 1 
replace order = 2 if treatment == 5
replace order = 3 if treatment == 3
replace order = 4 if treatment == 4
replace order = 5 if treatment == 2

lab def order 1 "Baseline" 2 "Revealed 12.5" 3 "Revealed 25" 4 "Hidden 12.5" 5 "Hidden 25" 
lab val order order

*All rounds, all games**********************************************************
*Clustered SEs******************************************************************

egen mean_coop_t1 = mean(cooperation) if treatment == 1
egen mean_coop_t2 = mean(cooperation) if treatment == 2
egen mean_coop_t3 = mean(cooperation) if treatment == 3
egen mean_coop_t4 = mean(cooperation) if treatment == 4
egen mean_coop_t5 = mean(cooperation) if treatment == 5

vcemway reg cooperation if treatment == 1, cluster (id group_2)

generate hiwrite_t1 = mean_coop_t1 + 0.0295582 if treatment == 1
generate lowrite_t1 = mean_coop_t1 - 0.0295582 if treatment == 1

vcemway reg cooperation if treatment == 2, cluster (id group_2)

generate hiwrite_t2 = mean_coop_t2 + 0.0377566 if treatment == 2
generate lowrite_t2 = mean_coop_t2 - 0.0377566 if treatment == 2

vcemway reg cooperation if treatment == 3, cluster (id group_2)

generate hiwrite_t3 = mean_coop_t3 + 0.033309 if treatment == 3
generate lowrite_t3 = mean_coop_t3 - 0.033309 if treatment == 3

vcemway reg cooperation if treatment == 4, cluster (id group_2)

generate hiwrite_t4 = mean_coop_t4 + 0.0407218 if treatment == 4
generate lowrite_t4 = mean_coop_t4 - 0.0407218 if treatment == 4

vcemway reg cooperation if treatment == 5, cluster (id group_2)

generate hiwrite_t5 = mean_coop_t5 + 0.0387416 if treatment == 5
generate lowrite_t5 = mean_coop_t5 - 0.0387416 if treatment == 5

twoway (bar mean_coop_t1 order if treatment == 1, color(edkblue) barw(0.95)) (bar mean_coop_t5 order if treatment == 5, color("0 114 86*.5") barw(0.95)) ///
(bar mean_coop_t3 order if treatment == 3, color("0 114 86*.8") barw(0.95))  ///
(bar mean_coop_t4 order if treatment == 4, color("189 30 36*.5") barw(0.95)) (bar mean_coop_t2 order if treatment == 2, color("189 30 36*.8") barw(0.95)) ///
(rcap hiwrite_t1 lowrite_t1 order if treatment ==1, color(black) lwidth(vvvthin)) (rcap hiwrite_t2 lowrite_t2 order if treatment == 2, color(black) lwidth(vvvthin)) ///
(rcap hiwrite_t3 lowrite_t3 order if treatment == 3, color(black) lwidth(vvvthin)) (rcap hiwrite_t4 lowrite_t4 order if treatment == 4 , color(black) lwidth(vvvthin)) ///
(rcap hiwrite_t5 lowrite_t5 order if treatment == 5, color(black) lwidth(vvvthin)), xlabel(1 2 3 4 5, valuelabel noticks angle(45)) legend(off) xtitle("") ylabel(0(0.2)1) ///
ytitle("CR: all supergames, all rounds") ysc(r(0 1))
graph save "${MY_PATH_OUT}Figure_1_Mean_CR_AllRounds_AllGames_Bar", replace

********************************************************************************
*All rounds, last five games****************************************************
*Clustered SEs******************************************************************

egen mean_coop_t1_l5 = mean(cooperation) if treatment == 1 & last_5 == 1
egen mean_coop_t2_l5 = mean(cooperation) if treatment == 2 & last_5 == 1
egen mean_coop_t3_l5 = mean(cooperation) if treatment == 3 & last_5 == 1
egen mean_coop_t4_l5 = mean(cooperation) if treatment == 4 & last_5 == 1
egen mean_coop_t5_l5 = mean(cooperation) if treatment == 5 & last_5 == 1

vcemway reg cooperation if treatment == 1 & last_5 == 1, cluster (id group_2)

generate hiwrite_t1_l5 = mean_coop_t1_l5 + 0.0201862 if treatment == 1
generate lowrite_t1_l5 = mean_coop_t1_l5 - 0.0201862 if treatment == 1

vcemway reg cooperation if treatment == 2 & last_5 == 1, cluster (id group_2)

generate hiwrite_t2_l5 = mean_coop_t2_l5 + 0.0542245 if treatment == 2
generate lowrite_t2_l5 = mean_coop_t2_l5 - 0.0542245 if treatment == 2

vcemway reg cooperation if treatment == 3 & last_5 == 1, cluster (id group_2)

generate hiwrite_t3_l5 = mean_coop_t3_l5 +  0.0484458 if treatment == 3
generate lowrite_t3_l5 = mean_coop_t3_l5 -  0.0484458 if treatment == 3

vcemway reg cooperation if treatment == 4 & last_5 == 1, cluster (id group_2)

generate hiwrite_t4_l5 = mean_coop_t4_l5 + 0.0509614 if treatment == 4
generate lowrite_t4_l5 = mean_coop_t4_l5 - 0.0509614 if treatment == 4

vcemway reg cooperation if treatment == 5 & last_5 == 1, cluster (id group_2)

generate hiwrite_t5_l5 = mean_coop_t5_l5 + 0.0597184 if treatment == 5
generate lowrite_t5_l5 = mean_coop_t5_l5 - 0.0597184 if treatment == 5

twoway (bar mean_coop_t1_l5 order if treatment == 1, color(edkblue) barw(0.95)) (bar mean_coop_t5_l5 order if treatment == 5, color("0 114 86*.5") barw(0.95)) ///
(bar mean_coop_t3_l5 order if treatment == 3, color("0 114 86*.8") barw(0.95))  ///
(bar mean_coop_t4_l5 order if treatment == 4, color("189 30 36*.5") barw(0.95)) (bar mean_coop_t2_l5 order if treatment == 2, color("189 30 36*.8") barw(0.95)) ///
(rcap hiwrite_t1_l5 lowrite_t1_l5 order if treatment ==1, color(black) lwidth(vvvthin)) (rcap hiwrite_t2_l5 lowrite_t2_l5 order if treatment == 2, color(black) lwidth(vvvthin)) ///
(rcap hiwrite_t3_l5 lowrite_t3_l5 order if treatment == 3, color(black) lwidth(vvvthin)) (rcap hiwrite_t4_l5 lowrite_t4_l5 order if treatment == 4 , color(black) lwidth(vvvthin)) ///
(rcap hiwrite_t5_l5 lowrite_t5_l5 order if treatment == 5, color(black) lwidth(vvvthin)), xlabel(1 2 3 4 5, valuelabel noticks angle(45)) legend(off) xtitle("") ylabel(0(0.2)1) ///
ytitle("CR: last five supergames, all rounds") ysc(r(0 1))

graph save "${MY_PATH_OUT}Figure_1_Mean_CR_AllRounds_LastFive_Bar", replace

********************************************************************************
*First round, all games*********************************************************
*Clustered SEs******************************************************************

egen mean_coop_t1_r1 = mean(cooperation) if treatment == 1 & period == 1
egen mean_coop_t2_r1 = mean(cooperation) if treatment == 2 & period == 1
egen mean_coop_t3_r1 = mean(cooperation) if treatment == 3 & period == 1
egen mean_coop_t4_r1 = mean(cooperation) if treatment == 4 & period == 1
egen mean_coop_t5_r1 = mean(cooperation) if treatment == 5 & period == 1

vcemway reg cooperation if treatment == 1 & period == 1, cluster (id group_2)

generate hiwrite_t1_r1 = mean_coop_t1_r1 + 0.0402791 if treatment == 1
generate lowrite_t1_r1 = mean_coop_t1_r1 - 0.0402791 if treatment == 1

vcemway reg cooperation if treatment == 2 & period == 1, cluster (id group_2)

generate hiwrite_t2_r1 = mean_coop_t2_r1 + 0.0533231 if treatment == 2
generate lowrite_t2_r1 = mean_coop_t2_r1 - 0.0533231 if treatment == 2

vcemway reg cooperation if treatment == 3 & period == 1, cluster (id group_2)

generate hiwrite_t3_r1 = mean_coop_t3_r1 + 0.034337 if treatment == 3
generate lowrite_t3_r1 = mean_coop_t3_r1 - 0.034337 if treatment == 3

vcemway reg cooperation if treatment == 4 & period == 1, cluster (id group_2)

generate hiwrite_t4_r1 = mean_coop_t4_r1 + 0.0477007 if treatment == 4
generate lowrite_t4_r1 = mean_coop_t4_r1 - 0.0477007 if treatment == 4

vcemway reg cooperation if treatment == 5 & period == 1, cluster (id group_2)

generate hiwrite_t5_r1 = mean_coop_t5_r1 + 0.0437971 if treatment == 5
generate lowrite_t5_r1 = mean_coop_t5_r1 - 0.0437971 if treatment == 5

twoway (bar mean_coop_t1_r1 order if treatment == 1, color(edkblue) barw(0.95)) (bar mean_coop_t5_r1 order if treatment == 5, color("0 114 86*.5") barw(0.95)) ///
(bar mean_coop_t3_r1 order if treatment == 3, color("0 114 86*.8") barw(0.95))  ///
(bar mean_coop_t4_r1 order if treatment == 4, color("189 30 36*.5") barw(0.95)) (bar mean_coop_t2_r1 order if treatment == 2, color("189 30 36*.8") barw(0.95)) ///
(rcap hiwrite_t1_r1 lowrite_t1_r1 order if treatment ==1, color(black) lwidth(vvvthin)) (rcap hiwrite_t2_r1 lowrite_t2_r1 order if treatment == 2, color(black) lwidth(vvvthin)) ///
(rcap hiwrite_t3_r1 lowrite_t3_r1 order if treatment == 3, color(black) lwidth(vvvthin)) (rcap hiwrite_t4_r1 lowrite_t4_r1 order if treatment == 4 , color(black) lwidth(vvvthin)) ///
(rcap hiwrite_t5_r1 lowrite_t5_r1 order if treatment == 5, color(black) lwidth(vvvthin)), xlabel(1 2 3 4 5, valuelabel noticks angle(45)) legend(off) xtitle("") ylabel(0(0.2)1) ///
ytitle("CR: all supergames, first round") ysc(r(0 1)) 

graph save "${MY_PATH_OUT}Figure_1_Mean_CR_FirstRounds_AllGames_Bar", replace

********************************************************************************
*Combine for Figure 1***********************************************************

gr combine "${MY_PATH_OUT}Figure_1_Mean_CR_AllRounds_AllGames_Bar" "${MY_PATH_OUT}Figure_1_Mean_CR_AllRounds_LastFive_Bar" "${MY_PATH_OUT}Figure_1_Mean_CR_FirstRounds_AllGames_Bar", rows(1)

********************************************************************************
*Section 5.1 - In Text - Significance levels************************************

*Last Five Supergames

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1) & last_5 == 1, cluster (id group_2)

*First Round

forvalues p = 1/31 {
    vcemway probit cooperation treatment_1 if (treatment_1 == 1 | treatment_5 == 1) & period == `p', cluster (id group_2)
}

forvalues p = 1/31 {
    vcemway probit cooperation treatment_1 if (treatment_1 == 1 | treatment_3 == 1) & period == `p', cluster (id group_2)
}

forvalues p = 1/31 {
    vcemway probit cooperation treatment_1 if (treatment_1 == 1 | treatment_4 == 1) & period == `p', cluster (id group_2)
}

forvalues p = 1/31 {
    vcemway probit cooperation treatment_1 if (treatment_1 == 1 | treatment_2 == 1) & period == `p', cluster (id group_2)
}

********************************************************************************
*Figure 2 - Line graph CR by period ********************************************

bys period: egen mean_round_t1 = mean(cooperation) if treatment == 1
lab var mean_round_t1 "Baseline"

bys period: egen mean_round_t2 = mean(cooperation) if treatment == 2
lab var mean_round_t2 "Hidden 25"

bys period: egen mean_round_t3 = mean(cooperation) if treatment == 3
lab var mean_round_t3 "Revealed 25"

bys period: egen mean_round_t4 = mean(cooperation) if treatment == 4
lab var mean_round_t4 "Hidden 12.5"

bys period: egen mean_round_t5 = mean(cooperation) if treatment == 5
lab var mean_round_t5 "Revealed 12.5"

grstyle init
grstyle set color gs14 edkblue "189 30 36*.8" "0 114 86*.8" "189 30 36*.5" "0 114 86*.5"
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno draw_major_vgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 8pt: subheading axis_title
grstyle set size 8pt: heading
grstyle set size 8pt: tick_label
grstyle set size 8pt: key_label
grstyle set size 8pt: legend

gen upper = 1

twoway (bar upper period if inrange(period,11,20), bcolor(gs14%30) base(0) lwidth(0)) (connected mean_round_t1 period if period <21, sort msize(small) msymbol(o) lcolor(edkblue) title("") xtitle("Round") ytitle("CR: all supergames by round")) (connected mean_round_t2 period if period <21, sort msize(small) msymbol(|) lcolor("189 30 36*.8")) /// 
(connected mean_round_t3 period if period <21, sort msize(small) msymbol(d)  lcolor("0 114 86*.8")) (connected mean_round_t4 period if period <21, sort msize(small) msymbol(T) lcolor("189 30 36*.5")) ///
(connected mean_round_t5 period if period <21, sort msize(small) msymbol(s)  lcolor("0 114 86*.5")), legend(title("") order(2 6 4 5 3) rows(1) stack)  ylabel(0(0.2)1) 

graph save "${MY_PATH_OUT}Figure_3_Mean_CR_PerRound_AllGames", replace

********************************************************************************
*Table 3************************************************************************

vcemway probit cooperation i.treatment_5 i.treatment_3 i.treatment_4 i.treatment_2, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r1

vcemway probit cooperation i.treatment_5 i.treatment_3 i.treatment_4 i.treatment_2 c.match c.period i.game_length_2 i.game_length_3 i.game_length_4, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r2

vcemway probit cooperation i.treatment_5 i.treatment_3 i.treatment_4 i.treatment_2 c.match c.period i.game_length_2 i.game_length_3 i.game_length_4 ///
c.age i.female i.course i.experiment_cat_2 i.experiment_cat_3 i.experiment_cat_4, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r3

vcemway probit cooperation i.treatment_5 i.treatment_3 i.treatment_1, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r4

vcemway probit cooperation i.treatment_5 i.treatment_3 i.treatment_1 c.match c.period i.game_length_2 i.game_length_3 i.game_length_4, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r5

vcemway probit cooperation i.treatment_5 i.treatment_3 i.treatment_1 c.match c.period i.game_length_2 i.game_length_3 i.game_length_4 c.age i.female i.course i.experiment_cat_2 i.experiment_cat_3 i.experiment_cat_4, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r6

esttab r1 r2 r3 r4 r5 r6 using "${OUT_TABLES}Table_2_1.rtf", varwidth(12) cells(b(star fmt(4)) se(par fmt(4))) stats(N, fmt(0 4 4)) starlevel(* 0.10 ** 0.05 *** 0.01) legend nobase style(fixed) nocons /// 
replace 

********************************************************************************
*Section 5.2. - In Text*********************************************************

tab cheat_c if treatment == 2
tab cheat_c if treatment == 3
tab cheat_c if treatment == 4
tab cheat_c if treatment == 5

tab cheat_d if treatment == 4
tab cheat_d if treatment == 2
tab cheat_d if treatment == 5
tab cheat_d if treatment == 3

vcemway probit cheat_d treatment_4 if (treatment_4==1|treatment_5==1), cluster (id group_2)
vcemway probit cheat_d treatment_2 if (treatment_2==1|treatment_3==1), cluster (id group_2)

vcemway probit honest_c treatment_4 if (treatment_4==1|treatment_5==1), cluster (id group_2)
vcemway probit honest_c treatment_2 if (treatment_2==1|treatment_3==1), cluster (id group_2)

vcemway probit honest_d treatment_4 if (treatment_4==1|treatment_5==1), cluster (id group_2)
vcemway probit honest_d treatment_2 if (treatment_2==1|treatment_3==1), cluster (id group_2)

vcemway probit cheat_d treatment_3 if (treatment_3==1|treatment_5==1), cluster (id group_2)
vcemway probit cheat_d treatment_2 if (treatment_2==1|treatment_4==1), cluster (id group_2)

vcemway probit cheat_d treatment_4 if (treatment_4==1|treatment_5==1) & other_signal_n1 == 1, cluster (id group_2)
vcemway probit cheat_d treatment_2 if (treatment_2==1|treatment_3==1) & other_signal_n1 == 1, cluster (id group_2)

********************************************************************************
*Figure 3 - Bar graph use of options in cheating treatments*********************

grstyle init
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid no
grstyle yesno draw_major_vgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 8pt: subheading axis_title
grstyle set size 8pt: heading
grstyle set size 8pt: tick_label
grstyle set size 8pt: key_label
grstyle set size 8pt: legend

sort id match period
gen other_show_n1 = other_show[_n-1] 
replace other_show_n1 = . if period == 1

egen mean_honest_c_t2 = mean(honest_c) if treatment == 2
egen mean_honest_c_t3 = mean(honest_c) if treatment == 3
egen mean_honest_c_t4 = mean(honest_c) if treatment == 4
egen mean_honest_c_t5 = mean(honest_c) if treatment == 5

egen mean_honest_d_t2 = mean(honest_d) if treatment == 2
egen mean_honest_d_t3 = mean(honest_d) if treatment == 3
egen mean_honest_d_t4 = mean(honest_d) if treatment == 4
egen mean_honest_d_t5 = mean(honest_d) if treatment == 5

egen mean_cheat_d_t2 = mean(cheat_d) if treatment == 2
egen mean_cheat_d_t3 = mean(cheat_d) if treatment == 3
egen mean_cheat_d_t4 = mean(cheat_d) if treatment == 4
egen mean_cheat_d_t5 = mean(cheat_d) if treatment == 5

egen mean_cheat_c_t2 = mean(cheat_c) if treatment == 2
egen mean_cheat_c_t3 = mean(cheat_c) if treatment == 3
egen mean_cheat_c_t4 = mean(cheat_c) if treatment == 4
egen mean_cheat_c_t5 = mean(cheat_c) if treatment == 5

********************************************************************************
*Upper Panel Error Bars T2******************************************************

vcemway reg honest_c if treatment == 2, cluster (id group_2)

generate hi_honest_c_t2 = mean_honest_c_t2 + 0.0380309 if treatment == 2
generate low_honest_c_t2 = mean_honest_c_t2 - 0.0380309 if treatment == 2

vcemway reg honest_d if treatment == 2, cluster (id group_2)

generate hi_honest_d_t2 = mean_honest_d_t2 + 0.0218304 if treatment == 2
generate low_honest_d_t2 = mean_honest_d_t2 - 0.0218304 if treatment == 2

vcemway reg cheat_d if treatment == 2, cluster (id group_2)

generate hi_cheat_d_t2 = mean_cheat_d_t2 + 0.0366592 if treatment == 2
generate low_cheat_d_t2 = mean_cheat_d_t2 - 0.0366592 if treatment == 2

vcemway reg cheat_c if treatment == 2, cluster (id group_2)

generate hi_cheat_c_t2 = mean_cheat_c_t2 + 0.0029157  if treatment == 2
generate low_cheat_c_t2 = mean_cheat_c_t2 - 0.0029157  if treatment == 2

********************************************************************************
*Upper Panel Error Bars T3******************************************************

vcemway reg honest_c if treatment == 3, cluster (id group_2)

generate hi_honest_c_t3 = mean_honest_c_t3 + 0.0339786 if treatment == 3
generate low_honest_c_t3 = mean_honest_c_t3 - 0.0339786 if treatment == 3

vcemway reg honest_d if treatment == 3, cluster (id group_2)

generate hi_honest_d_t3 = mean_honest_d_t3 + 0.021248 if treatment == 3
generate low_honest_d_t3 = mean_honest_d_t3 - 0.021248 if treatment == 3

vcemway reg cheat_d if treatment == 3, cluster (id group_2)

generate hi_cheat_d_t3 = mean_cheat_d_t3 + 0.0202679 if treatment == 3
generate low_cheat_d_t3 = mean_cheat_d_t3 - 0.0202679 if treatment == 3

vcemway reg cheat_c if treatment == 3, cluster (id group_2)

generate hi_cheat_c_t3 = mean_cheat_c_t3 + 0.003536 if treatment == 3
generate low_cheat_c_t3 = mean_cheat_c_t3 - 0.003536 if treatment == 3

********************************************************************************
*Upper Panel Error Bars T4******************************************************

vcemway reg honest_c if treatment == 4, cluster (id group_2)

generate hi_honest_c_t4 = mean_honest_c_t4 + 0.0411823 if treatment == 4
generate low_honest_c_t4 = mean_honest_c_t4 - 0.0411823 if treatment == 4

vcemway reg honest_d if treatment == 4, cluster (id group_2)

generate hi_honest_d_t4 = mean_honest_d_t4 + 0.0211097  if treatment == 4
generate low_honest_d_t4 = mean_honest_d_t4 - 0.0211097  if treatment == 4

vcemway reg cheat_d if treatment == 4, cluster (id group_2)

generate hi_cheat_d_t4 = mean_cheat_d_t4 + 0.038325 if treatment == 4
generate low_cheat_d_t4 = mean_cheat_d_t4 - 0.038325 if treatment == 4

vcemway reg cheat_c if treatment == 4, cluster (id group_2)

generate hi_cheat_c_t4 = mean_cheat_c_t4 + 0.007436 if treatment == 4
generate low_cheat_c_t4 = mean_cheat_c_t4 - 0.007436 if treatment == 4

********************************************************************************
*Upper Panel Error Bars T5******************************************************

vcemway reg honest_c if treatment == 5, cluster (id group_2)

generate hi_honest_c_t5 = mean_honest_c_t5 + 0.0397719 if treatment == 5
generate low_honest_c_t5 = mean_honest_c_t5 - 0.0397719 if treatment == 5

vcemway reg honest_d if treatment == 5, cluster (id group_2)

generate hi_honest_d_t5 = mean_honest_d_t5 + 0.0286445 if treatment == 5
generate low_honest_d_t5 = mean_honest_d_t5 - 0.0286445 if treatment == 5

vcemway reg cheat_d if treatment == 5, cluster (id group_2)

generate hi_cheat_d_t5 = mean_cheat_d_t5 + 0.023542 if treatment == 5
generate low_cheat_d_t5 = mean_cheat_d_t5 - 0.023542 if treatment == 5

vcemway reg cheat_c if treatment == 5, cluster (id group_2)

generate hi_cheat_c_t5 = mean_cheat_c_t5 + 0.0103198 if treatment == 5
generate low_cheat_c_t5 = mean_cheat_c_t5 - 0.0103198 if treatment == 5

********************************************************************************

gen order_100 = 1 if treatment == 5 
gen order_200 = 2 if treatment == 5 
gen order_300 = 3 if treatment == 5 
gen order_400 = 4 if treatment == 5 

gen order_500 = 6 if treatment == 3 
gen order_600 = 7 if treatment == 3 
gen order_700 = 8 if treatment == 3 
gen order_800 = 9 if treatment == 3 

gen order_1000 = 11 if treatment == 4 
gen order_1100 = 12 if treatment == 4 
gen order_1200 = 13 if treatment == 4 
gen order_1300 = 14 if treatment == 4 

gen order_1400 = 16 if treatment == 2
gen order_1500 = 17 if treatment == 2 
gen order_1600 = 18 if treatment == 2 
gen order_1700 = 19 if treatment == 2 

********************************************************************************
*Upper Panel********************************************************************

twoway (bar mean_honest_c_t5 order_100 if treatment == 5, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_t5 order_200 if treatment == 5, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_t5 order_300 if treatment == 5, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_t5 order_400 if treatment == 5, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_t3 order_500 if treatment == 3, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_t3 order_600 if treatment == 3, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_t3 order_700 if treatment == 3, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_t3 order_800 if treatment == 3, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_t4 order_1000 if treatment == 4, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_t4 order_1100 if treatment == 4, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_t4 order_1200 if treatment == 4, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_t4 order_1300 if treatment == 4, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_t2 order_1400 if treatment == 2, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_t2 order_1500 if treatment == 2, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_t2 order_1600 if treatment == 2, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_t2 order_1700 if treatment == 2, color("246 199 0*.5") barw(0.95)) ///
(rcap hi_honest_c_t2 low_honest_c_t2 order_1400 if treatment ==2, color(black) lwidth(vvvthin)) (rcap hi_honest_d_t2 low_honest_d_t2 order_1500 if treatment ==2, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_t2 low_cheat_d_t2 order_1600 if treatment ==2, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_t2 low_cheat_c_t2 order_1700 if treatment ==2, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_t3 low_honest_c_t3 order_500 if treatment ==3, color(black) lwidth(vvvthin)) (rcap hi_honest_d_t3 low_honest_d_t3 order_600 if treatment ==3, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_t3 low_cheat_d_t3 order_700 if treatment ==3, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_t3 low_cheat_c_t3 order_800 if treatment ==3, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_t4 low_honest_c_t4 order_1000 if treatment ==4, color(black) lwidth(vvvthin)) (rcap hi_honest_d_t4 low_honest_d_t4 order_1100 if treatment ==4, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_t4 low_cheat_d_t4 order_1200 if treatment ==4, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_t4 low_cheat_c_t4 order_1300 if treatment ==4, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_t5 low_honest_c_t5 order_100 if treatment ==5, color(black) lwidth(vvvthin)) (rcap hi_honest_d_t5 low_honest_d_t5 order_200 if treatment ==5, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_t5 low_cheat_d_t5 order_300 if treatment ==5, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_t5 low_cheat_c_t5 order_400 if treatment ==5, color(black) lwidth(vvvthin)), ///
xlabel(2.5 "Revealed 12.5" 7.5 "Revealed 25" 12.5 "Hidden 12.5" 17.5 "Hidden 25", valuelabel noticks angle(0)) legend(order(1 "Honest C" 2 "Honest D" 3 "Cheat D" 4 "Cheat C") cols(4)) title("All observations") ylabel(0(0.2)1) ytitle("Share choices: all supergames, all rounds") ysc(r(0 1)) 

graph save "${MY_PATH_OUT}Figure_3_1", replace

********************************************************************************
*Lower Left Panel***************************************************************

egen mean_honest_c_C_t2 = mean(honest_c) if treatment == 2 & other_signal_n1 == 1
egen mean_honest_c_C_t3 = mean(honest_c) if treatment == 3 & other_signal_n1 == 1 
egen mean_honest_c_C_t4 = mean(honest_c) if treatment == 4 & other_signal_n1 == 1
egen mean_honest_c_C_t5 = mean(honest_c) if treatment == 5 & other_signal_n1 == 1

egen mean_honest_d_C_t2 = mean(honest_d) if treatment == 2 & other_signal_n1 == 1
egen mean_honest_d_C_t3 = mean(honest_d) if treatment == 3 & other_signal_n1 == 1
egen mean_honest_d_C_t4 = mean(honest_d) if treatment == 4 & other_signal_n1 == 1
egen mean_honest_d_C_t5 = mean(honest_d) if treatment == 5 & other_signal_n1 == 1

egen mean_cheat_d_C_t2 = mean(cheat_d) if treatment == 2 & other_signal_n1 == 1
egen mean_cheat_d_C_t3 = mean(cheat_d) if treatment == 3 & other_signal_n1 == 1
egen mean_cheat_d_C_t4 = mean(cheat_d) if treatment == 4 & other_signal_n1 == 1
egen mean_cheat_d_C_t5 = mean(cheat_d) if treatment == 5 & other_signal_n1 == 1

egen mean_cheat_c_C_t2 = mean(cheat_c) if treatment == 2 & other_signal_n1 == 1
egen mean_cheat_c_C_t3 = mean(cheat_c) if treatment == 3 & other_signal_n1 == 1
egen mean_cheat_c_C_t4 = mean(cheat_c) if treatment == 4 & other_signal_n1 == 1
egen mean_cheat_c_C_t5 = mean(cheat_c) if treatment == 5 & other_signal_n1 == 1

********************************************************************************
*Lower Left Panel Error Bars T2*************************************************

vcemway reg honest_c if treatment == 2 & other_signal_n1 == 1, cluster (id group_2)

generate hi_honest_c_C_t2 = mean_honest_c_C_t2 + 0.0392782 if treatment == 2 & other_signal_n1 == 1
generate low_honest_c_C_t2 = mean_honest_c_C_t2 - 0.0392782  if treatment == 2 & other_signal_n1 == 1

vcemway reg honest_d if treatment == 2 & other_signal_n1 == 1 , cluster (id group_2)

generate hi_honest_d_C_t2 = mean_honest_d_C_t2  + 0.0132326 if treatment == 2 & other_signal_n1 == 1
generate low_honest_d_C_t2 = mean_honest_d_C_t2  - 0.0132326 if treatment == 2 & other_signal_n1 == 1

vcemway reg cheat_d if treatment == 2 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_d_C_t2 = mean_cheat_d_C_t2 + 0.034446 if treatment == 2 & other_signal_n1 == 1
generate low_cheat_d_C_t2 = mean_cheat_d_C_t2 - 0.034446 if treatment == 2 & other_signal_n1 == 1

vcemway reg cheat_c if treatment == 2 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_c_C_t2 = mean_cheat_c_C_t2 + 0.0021876 if treatment == 2 & other_signal_n1 == 1
generate low_cheat_c_C_t2 = mean_cheat_c_C_t2 - 0.0021876 if treatment == 2 & other_signal_n1 == 1

********************************************************************************
*Lower Left Panel Error Bars T3*************************************************

vcemway reg honest_c if treatment == 3 & other_signal_n1 == 1, cluster (id group_2)

generate hi_honest_c_C_t3 = mean_honest_c_C_t3 + 0.0258159 if treatment == 3 & other_signal_n1 == 1
generate low_honest_c_C_t3 = mean_honest_c_C_t3 - 0.0258159 if treatment == 3 & other_signal_n1 == 1

vcemway reg honest_d if treatment == 3 & other_signal_n1 == 1, cluster (id group_2)

generate hi_honest_d_C_t3 = mean_honest_d_C_t3 + 0.0113648 if treatment == 3 & other_signal_n1 == 1
generate low_honest_d_C_t3 = mean_honest_d_C_t3 - 0.0113648 if treatment == 3 & other_signal_n1 == 1

vcemway reg cheat_d if treatment == 3 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_d_C_t3 = mean_cheat_d_C_t3 + 0.0168059 if treatment == 3 & other_signal_n1 == 1
generate low_cheat_d_C_t3 = mean_cheat_d_C_t3 - 0.0168059 if treatment == 3 & other_signal_n1 == 1

vcemway reg cheat_c if treatment == 3 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_c_C_t3 = mean_cheat_c_C_t3 + 0.0022392 if treatment == 3 & other_signal_n1 == 1
generate low_cheat_c_C_t3 = mean_cheat_c_C_t3 - 0.0022392  if treatment == 3 & other_signal_n1 == 1

********************************************************************************
*Lower Left Panel Error Bars T4*************************************************

vcemway reg honest_c if treatment == 4 & other_signal_n1 == 1, cluster (id group_2)

generate hi_honest_c_C_t4 = mean_honest_c_C_t4 + 0.0370252 if treatment == 4 & other_signal_n1 == 1
generate low_honest_c_C_t4 = mean_honest_c_C_t4 - 0.0370252  if treatment == 4 & other_signal_n1 == 1

vcemway reg honest_d if treatment == 4 & other_signal_n1 == 1, cluster (id group_2)

generate hi_honest_d_C_t4 = mean_honest_d_C_t4 + 0.0100044 if treatment == 4 & other_signal_n1 == 1
generate low_honest_d_C_t4 = mean_honest_d_C_t4 - 0.0100044 if treatment == 4 & other_signal_n1 == 1

vcemway reg cheat_d if treatment == 4 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_d_C_t4 = mean_cheat_d_C_t4 + 0.0303029 if treatment == 4 & other_signal_n1 == 1
generate low_cheat_d_C_t4 = mean_cheat_d_C_t4 - 0.0303029 if treatment == 4 & other_signal_n1 == 1

vcemway reg cheat_c if treatment == 4 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_c_C_t4 = mean_cheat_c_C_t4 + 0.0102481 if treatment == 4 & other_signal_n1 == 1
generate low_cheat_c_C_t4 = mean_cheat_c_C_t4 - 0.0102481 if treatment == 4 & other_signal_n1 == 1

********************************************************************************
*Lower Left Panel Error Bars T5*************************************************

vcemway reg honest_c if treatment == 5 & other_signal_n1 == 1, cluster (id group_2)

generate hi_honest_c_C_t5 = mean_honest_c_C_t5 + 0.0306074 if treatment == 5 & other_signal_n1 == 1
generate low_honest_c_C_t5 = mean_honest_c_C_t5 - 0.0306074 if treatment == 5 & other_signal_n1 == 1

vcemway reg honest_d if treatment == 5 & other_signal_n1 == 1, cluster (id group_2)

generate hi_honest_d_C_t5 =  mean_honest_d_C_t5 + 0.0144095 if treatment == 5 & other_signal_n1 == 1
generate low_honest_d_C_t5 = mean_honest_d_C_t5 - 0.0144095 if treatment == 5 & other_signal_n1 == 1

vcemway reg cheat_d if treatment == 5 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_d_C_t5 = mean_cheat_d_C_t5 + 0.0180393 if treatment == 5 & other_signal_n1 == 1
generate low_cheat_d_C_t5 = mean_cheat_d_C_t5 - 0.0180393 if treatment == 5 & other_signal_n1 == 1

vcemway reg cheat_c if treatment == 5 & other_signal_n1 == 1, cluster (id group_2)

generate hi_cheat_c_C_t5 = mean_cheat_c_C_t5 + 0.0131267 if treatment == 5 & other_signal_n1 == 1
generate low_cheat_c_C_t5 = mean_cheat_c_C_t5 - 0.0131267 if treatment == 5 & other_signal_n1 == 1

********************************************************************************
*Lower Left Panel***************************************************************

twoway (bar mean_honest_c_C_t5 order_100 if treatment == 5, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_C_t5 order_200 if treatment == 5, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_C_t5 order_300 if treatment == 5, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_C_t5 order_400 if treatment == 5, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_C_t3 order_500 if treatment == 3, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_C_t3 order_600 if treatment == 3, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_C_t3 order_700 if treatment == 3, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_C_t3 order_800 if treatment == 3, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_C_t4 order_1000 if treatment == 4, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_C_t4 order_1100 if treatment == 4, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_C_t4 order_1200 if treatment == 4, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_C_t4 order_1300 if treatment == 4, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_C_t2 order_1400 if treatment == 2, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_C_t2 order_1500 if treatment == 2, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_C_t2 order_1600 if treatment == 2, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_C_t2 order_1700 if treatment == 2, color("246 199 0*.5") barw(0.95)) ///
(rcap hi_honest_c_C_t2 low_honest_c_C_t2 order_1400 if treatment ==2, color(black) lwidth(vvvthin)) (rcap hi_honest_d_C_t2 low_honest_d_C_t2 order_1500 if treatment ==2, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_C_t2 low_cheat_d_C_t2 order_1600 if treatment ==2, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_C_t2 low_cheat_c_C_t2 order_1700 if treatment ==2, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_C_t3 low_honest_c_C_t3 order_500 if treatment ==3, color(black) lwidth(vvvthin)) (rcap hi_honest_d_C_t3 low_honest_d_C_t3 order_600 if treatment ==3, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_C_t3 low_cheat_d_C_t3 order_700 if treatment ==3, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_C_t3 low_cheat_c_C_t3 order_800 if treatment ==3, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_C_t4 low_honest_c_C_t4 order_1000 if treatment ==4, color(black) lwidth(vvvthin)) (rcap hi_honest_d_C_t4 low_honest_d_C_t4 order_1100 if treatment ==4, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_C_t4 low_cheat_d_C_t4 order_1200 if treatment ==4, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_C_t4 low_cheat_c_C_t4 order_1300 if treatment ==4, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_C_t5 low_honest_c_C_t5 order_100 if treatment ==5, color(black) lwidth(vvvthin)) (rcap hi_honest_d_C_t5 low_honest_d_C_t5 order_200 if treatment ==5, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_C_t5 low_cheat_d_C_t5 order_300 if treatment ==5, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_C_t5 low_cheat_c_C_t5 order_400 if treatment ==5, color(black) lwidth(vvvthin)), ///
xlabel(2.5 "Revealed 12.5" 7.5 "Revealed 25" 12.5 "Hidden 12.5" 17.5 "Hidden 25", valuelabel noticks angle(0)) legend(order(1 "Honest C" 2 "Honest D" 3 "Cheat D" 4 "Cheat C") cols(4)) title("Co-Player: C or C* in t-1") ylabel(0(0.2)1) ysc(r(0 1)) 

graph save "${MY_PATH_OUT}Figure_3_2", replace

********************************************************************************
*Lower Right Panel**************************************************************

egen mean_honest_c_D_t2 = mean(honest_c) if treatment == 2 & other_signal_n1 == 0
egen mean_honest_c_D_t3 = mean(honest_c) if treatment == 3 & other_signal_n1 == 0 
egen mean_honest_c_D_t4 = mean(honest_c) if treatment == 4 & other_signal_n1 == 0
egen mean_honest_c_D_t5 = mean(honest_c) if treatment == 5 & other_signal_n1 == 0

egen mean_honest_d_D_t2 = mean(honest_d) if treatment == 2 & other_signal_n1 == 0
egen mean_honest_d_D_t3 = mean(honest_d) if treatment == 3 & other_signal_n1 == 0
egen mean_honest_d_D_t4 = mean(honest_d) if treatment == 4 & other_signal_n1 == 0
egen mean_honest_d_D_t5 = mean(honest_d) if treatment == 5 & other_signal_n1 == 0

egen mean_cheat_d_D_t2 = mean(cheat_d) if treatment == 2 & other_signal_n1 == 0
egen mean_cheat_d_D_t3 = mean(cheat_d) if treatment == 3 & other_signal_n1 == 0
egen mean_cheat_d_D_t4 = mean(cheat_d) if treatment == 4 & other_signal_n1 == 0
egen mean_cheat_d_D_t5 = mean(cheat_d) if treatment == 5 & other_signal_n1 == 0

egen mean_cheat_c_D_t2 = mean(cheat_c) if treatment == 2 & other_signal_n1 == 0
egen mean_cheat_c_D_t3 = mean(cheat_c) if treatment == 3 & other_signal_n1 == 0
egen mean_cheat_c_D_t4 = mean(cheat_c) if treatment == 4 & other_signal_n1 == 0
egen mean_cheat_c_D_t5 = mean(cheat_c) if treatment == 5 & other_signal_n1 == 0

********************************************************************************
*Lower Right Panel Error Bars T2************************************************

vcemway reg honest_c if treatment == 2 & other_signal_n1 == 0, cluster (id group_2)

generate hi_honest_c_D_t2 = mean_honest_c_D_t2 + 0.0137688 if treatment == 2 & other_signal_n1 == 0
generate low_honest_c_D_t2 = mean_honest_c_D_t2 - 0.0137688 if treatment == 2 & other_signal_n1 == 0

vcemway reg honest_d if treatment == 2 & other_signal_n1 == 0 , cluster (id group_2)

generate hi_honest_d_D_t2 = mean_honest_d_D_t2 + 0.0346142 if treatment == 2 & other_signal_n1 == 0
generate low_honest_d_D_t2 = mean_honest_d_D_t2 - 0.0346142 if treatment == 2 & other_signal_n1 == 0

vcemway reg cheat_d if treatment == 2 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_d_D_t2 = mean_cheat_d_D_t2 + 0.0380728 if treatment == 2 & other_signal_n1 == 0
generate low_cheat_d_D_t2 = mean_cheat_d_D_t2 - 0.0380728 if treatment == 2 & other_signal_n1 == 0

vcemway reg cheat_c if treatment == 2 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_c_D_t2 = mean_cheat_c_D_t2 + 0.0042766 if treatment == 2 & other_signal_n1 == 0
generate low_cheat_c_D_t2 = mean_cheat_c_D_t2 - 0.0042766 if treatment == 2 & other_signal_n1 == 0

********************************************************************************
*Lower Right Panel Error Bars T3************************************************

vcemway reg honest_c if treatment == 3 & other_signal_n1 == 0, cluster (id group_2)

generate hi_honest_c_D_t3 = mean_honest_c_D_t3 + 0.0148244 if treatment == 3 & other_signal_n1 == 0
generate low_honest_c_D_t3 = mean_honest_c_D_t3 - 0.0148244 if treatment == 3 & other_signal_n1 == 0

vcemway reg honest_d if treatment == 3 & other_signal_n1 == 0, cluster (id group_2)

generate hi_honest_d_D_t3 = mean_honest_d_D_t3 + 0.0353648  if treatment == 3 & other_signal_n1 == 0
generate low_honest_d_D_t3 = mean_honest_d_D_t3 - 0.0353648  if treatment == 3 & other_signal_n1 == 0

vcemway reg cheat_d if treatment == 3 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_d_D_t3 = mean_cheat_d_D_t3 + 0.0310518 if treatment == 3 & other_signal_n1 == 0
generate low_cheat_d_D_t3 = mean_cheat_d_D_t3 - 0.0310518 if treatment == 3 & other_signal_n1 == 0

vcemway reg cheat_c if treatment == 3 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_c_D_t3 = mean_cheat_c_D_t3 + 0.0042487 if treatment == 3 & other_signal_n1 == 0
generate low_cheat_c_D_t3 = mean_cheat_c_D_t3 - 0.0042487 if treatment == 3 & other_signal_n1 == 0

********************************************************************************
*Lower Right Panel Error Bars T4************************************************

vcemway reg honest_c if treatment == 4 & other_signal_n1 == 0, cluster (id group_2)

generate hi_honest_c_D_t4 = mean_honest_c_D_t4 + 0.0218491 if treatment == 4 & other_signal_n1 == 0
generate low_honest_c_D_t4 = mean_honest_c_D_t4 - 0.0218491 if treatment == 4 & other_signal_n1 == 0

vcemway reg honest_d if treatment == 4 & other_signal_n1 == 0, cluster (id group_2)

generate hi_honest_d_D_t4 = mean_honest_d_D_t4 + 0.0348563 if treatment == 4 & other_signal_n1 == 0
generate low_honest_d_D_t4 = mean_honest_d_D_t4 - 0.0348563 if treatment == 4 & other_signal_n1 == 0

vcemway reg cheat_d if treatment == 4 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_d_D_t4 = mean_cheat_d_D_t4 + 0.0433505 if treatment == 4 & other_signal_n1 == 0
generate low_cheat_d_D_t4 = mean_cheat_d_D_t4 - 0.0433505 if treatment == 4 & other_signal_n1 == 0

vcemway reg cheat_c if treatment == 4 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_c_D_t4 = mean_cheat_c_D_t4 + 0.0077143 if treatment == 4 & other_signal_n1 == 0
generate low_cheat_c_D_t4 = mean_cheat_c_D_t4 - 0.0077143 if treatment == 4 & other_signal_n1 == 0

********************************************************************************
*Lower Right Panel Error Bars T5************************************************

vcemway reg honest_c if treatment == 5 & other_signal_n1 == 0, cluster (id group_2)

generate hi_honest_c_D_t5 = mean_honest_c_D_t5 + 0.0162607 if treatment == 5 & other_signal_n1 == 0
generate low_honest_c_D_t5 = mean_honest_c_D_t5 - 0.0162607 if treatment == 5 & other_signal_n1 == 0

vcemway reg honest_d if treatment == 5 & other_signal_n1 == 0, cluster (id group_2)

generate hi_honest_d_D_t5 =  mean_honest_d_D_t5 + 0.0376336 if treatment == 5 & other_signal_n1 == 0
generate low_honest_d_D_t5 = mean_honest_d_D_t5 - 0.0376336 if treatment == 5 & other_signal_n1 == 0

vcemway reg cheat_d if treatment == 5 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_d_D_t5 = mean_cheat_d_D_t5 + 0.0347769 if treatment == 5 & other_signal_n1 == 0
generate low_cheat_d_D_t5 = mean_cheat_d_D_t5 - 0.0347769 if treatment == 5 & other_signal_n1 == 0

vcemway reg cheat_c if treatment == 5 & other_signal_n1 == 0, cluster (id group_2)

generate hi_cheat_c_D_t5 = mean_cheat_c_D_t5 + 0.0071609 if treatment == 5 & other_signal_n1 == 0
generate low_cheat_c_D_t5 = mean_cheat_c_D_t5 - 0.0071609 if treatment == 5 & other_signal_n1 == 0

********************************************************************************
*Lower Right Panel**************************************************************

twoway (bar mean_honest_c_D_t5 order_100 if treatment == 5, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_D_t5 order_200 if treatment == 5, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_D_t5 order_300 if treatment == 5, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_D_t5 order_400 if treatment == 5, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_D_t3 order_500 if treatment == 3, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_D_t3 order_600 if treatment == 3, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_D_t3 order_700 if treatment == 3, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_D_t3 order_800 if treatment == 3, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_D_t4 order_1000 if treatment == 4, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_D_t4 order_1100 if treatment == 4, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_D_t4 order_1200 if treatment == 4, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_D_t4 order_1300 if treatment == 4, color("246 199 0*.5") barw(0.95)) ///
(bar mean_honest_c_D_t2 order_1400 if treatment == 2, color("233 118 0*1") barw(0.95)) (bar mean_honest_d_D_t2 order_1500 if treatment == 2, color("150 79 142*1") barw(0.95)) ///
(bar mean_cheat_d_D_t2 order_1600 if treatment == 2, color("0 103 107*.5") barw(0.95)) (bar mean_cheat_c_D_t2 order_1700 if treatment == 2, color("246 199 0*.5") barw(0.95)) ///
(rcap hi_honest_c_D_t2 low_honest_c_D_t2 order_1400 if treatment ==2, color(black) lwidth(vvvthin)) (rcap hi_honest_d_D_t2 low_honest_d_D_t2 order_1500 if treatment ==2, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_D_t2 low_cheat_d_D_t2 order_1600 if treatment ==2, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_D_t2 low_cheat_c_D_t2 order_1700 if treatment ==2, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_D_t3 low_honest_c_D_t3 order_500 if treatment ==3, color(black) lwidth(vvvthin)) (rcap hi_honest_d_D_t3 low_honest_d_D_t3 order_600 if treatment ==3, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_D_t3 low_cheat_d_D_t3 order_700 if treatment ==3, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_D_t3 low_cheat_c_D_t3 order_800 if treatment ==3, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_D_t4 low_honest_c_D_t4 order_1000 if treatment ==4, color(black) lwidth(vvvthin)) (rcap hi_honest_d_D_t4 low_honest_d_D_t4 order_1100 if treatment ==4, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_D_t4 low_cheat_d_D_t4 order_1200 if treatment ==4, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_D_t4 low_cheat_c_D_t4 order_1300 if treatment ==4, color(black) lwidth(vvvthin)) ///
(rcap hi_honest_c_D_t5 low_honest_c_D_t5 order_100 if treatment ==5, color(black) lwidth(vvvthin)) (rcap hi_honest_d_D_t5 low_honest_d_D_t5 order_200 if treatment ==5, color(black) lwidth(vvvthin)) ///
(rcap hi_cheat_d_D_t5 low_cheat_d_D_t5 order_300 if treatment ==5, color(black) lwidth(vvvthin)) (rcap hi_cheat_c_D_t5 low_cheat_c_D_t5 order_400 if treatment ==5, color(black) lwidth(vvvthin)), ///
xlabel(2.5 "Revealed 12.5" 7.5 "Revealed 25" 12.5 "Hidden 12.5" 17.5 "Hidden 25", valuelabel noticks angle(0)) legend(order(1 "Honest C" 2 "Honest D" 3 "Cheat D" 4 "Cheat C") cols(4)) title("Co-Player: D or D* in t-1") ylabel(0(0.2)1) ysc(r(0 1)) 

graph save "${MY_PATH_OUT}Figure_3_3", replace

gr combine "${MY_PATH_OUT}Figure_3_2" "${MY_PATH_OUT}Figure_3_3", l1("Share choices: all supergames, all rounds") title("Conditional on signal")
graph save "${MY_PATH_OUT}Figure_3_4", replace
********************************************************************************
*Combine Figure 3***************************************************************

graph combine "${MY_PATH_OUT}Figure_3_1" "${MY_PATH_OUT}Figure_3_4", row(2)

********************************************************************************
*Scatter Payoff Cheat D*********************************************************
*Payoff per round***************************************************************

gen ave_payoff = totalpayoff/maxint 
replace ave_payoff = ave_payoff * 100
lab var ave_payoff "Payoff per round"

gen cheat_def_count = cheat_defect if cheat_defect == 1

egen N_cheat_def = count(cheat_def_count), by (id)

gen n_cheat_def_b = N_cheat_def/maxint
lab var n_cheat_def_b "Share Cheat D choices among all choices"

********************************************************************************
*Hidden 25**********************************************************************

grstyle init scheme_1, replace
grstyle set color "189 30 36*.8" "189 30 36*.8" 
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 8pt: subheading axis_title
grstyle set size 8pt: heading
grstyle set size 8pt: tick_label
grstyle set size 8pt: key_label
grstyle set size 8pt: legend
graph twoway (lfit ave_payoff n_cheat_def_b) (scatter ave_payoff n_cheat_def_b) if treatment == 2 & match == 1 & period == 1, title("Hidden 25 (p = 0.034)") ytitle("Payoff per round") yscale(range(12 20)) ylabel(12(2)20)
graph save "${MY_PATH_OUT}Scatter_Cheat_D_T2", replace

reg ave_payoff n_cheat_def_b if treatment == 2 & match == 1 & period == 1

********************************************************************************
*Revealed 25********************************************************************

grstyle init scheme_2, replace
grstyle set color  "0 114 86*.8" "0 114 86*.8"
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 8pt: subheading axis_title
grstyle set size 8pt: heading
grstyle set size 8pt: tick_label
grstyle set size 8pt: key_label
grstyle set size 8pt: legend
graph twoway (lfit ave_payoff n_cheat_def_b) (scatter ave_payoff n_cheat_def_b) if treatment == 3 & match == 1 & period == 1, title("Revealed 25 (p < 0.001)") ytitle("Payoff per round") yscale(range(12 20)) ylabel(12(2)20)
graph save "${MY_PATH_OUT}Scatter_Cheat_D_T3", replace

reg ave_payoff n_cheat_def_b if treatment == 3 & match == 1 & period == 1

********************************************************************************
*Hidden 12.5********************************************************************

grstyle init scheme_3, replace
grstyle set color "189 30 36*.5"  "189 30 36*.5" 
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 8pt: subheading axis_title
grstyle set size 8pt: heading
grstyle set size 8pt: tick_label
grstyle set size 8pt: key_label
grstyle set size 8pt: legend
graph twoway (lfit ave_payoff n_cheat_def_b) (scatter ave_payoff n_cheat_def_b) if treatment == 4 & match == 1 & period == 1, title("Hidden 12.5 (p < 0.001)") ytitle("Payoff per round") yscale(range(12 20)) ylabel(12(2)20)
graph save "${MY_PATH_OUT}Scatter_Cheat_D_T4", replace

reg ave_payoff n_cheat_def_b if treatment == 4 & match == 1 & period == 1

********************************************************************************
*Revealed 12.5******************************************************************

grstyle init scheme_4, replace
grstyle set color "0 114 86*.5" "0 114 86*.5"
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 8pt: subheading axis_title
grstyle set size 8pt: heading
grstyle set size 8pt: tick_label
grstyle set size 8pt: key_label
grstyle set size 8pt: legend
graph twoway (lfit ave_payoff n_cheat_def_b) (scatter ave_payoff n_cheat_def_b) if treatment == 5 & match == 1 & period == 1, title("Revealed 12.5 (p = 0.012)") ytitle("Payoff per round") yscale(range(12 20)) ylabel(12(2)20)
graph save "${MY_PATH_OUT}Scatter_Cheat_D_T5", replace

reg ave_payoff n_cheat_def_b if treatment == 5 & match == 1 & period == 1

********************************************************************************
*Combine for Figure 4***********************************************************

gr combine "${MY_PATH_OUT}Scatter_Cheat_D_T5" "${MY_PATH_OUT}Scatter_Cheat_D_T3" "${MY_PATH_OUT}Scatter_Cheat_D_T4" "${MY_PATH_OUT}Scatter_Cheat_D_T2"
graph save "${MY_PATH_OUT}Scatter_Cheat_D_All", replace

********************************************************************************
*APPENDIX***********************************************************************
********************************************************************************	
*Table A1 - Overview sample*****************************************************

sum female age course experiment_cat_1 experiment_cat_2 experiment_cat_3 experiment_cat_4 if match == 1 & period == 1
sum female age course experiment_cat_1 experiment_cat_2 experiment_cat_3 experiment_cat_4 if treatment == 1 & match == 1 & period == 1
sum female age course experiment_cat_1 experiment_cat_2 experiment_cat_3 experiment_cat_4 if treatment == 5 & match == 1 & period == 1
sum female age course experiment_cat_1 experiment_cat_2 experiment_cat_3 experiment_cat_4 if treatment == 3 & match == 1 & period == 1
sum female age course experiment_cat_1 experiment_cat_2 experiment_cat_3 experiment_cat_4 if treatment == 4 & match == 1 & period == 1
sum female age course experiment_cat_1 experiment_cat_2 experiment_cat_3 experiment_cat_4 if treatment == 2 & match == 1 & period == 1

tabulate female treatment if match == 1 & period == 1, chi2
kwallis age if  match == 1 & period == 1, by(treatment)
tabulate course treatment if match == 1 & period == 1, chi2
tabulate treatment experiment_cat_1 if match == 1 & period == 1, chi2
tabulate treatment experiment_cat_2 if match == 1 & period == 1, chi2
tabulate treatment experiment_cat_3 if match == 1 & period == 1, chi2
tabulate treatment experiment_cat_4 if match == 1 & period == 1, chi2

********************************************************************************
*Table A4 - Cooperation Rates***************************************************
*All Games**********************************************************************

tab cooperation if treatment == 1 & period == 1
tab cooperation if treatment == 5 & period == 1
tab cooperation if treatment == 3 & period == 1
tab cooperation if treatment == 4 & period == 1
tab cooperation if treatment == 2 & period == 1

tab cooperation if treatment == 1 
tab cooperation if treatment == 5 
tab cooperation if treatment == 3
tab cooperation if treatment == 4 
tab cooperation if treatment == 2 

* First Game********************************************************************

tab cooperation if treatment == 1 & period == 1 & match == 1
tab cooperation if treatment == 5 & period == 1 & match == 1
tab cooperation if treatment == 3 & period == 1 & match == 1
tab cooperation if treatment == 4 & period == 1 & match == 1
tab cooperation if treatment == 2 & period == 1 & match == 1

tab cooperation if treatment == 1 & match == 1
tab cooperation if treatment == 5 & match == 1
tab cooperation if treatment == 3 & match == 1
tab cooperation if treatment == 4 & match == 1
tab cooperation if treatment == 2 & match == 1

* Last Five Games***************************************************************

tab cooperation if treatment == 1 & period == 1 & last_5 == 1
tab cooperation if treatment == 5 & period == 1 & last_5 == 1
tab cooperation if treatment == 3 & period == 1 & last_5 == 1
tab cooperation if treatment == 4 & period == 1 & last_5 == 1
tab cooperation if treatment == 2 & period == 1 & last_5 == 1

tab cooperation if treatment == 1 & last_5 == 1
tab cooperation if treatment == 5 & last_5 == 1
tab cooperation if treatment == 3 & last_5 == 1
tab cooperation if treatment == 4 & last_5 == 1
tab cooperation if treatment == 2 & last_5 == 1

********************************************************************************
********************************************************************************

********************************************************************************
*Table A5 - Significance levels*************************************************

* First round, all games********************************************************

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1) & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1) & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1) & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1) & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_3==1) & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_4==1) & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_2==1) & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_4==1) & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_2==1) & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_2==1) & period == 1, cluster (id group_2)

* All rounds, all games*********************************************************

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1), cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1), cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1), cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1), cluster (id group_2)

vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_3==1), cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_4==1), cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_2==1), cluster (id group_2)

vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_4==1), cluster (id group_2)
vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_2==1), cluster (id group_2)

vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_2==1), cluster (id group_2)

* First round, first game********************************************************

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1) & match == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1) & match == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1) & match == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1) & match == 1 & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_3==1) & match == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_4==1) & match == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_2==1) & match == 1 & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_4==1) & match == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_2==1) & match == 1 & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_2==1) & match == 1 & period == 1, cluster (id group_2)

* All rounds, first game********************************************************

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1) & match == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1) & match == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1) & match == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1) & match == 1, cluster (id group_2)

vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_3==1) & match == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_4==1) & match == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_2==1) & match == 1, cluster (id group_2)

vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_4==1) & match == 1, cluster (id group_2)
vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_2==1) & match == 1, cluster (id group_2)

vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_2==1) & match == 1, cluster (id group_2)

* First round, last five games**************************************************

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1) & last_5 == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1) & last_5 == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1) & last_5 == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1) & last_5 == 1 & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_3==1) & last_5 == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_4==1) & last_5 == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_2==1) & last_5 == 1 & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_4==1) & last_5 == 1 & period == 1, cluster (id group_2)
vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_2==1) & last_5 == 1 & period == 1, cluster (id group_2)

vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_2==1) & last_5 == 1 & period == 1, cluster (id group_2)

* All rounds, last five games***************************************************

vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_5==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_3==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_4==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_1 if (treatment_1==1|treatment_2==1) & last_5 == 1, cluster (id group_2)

vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_3==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_4==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_5 if (treatment_5==1|treatment_2==1) & last_5 == 1, cluster (id group_2)

vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_4==1) & last_5 == 1, cluster (id group_2)
vcemway probit cooperation treatment_3 if (treatment_3==1|treatment_2==1) & last_5 == 1, cluster (id group_2)

vcemway probit cooperation treatment_4 if (treatment_4==1|treatment_2==1) & last_5 == 1, cluster (id group_2)

********************************************************************************
*Table A6 - Regression by Treatment*********************************************

vcemway probit cooperation c.match c.period i.game_length_2 i.game_length_3 i.game_length_4 c.age i.female i.course i.experiment_cat_2 i.experiment_cat_3 i.experiment_cat_4 if treatment==1, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r1

vcemway probit cooperation c.match c.period i.game_length_2 i.game_length_3 i.game_length_4 c.age i.female i.course i.experiment_cat_2 i.experiment_cat_3 i.experiment_cat_4 if treatment==5, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r2

vcemway probit cooperation c.match c.period i.game_length_2 i.game_length_3 i.game_length_4 c.age i.female i.course i.experiment_cat_2 i.experiment_cat_3 i.experiment_cat_4 if treatment==3, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r3

vcemway probit cooperation c.match c.period i.game_length_2 i.game_length_3 i.game_length_4 c.age i.female i.course i.experiment_cat_2 i.experiment_cat_3 i.experiment_cat_4 if treatment==4, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r4

vcemway probit cooperation c.match c.period i.game_length_2 i.game_length_3 i.game_length_4 c.age i.female i.course i.experiment_cat_2 i.experiment_cat_3 i.experiment_cat_4 if treatment==2, cluster (id group_2)
margins, dydx(*) atmeans post
est sto r5

esttab r1 r2 r3 r4 r5 using "${OUT_TABLES}Table_A1.rtf", varwidth(12) cells(b(star fmt(4)) se(par fmt(4))) stats(N, fmt(0 4 4)) starlevel(* 0.10 ** 0.05 *** 0.01) legend nobase style(fixed) nocons /// 
replace 

********************************************************************************
*Table A7 - Use of Cheating Options*********************************************

*All Games**********************************************************************

tab honest_c if treatment == 5 & period == 1
tab honest_c if treatment == 3 & period == 1
tab honest_c if treatment == 4 & period == 1
tab honest_c if treatment == 2 & period == 1

tab honest_c if treatment == 5 
tab honest_c if treatment == 3 
tab honest_c if treatment == 4 
tab honest_c if treatment == 2 

tab honest_d if treatment == 5 & period == 1
tab honest_d if treatment == 3 & period == 1
tab honest_d if treatment == 4 & period == 1
tab honest_d if treatment == 2 & period == 1

tab honest_d if treatment == 5 
tab honest_d if treatment == 3 
tab honest_d if treatment == 4 
tab honest_d if treatment == 2 

tab cheat_d if treatment == 5 & period == 1
tab cheat_d if treatment == 3 & period == 1
tab cheat_d if treatment == 4 & period == 1
tab cheat_d if treatment == 2 & period == 1

tab cheat_d if treatment == 5 
tab cheat_d if treatment == 3 
tab cheat_d if treatment == 4 
tab cheat_d if treatment == 2 

tab cheat_c if treatment == 5 & period == 1
tab cheat_c if treatment == 3 & period == 1
tab cheat_c if treatment == 4 & period == 1
tab cheat_c if treatment == 2 & period == 1

tab cheat_c if treatment == 5 
tab cheat_c if treatment == 3 
tab cheat_c if treatment == 4 
tab cheat_c if treatment == 2 

*First Game*********************************************************************

tab honest_c if treatment == 5 & period == 1 & match == 1
tab honest_c if treatment == 3 & period == 1 & match == 1
tab honest_c if treatment == 4 & period == 1 & match == 1
tab honest_c if treatment == 2 & period == 1 & match == 1

tab honest_c if treatment == 5 & match == 1
tab honest_c if treatment == 3 & match == 1
tab honest_c if treatment == 4 & match == 1
tab honest_c if treatment == 2 & match == 1

tab honest_d if treatment == 5 & period == 1 & match == 1
tab honest_d if treatment == 3 & period == 1 & match == 1
tab honest_d if treatment == 4 & period == 1 & match == 1
tab honest_d if treatment == 2 & period == 1 & match == 1

tab honest_d if treatment == 5 & match == 1
tab honest_d if treatment == 3 & match == 1
tab honest_d if treatment == 4 & match == 1
tab honest_d if treatment == 2 & match == 1

tab cheat_d if treatment == 5 & period == 1 & match == 1
tab cheat_d if treatment == 3 & period == 1 & match == 1
tab cheat_d if treatment == 4 & period == 1 & match == 1
tab cheat_d if treatment == 2 & period == 1 & match == 1

tab cheat_d if treatment == 5 & match == 1
tab cheat_d if treatment == 3 & match == 1
tab cheat_d if treatment == 4 & match == 1
tab cheat_d if treatment == 2 & match == 1

tab cheat_c if treatment == 5 & period == 1 & match == 1
tab cheat_c if treatment == 3 & period == 1 & match == 1
tab cheat_c if treatment == 4 & period == 1 & match == 1
tab cheat_c if treatment == 2 & period == 1 & match == 1

tab cheat_c if treatment == 5 & match == 1
tab cheat_c if treatment == 3 & match == 1
tab cheat_c if treatment == 4 & match == 1
tab cheat_c if treatment == 2 & match == 1

*Last 5 Games*******************************************************************

tab honest_c if treatment == 5 & period == 1 & last_5 == 1
tab honest_c if treatment == 3 & period == 1 & last_5 == 1
tab honest_c if treatment == 4 & period == 1 & last_5 == 1
tab honest_c if treatment == 2 & period == 1 & last_5 == 1

tab honest_c if treatment == 5 & last_5 == 1
tab honest_c if treatment == 3 & last_5 == 1
tab honest_c if treatment == 4 & last_5 == 1
tab honest_c if treatment == 2 & last_5 == 1

tab honest_d if treatment == 5 & period == 1 & last_5 == 1
tab honest_d if treatment == 3 & period == 1 & last_5 == 1
tab honest_d if treatment == 4 & period == 1 & last_5 == 1
tab honest_d if treatment == 2 & period == 1 & last_5 == 1

tab honest_d if treatment == 5 & last_5 == 1
tab honest_d if treatment == 3 & last_5 == 1
tab honest_d if treatment == 4 & last_5 == 1
tab honest_d if treatment == 2 & last_5 == 1

tab cheat_d if treatment == 5 & period == 1 & last_5 == 1
tab cheat_d if treatment == 3 & period == 1 & last_5 == 1
tab cheat_d if treatment == 4 & period == 1 & last_5 == 1
tab cheat_d if treatment == 2 & period == 1 & last_5 == 1

tab cheat_d if treatment == 5 & last_5 == 1
tab cheat_d if treatment == 3 & last_5 == 1
tab cheat_d if treatment == 4 & last_5 == 1
tab cheat_d if treatment == 2 & last_5 == 1

tab cheat_c if treatment == 5 & period == 1 & last_5 == 1
tab cheat_c if treatment == 3 & period == 1 & last_5 == 1
tab cheat_c if treatment == 4 & period == 1 & last_5 == 1
tab cheat_c if treatment == 2 & period == 1 & last_5 == 1

tab cheat_c if treatment == 5 & last_5 == 1
tab cheat_c if treatment == 3 & last_5 == 1
tab cheat_c if treatment == 4 & last_5 == 1
tab cheat_c if treatment == 2 & last_5 == 1

********************************************************************************
*Figure A1***********************************************************************

discard
grstyle init
grstyle set color gs14%30 "233 118 0*1" "150 79 142*1" "0 103 107*.5" "246 199 0*.5"
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal
grstyle linestyle legend none
grstyle ci_area black
grstyle set size 6pt: subheading axis_title
grstyle set size 6pt: heading
grstyle set size 6pt: tick_label
grstyle set size 6pt: key_label
grstyle set size 6pt: legend

********************************************************************************
*Average use of options over period for Treatment Hidden 25*********************

bys period: egen mean_honest_c_t2_1 = mean(honest_c) if treatment == 2
lab var mean_honest_c_t2_1 "Honest C"

bys period: egen mean_honest_d_t2_1 = mean(honest_d) if treatment == 2
lab var mean_honest_d_t2_1 "Honest D"

bys period: egen mean_cheat_d_t2_1 = mean(cheat_d) if treatment == 2
lab var mean_cheat_d_t2_1 "Cheat D"

bys period: egen mean_cheat_c_t2_1 = mean(cheat_c) if treatment == 2
lab var mean_cheat_c_t2_1 "Cheat C"

twoway (bar upper period if inrange(period,11,20), bcolor(gs14%30) base(0) lwidth(0)) (connected mean_honest_c_t2_1 period if period <21, sort msize(small) msymbol(o) lcolor("233 118 0*1") xtitle("Round")) (connected mean_honest_d_t2_1 period if period <21, sort msize(small) msymbol(s) lcolor("150 79 142*1")) /// 
(connected mean_cheat_d_t2_1 period if period <21, sort msize(small) msymbol(d) lcolor("0 103 107*.5")) (connected mean_cheat_c_t2_1 period if period <21, sort msize(small) msymbol(t) lcolor("246 199 0*.5")), legend(title(Hidden 25) order(2 3 4 5)) /// 
ylabel(0(0.2)1) ytitle("Average share: all supergames by round")

graph save "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T2", replace

*Average use of options over period for Treatment Revealed 25*******************

bys period: egen mean_honest_c_t3_1 = mean(honest_c) if treatment == 3
lab var mean_honest_c_t3_1 "Honest C"

bys period: egen mean_honest_d_t3_1 = mean(honest_d) if treatment == 3
lab var mean_honest_d_t3_1 "Honest D"

bys period: egen mean_cheat_d_t3_1 = mean(cheat_d) if treatment == 3
lab var mean_cheat_d_t3_1 "Cheat D"

bys period: egen mean_cheat_c_t3_1 = mean(cheat_c) if treatment == 3
lab var mean_cheat_c_t3_1 "Cheat C"

twoway (bar upper period if inrange(period,11,20), bcolor(gs14%30) base(0) lwidth(0)) (connected mean_honest_c_t3_1 period if period <21, sort msize(small) msymbol(o) lcolor("233 118 0*1") xtitle("Round")) (connected mean_honest_d_t3_1 period if period <21, sort msize(small) msymbol(s) lcolor("150 79 142*1")) /// 
(connected mean_cheat_d_t3_1 period if period <21, sort msize(small) msymbol(d) lcolor("0 103 107*.5")) (connected mean_cheat_c_t3_1 period if period <21, sort msize(small) msymbol(t) lcolor("246 199 0*.5")), legend(title(Revealed 25) order(2 3 4 5)) /// 
ylabel(0(0.2)1) ytitle("Average share: all supergames by round")

graph save "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T3", replace

*Average use of options over period for Treatment Hidden 12.5*******************

bys period: egen mean_honest_c_t4_1 = mean(honest_c) if treatment == 4
lab var mean_honest_c_t4_1 "Honest C"

bys period: egen mean_honest_d_t4_1 = mean(honest_d) if treatment == 4
lab var mean_honest_d_t4_1 "Honest D"

bys period: egen mean_cheat_d_t4_1 = mean(cheat_d) if treatment == 4
lab var mean_cheat_d_t4_1 "Cheat D"

bys period: egen mean_cheat_c_t4_1 = mean(cheat_c) if treatment == 4
lab var mean_cheat_c_t4_1 "Cheat C"

twoway (bar upper period if inrange(period,11,20), bcolor(gs14%30) base(0) lwidth(0)) (connected mean_honest_c_t4_1 period if period <21, sort msize(small) msymbol(o) lcolor("233 118 0*1") xtitle("Round")) (connected mean_honest_d_t4_1 period if period <21, sort msize(small) msymbol(s) lcolor("150 79 142*1")) /// 
(connected mean_cheat_d_t4_1 period if period <21, sort msize(small) msymbol(d) lcolor("0 103 107*.5")) (connected mean_cheat_c_t4_1 period if period <21, sort msize(small) msymbol(t) lcolor("246 199 0*.5")), legend(title(Hidden 12.5) order(2 3 4 5)) /// 
ylabel(0(0.2)1) ytitle("Average share: all supergames by round")

graph save "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T4", replace

*Average use of options over period for Treatment Hidden 12.5*******************

bys period: egen mean_honest_c_t5_1 = mean(honest_c) if treatment == 5
lab var mean_honest_c_t5_1 "Honest C"

bys period: egen mean_honest_d_t5_1 = mean(honest_d) if treatment == 5
lab var mean_honest_d_t5_1 "Honest D"

bys period: egen mean_cheat_d_t5_1 = mean(cheat_d) if treatment == 5
lab var mean_cheat_d_t5_1 "Cheat D"

bys period: egen mean_cheat_c_t5_1 = mean(cheat_c) if treatment == 5
lab var mean_cheat_c_t5_1 "Cheat C"

twoway (bar upper period if inrange(period,11,20), bcolor(gs14%30) base(0) lwidth(0)) (connected mean_honest_c_t5_1 period if period <21, sort msize(small) msymbol(o) lcolor("233 118 0*1") xtitle("Round")) (connected mean_honest_d_t5_1 period if period <21, sort msize(small) msymbol(s) lcolor("150 79 142*1")) /// 
(connected mean_cheat_d_t5_1 period if period <21, sort msize(small) msymbol(d) lcolor("0 103 107*.5")) (connected mean_cheat_c_t5_1 period if period <21, sort msize(small) msymbol(t) lcolor("246 199 0*.5")), legend(title(Revealed 12.5) order(2 3 4 5)) /// 
ylabel(0(0.2)1) ytitle("Average share: all supergames by round")

graph save "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T5", replace

*Combine for Figure A1**********************************************************

gr combine "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T5.gph" "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T3.gph" "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T4.gph" "${MY_PATH_OUT}Mean_Choice_PerRound_AllGames_T2.gph" 
graph save "${MY_PATH_OUT}Figure_30_Mean_Choice_PerRound_AllGames_AllTreatments", replace

********************************************************************************
********************************************************************************




































