* Project: MahindiMaster
* Created: 10/30/19 (JC)
* Last modified: 1/24/21 - JC
* Stata v.16

* Note: file directory is set in section 0 of master.do
* users only need to change the location of their path there

* does
	* Generates tables and graphs in order

* assumes
	* Add any dependencies here

* TO DO:
	* Move variable generation (Section 1) to different .do file

* **********************************************************************
* Start log file
    cap: log close
	log using "$dataWork/analysis/code/logs/paper_tables_graphs", replace

	set graphics off

* **********************************************************************
* 1 - Descriptive section headers
* **********************************************************************
* Open analysis data
	use "$dataWork/analysis/MMclean.dta", clear

	/*NOTE:
	cmean_pctchange: percent change for mean of subj. dist with fertilizer
	cvar_pctchange: percent change for variance of subj. dist with fertilizer
	dmean_pctchange: percent change for mean of subj. dist with fertilizer and lime
	dvar_pctchange: percent change for variance of subj. dist with fertilizer
	order_10pctshift: dummy if dap, can, OR lime order changed by more than 10%
	lime1: amount of lime ordered before playing the game (kg)
	lime2: amount of lime ordered after playing the game (kg)
	ph2: pH squared

	var1: 1 indicates occurred before game was played
	var2: 2 indicates occurred after game was played
*/

* **********************************************************************
* 2 - Labeling and variable creation (TO DO: MOVE)
* **********************************************************************

gen  lime12=.
replace lime12=0 if lime1==0 & lime2==0
label 	var lime12 "Positive lime order, pre and/or post"
replace	lime12=1 if lime12==.
label def lime12 1"Yes" 0"No"
label val lime12 lime12

gen 	can_share2=100-dap_share2-lime_share2
gen 	can_share1=100-dap_share1-lime_share1

gen 	can_diff=can2-can1
gen 	dap_diff=dap2-dap1

gen 	can_valdiff=can_val2-can_val1
gen		dap_valdiff=dap_val2-dap_val1
rename	lime_diff lime_valdiff

/*Farmer characteristics*/
label			var p_size_acres "Sampled plot size (acres)"
label			var fertseason_long "No. of seasons used fertilizer (long rains)"
label			var hybseason_long "No. of seasons used hybrids  (long rains)"
label 			var num_correct "No. farming quiz questions correct"
label 			var num_dontknow "No. farming quiz questions=don’t know"




*Label variables
label var ph "pH of sampled plot"
label var ph2 "pH Squared"
label var ph_step "pH"
label var lime_share1 "Lime Share (Pre)"
label var lime1 "Kg Lime (Pre)"

label var cmean_pctchange "\% Change, Mean (Fertilizer)"
label var dmean_pctchange "\% Change, Mean (Fertilizer and Lime)"
label var c_cv_pctchange "\% Change, CV (Fertilizer)"
label var d_cv_pctchange "\% Change, CV (Fertilizer and Lime)"

label var fertseason_sl "No. of Seasons Used Fertilizer"
label var fertseason_l "No. of seasons used fertilizer (long rains)"

label var over_conf "Overconfident (0/1)"
label var conf_diff "No. Believed Correct - No. Correct"
label var conf_pctdiff "Pct. Difference: No. Believed Correct - No. Correct"

label var totalrounds "Total Number of Rounds"
label var dap_share1 "DAP share (pre)"
label var can_share1 "CAN share (pre)"
label var cec "CEC of sampled plot"

label			var dap1"DAP (kg)"
label 			var can1"CAN (kg)"

label var c_mean1 "Mean (Fertilizer)"
label var d_mean1 "Mean (Fertilizer and Lime)"
label var c_cv1 "CV (Fertilizer)"
label var d_cv1 "CV (Fertilizer and Lime)"



label var usedap `"Uses any DAP in a 'normal' year"'
label var usecan `"Uses any CAN in a 'normal' year"'
label var uselime `"Uses any lime in a 'normal' year"'

label var dap_game "Amount of DAP (kg) across rounds"
label var can_game "Amount of CAN (kg) across rounds"
label var lime_game "Amount of Lime (kg) across rounds"
label var yield_game "Yields (kg/acre) obtained in game"
label var share_dap_game "Share of rounds with DAP"
label var share_can_game "Share of rounds with CAN (conditional on avail.)"
label var share_lime_game "Share of rounds with lime (conditional on avail.)"
label var share_nofert "Share of rounds with no fertilizer"
label var rand_rain "Random rainfall scenario (1=poor, 2=normal, 3=good)"
label var choice_rain "Chosen rainfall scenario (1=poor, 2=normal, 3=good)"
label var totalrounds "Rounds played"

label var b_mean1 "Mean (no fertilizer)"
label var c_mean1 "Mean (fertilizer)"
label var d_mean1 "Mean (fertilizer + lime)"
label var c_cv1 "CV (fertilizer)"
label var d_cv1 "CV (fertilizer and lime)"

label var dap1 "DAP (kg)"
label var can1 "CAN (kg)"
label var lime1 "Lime (kg)"
label var dap_val1 "DAP value (pre)"
label var can_val1 "CAN value (pre)"
label var lime_val1 "Lime value (pre)"

* **********************************************************************
* Table 1 - Panel A - Farmer characteristics
* **********************************************************************

	destring p_size_acres, replace

	eststo clear

	estpost summarize fertseason_long hybseason_long usedap usecan uselime ph cec p_size_acres num_correct num_dontknow over_conf
	esttab using "$dataWork/analysis/output/tables/farmchar.tex", cells("mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") unstack ///
	collabels("Mean""Std Deviation""Min""Max", begin("Panel A: Farmer and plot characteristics"))  ///
	eqlabels(none) label noobs nonum legend style (tex) mlabels (none) ///
		postfoot("& & & & \\" `"\hline"') ///
		prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Baseline characteristics and game interactions }"' ///
	`"\label{tab:farmchar}"' ///
	`"\begin{tabular}{l cccc}"' ///
	`"\hline"' `"\hline"') replace

* **********************************************************************
* Table 1 - Panel B - Game data
* **********************************************************************
	eststo clear
	estpost summarize dap_game can_game lime_game yield_game share_dap_game share_can_game share_lime_game share_nofert rand_rain choice_rain  totalrounds

	esttab using "$dataWork/analysis/output/tables/farmchar.tex", cells("mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") unstack ///
	collabels("Mean""Std Deviation""Min""Max", begin("Panel B: Game play")) ///
	eqlabels(none) label nonum legend style (tex) mlabels (none) ///
	prehead (%) ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\end{threeparttable}"' `"\end{table}"') 	append


* **********************************************************************
* Table 2 - Descriptives on fertilizer orders
* **********************************************************************
	/*Order*/


		foreach			var in	dap can lime dap_val can_val lime_val {


gen				diff_`var'_1=`var'2-`var'1    
                        /*generate differences in kg orders and value orders*/

						}

		*DAP*

		eststo clear

		reg				diff_dap_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum dap1
		estadd matrix mean1=r(mean)
		sum dap2
		estadd matrix mean2=r(mean)


		esttab using "$dataWork/analysis/output/tables/fertorder.tex" , cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels ("Mean(pre)" "Mean(post)" "\$t\$-statistic") ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "DAP (kg)") ///
		postfoot(%) ///
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\caption{Fertilizer Orders}"' ///
		`"\label{tab:fertorder}"' ///
		`"\begin{tabular}{l ccc}"' ///
		`"\hline"' `"\hline"') replace

		*CAN*
		eststo clear

		reg				diff_can_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum can1
		estadd matrix mean1=r(mean)
		sum can2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/fertorder.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "CAN (kg)") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append

		*Lime*

		eststo clear

		reg				diff_lime_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum lime1
		estadd matrix mean1=r(mean)
		sum lime2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/fertorder.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "Lime (kg)") ///
		prehead (%)posthead(%) ///
		postfoot(`"\hline"') 	append


		*DAP value*

		eststo clear

		reg				diff_dap_val_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum dap_val1
		estadd matrix mean1=r(mean)
		sum dap_val2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/fertorder.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "DAP (KHS)") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append


		*CAN value*

		eststo clear

		reg				diff_can_val_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum can_val1
		estadd matrix mean1=r(mean)
		sum can_val2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/fertorder.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "CAN (KHS)") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append

		*Lime value*

		eststo clear

		reg				diff_lime_val_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum lime_val1
		estadd matrix mean1=r(mean)
		sum lime_val2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/fertorder.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "Lime (KHS)") ///
		prehead (%)posthead(%) ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Note: Differences based on clustering at the village level}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append


* **********************************************************************
* Table 3 - Subjective expectations
* **********************************************************************

	/*Subjective expectations*/

	foreach			var in b_mean c_mean d_mean c_cv d_cv{

					local			i=`i'+1
					cap gen				diff_`var'_1=`var'2-`var'1
					}


		*Mean no fertilizer*
		eststo clear

		reg			b_mean1, vce(cluster village)
		sum b_mean1
		estadd matrix mean1=r(mean)

		esttab using "$dataWork/analysis/output/tables/subj_ttest.tex" , cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels ("Mean(pre)" "Mean(post)" "\$t\$-statistic") ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "Mean (no fertilizer)") ///
		postfoot(%) ///
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\caption{Subjective expectations pre- and post-game}"' ///
		`"\label{tab:subj ttest}"' ///
		`"\begin{tabular}{l ccc}"' ///
		`"\hline"' `"\hline"') replace

		*Mean fertilizer*
		eststo clear

		reg				diff_c_mean_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum c_mean1
		estadd matrix mean1=r(mean)
		sum c_mean2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/subj_ttest.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "Mean (fertilizer)") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append


		*Mean fertilizer*
		eststo clear

		reg				diff_d_mean_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum d_mean1
		estadd matrix mean1=r(mean)
		sum d_mean2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/subj_ttest.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "Mean (fertilizer + lime)") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append

		
		*CV no fertilizer*
		eststo clear
				
		reg				b_cv1, vce(cluster village)
		sum b_cv1
		estadd matrix mean1=r(mean)
		
		esttab using "$dataWork/analysis/output/tables/subj_ttest.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) /// 
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "CV (no fertilizer)") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append

		*CV fertilizer*
		eststo clear

		reg				diff_c_cv_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum c_cv1
		estadd matrix mean1=r(mean)
		sum c_cv2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/subj_ttest.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "CV (fertilizer)") ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append

		*CV fertilizer + lime*

		eststo clear

		reg				diff_d_cv_1, vce(cluster village)
		mat				M=r(table)
		estadd 	matrix t=M[3,1]
		sum d_cv1
		estadd matrix mean1=r(mean)
		sum d_cv2
		estadd matrix mean2=r(mean)

		esttab using "$dataWork/analysis/output/tables/subj_ttest.tex", cells("mean1(fmt(a2)) mean2(fmt(a2)) t(fmt(a2))") drop(_cons) collabels (none) ///
		eqlabels(none)  noobs nonum legend style (tex) mlabels (none) varlabels(c1 "CV (fertilizer + lime)") ///
		prehead (%)posthead(%) ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Note: Differences based on clustering at the village level}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append


* **********************************************************************
* Table 4 - pH and lime orders
* **********************************************************************
* Regress different measures of lime orders on soil pH

	*Hypothesis A3
	*create dummy to standardize lime order sample size

	gen missing_lime = missing(lime_share2) | missing(lime_share1) ///
    | missing(lime_valdiff) | missing(lime2) | missing(lime1)

eststo clear
/*    * MHT 
    reg lime_share2 ph ph2 lime_share1  if !missing_lime, r
    parmest, format(estimate min95 max95 %8.2f p %8.1e) stars(0.1 0.05 0.01) ///
    list(parm estimate min95 max95 p, clean noobs) ///
    saving("$dataWork/analysis/output/MHTparms1.dta", replace)
    
    reg lime_share2 i.ph_step lime_share1  if !missing_lime, r
    parmest, format(estimate min95 max95 %8.2f p %8.1e) stars(0.1 0.05 0.01) ///
    list(parm estimate min95 max95 p, clean noobs) ///
    saving("$dataWork/analysis/output/MHTparms2.dta", replace)
    
    reg lime_valdiff ph ph2 if !missing_lime, r
    parmest, format(estimate min95 max95 %8.2f p %8.1e) stars(0.1 0.05 0.01) ///
    list(parm estimate min95 max95 p, clean noobs) ///
    saving("$dataWork/analysis/output/MHTparms3.dta", replace)
    
    reg lime_valdiff i.ph_step if !missing_lime, r
    parmest, format(estimate min95 max95 %8.2f p %8.1e) stars(0.1 0.05 0.01) ///
    list(parm estimate min95 max95 p, clean noobs) ///
    saving("$dataWork/analysis/output/MHTparms4.dta", replace)
    
    reg lime2 ph ph2 lime1  if !missing_lime, r
    parmest, format(estimate min95 max95 %8.2f p %8.1e) stars(0.1 0.05 0.01) ///
    list(parm estimate min95 max95 p, clean noobs) ///
    saving("$dataWork/analysis/output/MHTparms5.dta", replace)
    
    reg lime2 i.ph_step lime1  if !missing_lime, r
    parmest, format(estimate min95 max95 %8.2f p %8.1e) stars(0.1 0.05 0.01) ///
    list(parm estimate min95 max95 p, clean noobs) ///
    saving("$dataWork/analysis/output/MHTparms6.dta", replace)
    
    preserve
    use "$dataWork/analysis/output/MHTparms1.dta", clear
    
    forval i = 2/6 {
        append using "$dataWork/analysis/output/MHTparms`i'.dta"
    }
        
    qqvalue p , qvalue(qbonf)
    qqvalue p , method(sidak) qvalue(qsidak)
    qqvalue p , method(simes) qvalue(qsimes)
    list 
    
    parmest, format(estimate min90 max90 %8.2f p %8.1e) stars(0.1 0.05 0.01) ///
    list(parm label estimate min95 max95 p, clean noobs)
    
  * wyoung controls the family-wise error rate
  * can only do it for a fixed main outcome variable 
  * so have to do ph separately from ph_step
  
   wyoung, cmd("reg lime_share2 c.ph##c.ph lime_share1  if !missing_lime" ///
    "reg lime_valdiff c.ph##c.ph if !missing_lime" ///
    "reg lime2 c.ph##c.ph lime1  if !missing_lime") ///
    familyp(c.ph) seed(86279787) bootstraps(199)
    
    wyoung, cmd("reg lime_share2 i.ph_step lime_share1  if !missing_lime" ///
    "reg lime_valdiff i.ph_step if !missing_lime" ///
    "reg lime2 i.ph_step lime1 if !missing_lime") ///
    familyp(ph_step) seed(86279787) bootstraps(199)
*/
    
*/    
    
	eststo clear

	reg lime_share2 ph ph2 lime_share1  if !missing_lime, r
    eststo lime1
	sum lime_share2 if !missing_lime
	eststo lime1, add(mean r(mean))

	reg lime_share2 i.ph_step lime_share1  if !missing_lime, r
	eststo lime2
	sum lime_share2 if !missing_lime
	eststo lime2, add(mean r(mean))

	reg lime_valdiff ph ph2 if !missing_lime, r
	eststo lime3
	sum lime_valdiff if !missing_lime
	eststo lime3, add(mean r(mean))

	reg lime_valdiff i.ph_step if !missing_lime, r
	eststo lime4
	sum lime_valdiff if !missing_lime
	eststo lime4, add(mean r(mean))

	reg lime2 ph ph2 lime1  if !missing_lime, r
	eststo lime5
	sum lime2 if !missing_lime
	eststo lime5, add(mean r(mean))

	reg lime2 i.ph_step lime1  if !missing_lime, r
	eststo lime6
	sum lime2 if !missing_lime
	eststo lime6, add(mean r(mean))

* vce(cluster village)
    

	*Export regression results
	*Table: pH and Lime Orders (\label{tab:A3})


 	esttab lime5 lime6 lime1 lime2 lime3 lime4 using "$dataWork/analysis/output/tables/HA3.tex",  label nonum legend ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2) label()) se(par fmt(2) label())) stats(r2 N  mean, fmt(2 0 2) labels ("\$R^2\$" "N" "Mean of dep. var:")) style(tex) ///
	mlabels("Amt. (kg)""Amt. (kg)""Share""Share""(post-pre)""(post-pre)") substitute("<" "$<$" ">" "$>$") collabels(none) ///
	varlabels (ph "pH" ph2 "pH$^2$" lime1 "Kg Lime (pre)" lime_share1 "Lime share (pre)"  _cons Intercept) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Post-game lime orders by soil pH}"' ///
	`"\label{tab:A3}"' ///
	`"\begin{tabular}{l cccccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable for (1) and (2) is amount of lime in kilograms ordered post-game, for (3) and (4) is share of the value of the post-game order allocated to lime, and for (5) and (6) is the difference in the value of the order allocated to lime (post-pre). \\ The omitted pH category is pH < 5.5. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') order (ph ph2 4.ph_step 5.ph_step 6.ph_step 7.ph_step) drop(3.ph_step) replace

* **********************************************************************
* Figure 3 - pH and lime orders - (\label{fig:ph})
* **********************************************************************    
    
*Figure 3*
	*Table: Average share of post-game order allocated to inputs, by soil pH 


	reg dap_share2 dap_share1 i.ph_step if !missing_lime, r
	margins ph_step
	marginsplot, scheme(plotplain) ylabel(0(10)70) recastci(rarea) ci1opts(color(gs13%40) lcolor(bg%0)) plot1opts(msymbol(i)) ///
	title("(A)") subtitle("DAP") ytitle("Average share of post-game order to DAP") saving(ph_dapshare,replace)
	graph export "$dataWork/analysis/output/figures/ph_dapshare.pdf", replace


	reg can_share2 can_share1 i.ph_step if !missing_lime, r
	margins ph_step
	marginsplot, scheme(plotplain) ylabel(0(10)70) recastci(rarea) ci1opts(color(gs13%40) lcolor(bg%0)) plot1opts(msymbol(i)) ///
	title("(B)") subtitle("CAN") ///
    ytitle("Average share of post-game order to CAN") saving(ph_canshare, replace)
	graph export "$dataWork/analysis/output/figures/ph_canshare.pdf", replace


	reg lime_share2 lime_share1 i.ph_step if !missing_lime, r
	margins ph_step
	marginsplot, scheme(plotplain) ylabel(0(10)70)  recastci(rarea) ci1opts(color(gs13%40) lcolor(bg%0)) plot1opts(msymbol(i)) ///
	title("(C)") subtitle("Lime") ytitle("Average share of post-game order to lime") saving(ph_limeshare, replace)
	graph export "$dataWork/analysis/output/figures/ph_limeshare.pdf", replace


	graph combine  ph_dapshare.gph ph_canshare.gph ph_limeshare.gph, row(1) ycomm iscale(1) scheme(plotplain) xsize(12.5) 
	graph export "$dataWork/analysis/output/figures/ph_dapcanlime.pdf", replace

* **********************************************************************
* Table 5 - Orders and farming ability - (\label{tab:c2order})
* ********************************************************************** 


*Table 5*

	gen missing_order = missing(dap_pctchange) | missing(can_pctchange) | missing(lime_valdiff) | missing(dap_share2) | missing(dap_share1) ///
	| missing(lime_share2) | missing(lime_share1) | missing(lime2) | missing(lime1)
	eststo clear

	eststo clear

	reg dap_valdiff num_correct dap_val1 if !missing_order, r
	eststo dap_valdiff_c

	reg can_valdiff num_correct can_val1 if !missing_order, r
	eststo can_valdiff_c

	reg lime_valdiff num_correct lime_val1 if !missing_order, r
	eststo lime_valdiff_c

	reg dap_valdiff  num_correct num_dontknow dap_val1 if !missing_order, r
	eststo dap_valdiff_cdk

	reg can_valdiff  num_correct num_dontknow can_val1 if !missing_order, r
	eststo can_valdiff_cdk

	reg lime_valdiff  num_correct num_dontknow lime_val1 if !missing_order, r
	eststo lime_valdiff_cdk

	*title too long in table, divide in two lines
	local titles "& DAP & CAN & Lime & DAP & CAN & Lime \\"
	local subtitles "& (post-pre) & (post-pre)& (post-pre) & (post-pre) & (post-pre) & (post-pre) \\ \hline"

	*Export regression results
	*Table: Orders and farming ability (\label{tab:c2order})

	esttab dap_valdiff_c can_valdiff_c lime_valdiff_c dap_valdiff_cdk can_valdiff_cdk lime_valdiff_cdk using "$dataWork/analysis/output/tables/c2order.tex",  ///
	label nonum legend varlabels(_cons "Intercept" num_dontknow "No. questions=‘Don’t know’" num_correct "No. questions correct") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2) label()) se(par fmt(2) label())) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$")) style(tex) ///
    substitute("<" "$<$" ">" "$>$") collabels(none) mlabels(none) ///
	posthead("`titles'" "`subtitles'") ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Orders and farming ability}"' ///
	`"\label{tab:c2order}"' ///
	`"\begin{tabular}{l cccccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable for (1) and (4) is the difference in value of the order allocated to DAP, for (2) and (5) is the difference in value of the order allocated to CAN, and for (3) and (6) is the difference in the value of the order allocated to lime (post-pre). \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') order (num_correct num_dontknow dap_val1 can_val1 lime_val1) replace

* **********************************************************************
* Table 6 - Updating and farmer ability / knowledge (\label{tab:C2beliefs})
* **********************************************************************     

*Table 6*

	*independent variable: number questions correct and number don't know
	eststo clear
	reg cmean_pctchange num_correct , r
	sum cmean_pctchange
	eststo cmean_nc, add(mean r(mean))

	reg cmean_pctchange num_dontknow num_correct, r
	sum cmean_pctchange
	eststo cmean_dk, add(mean r(mean))
	
	reg cmean_pctchange num_dontknow num_correct over_conf, r
	sum cmean_pctchange
	eststo cmean_dkc, add(mean r(mean))

	reg dmean_pctchange num_correct, r
	sum dmean_pctchange
	eststo dmean_nc, add(mean r(mean))
	
	reg dmean_pctchange num_dontknow num_correct, r
	sum dmean_pctchange
	eststo dmean_dk, add(mean r(mean))
	
	reg dmean_pctchange num_dontknow num_correct over_conf, r
	sum dmean_pctchange
	eststo dmean_dkc, add(mean r(mean))

	*title too long in table, divide in two lines
	local titles "& $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean \\"
	local subtitles "& fert. & fert. & fert. & fert+lime & fert+lime & fert+lime \\ \hline"

	*Export regression results
	*Table: Updating and Farming Ability (\label{tab:C2beliefs})
	esttab cmean_nc cmean_dk cmean_dkc dmean_nc dmean_dk dmean_dkc ///
	using "$dataWork/analysis/output/tables/C2beliefs.tex", label nonum legend style(tex) ///
	varlabels(_cons "Intercept" num_dontknow "No. q's = Don't know" num_correct "No. correct") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) ///
	se(par fmt(2))) stats(r2 N  mean, fmt(2 0 2) ///
	labels ("\$R^2\$" "\$N\$" "Mean of dep. var:"))  ///
	substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
	posthead("`titles'" "`subtitles'") ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Belief revisions}"' ///
	`"\label{tab:C2beliefs}"' ///
	`"\begin{tabular}{l ccc ccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"') replace

	

* **********************************************************************
* Figure 4 - Average share of rounds played with non-zero input amounts
*            by pH - (\label{fig:games_dapcanlime})
* **********************************************************************     
*Figure: Average share of rounds played with non-zero input amounts, by soil pH 

		eststo clear

		reg share_dap_game  i.ph_step, r
		margins ph_step
		marginsplot, scheme(plotplain) title ("(A)")subtitle("DAP")  ytitle("Average share of rounds played with DAP") recastci(rarea) ///
		ci1opts(color(gs13%40) lcolor(bg%0)) plot1opts(msymbol(i)) saving(margins_dap_share, replace)
		graph export "$dataWork/analysis/output/figures/margins_dap_share.pdf", replace

		reg share_can_game i.ph_step, r
		margins ph_step
		marginsplot, scheme(plotplain)  title ("(B)")subtitle("CAN")  ytitle("Average share of rounds played with CAN") recastci(rarea) ///
		ci1opts(color(gs13%40) lcolor(bg%0)) plot1opts(msymbol(i)) saving(margins_can_share, replace)
		graph export "$dataWork/analysis/output/figures/margins_can_share.pdf", replace


		reg share_lime_game i.ph_step, r
		margins ph_step
		marginsplot, scheme(plotplain)  title ("(C)")subtitle("Lime") ytitle("Average share of rounds played with lime") recastci(rarea) ///
		ci1opts(color(gs13%40) lcolor(bg%0)) plot1opts(msymbol(i))saving(margins_lime_share, replace)
		graph export "$dataWork/analysis/output/figures/margins_lime_share.pdf", replace


		graph combine  margins_dap_share.gph margins_can_share.gph margins_lime_share.gph, row(1) ycomm iscale(1) scheme(plotplain) xsize(12.5)
		graph export "$dataWork/analysis/output/figures/games_dapcanlime.pdf", replace

* **********************************************************************
* Table 7 - Share of rounds with DAP (\label{tab:D1dap})
* **********************************************************************     
     
*Table 7*

	*Hypothesis D1

	*Table: Share of Rounds with DAP (\label{tab:D1dap})

	*create dummy to standardize confidence sample size
	gen missing_conf = missing(conf_diff) | missing(conf_pctdiff) | missing(over_conf)

	eststo clear

	foreach var of varlist conf_pctdiff conf_diff  {
		reg share_can_game `var' num_correct usecan if !missing_conf, r
		sum share_can_game if !missing_conf
		eststo `var', add(mean r(mean))
		reg share_lime_game `var' num_correct if !missing_conf, r
		sum share_lime_game if !missing_conf
		eststo `var'2, add(mean r(mean))
		reg share_dap_game `var' num_correct usedap if !missing_conf, r
		sum share_dap_game if !missing_conf
		eststo `var'3, add(mean r(mean))

	}

	reg share_can_game over_conf num_correct usecan if !missing_conf, r
	sum share_can_game if !missing_conf
	eststo over_conf, add(mean r(mean))

	reg share_lime_game over_conf num_correct if !missing_conf, r
	sum share_lime_game if !missing_conf
	eststo over_conf2, add(mean r(mean))

	reg share_dap_game over_conf num_correct usedap if !missing_conf, r
	sum share_dap_game if !missing_conf
	eststo over_conf3, add(mean r(mean))


	*Export regression results
	esttab conf_pctdiff3 conf_diff3 over_conf3 using "$dataWork/analysis/output/tables/D1dap.tex", label legend style(tex) /*
	 */ varlabels(_cons "Intercept" over_conf "Overconfident (0/1)" num_correct "No. questions correct" ///
				conf_pctdiff  "(No. believed correct - No. correct)/ No. believed correct"  ///
				conf_diff "(No. believed correct - No. correct)" usedap "Uses DAP") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(3)) se(par fmt(2))) stats(r2 N  mean, fmt(2 0 2) labels ("\$R^2\$" "\$N\$" "Mean of dep. var:"))  ///
	mlabels("Share" "Share" "Share") nonum ///
	substitute("<" "$<$" ">" "$>$") collabels (none) order (conf_pctdiff num_correct conf_diff over_conf ) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Share of Rounds with DAP}"' ///
	`"\label{tab:D1dap}"' ///
	`"\begin{tabular}{l ccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable is the share of rounds played with a positive amount of DAP. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

* **********************************************************************
* Table 8 - Share of rounds with CAN (\label{tab:D1can})
* **********************************************************************     

	esttab conf_pctdiff conf_diff over_conf using "$dataWork/analysis/output/tables/D1can.tex", label legend style(tex)  /*
    */ varlabels(_cons "Intercept" over_conf "Overconfident (0/1)"  /*
    */ num_correct "No. questions correct"  /*
    */ conf_pctdiff  "(No. believed correct - No. correct)/ No. believed correct"  /*
    */ conf_diff "(No. believed correct - No. correct)" usecan "Uses CAN") /*
    */ starlevels(* 0.10 ** 0.05 *** 0.01)  /*
    */ cells(b(star fmt(3)) se(par fmt(2))) /*
    */ stats(r2 N  mean, fmt(2 0 2)  /*
    */ labels ("\$R^2\$" "\$N\$" "Mean of dep. var:"))  ///
	mlabels("Share" "Share" "Share") nonum ///
	substitute("<" "$<$" ">" "$>$") collabels (none) order (conf_pctdiff num_correct conf_diff over_conf ) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Share of Rounds with CAN}"' ///
	`"\label{tab:D1can}"' ///
	`"\begin{tabular}{l ccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable is the share of rounds played with a positive amount of CAN. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

* **********************************************************************
* Table 9 - Share of rounds with lime (\label{tab:D1lime})
* **********************************************************************     

	esttab conf_pctdiff2 conf_diff2 over_conf2 using ///
	"$dataWork/analysis/output/tables/D1lime.tex", /*
	*/ label legend style(tex) varlabels(_cons "Intercept"  /*
    */ over_conf "Overconfident (0/1)"  /*
    */ num_correct "No. questions correct" /*
    */ conf_pctdiff   /*
    */ "(No. believed correct - No. correct)/ No. believed correct"  /*
    */ conf_diff "(No. believed correct - No. correct)")  /*
    */ starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(3))  /*
    */ se(par fmt(2))) stats(r2 N  mean, fmt(2 0 2)  /*
    */ labels ("\$R^2\$" "\$N\$" "Mean of dep. var:")) /*
	*/	mlabels("Share" "Share" "Share") nonum /* 
    */ substitute("<" "$<$" ">" "$>$")  collabels (none)  /*
    */ order (conf_pctdiff num_correct conf_diff over_conf ) /*
    */ prehead( `"\begin{table}[htbp]"' `"\centering"'  /*
    */ `"\hspace*{-1.2cm}"'  /*
    */ `"\begin{threeparttable}"'  /*
    */ `"\caption{Share of Rounds with Lime}"'  /*
    */ `"\label{tab:D1lime}"'  /*
    */ `"\begin{tabular}{l ccc}"'  /*
    */ `"\hline"' `"\hline"') postfoot(`"\hline"' `"\hline"'  /*
    */ `"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"'  /*
    */ `"\item{Notes: Dependent variable is the share of rounds played with a positive amount of CAN. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

* **********************************************************************
* Appendix
* **********************************************************************     

*Table B1


	*Hypothesis D5

	*Table: Updating and Confidence (\label{tab:D5fert})

	eststo clear

	foreach var of varlist cmean_pctchange dmean_pctchange {
		reg `var' conf_pctdiff num_correct if !missing_conf, r
		sum `var' if !missing_conf
		eststo `var'1, add(mean r(mean))
		reg `var' conf_diff num_correct if !missing_conf, r
		sum `var' if !missing_conf
		eststo `var'2, add(mean r(mean))
		reg `var' over_conf num_correct if !missing_conf, r
		sum `var' if !missing_conf
		eststo `var'3, add(mean r(mean))
	}


	*Panel A: fertilizer distribution

	local titles 	" & $\% \Delta$ Mean & $\% \Delta$ Mean & $\% \Delta$ Mean \\ \hline"

	esttab cmean_pctchange1 cmean_pctchange2 cmean_pctchange3 using "$dataWork/analysis/output/tables/appendix/D5fert.tex", label legend style(tex) ///
	varlabels(_cons "Intercept" over_conf "Overconfident (0/1)" num_correct "No. questions correct" ///
				conf_pctdiff  "(No. believed correct - No. correct)/ No. believed correct"  ///
				conf_diff "(No. believed correct - No. correct)"  ) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats( x r2 N  mean, fmt(0 2 0 2) labels ("$R " "\$R^2\$" "\$N\$" "Mean of dep. var:"))  ///
	nonum collabels (none) order (conf_pctdiff num_correct conf_diff over_conf _cons ) ///
	refcat(conf_pctdiff "\textbf{Fertilizer Distribution:}", nolabel) ///
	posthead ("`titles'") 	mlabels (none) ///
	postfoot(`"\hline"')  noline ///
		prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Updating and confidence}"' ///
	`"\label{tab:D5fert}"' ///
	`"\begin{tabular}{l ccc}"' ///
	`"\hline"' `"\hline"')  replace

	*Panel B: fertilizer + lime distribution

	esttab dmean_pctchange1 dmean_pctchange2 dmean_pctchange3 using ///
	"$dataWork/analysis/output/tables/appendix/D5fert.tex", label ///
	legend style(tex) varlabels(_cons "Intercept" /*
	*/ over_conf "Overconfident (0/1)" num_correct "No. questions correct"  /*
	*/ conf_pctdiff  "(No. believed correct - No. correct)/ No. believed correct" /*
	*/ conf_diff "(No. believed correct - No. correct)" ) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	cells(b(star fmt(2)) se(par fmt(2)))  ///
	stats(x r2 N  mean, fmt(0 2 0 2)  ///
	labels("$R " "\$R^2\$" "\$N\$" "Mean of dep. var:"))  ///
	substitute("<" "$<$" ">" "$>$") nonum mlabels(none)  ///
	collabels (none) order (conf_pctdiff num_correct conf_diff over_conf ) ///
	refcat(conf_pctdiff "\textbf{Fertilizer + Lime Distribution:}", nolabel) ///
	prehead (%) noline ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable is the percentage change in mean of the subjective yields distribution for fertilizer and fertilizer + lime, where noted. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append


*Table B2*


	*Hypothesis A1

	*Table: Past Fertilizer Use and Belief Updating (\label{tab:A1updating})



	foreach var of varlist c_mean1 d_mean1 cmean_pctchange dmean_pctchange c_cv_pctchange d_cv_pctchange {
		reg `var' fertseason_sl, r
		sum `var'
		eststo `var'_sl, add (mean r(mean))
		reg `var' fertseason_long, r
		sum `var'
		eststo `var'_l, add (mean r(mean))
	}


	reg c_cv1 fertseason_sl, r
	sum c_cv1
	eststo c_cv1_sl, add (mean r(mean))

	test _b[fertseason_sl] = 0
	local sign_ccv_sl = sign(_b[fertseason_sl])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_ccv_sl'*sqrt(r(F)))

	reg c_cv1 fertseason_long, r
	sum c_cv1
	eststo c_cv1_l, add (mean r(mean))

	test _b[fertseason_long] = 0
	local sign_ccv_l = sign(_b[fertseason_long])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_ccv_l'*sqrt(r(F)))

	reg d_cv1 fertseason_sl, r
	sum d_cv1
	eststo d_cv1_sl, add (mean r(mean))

	test _b[fertseason_sl] = 0
	local sign_dcv_sl = sign(_b[fertseason_sl])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_dcv_sl'*sqrt(r(F)))

	reg d_cv1 fertseason_long, r
	sum d_cv1
	eststo d_cv1_l, add (mean r(mean))

	test _b[fertseason_long] = 0
	local sign_dcv_l = sign(_b[fertseason_long])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_dcv_l'*sqrt(r(F)))

	*Panel A: Short and Long Rains

	local titles "& $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean \\"
	local subtitles "& Fert. Dist. & Fert. + Lime Dist. & Fert. Dist. & Fert. + Lime Dist. \\ \hline"

	esttab  cmean_pctchange_sl dmean_pctchange_sl c_cv_pctchange_sl d_cv_pctchange_sl using "$dataWork/analysis/output/tables/appendix/A1updating.tex", label legend style(tex) ///
	varlabels(_cons "Intercept" fertseason_sl "No. of seasons used fertilizer") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(x r2, fmt(0 2) labels ("$R " "\$R^2\$"))  ///
	mlabels (none) posthead("`titles'" "`subtitles'") ///
	nonum collabels (none) ///
	refcat(fertseason_sl "\textbf{Short and long rains:}", nolabel) nolines ///
	postfoot(`"\hline"') ///
		prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Past fertilizer use and belief updating }"' ///
	`"\label{tab:A1updating}"' ///
	`"\begin{tabular}{l cccc}"' ///
	`"\hline"' `"\hline"')  replace

	*Panel B: Long Rains Only

	esttab cmean_pctchange_l dmean_pctchange_l c_cv_pctchange_l d_cv_pctchange_l using "$dataWork/analysis/output/tables/appendix/A1updating.tex", label legend style(tex) ///
	varlabels(_cons "Intercept" fertseason_long "No. of seasons used fertilizer") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(x r2 k N  mean, fmt(0 2 0 0 2) labels ("$r " "\$R^2\$" " \hline %" "\$N\$" "Mean of dep. var:"))  ///
	substitute("<" "$<$" ">" "$>$") nonum mlabels(none) collabels (none)  ///
	refcat(fertseason_long "\textbf{Long rains only:}", nolabel) nolines ///
	prehead (%) ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: The dependent variable is the percentage change in the fertilizer and fertilizer-plus-lime distributions: ((post-mean - premean)/pre-mean)*100 \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append

*Table C1*

	*Table: Correlations between soil pH and DAP, CAN orders (\label{tab:ph_dapcanshare})

	eststo clear

	reg dap_share2 dap_share1 i.ph_step if !missing_lime, r
	eststo ph_dapshare
	reg can_share2 can_share1 i.ph_step if !missing_lime, r
	eststo ph_canshare

	esttab ph_dapshare ph_canshare using "$dataWork/analysis/output/tables/appendix/ph_dapcanshare.tex", label legend style(tex) ///
	varlabels(_cons "Intercept") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
	substitute("<" "$<$" ">" "$>$") mlabels("Share of final order to DAP ""Share of final order to CAN") collabels (none) ///
	order (dap_share1  can_share1) nonum drop (3.ph_step) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Correlations between soil pH and DAP, CAN orders}"' ///
	`"\label{tab:ph_dapcanshare}"' ///
	`"\begin{tabular}{l cc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variables: (1) Share of the value of the post-game order allocated to DAP, (2) Share of the value of the post-game order allocated to CAN \\ The omitted pH category is pH < 5.5. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table C2*

	*Table:Correlations between soil pH and game behavior (\label{tab:games_dapcanlime})
	eststo clear

	reg share_dap_game  i.ph_step, r
	eststo dap_game


	reg share_can_game i.ph_step, r
	eststo can_game


	reg share_lime_game i.ph_step, r
	eststo lime_game


	esttab dap_game can_game lime_game using "$dataWork/analysis/output/tables/appendix/games_dapcanlime.tex", label legend style(tex) ///
	varlabels(_cons "Intercept") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
	substitute("<" "$<$" ">" "$>$") mlabels("Share DAP game" "Share CAN game" "Share lime game") collabels (none) ///
	nonum drop (3.ph_step) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Correlations between soil pH and game behavior}"' ///
	`"\label{tab:games_dapcanlime}"' ///
	`"\begin{tabular}{l ccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variables are as follows: (1) Share of rounds played using DAP, (2) Share of rounds played using CAN. (3) Share of rounds played using lime\\ The omitted pH category is pH < 5.5. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace



*Table D5*


	*Hypothesis A1
	*Table: Past Fertilizer Use and Fertilizer Belief Distribution (\label{tab:A1dist})


	eststo clear

	foreach var of varlist c_mean1 d_mean1 cmean_pctchange dmean_pctchange c_cv_pctchange d_cv_pctchange {
		reg `var' fertseason_sl, r
		sum `var'
		eststo `var'_sl, add(mean r(mean))
		reg `var' fertseason_long, r
		sum `var'
		eststo `var'_l, add (mean r(mean))
	}


	reg c_cv1 fertseason_sl, r
	eststo c_cv1_sl

	test _b[fertseason_sl] = 0
	local sign_ccv_sl = sign(_b[fertseason_sl])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_ccv_sl'*sqrt(r(F)))
	eststo c_cv1_sl, add (p_val ttail(r(df_r),`sign_ccv_sl'*sqrt(r(F))))


	reg c_cv1 fertseason_long, r
	sum c_cv1
	eststo c_cv1_l, add (mean r(mean))

	test _b[fertseason_long] = 0
	local sign_ccv_l = sign(_b[fertseason_long])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_ccv_l'*sqrt(r(F)))
	eststo c_cv1_l, add (p_val ttail(r(df_r),`sign_ccv_l'*sqrt(r(F))))


	reg d_cv1 fertseason_sl, r
	eststo d_cv1_sl

	test _b[fertseason_sl] = 0
	local sign_dcv_sl = sign(_b[fertseason_sl])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_dcv_sl'*sqrt(r(F)))
	eststo d_cv1_sl, add (p_val ttail(r(df_r),`sign_dcv_sl'*sqrt(r(F))))


	reg d_cv1 fertseason_long, r
	sum d_cv1
	eststo d_cv1_l, add (mean r(mean))

	test _b[fertseason_long] = 0
	local sign_dcv_l = sign(_b[fertseason_long])
	display "Ho: coef <= 0  p-value = " ttail(r(df_r),`sign_dcv_l'*sqrt(r(F)))
	eststo d_cv1_l, add (p_val ttail(r(df_r),`sign_dcv_l'*sqrt(r(F))))


	*Export regression results
	*Panel A: Short and Long Rains

	local titles "& Mean & Mean & CV & CV \\"
	local subtitles "& Fert. Dist & Fert. + Lime Dist. & Fert. Dist. & Fert. + Lime Dist. \\ \hline"

	esttab c_mean1_sl d_mean1_sl c_cv1_sl d_cv1_sl using "$dataWork/analysis/output/tables/appendix/A1dist.tex", label legend style(tex) ///
	varlabels(_cons "Intercept" fertseason_sl "No. of seasons used fertilizer") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(x r2 p_val, fmt(2 2) labels ("$r ""\$R^2\$" "Test \$ H_0 \$ :  coef <= 0 (\textit{p}-value)"))  ///
	substitute("<" "$<$" ">" "$>$", "\H_0$" "H_0$" ) mlabels (none) posthead("`titles'" "`subtitles'") ///
	nonum collabels (none) ///
	refcat(fertseason_sl "\textbf{Short and long rains:}", nolabel) nolines ///
	postfoot(`"\hline"')  nolines ///
		prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Past Fertilizer Use and Fertilizer Beliefs}"' ///
	`"\label{tab:A1dist}"' ///
	`"\begin{tabular}{l cccc}"' ///
	`"\hline"' `"\hline"') replace

	*Panel B: Long Rains Only

	esttab c_mean1_l d_mean1_l c_cv1_l d_cv1_l using "$dataWork/analysis/output/tables/appendix/A1dist.tex", label legend style(tex) ///
	varlabels(_cons "Intercept" fertseason_long "No. of seasons used fertilizer") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(x r2 p_val, fmt(0 2 2) labels ("$r ""\$R^2\$" "Test \$H_0\$ :  coef <= 0 (\textit{p}-value)"))  ///
	substitute("<" "$<$" ">" "$>$") nonum mlabels(none) collabels (none)  ///
	refcat(fertseason_long "\textbf{Long rains only:}", nolabel) nolines ///
	prehead (%) ///
	postfoot (`"\hline"') append


	*Adding N and Mean of dependent var
	esttab c_mean1_l d_mean1_l c_cv1_l d_cv1_l using "$dataWork/analysis/output/tables/appendix/A1dist.tex", cells (none) prehead (%) ///
	stats(N mean, fmt(0 2) labels ("\$N\$" "Mean of Dependent Variable:")) nonum mlabels(none) noline substitute("<" "$<$" ">" "$>$") ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: The dependent variables are the mean and coefficient of variation for the fertilizer and fertilizer + lime distributions measured before farmers’ interactions with Mahindi Master. The units are bags/acre. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') 	append


*Table D6*

	*Table: DAP Order Change and Fertilizer Use (\label{tab:A2dap_int})


	foreach var of varlist dap_pctchange can_pctchange lime_valdiff {

	reg `var' usedapcan##c.cmean_pctchange, r
	eststo `var'7
	reg `var' usedapcan##c.c_cv_pctchange, r
	eststo `var'8
	reg `var' usedapcan##c.dmean_pctchange, r
	eststo `var'9
	reg `var' usedapcan##c.d_cv_pctchange, r
	eststo `var'10
	reg `var' usedapcan##c.cmean_pctchange usedapcan##c.c_cv_pctchange, r
	eststo `var'11
	reg `var' usedapcan##c.dmean_pctchange usedapcan##c.d_cv_pctchange, r
	eststo `var'12
	}

	*DAP*
	esttab dap_pctchange7  dap_pctchange8 dap_pctchange9 dap_pctchange10 dap_pctchange11 dap_pctchange12 using "$dataWork/analysis/output/tables/appendix/HA2int_dap_pctchange_edited.tex", ///
	label legend style(tex) ///
	varlabels(_cons "Intercept" 1.usedapcan "Uses DAP/CAN" cmean_pctchange "$\%$ change, mean (fertilizer)" ///
	1.usedapcan#c.cmean_pctchange "Uses DAP/CAN $\times$ $\%$ change, mean (fert.)" c_cv_pctchange "$\%$ change, CV (fertilizer)" ///
	1.usedapcan#c.c_cv_pctchange "Uses DAP/CAN $\times$ $\%$ change, CV (fert.)" dmean_pctchange "$\%$ change, mean (fertilizer + lime)" ///
	1.usedapcan#c.dmean_pctchange "Uses DAP/CAN $\times$ $\%$ change, mean (fert. + lime)" ///
	d_cv_pctchange "$\%$ change, CV (fertilizer + lime)" 1.usedapcan#c.d_cv_pctchange "Uses DAP/CAN $\times$ $\%$ change, CV (fert. + lime)") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
	mtitles ("post-pre" "post-pre" "post-pre" "post-pre" "post-pre" "post-pre") nonum  ///
	substitute("<" "$<$" ">" "$>$") collabels (none) ///
	drop (0.usedapcan  0.usedapcan#c.cmean_pctchange 0.usedapcan#c.c_cv_pctchange 0.usedapcan#c.dmean_pctchange 0.usedapcan#c.d_cv_pctchange) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
	`"\small"' ///
    `"\caption{DAP Order Change and Fertilizer Use}"' ///
	`"\label{tab:A2dap_int}"' ///
	`"\begin{tabular}{l cccccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes:  Dependent variable is the percent change in the value of the DAP order \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D7*

	*Table: CAN Order Change and Fertilizer Use (\label{tab:A2can_int})


	*CAN*
	esttab can_pctchange7  can_pctchange8 can_pctchange9 can_pctchange10 can_pctchange11 can_pctchange12 using "$dataWork/analysis/output/tables/appendix/HA2int_can_pctchange1_edited.tex", ///
	label legend style(tex) ///
	varlabels(_cons "Intercept" 1.usedapcan "Uses DAP/CAN" cmean_pctchange "$\%$ change, mean (fertilizer)" ///
	1.usedapcan#c.cmean_pctchange "Uses DAP/CAN $\times$ $\%$ change, mean (fert.)" c_cv_pctchange "$\%$ change, CV (fertilizer)" ///
	1.usedapcan#c.c_cv_pctchange "Uses DAP/CAN $\times$ $\%$ change, CV (fert.)" dmean_pctchange "$\%$ change, mean (fertilizer + lime)" ///
	1.usedapcan#c.dmean_pctchange "Uses DAP/CAN $\times$ $\%$ change, mean (fert. + lime)" ///
	d_cv_pctchange "$\%$ change, CV (fertilizer + lime)" 1.usedapcan#c.d_cv_pctchange "Uses DAP/CAN $\times$ $\%$ change, CV (fert. + lime)") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
	mtitles ("post-pre" "post-pre" "post-pre" "post-pre" "post-pre" "post-pre") nonum  ///
	substitute("<" "$<$" ">" "$>$")  collabels (none) ///
	drop (0.usedapcan  0.usedapcan#c.cmean_pctchange 0.usedapcan#c.c_cv_pctchange 0.usedapcan#c.dmean_pctchange 0.usedapcan#c.d_cv_pctchange) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
	`"\small"' ///
    `"\caption{CAN Order Change and Fertilizer Use}"' ///
	`"\label{tab:A2can_int}"' ///
	`"\begin{tabular}{l cccccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes:  Dependent variable is the percent change in the value of the CAN order \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D8*

	*Table: Lime Order Change and Fertilizer Use (\label{tab:A2lime_int})


	*Lime*

	esttab lime_valdiff7  lime_valdiff8 lime_valdiff9 lime_valdiff10 lime_valdiff11 lime_valdiff12 using "$dataWork/analysis/output/tables/HA2int_lime_diff1_edited.tex", ///
	label legend style(tex) ///
	mtitles ("post-pre" "post-pre" "post-pre" "post-pre" "post-pre" "post-pre") nonum  ///
	varlabels(_cons "Intercept" 1.usedapcan "Uses DAP/CAN" cmean_pctchange "$\% \Delta$ Belief, mean (fertilizer)" ///
	1.usedapcan#c.cmean_pctchange "Uses DAP/CAN $\times$ $\% \Delta$ Belief, mean (fert.)" c_cv_pctchange "$\% \Delta$ Belief, CV (fertilizer)" ///
	1.usedapcan#c.c_cv_pctchange "Uses DAP/CAN $\times$ $\% \Delta$ Belief, CV (fert.)" dmean_pctchange "$\% \Delta$ Belief, mean (fertilizer + lime)" ///
	1.usedapcan#c.dmean_pctchange "Uses DAP/CAN $\times$ $\% \Delta$ Belief, mean (fert. + lime)" ///
	d_cv_pctchange "$\% \Delta$ Belief, CV (fertilizer + lime)" 1.usedapcan#c.d_cv_pctchange "Uses DAP/CAN $\times$ $\% \Delta$ Belief, CV (fert. + lime)") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
	substitute("<" "$<$" ">" "$>$") collabels (none) ///
	drop (0.usedapcan  0.usedapcan#c.cmean_pctchange 0.usedapcan#c.c_cv_pctchange 0.usedapcan#c.dmean_pctchange 0.usedapcan#c.d_cv_pctchange) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
	`"\small"' ///
    `"\caption{Lime Order Change and Fertilizer Use}"' ///
	`"\label{tab:A2lime_int}"' ///
	`"\begin{tabular}{l cccccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes:  Dependent variable is the difference of the order allocated to lime (post minus pre). \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace


*Table D9*

	*A4a
	*Table: Updating of fertilizer mean beliefs (\label{tab: A4bfert})

	merge 1:1 hhid using "$dataWork/analysis/WSC_harvest_mean.dta", keep (1 3) ///
	gen(harvestmerge)


	gen anyfertseason_sl_sq=anyfertseason_sl^2
	reg c_mean1 mean_harvest anyfertseason_sl anyfertseason_sl_sq, vce(cluster village)
	*A4b
	foreach var of varlist abs_cmean_pctchange abs_dmean_pctchange abs_dap_pctchange abs_can_pctchange abs_lime_diff order_10pctshift {

		reg `var' ib(2).yield_compare_adj, r
		eststo `var'1
		reg `var' differentyields, r
		eststo `var'2
		reg `var' i.plant_seq_adj, r
		eststo `var'3
		reg `var' differentseq, r
		eststo `var'4
		}
		
	local titles "& \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean \\"
	local subtitles"& Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert  \\ \hline"
	local subtitleslime"& Fert+ Lime Dist\rvert & Fert+ Lime Dist\rvert & Fert+ Lime Dist\rvert & Fert+ Lime Dist\rvert  \\ \hline"


		esttab abs_cmean_pctchange1 abs_cmean_pctchange2 abs_cmean_pctchange3 abs_cmean_pctchange4 using "$dataWork/analysis/output/tables/appendix/HA4B_abs_cmean_pctchange_edited.tex", ///
		label legend style(tex) ///
		varlabels (_cons "Intercept" 1.yield_compare_adj "Much higher or higher" 3.yield_compare_adj "Lower or much lower" differentyields "Different yields (0/1)" ///
		2.plant_seq_adj "Slightly different" 3.plant_seq_adj "Very different" differentseq "Planting sequence very different (0/1)" ) ///
		order(differentyields differentseq) drop (2.yield_compare_adj 1.plant_seq_adj) ///
		refcat(1.yield_compare_adj "\textbf{Yield perceptions:}" 2.plant_seq_adj "\textbf{Planting sequence perceptions:}", nolabel ) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitles'") nonum ///
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Updating of fertilizer mean beliefs}"' ///
		`"\label{tab:A4bfert}"' ///
		`"\begin{tabular}{l cccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the absolute value of the percent change in the mean for the fertilizer distribution. The omitted category for yield perceptions is the same, and the omitted category for planting sequence perceptions is similar or somewhat similar. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D10*

		*Table: Updating of fertilizer + lime mean beliefs (\label{tab: A4bfertlime})


		esttab abs_dmean_pctchange1 abs_dmean_pctchange2 abs_dmean_pctchange3 abs_dmean_pctchange4 using "$dataWork/analysis/output/tables/appendix/HA4B_abs_dmean_pctchange_edited.tex", ///
		label legend style(tex) ///
		varlabels (_cons "Intercept" 1.yield_compare_adj "Much higher or higher" 3.yield_compare_adj "Lower or much lower" differentyields "Different yields (0/1)" ///
		2.plant_seq_adj "Slightly different" 3.plant_seq_adj "Very different" differentseq "Planting sequence very different (0/1)" ) ///
		order(differentyields differentseq) drop (2.yield_compare_adj 1.plant_seq_adj) ///
		refcat(1.yield_compare_adj "\textbf{Yield perceptions:}" 2.plant_seq_adj "\textbf{Planting sequence perceptions:}", nolabel ) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitleslime'") nonum ///	
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Updating of fertilizer + lime mean beliefs}"' ///
		`"\label{tab:A4bfertlime}"' ///
		`"\begin{tabular}{l cccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the absolute value of the percent change in the mean for the fertilizer and lime distribution. The omitted category for yield perceptions is the same, and the omitted category for planting sequence perceptions is similar or somewhat similar. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace


*Table D11*

		*B1

		*Table: Number of Pre-Final Rounds and Confidence (\label{tab: B1})

		eststo clear
		foreach var of varlist conf_pctdiff conf_diff  {
			reg num_reground `var' num_correct, r
			eststo `var'
		}

		#delimit ;

		reg num_reground over_conf, r;
		eststo over_conf;

		reg num_reground ib(4).selfdoubt, r;
		eststo selfdoubt ;

		esttab conf_pctdiff conf_diff over_conf selfdoubt using
		"$dataWork/analysis/output/tables/appendix/HB1_edited.tex",
		mtitles("No. rounds" "No. rounds" "No. rounds" "No. rounds") nonum
		label legend style(tex)
		varlabels (conf_pctdiff "questions believed correct \& actually correct"
		num_correct "No. questions correct"
		conf_diff "No. believed correct - No. correct"
		over_conf  "Overconfident (0/1)"
		1.selfdoubt  "Almost always / Very often"
		2.selfdoubt "Sometimes" 3.selfdoubt "Rarely" _cons "Intercept")
		refcat (conf_pctdiff "Percentage difference between No. quiz"
		1.selfdoubt "\textbf{Farming self-doubt:}" , nola)
		drop (4.selfdoubt)
		starlevels(* 0.10 ** 0.05 *** 0.01)
		cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0)
		labels ("\$R^2\$" "\$N\$"))
		substitute("<" "$<$" ">" "$>$")  collabels (none)
		prehead( `"\begin{table}[htbp]"'
		`"\centering"'
		`"\hspace*{-1.2cm}"'
		`"\begin{threeparttable}"'
		`"\small"'
		`"\caption{Number of Pre-Final Rounds and Confidence}"'
		`"\label{tab:B1}"'
		`"\begin{tabular}{l cccc}"'
		`"\hline"' `"\hline"')
		postfoot(`"\hline"' `"\hline"'
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"'
		`"\item{Notes: Dependent variable is the total number of rounds played
		in the game, excluding the sanity check round and the final founds.
		The omitted category for farming self-doubt is 'Never.'
		\\ Standard errors in parentheses
		\\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"'
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace ;
		#delimit cr

*Table D12*

		*B2A

		*Table: Multiple Final Rounds and Confidence (\label{tab: B2a})

		eststo clear
		foreach var of varlist conf_pctdiff conf_diff  {
			reg mult_finalrounds `var' num_correct, r
			eststo `var'
		}

		reg mult_finalrounds over_conf, r
		eststo over_conf

		reg mult_finalrounds ib(4).selfdoubt, r
		eststo selfdoubt

		local titles "& Multiple & Multiple & Multiple & Multiple \\"
		local subtitles "& final rounds & final rounds & final rounds & final rounds \\ \hline"		
		
		#delimit ;

		esttab conf_pctdiff conf_diff over_conf selfdoubt using
		"$dataWork/analysis/output/tables/appendix/HB2a_edited.tex",
		posthead("`titles'" "`subtitles'") nonum ///	
		label legend style(tex)
		varlabels (conf_pctdiff "questions believed correct \& actually correct"
		num_correct "No. questions correct"
		conf_diff "No. believed correct - No. correct"
		over_conf  "Overconfident (0/1)"
		1.selfdoubt  "Almost always / Very often"
		2.selfdoubt "Sometimes" 3.selfdoubt "Rarely" _cons "Intercept")
		refcat (conf_pctdiff "Percentage difference between No. quiz"
		1.selfdoubt "\textbf{Farming self-doubt:}" , nola)
		drop (4.selfdoubt)
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2))
		se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none)
		prehead( `"\begin{table}[htbp]"'
		`"\centering"'
		`"\hspace*{-1.2cm}"'
		`"\begin{threeparttable}"'
		`"\small"'
		`"\caption{Multiple Final Rounds and Confidence}"'
		`"\label{tab:B2a}"'
		`"\begin{tabular}{l cccc}"'
		`"\hline"' `"\hline"')
		postfoot(`"\hline"' `"\hline"'
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"'
		`"\item{Notes: Dependent variable is a dummy that equals one if the
		participant played multiple final rounds and zero otherwise.
		Regressions were run for the full sample. The omitted category for
		farming self-doubt is 'Never'.
		\\ Standard errors in parentheses
		\\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"'
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace ;

		#delimit cr

*Table D13*


		*Table: Multiple Final Rounds and Confidence (\label{tab: B2a_any})

		eststo clear

		foreach var of varlist conf_pctdiff conf_diff  {
			reg mult_finalrounds `var' num_correct if any_finalrounds, r
			eststo `var'
		}

		reg mult_finalrounds over_conf if any_finalrounds, r
		eststo over_conf

		reg mult_finalrounds ib(4).selfdoubt if any_finalrounds, r
		eststo selfdoubt

		#delimit ;

		esttab conf_pctdiff conf_diff over_conf selfdoubt using
		"$dataWork/analysis/output/tables/appendix/HB2a_any_edited.tex",
		posthead("`titles'" "`subtitles'") nonum ///			
		label legend style(tex) varlabels (
		conf_pctdiff "questions believed correct \& actually correct"
		num_correct "No. questions correct"
		conf_diff "No. believed correct - No. correct"
		over_conf  "Overconfident (0/1)"
		1.selfdoubt  "Almost always / Very often"
		2.selfdoubt "Sometimes"
		3.selfdoubt "Rarely" _cons "Intercept")
		refcat (conf_pctdiff "Percentage difference between No. quiz"
		1.selfdoubt "\textbf{Farming self-doubt:}" , nola)
		drop (4.selfdoubt)
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2)))
		stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none)
		prehead( `"\begin{table}[htbp]"'
		`"\centering"'
		`"\hspace*{-1.2cm}"'
		`"\begin{threeparttable}"'
		`"\small"'
		`"\caption{Multiple Final Rounds and Confidence}"'
		`"\label{tab:B2a_any}"'
		`"\begin{tabular}{l cccc}"'
		`"\hline"' `"\hline"')
		postfoot(`"\hline"' `"\hline"'
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"'
		`"\item{Notes: Dependent variable is a dummy that equals one if
		the participant played multiple final rounds and zero otherwise.
		Regressions were run for the sample of farmers that played any number
		of final rounds. The omitted category for farming self-doubt is Never.
		\\ Standard errors in parentheses
		\\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"'
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace ;

		#delimit cr



*Table D14*

		*B3

		*Table: Number of Pre-Final Rounds and Risk (\label{tab: B3})


	 	eststo clear

		foreach var of varlist riskaverse_lottery riskerror riskaverse_svy riskaverse_svyfarm riskloving_svy riskloving_svyfarm {
			reg num_reground `var', r
			eststo `var'
		}

		reg num_reground i.riskmeasure_wq1, r
		eststo risk1

		reg num_reground i.riskmeasure, r
		eststo risk2
		
		local titles "& No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- \\"
		local subtitles"& Final R. & Final R. & Final R. & Final R. & Final R. & Final R. & Final R. & Final R.  \\ \hline"

		#delimit ;

		esttab using
		"$dataWork/analysis/output/tables/appendix/HB3_edited.tex",
		posthead("`titles'" "`subtitles'") nonum
		label legend style(tex)
		varlabels(
		riskaverse_lottery "Risk averse (Lottery)"
		riskerror "Chose safe option for Q1"
		riskaverse_svy "Risk averse (General)"
		riskaverse_svyfarm "Risk averse (Farming)"
		riskloving_svy "Risk loving (General)"
		riskloving_svyfarm "Risk loving (Farming)"
		2.riskmeasure_wq1 "Risk measure with Q1=2"
		3.riskmeasure_wq1 "Risk measure with Q1=3"
		4.riskmeasure_wq1 "Risk measure with Q1=4"
		5.riskmeasure_wq1 "Risk measure with Q1=5"
		2.riskmeasure "Risk measure=2"
		3.riskmeasure "Risk measure=3"
		4.riskmeasure "Risk measure=4"
		_cons "Intercept")
		drop (1.riskmeasure_wq1 1.riskmeasure)
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2))
		se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none)
		prehead( `"\begin{table}[htbp]"'
		`"\centering"'
		`"\hspace*{-1.2cm}"'
		`"\begin{threeparttable}"'
		`"\small"'
		`"\caption{Number of Pre-Final Rounds and Risk }"'
		`"\label{tab:B3}"'
		`"\begin{tabular}{l cccccccc}"'
		`"\hline"' `"\hline"')
		postfoot(`"\hline"' `"\hline"'
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"'
		`"\item{Notes: Dependent variable is the number of pre-final
		rounds played. \\ Standard errors in parentheses
		\\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"'
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace ;
		#delimit cr
    

*Table D15*

		*B4

		*Table: Low-Rainfall Scenarios and Risk (\label{tab: B4})


		eststo clear

		foreach var of varlist riskaverse_lot riskerror riskaverse_svy riskaverse_svyfarm riskloving_svy riskloving_svyfarm {
			reg poorrain `var' rand_poorrain, r
			eststo `var'
		}

		reg poorrain i.riskmeasure_wq1 rand_poorrain, r
		eststo risk1

		reg poorrain i.riskmeasure rand_poorrain, r
		eststo risk2
		
				
		local titles "& No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- & No. Pre- \\"
		local subtitles"& Low Rf. & Low Rf. & Low Rf. & Low Rf. & Low Rf. & Low Rf. & Low Rf. & Low Rf.  \\ \hline"

		esttab using "$dataWork/analysis/output/tables/appendix/HB4_edited.tex", label legend style(tex) ///
		posthead("`titles'" "`subtitles'") nonum ///
		varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" ///
		riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" ///
		2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" ///
		5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" ///
		_cons "Intercept") ///
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Low-Rainfall Scenarios and Risk}"' ///
		`"\label{tab:B4}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the number of rounds farmers choose to play with low rainfall. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D16*

		*B5

		*Table: Share of Rounds with CAN (\label{tab: B5can})

		eststo clear
		foreach var of varlist riskaverse_lot riskerror riskaverse_svy riskaverse_svyfarm riskloving_svy riskloving_svyfarm {
			reg share_can_game `var' usecan, r
			eststo `var'
		}

		reg share_can_game i.riskmeasure_wq1 usecan, r
		eststo risk1
		reg share_can_game i.riskmeasure usecan, r
		eststo risk2

		local titles "& Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. \\"
		local subtitles"& CAN& CAN& CAN& CAN& CAN& CAN& CAN& CAN \\ \hline"		
		
		esttab using "$dataWork/analysis/output/tables/appendix/HB5_edited.tex", label legend style(tex) ///
		posthead("`titles'" "`subtitles'") nonum ///				
		varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" ///
		riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" ///
		2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" ///
		5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" ///
		_cons "Intercept" usecan "Uses CAN") ///
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Share of Rounds with CAN}"' ///
		`"\label{tab:B5can}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the share of rounds played with a positive amount of CAN, where CAN was a choice. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D17*

		*Table: Share of Rounds with Lime (\label{tab: B5lime})

		eststo clear

		foreach var of varlist riskaverse_lot riskerror riskaverse_svy riskaverse_svyfarm riskloving_svy riskloving_svyfarm {
			reg share_lime_game `var', r
			eststo `var'2

		}

		reg share_lime_game i.riskmeasure_wq1, r
		eststo risk1_2
		reg share_lime_game i.riskmeasure, r
		eststo risk2_2

				
		local titles "& Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. & Sh. R. \\"
		local subtitles"& Lime& Lime& Lime& Lime& Lime& Lime& Lime& Lime \\ \hline"		

		esttab using "$dataWork/analysis/output/tables/appendix/HB5_lime_edited.tex", ///
		posthead("`titles'" "`subtitles'") nonum ///		
		label legend style(tex) varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" 2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" 	5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" _cons "Intercept" usecan "Uses CAN") ///
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Share of Rounds with Lime}"' ///
		`"\label{tab:B5lime}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the share of rounds played with a positive amount of lime, where lime was a choice. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace


*Table D18*

		*B6
		*Table: SNumber of Final Rounds (\label{tab: B6})

		eststo clear

		foreach var of varlist riskaverse_lot riskerror riskaverse_svy riskaverse_svyfarm riskloving_svy riskloving_svyfarm {
			reg num_finalrounds `var', r
			eststo `var'
			*reg num_finalrounds `var' if mult_finalrounds == 1, r
			*eststo `var'2
		}

		reg num_finalrounds i.riskmeasure_wq1, r
		eststo risk1
		*reg num_finalrounds i.riskmeasure_wq1 if mult_finalrounds == 1, r
		*eststo risk1_2

		reg num_finalrounds i.riskmeasure, r
		eststo risk2
		*reg num_finalrounds i.riskmeasure if mult_finalrounds == 1, r
		*eststo risk2_2

		local titles "& No. F. & No. F. & No. F. & No. F. & No. F. & No. F. & No. F. & No. F. \\"
		local subtitles"& Rounds & Rounds & Rounds & Rounds & Rounds & Rounds & Rounds & Rounds  \\ \hline"

		esttab using "$dataWork/analysis/output/tables/appendix/HB6_edited.tex", ///
		label legend style(tex) varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" 2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" 5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" 	_cons "Intercept" usecan "Uses CAN") ///
		posthead("`titles'" "`subtitles'") nonum ///		
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Number of Final Rounds}"' ///
		`"\label{tab:B6}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the number of final rounds played for the full sample of participants. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D19*

		*Table: Total Rounds and Updating (\label{tab: B6_cond})
		*C1
		eststo clear


		foreach var of varlist abs_cmean_pctchange abs_dmean_pctchange abs_dap_pctchange abs_can_pctchange abs_lime_diff {
			reg `var' totalrounds, r
			eststo `var'
		}

		local titles "& \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & DAP Sh. \\"
		local subtitles"& Fert. Dist\rvert & Fert+ Lime Dist\rvert & DAP val.\rvert & CAN val.\rvert & Lime val.\rvert & (Post)  \\ \hline"
		
		reg dap_share2 dap_share1 totalrounds, r
		eststo dap

		esttab using "$dataWork/analysis/output/tables/appendix/HC1_edited.tex", ///
		label legend style(tex) ///
		varlabels(totalrounds "Total number of rounds" dap_share1 "Share allocated to DAP (pre)" _cons "Intercept") ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitles'") nonum ///		
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Total Rounds and Updating}"' ///
		`"\label{tab:B6_cond}"' ///
		`"\begin{tabular}{l cccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable for each column is as follows: (1) absolute value of the percentage change in the mean of the subjective yields distribution with fertilizer, (2) absolute value of the percentage change in the mean of the subjective yields distribution with fertilizer and lime, (3) absolute value of the percentage change in DAP value, (4) absolute value of the percentage change in CAN value, (5) absolute value of the change in lime value, and (6) DAP Share (Post). \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D20*

		*D2
		*Table: Share of Pre-Final Rounds (\label{tab: D2})

		eststo clear

		foreach var of varlist conf_pctdiff conf_diff {
			reg share_reg `var' num_correct, r
			eststo `var'
		}

		reg share_reg over_conf, r
		eststo over_conf


		reg share_reg ib(4).selfdoubt, r
		eststo selfdoubt


		local titles "& Share Pre- & Share Pre- & Share Pre- & Share Pre-  \\"
		local subtitles"& Final R. & Final R. & Final R. & Final R.  \\ \hline"
		
		esttab conf_pctdiff conf_diff over_conf selfdoubt using "$dataWork/analysis/output/tables/appendix/HD2_reg_edited.tex", label legend style(tex)  ///
		varlabels (conf_pctdiff "questions believed correct \& actually correct" num_correct "No. questions correct" ///
		conf_diff "No. believed correct - No. correct" over_conf  "Overconfident (0/1)" 1.selfdoubt  "Almost always / Very often" ///
		2.selfdoubt "Sometimes" 3.selfdoubt "Rarely" _cons "Intercept") ///
		refcat (conf_pctdiff "Percentage difference between No. quiz" 1.selfdoubt "\textbf{Farming self-doubt:}" , nola) ///
		drop (4.selfdoubt) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitles'") nonum ///		
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Share of Pre-Final Rounds}"' ///
		`"\label{tab:D2}"' ///
		`"\begin{tabular}{l cccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the share of pre-final rounds of the total number of rounds played. The omitted category for farming self-doubt is 'Never.' \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D21*


		*D3
		*Table: Multiple Final Rounds (\label{tab: D3})

		eststo clear

		foreach var of varlist riskaverse_lot riskerror riskaverse_svy riskaverse_svyfarm riskloving_svy riskloving_svyfarm {
			reg mult_finalrounds `var', r
			eststo `var'
		}

		reg mult_finalrounds i.riskmeasure_wq1, r
		eststo risk1

		reg mult_finalrounds i.riskmeasure, r
		eststo risk2

		
		local titles "& Mult. & Mult. & Mult. & Mult. & Mult. & Mult. & Mult. & Mult.  \\"
		local subtitles"& Final R. & Final R. & Final R. & Final R. & Final R. & Final R. & Final R. & Final R.  \\ \hline"
		
		
		esttab using "$dataWork/analysis/output/tables/appendix/HD3_edited.tex", label legend style(tex) ///
		varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" ///
		riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" ///
		2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" ///
		5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" ///
		_cons "Intercept" usecan "Uses CAN") ///
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitles'") nonum ///				
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Multiple Final Rounds}"' ///
		`"\label{tab:D3}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is a dummy variable that equals one if multiple final rounds were played and zero otherwise. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D22*

		*D4

		*Table: Share of Pre-Final Rounds (\label{tab: D4})
		eststo clear

		
		foreach var of varlist riskaverse_lot riskerror riskaverse_svy riskaverse_svyfarm riskloving_svy riskloving_svyfarm {
			reg share_reg `var', r
			eststo `var'
		}

		reg share_reg i.riskmeasure_wq1, r
		eststo risk1


		reg share_reg i.riskmeasure, r
		eststo risk2

		local titles "& Sh. Pre- & Sh. Pre- & Sh. Pre- & Sh. Pre- & Sh. Pre- & Sh. Pre- & Sh. Pre- & Sh. Pre- \\"
		local subtitles"& Final R. & Final R. & Final R. & Final R. & Final R. & Final R. & Final R. & Final R.  \\ \hline"
		
		esttab using "$dataWork/analysis/output/tables/appendix/HD4_reg_edited.tex", label legend style(tex) ///
		varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" ///
		riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" ///
		2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" ///
		5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" ///
		_cons "Intercept" usecan "Uses CAN") ///
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitles'") nonum ///						
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Share of Pre-Final Rounds}"' ///
		`"\label{tab:D4}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the share of pre-final rounds of the total number of rounds played. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D23*

		*D5

		*Table: Updating and Risk (\label{tab: D5fertrisk})

		eststo clear

		foreach var of varlist abs_cmean_pctchange abs_dmean_pctchange abs_dap_pctchange abs_can_pctchange abs_lime_diff {
			reg `var' riskaverse_lottery, r
			eststo `var'1
			reg `var' riskerror, r
			eststo `var'2
			reg `var' i.riskmeasure_wq1, r
			eststo `var'3
			reg `var' i.riskmeasure, r
			eststo `var'4
			reg `var' riskaverse_svy, r
			eststo `var'5
			reg `var' riskaverse_svyfarm, r
			eststo `var'6
			reg `var' riskloving_svy, r
			eststo `var'7
			reg `var' riskloving_svyfarm, r
			eststo `var'8
			}

	
		
		local titles "& \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean \\"
		local subtitles"& Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert & Fert. Dist.\rvert  \\ \hline"			
			
		esttab abs_cmean_pctchange1 abs_cmean_pctchange2 abs_cmean_pctchange3 abs_cmean_pctchange4 ///
		abs_cmean_pctchange5 abs_cmean_pctchange6 abs_cmean_pctchange7 abs_cmean_pctchange8 using "$dataWork/analysis/output/tables/appendix/HD5_abs_cmean_pctchange_risk_edited.tex", label legend style(tex) ///
		varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" ///
		riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" ///
		2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" ///
		5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" ///
		_cons "Intercept" usecan "Uses CAN") ///
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitles'") nonum ///				
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Updating and Risk}"' ///
		`"\label{tab:D5fertrisk}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the absolute value of the percent change in mean of the subjective yields distribution for fertilizer. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

*Table D24*

		*Table: Updating and Risk (\label{tab: D5fertlimerisk})


		local titles "& \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean & \vert $\%\Delta$ Mean \\"
		local subtitles1"& Fert+Lime & Fert+Lime & Fert+Lime & Fert+Lime & Fert+Lime & Fert+Lime & Fert+Lime & Fert+Lime  \\ "			
		local subtitles2"& Dist.\rvert & Dist.\rvert & Dist.\rvert & Dist.\rvert & Dist.\rvert & Dist.\rvert & Dist.\rvert & Dist.\rvert  \\ \hline"			
		
		
		esttab abs_dmean_pctchange1 abs_dmean_pctchange2 abs_dmean_pctchange3 abs_dmean_pctchange4 ///
		abs_dmean_pctchange5 abs_dmean_pctchange6 abs_dmean_pctchange7 abs_dmean_pctchange8 using "$dataWork/analysis/output/tables/appendix/HD5_abs_dmean_pctchange_risk_edited.tex", ///
        label legend style(tex) varlabels(riskaverse_lottery "Risk averse (Lottery)" riskerror "Chose safe option for Q1" riskaverse_svy "Risk averse (General)" ///
		riskaverse_svyfarm "Risk averse (Farming)" riskloving_svy "Risk loving (General)" riskloving_svyfarm "Risk loving (Farming)" ///
		2.riskmeasure_wq1 "Risk measure with Q1=2" 3.riskmeasure_wq1 "Risk measure with Q1=3" 4.riskmeasure_wq1 "Risk measure with Q1=4" ///
		5.riskmeasure_wq1 "Risk measure with Q1=5" 2.riskmeasure "Risk measure=2" 3.riskmeasure "Risk measure=3" 4.riskmeasure "Risk measure=4" ///
		_cons "Intercept" usecan "Uses CAN") ///
		drop (1.riskmeasure_wq1 1.riskmeasure) ///
		starlevels(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(2)) se(par fmt(2))) stats(r2 N , fmt(2 0) labels ("\$R^2\$" "\$N\$"))  ///
		substitute("<" "$<$" ">" "$>$") mlabels(none) collabels (none) ///
		posthead("`titles'" "`subtitles1'" "`subtitles2'") nonum ///						
		prehead( `"\begin{table}[htbp]"' ///
		`"\centering"' ///
		`"\hspace*{-1.2cm}"' ///
		`"\begin{threeparttable}"' ///
		`"\small"' ///
		`"\caption{Updating and Risk}"' ///
		`"\label{tab:D5fertlimerisk}"' ///
		`"\begin{tabular}{l cccccccc}"' ///
		`"\hline"' `"\hline"') ///
		postfoot(`"\hline"' `"\hline"' ///
		`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
		`"\item{Notes: Dependent variable is the absolute value of the percent change in mean of the subjective yields distribution for fertilizer and lime. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
		`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace

        
* **********************************************************************
* Response to referees
* **********************************************************************     

set graphics on

* **********************************************************************
* Referee 2:
*           * non-response rate
*           * descriptive stats on beliefs and on harvests
*             ** Table with 
*           * number of bins with positive probability
*             **
*           * distribution of probability in bins
*             **
*           * rationale for log-normal
*             **
*           * correlations of beliefs with plot characteristics & past yields
*             **
* **********************************************************************     

* Table showing the average 

*Reviewer 2, comment 1, points i) and ii)*
*Table expectations about upper and lower bounds, pre and post *

	*Tranform elicited expectations from bags to kg to compare with yields 
	* A bag of maize weighs 90kg
	* Transform to per-acre
	
	destring p_size_acres, replace

	foreach var in exp_3 exp_4 exp_5 exp_6 exp_7 exp_8 exp_5_post exp_6_post ///
	exp_7_post exp_8_post{
			replace `var'=`var'*90
			gen `var'_kgpa=`var'/p_size_acres
            winsor2 `var'_kgpa, replace
			}

	*merge harvest data from WSC*

	merge 1:1 hhid using "$dataWork/analysis/WSC_harvest_mean_lrains.dta", ///
	keepus(mean_long_harvest) keep(1 3) gen(harvestmerge_lrains)
	
	
********************************
    foreach var of varlist exp_3_kgpa exp_5_kgpa exp_7_kgpa ///
    exp_5_post_kgpa exp_7_post_kgpa {
        label var `var' `"\hspace{0.1cm} Lower bound (kg/acre)"'
    }
    foreach var of varlist exp_4_kgpa exp_6_kgpa exp_8_kgpa ///
    exp_6_post_kgpa exp_8_post_kgpa {
        label var `var' `"\hspace{0.1cm} Upper bound (kg/acre)"'
    }



* **********************************************************************
* Panel A: Descriptive statistics, beliefs (\label{tab:expbounds})
*   * Pre-game
* ********************************************************************** 
    
    eststo clear
	estpost summarize exp_3_kgpa exp_4_kgpa exp_5_kgpa exp_6_kgpa ///
    exp_7_kgpa exp_8_kgpa
    
	esttab using "$dataWork/analysis/output/tables/expbounds.tex", ///
	cells("count(fmt(a2)) mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") ///
	collabels("Observations" "Mean" "s.d." "Min" "Max" ///
    , begin("Panel A: Bounds on belief distribution")) unstack ///
	eqlabels(none) label noobs nonum legend style(tex) mlabels(none) ///
    refcat(exp_3_kgpa "\textbf{Pre-game} & & & & \\ \textit{No fertilizer}" ///
    exp_5_kgpa "\textit{Fertilizer}" ///
	exp_7_kgpa "\textit{Fertilizer and lime}", nolabel) ///
	postfoot(" & & & & & \\") ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Descriptive statistics on yield beliefs}"' ///
	`"\label{tab:expbounds}"' ///
	`"\begin{tabular}{l ccccc}"' ///
	`"\hline"' `"\hline"') replace


* **********************************************************************
*   * Post-game
* ********************************************************************** 
	eststo clear
	estpost summarize exp_5_post_kgpa exp_6_post_kgpa exp_7_post_kgpa ///
    exp_8_post_kgpa

	esttab using "$dataWork/analysis/output/tables/expbounds.tex" ///
    , cells("count(fmt(a2)) mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") ///
    eqlabels(none) label nonum legend style(tex) mlabels(none) unstack  ///
    collabels(none) noobs refcat(exp_5_post_kgpa ///
    "\textbf{Post-game} & & & & \\ \textit{Fertilizer}" ///
    exp_7_post_kgpa "\textit{Fertilizer and lime}", nolabel) ///
	prehead (%) noline ///
	postfoot(" & & & & & \\") append

* **********************************************************************
* Add yield data: mean yield for long rains from RCT sample
* **********************************************************************     
	eststo clear
	estpost summarize mean_long_harvest

	esttab using "$dataWork/analysis/output/tables/expbounds.tex" ///
    , cells("count(fmt(a2)) mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") ///
    eqlabels(none) label nonum legend style(tex) mlabels(none) ///
    collabels(none) noobs unstack ///
    refcat(mean_long_harvest "\textbf{Avg. maize yields}", nolabel) ///
    varlabels(mean_long_harvest "(kg/acre)") ///
	prehead (%) noline ///
	postfoot(`"\hline"' `"\hline"' \\) append 
   
* **********************************************************************
* Describe bins with positive probability mass
* **********************************************************************  

* Summarize the number of bins with positive probability mass

	foreach var in exp_b exp_c exp_d{
			gen num_`var'=0
			label	var num_`var' "Number of bins with positive prob mass"
			forval i=1/5{
					replace num_`var'=num_`var'+1 if `var'`i'>0 & `var'`i'~=.
					}
			replace num_`var'=. if num_`var'==0
				}


		foreach var in  exp_c exp_d{
			gen num_`var'_post=0
			label	var num_`var'_post "Number of bins with positive prob mass"
			forval i=1/5{
					replace num_`var'_post=num_`var'_post+1 ///
                    if `var'`i'_post>0 & `var'`i'_post~=.
					}
			replace num_`var'_post=. if num_`var'_post==0
				}

* Table

	eststo clear

	estpost summarize num_exp_b num_exp_c num_exp_d num_exp_c_post ///
    num_exp_d_post
    
	esttab using "$dataWork/analysis/output/tables/expbounds.tex" ///
    , cells("count(fmt(a2)) mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") ///
	collabels("Observations" "Mean" "s.d." "Min" "Max" ///
    , begin("Panel B: Share of bins with probability mass > 0")) unstack ///
	eqlabels(none) label noobs nonum legend style(tex) mlabels(none) ///
	varlabels(num_exp_b "No fertilizer" num_exp_c "Fertilizer" ///
    num_exp_d "Fertilizer and lime" ///
	num_exp_c_post "Fertilizer" num_exp_d_post "Fertilizer and lime") ///
	refcat(num_exp_b "\textbf{Pre-game}" num_exp_c_post "\textbf{Post-game}" ///
    , nolabel) ///
	prehead (%) noline ///
	postfoot(`"\hline"' `"\hline"' ///
  	`"\end{tabular}"' `"\end{threeparttable}"' `"\end{table}"') append
  
   
* **********************************************************************
* Table describing distribution of probability across bins (\label{tab:distbins})
* ********************************************************************** 

	***Distribution of probability on each bin. Comment iv***

	foreach var in exp_b exp_c exp_d{
	forval i=1/5{
			gen `var'`i'_prop=`var'`i'/20
			}
			}

	foreach var in exp_c exp_d{
	forval i=1/5{
			gen `var'`i'_post_prop=`var'`i'_post/20
			}
			}

	* (\label{tab:distbins})*
	*pre-game no fert*


	eststo clear
	estpost tabstat exp_b1_prop exp_b2_prop exp_b3_prop exp_b4_prop ///
	exp_b5_prop, listwise statistics (mean)

	esttab using "$dataWork/analysis/output/tables/distbins.tex", ///
	cells(" exp_b1_prop(fmt(2)) exp_b2_prop(fmt(2)) exp_b3_prop(fmt(2)) exp_b4_prop(fmt(2)) exp_b5_prop(fmt(2))") unstack ///
	collabels ("Bin 1" "Bin 2" "Bin 3" "Bin 4" "Bin 5") ///
	varlabels(mean "No fertilizer") ///
	refcat(mean "\textbf{Pre-game}", nolabel) ///
	eqlabels(none) label noobs nonum legend style (tex) mlabels (none) ///
		postfoot(%) ///
		prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Probability distribution across bins }"' ///
	`"\label{tab:distbins}"' ///
	`"\begin{tabular}{l ccccc}"' ///
	`"\hline"' `"\hline"') replace

	*pre-game fert*

	eststo clear
	estpost tabstat exp_c1_prop exp_c2_prop exp_c3_prop exp_c4_prop ///
    exp_c5_prop, listwise statistics (mean)

	esttab using "$dataWork/analysis/output/tables/distbins.tex", ///
	cells("exp_c1_prop(fmt(2)) exp_c2_prop(fmt(2)) exp_c3_prop(fmt(2)) exp_c4_prop(fmt(2)) exp_c5_prop(fmt(2))") unstack collabels (none) ///
	varlabels(mean "Fertilizer") ///
	eqlabels(none)  noobs nonum legend style (tex) mlabels (none) ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append


	*pre-game fert and lime*

	eststo clear
	estpost tabstat exp_d1_prop exp_d2_prop exp_d3_prop exp_d4_prop exp_d5_prop, listwise statistics (mean)

	esttab using "$dataWork/analysis/output/tables/distbins.tex", ///
	cells("exp_d1_prop(fmt(2)) exp_d2_prop(fmt(2)) exp_d3_prop(fmt(2)) exp_d4_prop(fmt(2)) exp_d5_prop(fmt(2))") unstack collabels (none) ///
	varlabels(mean "Fertilizer and lime") ///
	eqlabels(none)  noobs nonum legend style (tex) mlabels (none) ///
		prehead (%)posthead(%) ///
		postfoot(`"\hline"') 	append

	*post-game fert*

	eststo clear
	estpost tabstat exp_c1_post_prop exp_c2_post_prop exp_c3_post_prop exp_c4_post_prop exp_c5_post_prop, listwise statistics (mean)

	esttab using "$dataWork/analysis/output/tables/distbins.tex", ///
	cells("exp_c1_post_prop(fmt(2)) exp_c2_post_prop(fmt(2)) exp_c3_post_prop(fmt(2)) exp_c4_post_prop(fmt(2)) exp_c5_post_prop(fmt(2))") unstack collabels (none) ///
	varlabels(mean "Fertilizer") ///
	refcat(mean  "\textbf{Post-game}", nolabel) ///
	eqlabels(none)  noobs nonum legend style (tex) mlabels (none) ///
		prehead (%)posthead(%) ///
		postfoot(%) 	append

	*post-game fert and lime*
	eststo clear
	estpost tabstat exp_d1_post_prop exp_d2_post_prop exp_d3_post_prop exp_d4_post_prop exp_d5_post_prop, listwise statistics (mean)

	esttab using "$dataWork/analysis/output/tables/distbins.tex", ///
	cells("exp_d1_post_prop(fmt(2)) exp_d2_post_prop(fmt(2)) exp_d3_post_prop(fmt(2)) exp_d4_post_prop(fmt(2)) exp_d5_post_prop(fmt(2))") unstack collabels (none) ///
	varlabels(mean "Fertilizer and lime") ///
	eqlabels(none)  noobs nonum legend style (tex) mlabels (none)  ///
	prehead (%) noline ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\end{threeparttable}"' `"\end{table}"') 	append


* **********************************************************************
* Correlations between beliefs and plot char's & past yields
* Figures: beliefs_harvests & beliefs_ph
* Table: (\label{tab:exppastchar})
* ********************************************************************** 

* Keep only farmers with past fertilizer experience
* Beliefs vs. harvest

	preserve
	keep if ffertl 
    foreach var of varlist b_mean1 c_mean1 c_mean2 ///
    d_mean1 d_mean2 {
        winsor2 `var', replace
    }
    replace b_mean1 = b_mean1*90
    replace c_mean1 = c_mean1*90
    replace d_mean1 = d_mean1*90
    replace c_mean2 = c_mean2*90
    replace d_mean2 = d_mean2*90
    count if !mi(num_exp_d)
    local obslimepre `r(N)'
    count if !mi(num_exp_d_post)
    local obslimepost `r(N)'
    
	twoway 	(lfitci b_mean1 mean_long_harvest, acolor(ebblue%50) ///
			clcolor(ebblue%75) alwidth(none) lpattern(shortdash)) ///
			(lfitci c_mean1 mean_long_harvest, acolor(magenta%50) ///
			clcolor(magenta%75) alwidth(none) lpattern(solid)) ///
			(scatter d_mean1 mean_long_harvest if !mi(num_exp_d) ///
			, msymb(Sh) mcolor(green%45) jitter(1))  ///
			(scatter b_mean1 mean_long_harvest, msymb(Th) mcolor(ebblue%65)) ///
			(scatter c_mean1 mean_long_harvest, msymb(Oh) mcolor(magenta%65)) ///
            , legend(pos(5) ///
            order(6 "Mean of belief distribution (no fertilizer)" ///
            - 2 "Linear fit" 1 "95% CI" ///
            7 "Mean of belief distribution (fertilizer)" ///
            - 4 "Linear fit" 3 "95% CI" ///
            5 "Mean of belief distribution (fertilizer + lime), N = `obslimepre'") ///
            cols(4)) ///
            title("Before app interaction") ///
            xtitle("Main season maize harvest, kg/acre") ///
            ytitle("Mean of belief distribution, kg/acre" " ") ///
			note("Sample limited to farmers who had some experience with fertilizer" ///
			"for comparative purposes (N=121)", size(small)) ///
            saving(pre, replace)
			
	twoway 	(lfitci c_mean2 mean_long_harvest, acolor(magenta%75) ///
			clcolor(magenta%75) alwidth(none) lpattern(solid)) ///
			(lfitci d_mean2 mean_long_harvest, acolor(green%35) ///
			clcolor(green%75) lpattern(dash) alwidth(none)) ///
			(scatter d_mean2 mean_long_harvest if !mi(num_exp_d_post) ///
			, msymb(S) mcolor(green%65) jitter(1))  ///
			(scatter c_mean2 mean_long_harvest, msymb(O) mcolor(magenta%50)) ///
			, legend(pos(5) order(6 "Mean of belief distribution (fertilizer)" ///
            - 2 "Linear fit" 1 "95% CI" ///
            5 "Mean of belief distribution (fertilizer + lime)" ///
            - 4 "Linear fit" 3 "95% CI" - "N = `obslimepost'" ) cols(4)) ///
            title("After app interaction") ///
            xtitle("Main season maize harvest, kg/acre") ///
            ytitle("Mean of belief distribution, kg/acre" " ") ///
			note("Sample limited to farmers who had some experience with fertilizer" ///
			"for comparative purposes", size(small)) ///
            saving(post, replace)
	
    graph combine pre.gph post.gph, ycommon xsize(10) iscale(1)
	graph export "$dataWork/analysis/output/figures/beliefsHarvests.pdf" ///
    , replace

* Beliefs vs. plot pH
    
    twoway 	(lfitci b_mean1 ph, acolor(ebblue%50) ///
			clcolor(ebblue%75) alwidth(none) lpattern(shortdash)) ///
			(lfitci c_mean1 ph, acolor(magenta%50) ///
			clcolor(magenta%75) alwidth(none) lpattern(solid)) ///
			(scatter d_mean1 ph if !mi(num_exp_d) ///
			, msymb(Sh) mcolor(green%65) jitter(1))  ///
			(scatter b_mean1 ph, msymb(Th) mcolor(ebblue%65)) ///
			(scatter c_mean1 ph, msymb(Oh) mcolor(magenta%65)) ///
            , legend(pos(5) ///
            order(6 "Mean of belief distribution (no fertilizer)" ///
            - 2 "Linear fit" 1 "95% CI" ///
            7 "Mean of belief distribution (fertilizer)" ///
            - 4 "Linear fit" 3 "95% CI" ///
            5 "Mean of belief distribution (fertilizer + lime), N = `obslimepre'") ///
            cols(4)) ///
            title("Before app interaction") ///
            xtitle("Plot pH") ///
            ytitle("Mean of belief distribution, kg/acre" " ") ///
			note("Sample limited to farmers who had some experience with fertilizer" ///
			"for comparative purposes (N=121)", size(small)) ///
            saving(phpre, replace)

	twoway 	(lfitci  c_mean2 ph, acolor(magenta%75) ///
			clcolor(magenta%75) alwidth(none) lpattern(solid)) ///
			(lfitci  d_mean2 ph, acolor(green%35) ///
			clcolor(green%75) lpattern(dash) alwidth(none)) ///
			(scatter d_mean2 ph if !mi(num_exp_d_post) ///
			, msymb(S) mcolor(green%45) jitter(1))  ///
			(scatter c_mean2 ph, msymb(O) mcolor(magenta%50)) ///
			, legend(pos(5) ///
            order(6 "Mean of belief distribution (fertilizer)" ///
            - 2 "Linear fit" 1 "95% CI" ///
            5 "Mean of belief distribution (fertilizer + lime)" ///
            - 4 "Linear fit" 3 "95% CI" - "N = `obslimepost'" ) ///
            cols(4)) ///
            title("After app interaction") ///
            xtitle("Plot pH") ///
            ytitle("Mean of belief distribution, kg/acre" " ") ///
			note("Sample limited to farmers who had some experience with fertilizer" ///
			"for comparative purposes (N=121)", size(small)) ///
            saving(phpost, replace)
	
    graph combine phpre.gph phpost.gph, ycommon xsize(10) iscale(1)
	graph export "$dataWork/analysis/output/figures/beliefspH.pdf", replace    
    
       
    
********** In regression form
                
    * (\label{tab:exppastchar})*
	*reg of mean yield exp on plot char and past yields*

	eststo clear

	reg b_mean1  c.ph c.cec mean_long_harvest, r
	eststo nofert
	reg b_mean1 c.ph c.cec mean_long_harvest fert_long_share, r
	eststo nofert1
	
	reg c_mean1 c.ph c.cec mean_long_harvest if exp_c1~=., r
	eststo fert
	reg c_mean1 c.ph c.cec mean_long_harvest fert_long_share, r
	eststo fert1
	
	reg b_cv1 c.ph c.cec mean_long_harvest, r
	eststo nofertcv
	reg b_cv1 c.ph c.cec mean_long_harvest fert_long_share, r
	eststo nofertcv1
	
	reg c_cv1 c.ph c.cec mean_long_harvest if exp_c1~=., r
	eststo fertcv
    reg c_cv1 c.ph c.cec mean_long_harvest fert_long_share if exp_c1~=., r
	eststo fertcv1
    
    esttab nofert nofert1 fert fert1 nofertcv nofertcv1 fertcv fertcv1 ///
	using "$dataWork/analysis/output/tables/exppastchar.tex" ///
	,  label nonum legend starlevels(* 0.10 ** 0.05 *** 0.01) ///
	cells(b(star fmt(2) label()) se(par fmt(2) label())) ///
	stats(r2 N, fmt(2 0) labels ("\$R^2\$" "N")) style(tex) ///
	mlabels("\shortstack{Mean\\ (no fert.)}" ///
    "\shortstack{Mean\\ (no fert.)}"  ///
    "\shortstack{Mean\\ (fert.)}" "\shortstack{Mean\\ (fert.)}"  ///
    "\shortstack{CV\\ (no fert.)}" "\shortstack{CV\\ (no fert.)}"  ///
    "\shortstack{CV\\ (fert.)}" "\shortstack{CV\\ (fert.)}") ///
	substitute("<" "$<$" ">" "$>$") collabels(none) ///
	varlabels (ph "pH" cec "CEC" mean_long_harvest "Mean yield"  ///
    fert_long_share "Proportion of seasons\\ with fertilizer"  ///
    _cons Intercept) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{Pre-game expectations, correlations with baseline characteristics}"' ///
	`"\label{tab:exppastchar}"' ///
	`"\begin{tabular}{l cccccccc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable in columns (1) and (2) is mean yield expectation without fertilizer; in (3) and (4) it is the mean of yield expectations with fertilizer; (4)-(7) show the coefficient of variation for yield beliefs without fertilizer and with fertilizer, respectively. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace
    
	restore		
   
    
/*	**scatter plots**
	**no fert**
	*(\label{fig:phexpnofert})*
	scatter  ph b_mean1, scheme(plotplain)
	graph export "$dataWork/analysis/output/figures/scatter_ph_exp_nofert.pdf", replace

	*(\label{fig:cecexpnofert})*
	scatter  cec b_mean1, scheme(plotplain)
	graph export "$dataWork/analysis/output/figures/scatter_cec_exp_nofert.pdf", replace

	*(\label{fig:yieldexpnofert})*
	scatter  mean_long_harvest b_mean1, scheme(plotplain)
	graph export "$dataWork/analysis/output/figures/scatter_yields_exp_nofert.pdf", replace


	**fert**
	*(\label{fig:phexpfert})*
	scatter  ph c_mean1, scheme(plotplain)
	graph export "$dataWork/analysis/output/figures/scatter_ph_exp_fert.pdf", replace

	*(\label{fig:cecexpfert})*
	scatter  cec c_mean1, scheme(plotplain)
	graph export "$dataWork/analysis/output/figures/scatter_cec_exp_fert.pdf", replace

	*(\label{fig:yieldexpfert})*
	scatter  mean_long_harvest c_mean1, scheme(plotplain)
	graph export "$dataWork/analysis/output/figures/scatter_yields_exp_fert.pdf", replace

*/    
       
* **********************************************************************
* Ref 2 - Comment 2 
* Experimentation in the game ==> belief updating (\label{tab:beliefsgame})
* **********************************************************************     

    * Totalrounds does not include the practice round (and it should)
    gen rounds = totalrounds+1
        lab var rounds "Total number of rounds played"
    histogram rounds, percent acolor(ebblue%90) xtitle("Total number of rounds played")
	graph export "$dataWork/analysis/output/figures/histogramRounds.pdf" ///
    , replace
    
    gen share_poorrain = rand_poorrain/7
        lab var share_poorrain "Share of of poor rain"
    gen share_okayrain = rand_okayrain/7
        lab var share_okayrain "Share of of ok rain"
    gen share_goodrain = rand_goodrain/7
        lab var share_poorrain "Share of good rain"

* Total rounds vs. random poor rain
    twoway (scatter rounds share_poorrain ///
    , jitter(3) msymb(oh) mcolor(orange%95) ytitle(Total number of rounds))
	
* Total rounds vs. random rain draws	
    twoway (lfitci rounds share_goodrain ///
    , acolor(ebblue%65) clcolor(ebblue%45) lwidth(wide) alwidth(none)) ///
    (lfitci rounds share_poorrain ///
    , acolor(orange%65) clcolor(orange%45) alwidth(none)) ///
    (lfitci rounds share_okayrain ///
    , acolor(green%65) clcolor(green%45) alwidth(none)) ///
    (scatter rounds share_goodrain ///
    , jitter(5) msymb(Oh) mcolor(ebblue%65)) ///  
    (scatter rounds share_okayrain ///
    , jitter(5) msymb(s) mcolor(green%55)) ///  
    (scatter rounds share_poorrain ///
    , jitter(5) msymb(Th) mcolor(orange%65)) ///
    , ytitle(Total number of rounds) ///
    xtitle(Share of rounds with random good/bad weather draws)
    

* Percentage change in beliefs vs. experimentation
	
	preserve
    * Relabel for graphs
	lab var num_correct "No. correct"
	lab var num_dontknow "No. q's = Don't know"
	lab var over_conf "Overconfident (0/1)"
	lab var rounds "No. rounds"
	lab var share_can_game "Share w/ CAN"
	lab var share_lime_game "Share w/ lime"
	lab var ph "pH"
	lab var fertseason_long "Share of seasons w/ fert."
	lab var usedap "Experience w/ DAP"
	lab var usecan "Experience w/ CAN"
	lab var p_size_acres "Land size (acres)"

* Control variables	
	global controls1 num_correct num_dontknow over_conf
	global controls2 $controls1 rounds share_can_game share_lime_game
	global controls3 $controls2 ph fertseason_long usedap usecan p_size_acres
	
* Fertilizer beliefs
	
	eststo clear
	eststo fert1: reg cmean_pctchange  ///
		$controls1 , r
	eststo fert2: reg cmean_pctchange ///
		$controls2 , r
	eststo fert3: reg cmean_pctchange ///
		$controls3 , r
	*esttab, starlevels(* 0.10 ** 0.05 *** 0.01)

* Fertilizer + lime beliefs	
	eststo lime1: reg dmean_pctchange  ///
		$controls1 , r
	eststo lime2: reg dmean_pctchange ///
		$controls2 , r
	eststo lime3: reg dmean_pctchange ///
		$controls3 , r
	esttab, starlevels(* 0.10 ** 0.05 *** 0.01)
	
	
* Divide title in two lines (too wide)
	local titles "& $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean \\"
	local subtitles "& fert. & fert. & fert. & fert+lime & fert+lime & fert+lime & \\ \hline"

* Table options
	global tableOpt1 label mlabels(none) nonum legend style(tex)
	global tableOpt2 cells(b(star fmt(2)) se(par fmt(2)))
	global tableOpt3 stats(r2 N  mean, fmt(2 0 2)
	global stars starlevels(* 0.10 ** 0.05 *** 0.01)
		
* Export regression results
	esttab fert1 fert2 fert3 lime1 lime2 lime2 ///
	using "$dataWork/analysis/output/tables/C2beliefs.tex" ///
	, $stars $tableOpt1 $tableOpt2 $tableOpt3 ///
	labels ("\$R^2\$" "\$N\$")) ///
	refcat(num_correct "\textit{Exploratory analysis:}", nolabel) ///
	substitute("<" "$<$" ">" "$>$") ///
	varlabels(_cons "Intercept") ///
	collabels(none) ///
	prehead(%) posthead(%) ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable is the percentage change in mean of the subjective yields distribution for fertilizer and fertilizer + lime, where noted. \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') append
	

	
	
* Instrumental variables regression assuming random rain does not directly 
* affect updating	
	
eststo clear
	
	eststo iv1: ivreg2 cmean_pctchange ph ///
	(share_lime_game = share_poorrain share_okayrain share_goodrain) ///
	, robust  ffirst first savefirst savefprefix(iv1_st1)
	
	eststo iv2: ivreg2 dmean_pctchange ph ///
	(share_lime_game = share_poorrain share_okayrain share_goodrain)   ///
	, robust ffirst  first savefirst savefprefix(iv2_st1)
	
/*	esttab iv1 iv2 using "$dataWork/analysis/output/tables/ivBeliefs.tex" ///
	, label nonum legend starlevels(* 0.10 ** 0.05 *** 0.01) ///
	cells(b(star fmt(2) label()) se(par fmt(2) label())) ///
    varlab(share_lime_game "Share of rounds w/ lime") ///
	stats(r2 N widstat, fmt(2 0) ///
	labels("\$R^2\$" "N" "Kleibergen-Paap F-stat")) style(tex)	///
	replace
*/	
	
* Divide title in two lines (too wide)titles
	local titles "& $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean & $\%\Delta$ Mean \\"
	local subtitles "& fert. & fert. & fert. & fert+lime & fert+lime & fert+lime & \\ \hline"

* Table options
	global tableOpt1 label nonum legend style(tex)
	global tableOpt2 cells(b(star fmt(2) label()) se(par fmt(2) label()))
	global tableOpt3 stats(r2 N  widstat, fmt(2 0 2) 
	global stars starlevels(* 0.10 ** 0.05 *** 0.01)
		
* Export regression results

*First Stage
	esttab iv1_st1* iv2_st1* ///
	using "$dataWork/analysis/output/tables/ivbeliefs.tex", ///
	$stars $tableOpt1 	substitute("<" "$<$" ">" "$>$") ///
	varlabels(_cons "Intercept" ph "pH" share_lime_game "Share of rounds w/ lime" ///
	share_poorrain "Share of rounds w/ poor rain" share_okayrain "Share of rounds w/ fair rain" share_goodrain "Share of rounds w/ good rain") ///
	mtitles("$\%\Delta$  Mean fert."  "$\%\Delta$ Mean fert+lime") noobs ///
	refcat(share_poorrain "\textit{First Stage}", nolabel) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{2SLS regression: effect of experimenting with lime on belief updating}"' ///
	`"\label{tab:ivBeliefs}"' ///
	`"\begin{tabular}{l cc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"') replace
	
	
	esttab iv1 iv2 ///
	using "$dataWork/analysis/output/tables/ivbeliefs.tex", ///
	nonum mlabels(none)$tableOpt2 $tableOpt3 ///
	labels("\$R^2\$" "N" "Kleibergen-Paap F-stat")) ///
	substitute("<" "$<$" ">" "$>$") ///
	varlabels(_cons "Intercept" ph "pH" share_lime_game "Share of rounds w/ lime") ///
	refcat(share_lime_game "\textit{Second Stage}", nolabel) ///	
	collabels(none) ///
	prehead(%) posthead(%) ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable: percentage change in mean of the subjective yields distribution for fertilizer and fertilizer + lime. Share of rounds played with lime is instrumented using the share of rounds that the respondent received good, fair, and bad rainfall draws (during rounds when they had no choice).) \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') append

/*	
*This code only produces second stage results. 
	esttab iv1 iv2 ///
	using "$dataWork/analysis/output/tables/ivbeliefs.tex" ///
	, mtitles("$\%\Delta$  Mean fert."  "$\%\Delta$ Mean fert+lime") ///
	$stars $tableOpt1 $tableOpt2 $tableOpt3 ///
	labels("\$R^2\$" "N" "Kleibergen-Paap F-stat")) ///
	substitute("<" "$<$" ">" "$>$") ///
	varlabels(_cons "Intercept" share_lime_game "Share of rounds w/ lime") ///
	collabels(none) ///
	prehead( `"\begin{table}[htbp]"' ///
	`"\centering"' ///
	`"\hspace*{-1.2cm}"' ///
	`"\begin{threeparttable}"' ///
    `"\caption{2SLS regression: effect of experimenting with lime on belief updating}"' ///
	`"\label{tab:ivBeliefs}"' ///
	`"\begin{tabular}{l cc}"' ///
	`"\hline"' `"\hline"') ///
	postfoot(`"\hline"' `"\hline"' ///
	`"\end{tabular}"' `"\begin{tablenotes}"' `"\footnotesize"' ///
	`"\item{Notes: Dependent variable: percentage change in mean of the subjective yields distribution for fertilizer and fertilizer + lime. Share of rounds played with lime is instrumented using the share of rounds that the respondent received good and bad rainfall draws (during rounds when they had no choice).) \\ Standard errors in parentheses \\ *\textit{p} < 0.10, **\textit{p} < 0.05, ***\textit{p} < 0.01.}"' ///
	`"\end{tablenotes}"' `"\end{threeparttable}"' `"\end{table}"') replace
*/		
restore	
	

	
	
* Get all files from output/tables
	local tables : dir "$dataWork/analysis/output/tables" files "*.tex", ///
    respectcase
	foreach file of local tables {
		copy "$dataWork/analysis/output/tables/`file'" ///
		"$dropbox/tables/`file'", replace
	}
	
	local apptables : dir "$dataWork/analysis/output/tables/appendix" ///
	files "*.tex", respectcase
	foreach file of local apptables {
		copy "$dataWork/analysis/output/tables/appendix/`file'" ///
		"$dropbox/tables_appendix/`file'", replace
	}

* Copy over figures	
	local tables : dir "$dataWork/analysis/output/figures" files "*.pdf", ///
    respectcase
	foreach file of local tables {
		copy "$dataWork/analysis/output/figures/`file'" ///
		"$dropbox/figures/`file'", replace
	}	

