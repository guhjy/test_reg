capture program drop test4
program test4, byable(recall)
version 11.0
syntax varlist [if] [in] [pweight], link(string) iv(string) [regOpt(string) ///
									cv(string) cont cons header(integer 2) ///
									ci(integer 95) predict noclear goon]
	marksample touse, novarlist
	if _byindex() == 1{ //ROUTINE AT START: PARSE AND PREPARE DATASET
	
		//PARSER START
		foreach x in iv cv{
			local `x'_l 0
			local `x' = trim(ltrim(subinstr(subinstr("``x''", "+", " + ", .), ",", " , ", .))) // lol sorry
			tokenize "``x''"
			while "`1'" != ""{
				if !("`1'" == "," | "`1'" == "+" ){
					local buff_a "`buff_a' `1' "
					if strmatch("`1'", "*.*"){
						gettoken tmp 1 : 1, parse(.)
						local 1 : subinstr local 1 "." ""
						qui levelsof `1'
						foreach lvl in `r(levels)'{
							local tmL : label (`1') `lvl'
							local tmL : subinstr local tmL " " "@_@", all
							local buff_c "`buff_c' `tmL' "
							local buff_b "`buff_b' `lvl'.`1'"
						}
					}
					else{
						local tmL : variable label `1'
						local tmL : subinstr local tmL " " "@_@", all
						local buff_c "`buff_c' `tmL' "
						local buff_b "`buff_b' `1' "
					}
				}
				if ("`1'" == "," | "`1'" == "+" | "`2'" == ""){
					global `x'_mod_`++`x'_l'_a : list clean local buff_a
					global `x'_mod_``x'_l'_b : list clean local buff_b
					global `x'_mod_``x'_l'_c : list clean local buff_c
					if "`1'" == "," | "`2'" == ""{
						local buff_a ""
						local buff_b ""
						local buff_c ""
					}
				}
				mac shift
			}
			global `x'_l = ``x'_l'
		}
		// PARSER END. MACROS NOW DEFINE ALL MODELS AND THEIR VARIABLES

		// PREPARE DATAFILE
		if "`goon'" == ""{
			capture drop ____*
			if "`clear'" == "noclear"{
				qui gen ____regGroup = .
				global gpnum 0
			}
			if _by() qui gen ____byvar = ""
			global header = `header'
			qui gen str20 ____testvar = ""
			qui gen str20 ____labvar = ""
			if (`iv_l' > 1 | "`clear'" == "noclear") qui gen ____indMod = .
			if (`cv_l' > 1 | "`clear'" == "noclear") qui gen ____conMod = .
			foreach var of local varlist{
				qui gen ____`var'_est = .
				qui gen ____`var'_se = .
				qui gen ____`var'_min`ci' = .
				qui gen ____`var'_max`ci'= .
				qui gen ____`var'_p = .
			}
		}
		// END PREPARE DATAFILE
	}
	
	// START OF REGRESSIONS, FOR EACH BY GROUP
	else local header = ${header}	
	local i 0 
	forvalues ivm = 1(1)$iv_l{
		local cvl = $cv_l + !$cv_l
		forvalues cvm = 1(1)`cvl'{
			if "`cont'" == "" local uselist "${iv_mod_`ivm'_b}"
			if "`cont'" == "cont" local uselist "${iv_mod_`ivm'_b} ${cv_mod_`cvm'_b}"
			local matLen = wordcount("${iv_mod_`ivm'_b}") + ("`cont'"=="cont")*wordcount("${cv_mod_`cvm'_b}")
			mata: tmpMat = tokens("`uselist'")'
			mata: st_sstore((`header' + 1, `header' + `matLen'), ("____testvar"), tmpMat)
			mata: st_sstore((`header' + 1, `header' + `matLen'), ("____labvar"), tokens("${iv_mod_`ivm'_c} ${cv_mod_`cvm'_c}")'[1::`matLen'])
			foreach var of local varlist{
				`link' `var' ${iv_mod_`ivm'_a} ${cv_mod_`cvm'_a} if `touse' [`weight'`exp'], `regOpt' 
				if "`predict'" == "predict"{
					mata: st_store((`header' + 1, `header' + `matLen'), ("____`var'_est"), st_matrix("e(b)")'[1::`matLen'])
					mata: st_store((`header' + 1, `header' + `matLen'), ("____`var'_se"), diagonal(st_matrix("e(V)"))[1::`matLen'])
				}
				mata: st_store((`header' + 1, `header' + `matLen'), ("____`var'_est"), st_matrix("e(b)")'[1::`matLen'])
				mata: st_store((`header' + 1, `header' + `matLen'), ("____`var'_se"), diagonal(st_matrix("e(V)"))[1::`matLen'])
				if ${iv_l} > 1 mata: st_store((`header' + 1, `header' + `matLen'), ("____indMod"), J(`matLen', 1, `ivm'))
				if ${cv_l} > 1 mata: st_store((`header' + 1, `header' + `matLen'), ("____conMod"), J(`matLen', 1, `cvm'))
			}
		local header = `header' + `matLen' + 2
		}
	}
	// END REGRESSIONS
	
	// START OF POST-PROCESSING
	if _by(){ 	
		qui levelsof `_byvars' if `touse'
		qui replace ____byvar = "`r(levels)'" if ____testvar != "" in ${header}/`header'
	}
	
	// LAST LOOP POST-PROCESSING
	if _bylastcall(){
		qui replace ____labvar = subinstr(____labvar, "@_@", " ", .)
		if ${cv_l} > 1 order ____conMod, after(____labvar)
		if ${iv_l} > 1 order ____indMod, after(____labvar)
		capture confirm variable ____byvar
		if _rc == 0{
			qui destring ____byvar, replace
			local labtmp : value label `_byvars'
			label values ____byvar `labtmp'
			order ____byvar, after(____labvar)
		}
		foreach var of local varlist{
			qui replace ____`var'_se = sqrt(____`var'_se)
			if "`e(vce)'" == "cluster"{
				qui replace ____`var'_min`ci' = ____`var'_est - invt(`e(df_r)', (1+0.`ci')/2)*____`var'_se
				qui replace ____`var'_max`ci' = ____`var'_est + invt(`e(df_r)', (1+0.`ci')/2)*____`var'_se
				qui replace ____`var'_p = 2*t(`e(df_r)', ____`var'_est/____`var'_se)
			}
			else{
				qui replace ____`var'_min`ci' = ____`var'_est - invnorm((1+0.`ci')/2)*____`var'_se
				qui replace ____`var'_max`ci' = ____`var'_est + invnorm((1+0.`ci')/2)*____`var'_se
				qui replace ____`var'_p = 2*normal(-abs(____`var'_est / ____`var'_se))
			}
		}
		if "`clear'" == "noclear" | "`goon'" == "goon"{
			global gpnum = $gpnum + 1
			replace ____regGroup = $gpnum if ____testvar != "" in ${header}/`header'
		}
		if "`clear'" == "" 	& ${iv_l} > 1 label values ____indMod iv_label
		if "`clear'" == "" 	& ${cv_l} > 1 label values ____conMod cv_label
		if "`clear'" == "" 	mac drop _all //cv* iv* header gpnum
		if "`clear'" == ""{
			qui levelsof ____indMod
			if "`r(levels)'" == "" drop ____indMod
			qui levelsof ____conMod
			if "`r(levels)'" == "" drop ____conMod
		}
		else global header = `header'
		mata: mata clear
	}
	else global header = `header'
	// END POST-PROCESSING
	
end
