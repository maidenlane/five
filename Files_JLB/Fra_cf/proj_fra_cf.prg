'
'===========================================
'       This program performs simulations over the future
     
'       Behaviors can be changed
'       Assumptions are set
'       Endogenous are initialized
'       A base simulation is produced
'       Assumptions are shocked 
'           and the consequences measured

'===========================================

'      We set the directory

cd "c:\users\jean louis\skydrive\eviews\book_files\fra_cf"

'    We define the output file as proj_1.rtf

output(r) proj_1

'      We copy the original mod_1 file into a forecast file called proj_1

'
close proj_1.wf1
close mod_1.wf1
open mod_1.wf1
save proj_1

'     We expand it until 2100

expand 1962Q1 2100Q4

'     We define long-term growth rates 
'     Actually they will be applied to Dlogs (the same to the first order)
'     as we shall see later

scalar txq=0.005
scalar txn=0.002
scalar txp=0.006  

'========================================
'       Changing elements in estimated equations
'========================================

'      We will show how to modify equations
'      taking the example of capital

'    We will do it also for the value added deflator, the wage rate, imports and exports

'      First we estimate the equation as usual
'      but we add a "1" to its usual name

smpl 1962Q1 2004Q4
genr k_ec=0
smpl 1962Q1 2003Q4
equation _eq_k1.ls(p) dlog(k)=c_k(1)*dlog(k(-1))+0.50*c_k(2)*(q-q(-4))/q(-4)-c_k(2)*(urd-ur)/ur+c_k(4)*@movav(rprob,3)+c_k(5)*((t-2005)*(t<=2005))+c_k(6)   +k_ec 

'    Now we create a vector of parameters
'    equivalent to the vector of coefficients
'    but with "p" instead of "c"

vector(10) p_k

'     We transfer the estimated values except the constant

p_k(1)=c_k(1)
p_k(2)=c_k(2)
p_k(3)=c_k(2)
p_k(4)=c_k(4)
p_k(5)=c_k(5)

'      We reproduce the equation
'       with the original name
'       but some coefficients are replaced by vector elements
'       (not the constant)

'      We estimate the equation and compute the residual

'       As written, the simulated equation will work in the same way as before
'       but we can fix the value of any set of coefficients
'       by replacing "c" by "p" and giving a value as above
'       the remaining coefficients will be estimated

'       This can apply to one or several coefficients
'        but not to the constant
'        as EViews requires at least one coefficient to estimate

equation _eq_k.ls(p) dlog(k)=p_k(1)*dlog(k(-1))+0.50*p_k(2)*(q-q(-4))/q(-4)-p_k(2)*(urd-ur)/ur+p_k(4)*@movav(rprob,3)+p_k(5)*((t-2005)*(t<=2005))+c_k(6)+k_ec 
genr k_ec=resid

'       Forecasting the residuals for 2004 is done as usual

for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_k.forecast(d) _z
genr k_ec=dlog(k)-_z
next
_mod_1.merge _eq_k

'   ------- changing the wage rate equation -----------------------------------------------------------------------------------

smpl 1962Q1 2004Q4
genr wr_ec=0
vector(10) p_wr
p_wr(1)=1	 	 
smpl 1962Q1 2003Q4
equation _eq_wr1.ls(p) dlog(wr)=c_wr(1)*@movav(dlog(pcoh),4)+c_wr(2)*@movav(d(unr),4)+c_wr(3)*res_wr(-1)+c_wr(4)*(t=1978)+c_wr(5)*(t-2005)*(t<=2005)+c_wr(6)+wr_ec
p_wr(1)=p_wr(1)
p_wr(2)=c_wr(2)
p_wr(3)=c_wr(3)
p_wr(4)=c_wr(4)
p_wr(5)=c_wr(5)


p_res_wr(1)=0.5
p_res_wr(2)=p_res_wr(2)
p_res_wr(3)=p_res_wr(3)
equation _eq_wr.ls(p) dlog(wr)=p_wr(1)*@movav(dlog(pcoh),4)+p_wr(2)*@movav(d(unr),4)+p_wr(3)*res_wr(-1)+p_wr(4)*(t=1978)+c_wr(5)*(t-2005)*(t<=2005)+c_wr(6)+wr_ec
_mod_1.update
genr wr_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_wr.forecast(d) _z
genr wr_ec= dlog(wr)-_z
next

'   ----- changing the value added deflator equation -------------------------------------------------

 
smpl 1962Q1 2004Q4
vector(10) p_pq
genr pq_ec=0
smpl 1962Q1 2003Q4
equation _eq_pq1.ls(p) dlog(pq)=c_pq(1)*dlog(uwc)+c_pq(2)*dlog(ur)+c_pq(3)*log(pq(-1)/uwc(-1))-c_pq(4)*log(ur(-1))+c_pq(5)+pq_ec
p_pq(1)=c_pq(1)
p_pq(2)=c_pq(2)
p_pq(3)=c_pq(3)
p_pq(4)=c_pq(4) 
equation _eq_pq.ls(p) dlog(pq)=p_pq(1)*dlog(uwc)+p_pq(2)*dlog(ur)+p_pq(3)*log(pq(-1)/uwc(-1))-p_pq(4)*log(ur(-1))+c_pq(5)+pq_ec
genr pq_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_pq.forecast(d) _z
genr pq_ec= dlog(pq)-_z
next

'    ----- changing the imports equation -----------------------------------------------------

 
coef(10) c_m
smpl 1962Q1 2004Q4
genr m_ec=0
smpl 1962Q1 2003Q4
equation _eq_m1.ls(p) dlog(m)=dlog(fd+0.5*x)+c_m(2)*log(ur)+c_m(3)*log(@movav(compm,6))+c_m(4)+c_m(5)*(t-2005)*(t<=2005)+[ar(1)=c_m(6)]+c_m(7)*log(m(-1)/(fd(-1)+0.5*x(-1)))+m_ec
genr resid_m=resid
smpl 1962Q1 2003Q4 if (resid<>na) and (resid(-1)=na)
_eq_m.forecast(d,s) _z
genr m_ec=dlog(m)-_z
smpl 1962Q1 2003Q4 if (resid_m<>na) and (resid_m(-1)<>na)
genr m_ec=resid_m+m_ec(-1)*c_m(6)
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_m.forecast(d) _z
genr m_ec= dlog(m)-_z
next

'    ----- changing the exports equation -----------------------------------------------------

vector(10) p_x
smpl 1962Q1 2004Q4
genr x_ec=0
smpl 1962Q1 2003Q4
equation _eq_x1.ls(p) dlog(x)=c_x(1)*dlog(wd)+c_x(2)*log(x(-1)/wd(-1))+c_x(3)*0.5*(log(ur)+log(ur(-1)))+c_x(4)*log(compx)+c_x(5)+c_x(6)*(t-2005)*(t<=2005)+x_ec
p_x(1)=c_x(1)
p_x(2)=c_x(2)
p_x(3)=c_x(3)
p_x(4)=c_x(4)
p_x(6)=c_x(6)
equation _eq_x.ls(p) dlog(x)=p_x(1)*dlog(wd)+p_x(2)*log(x(-1)/wd(-1))+p_x(3)*0.5*(log(ur)+log(ur(-1)))+p_x(4)*log(compx)+c_x(5)+p_x(6)*(t-2005)*(t<=2005)+x_ec
genr x_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_x.forecast(d) _z
genr x_ec= dlog(x)-_z
next

'==========================================================
'     Setting the assumptions for the exogenous
'==========================================================

'      The above values will be applied to the Dlogs 
'       instead of being used as growth rates 

'       The reason quite technical (we are in a study phase)
'        It makes it easier to observe convergence to a steady state path 
'        (in particular visually)
'        if the dimension of the variable is a combination of basic ones
'        for instance for values (price+quantity) and wage rates (price+quantity-population)

'        A combination of "round" dlogs is a round dlog
'        which is not exactly true for growth rates

'        However for actual forecasts one can prefer reverting to growth rates
'        One just has to replace "dlog" by "@pch" 
'        in the following formulas

	'   Constant values : Dlog=0

smpl 2005Q1 2100Q4
for %1 COH_EC ERX FDXR  K_EC CI_EC IR_ER IRL_ER IRMX IRSR IRST IRSX IRX LF_EC  M_EC NIF_ER NIG_ER PK PM_EC POPAC_EC PQ_EC PX_EC R_EXPG R_ICT R_IFP R_OIT R_PCOH R_PCOG R_PI R_PIG R_REVG R_REVQ R_SCF R_SCG R_SCW R_SUBS R_TAR R_TARX R_VAT RDEP TC URD  WR_EC X_EC IRM_ER relax_q   relax_pfd fcapf_er prof_er
genr {%1}={%1}(-1)
next

	'  Variables at constant prices : Dlog=txq

for %1  COG   IG   WD R_REVX HIH
genr {%1}={%1}(-1)*exp(txq)
next

	'  Populations : Dlog=txn

for %1 LG POP65 POP
genr {%1}={%1}(-1)*exp(txn)
next

	'  Prices : Dlog=txp

for %1  PPX 
genr {%1}={%1}(-1)*exp(txn)
next

'            Purchasing power per head : Dlog=txp-txn

for %1   SOCBR
genr {%1}={%1}(-1)*exp(txq-txn)
next

'            The time trend

genr t=t(-1)+0.25

'             Now we give values to the endogenous
'             There are two main reasons

'             1 - This gives an alternate starting point for the solution algorithms
'              Sometimes this starting point is more efficient
'              One can just check

'             2 - This allows to make a residual check on the future
'              which is useful to check that :
'              using the exogenous assumptions 
'              applying the formula provides a steady state path
'              with the same growth rate as the variable it computes

'              When the condition is met for all equations
'              a steady state path exists
'              and will be reached provided the process is converging

'              A difference can come from :
'              - wrong dimensions in the assumptions
'              - non homogenous formulas
'              - wrong initialization of endogenous (not a real problem)

smpl 2005Q1 2100Q4
 
'        Constant values : Dlog=0

for %1   COMPM COMPX ER  FCAPGP IR IRL IRM IRS RCVAL RCVOL RES_WR  RMARG RPROB RPROF TTRAD UNR  UR
genr {%1}={%1}(-1)
next

'       Variables at constant prices : Dlog=txq

for %1  CAP COH  FD HRDI I CI K  M  Q  X GDPM IC 
genr {%1}={%1}(-1)*exp(txq)
next

'       Populations : Dlog=txn

for %1 LFD  LF  LT   POPAC UN  popt
genr {%1}={%1}(-1)*exp(txn)
next

'      Deflators : Dlog = txp

for %1    PCOH PFD PFDXT PI PIG PM PMT  PP PQ PX UWC
genr {%1}={%1}(-1)*exp(txp)
next

'       Variables at current prices : Dlog = txp+txq

for %1 CGV  EXPG FCAPF FCAPG FCAPX FDGV HDI HI ICT IFP IGV MARG MVAL NIF NIG NIX NIXL NIXX OIT PROF GDPMVAL QVAL  REVG REVQ REVX  SCF SCG SCW SOCB SUBS TAR TRB  VAT W WF WG   XVAL  
genr {%1}={%1}(-1)*exp(txp+txq)
next

'       Variables at current prices  per head : Dlog=txp+txq-txn

for %1 WR  
genr {%1}={%1}(-1)*exp(txp+txq-txn)
next

'      Variables at constant prices  per head : Dlog=txp+txq-txn

for %1  PL  PLT
genr {%1}={%1}(-1)*exp(txq-txn)
next

'==========================================================
'     Solving the model on the future (basic simulation)

'==========================================================

'      Now we solve the model on the future
'      until 2100
'      using the basic assumptions

'       The scenario is "scenario 1"
'       The suffix is "_b"
'       The convergence criterion is 10-6 
'                   (relative difference from an iteration to the next)
'        The maximum number of iterations is 1001
'                    giving a special number can help to associate a failure message
'                    to a specific solve statement'               
'        The exclude statement states that all endogenous will be computed
'                    it cancels any previous exclusions
'                    which may remain from previous simulations of "Scenario 1"

smpl 2005Q1 2100Q4
_mod_1.append assign @all _b
_mod_1.solveopt(n=t,m=1000,c=1e-6,o=g,d=d )
_mod_1.scenario "scenario 1"
_mod_1.exclude 
smpl 2005Q1 2100Q4
solve(n=t,m=1001,c=1e-6,o=g,d=d ) _mod_1

'         Now we compute the growth rates
'         or rather here the dlogs

'         We compute the ratio of the present to the previous value
'         for each endogenous
'         If it is positive we take the logarithm to get the Dlog
'         otherwise we give the value "na"
'         This gives the same result as a direct computation
'          but avoids the error messages

for !i=1 to _g_vendo.@count
%st1=_g_vendo.@seriesname(!i)
smpl 2005Q1 2100Q4
series _z=({%st1}_1+({%st1}_1=0))/({%st1}_1(-1)+({%st1}_1(-1)=0))
series {%st1}_tc=na
smpl 2005Q1 2100Q4 if _z>0
series {%st1}_tc=log(_z)

'             Now we do the same for the initialization values
'             and compute the difference on the growth rates

smpl 2005Q1 2100Q4
series _z=({%st1}+({%st1}=0))/({%st1}(-1)+({%st1}(-1)=0))
series {%st1}_tc0=na
smpl 2005Q1 2100Q4 if _z>0
series {%st1}_tc0=log(_z)
series {%st1}_DTC=100*({%st1}_tc-{%st1}_tc0)
next

'==========================================================
'     Solving the model on the future (shocked assumptions)
'==========================================================

'     Now we shall produce a set shocks (in the present case 7)

smpl 2000 2500

'    The group called shocks_v will contain the list of 7 shocked variables

group shocks_v ig erx r_vat r_tarx r_tar wd m_ec  

'    The group called shocks_l will contain 7 letters associated to each shock
'     but the letters must be known as series 
'     as groups can contain only series

'     We create artificial series 
'      but only for the non-existing ones of course (not t or x or m.......)


if @isobject("shocks_l") then
delete shocks_l
endif
group shocks_l
for %1 gd ex vt tx tl dx dl  
if @isobject(%1)=0 then
genr {%1}=na
endif
shocks_l.add {%1}
next 

'      We create the group 

'    Now we compute the additional change for each assumption, 
'    using the name of the variable
'    The shock will only start in 2006
'    to check that the difference comes only from the shock

'      1 - Shock g : +1 GDP point on Government demand

genr ig_gd=ig+.01*gdpm_1*(t>=2006)

'      2 - Shock r : 1% devaluation of the Euro

genr erx_ex=erx*(1+.01*(t>=2006))

'      3 - Shock t : -1 point on the VAT rate

genr r_vat_vt=r_vat-.01*(t>=2006)

'      4 - Shock y : -1 point on the foreign tariffs rate

genr r_tarx_tx=r_tarx-.01*(t>=2006)

'      5 - Shock f : -1 point on the local tariffs rate

genr r_tar_tl=r_tar-.01*(t>=2006)

'      6 - Shock w : +1% on World demand addressed to France

genr wd_dx=wd*(1+.01*(t>=2006))

'      7 - Shock n : +1% on ex-ante imports

genr m_ec_dl=m_ec+.01*(t>=2006) 

'     The loop on the shocks

for !j=1 to shocks_v.@count

smpl 2005Q1 2100Q4

'   We get the name of the shocked variable and the associated letter

%2=shocks_v.@seriesname(!j)
%3=shocks_l.@seriesname(!j)

'    We set the solution suffix using the letter 
'    We control no endogenous is excluded from the simulation
'    We override the variable associated to the current shock
'    We solve the model

smpl 2005Q1 2100Q4

'    We solve the model

_mod_1.scenario "scenario 1"
_mod_1.append assign @all _{%3}
_mod_1.exclude  
_mod_1.override {%2}
solve(n=t,m=1002,c=1e-6,o=g,d=d) _mod_1

'   We compute the difference to the base simulation, in absolute and relative terms

for !i=1 to _g_vendo.@count
%st1=_g_vendo.@seriesname(!i)
series d{%3}_{%st1}={%st1}_{%3}-{%st1}_1
series p{%3}_{%st1}=100* d{%3}_{%st1}/({%st1}_1+ ({%st1}_1=0))
series dv_{%st1}=d{%3}_{%st1}
series pv_{%st1}=p{%3}_{%st1}
next

'   We create groups for variations for a specific list of important variables

group g_v{%3} P{%3}_PFD P{%3}_PM P{%3}_PX P{%3}_PQ P{%3}_WR P{%3}_FD P{%3}_gdpm P{%3}_X P{%3}_M P{%3}_I P{%3}_COH P{%3}_UR P{%3}_CAP  P{%3}_LF P{%3}_K P{%3}_UNR  D{%3}_FCAPGP P{%3}_RCVAL P{%3}_RCVOL P{%3}_TTRAD 

'   Or very important ones

group g_w{%3} P{%3}_FD P{%3}_gdpm P{%3}_X P{%3}_M   P{%3}_PQ  

'    We create a group for variations for all variables

group g_v2 pV_* dV_* 

'      We store all shocks in Excels files (2025 and 2100)
'      using the letter associated to the shock

smpl 2005Q1 2025Q4
write(t=xls) v2_{%3}.xls g_v2 
smpl 2005Q1 2100Q4
write(t=xls) w2_{%3}.xls g_v2 

next

'     Producing the graphs

alpha title_gd="government demand"
alpha title_vt="the value added tax"  
alpha title_ex="the exchange rate" 
alpha title_tx="foreign tariffs"
alpha title_tl="local tariffs"
alpha title_dx="shock on world demand"
alpha title_dl="local quotas"

alpha units_gd="+1 point of GDP"
alpha units_vt="-1 point of the rate" 
alpha units_ex="1% on the average rate" 
alpha units_tx="-1 point of the rate"
alpha units_tl="-1 point of the rate" 
alpha units_dx="+1% of ex ante world demand"
alpha units_dl="+1% of ex ante imports "

for !j=1 to shocks_v.@count

smpl 2005Q1 2060Q4 2096Q1 2100Q4

'   We get the name of the shocked variable and the associated letter

%2=shocks_v.@seriesname(!j)
%3=shocks_l.@seriesname(!j)

for !i=1 to 6
%gr="_gr_"+%3+@str({!i})
if @isobject(%gr) then
delete {%gr}
endif
next

%title=@elem(title_{%3},"2020")
%units=@elem(units_{%3},"2020")

'    Supply - demand graph

graph _gr_{%3}1.line p{%3}_fd p{%3}_q p{%3}_x p{%3}_m p{%3}_pq 
_gr_{%3}1.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Final demand) symsize(s)
_gr_{%3}1.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(GDP) symsize(s)
_gr_{%3}1.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Exports) symsize(s)
_gr_{%3}1.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Imports) symsize(s)
_gr_{%3}1.setelem(5) symbol(10) lcolor(purple) lwidth(0.25) lpat(5) legend(GDP deflator)  symsize(s)
_gr_{%3}1.legend position(botleft)
_gr_{%3}1.options gridl gridauto 
_gr_{%3}1.addtext(t,font=12,x) Shock on {%title} : The supply - demand equilibrium  
_gr_{%3}1.addtext(l,font=11) in percentage 
_gr_{%3}1.addtext(r,font=10) shock : {%units}

print(l) _gr_{%3}1

close _gr_{%3}1 

'    Production

graph _gr_{%3}2.line P{%3}_q p{%3}_cap p{%3}_k p{%3}_lf p{%3}_ur
_gr_{%3}2.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Value added) symsize(s)
_gr_{%3}2.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Capacity) symsize(s)
_gr_{%3}2.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Capital) symsize(s)
_gr_{%3}2.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Employment) symsize(s)
_gr_{%3}2.setelem(5) symbol(10) lcolor(purple) lwidth(0.25) lpat(5) legend(Rate of use)  symsize(s)
_gr_{%3}2.legend position(botleft)
_gr_{%3}2.options gridl gridauto 
_gr_{%3}2.addtext(t,font=12,x)  Shock on {%title} : The production elements 
_gr_{%3}2.addtext(l,font=11) in percentage
_gr_{%3}2.addtext(r,font=10) shock : {%units} 

print(l) _gr_{%3}2

close _gr_{%3}2


'    Ratios

graph _gr_{%3}3.line P{%3}_wr-P{%3}_pfd p{%3}_k-p{%3}_lf   p{%3}_ur d{%3}_unr
_gr_{%3}3.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Relative cost) symsize(s)
_gr_{%3}3.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Capital-Labour ratio) symsize(s)
_gr_{%3}3.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Rate of use) symsize(s)
_gr_{%3}3.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Unemployment rate in points)  symsize(s)
_gr_{%3}3.legend position(botleft)
_gr_{%3}3.options gridl gridauto 
_gr_{%3}3.addtext(t,font=12,x)  Shock on {%title} : The ratios 
_gr_{%3}3.addtext(l,font=11) in percentage
_gr_{%3}3.addtext(r,font=10) shock : {%units}  

print(l) _gr_{%3}3

close _gr_{%3}3

'    External trade 1

graph _gr_{%3}4.line p{%3}_xval p{%3}_mval p{%3}_rcval
_gr_{%3}4.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Exports) symsize(s)
_gr_{%3}4.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Imports) symsize(s)
_gr_{%3}4.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Exports-Imports ratio)  symsize(s)
_gr_{%3}4.legend position(botleft)
_gr_{%3}4.options gridl gridauto 
_gr_{%3}4.addtext(t,font=12,x)  Shock on {%title} : External trade at current prices  
_gr_{%3}4.addtext(l,font=11) in percentage
_gr_{%3}4.addtext(r,font=10) shock : {%units}  

' print(l) _gr_{%3}4

close _gr_{%3}4


'    External trade 2

graph _gr_{%3}5.line p{%3}_rcvol p{%3}_ttrad p{%3}_rcval
_gr_{%3}5.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(At constant prices) symsize(s)
_gr_{%3}5.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Terms of trade) symsize(s)
_gr_{%3}5.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(At constant prices)  symsize(s)
_gr_{%3}5.legend position(botleft)
_gr_{%3}5.options gridl gridauto 
_gr_{%3}5.addtext(t,font=12,x)  Shock on {%title} : The exports - import ratios 
_gr_{%3}5.addtext(l,font=11) in percentage
_gr_{%3}5.addtext(r,font=10) shock : {%units}  

print(l) _gr_{%3}5

close _gr_{%3}5

'    Prices

graph _gr_{%3}6.line p{%3}_wr p{%3}_pfd p{%3}_pq p{%3}_px p{%3}_pm
_gr_{%3}6.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Wage rate) symsize(s)
_gr_{%3}6.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Consumption) symsize(s)
_gr_{%3}6.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Value added) symsize(s)
_gr_{%3}6.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Exports) symsize(s)
_gr_{%3}6.setelem(5) symbol(10) lcolor(purple) lwidth(0.25) lpat(5) legend(Imports)  symsize(s)
_gr_{%3}6.legend position(botleft)
_gr_{%3}6.options gridl gridauto 
_gr_{%3}6.addtext(t,font=12,x)  Shock on {%title} : The prices   
_gr_{%3}6.addtext(l,font=11) in percentage 
_gr_{%3}6.addtext(r,font=10) shock : {%units} 

' print(l) _gr_{%3}6

close _gr_{%3}6

next




'       We save the workfile

save proj_1.wf1


