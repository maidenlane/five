' ________________________________________________
'
'        This prgram solves the model over the future
'        and produces a series of seven shocks

'_________________________________________________

'   We specify the directory

cd "c:\users\jean louis\skydrive\eviews\book_files\fra_cd"

'    We specify the output file

output(r,o) proj_cd

'     we set the workfile to mod_cd_1, quarterly from 1962 to 2100
'       
'
close proj_cd_1.wf1
close mod_cd_1.wf1
open mod_cd_1.wf1
save proj_cd_1
pageselect model
pagecopy(page=proj)
expand 1962Q1 2100Q4

'	we define long-term growth rates 

scalar txq=exp(0.005)-1
scalar txn=exp(0.002)-1
scalar txp=exp(0.006)-1

genr relax_q=0.30
genr relax_pfd=0.5

save proj_cd_1

'   ------------------------------------------------------------------------------------------

smpl 1962Q1 2004Q4
genr wr_ec=0
vector(10) p_wr
p_wr(1)=1.00	 	 
smpl 1962Q1 2003Q4
equation _eq_wr1.ls(p) dlog(wr)=c_wr(1)*@movav(dlog(pc),4)+c_wr(2)*@movav(d(unr),4)+c_wr(3)*res_wr(-1)+c_wr(4)*(t=1978)+c_wr(5)*(t-2005)*(t<=2005)+wr_ec
p_wr(1)=c_wr(1)
p_wr(2)=c_wr(2)
p_wr(3)=c_wr(3)
p_wr(4)=c_wr(4)
p_wr(5)=c_wr(5)
equation _eq_wr.ls(p) dlog(wr)=p_wr(1)*@movav(dlog(pc),4)+p_wr(2)*@movav(d(unr),4)+p_wr(3)*res_wr(-1)+p_wr(4)*(t=1978)+c_wr(5)*(t-2005)*(t<=2005)+wr_ec
genr wr_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_wr.forecast(d) _z
genr wr_ec= dlog(wr)-_z
next

'   ------------------------------------------------------------------------------------------------------------

'    estimating the GDP deflator

smpl 1962Q1 2004Q4
vector(10) p_pq
genr pq_ec=0
smpl 1962Q1 2003Q4
equation _eq_pq1.ls(p) dlog(pq)=dlog(pq)=c_pq(1)*dlog(cost)+c_pq(2)*dlog(ur)+c_pq(3)*log(pq(-1)/cost(-1))-c_pq(2)*c_pq(3)*log(ur(-1))+c_pq(5) +pq_ec
p_pq(1)=c_pq(1)
p_pq(2)=c_pq(2)
p_pq(3)=c_pq(3)
equation _eq_pq.ls(p) dlog(pq)=c_pq(1)*dlog(cost)+c_pq(2)*dlog(ur)+c_pq(3)*log(pq(-1)/cost(-1))-c_pq(2)*c_pq(3)*log(ur(-1))+c_pq(5)+pq_ec
genr pq_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_pq.forecast(d) _z
genr pq_ec= dlog(pq)-_z
next


'    ----------------------------------------------------------

'    estimating imports

coef(10) c_m
smpl 1962Q1 2004Q4
genr m_ec=0
smpl 1962Q1 2003Q4
equation _eq_m1.ls(p) dlog(m)=1*dlog(fd+0.5*x)+c_m(2)*dlog(ur)+c_m(3)*log(@movav(compm,6))+c_m(4)+c_m(7)*log(m(-1)/(fd(-1)+0.5*x(-1))) -c_m(7)*c_m(2)*log(ur(-1))+[ar(1)=c_m(6)]+m_ec
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




'    ----------------------------------------------------------

'    estimating exports

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

' we expand series for projection : EXOGENOUS variables

	' constant expansion

smpl 2005Q1 2100Q4
for %1 COH_EC ERX FDXR  K_EC IC_EC IR_ER IRL_ER IRMX IRSR IRST IRSX IRX LF_EC  M_EC NIF_ER NIG_ER PK PM_EC POPAC_EC PQ_EC PX_EC R_EXPG R_ICT R_IFP R_OIT R_PC R_PI R_PIG R_REVG R_REVQ R_SCF R_SCG R_SCW R_SUBS R_TAR R_TARX R_VAT RDEP TC URD  WR_EC X_EC relax_q relax_pfd
genr {%1}={%1}(-1)
next

	' expansion with long-term GDP growth rate

for %1  COG GD IG   WD R_REVX 
genr {%1}={%1}(-1)*(1+txq)
next

	' expansion with LT population growth rate

for %1 LG POP65
genr {%1}={%1}(-1)*(1+txn)
next

	' expansion with LT deflators growth rate

for %1  PPX 
genr {%1}={%1}(-1)*(1+txp)
next

for %1   SOCBR
genr {%1}={%1}(-1)*(1+txq)/(1+txn)
next

genr t=t(-1)+0.25

smpl 2005Q1 2100Q4
 
' we expand series for projection : we initialize ENDOGENOUS variables 

 

for %1   COMPM COMPX ER  FCAPGP IR IRL IRM IRS RCVAL RCVOL RES_WR  RMARG RPROB RPROF TTRAD UNR  UR  
genr {%1}={%1}(-1)
next

for %1  CAP COH  FD HRDI I IC ID K  M  Q  X GDPM KD
genr {%1}={%1}(-1)*(1+txq)
next

for %1 LFD  LF  LT   POPAC UN  popt
genr {%1}={%1}(-1)*(1+txn)
next

for %1    PC PFD PFDXT PI PIG PM PMT  PP PQ PX UWC PGDPM COST
genr {%1}={%1}(-1)*(1+txp)
next

for %1 WR 
genr {%1}={%1}(-1)*(1+txp)*(1+txq)/(1+txn)
next

for %1 COGV  EXPG FCAPF FCAPG FCAPX FDGV HDI HI ICT IFP IGV MARG MVAL NIF NIG NIX NIXL NIXX OIT PROF GDPMVAL QVAL  REVG REVQ REVX  SCF SCG SCW SOCB SUBS TAR TRB  VAT W WF WG   XVAL  
genr {%1}={%1}(-1)*(1+txp)*(1+txq)
next

for %1  PL  PLT RELC
genr {%1}={%1}(-1)*(1+txq)/(1+txn)
next

' we solve the model in PROJECTION (NO SHOCK)

genr irst=3
genr relax_q=0.5
genr relax_pfd=0.5

smpl 2005Q1 2100Q4
_mod_1.append assign @all _b
_mod_1.solveopt(n=t m=1001,c=1e-6,o=g,d=d)
_mod_1.scenario "scenario 1"
_mod_1.exclude 
smpl 2005Q1 2100Q4
solve(n=t m=1001 c=1e-6 o=g d=d) _mod_1

for !i=1 to _g_vendo.@count
%st1=_g_vendo.@seriesname(!i)
smpl 2005Q1 2100Q4
series _z=({%st1}_1+({%st1}_1=0))/({%st1}_1(-1)+({%st1}_1(-1)=0))
series {%st1}_tc=na
smpl 2005Q1 2100Q4 if _z>0
series {%st1}_tc=log(_z)

smpl 2005Q1 2100Q4
series _z=({%st1}+({%st1}=0))/({%st1}(-1)+({%st1}(-1)=0))
series {%st1}_tc0=na
smpl 2005Q1 2100Q4 if _z>0
series {%st1}_tc0=log(_z)
series {%st1}_DTC=100*({%st1}_tc-{%st1}_tc0)
next

 
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

'   We get the name of the variable and the letter

%2=shocks_v.@seriesname(!j)
%3=shocks_l.@seriesname(!j)

'   We set the solution suffix using the letter

'   We shock the assumption

smpl 2005Q1 2100Q4

'    We solve the model

_mod_1.scenario "scenario 1"
_mod_1.append assign @all _{%3}
_mod_1.exclude  
_mod_1.override {%2}
solve(n=t m=10002 c=1e-6 o=g d=d) _mod_1

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

'      We store all shocks in Excels files (2020 and 2100)
'      using the letter associated to the shock

smpl 2005Q1 2024Q4
write(t=xls) v2_{%3}.xls g_v2 
smpl 2005Q1 2100Q4
write(t=xls) w2_{%3}.xls g_v2 

next

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

smpl 2005Q1 2035Q4 2096Q1 2100Q4

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
_gr_{%3}1.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Final demand) symsize(xs)
_gr_{%3}1.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(GDP) symsize(xs)
_gr_{%3}1.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Exports) symsize(xs)
_gr_{%3}1.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Imports) symsize(xs)
_gr_{%3}1.setelem(5) symbol(10) lcolor(purple) lwidth(0.25) lpat(5) legend(GDP deflator)  symsize(xs)
_gr_{%3}1.legend position(botleft)
_gr_{%3}1.options gridl gridauto 
_gr_{%3}1.addtext(t,font=12,x) Shock on {%title} : The supply - demand equilibrium  
_gr_{%3}1.addtext(l,font=11) in percentage 
_gr_{%3}1.addtext(r,font=10) shock : {%units}

print(l) _gr_{%3}1

close _gr_{%3}1 

'    Production

graph _gr_{%3}2.line P{%3}_q p{%3}_cap p{%3}_k p{%3}_lf p{%3}_ur
_gr_{%3}2.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Value added) symsize(xs)
_gr_{%3}2.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Capacity) symsize(xs)
_gr_{%3}2.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Capital) symsize(xs)
_gr_{%3}2.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Employment) symsize(xs)
_gr_{%3}2.setelem(5) symbol(10) lcolor(purple) lwidth(0.25) lpat(5) legend(Rate of use)  symsize(xs)
_gr_{%3}2.legend position(botleft)
_gr_{%3}2.options gridl gridauto 
_gr_{%3}2.addtext(t,font=12,x)  Shock on {%title} : The production elements 
_gr_{%3}2.addtext(l,font=11) in percentage
_gr_{%3}2.addtext(r,font=10) shock : {%units} 

print(l) _gr_{%3}2

close _gr_{%3}2


'    Ratios

graph _gr_{%3}3.line P{%3}_wr-P{%3}_pfd p{%3}_k-p{%3}_lf   p{%3}_ur d{%3}_unr
_gr_{%3}3.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Relative cost) symsize(xs)
_gr_{%3}3.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Capital-Labour ratio) symsize(xs)
_gr_{%3}3.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Rate of use) symsize(xs)
_gr_{%3}3.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Unemployment rate in points)  symsize(xs)
_gr_{%3}3.legend position(botleft)
_gr_{%3}3.options gridl gridauto 
_gr_{%3}3.addtext(t,font=12,x)  Shock on {%title} : The ratios 
_gr_{%3}3.addtext(l,font=11) in percentage
_gr_{%3}3.addtext(r,font=10) shock : {%units}  

print(l) _gr_{%3}3

close _gr_{%3}3

'    External trade 1

graph _gr_{%3}4.line p{%3}_xval p{%3}_mval p{%3}_rcval
_gr_{%3}4.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Exports) symsize(xs)
_gr_{%3}4.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Imports) symsize(xs)
_gr_{%3}4.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Exports-Imports ratio)  symsize(xs)
_gr_{%3}4.legend position(botleft)
_gr_{%3}4.options gridl gridauto 
_gr_{%3}4.addtext(t,font=12,x)  Shock on {%title} : External trade at current prices  
_gr_{%3}4.addtext(l,font=11) in percentage
_gr_{%3}4.addtext(r,font=10) shock : {%units}  

' print(l) _gr_{%3}4

close _gr_{%3}4


'    External trade 2

graph _gr_{%3}5.line p{%3}_rcvol p{%3}_ttrad p{%3}_rcval
_gr_{%3}5.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(At constant prices) symsize(xs)
_gr_{%3}5.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Terms of trade) symsize(xs)
_gr_{%3}5.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(At constant prices)  symsize(xs)
_gr_{%3}5.legend position(botleft)
_gr_{%3}5.options gridl gridauto 
_gr_{%3}5.addtext(t,font=12,x)  Shock on {%title} : The exports - import ratios 
_gr_{%3}5.addtext(l,font=11) in percentage
_gr_{%3}5.addtext(r,font=10) shock : {%units}  

print(l) _gr_{%3}5

close _gr_{%3}5

'    Prices

graph _gr_{%3}6.line p{%3}_wr p{%3}_pfd p{%3}_pq p{%3}_px p{%3}_pm
_gr_{%3}6.setelem(1) symbol(1) lcolor(blue) lwidth(0.25) lpat(1) legend(Wage rate) symsize(xs)
_gr_{%3}6.setelem(2) symbol(2) lcolor(red) lwidth(0.25) lpat(2) legend(Consumption) symsize(xs)
_gr_{%3}6.setelem(3) symbol(8)lcolor(green) lwidth(0.25) lpat(3) legend(Value added) symsize(xs)
_gr_{%3}6.setelem(4) symbol(4) lcolor(orange) lwidth(0.25) lpat(4) legend(Exports) symsize(xs)
_gr_{%3}6.setelem(5) symbol(10) lcolor(purple) lwidth(0.25) lpat(5) legend(Imports)  symsize(xs)
_gr_{%3}6.legend position(botleft)
_gr_{%3}6.options gridl gridauto 
_gr_{%3}6.addtext(t,font=12,x)  Shock on {%title} : The prices   
_gr_{%3}6.addtext(l,font=11) in percentage 
_gr_{%3}6.addtext(r,font=10) shock : {%units} 

' print(l) _gr_{%3}6

close _gr_{%3}6

next

save proj_cd_1.wf1


