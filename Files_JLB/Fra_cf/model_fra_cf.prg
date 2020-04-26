
'      this program creates an example model


'      We define the directory 
'      this has to be adapted to your specifications

cd "c:\users\jean louis\skydrive\eviews\book_files\fra_cf"

'      The output (estimations, tests) will also be directed to a text file
'       Its name is _mod_1.txt

output(r) _mod_1

'       We start from the data file data_1
'       We first save it under the name mod_1
'       But as usual, we must avoid having other versions open at the same time
'       We send close statements as a precaution
'        
close data_1 
close mod_1 
open data_1  
wfsave mod_1

'       We set the sample to maximum
'        The workfile dimension

smpl 1960Q1 2005Q4

'       We eliminate the f_ prefix
'       We do not need this identifier
'       As we are working with a single country

group g_f f_*
for !i=1 to g_f.@count
%1=g_f.@seriesname(!i)
%2=@mid(%1,3)
series {%2}={%1}
next

'      The forecasts will use long term growth rates
'      We shall define them now
'      But they will only apply beyond the last period
'      for which data is available
'      So they will affect neither estimations nor computations

'      txq : growth rates of quantities at constant prices (at country level)
'      txp : growth rates of populations (numbers of persons)
'      txp : growth rates of deflators

'      Actually by using the following growth rates
'      the growth rates will not take round values
'      but the Dlog of the variables will
'      including the results of all multiplications and divisions

scalar txq=exp(0.005)-1
scalar txn=exp(0.002)-1
scalar txp=exp(0.006)-1

'     We delete the "f" scalar used earlier

if @isobject("f") then
delete f
endif

'   ==============================================

'    We start with the estimations

'   ==============================================

'   ----- estimating investment -----------------------------------------------

'      We are going to give extended explanations for the first case
'      Later the logic is mostly the same
'      We shall only present the new elements

'      This creates an artificial error
'       which helps locate true errors
'       in case the message is not clear enough

show err_k

'      We create a vector of coefficients
'      Its name is built by attaching a constant string
'      to the name of the associated variable
'      So knowing one we know the other

coef(10) c_k

'     We start with the full period

smpl 1962Q1 2004Q4

'      We want to use the same equation item at estimation and simulation time
'      at which we want to manipulate a residual 
'      with a name built from the variable and the suffix "_ec"
'      The simple solution is to set the initial residual to zero
'      Obviously, the estimation is performed as if there was no residual

genr k_ec=0

'     We restrict the sample to the historical period
     
smpl 1962Q1 2003Q4

'     We define the estimated equation with :

'      A name associated with the estimated variable, adding "_eq_"  first
'      the underscore puts the equation item at the top of the workfile
'      making it easier to locate it

'      An immediate estimation using least squares through ".lp"
'      with the output transferred to a text file through "(p)"

'      The left hand side can be an expression, with the first element as the dependent variable
'       
'      The time trend stops in 2005
'       Giving it a zero value in 2005 is the easiest way to do it
'       and this does not change the explanation, the constant will adapt
 
equation _eq_k.ls(p) dlog(k)=c_k(1)*dlog(k(-1))+0.50*c_k(2)*(q-q(-4))/q(-4)-c_k(2)*(urd-ur)/ur+c_k(4)*@movav(rprob,3)+c_k(5)*((t-2005)*(t<=2005))+c_k(6)   +k_ec ' -0.0003*(irl-100*@pchy(pcoh))

'       We transfer the residuals in graphic form to the output file
 
_eq_k.resids(p)
close _eq_k

'       Now we put the residual to the estimated value, contained in the standard variable RESID

genr k_ec=resid

'      We have now to compute the residual out of the sample
'       we use the forecast statement
'       but it has do be applied for separate periods
'       This is controlled by our time trend t

'      (d) forecasts the left hand expression and not the dependent variable

'       There are other ways to do it

for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_k.forecast(d) _z
genr k_ec= dlog(k)-_z
next

'---  estimating the change in inventories -----------------------------------------------

show err_ci
coef(10) c_ci
smpl 1962Q1 2004Q4
genr ci_ec=0
smpl 1970Q1 2003Q4
equation _eq_ci.ls(p) ci/q(-1)=c_ci(1)*@pchy(q)+c_ci(2)+c_ci(3)*ci(-1)/q(-2)+c_ci(4)*(t-1985)*(t<=1985)+ci_ec
_eq_ci.resids(p)
close _eq_ci
genr ci_ec=resid

for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_ci.forecast(d) _z
genr ci_ec= ci/q(-1)-_z
next


'  ---- estimating the trend in labour productivity --------------------------------------

smpl 1962Q1 2003Q4
show err_plt
coef(10) c_plt
equation _eq_plt.ls(p) LOG(Q/LF) = C_plt(1) + (t-2005)*(C_plt(2)*(t<=2005)+4*log((1+txq)/(1+txn))*(t>2005)) + C_plt(3)*((T<=1975)*(T-1975)) + C_plt(4)*((T<=1992)*(T-1992))
_eq_plt.resids(p)
close _eq_plt
smpl 1962Q1 2004Q4
genr log(plt) = C_plt(1) + (t-2005)*(C_plt(2)*(t<=2005)+4*log((1+txq)/(1+txn))*(t>2005)) + C_plt(3)*((T<=1975)*(T-1975)) + C_plt(4)*((T<=1992)*(T-1992))

genr lfd=q/plt

'  ------ estimating employment -------------------------

show err_lf

coef(10) c_lf
smpl 1962Q1 2004Q4
genr lf_ec=0
smpl 1962Q1 2003Q4
equation _eq_lf.ls(p) dlog(lf)=c_lf(1)*dlog(lfd)+c_lf(2)*log(lfd(-1)/lf(-1))+c_lf(3)*(t=1968.25)+c_lf(4)*(t=1968.50)+c_lf(5)*(t=1968)+lf_ec
_eq_lf.resids(p)
close _eq_lf
genr lf_ec=resid

for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_lf.forecast(d) _z
genr lf_ec= dlog(lf)-_z
next

'    ---- estimating the actual work force ------------------------------------------------------

show err_popac
coef(10) c_popac
vector(10) p_popac

'      Special case : we want to set the value of a coefficient and estimate the rest

'      We create a normal vector in the same way as coefficients
'       with "p" instead of "c"

'      We just have to replace "c" by "p" in the equation
'      and give our value to the corresponding "p" element

'      This makes it easy to switch the definition

'      Here this applies to the third coefficient
'      with the value 0.6
  
p_popac(3)=0.80
smpl 1962Q1 2004Q4
genr popac_ec=0
smpl 1962Q1 2003Q4
equation _eq_popac.ls(p) d(popac)/pop65(-1)=c_popac(1)*d(lt)/pop65(-1)+c_popac(2)*d(pop65)/pop65(-1)-p_popac(3)*(popac(-1)/pop65(-1)-c_popac(4)*lt(-1)/pop65(-1)-c_popac(5))+[ar(1)=c_popac(6)]+popac_ec
_eq_popac.resids(p)
close _eq_popac

'    Managing the residuals is also special
'    as the equation uses an AR process

'    We start by storing the RESID variable

genr resid_popac=resid

'    Then we "forecast" the residual for the first period in the sample
'     for which resid is available and was not for the previous one

'     (s) eliminates the AR part from the forecast

smpl 1962Q1 2003Q4 if (resid<>na) and (resid(-1)=na)
_eq_popac.forecast(d,s) _z
genr popac_ec=d(popac)/pop65(-1)-_z

'     Now we compute the residual for all periods but the first one
'     from the "official" residual and the memory of the previous computed one
'     applying the AR coefficient

smpl 1962Q1 2003Q4 if (resid_popac<>na) and (resid_popac(-1)<>na)
genr popac_ec=resid_popac+popac_ec(-1)*c_popac(6)

'      For the out-of-sample periods we apply the usual formula

for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_popac.forecast(d) _z
genr popac_ec= d(popac)/pop65(-1)-_z
next

'   There might be a simpler method but this one works
'    and it is easy to reproduce

'    ----------------------------------------------------------

'    estimating the GDP deflator

show err_pq
smpl 1962Q1 2004Q4
coef(10) c_pq
genr pq_ec=0
smpl 1962Q1 2003Q4
equation _eq_pq.ls(p) dlog(pq)=c_pq(1)*dlog(uwc)+c_pq(2)*dlog(ur)+c_pq(3)*log(pq(-1)/uwc(-1))-c_pq(2)*c_pq(3)*log(ur(-1))+c_pq(5)+pq_ec
_eq_pq.resids(p)
close _eq_pq
genr pq_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_pq.forecast(d) _z
genr pq_ec= dlog(pq)-_z
next

'    ----- estimating the wage rate -----------------------------------------------------

'      This time we apply cointegration
'      we would like to use this technique more often
'      but we did not find another working case

'      So we have only one example

show err_wr

'    we have to create an intermediary variable

smpl 1972Q1 2003Q4

'    We create a vector of parameters for the long term equation
'     This will allow us to change the values in forecasts
'     And to modify the indexation coefficient
  
vector(10) p_res_wr

'      We give equal shares to value added and consumption deflators

p_res_wr(1)=0.5

'       We have to create an intermediary variable 
'       as EViews does not accept log expressions in the test  (a bug)

genr _luwc=LOG(UWC)-p_res_wr(1)*log(pq)-(1-p_res_wr(1))*log(pcoh)

'       We test stationarity of the two elements

uroot(p) _luwc
uroot(p) unr

'       If fails, we test for cointegration

coint(b,p) _luwc unr

'       It works, we repeat the test through a var
'       the only way to get the values of the cointegrating coefficients

var _var_wr.ec(b,p) 1 2 _luwc unr
vector(10) p_res_wr
p_res_wr(2)=_var_wr.b(1,1)
p_res_wr(3)=_var_wr.b(1,2)
p_res_wr(4)=_var_wr.b(1,3) 

'       And we compute the error

smpl 1972Q3 2004Q4    	
genr res_wr=p_res_wr(2)*(LOG(UWC)-p_res_wr(1)*log(pq)-(1-p_res_wr(1))*log(pcoh))+p_res_wr(3)*UNR+p_res_wr(4)

'       Now we estimate the dynamic equation using the lagged error 
'       The technique is the same as usual

coef(10) c_wr
vector(10) p_wr
smpl 1962Q1 2004Q4
genr wr_ec=0
p_wr(1)=1	 	 
smpl 1962Q1 2003Q4
equation _eq_wr.ls(p) dlog(wr)=p_wr(1)*@movav(dlog(pcoh),4)+c_wr(2)*d(unr)+c_wr(3)*res_wr(-1)+c_wr(7)*(t=1978)+c_wr(8)*t+wr_ec
_eq_wr.resids(p)
close _eq_wr
genr wr_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_wr.forecast(d) _z
genr wr_ec= dlog(wr)-_z
next

'    ----- estimating the  import price -----------------------------------------------------

coef(10) c_pm
smpl 1962Q1 2004Q4
genr pm_ec=0
smpl 1962Q1 2003Q4
equation _eq_pm.ls(p) log(pm)=c_pm(4)*log(pp)+(1-c_pm(4))*log(ppx*er)+c_pm(6)+c_pm(5)*(t-2005)*(t<=2005)+[ar(1)=c_pm(7)]+pm_ec
_eq_pm.resids(p)
close _eq_pm
genr pm_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_pm.forecast(d) _z
genr pm_ec= log(pm)-_z
next

'    ----- estimating the  export price -----------------------------------------------------

show err_px
coef(10) c_px
smpl 1962Q1 2004Q4
genr px_ec=0
smpl 1962Q1 2003Q4
equation _eq_px.ls(p) log(px)=c_px(4)*log(pp)+(1-c_px(4))*log(ppx*er)+c_px(6)+c_px(5)*(t-2005)*(t<=2005)+[ar(1)=c_px(7)]+px_ec
_eq_px.resids(p)
close _eq_px
genr px_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_px.forecast(d) _z
genr px_ec= log(px)-_z
next

'    ----- estimating household consumption -----------------------------------------------------

show err_coh
coef(10) c_coh
smpl 1962Q1 2004Q4
genr coh_ec=0
smpl 1962Q1 2003Q4
equation _eq_coh.ls(p) dlog(coh)=c_coh(1)*.25*log(hrdi/hrdi(-4))+c_coh(2)*dlog(unr)+c_coh(3)*log(pcoh/pcoh(-4))+c_coh(5)*dlog(coh(-1))+c_coh(6)+c_coh(7)*log(coh(-1)/hrdi(-1))+c_coh(8)*(t-2005)*(t<=2005)+coh_ec
_eq_coh.resids(p)
close _eq_coh
genr coh_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_coh.forecast(d) _z
genr coh_ec= dlog(coh)-_z
next

'    ----- estimating imports -----------------------------------------------------

show err_m
coef(10) c_m
smpl 1962Q1 2004Q4
genr m_ec=0
smpl 1962Q1 2003Q4
equation _eq_m.ls(p) dlog(m)=dlog(fd+tc*q)+c_m(2)*log(ur)+c_m(3)*log(@movav(compm,6))+c_m(4)+c_m(5)*(t-2005)*(t<=2005)+[ar(1)=c_m(6)]+c_m(7)*log(m(-1)/(fd(-1)+tc(-1)*q(-1)))+m_ec
_eq_m.resids(p)
close _eq_m
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

'    ----- estimating  exports -----------------------------------------------------

show err_x
coef(10) c_x
smpl 1962Q1 2004Q4
genr x_ec=0
smpl 1962Q1 2003Q4
equation _eq_x.ls(p) dlog(x)=c_x(1)*dlog(wd)+c_x(2)*log(x(-1)/wd(-1))+c_x(3)*0.5*(log(ur)+log(ur(-1)))+c_x(4)*0.5*(log(compx(-1))+log(compx))+c_x(5)+c_x(6)*(t-2005)*(t<=2005)+x_ec
_eq_x.resids(p)
close _eq_x
genr x_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_x.forecast(d) _z
genr x_ec= dlog(x)-_z
next

'   ==============================================

'    Now we create the model

'   ==============================================


'     creating a blank model

if @isobject("_mod_1") then
delete _mod_1
endif
model _mod_1

'      scalars

scalar r_vat0=f_r_vat0
scalar r_tar0=f_r_tar0
scalar r_tarx0=f_r_tarx0
smpl 1962Q1 2004Q4

'==============================
'     The supply block
'==============================

_mod_1.append GDPM=relax_q*(FD+X-M)+(1-relax_q)*GDPM
_mod_1.append Q=GDPM-r_vat0*FD/(1+r_vat0)
_mod_1.append UR=Q/CAP
_mod_1.merge _eq_k
_mod_1.merge _eq_ci
_mod_1.append log(plt) = C_plt(1) + (t-2005)*(C_plt(2)*(t<=2005)+4*log((1+txq)/(1+txn))*(t>2005)) + C_plt(3)*((T<=1975)*(T-1975)) + C_plt(4)*((T<=1992)*(T-1992))
_mod_1.append LFD=Q/plt
_mod_1.merge _eq_lf
_mod_1.append PL=Q/LF
_mod_1.append LT=LF+lg
_mod_1.append I=K-K(-1)*(1-rdep)
_mod_1.append CAP=pk*K(-1)
_mod_1.merge _eq_popac
_mod_1.append UN=POPAC-LT
_mod_1.append UNR=UN/POPAC

'==============================
'     The price block
'==============================

_mod_1.append UWC=WR*(1+r_scf)/PL  
_mod_1.merge _eq_pq
_mod_1.append PP=(PQ+tc*PFDXT)/(1+tc)
_mod_1.append PFD=relax_pfd*(GDPMVAL+MVAL-XVAL)/(GDPM+M-X)+(1-relax_pfd)*PFD
_mod_1.append PFDXT=PFD*(1+r_vat0)/(1+r_vat) 
_mod_1.append PCoh=r_pcoh*PFD
_mod_1.append PCog=r_pcog*PFD
_mod_1.append pi=r_pi*PFD
_mod_1.append PIG=r_pig*PFD
_mod_1.append res_wr=p_res_wr(2)*(LOG(UWC)-p_res_wr(1)*log(pq)-(1-p_res_wr(1))*log(pcoh))+p_res_wr(3)*UNR+p_res_wr(4)
_mod_1.merge _eq_wr
_mod_1.merge _eq_px
_mod_1.merge _eq_pm
scalar p_er=1
_mod_1.append dlog(ER)=(p_er=1)*dlog(erx)+(p_er=2)*dlog(pcoh)
scalar p_irs=3
_mod_1.append IRS=(p_irs=1)*IRSX+(p_IRS=2)*(IRSR+100*@pchy(PCoh))+(p_IRS=3)*(150*@pchy(PCoh)+50*(UR-urd)/urd+irst)
_mod_1.append IRL=@movav(IRS,4)+IRL_ER
genr irl_er=(IRL-@movav(IRS,4))
scalar p_ir=0.5
_mod_1.append IR=p_ir*IRS+(1-p_ir)*IRL+ir_ER
genr ir_er=IR-(p_ir*IRS+(1-p_ir)*IRL)
scalar p_irm=0.8
_mod_1.append IRM=p_irm*IRM(-1)+(1-p_irm)*IR+IRM_ER

'===================================
'   The households block
'===================================

_mod_1.append WF=WR*LF
_mod_1.append WG=WR*lg
_mod_1.append W=WF+WG
_mod_1.append SOCB=socbr*PCoh*pop
_mod_1.append REVQ=r_revq*QVAL
_mod_1.append REVX=r_revx*PFD
_mod_1.append SCW=r_scw*W
_mod_1.append HI=W-SCW+REVQ+REVX+SOCB
_mod_1.append ICT=r_ict*HI(-1)
_mod_1.append HDI=HI-ICT
_mod_1.append HRDI=HDI/PCoh
_mod_1.merge _eq_coh

'===================================
'   The firms block
'===================================

_mod_1.append QVAL=PQ*Q
_mod_1.append VAT=r_vat*PFD*FD/(1+r_vat)
_mod_1.append GDPMVAL=QVAL+VAT
_mod_1.append pgdpm=GDPMVAL/GDPM
_mod_1.append SUBS=r_subs*QVAL
_mod_1.append MARG=PQ*Q*(1+r_subs-r_oit)-WR*LF*(1+r_scf)
_mod_1.append RMARG=MARG/QVAL
_mod_1.append IFP=(PROF(-1)+IFP(-1))*r_ifp
_mod_1.append NIF=NIF(-1)*IRM/IRM(-1)-IR/400*FCAPF 
_mod_1.append PROF=MARG-REVQ-IFP-NIF+prof_er*qval
_mod_1.append RPROF=PROF/(PFD*K(-1))
_mod_1.append RPROB=MARG/(PFDXT*K(-1))
_mod_1.append FCAPF=PROF-PI*I*(1+r_vat0)/(1+r_vat)-PFD*ci+qval*fcapf_er

'===================================
'   The external trade block
'===================================

_mod_1.append PMT=PM*(1+r_tar)/(1+r_tar0)
_mod_1.append COMPM=PMT/PP
_mod_1.append FD=COH+I+CI+HIH+COG+IG 
_mod_1.merge _eq_m
_mod_1.append COMPX=PX*(1+r_tarx)/(1+r_tarx0)/(PPX*ER)
_mod_1.merge _eq_x
_mod_1.append MVAL=PM*M
_mod_1.append XVAL=PX*X
_mod_1.append RCVAL=XVAL/MVAL
_mod_1.append RCVOL=X/M
_mod_1.append TTRAD=PX/PM
_mod_1.append TRB=XVAL-MVAL
scalar p_nix=0.5
_mod_1.append NIXL=NIXL(-1)*IRM/IRM(-1)-IR/400*(p_nix*TRB-nixl)
_mod_1.append NIXX=NIXX(-1)*IRMX/IRMX(-1)*(er/er(-1))-IRX/400*((1-p_nix)*TRB-nixx)
_mod_1.append NIX=nixL+NIXX
_mod_1.append FCAPX=TRB-NIX

'===================================
'   The State budget block
'===================================

_mod_1.append SCF=r_scf*Wf
_mod_1.append OIT=r_oit*QVAL
_mod_1.append TAR=r_tar*MVAL
_mod_1.append SCG=R_SCG*WG
_mod_1.append REVG=SCF+SCG+SCW+OIT+IFP+ICT+VAT+TAR+r_revg*QVAL
_mod_1.append IGV=IG*PIG
_mod_1.append COGV=COG*PCOG
_mod_1.append FDGV=COGV+IGV
_mod_1.append NIG=NIG(-1)*IRM/IRM(-1)-IR/400*FCAPG+nig_er*qval
_mod_1.append EXPG=FDGV+WG+SUBS+SOCB+NIG+SCG+r_expg*QVAL
_mod_1.append FCAPG=REVG-EXPG
_mod_1.append FCAPGP=100*FCAPG/GDPMVAL
_mod_1.append GDPVAL=GDPMVAL+WG+SCG

'====================================
'    End of model definition
'====================================

'     We check the residuals

_mod_1.append assign @all _c

smpl 1995Q1 2004Q4

'    We create the groups
'     separating endogenous into behavorial and identity

_mod_1.makegroup(a,n) _g_vendo @endog
_mod_1.makegroup(a,n) _g_vexog @exog
_mod_1.makegroup(a,n) _g_viden @endog
group _g_vbeha

for !i=1 to _g_vendo.@count
%1=_g_vendo.@seriesname(!i)
scalar  {%1}_is=@isobject(%1+"_ec")
if  {%1}_is=1 then
_g_vbeha.add {%1}
_g_viden.drop {%1}
endif
next

smpl 2000Q1 2002Q4
_mod_1.append assign @all _c
_mod_1.solve(d=f)

for !i=1 to _g_vendo.@count
%1=_g_vendo.@seriesname(!i)
genr dd_{%1}={%1}-{%1}_c
genr pd_{%1}=100*dd_{%1}/({%1}+({%1}=0))
next



'      We perform a manual residual check 
'      due to a bug in EViews

'      The equations are the same as above

'      The identities are reproduced
'      (be careful if they compute an expression)

'      For the behavioral, the "merge" statement is replaced by "forecast"

genr h_GDPM=relax_q*(FD+X-M)+(1-relax_q)*GDPM
genr h_Q=GDPM-r_vat0*FD/(1+r_vat0)
genr h_UR=Q/CAP
_eq_k.forecast h_k
_eq_ci.forecast h_ci
genr log(h_plt) = C_plt(1) + (t-2005)*(C_plt(2)*(t<=2005)+4*log((1+txq)/(1+txn))*(t>2005)) + C_plt(3)*((T<=1975)*(T-1975)) + C_plt(4)*((T<=1992)*(T-1992))
genr h_LFD=Q/plt
_eq_lf.forecast h_lf
genr h_PL=Q/LF
genr h_LT=LF+lg
genr h_I=K-K(-1)*(1-rdep)
genr h_CAP=pk*K(-1)
_eq_popac.forecast h_popac
genr h_UN=POPAC-LT
genr h_UNR=UN/POPAC
genr h_UWC=WR*(1+r_scf)/PL  
_eq_pq.forecast h_pq
genr h_PP=(PQ+tc*PFDXT)/(1+tc)
genr h_PFD=relax_pfd*(GDPMVAL+MVAL-XVAL)/(GDPM+M-X)+(1-relax_pfd)*PFD
genr h_PFDXT=PFD*(1+r_vat0)/(1+r_vat) 
genr h_PCoh=r_pcoh*PFD
genr h_PCog=r_pcog*PFD
genr h_pi=r_pi*PFD
genr h_PIG=r_pig*PFD
genr h_res_wr=p_res_wr(2)*(LOG(UWC)-p_res_wr(1)*log(pq)-(1-p_res_wr(1))*log(pcoh))+p_res_wr(3)*UNR+p_res_wr(4)
_eq_wr.forecast h_wr
_eq_px.forecast h_px
_eq_pm.forecast h_pm
scalar p_er=1
genr log(h_ER)=log(er(-1))+(p_er=1)*dlog(erx)+(p_er=2)*dlog(pcoh)
scalar p_irs=3
genr h_IRS=(p_irs=1)*IRSX+(p_IRS=2)*(IRSR+100*@pchy(PCoh))+(p_IRS=3)*(150*@pchy(PCoh)+50*(UR-urd)/urd+irst)
genr h_IRL=@movav(IRS,4)+IRL_ER
genr irl_er=(IRL-@movav(IRS,4))
scalar p_ir=0.5
genr h_IR=p_ir*IRS+(1-p_ir)*IRL+ir_eR
genr ir_er=IR-(p_ir*IRS+(1-p_ir)*IRL)
scalar p_irm=0.8
genr h_IRM=p_irm*IRM(-1)+(1-p_irm)*IR+irm_er
genr h_REVQ=r_revq*QVAL
genr h_REVX=r_revx*PFD
genr h_SOCB=socbr*PCoh*pop
genr h_WG=WR*lg
genr h_W=WF+WG
genr h_SCW=r_scw*W
genr h_HI=W-SCW+REVQ+REVX+SOCB
genr h_ICT=r_ict*HI(-1)
genr h_HDI=HI-ICT
genr h_HRDI=HDI/PCoh
_eq_coh.forecast h_coh
genr h_QVAL=PQ*Q
genr h_GDPMVAL=QVAL+VAT
genr h_pgdpm=GDPMVAL/GDPM
genr h_WF=WR*LF
genr h_SUBS=r_subs*QVAL
genr h_MARG=PQ*Q*(1+r_subs-r_oit)-WR*LF*(1+r_scf)
genr h_RMARG=MARG/QVAL
genr h_IFP=(PROF(-1)+IFP(-1))*r_ifp
genr h_NIF=NIF(-1)*IRM/IRM(-1)-IR/400*FCAPF
genr h_PROF=MARG-REVQ-IFP-NIF+prof_er*qval
genr h_RPROF=PROF/(PFD*K(-1))
genr h_RPROB=MARG/(PFD*K(-1))
genr h_FCAPF=PROF-PI*I-PFD*ci+qval*fcapf_er
genr h_PMT=PM*(1+r_tar)/(1+r_tar0)
genr h_COMPM=PMT/PP
genr h_FD=COH+I+CI+HIH+COG+IG 
_eq_m.forecast h_m
genr h_COMPX=PX*(1+r_tarx)/(1+r_tarx0)/(PPX*ER)
_eq_x.forecast h_x
genr h_MVAL=PM*M
genr h_XVAL=PX*X
genr h_RCVAL=XVAL/MVAL
genr h_RCVOL=X/M
genr h_TTRAD=PX/PM
genr h_TRB=XVAL-MVAL
scalar p_nix=0.5
genr h_NIXL=NIXL(-1)*IRM/IRM(-1)-IR/400*(p_nix*TRB-nixl)
genr h_NIXX=NIXX(-1)*IRMX/IRMX(-1)*(er/er(-1))-IRX/400*((1-p_nix)*TRB-nixx)
genr h_NIX=nixL+NIXX
genr h_FCAPX=TRB-NIX
genr h_VAT=r_vat*PFD*FD/(1+r_vat)
genr h_SCF=r_scf*Wf
genr h_OIT=r_oit*QVAL
genr h_TAR=r_tar*MVAL
genr h_SCG=R_SCG*WG
genr h_REVG=SCF+SCG+SCW+OIT+IFP+ICT+VAT+TAR+r_revg*QVAL
genr h_IGV=IG*PIG
genr h_COGV=COG*PCOG
genr h_FDGV=COGV+IGV
genr h_NIG=NIG(-1)*IRM/IRM(-1)-IR/400*FCAPG+nig_er*qval
genr h_EXPG=FDGV+WG+SUBS+SOCB+NIG+SCG+r_expg*QVAL
genr h_FCAPG=REVG-EXPG
genr h_FCAPGP=100*FCAPG/QVAL
genr h_GDPVAL=GDPMVAL+WG+SCG
for !i=1 to _g_vendo.@count
%1=_g_vendo.@seriesname(!i)
genr dc_{%1}={%1}-h_{%1}
genr pc_{%1}=100*dc_{%1}/({%1}+({%1}=0))
next

'     We save the file

save mod_1


