
'      this program creates an example model


'      We define the directory 
'      this has to be adapted to your specifications

cd "c:\users\jean louis\skydrive\eviews\files_book_test\fra_cd"

'      The output (estimations, tests) will also be directed to a text file
'       Its name is _mod_1.txt

output(t) _mod_1

'       We start from the data file data_1
'       We first save it under the name mod_1
'       But as usal, we must avoid having other versions open at the same time
'       We send close statements as a precaution
'        
close data_1 
close mod_cd_1
open data_1  
wfsave mod_cd_1
pageselect model

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

'   ==============================================

'    We start with the estimations

'   ==============================================

'     We estimate the production factors as a system

smpl 1962Q1 2004Q4
genr lf_ec=0
genr k_ec=0

'    We define the relative cost of labor and capital

genr relc=wr*(1+r_scf)/pi/(ir/100-@pchy(pc)-4*log(1-rdep))

'    We define the coefficients

coef(10) c_cd
vector(10) p_cd 

'      We create the system

if @isobject("cd") then
delete cd
endif

smpl 1962Q1 2003Q4
system cd 
cd.append log(k/q(-1))=-c_cd(3)*(t-2005)-c_cd(7)+c_cd(2)*log(relc)
cd.append log(lf/q(-1))=-c_cd(3)*(t-2005)-c_cd(1)+(c_cd(2)-1)*log(relc)

'    We initialize the coefficients

c_cd(4)=0.1
c_cd(1)=100
c_cd(3)=0.02
c_cd(2)=0.65
p_cd(8)=0.0
p_cd(9)=0.0

smpl  1975Q4 2003Q4

'    We estimate using FIML and SUR

cd.fiml(p)
cd.sur(p)

'   We create the estimated series, as the target values

smpl 1970Q4 2004Q4
genr log(kd/q(-1))=-c_cd(3)*(t-2005)-c_cd(7)+c_cd(2)*log(relc)
genr log(lfd/q(-1))=-c_cd(3)*(t-2005)-c_cd(1)+(c_cd(2)-1)*log(relc)

'   We estimate the actual values as an average of target and past values

'    In this vesrion we calibrate the inertia factor
'    and estimate the trend and constant

smpl  1970Q4 2003Q4
coef(10) c_k
vector(10) p_k
genr k_ec=0 
coef(10) c_lf
vector(10) p_lf
genr lf_ec=0
p_lf(1)=0.20
p_k(1)=1.00
equation _eq_k.ls(p) log(k/q(-1))=p_k(1)*log(kd/q)+(1-p_k(1))*log(k(-1)/q(-1))+c_k(2)+c_k(3)*(t-2005)*(t<=2005)+k_ec
genr k_ec=resid
equation _eq_lf.ls(p) log(lf/q(-1))=p_lf(1)*log(lfd/q)+(1-p_lf(1))*log(lf(-1)/q(-1))+c_lf(2)+c_lf(3)*(t-2005)*(t<=2005)+lf_ec
genr lf_ec=resid

'     We create the productive capacity from the actual values of factors

genr log(cap)=c_cd(7)*(1-c_cd(2))+c_cd(1)*c_cd(2)+c_cd(3)*(t-2005)*(t<=2005)+c_cd(2)*4*log((1+txq)/(1+txn))*(t-2005)*(t>2005)+c_cd(2)*log(lf)+(1-c_cd(2))*log(k(-1))

'     We define the rate of use

genr ur=q/cap

'   We forecast the values 

for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_k.forecast(d) _z
genr k_ec=log(k/q)-_z
_eq_lf.forecast(d) _z
genr lf_ec=log(lf/q)-_z
next



'---  estimating the change in inventories -----------------------------------------------


coef(10) c_ic
smpl 1962Q1 2004Q4
genr ic_ec=0
smpl 1970Q1 2003Q4
equation _eq_ic.ls(p) ic/q(-1)=c_ic(1)*@pchy(q)+c_ic(2)+c_ic(3)*ic(-1)/q(-2)+c_ic(4)*(t-1985)*(t<=1985)+ic_ec
genr ic_ec=resid

for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_ic.forecast(d) _z
genr ic_ec= ic/q(-1)-_z
next

'    ---- estimating the actual work force ------------------------------------------------------


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


smpl 1962Q1 2004Q4
scalar p_cost=0.05
genr cost=(wr*lf*(1+r_scf)+p_cost*pi*k(-1))/q
coef(10) c_pq
genr pq_ec=0
smpl 1962Q1 2003Q4
equation _eq_pq.ls(p) dlog(pq)=c_pq(1)*dlog(cost)+c_pq(2)*dlog(ur)+c_pq(3)*log(pq(-1)/cost(-1))-c_pq(2)*c_pq(3)*log(ur(-1))+c_pq(5)+pq_ec
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


smpl 1962Q1 2003Q4

'    we have to create an intermediary variable

smpl 1972Q1 2003Q4  
genr _luwc=LOG(UWC)-0.5*log(pq)-0.5*log(pc)
uroot(p) log(uwc/pc)
uroot(p) unr
coint(b,p) _luwc unr
coef(10) c_wr
vector(10) p_wr
smpl 1972Q3 2004Q4    	
genr res_wr=LOG(UWC)-0.5*log(pq)-0.5*log(pc)+3.455570*UNR+0.183764
smpl 1962Q1 2004Q4
genr wr_ec=0
p_wr(1)=1	 	 
smpl 1962Q1 2003Q4
equation _eq_wr.ls(p) dlog(wr)=c_wr(1)*@movav(dlog(pc),4)+c_wr(2)*d(unr)+c_wr(3)*res_wr(-1)+c_wr(7)*(t=1978)+c_wr(8)*t+wr_ec
genr wr_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_wr.forecast(d) _z
genr wr_ec= dlog(wr)-_z
next

'    ----------------------------------------------------------


'    estimating the import price

coef(10) c_pm
smpl 1962Q1 2004Q4
genr pm_ec=0
smpl 1962Q1 2003Q4
equation _eq_pm.ls(p) log(pm)=c_pm(4)*log(pp)+(1-c_pm(4))*log(ppx*er)+c_pm(6)+c_pm(5)*(t-2005)*(t<=2005)+[ar(1)=c_pm(7)]+pm_ec
genr pm_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_pm.forecast(d) _z
genr pm_ec= dlog(pm)-_z
next



coef(10) c_px
smpl 1962Q1 2004Q4
genr px_ec=0
smpl 1962Q1 2003Q4
equation _eq_px.ls(p) log(px)=c_px(4)*log(pp)+(1-c_px(4))*log(ppx*er)+c_px(6)+c_px(5)*(t-2005)*(t<=2005)+[ar(1)=c_px(7)]+px_ec
genr px_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_px.forecast(d) _z
genr px_ec= dlog(px)-_z
next

'    ----------------------------------------------------------

'    estimating household consumption

coef(10) c_coh
smpl 1962Q1 2004Q4
genr coh_ec=0
smpl 1962Q1 2003Q4
equation _eq_coh.ls(p) dlog(coh)=c_coh(1)*.25*log(hrdi/hrdi(-4))+c_coh(2)*dlog(unr)+c_coh(3)*log(pc/pc(-4))+c_coh(5)*dlog(coh(-1))+c_coh(6)+c_coh(7)*log(coh(-1)/hrdi(-1))+c_coh(8)*(t-2005)*(t<=2005)+coh_ec
genr coh_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_coh.forecast(d) _z
genr coh_ec= dlog(coh)-_z
next


'    ----------------------------------------------------------

'    estimating imports

coef(10) c_m
smpl 1962Q1 2004Q4
genr m_ec=0
smpl 1962Q1 2003Q4
equation _eq_m.ls(p) dlog(m)=dlog(fd+0.5*x)+c_m(2)*log(ur)+c_m(3)*log(@movav(compm,6))+c_m(4)+c_m(5)*(t-2005)*(t<=2005)+[ar(1)=c_m(6)]+c_m(7)*log(m(-1)/(fd(-1)+0.5*x(-1)))+m_ec
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

coef(10) c_x
smpl 1962Q1 2004Q4
genr x_ec=0
smpl 1962Q1 2003Q4
equation _eq_x.ls(p) dlog(x)=c_x(1)*dlog(wd)+c_x(2)*log(x(-1)/wd(-1))+c_x(3)*0.5*(log(ur)+log(ur(-1)))+c_x(4)*0.5*(log(compx(-1))+log(compx))+c_x(5)+c_x(6)*(t-2005)*(t<=2005)+x_ec
genr x_ec=resid
for !i=1 to 4
smpl 2004Q1 2004Q4 if (t=2004+(!i-1)/4)
_eq_x.forecast(d) _z
genr x_ec= dlog(x)-_z
next

'   ==============================================

'    creating the model

'   ==============================================


'     creating a blank model

if @isobject("_mod_1") then
delete _mod_1
endif

model _mod_1

'      scalars

scalar f
scalar r_vat0=f_r_vat0
scalar r_tar0=f_r_tar0
scalar r_tarx0=f_r_tarx0

''
'     production block
'

smpl 1962Q1 2004Q4

'==============================
'     production block
'==============================

_mod_1.append GDPM=relax_q*(FD+X-M)+(1-relax_q)*GDPM
_mod_1.append Q=GDPM-r_vat0*FD/(1+r_vat0)
_mod_1.append UR=Q/CAP
_mod_1.append log(kd/q(-1))=-c_cd(3)*(t-2005)*(t<=2005)-c_cd(7)+c_cd(2)*log(relc)-c_cd(2)*4*log((1+txq)/(1+txn))*(t-2005)*(t>2005) 
_mod_1.append log(lfd/q(-1))=-c_cd(3)*(t-2005)*(t<=2005)-c_cd(1)+(c_cd(2)-1)*log(relc)-c_cd(2)*4*log((1+txq)/(1+txn))*(t-2005)*(t>2005)   
_mod_1.merge _eq_lf
_mod_1.append PL=Q/LF
_mod_1.merge _eq_ic
_mod_1.append LT=LF+lg
genr id=Kd-K(-1)*(1-rdep)
_mod_1.append id=Kd-K(-1)*(1-rdep)
vector(10) p_id
p_id(1)=0.04
_mod_1.append i/k(-1)=p_id(1)*id/k(-1)+(1-p_id(1))*i(-1)/k(-2)
_mod_1.append k=k(-1)*(1-rdep)+i
_mod_1.append  log(cap)=c_cd(7)*(1-c_cd(2))+c_cd(1)*c_cd(2)+c_cd(3)*(t-2005)*(t<=2005)+c_cd(2)*4*log((1+txq)/(1+txn))*(t-2005)*(t>2005)+c_cd(2)*log(lf)+(1-c_cd(2))*log(k(-1))
_mod_1.merge _eq_popac
_mod_1.append UN=POPAC-LT
_mod_1.append UNR=UN/POPAC

'==============================
'    price block
'==============================

_mod_1.append UWC=WR*(1+r_scf)/PL  
_mod_1.merge _eq_pq
_mod_1.append PP=(PQ+tc*PFDXT)/(1+tc)
_mod_1.append PFD=relax_pfd*(GDPMVAL+MVAL*(1+r_tar)-XVAL)/(GDPM+M-X)+(1-relax_pfd)*PFD
_mod_1.append PFDXT=PFD*(1+r_vat0)/(1+r_vat) 
_mod_1.append PC=r_pc*PFD
_mod_1.append pi=r_pi*PFD
_mod_1.append PIG=r_pig*PFD
_mod_1.append res_wr=LOG(UWC)-0.5*log(pq)-0.5*log(pc)+3.455570*UNR+0.183764
_mod_1.merge _eq_wr
_mod_1.append cost=(wr*lf*(1+r_scf)+p_cost*pi*k(-1))/q
_mod_1.merge _eq_px
_mod_1.merge _eq_pm
scalar p_er=1
_mod_1.append dlog(ER)=(p_er=1)*dlog(erx)+(p_er=2)*dlog(pc)
scalar p_irs=2
_mod_1.append IRS=(p_irs=1)*IRSX+(p_IRS=2)*(IRSR+100*@pchy(PC))+(p_IRS=3)*(150*@pchy(PC)+50*(UR-urd)/urd+irst)
_mod_1.append IRL=@movav(IRS,4)+IRL_ER
genr irl_er=(IRL-@movav(IRS,4))
scalar p_ir=0.5
_mod_1.append IR=p_ir*IRS+(1-p_ir)*IRL+ir_eR
genr ir_er=IR-(p_ir*IRS+(1-p_ir)*IRL)
scalar p_irm=0.8
_mod_1.append IRM=p_irm*IRM(-1)+(1-p_irm)*IR
_mod_1.append relc=wr*(1+r_scf)/pi/(ir/100-@pchy(pc)-4*log(1-rdep))

'==============================
'   households block
'==============================

_mod_1.append REVQ=r_revq*QVAL
_mod_1.append REVX=r_revx*PFD
_mod_1.append SOCB=socbr*PC*popt
_mod_1.append WG=WR*lg
_mod_1.append W=WF+WG
_mod_1.append SCW=r_scw*W
_mod_1.append HI=W-SCW+REVQ+REVX+SOCB
_mod_1.append ICT=r_ict*HI(-1)
_mod_1.append HDI=HI-ICT
_mod_1.append HRDI=HDI/PC
_mod_1.merge _eq_coh

'==============================
'    firms block
'==============================

_mod_1.append QVAL=PQ*Q
_mod_1.append GDPMVAL=QVAL+VAT
_mod_1.append pgdpm=GDPMVAL/GDPM
_mod_1.append WF=WR*LF
_mod_1.append SUBS=r_subs*QVAL
_mod_1.append MARG=PQ*Q*(1+r_subs-r_oit)-WR*LF*(1+r_scf)
_mod_1.append RMARG=MARG/QVAL
_mod_1.append IFP=(PROF(-1)+IFP(-1))*r_ifp
_mod_1.append NIF=NIF(-1)*IRM/IRM(-1)-IR/100*FCAPF+nif_er*qval
_mod_1.append PROF=MARG-REVQ-IFP-NIF
_mod_1.append RPROF=PROF/(PFD*K(-1))
_mod_1.append RPROB=MARG/(PFD*K(-1))
_mod_1.append FCAPF=PROF-PI*I-PFD*IC

'==============================
'     external trade block
'==============================

_mod_1.append PMT=PM*(1+r_tar)/(1+r_tar0)
_mod_1.append COMPM=PMT/PP
_mod_1.append FD=COH+I+IC+cog+IG+fdxr*Q
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
_mod_1.append NIXL=NIXL(-1)*IRM/IRM(-1)-IR/100*(p_nix*TRB-nixl)
_mod_1.append NIXX=NIXX(-1)*IRMX/IRMX(-1)*(er/er(-1))-IRX/100*((1-p_nix)*TRB-nixx)
_mod_1.append NIX=nixL+NIXX
_mod_1.append FCAPX=TRB-NIX
'
'==============================
'     State budget block
'==============================
'
_mod_1.append VAT=r_vat*PFD*FD/(1+r_vat)
_mod_1.append SCF=r_scf*Wf
_mod_1.append OIT=r_oit*QVAL
_mod_1.append TAR=r_tar*MVAL
_mod_1.append SCG=R_SCG*WG
_mod_1.append REVG=SCF+SCG+SCW+OIT+IFP+ICT+VAT+TAR+r_revg*QVAL
_mod_1.append IGV=IG*PIG
_mod_1.append COGV=COG*PFD
_mod_1.append FDGV=COGV+IGV
_mod_1.append NIG=NIG(-1)*IRM/IRM(-1)-IR/100*FCAPG+nig_er*qval
_mod_1.append EXPG=FDGV+WG+SUBS+SOCB+NIG+SCG+r_expg*QVAL
_mod_1.append FCAPG=REVG-EXPG
_mod_1.append FCAPGP=100*FCAPG/QVAL

'==============================
'     We check the residuals
'==============================

_mod_1.append assign @all _c

smpl 1995Q1 2004Q4

'    We create the groups

_mod_1.makegroup(a,n) _g_vendo @endog
_mod_1.makegroup(a,n) _g_vexog @exog
_mod_1.makegroup(a,n) _g_viden @endog
group _g_vbeha
group _g_viden

'     At first the groups for identities and behavioral equations are both empty

'     We test for the existennce of a residual variable

'     If yes the variable is added to the behavioral
'     If no to the identities

for !i=1 to _g_vendo.@count
%1=_g_vendo.@seriesname(!i)
scalar  {%1}_is=@isobject(%1+"_ec")
if  {%1}_is=1 then
_g_vbeha.add {%1}
else
_g_viden.add {%1}
endif
next

'    We solve the model using the "fit" option
'    and applying the suffit "_c"

smpl 1990Q1 2004Q4
_mod_1.append assign @all _c
_mod_1.solve(d=f)

'    We compute the differences both in absolute and relative terms

for !i=1 to _g_vendo.@count
%1=_g_vendo.@seriesname(!i)
genr dc_{%1}={%1}-{%1}_c
genr pc_{%1}=100*dc_{%1}/({%1}+({%1}=0))
next
smpl @all
save mod_cd_1


