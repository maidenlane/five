'   

'     This program associates definitions with the series in the model

'       r  for remarks (actually the formula for creating the series)
'       d  for definition
'       u for units

CAP.label(c)  
CAP.label(r)   cap=q/ur
CAP.label(d)  Productive capacity
CAP.label(u)  Euros of 1995
CI.label(c)  
CI.label(r)   ci=fra_iskv
CI.label(d)  Intermediate consumption
CI.label(u)  Euros of 1995
COG.label(c)  
COG.label(r)   cg=(fra_cg-fra_cgw) /pcog
COG.label(d)  Government consumption (real) 
COG.label(u)  Euros of 1995
COGV.label(c)  
COGV.label(r)   cgv=fra_cg-fra_cgw
COGV.label(d)  Government consumption (current) 
COGV.label(u)  Euros
COH.label(c)  
COH.label(r)   coh=fra_cpv
COH.label(d)  Household consumption
COH.label(u)  Euros of 1995
COST.label(c)  
COST.label(r)   cost=(wr*lf*(1+r_scf)+p_cost*pi*k(-1))/q
COST.label(d)  Global production cost
COST.label(u)  Euros 
COMPM.label(c)  
COMPM.label(r)   compm=pmt/pp
COMPM.label(d)  Imports competitiveness
COMPM.label(u)  Ratio
COMPX.label(c)  
COMPX.label(r)  compx=px*(1+r_tarx) /(1+r_tarx0) /(ppx*er) 
COMPX.label(d)  Exports competitiveness
COMPX.label(u)  Ratio
ER.label(c)  
ER.label(r)  er=1/(fra_excheb/@elem(fra_excheb,"1995") ) 
ER.label(d)  Exchange rate
ER.label(u)  Deflator, base 1995
ERX.label(c)  
ERX.label(r)   erx=er
ERX.label(d)  Exchange rate (exogenous) 
ERX.label(u)  Deflator, base 1995
EXPG.label(c)  
EXPG.label(r)   expg=revg-fcapg
EXPG.label(d)  Government expenditures
EXPG.label(u)  Euros
FCAPF.label(c)  
FCAPF.label(r)   fcapf=-fra_nlb
FCAPF.label(d)  Firms financing capacity
FCAPF.label(u)  Euros
FCAPG.label(c)  
FCAPG.label(r)   fcapg=-fra_nlg
FCAPG.label(d)  Government Financing capacity
FCAPG.label(u)  Euros
FCAPGP.label(c)  
FCAPGP.label(r)   fcapgp=100*fcapg/gdpmval
FCAPGP.label(d)  Government Financing capacity (GDP points) 
FCAPGP.label(u)  Ratio
FCAPX.label(c)  
FCAPX.label(r)   fcapx=fra_fbgsv  -- Modified: 1990Q2 2004Q4 
FCAPX.label(d)  Rest of the world Financing capacity 
FCAPX.label(u)  Euros
FD.label(c)  
FD.label(r)   fd=fra_tddv-fra_cgw/pcog
FD.label(d)  Final domestic demand
FD.label(u)  
FDG.label(c)  
FDG.label(r)   fdg=cog+ig
FDG.label(d)  Government demand
FDG.label(u)  Euros of 1995
FDGV.label(c)  
FDGV.label(r)   fdgv=cogv+igv
FDGV.label(d)  Government demand
FDGV.label(u)  Euros
FDXR.label(c)  
FDXR.label(r)   fdxr=(fd-coh-i-hih-ic-fdg) /q
FDXR.label(d)  Residual demand
FDXR.label(u)  Ratio
GDP.label(c)  
GDP.label(r)   gdp=fra_gdpv
GDP.label(d)  Gross Domestic Product
GDP.label(u)  Euros of 1995
GDPM.label(c)  
GDPM.label(r)   gdpm=fra_gdpv-fra_cgw/pcog
GDPM.label(d)  Gross Domestic Product Market 
GDPM.label(u)  Euros of 1995
GDPMVAL.label(c)  
GDPMVAL.label(r)   gdpmval=fra_gdp-fra_cgw
GDPMVAL.label(d)  Gross Domestic Product Market 
GDPMVAL.label(u)  Euros
GDPVAL.label(c)  
GDPVAL.label(r)   gdpval=fra_gdp
GDPVAL.label(d)  Gross Domestic Product
GDPVAL.label(u)  Euros
HDI.label(c)  
HDI.label(r)   hdi=hi-fra_tyh
HDI.label(d)  Household disposble income
HDI.label(u)  Euros
HI.label(c)  
HI.label(r)   hi =fra_yrh-fra_trph
HI.label(d)  Household Income
HI.label(u)  Euros of 1995
HIH.label(c)  
HIH.label(r)   hih=fra_ihv
HIH.label(d)  Housing investment by households
HIH.label(u)  Euros of 1995
HRDI.label(c)  
HRDI.label(r)   hrdi=hdi/pcoh
HRDI.label(d)  Household disposble income
HRDI.label(u)  Euros of 1995
I.label(c)  
I.label(r)   i=fra_ibv
I.label(d)  Investment
I.label(u)  Euros of 1995
IC.label(c)  
IC.label(r)   ic=tc*q
IC.label(d)  Intermediate consumption
IC.label(u)  Euros of 1995
ICT.label(c)  
ICT.label(r)   ict=fra_tyh
ICT.label(d)  Income tax
ICT.label(u)  Euros
ID.label(c)  
ID.label(r)   id=kd-k(-1)*(1-rdep)
ID.label(d)  Target investment
ID.label(u)  Euros of 1995
IFP.label(c)  
IFP.label(r)   ifp=fra_tyb
IFP.label(d) Tax on firms profits
IFP.label(u)  Euros
IG.label(c)  
IG.label(r)   ig=fra_igv
IG.label(d)  Government investmeant
IG.label(u)  Euros of 1995
IGV.label(c)  
IGV.label(r)   igv=fra_ig
IGV.label(d)  Government investmeant
IGV.label(u)  Euros
IR.label(c)  
IR.label(r)   ir = fra_irwyp
IR.label(d)  Interest rate, average on new borrowing
IR.label(u)  Points
IRL.label(c)  
IRL.label(r)   irl=fra_irl
IRL.label(d)  Interest rate, long run
IRL.label(u)  Points
IRL_EC.label(c)  
IRL_EC.label(r)   irl_ec=0
IRL_EC.label(d)  Residual on the interest rate, long run
IRL_EC.label(u)  Points 
IRM.label(c)  
IRM.label(r)   irm=ir
IRM.label(d)  interest rate, average on current debt
IRM.label(u)  Points 
IRMX.label(c)  
IRMX.label(r)   irmx=fra_irfor
IRMX.label(d)  interest rate, average on current debt, exogenous
IRMX.label(u)  Points
IRS.label(c)  
IRS.label(r)   irs=fra_irs
IRS.label(d)  Interest rate, short term
IRS.label(u)  Points
IRSR.label(c)  
IRSR.label(r)   irsr=irs-100*@pchy(pcoh) 
IRSR.label(d)  Interest rate, short term, real
IRSR.label(u)  Points
IRST.label(c)  
IRST.label(r)   irst=irs-(150*@pchy(pcoh) +50*(ur-urd) /urd) 
IRST.label(d)  Interest rate, Taylor residual
IRST.label(u)  Points
IRSX.label(c)  
IRSX.label(r)   irsx=fra_irsaf
IRSX.label(d)  Interest rate, short term exogenous
IRSX.label(u)  Points
IRX.label(c)  
IRX.label(r)   irx=fra_irfor
IRX.label(d)  Interest rate, foreign
IRX.label(u)  Points
K.label(c)  
K.label(r)   k=fra_kbv
K.label(d)  Productive capital
K.label(u)  Euros of 1995
KD.label(c)  
KD.label(r)   log(kd/q(-1))=-c_cd(3)*(t-2005)-c_cd(7)+c_cd(2)*log(relc)
KD.label(d)  Target productive capital
KD.label(u)  Euros of 1995
LF.label(c)  
LF.label(r)   lf=fra_etb
LF.label(d)  Employment of firms
LF.label(u)  Persons
LFD.label(c)  
LFD.label(r)   log(lfd/q(-1))=-c_cd(3)*(t-2005)-c_cd(1)+(c_cd(2)-1)*log(relc)
LFD.label(d)  Target employment of firms
LFD.label(u)  Persons
LG.label(c)  
LG.label(r)   lg=fra_eg
LG.label(d)  Employment of Government
LG.label(u)  Persons
LT.label(c)  
LT.label(r)   lt=fra_et
LT.label(d)  Employment, total
LT.label(u)  Persons
M.label(c)  
M.label(r)   m=fra_mgsv
M.label(d)  Imports
M.label(u)  Euros of 1995
MARG.label(c)  
MARG.label(r)   marg=qval*(1+r_subs-r_oit) -wf*(1+r_scf) 
MARG.label(d)  Firms margins
MARG.label(u)  Euros
MVAL.label(c)  
MVAL.label(r)   mval=pm*m
MVAL.label(d)  Imports
MVAL.label(u)  Euros
NIF.label(c)  
NIF.label(r)  nif = nif(-1)  * irm / irm(-1)  - ir / 100 * fcapf + nier * qval
NIF.label(d)  Interests paid by firms, net
NIF.label(u)  Euros
NIG.label(c)  
NIG.label(r)   nig=fra_gnintp
NIG.label(d)  Interests paid by Government
NIG.label(u)  Euros
NIG_ER.label(c)  
NIG_ER.label(r)   nig_er=(nig-(nig(-1) *irm/irm(-1) -ir/400*fcapg) ) /qval
NIG_ER.label(d)  Interests paid by Government
NIG_ER.label(u)  Ratio
NIX.label(c)  
NIX.label(r)   nix=nixl+nixx
NIX.label(d)  Interests paid by the rest of the world
NIX.label(u)  Euros
NIXL.label(c)  
NIXL.label(r)   nixl=-ir*trb/40*p_nix  -- Modified: 1990Q2 2004Q4 => 
NIXL.label(d)  Interests paid by the rest of the world, local currency
NIXL.label(u)  Euros
NIXX.label(c)  
NIXX.label(r)   nixx=-irx*trb/40*(1-p_nix)   -- Modified: 1990Q2 2004Q4 => 
NIXX.label(d)  Interests paid by the rest of the world, foreign currency
NIXX.label(u)  Euros
OIT.label(c)  
OIT.label(r)   oit= p_oit* fra_tind
OIT.label(d)  Other indirect taxes
OIT.label(u)  Euros 
PC.label(c)  
PC.label(r)   pcoh=fra_cp/fra_cpv
PC.label(d)  Deflator of household consumption
PC.label(u)  Deflator, base 1995
PFD.label(c)  
PFD.label(r)   nixl=(nixl(-1) *irm/irm(-1) -ir*trb/100*p_nix) /(1-ir/100) 
PFD.label(d)  Deflator of final demand
PFD.label(u)  Deflator, base 1995
PFDXT.label(c)  
PFDXT.label(r)   nixx=(nixx(-1) *irmx/irmx(-1) *er/er(-1) -irx*trb/100*(1-p_nix) ) /(1-irx/100) 
PFDXT.label(d)  Deflator of Final demand, excluding VAT
PFDXT.label(u)  Deflator, base 1995
PGDPM.label(c)  
PGDPM.label(r)   pgdpm=gdpmval/gdpm
PGDPM.label(d)  Deflator of Gross Domestic Product
PGDPM.label(u)  Deflator, base 1995
PI.label(c)  
PI.label(r)   pi=fra_ib/fra_ib
PI.label(d)  Deflator of Firms investment
PI.label(u)  Deflator, base 1995
PIG.label(c)  
PIG.label(r)   pig=igv/ig
PIG.label(d)  Deflator of Government investment
PIG.label(u)  Deflator, base 1995
PK.label(c)  
PK.label(r)   pk=cap/k(-1) 
PK.label(d)  Productivity of capital
PK.label(u)  Deflator, base 1995
PL.label(c)  
PL.label(r)   pl=q / lf
PL.label(d)  Productivity of labor
PL.label(u)  Deflator, base 1995
PM.label(c)  
PM.label(r)   pm=fra_pmgs
PM.label(d)  Deflator of imports
PM.label(u)  Deflator, base 1995
PMT.label(c)  
PMT.label(r)   pmt=pm*(1+r_tar) /(1+r_tar0) 
PMT.label(d)  Deflator of imports including tariffs
PMT.label(u)  Deflator, base 1995
POP.label(c)  
POP.label(r)   pop=fra_popt
POP.label(d)  Opopulation, total
POP.label(u)  Deflator, base 1995
POP65.label(c)  
POP65.label(r)   pop65=fra_popt
POP65.label(d)  Population in age of working
POP65.label(u)  Deflator, base 1995
POPAC.label(c)  
POPAC.label(r)   popac=lt+un
POPAC.label(d)  Work force
POPAC.label(u)  Deflator, base 1995
PP.label(c)  
PP.label(r)   pp=(qval+ic*pfdxt) /(q+ic) 
PP.label(d)  Deflator of Production
PP.label(u)  Deflator, base 1995
PPX.label(c)  
PPX.label(r)   ppx=oecd_pgdp
PPX.label(d)  Deflator of foreign production
PPX.label(u)  Deflator, base 1995
PQ.label(c)  
PQ.label(r)   pq=qval/q
PQ.label(d)  Deflator of value added
PQ.label(u)  Deflator, base 1995
PROF.label(c)  
PROF.label(r)   prof=fra_prof
PROF.label(d)  Firms profits
PROF.label(u)  Euros 
PROF1.label(c)  
PROF1.label(r)   prof1=marg-revq-ifp-nif
PROF1.label(d)  Firms profits, second definition
PROF1.label(u)  Euros
PX.label(c)  
PX.label(r)   px=fra_pxgs
PX.label(d)  Deflator, exports
PX.label(u)  Deflator, base 1995
Q.label(c)  
Q.label(r)   q = gdpm-r_vat0*fd/(1+r_vat0) 
Q.label(d)  Value added
Q.label(u)  Euros of 1995
QVAL.label(c)  
QVAL.label(r)   qval=gdpmval-vat
QVAL.label(d)  Value added
QVAL.label(u)  Euros
R_EXPG.label(c)  
R_EXPG.label(r)   r_expg=(expg-(fdgv+wg+scg+nig+socb+subs) ) /qval
R_EXPG.label(d)  Residual on Government expenditures
R_EXPG.label(u)  Ratio
R_ICT.label(c)  
R_ICT.label(r)   r_ict = fra_tyh / hi(-1) 
R_ICT.label(d)  Income tax rate
R_ICT.label(u)  Ratio
R_IFP.label(c)  
R_IFP.label(r)   r_ifp=ifp/(prof(-1) +ifp(-1) ) 
R_IFP.label(d)  Rate of the tax on Firms profits 
R_IFP.label(u)  Ratio
R_OIT.label(c)  
R_OIT.label(r)   r_oit=oit/(gdpmval-vat-oit) 
R_OIT.label(d)  Other indirect taxes rate
R_OIT.label(u)  Ratio 
R_PI.label(c)  
R_PI.label(r)   r_pi=pi/pfd
R_PI.label(d)  Ratio of the firms investment price to the global demand deflator
R_PI.label(u)  Ratio
R_PIG.label(c)  
R_PIG.label(r)   r_pig=pig/pfd
R_PIG.label(d)  Ratio of the Government investment price to the global demand deflator
R_PIG.label(u)  Ratio
R_REVG.label(c)  
R_REVG.label(r)   r_revg=0
R_REVG.label(d)  Residual on Government revenue
R_REVG.label(u)  Ratio
R_REVQ.label(c)  
R_REVQ.label(r)   r_revq=revq/qval
R_REVQ.label(d)  Other household revenue based on GDP
R_REVQ.label(u)  Ratio
R_REVX.label(c)  
R_REVX.label(r)   r_revx=revx/pfd
R_REVX.label(d)  Other household revenue not based on GDP
R_REVX.label(u)  Ratio
R_SCF.label(c)  
R_SCF.label(r)   r_scf =  fra_wsss/fra_wage -1
R_SCF.label(d)  Rate of social security contributions paid by firms
R_SCF.label(u)  Ratio
R_SCG.label(c)  
R_SCG.label(r)   r_scg=  r_scf
R_SCG.label(d)  Rate of social security contributions paid by Government
R_SCG.label(u)  Ratio
R_SCW.label(c)  
R_SCW.label(r)   r_scw=scw/w
R_SCW.label(d)  Rate of social security contributions paid by households
R_SCW.label(u)  Ratio
R_SUBS.label(c)  
R_SUBS.label(r)   r_subs = fra_tsub / (qval) 
R_SUBS.label(d)  Rate of firms subsidies to value added
R_SUBS.label(u)  Ratio
R_TAR.label(c)  
R_TAR.label(r)   r_tar=tar/mval
R_TAR.label(d)  Rate of local tariffs
R_TAR.label(u)  Ratio
R_TARX.label(c)  
R_TARX.label(r)   r_tarx=0
R_TARX.label(d)  Rate of foreign tariffs
R_TARX.label(u)  Ratio
R_VAT.label(c)  
R_VAT.label(r)   r_vat =vat/(fd*pfd-vat) 
R_VAT.label(d)  VAT rate
R_VAT.label(u)  Ratio 
RCVAL.label(c)  
RCVAL.label(r)   rcval=xval/mval
RCVAL.label(d)  Ratio of exports to import
RCVAL.label(u)  Ratio
RCVOL.label(c)  
RCVOL.label(r)   rcvol=x/m
RCVOL.label(d)  Ratio of exports to import
RCVOL.label(u)  Ratio
RDEP.label(c)  
RDEP.label(r)   rdep = (k(-1)  + i - k)  / k(-1) 
RDEP.label(d)  Depreciation rate of capital
RDEP.label(u)  Ratio
RELAX_PFD.label(c)  
RELAX_PFD.label(r)   relax_pfd=1
RELAX_PFD.label(d)  Damping factor on the demand deflator
RELAX_PFD.label(u)  Technical
RELAX_Q.label(c)  
RELAX_Q.label(r)   relax_q=1
RELAX_Q.label(d)  Damping factor on value added
RELAX_Q.label(u)  Technical
RES_WR.label(c)  
RES_WR.label(r)   res_wr=LOG(UWC)-0.5*log(pq)-0.5*log(pc)+3.455570*UNR+0.183764
RES_WR.label(d)  Residual on the cointegrating equation for WR
RES_WR.label(u)  Technical
REVG.label(c)  
REVG.label(r)   revg=ict+oit+vat+scf+scg+tar+scw+ifp
REVG.label(d)  Government revenue
REVG.label(u)  Euros
REVQ.label(c)  
REVQ.label(r)   revq=0.5*rpro
REVQ.label(d)  Household revenue, GDP based (excluding wages) 
REVQ.label(u)  Euros
REVX.label(c)  
REVX.label(r)   revx=(1-0.5) *rpro
REVX.label(d)  Household revenue, non GDP based 
REVX.label(u)  Euros
RMARG.label(c)  
RMARG.label(r)   rmarg =marg / qval
RMARG.label(d)  Margins ratio of firms
RMARG.label(u)  Ratio
RPRO.label(c)  
RPRO.label(r)   rpro=hi-(w-scw+socb) 
RPRO.label(d)  Household revenue other than wages and benefits
RPRO.label(u)  Ratio
RPROB.label(c)  
RPROB.label(r)   rprob = marg/(pfd*k(-1) ) 
RPROB.label(d)  Profits ratio of firms, second definition
RPROB.label(u)  Ratio
RPROF.label(c)  
RPROF.label(r)   rprof=prof/(pfd*k(-1) ) 
RPROF.label(d)  Profits ratio of firms
RPROF.label(u)  Ratio
SCF.label(c)  
SCF.label(r)   scf=r_scf*wf
SCF.label(d)  Social security payments of firms
SCF.label(u)  Euros
SCG.label(c)  
SCG.label(r)   scg=wg*r_scf
SCG.label(d)  Social security payments of Government
SCG.label(u)  Euros
SCW.label(c)  
SCW.label(r)   scw= fra_trph  - r_scf*w
SCW.label(d)  Social security payments of workers
SCW.label(u)  Euros
SOCB.label(c)  
SOCB.label(r)   socb=fra_trrh
SOCB.label(d)  Social benefits
SOCB.label(u)  Euros
SOCBR.label(c)  
SOCBR.label(r)   socbr = fra_trrh /pcoh/pop
SOCBR.label(d)  Social benefits
SOCBR.label(u)  Euros of 1995
SR.label(c)  
SR.label(r)   sr=1-coh/(hrdi) 
SR.label(d)  Savings rate
SR.label(u)  Ratio
SUBS.label(c)  
SUBS.label(r)   subs=fra_tsub
SUBS.label(d)  Firms subsidies
SUBS.label(u)  Euros
T.label(c)  
T.label(r)   t=t(-1) +0.25
T.label(d)  Time
T.label(u)  Year
TAR.label(c)  
TAR.label(r)   tar=0
TAR.label(d)  Tariffs
TAR.label(u)  Euros
TC.label(c)  
TC.label(r)   tc=1
TC.label(d)  Technical coeffficient
TC.label(u)  Euros
TRB.label(c)  
TRB.label(r)   trb=xval-mval
TRB.label(d)  Trade balance
TRB.label(u)  Euros
TTRAD.label(c)  
TTRAD.label(r)   ttrad=px/pm
TTRAD.label(d)  Terms of trade
TTRAD.label(u)  Ratio
UN.label(c)  
UN.label(r)   un=fra_un
UN.label(d)  Unemployment
UN.label(u)  Persons
UNR.label(c)  
UNR.label(r)   unr=fra_un/(fra_et+fra_un) 
UNR.label(d)  Unemploymant rate
UNR.label(u)  Ratio
UR.label(c)  
UR.label(r)   ur=fra_gdpv/fra_gdpvtr
UR.label(d)  Capacity utilization rate
UR.label(u)  Ratio
URD.label(c)  
URD.label(r)   urd=1
URD.label(d)  Target capacity utilization rate
URD.label(u)  Ratio
UWC.label(c)  
UWC.label(r)   uwc=wr*(1+r_scf) /pl
UWC.label(d)  Unitary wage cost
UWC.label(u)  Deflator, base 1995
VAT.label(c)  
VAT.label(r)   vat = (1-p_oit) * fra_tind
VAT.label(d)  Euros
VAT.label(u)  Euros
W.label(c)  
W.label(r)   w=wr*lt
W.label(d)  Wages
W.label(u)  Euros
WD.label(c)  
WD.label(r)   wd = fra_xmvmkt
WD.label(d)  World demand
WD.label(u)  Volume
WF.label(c)  
WF.label(r)   wf=lt*wr-wg
WF.label(d)  Wages paid by firms
WF.label(u)  Euros
WG.label(c)  
WG.label(r)   wg=fra_cgw/(1+r_scg)    
WG.label(d)  Wages paid by Government
WG.label(u)  Euros
WR.label(c)  
WR.label(r)   wr=fra_wage/(fra_et-fra_es) 
WR.label(d)  Wage rate
WR.label(u)  Euros
X.label(c)  
X.label(r)   x=fra_xgsv
X.label(d)  Exports
X.label(u)  Euros of 1995
XVAL.label(c)  
XVAL.label(r)   x=fra_xgs 
XVAL.label(d)  Exports
XVAL.label(u)  Euros


