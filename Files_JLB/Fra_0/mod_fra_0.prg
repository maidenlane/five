'
'    This program creates the framework for a small single product model

'     The identities are completely specified and fully consistent with the data

'     The behavioral equations are stated as "declarations of intent" such as

'      _mod_1.append I=f*(Q+RPROF+UR)
 
'      stating that productive investment I depends on 
'      * value added Q
'      * the profits rate RPROF
'      * the rate of use UR

'     Of course this is only a starting point, but it allows :

'      * to check that all elements are available for estimation
'      * that the structure of the model is consistent with expectations and theory 
'      * which means that errors can be treated early

'       We specify the directory

cd "c:\users\jean louis\skydrive\eviews\book_files\fra_0"

'       We specify the output file

output(r,o) _mod_1

'      We close the workfiles

close data_1 
close mod_1 

'      We open the data workfile
'      We save it as a model file

open data_1  
wfsave mod_1
smpl 1960Q1 2005Q4

'     We start from the data page

pageselect data

'    We create a group for the transformed series

group g_f f_*

'     We create a blank "model" page

pagecreate(page=model)  q 1960Q1 2005Q4

'      We create a group for the series in the model

if @isobject("g_mod") then
delete g_mod
endif

group g_mod

'     We copy the group g_f into model

copy(g=l) data\g_f 

'     From the group g_f we create linked variables excluding the "f_" suffix
'     We link them to the original "data" series
'     We add them to the "g_mod" group

for !i=1 to g_f.@count
%1=g_f.@seriesname(!i)
%2=@mid(%1,3)
link {%2}
{%2}.linkto data\f_{%2}
g_mod.add {%2}
next 

'      We copy the four scalars from the page "data"

for %1 r_vat0 r_oit0 r_tar0 r_tarx0
copy data\f_{%1} {%1}
next

'      Now we start creating the model

'      We create a blank model

'     We create a "f" scalar

scalar f

if @isobject("_mod_1") then
delete _mod_1
endif

model _mod_1

'________________________
'
'     Model specifications
'________________________
'
'________________________
'
'     The production block
'________________________
'
'      The rate of use

_mod_1.append UR=Q/CAP

'      Value added (excluding VAT)

_mod_1.append Q=FD/(1+r_vat0)+X-M

'      Productive investment (to be estimated)

_mod_1.append I=f*(Q+RPROF+UR)

'     Change in inventories

_mod_1.append IC=f*(Q)

'     Employment of firms (to be estimated)

_mod_1.append LF=f*(Q)

'     Labor productivity

_mod_1.append PL=Q/LF

'      Productive capital

_mod_1.append K=K(-1)*(1-rdep)+I

'      Productive capacity (Complementary factors)

_mod_1.append CAP=pk*K(-1)

'      Total employment

_mod_1.append LT=LF+lg

'       Work force (to be estimated)

_mod_1.append POPAC=f*(LT+pop65)

'       Unemployment

_mod_1.append UN=POPAC-LT

'      Unemployment rate

_mod_1.append UNR=UN/POPAC
'________________________
'
'     The price block
'________________________

'      Unitary wage  cost

_mod_1.append UWC=WR*(1+r_scf)/PL

'      Value added deflator

_mod_1.append PQ=f*(UWC+UR)

'       Production deflator

_mod_1.append PP=(PQ+tc*PFDXT)/(1+tc)

'       Final demand deflator

_mod_1.append PFD=(GDPMVAL+MVAL-XVAL)/(GDPM+M-X)

'       Final demand deflator (excluding VAT)

_mod_1.append PFDXT=PFD*(1+r_vat0)/(1+r_vat) 

'       Household consumption price

_mod_1.append PC=f*(PFD)

'       Wage rate

_mod_1.append WR=f*(PC+UNR)

'       Imports deflator

_mod_1.append PM=f*(PP+PPX+ER)

'      Exports deflator

_mod_1.append PX=f*(PP+PPX+ER)

'       Exchange rate

_mod_1.append ER=f*(erx+PC+IR)

'       Average interest rate on new loans

_mod_1.append IR=f*(IRS+IRL)

'       Short term interest rate

_mod_1.append IRS=f*(PC+UR)

'      Long term interest rate

_mod_1.append IRL=f*(IRS)

'       Average interest rate on old loans

_mod_1.append IRM=f*(IRM(-1)+IR)

'_______________________
'
'     The households block
'_________________________

'      Social benefits

_mod_1.append SOCB=socbr*PC*pop

'      Civil servants wages

_mod_1.append WG=WR*lg

'       Total wages

_mod_1.append W=WF+WG

'        Social contributions paid by wage earners

_mod_1.append SCW=r_scw*W

'     Other revenue linked to production

_mod_1.append REVQ=r_revq*QVAL

'     Other revenue not linked with production

_mod_1.append REVX=r_revx*PC

'      Household income

_mod_1.append HI=W-SCW+REVQ+REVX+SOCB

'       Income tax

_mod_1.append ICT=r_ict*HI(-1)

'       Household disposable income

_mod_1.append HDI=HI-ICT

'       Household real disposable income

_mod_1.append HRDI=HDI/PC

'       Household consumption

_mod_1.append COH=f*(HRDI+PC+UNR+IRS+COH(-1))
'_______________________
'
'     The firms block
'_________________________

'       Value added at current prices

_mod_1.append QVAL=PQ*Q

'       Wages paid by firms

_mod_1.append WF=WR*LF

'        Subsidies to firms

_mod_1.append SUBS=r_subs*QVAL

'        Margins

_mod_1.append MARG=PQ*Q*(1+r_subs-r_oit)-WR*LF*(1+r_scf)

'        Margins rate

_mod_1.append RMARG=MARG/QVAL

'        Tax on firms profits

_mod_1.append IFP=(PROF(-1)+IFP(-1))*r_ifp

'         Net interests paid by firms

_mod_1.append NIF=f*(NIF(-1)+IRM+IRM(-1)+IR+FCAPF)

'         Profits

_mod_1.append PROF=MARG-REVQ-IFP-NIF

'        Profits rate

_mod_1.append RPROF=PROF/(PFD*K(-1))

'        Ratio of margins to capital

_mod_1.append RPROB=MARG/(PFD*K(-1))

'        Financing capacity

_mod_1.append FCAPF=PROF-PFD*I-PFD*IC
'_______________________
'
'     The external trade block
'_________________________

'       Imports deflator including tariffs

_mod_1.append PMT=PM*(1+r_tar)/(1+r_tar0)

'       Import price competitiveness

_mod_1.append COMPM=PMT/PP

'        Final demand

_mod_1.append FD=COH+I+IC+FDG+fdxr*Q

'        Imports

_mod_1.append M=f*(FD+X+UR+COMPM)

'        Export price competitiveness

_mod_1.append COMPX=PX*(1+r_tarx)/(1+r_tarx0)/(PPX*ER)

'        Exports

_mod_1.append X=f*(wd+UR+COMPX)

'        Imports at current prices

_mod_1.append MVAL=PM*M

'       Exports at current prices

_mod_1.append XVAL=PX*X

'        Exports - imports ratio at current prices

_mod_1.append RCVAL=XVAL/MVAL

'        Exports - imports ratio at constant prices

_mod_1.append RCVOL=X/M

'       Terms of trade

_mod_1.append TTRAD=PX/PM

'      Trade balance

_mod_1.append TRB=XVAL-MVAL

'        Net interests paid to the Rest of the World

_mod_1.append NIX=f*(NIX(-1)+IRM+IRM(-1)+IR+TRB+ER)

'        Financing capacity of the Nation

_mod_1.append FCAPX=TRB-NIX
'_______________________
'
'     The state budget block
'_________________________

'       Value added tax

_mod_1.append VAT=r_vat*PFD*FD/(1+r_vat)

'       GDP at current prices

_mod_1.append GDPMVAL=QVAL+VAT

'       GDP at constant prices

_mod_1.append GDPM=Q+FD*r_vat0/(1+r_vat0)

'       Social contributions of firms

_mod_1.append SCF=r_scf*Wf

'        Other indirect taxes

_mod_1.append OIT=r_oit*QVAL

'        Tariffs

_mod_1.append TAR=r_tar*MVAL

'        Social contributions paid by Government

_mod_1.append SCG=R_scg*WG

'        Government revenue

_mod_1.append REVG=SCF+scg+SCW+OIT+IFP+ICT+VAT+TAR+r_revg*QVAL

'        Government investment at current prices

_mod_1.append IGV=IG*PFD

'        Government consumption at current prices (excluding wages)

_mod_1.append COGV=cog*PFD

'       Government demand at current prices

_mod_1.append FDGV=cogV+IGV

'      Net interests paid by Government

_mod_1.append NIG=f*(NIG(-1)+IRM+IRM(-1)+IR+FCAPG)

'       Government expenditures

_mod_1.append EXPG=FDGV+WG+SUBS+SOCB+NIG+scg+r_expg*QVAL

'        Government financing capacity

_mod_1.append FCAPG=REVG-EXPG

'        Government financing capacity in GDP points

_mod_1.append FCAPGP=100*FCAPG/GDPMVAL

'_______________________
'
'     End of model specifications
'_________________________
 
'     We create groups for the endogenous and exogenous
        
_mod_1.makegroup(a,n) g_vendo @endog
_mod_1.makegroup(a,n) g_vexog @exog

'      We create groups for the behavioral and identity variables

group g_vbeha I LF IC POPAC WR ER PQ PC ER IR IRS IRL IRM X M PX PM COH NIX NIF NIG

'      We start with a full group and we drop the behavioral (much less numerous)

_mod_1.makegroup(a,n) g_viden @endog 
g_viden.drop I LF IC POPAC WR ER PQ PC ER IR IRS IRL IRM X M PX PM COH NIX NIF NIG 

'     We check the residuals

_mod_1.append assign @all _c

smpl 1995Q1 2004Q4
solve(d=f) _mod_1

'       We compute the level and relative differences

for !i=1 to g_viden.@count
%1=g_viden.@seriesname(!i)
genr dc_{%1}={%1}-{%1}_c
genr pc_{%1}=100*dc_{%1}/({%1}+({%1}=0))
next

'       We save the workfile

save mod_1


