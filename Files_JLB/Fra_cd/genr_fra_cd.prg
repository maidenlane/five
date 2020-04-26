
'       An example of data transfer

'      This program will start from the original French data
'       provided by OECD Economic perspectives and named fra_*, 
'       the prefix used by OECD to identify French statistics

'      We decide on the directory
'      This is not generally necessary
'       except if one works on several projects
'      or maintains several directories for the same project
'       It guarantees trial versions do not destroy official ones

cd  "c:\users\jean louis\skydrive\eviews\book_filesfra_cd"

'      It will create the data for our model
'      with the prefix f_*

'-------------------------------------------------------------------------
'     This technique can be used with any source
'      where the original series use the same prefix
'      the results will be created with any different prefix

'      In the best case, if the set available is the same (or larger) than the OECD set
'      one has just to replace the OECD names in the following statements
'-------------------------------------------------------------------------

'       we close the original file fra_1 
'       containing the sole fra_* data
'      and some global OECD series named OECD_*

'      we close also the file which will receive the French data
'      in case it is already open 
'      having two versions of the same file open in memory is quite dangerous...

close fra_1
close data_1

'     We open the original file (presently closed)
'    a nd save it under the name data_1 for the French data

open fra_1
save data_1

'     Now the file should contain only original data
'     called fra_*

'     we delete any existing f_* series
'      just in case, this should not happen...

delete f_*

'  We start with the base page

pageselect base

'   We copy all series into a "model" page

pagedelete model
pagecreate(page=model) q 1962Q1 2010Q4
copy base\fra_* 
copy base\oecd_*

'     We produce a loop over all original series

'   Instead of a physical series, we will establish a like with the original page
'   For this, we need to delete the series first

'   Using this technique we run no risk of altering the original information

group g_origin fra_* oecd_*
for !i=1 to g_origin.@count
%1=g_origin.@seriesname(!i)
delete {%1}
link {%1}
{%1}.linkto base\{%1}
next

 
'     We have to make an assumption on the sharing of indirect taxes 
'     into VAT and other indirect taxes
'     as OECD provides only a global variable

'     p_oit = assumption on the share of oit in indirect taxes
'
scalar f_p_oit=0.2

'     we create a time trend
'     with the value of the year for the first quarter
'     to which we add 0.25 for each following quarter of the year

'      1994      : 1994Q1
'      1994.25 : 1994Q2
'      1994.50 : 1994Q3
'      1994.75 : 1994Q4
'      1995.00 : 1995Q1 ....

'   This will be quite useful to :

'      create yearly time trends
'      replace actual dummy variables by expressions using logical conditions
'      much easier to manage

smpl 1962Q1 1962Q1
genr f_t=1962
smpl 1962Q2 2010Q4
genr f_t=f_t(-1)+0.25

'     Now we start with the supply - demand equilibrium

smpl 1962Q1 2010Q4
genr f_gdpval=fra_gdp
genr f_gdp=fra_GDPV

'   The model separates market GDP

genr f_gdpmval=fra_gdp-fra_cgw
genr f_gdpm=fra_GDPV-fra_CGw/fra_pcp
genr f_pgdpm=f_gdpmval/f_gdpm

'      Trade af current and constant prices, deflators

genr f_m=fra_MGSV
genr f_x=fra_XGSV
genr f_pm=fra_PMGS
genr f_px=fra_PXGS
genr f_xval=f_px*f_x
genr f_mval=f_pm*f_m

'      Final demand, VAT and value added (excluding VAT)
'      This separation is important as VAT applies to final demand

genr f_pfd=(f_GDPmVAL+f_MVAL-f_XVAL)/(f_GDPm+f_M-f_X)
genr f_fd=fra_TDDV-fra_CGw/fra_pcp
genr f_oit= f_p_oit* fra_TIND 
genr f_vat = (1-f_p_oit)* fra_TIND
genr f_r_vat =f_vat/(f_fd*f_pfd-f_vat)
genr f_qval=f_gdpmval-f_vat
genr f_r_oit=f_oit/(f_qval-f_oit)
scalar f_r_vat0=@elem(f_r_vat,"1995")
genr f_q = f_gdpm-f_r_vat0*f_fd/(1+f_r_vat0)
genr f_pq=f_qval/f_q
genr f_k=fra_KBV
genr f_ur=fra_GDPV/fra_GDPVTR
genr f_tc=1
genr f_cap=f_q/f_ur
genr f_pk=f_cap/f_k(-1)
genr f_urd=1
genr f_ci=f_tc*f_q

'    elements of demand

genr f_coh=fra_CPV
genr f_pc=fra_cp/fra_cpv

genr f_i=fra_IBV
genr f_rdep = (f_k(-1) + f_i - f_k) / f_k(-1)
genr f_pi=fra_ib/fra_ib

genr f_hih=fra_IHV

genr f_ig=fra_IGV
genr f_igv=fra_IG
genr f_pig=f_igv/f_ig

genr f_ic=fra_iskv

genr f_COG=0
genr f_COGv=0

genr f_fdgv=f_COGv+f_igv
genr f_fdg=f_COG+f_ig

genr f_cog=0
genr f_gd=f_ig+f_cog

genr f_fdxr=(f_fd-f_coh-f_i-f_hih-f_ic-f_gd)/f_q

'    employment, unemployment, population

genr f_lt=fra_ET
genr f_lg=fra_EG
genr f_lf=fra_ET-fra_EG
genr f_pl=f_q / f_lf
genr f_un=fra_UN
genr f_unr=fra_UN/(fra_ET+fra_UN)
genr f_pop=fra_POPT
genr f_pop65=fra_POPT
smpl 1997Q1 2010Q4
genr f_pop=f_pop(-1)*1.005
smpl 1961Q1 2010Q4
genr f_popt=fra_popt
genr f_popac=f_lt+f_un

'    wages and further deflators


genr f_pp=(f_qval + f_pfd*f_ci*(1+f_r_vat0)/(1+f_r_vat)) / (f_q+f_ci)
genr f_wr=fra_WAGE/(fra_ET-fra_ES)
genr f_r_scf =  fra_WSSS/fra_WAGE -1
genr f_w=f_wr*f_lt
genr f_wf=f_lf*f_wr
genr f_scf=f_r_scf*f_wf
genr f_uwc=f_wr*(1+f_r_scf)/f_pl
genr f_pfdxt=f_pfd*(1+f_r_vat0)/(1+f_r_vat)
genr f_pp=(f_pq+f_tc*f_pfdxt)/(1+f_tc) 
smpl 1962Q1 2005Q4
genr f_er=1/(fra_EXCHEB/@elem(fra_EXCHEB,"1995"))
smpl 2006Q1 2010Q4
genr f_er=f_er(-1)
smpl 1962Q1 2005Q4
genr f_erx=f_er
genr f_r_pi=f_pi/f_pfd
genr f_r_pc=f_pc/f_pfd
genr f_r_pig=f_pig/f_pfd

'    interest rates

genr f_irs=fra_IRS
genr f_irl=fra_IRL
smpl 1959Q1 1969Q4
genr f_irs=f_irl
smpl 1963Q1 2010Q4
genr f_ir = 0.3 * f_irs + 0.7 * f_irl
smpl 1963Q1 1963Q1
genr f_irm=f_ir
smpl 1963Q2 2010Q4
genr f_irm=0.2*f_ir+0.8*f_irm(-1)
genr f_irsx=3
genr f_irmx=3
genr f_irx=3
genr f_irr=f_ir-100*@pch(f_pc)
genr f_irl_ec=0
genr f_irsr=f_irs-100*@pchy(f_pc)
genr f_irst=0

'     households


genr f_socb=fra_trrh
genr f_hi =fra_YRH-fra_TRPH
genr f_ict=fra_tyh
genr f_hdi=f_hi-fra_tyh
genr f_hrdi=f_hdi/f_pc
genr f_sr=1-f_coh/(f_hrdi)
genr f_scw= fra_TRPH  - f_r_scf*f_w
genr f_r_scw=f_scw/f_w
genr f_rpro=f_hi-(f_w-f_scw+f_socb)
genr f_revx=(1-0.5)*f_rpro
genr f_revq=0.5*f_rpro
genr f_r_revx=f_revx/f_pfd
genr f_r_revq=f_revq/f_qval
genr f_wg=f_wr*f_lg
genr f_hdir=f_hdi/f_pc

'    rates

genr f_r_scg=  f_r_scf
genr f_r_ict = fra_TYH / f_hi(-1)
scalar f_r_oit0=@elem(f_r_oit,"1995")

'     external trade

genr f_tar=0  
genr f_r_tar=f_tar/f_mval 
genr f_r_tarx=0
scalar f_r_tarx0=@elem(f_r_tarx,"1995")
scalar f_r_tar0=@elem(f_r_tar,"1995")
smpl 1962Q1 2004Q4 
genr f_ppx=OECD_PGDP
smpl 2005Q1 2010Q4
genr f_ppx=f_ppx(-1)*1.025
smpl 1962Q1 2010Q4
genr f_FCAPX=fra_FBGSV
genr f_wd = fra_XMVMKT
genr f_rcvol=f_x/f_m
genr f_rcval=f_xval/f_mval
genr f_ttrad=f_px/f_pm
genr f_trb=f_xval-f_mval
smpl 1970Q1 2010Q4
scalar p_nix=0.5
genr f_nixl=-f_ir*f_trb/40*p_nix
genr f_nixx=-f_irx*f_trb/40*(1-p_nix)
smpl 1970Q2 2010Q4
genr f_nixl=(f_nixl(-1)*f_irm/f_irm(-1)-f_ir*f_trb/100*p_nix)/(1-f_ir/100)
genr f_nixx=(f_nixx(-1)*f_irmx/f_irmx(-1)*f_er/f_er(-1)-f_irx*f_trb/100*(1-p_nix))/(1-f_irx/100)
genr f_nix=f_nixl+f_nixx
genr f_fcapx=f_trb-f_nix
genr f_PMT=f_PM*(1+f_r_tar)/(1+f_r_tar0)
genr f_compm=f_pmt/f_pp
genr f_COMPX=f_PX*(1+f_r_tarx)/(1+f_r_tarx0)/(f_PPX*f_ER)

'     budget elements

genr f_ifp=fra_tyb
genr f_fcapg=-FRA_NLG
genr f_socbr = fra_TRRH /f_pc/f_popt
genr f_r_oit=f_oit/(f_qval)
genr f_subs=fra_TSUB
genr f_r_subs = fra_TSUB / (f_qval)
genr f_cfg=fra_nlg
genr f_nig=fra_gnintp
genr f_scg=f_wg*f_r_scf
genr f_revg=f_ict+f_oit+f_vat+f_scf+f_scg+f_tar+f_scw+f_ifp
genr f_r_revg=0
genr f_expg=f_revg-f_fcapg
genr f_dfgv=f_igv
genr f_r_expg=(f_expg-(f_fdgv+f_wg+f_scg+f_nig+f_socb+f_subs))/f_qval
genr f_recg=0
genr f_r_recg=0
genr f_fcapgp=100*f_fcapg/f_qval
genr f_nig_er=(f_NIG-(f_NIG(-1)*f_IRM/f_IRM(-1)-f_IR/100*f_FCAPG))/f_qval

'      firms account

genr f_prof1=(fra_PXGS*fra_XGSV-fra_PMGS*fra_MGSV)-fra_NLG-fra_YDH+fra_CPV*fra_PCP+fra_IHV*fra_PIT+f_pfd*f_i+f_pp*f_ic
smpl 1975Q1 2010Q4
genr f_fcapf=f_prof1-f_pi*f_i-f_pfd*f_ic
smpl 1975Q1 1975Q1
genr f_nif=f_ir*f_fcapf/11
smpl 1975Q2 2010Q4
genr f_nif=(1-f_ir/100)*(f_nif(-1)*f_irm/f_irm(-1)+f_ir*(f_prof1-f_pi*f_i-f_pfd*f_ic)/100)
genr f_marg=(f_qval*(1-f_r_oit+f_r_subs)-f_wf*(1+f_r_scf))
genr f_rmarg =f_marg / f_qval
smpl 1962Q1 2010Q4
genr f_marg=f_qval*(1+f_r_subs-f_r_oit)-f_wf*(1+f_r_scf)
genr f_rprob = f_marg/(f_pfd*f_k(-1))
genr f_fdxr=(f_fd-(f_coh+f_i+f_ic+f_fdg))/f_q
genr f_prof=f_marg-f_revq-f_ifp-f_nif
genr f_r_IFP=f_ifp/(f_PROF(-1)+f_IFP(-1))
genr f_rprof=f_prof/(f_pfd*f_k(-1))
genr f_fcapf=f_prof-f_pi*f_i-f_pfd*f_ic 
genr f_NIF_er=(f_nif-(f_NIF(-1)*f_IRM/f_IRM(-1)-f_IR/100*f_FCAPF))/f_qval

genr f_relax_q=1
genr f_relax_pfd=1
genr f_tc=1

save data_1


