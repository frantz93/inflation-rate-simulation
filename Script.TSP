?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
? SIMULATION CONJONCTURE ECONOMIQUE       MASTER 2        IEE       
                         
?XXXXXXXXXXXXXXXXX
NAME SIMULATION;
FREQ Q;
SMPL 1963:1 2021:1;

TREND TIME;	
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
?                               ***********************
?                               * LECTURE DES DONNEES *
?                               ***********************

LOAD(FILE='Data1963-2021.XLS',format=EXCEL) ;
SHOW series;

?PRINT rd;
?STOP;
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
?                         ******************************
?                         * CONSTRUCTION DES VARIABLES *
?                         ******************************
?                ATTENTION: NE RIEN MODIFIER DANS CETTE PARTIE
?                *********************************************
?GENR lsmic=log(smic);
GENR LPqe = log(Pqe);
GENR LQePqe = log(QePqe);
GENR LPy = log(Py);
GENR LPc = log(Pc);
GENR LPin = log(Pinv);
GENR LPm = log(Pm);
GENR LPx = log(Px);
GENR LYPy = log(YPy);
GENR LCPc = log(CPc);
GENR LINVP = log(INVP);
GENR LMPm = log(MPm);
GENR LXPx = log(XPx);
GENR LW = log(W);
GENR LH = log(H);
GENR LN = log(L);
GENR LGpg = log(Gpg);
GENR lpg=log(pg);
GENR lpdi=log(pdi);
GENR lrd=log(rd);
GENR ltcot=log(tcot);
GENR LuPRES = log(1.0-TPRES/1000);
GENR LuCoT = log(1.0-TCot/1000);
GENR LCSU = LW + LN + LH - LYPy - LuCot;
GENR LPyPm = LPy - LPm;
GENR LPinPy = LPin - LPy;
GENR LCTSRL = LW + LH - LuCot - LPY;
GENR BC = Px*XPX - PM*MPM;
GENR LNK = LN - log(K);
GENR a = ypy/l;
GENR LA = log(a);
GENR lac=la-ltcot;
GENR lwpc=lw-lpc;
GENR lwtpy=lw+ltcot-lpy;
GENR lwta=lw+ltcot-la;
GENR LPxPqe = LPx - LPqe;


?                       *************************************
?                       * CALCUL DU COUT D'USAGE DU CAPITAL *
?                       *************************************
SMPL 1963:2 2021:1;
GENR DLPY = LPY-lPY(-1);                                                        
SMPL 1964:2 2021:1;
GENR DT = .02 + .1*(.2*LPYPM + .4*LPYPM(-1) + .2*LPYPM(-2) + .1*LPYPM(-3)
	  + .1*LPYPM(-4));
SMPL 1963:1 64:1;
GENR DT = DT(1964:2);
SMPL 1964:2 2021:1;
GENR PP = .2*DLPY + .4*DLPY(-1) + .2*DLPY(-2) + .1*DLPY(-3) + .1*DLPY(-4);
SMPL 1963:1 1964:1;
GENR PP = PP(1964:2);
smpl 1963:1 2021:1;
GENR LRPD = log(tr - 100*PP + 100*DT);
GENR LVC = LPinPy + LRPD;
GENR LWLVC = LW + LH - LuCot - LVC - LPY;
smpl 1963:2 2021:1;
GENR IK = INVP/K(-1);
GENR lik=log(ik);
?**************************************************
smpl 1963:2 2021:1;
GENR dlwtpy=lwtpy-lwtpy(-1);
GENR dlnk=lnk-lnk(-1);
GENR dlpy=log(py/py(-1));
GENR dduc=duc-duc(-1);
GENR dlcsu=lcsu-lcsu(-1);
GENR dlypy=log(ypy/ypy(-1));
GENR dlpinpy=lpinpy-lpinpy(-1);
GENR dlrpd=lrpd-lrpd(-1);
GENR dln=log(l/l(-1));
GENR dlwlvc=lwlvc-lwlvc(-1);
GENR dlh=log(h/h(-1));
GENR dlvc=lvc-lvc(-1);
GENR dlnk=lnk-lnk(-1);
GENR dlcpc=log(cpc/cpc(-1));
GENR dlrd=lrd-lrd(-1);
GENR dlxpx=log(xpx/xpx(-1));
GENR dlqepqe=log(qepqe/qepqe(-1));
GENR dlpxpqe=lpxpqe-lpxpqe(-1);
GENR dlmpm=log(mpm/mpm(-1));
GENR dlpypm=lpypm-lpypm(-1);
GENR dlpx=log(px/px(-1));
GENR dlpqe=log(pqe/pqe(-1));
GENR dlinvp=log(invp/invp(-1));
GENR dlg=log(gpg/gpg(-1));
GENR dtcho=tcho-tcho(-1);
GENR dlwpc=lwpc-lwpc(-1);
GENR dla=la-la(-1);
GENR dlpc=lpc-lpc(-1);
GENR dlpm=lpm-lpm(-1);
GENR dlw=lw-lw(-1);
GENR dtr=tr-tr(-1);
GENR dlwta=lwta-lwta(-1);
GENR dlpdi=lpdi-lpdi(-1);
GENR dlac=lac-lac(-1);

smpl 1963:3 2021:1;
GENR ddlpy=dlpy-dlpy(-1);
GENR dik=ik-ik(-1);
GENR dlik=lik-lik(-1);
GENR dtinfl = tinfl-tinfl(-1);

?STOP;

?XXXXXXXXXXXXXXXXXXXXXXXX
?    ZONE DE TRAVAIL 	X
?XXXXXXXXXXXXXXXXXXXXXXXX

? PARTIE 1 : ESTIMATION SEPAREES DES EQUATIONS

SMPL 1964:2 2021:1;						


?				  ********** Taux inflation **********

GENR dTCHO03 = TCHO - TCHO(3);
GENR DLGPG34 = LGPG(-3) - LGPG(-4);
GENR DLINVP13 = LINVP(-1) - LINVP(-3);
GENR dlrd15 = lrd(-1) - lrd(-5);
GENR dlrd04 = lrd - lrd(-4);
GENR DLGPG02 = LGPG - LGPG(-2);
GENR dtinfl15 = tinfl(-1) - tinfl(-5);
GENR dlypy15 = lypy(-1) - lypy(-5);
GENR dlypy03 = lypy - lypy(-3);
GENR dtinfl24 = tinfl(-2) - tinfl(-4);

? Modèle brut initial
?olsq dtinfl c tinfl(-1) tinfl(-2) tinfl(-3) tinfl(-4)tinfl(-5)
?			  lgpg   lgpg(-1) lgpg(-2) lgpg(-3) lgpg(-4) lgpg(-5)
?			  tcho	 tcho(-1) tcho(-2) tcho(-3)	tcho(-4) tcho(-5)
?			  lypy   lypy(-1) lypy(-2)lypy(-3) lypy(-4) lypy(-5)
?			  linvp  linvp(-1)linvp(-2) linvp(-3) linvp(-4)linvp(-5)
?			  lrd    lrd (-1) lrd(-2) lrd(-3) lrd(-4)lrd(-5) time;

? Modèle finalisé

olsq dtinfl  c	  tinfl(-3) 
			  DLGPG02	lgpg(-5)
			  tcho(-4) 
			  dlypy03	dlypy15		lypy(-2)	lypy(-4) 
			  DLINVP13	linvp		linvp(-4)	linvp(-5)
			  dlrd04	lrd(-2)		time;
?stop;


? Equation du modèle

FRML eqtinfl tinfl = a0 + a1*tinfl(-1) + a2*tinfl(-3)
			  		+ a3*(lgpg - lgpg(-2)) + a4*lgpg(-5)
					+ a5*tcho(-4)
					+ a6*(lypy - lypy(-3)) + a7*(lypy(-1) - lypy(-5))
					+ a8*lypy(-2) + a9*lypy(-4)
			  		+ a10*(linvp(-1) - linvp(-3))+ a11*linvp
					+ a12*linvp(-4)	+ a13*linvp(-5)
					+ a14*(lrd - lrd(-4)) + a15*lrd(-2) + a16*time;
					
PARAM a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16;
LSQ eqtinfl;

?stop;

?                   ********** investissement **********

? PROVIENT DU MCE
?olsq dik c dlyp dlpinpy(-2) dlrpd(-4) ddlpy(-3) lpypm(-4)
?      ik(-1)  time;


?REGOPT(star) all;
FRML eqik ik=b0+(1+b1)*ik(-1)+b2*(lypy-lypy(-1))+b3*(lpinpy(-2)-lpinpy(-3))+
	     b4*(lrpd(-4)-lrpd(-5))+b5*(dlpy(-3)-dlpy(-4))+b6*lpypm(-4)
	     +b10*time;

PARAM b0 b1 b2 b3 b4 b5 b6 b10;
LSQ eqik;

?stop;
 ?                       *************************
?                       * RELATIONS D'IDENTITES *
?                       *************************

IDENT IDLRPD LRPD = log(tr - 100*PP + 100*DT);
IDENT IDPP   PP = .2*(LPy-LPy(-1)) + .4*(LPy(-1)-LPy(-2)) +
		  .2*(LPy(-2)-LPy(-3)) + .1*(LPy(-3)-LPy(-4)) +
		  .1*(LPy(-4)-LPy(-5));
IDENT IDDT DT = .02 + .1*(.2*LPyPm + .4*LPyPm(-1) + .2*LPyPm(-2) +
		  .1*LPyPm(-3) + .1*LPyPm(-4));
IDENT idlvc lvc=lpinpy+lrpd;
IDENT IDINVP INVP = IK*K(-1);           
IDENT IDlPyPm LPyPm = LPy - LPm;
IDENT IDlPinPy LPinPy = LPin - LPy;
ident iddlpy dlpy=lpy-lpy(-1);
 
ident idlpxpqe LPxPqe = LPx - LPqe;

?                ************** consommation **************

?OLSQ dlcpc c dlcpc(-3) dlrd lcpc(-1) lrd(-1) tcho(-1) lpc(-2);
?stop;
FRML eqlcpc lcpc=d0+(1+d1)*lcpc(-1)+d2*(lcpc(-3)-lcpc(-4))+d3*(lrd-lrd(-1))+
		 d4*lrd(-1)+d5*tcho(-1)+d6*lpc(-2);

PARAM d0 d1 d2 d3 d4 d5 d6;
?const d6 -1;
LSQ eqlcpc;

?STOP;

?          **************** equation d'equilibre BOUCLAGE DYNAMIQUE*****************


?OLSQ dlyp c lyp(-1) dlcpc dlinvp dlg dlxpx dlmpm lcpc(-1) linvp(-1)
?	  lg(-1) lxpx(-1) lmpm(-1);

FRML eqlyp lypy=i0+(1+i1)*lypy(-1)+i2*lcpc+(i3-i2)*lcpc(-1)+
				 i4*linvp+(i5-i4)*linvp(-1)+
				 i6*lgpg+(i7-i6)*lgpg(-1)+
				 i8*lxpx+(i9*i8)*lxpx(-1)+
				 i10*lmpm+(i11-i10)*lmpm(-1);

PARAM i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11;
LSQ eqlyp;

?                ************** exportations **************

?OLSQ dlxpx c dlxpx12 lxpx duc(-3) lqepqe dlqepqe12 LPxPqe(-3) dLPxPqe12  ;

FRML eqlxpx lxpx= f0 + (f1-1)*lxpx(-1) - f2*lxpx(-2) + f3*duc(-3) + f4*lqepqe  +
f5*(lqepqe(-1)-lqepqe(-2)) + f6*lpxpqe(-3) + f7*(lpxpqe(-1)-lpxpqe(-2));

PARAM f0 f1 f2 f3 f4 f5 f6 f7;
LSQ eqlxpx;

?                ************** IMPORTATIONS **************
?GENR dLPYPM13=lpypm(-1) - lpypm(-3);
?GENR dLmpm13=lmpm(-1) - lmpm(-3);
?GENR dLypy12=lypy(-1) - lypy(-2);

?OLSQ dlMpM c dlMpM13 lMpM duc(-3) lYPY(-3) dlYPY12 LPYPM dLPYPM13  ;

FRML eqlmpm lmpm= g0 + (g1-1)*lxpx(-1) - g2*lxpx(-3) + g3*duc(-3) + g4*lypy  +
g5*(lypy(-1)-lypy(-2)) + g6*lpypm(-3) + g7*(lpypm(-1)-lpypm(-3));

PARAM g0 g1 g2 g3 g4 g5 g6 g7;
LSQ eqlmpm;


?STOP;

?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
? PARTIE 2 : MAXIMUM DE VRAISEMBLANCE 
? FIML : full information maximum de vraisemblance

FIML (ENDOG=(ik,lcpc,lypy,lxpx,lmpm,tinfl)) eqik, eqlcpc, eqlyp, eqlxpx, eqlmpm, eqtinfl;


SIML(tag=SS,ENDOG=(ik,lcpc,lypy,lxpx,lmpm,tinfl,lrpd,pp,dt,lvc,invp,lpypm,lpinpy,dlpy,lpxpqe))
eqik eqlcpc eqlyp eqlxpx eqlmpm eqtinfl IDLRPD IDPP IDDT idlvc
IDINVP IDlPyPm IDlPinPy iddlpy idlpxpqe;

?plot(prev,titl='SIMULATION DE L INVESTISSEMENT') ik ikss;
?plot(prev,titl='SIMULATION DE LA CONSOMMATION') LCPC LCPCSs;
?plot(prev,titl='SIMULATION DE LA PRODUCTION') LYPy LYPyss;
?plot(prev,titl='SIMULATION DES EXPORTATIONS') LXPX LXPXss;

?stop;

?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
? PARTIE 3 : SIMULATION SANS CHOC 

SMPL 1964:3 2021:1;

LIST ENDOG ik lcpc lypy LXPX lmpm tinfl lrpd pp dt lvc invp lpypm lpinpy dlpy lpxpqe;
LIST EQUAT eqik eqlcpc  eqlyp EQLXPX eqlmpm eqtinfl IDLRPD IDPP IDDT idlvc
IDINVP IDlPyPm IDlPinPy iddlpy idlpxpqe;
	  
MODEL equat endog fin;
SOLVE(tag=s0,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

plot(prev,titl='SIMULATION DE L INVESTISSEMENT') ik iks0;
plot(prev,titl='SIMULATION DE LA CONSOMMATION') LCPC LCPCS0;
plot(prev,titl='SIMULATION DE LA PRODUCTION') LYPy LYPys0;
plot(prev,titl='SIMULATION DES EXPORTATIONS') LXPX LXPXs0;
plot(prev,titl='SIMULATION DES IMPORTATIONS') Lmpm Lmpms0;
plot(prev,titl='SIMULATION DE L INFLATION') tinfl tinfls0;

?WRITE (FILE='simul.xls',FORMAT=excel) ik iks0 lcpc lcpcs0 lyp lyps0;

?STOP;

?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
?goto 2; ? pour choc 2
?goto 3; ? pour choc 3
?goto 4; ? pour choc 4
?goto 5; ? pour choc 5
?goto 6; ? pour choc 6


? PARTIE 3 : SIMULATION AVEC CHOC

? CHOC N°1: AUGMENTATION DU REVENU disponible DE 10% en début de période d estimation
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

SMPL 64:3 64:3;
SET z=1.10;	   ? augmentation de 10%
GENR rd = rd*z;
GENR Lrd = log(rd);
print rd;
SMPL 1964:3 2021:1;

? SIMULATION DU MODELE AVEC CHOC

SOLVE(tag=c1,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

GENR C1LCPC = (LCPCC1 - LCPCS0)/z*100;
GENR C1LYPy = (LYPYC1 - LYPYS0)/z*100;
GENR C1IK = (IKC1 - IKS0)/z*100;
GENR c1lxpx = (LXPXC1 - LXPXS0)/z*100;
GENR c1lmpm = (LmpmC1 - LmpmS0)/z*100;
GENR c1tinfl = (tinflC1 - tinflS0)/z*100;

plot(prev,titl='GR 1 SIMULATION conso avec choc SUR rd DE + 10%')  c1lcpc ;
plot(prev,titl='GR 1 SIMULATION production avec choc SUR rd DE + 10% ')  c1lypy ;
plot(prev,titl='GR 1 SIMULATION investissement avec choc SUR rd DE + 10%')  c1ik ;
plot(prev,titl='GR 1 SIMULATION EXPORTATION avec choc SUR rd DE + 10% ')  c1lXpX ;
plot(prev,titl='GR 1 SIMULATION IMPORTATION avec choc SUR rd DE + 10% ')  c1lMpM ;
plot(prev,titl='GR 1 SIMULATION INFLATION avec choc SUR rd DE + 10% ')  c1tinfl ;

title 'choc SUR rd DE +10%';
PRINT C1LCPC C1LYPY C1IK c1lxpx c1lMpM c1tinfl;

?stop;


? CALCUL DES MULTIPLICATEURS DE LONG TERME

SMPL 64:2 64:2;

C1LCPC=0.0;
C1LYPY=0.0;
C1IK=0.0;
c1lxpx=0.0;
c1lMpM=0.0;
c1tinfl=0.0;

SMPL 64:2 2021:1;

CAPITL c1ik,0.0,sum1ik;
CAPITL c1lcpc,0.0,sum1lcpc;
CAPITL c1lypy,0.0,sum1lypy;
CAPITL c1lxpx,0.0,sum1lxpx;
CAPITL c1lMpM,0.0,sum1lMpM;
CAPITL c1tinfl,0.0,sum1tinfl;

plot(prev,titl='GR 1M MULTIPLICATEUR total CONSOMMATION CHOC +') sum1lcpc;
plot(prev,titl='GR 1M MULTIPLICATEUR total PRODUCTION CHOC +') sum1lypy;
plot(prev,titl='GR 1M MULTIPLICATEUR total INVESTISSEMENT CHOC +') sum1ik;
plot(prev,titl='GR 1M MULTIPLICATEUR total EXPORTATION CHOC +') sum1LXPX;
plot(prev,titl='GR 1M MULTIPLICATEUR total IMPORTATION CHOC +') sum1LMPM;
plot(prev,titl='GR 1M MULTIPLICATEUR total INFLATION CHOC +') sum1tinfl;

title 'MULTIPLICATEURS DE LONG TERME +';
PRINT sum1LCPC sum1LYPY sum1IK SUM1LXPX sum1LMPM sum1tinfl;

?stop;
? Calcul multiplicateurs d'intérim d'ordre j	/ MT


GENR interimconso1=	c1lcpc/5.00306 *100;
GENR interimprod1=	c1lypy/2.55303 *100;
GENR interiminv1 =	c1ik/0.00015697*100 ;
GENR interimEXP1 =	c1LXPX/0*100 ;
GENR interimIMP1 =	c1LMPM/1.22033*100;
GENR interimTINFL1 = c1tinfl/447.55212*100;

TITLE 'MULTIPLICATEUR INTERIM +';
print time interimconso1 interimprod1 interiminv1 interimEXP1 interimIMP1 interimTINFL1;

plot(prev,titl='GR 1M MULTIPLICATEUR interim % CONSOMMATION CHOC +') interimconso1;
plot(prev,titl='GR 1M MULTIPLICATEUR interim % PRODUCTION CHOC +') interimprod1;
plot(prev,titl='GR 1M MULTIPLICATEUR interim % INVESTISSEMENT CHOC +') interiminv1;
plot(prev,titl='GR 1M MULTIPLICATEUR interim % EXPORTATION CHOC +') interimEXP1;
plot(prev,titl='GR 1M MULTIPLICATEUR interim % IMPORTATION CHOC +') interimIMP1;
plot(prev,titl='GR 1M MULTIPLICATEUR interim % INFLATION CHOC +') interimTINFL1;

?GOTO 7;  ? Pour choc 7, car ce choc est dependant du choc 1

STOP;


? CHOC N°2: Baisse DU REVENU disponible DE 10% en début de période  d estimation
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

2 SMPL 64:3 64:3;
SET z=0.90;	   ? baisse de 10%
GENR rd = rd*z;
GENR Lrd = log(rd);

SMPL 1964:3 2021:1;

? SIMULATION DU MODELE AVEC CHOC

SOLVE(tag=c2,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

GENR C2LCPC = (LCPCC2 - LCPCS0)/z*100;
GENR C2LYPy = (LYPYC2 - LYPYS0)/z*100;
GENR C2IK = (IKC2 - IKS0)/z*100;
GENR c2lxpx = (LXPXC2 - LXPXS0)/z*100;
GENR c2lmpm = (LmPmC2 - LmPmS0)/z*100;
GENR c2tinfl = (tinflC2 - tinflS0)/z*100;

plot(prev,titl='GR 2 SIMULATION conso avec choc SUR rd DE - 10%')  c2lcpc ;
plot(prev,titl='GR 2 SIMULATION production avec choc SUR rd DE - 10% ')  c2lypy ;
plot(prev,titl='GR 2 SIMULATION investissement avec choc SUR rd DE - 10%')  c2ik ;
plot(prev,titl='GR 2 SIMULATION EXPORTATION avec choc SUR rd DE - 10% ')  c2lXpX ;
plot(prev,titl='GR 2 SIMULATION IMPORTATION avec choc SUR rd DE - 10% ')  c2lMpM ;
plot(prev,titl='GR 2 SIMULATION INFLATION avec choc SUR rd DE - 10% ')  c2tinfl ;

title 'choc SUR rd DE 10% -';
PRINT C2LCPC C2LYPY C2IK c2lxpx c2lMpM c2tinfl;
?Stop;

? CALCUL DES MULTIPLICATEURS DE LONG TERME

SMPL 64:2 64:2;

C2LCPC=0.0;
C2LYPY=0.0;
C2IK=0.0;
c2lxpx=0.0;
c2lMpM=0.0;
c2tinfl=0.0;

SMPL 64:2 2021:1;

CAPITL c2ik,0.0,sum2ik;
CAPITL c2lcpc,0.0,sum2lcpc;
CAPITL c2lypy,0.0,sum2lypy;
CAPITL c2lxpx,0.0,sum2lxpx;
CAPITL c2lMpM,0.0,sum2lMpM;
CAPITL c2tinfl,0.0,sum2tinfl;

plot(prev,titl='GR 2M MULTIPLICATEUR total CONSOMMATION CHOC -') sum2lcpc;
plot(prev,titl='GR 2M MULTIPLICATEUR total PRODUCTION CHOC -') sum2lypy;
plot(prev,titl='GR 2M MULTIPLICATEUR total INVESTISSEMENT CHOC -') sum2ik;
plot(prev,titl='GR 2M MULTIPLICATEUR total EXPORTATION CHOC -') sum2LXPX;
plot(prev,titl='GR 2M MULTIPLICATEUR total IMPORTATION CHOC -') sum2LMPM;
plot(prev,titl='GR 2M MULTIPLICATEUR total IMPORTATION CHOC -') sum2tinfl;

title 'MULTIPLICATEURS DE LONG TERME -';
PRINT sum2LCPC sum2LYPY sum2IK SUM2LXPX sum2LMPM sum2tinfl;

?stop;

? Calcul multiplicateurs d'intérim d'ordre j	/ MT


GENR interimconso2=	c2lcpc/-6.75975 *100;
GENR interimprod2=	c2lypy/-3.44955 *100;
GENR interiminv2 =	c2ik/-0.00021120*100 ;
GENR interimEXP2 =	c2LXPX/0*100 ;
GENR interimIMP2 =	c2LMPM/-1.64882*100 ;
GENR interimTINFL2 = c2tinfl/-604.68945*100 ;
 
TITLE 'MULTIPLICATEUR INTERIM +';
print time interimconso2 interimprod2 interiminv2 interimEXP2 interimIMP2 interimTINFL2;

plot(prev,titl='GR 2M MULTIPLICATEUR interim % CONSOMMATION CHOC -') interimconso2;
plot(prev,titl='GR 2M MULTIPLICATEUR interim % PRODUCTION CHOC -') interimprod2;
plot(prev,titl='GR 2M MULTIPLICATEUR interim % INVESTISSEMENT CHOC -') interiminv2;
plot(prev,titl='GR 2M MULTIPLICATEUR interim % EXPORTATION CHOC -') interimEXP2;
plot(prev,titl='GR 2M MULTIPLICATEUR interim % IMPORTATION CHOC -') interimIMP2;
plot(prev,titl='GR 2M MULTIPLICATEUR interim % INFLATION CHOC -') interimTINFL2;

STOP;



? CHOC N°3: AUGMENTATION DES DEPENSES GOUVERNEMENTALES DE 10% en début de période d estimation
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

3 SMPL 64:3 64:3;
SET Z=1.10;	   ? augmentation de 10%
GENR gpg = gpg*z;
GENR Lgpg = log(gpg);
print gpg;
SMPL 1964:3 2021:1;

? SIMULATION DU MODELE AVEC CHOC

SOLVE(tag=c3,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

GENR C3LCPC = (LCPCC3 - LCPCS0)/z*100;
GENR C3LYPy = (LYPYC3 - LYPYS0)/z*100;
GENR C3IK = (IKC3 - IKS0)/z*100;
GENR c3lxpx = (LXPXC3 - LXPXS0)/z*100;
GENR c3lmpm = (LmpmC3 - LmpmS0)/z*100;
GENR c3tinfl = (tinflC3 - tinflS0)/z*100;

plot(prev,titl='GR 3 SIMULATION conso avec choc SUR gpg DE + 10%')  c3lcpc ;
plot(prev,titl='GR 3 SIMULATION production avec choc SUR gpg DE + 10% ')  c3lypy ;
plot(prev,titl='GR 3 SIMULATION investissement avec choc SUR gpg DE + 10%')  c3ik ;
plot(prev,titl='GR 3 SIMULATION EXPORTATION avec choc SUR gpg + 10% ')  c3lXpX ;
plot(prev,titl='GR 3 SIMULATION IMPORTATION avec choc SUR gpg + 10% ')  c3lMpM ;
plot(prev,titl='GR 3 SIMULATION INFLATION avec choc SUR gpg + 10% ')  c3tinfl ;


title 'choc SUR gpg DE +10%';
PRINT C3LCPC C3LYPY C3IK c3lxpx c3lMpM c3tinfl;

?stop;

? CALCUL DES MULTIPLICATEURS DE LONG TERME

SMPL 64:2 64:2;

C3LCPC=0.0;
C3LYPY=0.0;
C3IK=0.0;
c3lxpx=0.0;
c3lMpM=0.0;
c3tinfl=0.0;

SMPL 64:2 2021:1;

CAPITL c3ik,0.0,sum3ik;
CAPITL c3lcpc,0.0,sum3lcpc;
CAPITL c3lypy,0.0,sum3lypy;
CAPITL c3lxpx,0.0,sum3lxpx;
CAPITL c3lMpM,0.0,sum3lMpM;
CAPITL c3tinfl,0.0,sum3tinfl;

plot(prev,titl='GR 3M MULTIPLICATEUR total CONSOMMATION CHOC -') sum3lcpc;
plot(prev,titl='GR 3M MULTIPLICATEUR total PRODUCTION CHOC -') sum3lypy;
plot(prev,titl='GR 3M MULTIPLICATEUR total INVESTISSEMENT CHOC -') sum3ik;
plot(prev,titl='GR 3M MULTIPLICATEUR total EXPORTATION CHOC -') sum3LXPX;
plot(prev,titl='GR 3M MULTIPLICATEUR total IMPORTATION CHOC -') sum3LMPM;
plot(prev,titl='GR 3M MULTIPLICATEUR total INFLATION CHOC -') sum3tinfl;

title 'MULTIPLICATEURS DE LONG TERME -';
PRINT sum3LCPC sum3LYPY sum3IK SUM3LXPX sum3LMPM sum3tinfl;

?stop;

? Calcul multiplicateurs d'intérim d'ordre j	/ MT


GENR interimconso3 = c3lcpc/-6.67853 *100;
GENR interimprod3 =	c3lypy/-3.40213 *100;
GENR interiminv3 =	c3ik/-0.00019237*100 ;
GENR interimEXP3 =	c3LXPX/0*100 ;
GENR interimIMP3 =	c3LMPM/-1.63346*100 ;
GENR interimTINFL3 = c3tinfl/-1227.94995*100 ;
 
TITLE 'MULTIPLICATEUR INTERIM +';
print time interimconso3 interimprod3 interiminv3 interimEXP3 interimIMP3 interimTINFL3;

plot(prev,titl='GR 3M MULTIPLICATEUR interim % CONSOMMATION CHOC -') interimconso3;
plot(prev,titl='GR 3M MULTIPLICATEUR interim % PRODUCTION CHOC -') interimprod3;
plot(prev,titl='GR 3M MULTIPLICATEUR interim % INVESTISSEMENT CHOC -') interiminv3;
plot(prev,titl='GR 3M MULTIPLICATEUR interim % EXPORTATION CHOC -') interimEXP3;
plot(prev,titl='GR 3M MULTIPLICATEUR interim % IMPORTATION CHOC -') interimIMP3;
plot(prev,titl='GR 3M MULTIPLICATEUR interim % INFLATION CHOC -') interimTINFL3;

STOP;



? CHOC N°4: DIMINUTION DES DEPENSES GOUVERNEMENTALES DE 10% en début de période  d estimation
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

4 SMPL 64:3 64:3;
SET Z=0.90;	   ? augmentation de 10%
GENR gpg = gpg*z;
GENR Lgpg = log(gpg);
print gpg;
SMPL 1964:3 2021:1;

? SIMULATION DU MODELE AVEC CHOC

SOLVE(tag=c4,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

GENR C4LCPC = (LCPCC4 - LCPCS0)/z*100;
GENR C4LYPy = (LYPYC4 - LYPYS0)/z*100;
GENR C4IK = (IKC4 - IKS0)/z*100;
GENR C4lxpx = (LXPXC4 - LXPXS0)/z*100;
GENR C4lmpm = (LmpmC4 - LmpmS0)/z*100;
GENR C4tinfl = (tinflC4 - tinflS0)/z*100;

plot(prev,titl='GR 4 SIMULATION conso avec choc SUR gpg DE - 10%')  C4lcpc ;
plot(prev,titl='GR 4 SIMULATION production avec choc SUR gpg DE - 10% ')  C4lypy ;
plot(prev,titl='GR 4 SIMULATION investissement avec choc SUR gpg DE - 10%')  C4ik ;
plot(prev,titl='GR 4 SIMULATION EXPORTATION avec choc SUR gpg - 10% ')  C4lXpX ;
plot(prev,titl='GR 4 SIMULATION IMPORTATION avec choc SUR gpg - 10% ')  C4lMpM ;
plot(prev,titl='GR 4 SIMULATION INFLATION avec choc SUR gpg - 10% ')  C4tinfl ;


title 'choc SUR gpg DE -10%';
PRINT C4LCPC C4LYPY C4IK C4lxpx C4lMpM C4tinfl;

?stop;

? CALCUL DES MULTIPLICATEURS DE LONG TERME

SMPL 64:2 64:2;

C4LCPC=0.0;
C4LYPY=0.0;
C4IK=0.0;
C4lxpx=0.0;
C4lMpM=0.0;
C4tinfl=0.0;

SMPL 64:2 2021:1;

CAPITL C4ik,0.0,sum4ik;
CAPITL C4lcpc,0.0,sum4lcpc;
CAPITL C4lypy,0.0,sum4lypy;
CAPITL C4lxpx,0.0,sum4lxpx;
CAPITL C4lMpM,0.0,sum4lMpM;
CAPITL C4tinfl,0.0,sum4tinfl;

plot(prev,titl='GR 4M MULTIPLICATEUR total CONSOMMATION CHOC -') sum4lcpc;
plot(prev,titl='GR 4M MULTIPLICATEUR total PRODUCTION CHOC -') sum4lypy;
plot(prev,titl='GR 4M MULTIPLICATEUR total INVESTISSEMENT CHOC -') sum4ik;
plot(prev,titl='GR 4M MULTIPLICATEUR total EXPORTATION CHOC -') sum4LXPX;
plot(prev,titl='GR 4M MULTIPLICATEUR total IMPORTATION CHOC -') sum4LMPM;
plot(prev,titl='GR 4M MULTIPLICATEUR total INFLATION CHOC -') sum4tinfl;

title 'MULTIPLICATEURS DE LONG TERME -';
PRINT sum4LCPC sum4LYPY sum4IK SUM4LXPX sum4LMPM sum4tinfl;

?stop;

? Calcul multiplicateurs d'intérim d'ordre j	/ MT

GENR interimconso4 = C4lcpc/0 *100;
GENR interimprod4 =	C4lypy/-1.16528 *100;
GENR interiminv4 =	C4ik/-0.000068711*100 ;
GENR interimEXP4 =	C4LXPX/0*100 ;
GENR interimIMP4 =	C4LMPM/-0.55692*100 ;
GENR interimTINFL4 = C4tinfl/-275.79337 *100 ;
 
TITLE 'MULTIPLICATEUR INTERIM +';
print time interimconso4 interimprod4 interiminv4 interimEXP4 interimIMP4 interimTINFL4;

plot(prev,titl='GR 4M MULTIPLICATEUR interim % CONSOMMATION CHOC -') interimconso4;
plot(prev,titl='GR 4M MULTIPLICATEUR interim % PRODUCTION CHOC -') interimprod4;
plot(prev,titl='GR 4M MULTIPLICATEUR interim % INVESTISSEMENT CHOC -') interiminv4;
plot(prev,titl='GR 4M MULTIPLICATEUR interim % EXPORTATION CHOC -') interimEXP4;
plot(prev,titl='GR 4M MULTIPLICATEUR interim % IMPORTATION CHOC -') interimIMP4;
plot(prev,titl='GR 4M MULTIPLICATEUR interim % INFLATION CHOC -') interimTINFL4;

STOP;


? CHOC N°5: AUGMENTATION DU TAUX DE CHOMAGE DE 10% en début de période d estimation
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

5 SMPL 64:3 64:3;
SET Z=1.10;	   ? augmentation de 10%
GENR tcho = tcho*z;
print tcho;
SMPL 1964:3 2021:1;

? SIMULATION DU MODELE AVEC CHOC

SOLVE(tag=C5,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

GENR C5LCPC = (LCPCC5 - LCPCS0)/z*100;
GENR C5LYPy = (LYPYC5 - LYPYS0)/z*100;
GENR C5IK = (IKC5 - IKS0)/z*100;
GENR C5lxpx = (LXPXC5 - LXPXS0)/z*100;
GENR C5lmpm = (LmpmC5 - LmpmS0)/z*100;
GENR C5tinfl = (tinflC5 - tinflS0)/z*100;

plot(prev,titl='GR 5 SIMULATION conso avec choc SUR tcho DE + 10%')  C5lcpc ;
plot(prev,titl='GR 5 SIMULATION production avec choc SUR tcho DE + 10% ')  C5lypy ;
plot(prev,titl='GR 5 SIMULATION investissement avec choc SUR tcho DE + 10%')  C5ik ;
plot(prev,titl='GR 5 SIMULATION EXPORTATION avec choc SUR tcho + 10% ')  C5lXpX ;
plot(prev,titl='GR 5 SIMULATION IMPORTATION avec choc SUR tcho + 10% ')  C5lMpM ;
plot(prev,titl='GR 5 SIMULATION INFLATION avec choc SUR tcho + 10% ')  C5tinfl ;


title 'choc SUR tcho DE +10%';
PRINT C5LCPC C5LYPY C5IK C5lxpx C5lMpM C5tinfl;

?stop;

? CALCUL DES MULTIPLICATEURS DE LONG TERME

SMPL 64:2 64:2;

C5LCPC=0.0;
C5LYPY=0.0;
C5IK=0.0;
C5lxpx=0.0;
C5lMpM=0.0;
C5tinfl=0.0;

SMPL 64:2 2021:1;

CAPITL C5ik,0.0,sum5ik;
CAPITL C5lcpc,0.0,sum5lcpc;
CAPITL C5lypy,0.0,sum5lypy;
CAPITL C5lxpx,0.0,sum5lxpx;
CAPITL C5lMpM,0.0,sum5lMpM;
CAPITL C5tinfl,0.0,sum5tinfl;

plot(prev,titl='GR 5M MULTIPLICATEUR total CONSOMMATION CHOC +') sum5lcpc;
plot(prev,titl='GR 5M MULTIPLICATEUR total PRODUCTION CHOC +') sum5lypy;
plot(prev,titl='GR 5M MULTIPLICATEUR total INVESTISSEMENT CHOC +') sum5ik;
plot(prev,titl='GR 5M MULTIPLICATEUR total EXPORTATION CHOC +') sum5LXPX;
plot(prev,titl='GR 5M MULTIPLICATEUR total IMPORTATION CHOC +') sum5LMPM;
plot(prev,titl='GR 5M MULTIPLICATEUR total INFLATION CHOC +') sum5tinfl;

title 'MULTIPLICATEURS DE LONG TERME +';
PRINT sum5LCPC sum5LYPY sum5IK sum5LXPX sum5LMPM sum5tinfl;

?stop;

? Calcul multiplicateurs d'intérim d'ordre j	/ MT


GENR interimconso5 = C5lcpc/-0.037887 *100;
GENR interimprod5 =	C5lypy/-0.019464 *100;
GENR interiminv5 =	C5ik/-7.61991D-07*100 ;		
GENR interimEXP5 =	C5LXPX/0*100 ;
GENR interimIMP5 =	C5LMPM/-0.0092333*100 ;
GENR interimTINFL5 = C5tinfl/-35.20287*100 ;
 
TITLE 'MULTIPLICATEUR INTERIM +';
print time interimconso5 interimprod5 interiminv5 interimEXP5 interimIMP5 interimTINFL5;

plot(prev,titl='GR 5M MULTIPLICATEUR interim % CONSOMMATION CHOC +') interimconso5;
plot(prev,titl='GR 5M MULTIPLICATEUR interim % PRODUCTION CHOC +') interimprod5;
plot(prev,titl='GR 5M MULTIPLICATEUR interim % INVESTISSEMENT CHOC +') interiminv5;
plot(prev,titl='GR 5M MULTIPLICATEUR interim % EXPORTATION CHOC +') interimEXP5;
plot(prev,titl='GR 5M MULTIPLICATEUR interim % IMPORTATION CHOC +') interimIMP5;
plot(prev,titl='GR 5M MULTIPLICATEUR interim % INFLATION CHOC +') interimTINFL5;

STOP;


? CHOC N°6: DIMINUTION DU TAUX DE CHOMAGE DE 10% en début de période d estimation
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

6 SMPL 64:3 64:3;
SET Z=0.90;	   ? diminution de 10%
GENR tcho = tcho*z;
print tcho;
SMPL 1964:3 2021:1;

? SIMULATION DU MODELE AVEC CHOC

SOLVE(tag=C6,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

GENR C6LCPC = (LCPCC6 - LCPCS0)/z*100;
GENR C6LYPy = (LYPYC6 - LYPYS0)/z*100;
GENR C6IK = (IKC6 - IKS0)/z*100;
GENR C6lxpx = (LXPXC6 - LXPXS0)/z*100;
GENR C6lmpm = (LmpmC6 - LmpmS0)/z*100;
GENR C6tinfl = (tinflC6 - tinflS0)/z*100;

plot(prev,titl='GR 6 SIMULATION conso avec choc SUR tcho DE - 10%')  C6lcpc ;
plot(prev,titl='GR 6 SIMULATION production avec choc SUR tcho DE - 10% ')  C6lypy ;
plot(prev,titl='GR 6 SIMULATION investissement avec choc SUR tcho DE - 10%')  C6ik ;
plot(prev,titl='GR 6 SIMULATION EXPORTATION avec choc SUR tcho - 10% ')  C6lXpX ;
plot(prev,titl='GR 6 SIMULATION IMPORTATION avec choc SUR tcho - 10% ')  C6lMpM ;
plot(prev,titl='GR 6 SIMULATION INFLATION avec choc SUR tcho - 10% ')  C6tinfl ;


title 'choc SUR tcho DE -10%';
PRINT C6LCPC C6LYPY C6IK C6lxpx C6lMpM C6tinfl;

?stop;

? CALCUL DES MULTIPLICATEURS DE LONG TERME

SMPL 64:2 64:2;

C6LCPC=0.0;
C6LYPY=0.0;
C6IK=0.0;
C6lxpx=0.0;
C6lMpM=0.0;
C6tinfl=0.0;

SMPL 64:2 2021:1;

CAPITL C6ik,0.0,sum6ik;
CAPITL C6lcpc,0.0,sum6lcpc;
CAPITL C6lypy,0.0,sum6lypy;
CAPITL C6lxpx,0.0,sum6lxpx;
CAPITL C6lMpM,0.0,sum6lMpM;
CAPITL C6tinfl,0.0,sum6tinfl;

plot(prev,titl='GR 6M MULTIPLICATEUR total CONSOMMATION CHOC -') sum6lcpc;
plot(prev,titl='GR 6M MULTIPLICATEUR total PRODUCTION CHOC -') sum6lypy;
plot(prev,titl='GR 6M MULTIPLICATEUR total INVESTISSEMENT CHOC -') sum6ik;
plot(prev,titl='GR 6M MULTIPLICATEUR total EXPORTATION CHOC -') sum6LXPX;
plot(prev,titl='GR 6M MULTIPLICATEUR total IMPORTATION CHOC -') sum6LMPM;
plot(prev,titl='GR 6M MULTIPLICATEUR total INFLATION CHOC -') sum6tinfl;

title 'MULTIPLICATEURS DE LONG TERME -';
PRINT sum6LCPC sum6LYPY sum6IK sum6LXPX sum6LMPM sum6tinfl;

?stop;

? Calcul multiplicateurs d'intérim d'ordre j	/ MT

GENR interimconso6 = C6lcpc/0.046147 *100;
GENR interimprod6 =	C6lypy/0.023471 *100;
GENR interiminv6 =	C6ik/1.44872D-06*100 ;
GENR interimEXP6 =	C6LXPX/0*100 ;
GENR interimIMP6 =	C6LMPM/0.011312*100 ;
GENR interimTINFL6 = C6tinfl/43.02560 *100 ;
 
TITLE 'MULTIPLICATEUR INTERIM +';
print time interimconso6 interimprod6 interiminv6 interimEXP6 interimIMP6 interimTINFL6;

plot(prev,titl='GR 6M MULTIPLICATEUR interim % CONSOMMATION CHOC -') interimconso6;
plot(prev,titl='GR 6M MULTIPLICATEUR interim % PRODUCTION CHOC -') interimprod6;
plot(prev,titl='GR 6M MULTIPLICATEUR interim % INVESTISSEMENT CHOC -') interiminv6;
plot(prev,titl='GR 6M MULTIPLICATEUR interim % EXPORTATION CHOC -') interimEXP6;
plot(prev,titl='GR 6M MULTIPLICATEUR interim % IMPORTATION CHOC -') interimIMP6;
plot(prev,titl='GR 6M MULTIPLICATEUR interim % INFLATION CHOC -') interimTINFL6;

STOP;


?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
? CHOC N°7: AUGMENTATION DU REVENU DE 10% ET BAISSE DE GPG DE 10% en début de période
?XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

7 SMPL 64:3 64:3;
SET z=1.10;	   ? augmentation de 10% SUR RD
GENR rd = rd*z;
GENR Lrd = log(rd);
SET z1=0.90;	 ? baisse de 10% SUR GPG
GENR gpg= gpg*z1;
GENR lgpg=log(gpg);

SMPL 1964:3 2021:1;

? SIMULATION DU MODELE AVEC CHOC

SOLVE(tag=C7,tol=.0001,maxit=70,method=gauss,dynam,noprint) fin;

? print lcpcc1 lcpcc2 lcpcc3 lcpcC7 lcpcs0;

GENR C7LCPC = (LCPCC7 - LCPCc1)/z1*100;
GENR C7LYPy = (LYPYC7 - lypyc1)/z1*100;
GENR C7IK = (IKC7 -  IKc1)/z1*100;
GENR C7lxpx = (LXPXC7 - LXPXC1)/z*100;
GENR C7lmpm = (LmPmC7 - LmPmC1)/z*100;
GENR C7tinfl = (tinflC7 - tinflC1)/z*100;


plot(prev,titl='GR 7 SIMULATION conso avec choc SUR rd de + 10% ET GPG DE - 10%')  C7lcpc ;
plot(prev,titl='GR 7 SIMULATION production avec choc SUR rd de + 10% ET GPG DE - 10% ')  C7lypy ;
plot(prev,titl='GR 7 SIMULATION investissement avec choc SUR rd de + 10% ET GPG DE - 10%')  C7ik ;
plot(prev,titl='GR 7 SIMULATION EXPORTATION avec choc SUR rd DE + 10% ET GPG DE - 10%')  C7lXpX ;
plot(prev,titl='GR 7 SIMULATION IMPORTATION avec choc SUR rd DE + 10% ET GPG DE - 10%')  C7lMpM ;
plot(prev,titl='GR 7 SIMULATION INFLATION avec choc SUR rd DE + 10% ET GPG DE - 10%')  C7tinfl ;

title 'choc SUR rd + 10% ET gpg DE - 10%';
PRINT C7LCPC C7LYPY C7IK  C7lXpX  C7lMpM C7tinfl;


? CALCUL DES MULTIPLICATEURS DE LONG TERME

SMPL 64:2 64:2;

C7LCPC=0.0;
C7LYPY=0.0;
C7IK=0.0;
C7lxpx=0.0;
C7lMpM=0.0;
C7tinfl=0.0;

SMPL 64:2 2021:1;

CAPITL C7ik,0.0,sum7ik;
CAPITL C7lcpc,0.0,sum7lcpc;
CAPITL C7lypy,0.0,sum7lypy;
CAPITL C7lxpx,0.0,sum7lxpx;
CAPITL C7lMPM,0.0,sum7lMPM;
CAPITL C7tinfl,0.0,sum7tinfl;

plot(prev,titl='GR 7M MULTIPLICATEUR total CONSOMMATION CHOC + -') sum7lcpc;
plot(prev,titl='GR 7M MULTIPLICATEUR total PRODUCTION CHOC + -') sum7lypy;
plot(prev,titl='GR 7M MULTIPLICATEUR total INVESTISSEMENT CHOC + -') sum7ik;
plot(prev,titl='GR 7M MULTIPLICATEUR total EXPORTATION CHOC + -') sum7LXPX;
plot(prev,titl='GR 7M MULTIPLICATEUR total IMPORTATION CHOC + -') sum7LMPM;
plot(prev,titl='GR 7M MULTIPLICATEUR total INFLATION CHOC + -') sum7tinfl;

title 'MULTIPLICATEURS DE LONG TERME + -';
PRINT sum7LCPC sum7LYPY sum7IK sum7LXPX sum7LMPM sum7tinfl;

?stop;

? Calcul multiplicateurs d'intérim d'ordre j	/ MT


GENR interimconso7 = C7lcpc/6.11501 *100;
GENR interimprod7 =	C7lypy/1.95546 *100;
GENR interiminv7 =	C7ik/0.00012273*100 ;
GENR interimEXP7 =	C7LXPX/0*100 ;
GENR interimIMP7 =	C7LMPM/0.76478*100 ;
GENR interimtinfl7 = C7tinfl/221.90092*100 ;

TITLE 'MULTIPLICATEUR INTERIM + -';
print time interimconso7 interimprod7 interiminv7 interimEXP7 interimIMP7 interimtinfl7;

plot(prev,titl='GR 7M MULTIPLICATEUR interim % CONSOMMATION CHOC + -') interimconso7;
plot(prev,titl='GR 7M MULTIPLICATEUR interim % PRODUCTION CHOC + -') interimprod7;
plot(prev,titl='GR 7M MULTIPLICATEUR interim % INVESTISSEMENT CHOC + -') interiminv7;
plot(prev,titl='GR 7M MULTIPLICATEUR interim % EXPORTATION CHOC + -') interimEXP7;
plot(prev,titl='GR 7M MULTIPLICATEUR interim % IMPORTATION CHOC + -') interimIMP7;
plot(prev,titl='GR 7M MULTIPLICATEUR interim % INFLATION CHOC + -') interimtinfl7;

END;