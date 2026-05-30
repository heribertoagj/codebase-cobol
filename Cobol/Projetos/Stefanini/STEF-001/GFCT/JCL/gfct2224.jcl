//GFCT2224 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    *---------------------------------------------------------*
//* ***    *  CLIENTES SEM CESTA - PREPARACAO CARGA GFCTB0N9
//* ***    *---------------------------------------------------------*
//*
//STEP1    EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  CONCATENA DESPREZADOS E FINAL PARA CARGA
//* ***    *---------------------------------------------------------*
//*
//SORTIN   DD DSN=MX.GFCT.J2222S01.DESPREZA(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.J2222S02.DESPREZA(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.J2222S03.DESPREZA(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.J2222S04.DESPREZA(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.J2222S05.DESPREZA(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.J2222S07.DESPREZA(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.J2222S10.DESPREZA(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.J2222S12.ADESAOSC(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2224S01.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,50),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=COPY
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
