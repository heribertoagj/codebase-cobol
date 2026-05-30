//GFCT2216 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
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
//* ***    *------------------------------------------------*
//* ***    *  PREPARA ARQUIVO RENTABILIDADE
//* ***    *------------------------------------------------*
//*
//SORTIN01 DD DSN=MX.GFCT.PRV.ARQURENC(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2216S01.ARQURENC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(065500,13107),RLSE),
//       DCB=(MX.A,LRECL=0080,RECFM=FB)
//SYSIN    DD *
 INCLUDE COND=(62,4,ZD,EQ,0)
 MERGE FIELDS=(53,9,A),FORMAT=BI
 SUM FIELDS=NONE
 END
//*
