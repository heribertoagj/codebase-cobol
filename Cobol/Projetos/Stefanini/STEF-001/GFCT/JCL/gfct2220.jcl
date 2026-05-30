//GFCT2220 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    *---------------------------------------------------------*
//* ***    *  CLIENTES SEM CESTA - PREPARACAO CARGA GFCTB0N9
//* ***    *---------------------------------------------------------*
//*
//STEP1    EXEC PGM=GFCT4436
//*
//* ***    *---------------------------------------------------------*
//* ***    *  PREPARA CADU INFORMACOES DE TELEFONE
//* ***    *---------------------------------------------------------*
//*
//CADUFONI DD DSN=MX.PSDC.PRV.CADUB006(0),
//       DISP=SHR
//CADUFONO DD DSN=MX.GFCT.J2220S01.INFOFONE(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,18),
//       SPACE=(TRK,(065500,13107),RLSE),
//       DCB=(MX.A,LRECL=36,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  PREPARA CADU INFORMACOES DE TELEFONE
//* ***    *---------------------------------------------------------*
//*
//SORTIN   DD DSN=*.STEP1.CADUFONO,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2220S02.INFOFONE(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,18),
//       SPACE=(TRK,(065500,13107),RLSE),
//       DCB=(MX.A,LRECL=36,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,7,A),FORMAT=BI
 END
//*
//STEP3    EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  PREPARA CADU INFORMACOES DE TELEFONE
//* ***    *---------------------------------------------------------*
//*
//SORTIN01 DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=OLD
//SORTOUT  DD DSN=MX.GFCT.J2220S03.INFOFONE(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,18),
//       SPACE=(TRK,(065500,13107),RLSE),
//       DCB=(MX.A,LRECL=0036,RECFM=FB)
//SYSIN    DD *
 MERGE FIELDS=(1,6,A),FORMAT=BI
 SUM FIELDS=NONE
 END
//*
