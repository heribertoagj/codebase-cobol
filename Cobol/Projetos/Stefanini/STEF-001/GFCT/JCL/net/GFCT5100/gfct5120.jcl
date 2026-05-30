//GFCT5120 JOB 'GFCT,4220,PR14','D336356',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC PGM=GFCT2652
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVO DE CANCELADOS.
//* ***    ------------------------------------------------------------
//*
//EXMSTCLI DD DSN=MX.GFCT.EXMSTCLI(0),
//       DISP=SHR
//CANCECLA DD DSN=MX.GFCT.CANCELBX(0),
//       DISP=SHR
//EXTCANOK DD DSN=MX.GFCT.EXTCANOK(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//EXMSTCLO DD DSN=MX.GFCT.EXMSTCL1(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC PGM=GFCT2654
//*
//* ***    ------------------------------------------------------------
//* ***    OBTEM PAB E VALOR DA RENDA DO CLIENTE.
//* ***    ------------------------------------------------------------
//*
//EXMSTCLI DD DSN=*.STEP1.EXMSTCLO,
//       DISP=SHR
//EXMSTC1A DD DSN=MX.GFCT.EXMSTC1A(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0100,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC SORTD
//*
//* ***    -----------------------------------------------
//* ***    CLASSIFICA POR CID-CLI
//* ***    -----------------------------------------------
//*
//SORTIN   DD DSN=*.STEP2.EXMSTC1A,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTC1A.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0100,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(60,26,A),FORMAT=BI
 END
//*
//STEP4    EXEC PGM=GFCT2655
//*
//* ***    ------------------------------------------------------------
//* ***    OBTEM PAB E VALOR DA RENDA DO CLIENTE.
//* ***    ------------------------------------------------------------
//*
//EXMSTC1A DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//EXMPREND DD DSN=MX.GFCT.EXMSTC1B(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC SORTD
//*
//* ***    -----------------------------------------------
//* ***    CLASSIFICA POR AGENCIA / CONTA
//* ***    -----------------------------------------------
//*
//SORTIN   DD DSN=*.STEP4.EXMPREND,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(60,3,ZD,A),FORMAT=BI
 END
//*
//* ***    ----------------------------------------
//* ***    TRATA ARQUIVO DE PARAMETROS: SEGMENTOS
//* ***    ----------------------------------------
//*
//*
//STEP6    EXEC PGM=GFCT2676
//*
//* ***    ------------------------------------------------------------
//* ***    OBTER DADOS DO SEGMENTO GFCT A PARTIR DA AGENCIA E CONTA.
//* ***    ------------------------------------------------------------
//*
//EXMSTCLI DD DSN=*.STEP5.SORT.SORTOUT,
//       DISP=SHR
//EXMCSEGT DD DSN=MX.GFCT.EXMSTC1D(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
