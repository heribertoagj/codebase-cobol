//GFCT5150 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    --------------------------
//* ***    TRATA CLIENTES SEGMENTOS
//* ***    --------------------------
//*
//STEP1    EXEC PGM=PLAN0260
//*
//* ***    -------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    -------------------------
//*
//LEITURA  DD DSN=MX.GFCT.EMISEGTO(0),
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAET - SEGMENTO GFCT / AGENCIA / CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXMSTCL6(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMCSEGT.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(27,5,ZD,A,5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//STEP3    EXEC PGM=GFCT2680,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    OBTER DADOS DO SEGMENTO GFCT A PARTIR DA AGENCIA E CONTA.
//* ***    ------------------------------------------------------------
//*
//EXMCSEGT DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//EMISEGTO DD DSN=MX.GFCT.EMISEGTO(0),
//       DISP=SHR
//EXMSGNOK DD DSN=MX.GFCT.EXCSGNOK(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//EXMCSGOK DD DSN=MX.GFCT.EXMCSGOK(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
