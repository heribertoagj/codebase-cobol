//GFCT5140 JOB 'GFCT,4220,PR14','B142376',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA CLIENTES BRADESCO EXPRESSO
//* ***    ------------------------------------------------------------
//*
//STEP1    EXEC PGM=PLAN0260
//*
//* ***    ------------------------------------------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    ------------------------------------------------------------
//*
//LEITURA  DD DSN=MX.GFCT.EMIBEXPR(0),
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD,
//       COND=(4,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAET - AGENCIA / CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXMSTCL5(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL6(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVO DE PARAMETROS: BRADESCO EXPRESSO
//* ***    ------------------------------------------------------------
//*
//*
//STEP3    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAET - PAB / AGENCIA / CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXMSTCL5(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL5.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(22,5,ZD,A,5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVO DE PARAMETROS: BRADESCO EXPRESSO
//* ***    ------------------------------------------------------------
//*
//*
//STEP4    EXEC PGM=GFCT2664,
//       PARM=1,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    IDENTIFICA NO MESTRE DE CLIENTES QUEM EH AGRUP.
//* ***    BRADESCO EXPRESSO.
//* ***    ------------------------------------------------------------
//*
//EXMSTCLI DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//EXSEMPAB DD DSN=MX.GFCT.EXSEBEXP(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//EXCLIPAB DD DSN=MX.GFCT.EXMCLIBE(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC PGM=GFCT2672,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    EXCLUI CLIENTES QUE NAO POSSUEM RENTABILIDADE OU RENDA,
//* ***    COMPATIVEL COM PARAMETROS.
//* ***    ------------------------------------------------------------
//*
//EXCLIPAB DD DSN=*.STEP4.EXCLIPAB,
//       DISP=SHR
//EMITBPBE DD DSN=MX.GFCT.EMIBEXPR(0),
//       DISP=SHR
//EMPABNOK DD DSN=MX.GFCT.EMCBENOK(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//EXMPABOK DD DSN=MX.GFCT.EXMCBEOK(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//* ***    ------------------------------------------------------------
//* ***    PREPARA ARQUIVOS PARA PROXIMO TRATAMENTO DE PARAMETROS:
//* ***    SEGMENTOS.
//* ***    ------------------------------------------------------------
//*
//*
//STEP6    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAET - AGENCIA E CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP4.EXSEMPAB,
//       DISP=SHR
//         DD DSN=*.STEP5.EMPABNOK,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL6(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
