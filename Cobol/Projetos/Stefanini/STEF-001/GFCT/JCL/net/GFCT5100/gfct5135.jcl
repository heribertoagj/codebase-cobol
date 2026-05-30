//GFCT5135 JOB 'GFCT,4220,PR14','B142376',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA CLIENTES BANCO POSTAL
//* ***    ------------------------------------------------------------
//*
//STEP1    EXEC PGM=PLAN0260
//*
//* ***    ------------------------------------------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    ------------------------------------------------------------
//*
//LEITURA  DD DSN=MX.GFCT.EMIBPOST(0),
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
//SORTIN   DD DSN=MX.GFCT.EXMSTCL4(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL5(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVO DE PARAMETROS: BANCO POSTAL
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
//SORTIN   DD DSN=MX.GFCT.EXMSTCL4(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL4.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(22,5,ZD,A,5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVO DE PARAMETROS: BANCO POSTAL
//* ***    ------------------------------------------------------------
//*
//*
//STEP4    EXEC PGM=GFCT2664,
//       PARM=0,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    IDENTIFICA NO MESTRE DE CLIENTES QUEM EH AGRUP.
//* ***    BANCO POSTAL.
//* ***    ------------------------------------------------------------
//*
//EXMSTCLI DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//EXSEMPAB DD DSN=MX.GFCT.EXSEPOST(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//EXCLIPAB DD DSN=MX.GFCT.EXMCLIBP(+1),
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
//EMITBPBE DD DSN=MX.GFCT.EMIBPOST(0),
//       DISP=SHR
//EMPABNOK DD DSN=MX.GFCT.EMCBPNOK(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//EXMPABOK DD DSN=MX.GFCT.EXMCBPOK(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//* ***    ------------------------------------------------------------
//* ***    PREPARA ARQUIVOS PARA PROXIMO TRATAMENTO DE PARAMETROS:
//* ***    BRADESCO EXPRESSO.
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
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL5(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
