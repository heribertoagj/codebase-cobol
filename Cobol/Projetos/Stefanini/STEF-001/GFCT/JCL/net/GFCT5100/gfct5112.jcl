//GFCT5112 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC SORTD
//*
//* ***    ----------------------------------------------------
//* ***    SORT GFCTWADS - AGENCIA, CONTA
//* ***    ----------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.PRV.EXTPREVI(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXPRWADS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=040,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//STEP2    EXEC PGM=GFCT2646
//*
//* ***    ------------------------------------------------------------
//* ***    GERA LAYOUT UTILIZADO NA PREVIA DE EMISSAO.
//* ***    ------------------------------------------------------------
//*
//EXPRWADS DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//EXPRWAET DD DSN=MX.GFCT.EXPRWAET(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC PGM=PLAN0260
//*
//* ***    ------------------------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    ------------------------------------------
//*
//LEITURA  DD DSN=MX.GFCT.IDTFEMIS(0),
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC SORTD,
//       COND=(4,NE,STEP3)
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWADS - AGENCIA, CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.IDTPERAG(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.IDTPERAG.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=070,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(7,5,ZD,A,5,2,ZD,A),FORMAT=BI
 END
//*
//STEP5    EXEC PGM=GFCT2692,
//       COND=(4,NE,STEP3)
//*
//* ***    ------------------------------------------------------------
//* ***    IDENTIFICA AGENCIA ESCOLHIDA PARA PREVIA.
//* ***    ------------------------------------------------------------
//*
//EXMSTPRV DD DSN=*.STEP2.EXPRWAET,
//       DISP=SHR
//IDTPERAG DD DSN=*.STEP4.SORT.SORTOUT,
//       DISP=SHR
//EXMSTPV1 DD DSN=MX.GFCT.EXMSTPVI(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP6    EXEC SORTD,
//       COND=(4,NE,STEP3)
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAET - AGENCIA, CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP5.EXMSTPV1,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXTPREVI.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//STEP7    EXEC SORTD,
//       COND=(0,NE,STEP3)
//*
//* ***    --------------------------------------------------
//* ***    CLASSIFICA POR AGENCIA / CONTA
//* ***    --------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP2.EXPRWAET,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXTPREVI.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=COPY
 END
//*
