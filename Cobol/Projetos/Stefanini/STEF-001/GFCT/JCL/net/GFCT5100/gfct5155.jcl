//GFCT5155 JOB 'GFCT,4220,PR14','D336356',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVOS IDENTIFICADOS PARA PREVIA DE EMISSAO DE
//* ***    EXTRATO ANUAL.
//* ***    ------------------------------------------------------------
//*
//STEP0    EXEC PGM=PLAN6130,
//       PARM='SOCAT'
//*
//* ***    RECEPCIONA ARQUIVO ROTINA EXGX PARA GFCT
//*
//TABELA   DD *
K DSN=MX.EXGX.S048450.ARQEMIDI(0)
//DDOUT    DD DSN=MX.GFCT.ARQEMIDI.EXGX(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0026,RECFM=FB)
//COPIAOUT DD DUMMY,
//       DCB=(LRECL=0026,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//PRINTER  DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP1    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAET - AGENCIA, CONTA,
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXTMAUOK,
//       DISP=SHR
//         DD DSN=MX.GFCT.EXMCBPOK,
//       DISP=SHR
//         DD DSN=MX.GFCT.EXMCBEOK,
//       DISP=SHR
//         DD DSN=MX.GFCT.EXMCSGOK,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTPRV.ANT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,A,10,7,A),FORMAT=BI
 END
//*
//STEP2    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAKB - AGENCIA, CONTA,
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP0.DDOUT,
//       DISP=OLD
//SORTOUT  DD DSN=MX.GFCT.ARQEMIDI.EXGX.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0026,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,5,A,6,7,A),FORMAT=BI
 END
//*
//STEP3    EXEC PGM=GFCT2768
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVOS IDENTIFICADOS PARA PREVIA DE
//* ***    EMISSAO DE EXTRATO ANUAL.
//* ***    ------------------------------------------------------------
//*
//EXMSTPRV DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//ARQEMIDI DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//EXQTEMIT DD DSN=MX.GFCT.EXQTEMIT.FIL(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT GFCTWAET - AGENCIA, CONTA,
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5121.EXMAUTO2,
//       DISP=SHR
//         DD DSN=MX.GFCT.GFCT5122.EXMAUTO3,
//       DISP=SHR
//         DD DSN=MX.GFCT.GFCT5123.EXMAUTO4,
//       DISP=SHR
//         DD DSN=MX.GFCT.GFCT5124.EXMAUTO5,
//       DISP=SHR
//         DD DSN=*.STEP3.EXQTEMIT,
//       DISP=OLD
//SORTOUT  DD DSN=MX.GFCT.EXMSTPRV(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,10,7,ZD,A),FORMAT=BI
 END
//*
//STEP5    EXEC PGM=GFCT2688
//*
//* ***    ------------------------------------------------------------
//* ***    TRATA ARQUIVOS IDENTIFICADOS PARA PREVIA DE
//* ***    EMISSAO DE EXTRATO ANUAL.
//* ***    ------------------------------------------------------------
//*
//EXMSTPRV DD DSN=*.STEP4.SORT.SORTOUT,
//       DISP=SHR
//EXQTEMIT DD DSN=MX.GFCT.EXQTEMIT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
