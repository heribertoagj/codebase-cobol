//GFCTA110 JOB 'GFCT,4220,PR14','B142376',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    TRATAMENTO DE SOLICITACOES DE CANCELAMENTO (EXCLUSAO)
//* ***    15-PAB / 17-BANCO POSTAL / 20-BRADESCO EXPRESSO
//*
//STEP1    EXEC PGM=PLAN0260
//*
//* ***    ------------------------------------------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    ------------------------------------------------------------
//*
//LEITURA  DD DSN=MX.GFCT.EXCLA108.SOLICPAB(0),
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA ARQCESTA POR AGENCIA E CONTA.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXCLA104.ARQCESTA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXCLA110.ARQCESTA(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(005001,0501),RLSE),
//       DCB=(MX.A,LRECL=093,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,3,A,4,4,A),FORMAT=BI
 END
//*
//STEP3    EXEC PGM=POOL0003,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    DESCOMPRIME ARQUIVO DE CLIENTES
//* ***    ------------------------------------------------------------
//*
//ENTRA    DD DSN=MJ.CLIE.PRP.CADNOVGR.ALFA(0),
//       DISP=SHR
//SAI      DD DSN=MX.GFCT.EXCLA110.ARQCLIE1(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(010000,02000),RLSE),
//       DCB=(MX.A,LRECL=180,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP  DD  SYSOUT=*
//*
//STEP4    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA ARQCLIE1 POR AGENCIA, CONTA.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP3.SAI,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXCLA110.ARQCLIE1.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(010000,02000),RLSE),
//       DCB=(MX.A,LRECL=180,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,3,A,7,4,A),FORMAT=BI
 END
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK11 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//SORTWK12 DD UNIT=DISCO,
//       SPACE=(CYL,(1100))
//*
//STEP5    EXEC PGM=GFCT2401,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    INCLUI INFORMACAO DE PAB NO ARQUIVO COM A BAIXA DE ADESOES
//* ***    INDIVIDUAIS.
//* ***    ------------------------------------------------------------
//*
//ARQCESTA DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//ARQCLIE1 DD DSN=*.STEP4.SORT.SORTOUT,
//       DISP=SHR
//CLICESTA DD DSN=MX.GFCT.EXCLA110.CLICESTA(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(005001,0501),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP6    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA SOLICPAB POR TARIFA, AGENCIA E POSTO.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXCLA108.SOLICPAB(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXCLA110.SOLICPAB(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(002001,0201),RLSE),
//       DCB=(MX.A,LRECL=700,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(3,3,A,650,3,A,653,3,A),FORMAT=BI
 END
//*
//STEP7    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA CLIECESTA POR TARIFA, AGENCIA E PAB.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP5.CLICESTA,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXCLA110.CLICESTA.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(005001,0501),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(8,3,A,1,3,A,128,3,A),FORMAT=BI
 END
//*
//STEP8    EXEC PGM=GFCT2402,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    EXCLUI CLIENTES DE OUTROS PABS.
//* ***    ------------------------------------------------------------
//*
//CLICESTA DD DSN=*.STEP7.SORT.SORTOUT,
//       DISP=SHR
//SOLICPAB DD DSN=*.STEP6.SORT.SORTOUT,
//       DISP=SHR
//DELETPAB DD DSN=MX.GFCT.EXCLA110.DELETPAB(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(002001,0201),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP9    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA SOLICPAB POR AGRUPAMENTO, TARIFA E TIMESTAMP.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXCLA108.SOLICPAB(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXCLA108.SOLICPAB.CLAS2(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(002001,0201),RLSE),
//       DCB=(MX.A,LRECL=700,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,2,A,3,3,A,6,26,A),FORMAT=BI
 END
//*
//STEP10   EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA DELETPAB POR AGRUPAMENTO, TARIFA E TIMESTAMP.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP8.DELETPAB,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXCLA110.DELETPAB.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(002001,0201),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(94,2,A,96,3,A,99,26,A),FORMAT=BI
 END
//*
//STEP11   EXEC PGM=GFCT2403,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------------
//* ***    CONTAR EVENTOS DELETADOS PARA INCLUIR NA SOLICITACAO.
//* ***    ------------------------------------------------------------
//*
//SOLICPAB DD DSN=*.STEP9.SORT.SORTOUT,
//       DISP=SHR
//DELETPAB DD DSN=*.STEP10.SORT.SORTOUT,
//       DISP=SHR
//ATUALPAB DD DSN=MX.GFCT.EXCLA110.ATUALPAB(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(002001,0201),RLSE),
//       DCB=(MX.A,LRECL=700,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
