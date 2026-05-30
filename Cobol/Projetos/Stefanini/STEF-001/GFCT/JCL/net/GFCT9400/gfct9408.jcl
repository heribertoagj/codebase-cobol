//GFCT9408 JOB 'GFCT,4220,PR14','H65052',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA POR AGENCIA E CONTA.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.PRP.POUPB005(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.POUPB005.APEDIDO.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0007,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,3,A,4,4,A),FORMAT=BI
 END
//*
//STEP2    EXEC PGM=GFCT2117
//*
//* ***    ------------------------------------------------------------
//* ***    CADASTR0 DE CLIENTES POUP SOMENTE COM AGENCIA VALIDAS.
//* ***    ------------------------------------------------------------
//*
//CADAPOUP DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//POUPADES DD DSN=MX.GFCT.POUPADE0.APEDIDO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0040,RECFM=FB)
//POUPINVA DD DSN=MX.GFCT.POUPINVA.APEDIDO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0007,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA POR AGENCIA, CONTA E RAZAO.
//* ***    ------------------------------------------------------------
//*                
//SORTIN   DD DSN=MJ.CLIE.PRP.CADNOVGR.ALFA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CADPREDS.APEDIDO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0029,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(1,3,A,7,4,A,4,3,A),FORMAT=BI
 END
//*
//STEP4    EXEC PGM=GFCT2118
//*
//* ***    ------------------------------------------------------------
//* ***    CADASTR0 DE CLIENTES POUP SOMENTE COM AGENCIA VALIDAS.
//* ***    ------------------------------------------------------------
//*
//POUPADES DD DSN=*.STEP2.POUPADES,
//       DISP=SHR
//CADPREDU DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//POUPADE1 DD DSN=MX.GFCT.POUPADE1.APEDIDO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0055,RECFM=FB)
//POUPSCAD DD DSN=MX.GFCT.POUPSCAD.APEDIDO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0055,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    ORDENA POR FILIAL DO CNPJ/CPF.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP4.POUPADE1,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.POUPADE2.APEDIDO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0040,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(16,3,A),FORMAT=BI
 END
//*
//STEP6    EXEC PGM=GFCT2119,
//       PARM='F01237-J01238'
//*
//* ***    ------------------------------------------------------------
//* ***    CADASTR0 DE CLIENTES POUP SOMENTE COM AGENCIA VALIDAS.
//* ***    ------------------------------------------------------------
//*
//POUPADE2 DD DSN=*.STEP5.SORT.SORTOUT,
//       DISP=SHR
//POUPADES DD DSN=MX.GFCT.POUPADES.APEDIDO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(3300,3300),RLSE),
//       DCB=(MX.A,LRECL=0050,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
