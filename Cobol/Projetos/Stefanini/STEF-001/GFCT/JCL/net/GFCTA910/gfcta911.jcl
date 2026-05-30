//GFCTA911 JOB 'GFCT,4008,PR14','B460076',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=MJ.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC SORTD
//*
//* ***    ----------------------------------------------------
//* ***  * ARQUIVO CONTENDO NEG VIGENTES
//* ***    ----------------------------------------------------
//* ***  * CAG_BCRIA_SALRL    (03,03,PD,A)            GFCTWBDF
//* ***  * CCTA_BCRIA_SALRL   (06,07,PD,A)
//* ***    ----------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.JA910S06.SORTOUT(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JA911S01.SORTOUT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0679,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(03,03,PD,A,06,07,PD,A)
 END
//*
//STEP2    EXEC SORTD
//*
//* ***    ----------------------------------------------------
//* ***  * ARQUIVO DO  CLIE CONTENDO GRUPO E SUBGRUPO CONTA
//* ***    ----------------------------------------------------
//* ***  * AGENCIA CLIE    (01,03,PD,A)               I#CHEQ04
//* ***  * CONTA CLIE      (07,04,PD,A)
//* ***    ----------------------------------------------------
//*
//SORTIN   DD DSN=MJ.CLIE.PRP.REDUZGER.ALFA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JA911S02.SORTOUT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=029,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(01,03,PD,A,07,04,PD,A)
 END
//*
//STEP3    EXEC PGM=GFCT2BDF
//*
//* ***    BUSCAR GRUPO E SUBGRUPO NO ARQUIVO DO CLIE PARA CONTA
//* ***    SALARIO
//*
//NEGCVIGE DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//ECCTACLI DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//NEGCVIGS DD DSN=MX.GFCT.JA911S03.NEGCVIGS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0364,RECFM=FB),
//       DATACLAS=PRODX37
//NEGCVIGI DD DSN=MX.GFCT.JA911S03.NEGCVIGI(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0679,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC SORTD
//*
//* ***    ----------------------------------------------------
//* ***  * ARQUIVO CONTENDO NEG VIGENTES
//* ***    ----------------------------------------------------
//* ***  * CAG-BCRIA        (25,03,PD,A)              GFCTWBTF
//* ***  * CCTA-BCRIA-CLI   (28,07,PD,A)
//* ***    ----------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP3.NEGCVIGS,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JA911S04.SORTOUT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0364,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(25,03,PD,A,28,07,PD,A)
 END
//*
//STEP5    EXEC PGM=GFCT2BYF
//*
//* ***    BUSCAR GRUPO E SUBGRUPO NO ARQUIVO DO CLIE PARA CONTA
//* ***    CLIENTE
//*
//NEGCVIGE DD DSN=*.STEP4.SORT.SORTOUT,
//       DISP=SHR
//ECCTACLI DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//NEGCVIGS DD DSN=MX.GFCT.JA911S05.NEGCVIGS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0364,RECFM=FB),
//       DATACLAS=PRODX37
//NEGCVIGI DD DSN=MX.GFCT.JA911S05.NEGCVIGI(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0364,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
