//GFCT5125 JOB 'GFCT,4220,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT I#SIGB06 - AGENCIA, CONTA, CARTEIRA E SOMENTE
//* ***                    CARTEIRAS 050, 340, 360, 365, 370, 390, 775.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.SIGB.S077034.CARGMENS(0),
//       DISP=SHR
//         DD DSN=MX.SIGB.S077034.CARGTRIM(0),
//       DISP=SHR
//         DD DSN=MX.SIGB.S077034.CARGSEMT(0),
//       DISP=SHR
//         DD DSN=MX.SIGB.S077508.CARGPOUP(0),
//       DISP=SHR
//         DD DSN=MX.SIGB.S077053.CARGRDIA(0),
//       DISP=SHR
//         DD DSN=MX.SIGB.S077315.PRV.CARGRSEM(0),
//       DISP=SHR
//         DD DSN=MX.SIGB.S077029.CARGRMES(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5125.DADOSIGB(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0018,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(1,3,PD,A,4,4,PD,A,8,3,PD,A),FORMAT=BI
  OUTFIL INCLUDE=(8,3,PD,EQ,050,OR,8,3,PD,EQ,340,OR,8,3,PD,EQ,360,OR,
                  8,3,PD,EQ,365,OR,8,3,PD,EQ,370,OR,8,3,PD,EQ,390,OR,
                  8,3,PD,EQ,775),FNAMES=SORTOUT
  END
//*
//STEP1A   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SUM FIELDS I#SIGB06 - AGENCIA, CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5125.SIGBSUM(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0018,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(1,3,PD,A,4,4,PD,A),FORMAT=BI
  SUM  FIELDS=(11,8,PD)
  END
//*
//STEP1B   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    RETIRAR REGISTROS COM RENTABILIDADE NEGATIVA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP1A.SORT.SORTOUT,
//       DISP=SHR
//NEGATIVO DD DSN=MX.GFCT.GFCT5125.SIGBSUM.NEGTO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0018,RECFM=FB)
//POSITIVO DD DSN=MX.GFCT.GFCT5125.SIGBSUM.POSITO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0018,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=COPY
  OUTFIL INCLUDE=(11,8,PD,LE,000000000000000),FNAMES=NEGATIVO
  OUTFIL INCLUDE=(11,8,PD,GT,000000000000000),FNAMES=POSITIVO
  END
//*
//STEP2    EXEC PGM=GFCT2656
//*
//* ***    -----------------------------
//* ***    OBTEM VALOR DA RENDA NO SIGB
//* ***    -----------------------------
//*
//EXMSTCLI DD DSN=MX.GFCT.EXMSTCL2(0),
//       DISP=SHR
//SIGBSUMI DD DSN=*.STEP1B.SORT.POSITIVO,
//       DISP=SHR
//EXMSRENT DD DSN=MX.GFCT.EXMSTCL3(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC SORTD
//*
//* ***    --------------------------------
//* ***    SORT I#GECTAF - AGENCIA, CONTA
//* ***    --------------------------------
//*
//SORTIN   DD DSN=MJ.GECT.PRP.CONTGERE(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CONTGERE.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0020,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(1,3,PD,A,4,4,PD,A),FORMAT=BI
  END
//*
//STEP4    EXEC PGM=GFCT2684
//*
//* ***    -------------------------
//* ***    INDICA SE TEM NO GECT.
//* ***    -------------------------
//*
//EXMSTCLI DD DSN=*.STEP2.EXMSRENT,
//       DISP=SHR
//CADAGECT DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//EXMSGECT DD DSN=MX.GFCT.EXMSTCL4(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
