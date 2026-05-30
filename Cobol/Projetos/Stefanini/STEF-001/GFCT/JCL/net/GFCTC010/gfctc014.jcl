//GFCTC014 JOB 'GFCT,4220,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    MOVIMENTO GERACAO DE PARCELAS
//* ***    BASE BOOK I#GFCTVA (265 BYTES)
//* ***    APENAS REGISTROS COM VALOR LIQ. <> ZEROS
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.MSSCALCM(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.MOVIEVEN(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,20),
//       SPACE=(TRK,(090180,18036),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB)
//SYSIN    DD *
  INCLUDE COND=(76,6,PD,GT,0)
  SORT FIELDS=(23,3,A,122,4,A,119,2,A,116,2,A),FORMAT=BI
  END
//*
//STEP2    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    MOVIMENTO GERACAO DE PARCELAS
//* ***    BASE BOOK I#GFCTVA (250 BYTES)
//* ***    SELECIONA REGISTROS QUE NAO ATENDEM (LIQ <= ZEROS)
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.MSSCALCM(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.MOVIZERA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,10),
//       SPACE=(TRK,(047880,09576),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB)
//SYSIN    DD *
  INCLUDE COND=(76,6,PD,LE,0)
  SORT FIELDS=COPY
  END
//*
