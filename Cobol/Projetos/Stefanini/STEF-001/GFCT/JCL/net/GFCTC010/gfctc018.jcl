//GFCTC018 JOB 'GFCT,4220,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP3    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    CONCATENA EVENTOS PARA CARGA NA GFCTB092
//* ***    EVENTOS VALOR LIQ. = 0
//* ***    EVENTOS NAO PARCELADOS
//* ***    EVENTOS PARCELADOS
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.CARGA092(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.MOVIZERA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CARG92CL(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,30),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(1,10,A,11,4,A,15,2,A,17,6,A),FORMAT=BI
  END
//*
