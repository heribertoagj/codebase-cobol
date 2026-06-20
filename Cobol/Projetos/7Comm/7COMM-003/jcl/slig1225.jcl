//SLIG1225 JOB 'SLIG,4130,PR11','C085384',MSGCLASS=Z,REGION=4M,
//       SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//STEP0    EXEC PGM=PLAN2440,
//       PARM=001
//*
//* ***    FAZ COPIA DO ARQUIVO MX.SLIG.PRV.MVTOCHDE(0).
//*
//ENT00001 DD DSN=MX.SLIG.PRV.MVTOCHDE(0),
//       DISP=SHR
//SAI00001 DD DSN=MX.SLIG.D0PRVMZ.FIMPERCH(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=VTS,
//       VOL=(,,,255),
//       DCB=(MX.A)
//PRINTER  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
**************************** Bottom of Data ****************************























































