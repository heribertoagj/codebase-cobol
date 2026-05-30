//GFCTA524 JOB 'GFCT,4220,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC DB2M1UTB,
//       UID='GFCTA524',
//       SOUT=Z
//*
//* ***    **************************************
//* ***    LOAD (RTS) RESUME YES DA TABELA
//* ***    DB2PRD.TCTRL_NGTVC_CLI - (GFCTB0N2)
//* ***    **************************************
//*
//SYSIN    DD DSN=MX.DBII.DEFUTIL(GFCTY0N2),
//       DISP=SHR
//DB2UPROC.SYSREC00 DD DSN=MX.GFCT.CARGAN2C(0),
//       DISP=SHR
//*
