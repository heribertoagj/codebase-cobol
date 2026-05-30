//GFCTA203 JOB 'GFCT,4008,PR14','I351912',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ************************************************************
//* ***    GERACAO DE ARQUIVO PARA A ROTINA BSPI
//* ***    TARIFAS DEBITADAS NO DIA
//* ***    ************************************************************
//*
//STEP01   EXEC PGM=GFCT2503
//*
//* ***    ------------------------------------------------------------
//* ***    GERADOR DE SELECT DINAMICO PARA HPU:
//* ***    GFCTB092 - MOVTO_EVNTO_CRRTT.
//* ***    ------------------------------------------------------------
//*
//SYSINHPU DD DSN=MX.GFCT.JA203S01.SYSINHPU(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(000100,0010),RLSE),
//       DCB=(MX.A,LRECL=060,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP02   EXEC DB2M1HPU,
//       UID='GFCTA203'
//*
//* ***    ************************************************************
//* ***    HPU - DESCARREGA A TABELA GFCTB092 EM ARQ.SEQUENCIAL.
//* ***    ************************************************************
//*
//SYSPUNCH DD DUMMY
//SYSREC00 DD DSN=MX.GFCT.JA203S02.SYSREC00(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A),
//       DATACLAS=PRODX37
//SYSIN    DD DSN=MX.GQPM.PRV.HPUZI60,
//       DISP=SHR
//         DD DSN=*.STEP01.SYSINHPU,
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//* ***    ************************************************************
//* ***    FORMATA ARQUIVO NO LAYOUT DE ENVIO PARA O BSPI
//* ***    ************************************************************
//*
//*
//STEP03   EXEC PGM=GFCT2504
//EVENTO92 DD DSN=*.STEP02.HPU.SYSREC00,
//       DISP=SHR
//ARQUBSPI DD DSN=MX.GFCT.JA203S03.ARQUBSPI(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(080000,8000),RLSE),
//       DCB=(MX.A,LRECL=0212,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
