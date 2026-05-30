//GFCTN206 JOB 'GFCT,4008,PR14','D336380',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ************************************************************
//* ***    GERACAO DE ARQUIVO PARA A ROTINA BSPI
//* ***    TARIFAS DEBITADAS NO MES ANTERIOR
//* ***    ************************************************************
//*
//STEP01   EXEC PGM=GFCT2505
//*
//* ***    ------------------------------------------------------------
//* ***    GERADOR DE SELECT DINAMICO PARA HPU:
//* ***    GFCTB092 - MOVTO_EVNTO_CRRTT.
//* ***    ------------------------------------------------------------
//*
//SYSINHPU DD DSN=MX.GFCT.JN206S01.SYSINHPU(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(000100,0010),RLSE),
//       DCB=(MX.A,LRECL=060,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP02   EXEC DB2M1HPU,
//       UID='GFCTN206'
//*
//* ***    ************************************************************
//* ***    HPU - DESCARREGA A TABELA GFCTB092 EM ARQ.SEQUENCIAL.
//* ***    ************************************************************
//*
//SYSPUNCH DD DUMMY
//SYSREC00 DD DSN=MX.GFCT.JN206S02.SYSREC00(+1),
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
//ARQUBSPI DD DSN=MX.GFCT.JN206S03.ARQUBSPI(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(080000,8000),RLSE),
//       DCB=(MX.A,LRECL=0212,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP04   EXEC PGM=PLAN1010,
//       PARM='SIM'
//*
//* ***    ------------------------------------------------------------
//* ***    GERA TRANSITO PARA BSPIXXXX
//* ***    ------------------------------------------------------------
//*
//SYSUT1   DD DSN=*.STEP03.ARQUBSPI,
//       DISP=SHR
//SYSUT2   DD DSN=MX.GFCT.BSPI.TARIFPIX,
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(080000,8000),RLSE),
//       DCB=(MX.A,LRECL=0212,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//PRINTER  DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
