//GFCT5122 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC PGM=PLAN0260
//*
//* ***    ------------------------------------------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    ------------------------------------------------------------
//*
//LEITURA  DD DSN=MX.GFCT.GFCT5116.AGRUPOST(0),
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------------
//* ***    CLASSIFICA PROCESSO DE EXTRATO.
//* ***    ORDEM: AGENCIA E POSTO/PAB.
//* ***    ------------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5121.EXMAGRP2(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5122.EXMAGRP2.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,ZD,A,22,5,ZD,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    --------------------------------------------------
//* ***    CLASSIFICA ARQUIVO DE AGRUPAMENTO - BANCO POSTAL.
//* ***    ORDEM: AGENCIA E CODIGO DE POSTO.
//* ***    --------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5116.AGRUPOST(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5122.AGRUPOST.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=038,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(33,3,PD,A,36,3,PD,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC PGM=GFCT2764,
//       COND=(0,NE,STEP1)
//*
//* ***    -------------------------------------------------
//* ***    BALANCE LINE ENTRE PROCESSO DE EXTRATO COM
//* ***    AGRUPAMENTO BANCO POSTAL.
//* ***    -------------------------------------------------
//*
//AGRUPOST DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//EXMAGRP2 DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//EXMAGRP3 DD DSN=MX.GFCT.GFCT5122.EXMAGRP3(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//EXMAUTO3 DD DSN=MX.GFCT.GFCT5122.EXMAUTO3(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//EXMCANC3 DD DSN=MX.GFCT.GFCT5122.EXMCANC3(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC SORTD,
//       COND=(4,NE,STEP1)
//*
//* ***    ------------------------------------------------
//* ***    REGRAVA ARQUIVO DE EXTRATO COM OUTRO NOME.
//* ***    ARQUIVO DE AGRUP. BANCO POSTAL VAZIO.
//* ***    ------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5121.EXMAGRP2(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5122.EXMAGRP3(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=COPY
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
