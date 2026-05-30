//GFCT5121 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC PGM=PLAN0260
//*
//* ***    -------------------------------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    -------------------------------------------------
//*
//LEITURA  DD DSN=MX.GFCT.GFCT5117.AGRUPCLI(0),
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ---------------------------------------------------
//* ***    CLASSIFICA PROCESSO DE EXTRATO.
//* ***    ORDEM: CNPJ/CPF.
//* ***    ---------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXMSTC1D(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5121.EXTRAGP1.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(78,9,CH,A,87,4,CH,A,91,2,ZD,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ----------------------------------------------------
//* ***    CLASSIFICA ARQUIVO DE AGRUPAMENTO - CLIENTE.
//* ***    ORDEM: CPF/CNPJ.
//* ***    ----------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5117.AGRUPCLI(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5121.AGRUPCLI.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=054,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(40,9,CH,A,49,4,CH,A,53,2,ZD,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC PGM=GFCT2763,
//       COND=(0,NE,STEP1)
//*
//* ***    --------------------------------------------------
//* ***    BALANCE LINE ENTRE PROCESSO DE EXTRATO COM
//* ***    AGRUPAMENTO CLIENTE.
//* ***    --------------------------------------------------
//*
//AGRUPCLI DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//EXMAGRP1 DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//EXMAGRP2 DD DSN=MX.GFCT.GFCT5121.EXMAGRP2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//EXMAUTO2 DD DSN=MX.GFCT.GFCT5121.EXMAUTO2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//EXMCANC2 DD DSN=MX.GFCT.GFCT5121.EXMCANC2(+1),
//       DISP=(,CATLG,DELETE),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC SORTD,
//       COND=(4,NE,STEP1)
//*
//* ***    -------------------------------------------------
//* ***    REGRAVA ARQUIVO DE EXTRATO COM OUTRO NOME.
//* ***    ARQUIVO DE AGRUP. CLIENTES VAZIO.
//* ***    -------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.EXMSTC1D(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5121.EXMAGRP2(+1),
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
