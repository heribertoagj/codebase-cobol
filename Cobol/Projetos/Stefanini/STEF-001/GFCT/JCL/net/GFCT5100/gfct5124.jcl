//GFCT5124 JOB 'GFCT,4220,PR14','B031531',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC PGM=PLAN0260
//*
//* ***    -----------------------------------------------
//* ***    VERIFICA ARQUIVO VAZIO.
//* ***    -----------------------------------------------
//*
//LEITURA  DD DSN=MX.GFCT.GFCT5119.AGRUPSGT(0),
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------
//* ***    CLASSIFICA PROCESSO DE EXTRATO.
//* ***    ORDEM: SEGMENTO.
//* ***    ------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5123.EXMAGRP4(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5124.EXMAGRP4.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(27,5,ZD,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    --------------------------------------------------
//* ***    CLASSIFICA ARQUIVO DE AGRUPAMENTO - SEGMENTO.
//* ***    ORDEM: SEGMENTO.
//* ***    --------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5119.AGRUPSGT(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT5124.AGRUPSGT.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=031,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(30,2,PD,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC PGM=GFCT2766,
//       COND=(0,NE,STEP1)
//*
//* ***    ------------------------------------------------
//* ***    BALANCE LINE ENTRE PROCESSO DE EXTRATO COM
//* ***    AGRUPAMENTO SEGMENTO.
//* ***    ------------------------------------------------
//*
//AGRUPSGT DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//EXMAGRP4 DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//EXMAGRP5 DD DSN=MX.GFCT.GFCT5124.EXMAGRP5(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//EXMAUTO5 DD DSN=MX.GFCT.GFCT5124.EXMAUTO5(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//EXMCANC5 DD DSN=MX.GFCT.GFCT5124.EXMCANC5(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC SORTD,
//       COND=(0,NE,STEP1)
//*
//* ***    --------------------------------------------
//* ***    CLASSIFICA POR AGENCIA E CONTA.
//* ***    --------------------------------------------
//*
//SORTIN   DD DSN=*.STEP4.EXMAGRP5,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,A,10,7,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP6    EXEC SORTD,
//       COND=(4,NE,STEP1)
//*
//* ***    ------------------------------------------------
//* ***    REGRAVA ARQUIVO DE EXTRATO COM OUTRO NOME.
//* ***    ARQUIVO DE AGRUP. SEGMENTO VAZIO.
//* ***    CLASSIFICA POR AGENCIA E CONTA.
//* ***    ------------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.GFCT5123.EXMAGRP4(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.EXMSTCL2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=095,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(5,5,A,10,7,A),FORMAT=BI
 END
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
