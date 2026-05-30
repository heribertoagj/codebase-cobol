//GFCTC025 JOB 'GFCT,4008,PR14','A299565',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC SORTD
//*
//* ***    ************************************************************
//* ***    OBJETIVOS : CONCATENA ARQUIVOS DE EXCEDENTE DE PACOTES     *
//* ***        MESANT E MESCOR - E INCLUINDO EXCEDENTE DE RESTRICAO   *
//* ***        E EXCEDENTE DE TARIFAS DE GRUPO DE SERVICO             *
//* ***    ************************************************************
//*
//SORTIN   DD DSN=MX.GFCT.EXCED92O.MESANT(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.EXCED92O.MESCOR(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.EXCEDRES.MESANT(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.EXCEDRES.MESCOR(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.EVTOSGRP.MESANT(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.EVTOSGRP.MESCOR(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JC025S01.EXCEDSRT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(010000,002000),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=COPY
 END
//*
//STEP2    EXEC PGM=GFCT2280,
//       PARM=1
//*
//* ***    ************************************************************
//* ***    OBJETIVOS : GERAR PREVIA DIARIA DA INTERFACE COM EXTF/CMET *
//* ***        PARM = 1 - TRATA ARQUIVOS DE EXCEDENTES                *
//* ***    ************************************************************
//*
//EVCADPRE DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//PREVEXTF DD DSN=MX.GFCT.JC025S02.PREVEXCD(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(010000,002000),RLSE),
//       DCB=(MX.A,LRECL=0122,RECFM=FB),
//       DATACLAS=PRODX37
//SYSUDUMP DD SYSOUT=Y
//SYSOUT   DD SYSOUT=*
//*
//STEP3    EXEC SORTD
//*
//* ***    ************************************************************
//* ***    OBJETIVOS : CONCATENA ARQUIVOS DE ISENTOS POR PACOTE       *
//* ***        MESANT E MESCOR - E INCLUINDO ISENTOS  DE              *
//* ***        GRUPO DE SERVICO                                       *
//* ***    ************************************************************
//*
//SORTIN   DD DSN=MX.GFCT.ISENT92O.MESANT(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.ISENT92O.MESCOR(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.ST18T92O.MESANT(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.ST18T92O.MESCOR(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.ISENTGRP.MESANT(0),
//       DISP=SHR
//         DD DSN=MX.GFCT.ISENTGRP.MESCOR(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JC025S03.ISENTSRT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,06),
//       SPACE=(TRK,(021040,04208),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=COPY
 END
//*
//STEP4    EXEC PGM=GFCT2280,
//       PARM=2
//*
//* ***    ************************************************************
//* ***    OBJETIVOS : GERAR PREVIA DIARIA DA INTERFACE COM EXTF/CMET *
//* ***        PARM = 2 - TRATA ARQUIVOS DE ISENTOS                   *
//* ***    ************************************************************
//*
//EVCADPRE DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//PREVEXTF DD DSN=MX.GFCT.JC025S04.PREVISEN(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(010000,002000),RLSE),
//       DCB=(MX.A,LRECL=0122,RECFM=FB),
//       DATACLAS=PRODX37
//SYSUDUMP DD SYSOUT=Y
//SYSOUT   DD SYSOUT=*
//*
//STEP5    EXEC SORTD
//*
//* ***    ************************************************************
//* ***    OBJETIVOS : CONCATENA PREVIA  DE EXTRATO DE UTILIZACAO     *
//* ***        COM ARQUIVO DE ACUMULADO MENSAL                        *
//* ***    ************************************************************
//*
//SORTIN   DD DSN=*.STEP2.PREVEXTF,
//       DISP=SHR
//         DD DSN=*.STEP4.PREVEXTF,
//       DISP=SHR
//         DD DSN=MX.GFCT.PRV.PRECEXTF(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.PRV.PRECEXTF(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,12),
//       SPACE=(TRK,(050260,10052),RLSE),
//       DCB=(MX.A,LRECL=0122,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=COPY
 END
//*
