//GFCT1599 JOB 'GFCT,4008,PR14','D336356',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC SORTD
//*
//* ***    SORTEIA PELA CHAVE:
//* ***    BANCO / AGENCIA / CONTA / TARIFA / DATA EVENTO
//* ***    PARA PARTICIPAR FLEX POR % TRANSACAO
//*
//SORTIN   DD DSN=MX.GFCT.J1529S4.SEGCOBRA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J1599S1.MOTVFLEX(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(091150,18230),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(45,2,PD,A,47,3,PD,A,50,07,PD,A,23,3,PD,A,122,4,ZD,A,
              119,2,ZD,A,116,2,ZD,A),FORMAT=BI
 END
//*
//STEP2    EXEC PGM=GFCT1888
//EVENTO92 DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//ISENTO92 DD DSN=MX.GFCT.ISENTO92.GFCT1888(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB),
//       DATACLAS=PRODX37
//EVENTOAT DD DSN=MX.GFCT.GFCT1888.EVENTOAT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB),
//       DATACLAS=PRODX37
//DESCTO41 DD DSN=MX.GFCT.DESCONTO.GFCT1888(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0150,RECFM=FB),
//       DATACLAS=PRODX37
//VALIDADO DD DSN=MX.GFCT.GFCT1888.VALIDADO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0192,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC PGM=ICETOOL
//*
//* ***    LISTA REGISTROS APLICADOS
//* ***    FLEXIBILIZACAO
//*
//ARQUIVO  DD DSN=*.STEP2.VALIDADO,
//       DISP=SHR
//RELATO   DD DSN=MX.GFCT.J1599S3.RELAT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,1100),RLSE),
//       DATACLAS=PRODX37
//SORTCNTL DD *
 SORT FIELDS=COPY
//TOOLIN   DD *
 DISPLAY  FROM(ARQUIVO)      LIST(RELATO)                             -
 TITLE('GFCT - RELAT. FLEXIBILIZACAO APLICADA ')                      -
 PAGE DATE(DM4/)          TIME                    BETWEEN(1)          -
 HEADER('AGENCIA')         ON(026,003,PD)                 -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('CONTA')           ON(029,007,PD)                 -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('TARIFA')          ON(023,003,PD)                 -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('DESCRICAO')       ON(082,060,CH)                 -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('VLR CARTAZ  ')    ON(68,7,PD,C2,N05)             -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('VLR LIQUIDO')     ON(42,6,PD,C2,N05)             -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('VLR OFERTA ')     ON(42,6,PD,C2,N05)             -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('VLR DESCONTO')    ON(48,6,PD,C2,N05)             -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('ROTINA')          ON(011,004,CH)                 -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('DT-EVTO')         ON(001,010,CH)                 -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('NUM-ARQ')         ON(015,002,PD)                 -
 HEADER(';')               ON(142,001,CH)                 -
 HEADER('COD-SEQ')         ON(017,006,PD)                 -
 BLANK                                                    -
 COUNT('TOTAL GERAL:') EDCOUNT(U10)
//DFSMSG   DD SYSOUT=*
//TOOLMSG  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC SORTD
//SORTIN   DD DSN=*.STEP2.EVENTOAT,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J1599S4.SEGCOBRA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0265,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(45,2,PD,A,47,3,PD,A,50,07,PD,A,23,3,PD,A,122,4,ZD,A,
              119,2,ZD,A,116,2,ZD,A),FORMAT=BI
 END
//*
//*
//STEP05   EXEC PGM=GARQ2000
//*
//* ***    *******************************************************
//* ***    **  ENVIA ARQUIVO PARA O GESTOR VIA XFB. RELAT OK **
//* ***    *******************************************************
//*
//ENTRADA  DD DSN=*.STEP3.RELATO,
//       DISP=SHR
//SAIDA    DD DSN=M2.GFCT.GARQ.FLEXMNOR,
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(6000,2400),RLSE),
//       FREE=CLOSE,
//       DATACLAS=DCZEDC
//SYSIN    DD *
NOME_DO_FLUXO=M2.GFCT.GARQ.FLEXMNOR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
