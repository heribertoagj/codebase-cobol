//GFCT2091 JOB 'GFCT,4220,PR14','C089177',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*#//STEP1 EXEC PGM=GFCT2097
//*#//*
//*#//* *** ----------------------
//*#//* *** EXCLUI CONTA SALARIO
//*#//* *** ----------------------
//*#//*
//*#//ADESAO DD DSN=MX.GFCT.GFCT2097.ADESAO.CLASS(0),
//*#//   DISP=SHR
//*#//CTASALAR DD DSN=MX.GFCT.GFCT2097.CTASALAR.CLASS(0),
//*#//   DISP=SHR
//*#//SADESAO DD DSN=MX.GFCT.GFCT2097.SADESAO(+1),
//*#//   DISP=(,CATLG,DELETE),
//*#//   UNIT=DISCO,
//*#//   SPACE=(TRK,(020000,4000),RLSE),
//*#//   DCB=(MX.A,LRECL=0200,RECFM=FB)
//*#//SYSOUT DD SYSOUT=*
//*#//SYSUDUMP DD SYSOUT=Y
//*#//*
//STEP2    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT ARQUIVO DE CTASCIEE - ORDEM: AGENCIA E CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MJ.SCCE.PRP.CTASCIEE(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT2098.CTASCIEE.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0100,RECFM=FB)
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,8,4,PD,A),FORMAT=BI
 END
//*
//STEP3    EXEC PGM=GFCT2098
//*
//* ***    -------------------
//* ***    EXCLUI CONTA CIEE
//* ***    -------------------
//*
//ADESAO   DD DSN=MX.GFCT.GFCT2097.ADESAO.CLASS(0),
//       DISP=SHR
//CTASCIEE DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//SADESAO  DD DSN=MX.GFCT.GFCT2098.SADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT ARQUIVO DE CTASMMOV
//* ***    SELECIONA AS CONTAS QUE NUNCA MOVIMENTARAM:
//* ***    - DATA DE ABERTURA = DATA DA ULTIMA MOVIMENTACAO  E
//* ***    - SALDO CONTABIL = ZEROS
//* ***    - ORDEM: AGENCIA, RAZAO E CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MJ.CCMS.S076601.GRESPAG.A2000(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT2101.CTASMMOV.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0155,RECFM=FB)
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,4,3,PD,A,7,4,PD,A),FORMAT=BI
 OUTFIL INCLUDE=((141,5,PD,EQ,146,5,PD),AND,(91,8,PD,EQ,0),AND,
                 (130,1,CH,NE,C'1')),FNAMES=SORTOUT
 END
//*
//STEP5    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT ARQUIVO DE ADESAO - ORDEM: AGENCIA, RAZAO E CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP3.SADESAO,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT2101.SADESAO.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,4,3,PD,A,7,4,PD,A),FORMAT=BI
 END
//*
//STEP6    EXEC PGM=GFCT2101
//*
//* ***    ------------------------------------------------------------
//* ***    EXCLUIR CONTAS SEM MOVIMENTACAO
//* ***    ------------------------------------------------------------
//*
//ADESAO   DD DSN=*.STEP5.SORT.SORTOUT,
//       DISP=SHR
//CTASMMOV DD DSN=*.STEP4.SORT.SORTOUT,
//       DISP=SHR
//SADESAO  DD DSN=MX.GFCT.GFCT2101.SADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP7    EXEC SORTD
//*
//* ***    -------------------------------------------------------------
//* ***    SORT ARQUIVO DE ADESAO
//* ***    - ORDEM: AGENCIA, RAZAO, CONTA E POSTO
//* ***    -------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP6.SADESAO,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT2102.SADESAO.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,4,3,PD,A,7,4,PD,A,126,2,PD,A),FORMAT=BI
 END
//*
//STEP8    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT ARQUIVO DE CONTAS TRAGUEADAS
//* ***    - ORDEM: AGENCIA, RAZAO, CONTA E POSTO
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MJ.TRAG.PRV.TRANSROT.MULTI(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT2102.TRAG.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0110,RECFM=FB)
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SYSIN    DD *
 SORT FIELDS=(3,3,PD,A,6,3,PD,A,9,7,PD,A,18,2,PD,A),FORMAT=BI
 END
//*
//STEP9    EXEC PGM=GFCT2102
//*
//* ***    ------------------------------------------------------------
//* ***    EXCLUIR REGISTROS COM CONTAS TRAGUEADAS
//* ***    ------------------------------------------------------------
//*
//ADESAO   DD DSN=*.STEP7.SORT.SORTOUT,
//       DISP=SHR
//TRANSROT DD DSN=*.STEP8.SORT.SORTOUT,
//       DISP=SHR
//SADESAO  DD DSN=MX.GFCT.GFCT2102.SADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP10   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT I#GFCTD0 - AGENCIA E CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP9.SADESAO,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT2104.ADESAO.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,7,4,PD,A),FORMAT=BI
 END
//*
//STEP11   EXEC PGM=GFCT2104
//*
//* ***    ------------------------------------------------------------
//* ***    VERIFICAR CONTA GARANTIDA
//* ***    ------------------------------------------------------------
//*
//ADESAO   DD DSN=*.STEP10.SORT.SORTOUT,
//       DISP=SHR
//SADESAO  DD DSN=MX.GFCT.GFCT2104.SADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP12   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SORT I#GFCTD0 - AGENCIA E PAB
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP11.SADESAO,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.GFCT2112.ADESAO.CLASS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00200))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,126,2,PD,A),FORMAT=BI
 END
//*
//STEP13   EXEC PGM=GFCT2112
//*
//* ***    ------------------------------------------------------------
//* ***    VERIFICAR BANCO POSTAL
//* ***    ------------------------------------------------------------
//*
//ADESAO   DD DSN=*.STEP12.SORT.SORTOUT,
//       DISP=SHR
//SADESAO  DD DSN=MX.GFCT.GFCT2112.SADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0215,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP14   EXEC PGM=GFCT2099
//*
//* ***    ------------------------------------------------------------
//* ***    MOVIMENTACAO DA I#GFCTD0 PARA FD-ADESAO UTILIZADA NOS PGMS
//* ***    DE ADESAO.
//* ***    ------------------------------------------------------------
//*
//EADESAO  DD DSN=*.STEP13.SADESAO,
//       DISP=SHR
//SADESAO  DD DSN=MX.GFCT.GFCT2099.SADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0060,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
