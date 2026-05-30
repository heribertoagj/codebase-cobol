//GFCT9938 JOB 'GFCT,4220,PR14','D336356',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC PGM=ICETOOL
//*
//* ***    ------------------------------------------------------------
//* ***    ORGANIZA ARQUIVO DE CLIENTES.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=MJ.CLIE.PRP.REDGAGCT.ALFA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.REDGAGCT.CLAS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0029,RECFM=FB)
//TEMP     DD DSN=MX.GFCT.TEMP9938(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0029,RECFM=FB)
//SORTLIB  DD DSN=SYS1.SORTLIB,
//       DISP=SHR
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK11 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK12 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//DFSPARM  DD *
 EFS=MESU9440
//TOOLIN   DD *
 SORT FROM(SORTIN) TO(TEMP) USING(SORT)
 COPY FROM(TEMP) TO(SORTOUT) USING(SOUT)
//SORTCNTL DD *
 SORT FIELDS=(1,3,PD,A,7,4,PD,A),FORMAT=BI
//SOUTCNTL DD *
 INCLUDE COND=((1,3,CJ,EQ,C'OK',OR,1,3,CJ,NE,C'OK'),AND,
               (1,3,CJ,EQ,C'AG',AND,1,3,PD,NE,1,AND,
                 4,3,PD,NE,02510,AND,
                 4,3,PD,NE,04770,AND,
                 4,3,PD,NE,07010,AND,
                 4,3,PD,NE,07020,AND,
                 4,3,PD,NE,07040,AND,
                 4,3,PD,NE,07090,AND,
                 4,3,PD,NE,07100,AND,
                 4,3,PD,NE,07130,AND,
                 4,3,PD,NE,07160,AND,
                 4,3,PD,NE,07180,AND,
                 4,3,PD,NE,07240,AND,
                 4,3,PD,NE,07250,AND,
                 4,3,PD,NE,07260,AND,
                 4,3,PD,NE,07290,AND,
                 4,3,PD,NE,07310,AND,
                 4,3,PD,NE,07340,AND,
                 4,3,PD,NE,07360,AND,
                 4,3,PD,NE,07370,AND,
                 4,3,PD,NE,07380,AND,
                 4,3,PD,NE,07430,AND,
                 4,3,PD,NE,07450,AND,
                 4,3,PD,NE,07520,AND,
                 4,3,PD,NE,07530,AND,
                 4,3,PD,NE,07540,AND,
                 4,3,PD,NE,07570,AND,
                 4,3,PD,NE,07600,AND,
                 4,3,PD,NE,07630,AND,
                 4,3,PD,NE,07700,AND,
                 4,3,PD,NE,07710,AND,
                 4,3,PD,NE,07720,AND,
                 4,3,PD,NE,07730,AND,
                 4,3,PD,NE,07740,AND,
                 4,3,PD,NE,07750,AND,
                 4,3,PD,NE,07770,AND,
                 4,3,PD,NE,07780,AND,
                 4,3,PD,NE,07790,AND,
                 4,3,PD,NE,07930,AND,
                 4,3,PD,NE,07950,AND,
                 4,3,PD,NE,10510,AND,
                 4,3,PD,NE,17060,AND,
                 4,3,PD,NE,70500,AND,
                 4,3,PD,NE,70900,AND,
                 4,3,PD,NE,71300,AND,
                 4,3,PD,NE,71400,AND,
                 17,4,ZD,EQ,0))
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC SORTD
//*
//* ***    -----------------------------------------------
//* ***    ORGANIZA CADASTRO DE SITUACAO DE CLIENTE.
//* ***    -----------------------------------------------
//*
//SORTIN   DD DSN=MJ.CCMS.S275211.CADASITU(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CADASITU.CLAS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0060,RECFM=FB)
//SORTLIB  DD DSN=SYS1.SORTLIB,
//       DISP=SHR
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK11 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK12 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,7,4,PD,A,22,5,PD,A),FORMAT=BI
 OUTFIL INCLUDE=(47,1,CH,EQ,C'A'),FNAMES=SORTOUT
 END
//*
//STEP3    EXEC PGM=GFCT1731,
//       PARM='220070101'
//*
//* ***    ------------------------------------------------
//* ***    GERAR ARQUIVO CONTENDO CONTAS ATIVAS DO PERIODO.
//* ***    ------------------------------------------------
//*
//REDGAGCT DD DSN=*.STEP1.SORTOUT,
//       DISP=SHR
//CADASITU DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//SELECAO1 DD DSN=MX.GFCT.GFCT1731.SELECAO1(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC SORTD
//*
//* ***    ---------------------------------------
//* ***    SORT ARQUIVO DE FUNCIONARIOS
//* ***    ---------------------------------------
//*
//SORTIN   DD DSN=MJ.CEPT.PRV.ARQFUNC(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.ARQFUNC.CLAS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0009,RECFM=FB)
//SORTLIB  DD DSN=SYS1.SORTLIB,
//       DISP=SHR
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK11 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK12 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,4,4,PD,A),FORMAT=BI
 END
//*
//STEP5    EXEC PGM=GFCT1732
//*
//* ***    ---------------------------------------
//* ***    OBJETIVO: EXCLUI FUNCIONARIOS DO BANCO.
//* ***    ---------------------------------------
//*
//CONTCLIE DD DSN=*.STEP3.SELECAO1,
//       DISP=SHR
//CONTFUNC DD DSN=*.STEP4.SORT.SORTOUT,
//       DISP=SHR
//GRAVCONT DD DSN=MX.GFCT.GFCT1732.SELECAO2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP6    EXEC SORTD
//*
//* ***    ---------------------------------------------
//* ***    SORT ARQUIVO DIRETORES DO BANCO.
//* ***    ---------------------------------------------
//*
//SORTIN   DD DSN=MJ.CBMZ.PRP.CTPTDIRE(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CTPTDIRE.CLAS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0080,RECFM=FB)
//SORTLIB  DD DSN=SYS1.SORTLIB,
//       DISP=SHR
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK11 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK12 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SYSIN    DD *
 SORT FIELDS=(1,5,ZD,A,11,6,ZD,A),FORMAT=BI
 END
//*
//STEP7    EXEC PGM=GFCT1733
//*
//* ***    ---------------------------------------------
//* ***    EXCLUI REG. DIRETORES DO BANCO.
//* ***    ---------------------------------------------
//*
//SELECAO2 DD DSN=*.STEP5.GRAVCONT,
//       DISP=SHR
//CTPTDIRE DD DSN=*.STEP6.SORT.SORTOUT,
//       DISP=SHR
//SELECAO3 DD DSN=MX.GFCT.GFCT1733.SELECAO3(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP8    EXEC SORTD
//*
//* ***    --------------------------------------------
//* ***    SORT ARQUIVO ADESAO A PACOTES - AG / CONTA
//* ***    --------------------------------------------
//*
//SORTIN   DD DSN=MX.GFCT.PRV.ADESOMES(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.ADESOMES.CLAS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//SORTLIB  DD DSN=SYS1.SORTLIB,
//       DISP=SHR
//SORTWK01 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK02 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK03 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK04 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK05 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK06 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK07 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK08 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK09 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK10 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK11 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SORTWK12 DD UNIT=DISCO,
//       SPACE=(CYL,(00800))
//SYSIN    DD *
 SORT FIELDS=(1,3,PD,A,4,4,PD,A),FORMAT=BI
 OUTFIL INCLUDE=(11,3,PD,EQ,1103,OR,
                 11,3,PD,EQ,1104,OR,
                 11,3,PD,EQ,1228),FNAMES=SORTOUT
 END
//*
//STEP9    EXEC PGM=GFCT1734
//*
//* ***    ---------------------------------------------------
//* ***    EXCLUI CLTES PARTICIPANTES DE PACOTES - AG / CONTA
//* ***    ---------------------------------------------------
//*
//SELECAO3 DD DSN=*.STEP7.SELECAO3,
//       DISP=SHR
//ADESOMES DD DSN=*.STEP8.SORT.SORTOUT,
//       DISP=SHR
//SELECAO4 DD DSN=MX.GFCT.GFCT1734.SELECAO4(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//ADPACOTE DD DSN=MX.GFCT.GFCT1734.ADPACOTE(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0009,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP10   EXEC PGM=GFCT1735
//*
//* ***     ------------------------------------
//* ***     INCLUI CPF NO ARQUIVO DE ACIONISTAS.
//* ***     ------------------------------------
//*
//SELECAO4 DD DSN=*.STEP9.SELECAO4,
//       DISP=SHR
//ACOEBBDC DD DSN=MX.GFCT.GFCT9937.ACOEBBDC(0),
//       DISP=SHR
//ACOESCPF DD DSN=MX.GFCT.GFCT1735.ACOEBBDC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0040,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
