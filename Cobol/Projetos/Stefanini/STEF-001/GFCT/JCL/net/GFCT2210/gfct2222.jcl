//GFCT2222 JOB 'GFCT,4220,PR14','C085384',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    *---------------------------------------------------------*
//* ***    *  CLIENTES SEM CESTA - PREPARACAO CARGA GFCTB0N9
//* ***    *---------------------------------------------------------*
//*
//STEP1    EXEC PGM=GFCT4420
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM CLIE
//* ***    *  GERA LAYOUT 404 POSICOES - GFCTWBD0
//* ***    *  DD CADACLIE COMPRIMIDO - LIDO VIA POOL0300
//* ***    *  DESPREZA CASO NAO EXISTA NO CLIE
//* ***    *---------------------------------------------------------*
//*
//ARQSYSIN DD DSN=MX.GFCT.PRP.CTRLDATA(0),
//       DISP=SHR
//CADACLIE DD DSN=MJ.CLIE.PRP.CADNGERA.ALFA(0),
//       DISP=SHR
//ADMESCOR DD DSN=MX.GFCT.J2210S01.MESCORSL(0),
//       DISP=SHR
//ADESAOSC DD DSN=MX.GFCT.J2222S01.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,50),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//DESPREZA DD DSN=MX.GFCT.J2222S01.DESPREZA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,50),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP2    EXEC PGM=GFCT4422
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM CONTAS DE FUNCIONARIOS
//* ***    *  DESPREZA CASO SEJA CONTA DE FUNCIONARIO
//* ***    *---------------------------------------------------------*
//*
//ARQFUNC  DD DSN=MJ.CEPT.PRV.ARQFUNC(0),
//       DISP=SHR
//ADMESCOR DD DSN=*.STEP1.ADESAOSC,
//       DISP=OLD
//ADESAOSC DD DSN=MX.GFCT.J2222S02.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,50),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//DESPREZA DD DSN=MX.GFCT.J2222S02.DESPREZA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,05),
//       SPACE=(TRK,(015180,3036),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP3    EXEC PGM=GFCT4424
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM CONTAS TRAGUEADAS
//* ***    *  DESPREZA CASO SEJA CONTA TRAGUEADA
//* ***    *---------------------------------------------------------*
//*
//ARQUTRAG DD DSN=MJ.TRAG.PRP.TRANSCOP.A2000(0),
//       DISP=SHR
//ADMESCOR DD DSN=*.STEP2.ADESAOSC,
//       DISP=OLD
//ADESAOSC DD DSN=MX.GFCT.J2222S03.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,40),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//DESPREZA DD DSN=MX.GFCT.J2222S03.DESPREZA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,06),
//       SPACE=(TRK,(023290,04658),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP4    EXEC PGM=GFCT4426
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM CONTAS INATIVAS
//* ***    *  DESPREZA CASO SEJA CONTA INATIVA
//* ***    *---------------------------------------------------------*
//*
//CTAINATI DD DSN=MX.GFCT.J2212S01.CTAINATI(0),
//       DISP=SHR
//ADMESCOR DD DSN=*.STEP3.ADESAOSC,
//       DISP=OLD
//ADESAOSC DD DSN=MX.GFCT.J2222S04.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//DESPREZA DD DSN=MX.GFCT.J2222S04.DESPREZA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,40),
//       SPACE=(TRK,(099900,19980),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5    EXEC PGM=GFCT4428
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM CONTAS SALARIO
//* ***    *  INCLUI INFORMCAO DE CONTA SALARIO (S/N)
//* ***    *  DESPREZA CASO CONTA SALARIO BLOQUEADA
//* ***    *---------------------------------------------------------*
//*
//CTASALAR DD DSN=MX.GFCT.J2214S01.CTASALAR(0),
//       DISP=SHR
//ADMESCOR DD DSN=*.STEP4.ADESAOSC,
//       DISP=OLD
//ADESAOSC DD DSN=MX.GFCT.J2222S05.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//DESPREZA DD DSN=MX.GFCT.J2222S05.DESPREZA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,05),
//       SPACE=(TRK,(015180,3036),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP5A   EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  PREPARA ARQUIVO DE ADESOES
//* ***    *---------------------------------------------------------*
//*
//SORTIN   DD DSN=*.STEP5.ADESAOSC,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2222S5A.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(39,9,A),FORMAT=BI
 END
//*
//STEP6    EXEC PGM=GFCT4430
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM RENTABILIDADE
//* ***    *---------------------------------------------------------*
//*
//ARQURENC DD DSN=MX.GFCT.J2216S01.ARQURENC(0),
//       DISP=SHR
//ADMESCOR DD DSN=*.STEP5A.SORT.SORTOUT,
//       DISP=OLD
//ADESAOSC DD DSN=MX.GFCT.J2222S06.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP6A   EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  PREPARA ARQUIVO DE ADESOES
//* ***    *---------------------------------------------------------*
//*
//SORTIN   DD DSN=*.STEP6.ADESAOSC,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2222S6A.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(22,5,A,27,13,A),FORMAT=BI
 END
//*
//STEP7    EXEC PGM=GFCT4432
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM CADU PARA DETERMINAR CLUB
//* ***    *  DD CADUCLUB COMPRIMIDO - LIDO VIA POOL0300
//* ***    *  DESPREZA CASO NAO ENCONTRE NO CADU
//* ***    *---------------------------------------------------------*
//*
//ADMESCOR DD DSN=*.STEP6A.SORT.SORTOUT,
//       DISP=OLD
//CADUCLUB DD DSN=MX.GFCT.J2218S01.CADUB0A3(0),
//       DISP=OLD
//ADESAOSC DD DSN=MX.GFCT.J2222S07.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//DESPREZA DD DSN=MX.GFCT.J2222S07.DESPREZA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,05),
//       SPACE=(TRK,(015180,3036),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP7A   EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  PREPARA ARQUIVO DE ADESOES
//* ***    *---------------------------------------------------------*
//*
//SORTIN   DD DSN=*.STEP7.ADESAOSC,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2222S7A.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(190,11,A,21,5,A),FORMAT=BI
 END
//*
//STEP8    EXEC PGM=GFCT4434
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM ARQUIVO DE CLUB'S BLOQUEADOS
//* ***    *  INCLUI INFORMACAO DO PROCON
//* ***    *---------------------------------------------------------*
//*
//ADMESCOR DD DSN=*.STEP7A.SORT.SORTOUT,
//       DISP=OLD
//CLUBBLOQ DD DSN=MX.PSCE.PRV.JI010S06.CLUBBLOQ(0),
//       DISP=SHR
//ADESAOSC DD DSN=MX.GFCT.J2222S08.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP9    EXEC PGM=GFCT4438
//*
//* ***    *---------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM CADU PARA DETERMINAR TELEFONE
//* ***    *---------------------------------------------------------*
//*
//ADMESCOR DD DSN=*.STEP8.ADESAOSC,
//       DISP=OLD
//CLUBTELE DD DSN=MX.GFCT.J2220S03.INFOFONE(0),
//       DISP=OLD
//ADESAOSC DD DSN=MX.GFCT.J2222S09.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP10   EXEC PGM=GFCT4440
//*
//* ***    *-------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM GERENTES PARA DETERMINAR GER. DA CTA
//* ***    *-------------------------------------------------------*
//*
//ADMESCOR DD DSN=*.STEP9.ADESAOSC,
//       DISP=OLD
//CLUBGERE DD DSN=MX.GFCT.J2221S10.CLUBGSAI(0),
//       DISP=SHR
//ADESAOSC DD DSN=MX.GFCT.J2222S10.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//DESPREZA DD DSN=MX.GFCT.J2222S10.DESPREZA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,05),
//       SPACE=(TRK,(015180,3036),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP11   EXEC SORTD
//*
//* ***    *-------------------------------------------------------*
//* ***    *  CLASSIFICA ADMESCOR POR AG/CTA
//* ***    *-------------------------------------------------------*
//*
//SORTIN   DD DSN=*.STEP10.ADESAOSC,
//       DISP=OLD
//SORTOUT  DD DSN=MX.GFCT.J2222S11.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSIN    DD *
 SORT FIELDS=(21,5,A,26,13,A),FORMAT=BI
 END
//*
//STEP12   EXEC PGM=GFCT4442
//*
//* ***    *-------------------------------------------------------*
//* ***    *  BATE ADMESCOR COM INVESTIMENTOS
//* ***    *-------------------------------------------------------*
//*
//ADMESCOR DD DSN=*.STEP11.SORT.SORTOUT,
//       DISP=OLD
//CADAINVE DD DSN=IM.YMIV.PRV.YMIVSSDO(0),
//       DISP=SHR
//ADESAOSC DD DSN=MX.GFCT.J2222S12.ADESAOSC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,16),
//       SPACE=(TRK,(055000,11000),RLSE),
//       DCB=(MX.A,LRECL=0404,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP13   EXEC PGM=GFCT4444
//*
//* ***    *-------------------------------------------------------*
//* ***    *  CRIA IMAGEM DA TABELA GFCTB0N9 PARA CARGA
//* ***    *-------------------------------------------------------*
//*
//ADMESCOR DD DSN=*.STEP12.ADESAOSC,
//       DISP=OLD
//IMAGB0N9 DD DSN=MX.GFCT.J2222S13.IMAGB0N9(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,08),
//       SPACE=(TRK,(038960,07792),RLSE),
//       DCB=(MX.A,LRECL=0220,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
