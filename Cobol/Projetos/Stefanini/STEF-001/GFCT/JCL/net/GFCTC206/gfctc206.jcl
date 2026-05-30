//GFCTC206 JOB 'GFCT,4008,PR14','I391162',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ------------------------------------------------------------
//* ***    TEST DRIVE - REPRESENTANTES AVON
//* ***    ------------------------------------------------------------
//*
//STEP01   EXEC PGM=PARM4000
//*
//* ***    ------------------------------------------------------------
//* ***    TEST DRIVE CESTAS - TAREFA PARM SCMP0024
//* ***    ------------------------------------------------------------
//*
//ENTRADA  DD *
SCMP0024    0000
//REGISTRO DD DSN=MX.GFCT.JC206S01.REGISTRO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0255,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP02   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    CLASSIFICA ARQUIVO DE PARAMETROS POR
//* ***    CESTAS CONVENIO AVON
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP01.REGISTRO,
//       DISP=OLD
//SORTOUT  DD DSN=MX.GFCT.JC206S02.CARENCIA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0255,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=COPY
  INCLUDE COND=(24,2,ZD,EQ,66)
  END
//*
//STEP03   EXEC DB2M1HPU
//*
//* ***    ------------------------------------------------------------
//* ***    DESCARGA DE TABELA SCCE EM ARQUIVO SEQUENCIAL
//* ***    UNLOAD DA TABELA DB2PRD.V01CTA_SERVC_ESPEC
//* ***    ------------------------------------------------------------
//*
//SYSPUNCH DD DUMMY
//SYSREC00 DD DSN=MX.GFCT.JC206S03.CONTAVON(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(50000,10000),RLSE),
//       DCB=(MX.A,LRECL=0038,RECFM=FB)
//SYSIN    DD *
  UNLOAD TABLESPACE SCCED000.SCCES041
  DB2 NO
  QUIESCE NO
  SELECT  CJUNC_DEPDC,CCTA_CORR,
          DINIC_VALDD_SERVC, DFIM_VALDD_SERVC,
          CPARM_GNRCO, CSERVC_ESPEC
  FROM    DB2PRD.V01CTA_SERVC_ESPEC
  WHERE   CSERVC_ESPEC = 66
    AND   DFIM_VALDD_SERVC >= CURRENT DATE
  OUTDDN (SYSREC00)
  LOADDDN SYSPUNCH
  FORMAT  DSNTIAUL
//SYSOUT   DD SYSOUT=*
//LISTING  DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP04   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    CLASSIFICA POR AGENCIA / CONTA
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP03.HPU.SYSREC00,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JC206S04.CONTAVON(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(50000,20000),RLSE),
//       DCB=(MX.A,LRECL=0038,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
  SORT FIELDS=(1,3,A,4,4,A),FORMAT=PD
  END
//*
//STEP05   EXEC PGM=GFCT6850
//*
//* ***    ------------------------------------------------------------
//* ***    GERAR ARQUIVO DE TEST DRIVE CESTAS AVON
//* ***    DADOS DO CLIENTE
//* ***    ------------------------------------------------------------
//*
//CTAAVONE DD DSN=*.STEP04.SORT.SORTOUT,
//       DISP=SHR
//CADACLIE DD DSN=MJ.CLIE.PRP.CADNGERA.ALFA(0),
//       DISP=SHR
//CTAAVONS DD DSN=MX.GFCT.JC206S05.CONTCLIE(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE),
//       DCB=(MX.A,LRECL=0200,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP06   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    BALANCE LINE ENTRE ARQUIVOS:
//* ***    RTADESAO  X ARQUIVO CONTAS AVON
//* ***    AGREGA CODIGO DA CESTAS DE SERVICO
//* ***    ------------------------------------------------------------
//*
//IN2      DD DSN=*.STEP05.CTAAVONS,
//       DISP=SHR
//IN1      DD DSN=MX.GFCT.PRP.RTADESAO(0),
//       DISP=SHR
//OUT      DD DSN=MX.GFCT.JC206S06.REGT.MATCH(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE)
//OUT1     DD DSN=MX.GFCT.JC206S06.REGT.NMATC(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE)
//SYSIN    DD *
  JOINKEYS F1=IN1,FIELDS=(1,05,A,06,07,A)
  JOINKEYS F2=IN2,FIELDS=(1,05,A,06,07,A)
  JOIN UNPAIRED,F2
  REFORMAT FIELDS=(?,F2:1,101,F1:13,5,F2:107,94) --> REGISTRO LIMPO
  OUTFIL FNAMES=OUT,                             --> MATCH
        INCLUDE=(01,01,CH,EQ,C'B'),
         OUTREC=(02,200)
  OUTFIL FNAMES=OUT1,                            --> NO MATCH
        INCLUDE=(01,01,CH,EQ,C'2'),
         OUTREC=(02,200)
  SORT FIELDS=COPY
  END
//JNF1JMSG DD SYSOUT=*
//JNF2JMSG DD SYSOUT=*
//*
//STEP07   EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    CLASSIFICA O ARQUIVO SEGMEATU POR:
//* ***    AGENCIA, CONTA E SEGMENTO.
//* ***    ------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP06.SORT.OUT,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JC206S07.PCTAT.CLAS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE),
//       DCB=(MX.A,LRECL=0200,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(1,5,ZD,A,6,7,ZD,A)
  END
//*
//STEP08   EXEC PGM=GFCT6855
//*
//* ***    ------------------------------------------------------------
//* ***    GERAR ARQUIVO DE SERVICO ATUALIZADO E EMITIR RELATORIO
//* ***    DAS CONTAS QUE SAIRAM DO ARQUIVO DE SERVICO.
//* ***    REGRA:  - SO SEGMEATU -> GERA NOVO
//* ***            - SO SEGMEANT -> REGRAVA ANTERIOR
//* ***            - EM AMBOS    -> REGRAVA ANTERIOR FORCANDO
//* ***                             ZEROS NA CESTA DE SERVICOS
//* ***    ------------------------------------------------------------
//*
//SEGMEATU DD DSN=*.STEP07.SORT.SORTOUT,
//       DISP=SHR
//SEGMEANT DD DSN=MX.GFCT.PRV.PCTEAVON(0),
//       DISP=OLD
//SEGMOVTO DD DSN=MX.GFCT.PRV.PCTEAVON(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE),
//       DCB=(MX.A,LRECL=0200,RECFM=FB)
//RELATO   DD DSN=MX.GFCT.JC206S08.RELATO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0133,RECFM=FBA)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP09   EXEC PGM=GFCT6865
//*
//* ***    ------------------------------------------------------------
//* ***    GERAR ARQUIVO BASE PARA TESTE DRIVE.
//* ***    UTILIZANDO APENAS UMA CONTA POR CPF
//* ***    COM MENOR DATA DE INGRESSO NO SEGMENTO
//* ***    ------------------------------------------------------------
//*
//ARQSEGTO DD DSN=*.STEP02.SORT.SORTOUT,
//       DISP=SHR
//SEGMOVTO DD DSN=*.STEP08.SEGMOVTO,
//       DISP=SHR
//ARQDRIVE DD DSN=MX.GFCT.JC206S09.TESTDRIV(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE),
//       DCB=(MX.A,LRECL=0200,RECFM=FB)
//ARQDESPR DD DSN=MX.GFCT.JC206S09.ARQDESAV(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE),
//       DCB=(MX.A,LRECL=0200,RECFM=FB)
//RELDRIVE DD DSN=MX.GFCT.JC206S09.REL0605C(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0133,RECFM=FBA)
//RELDESPR DD DSN=MX.GFCT.JC206S09.REL0605D(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0133,RECFM=FBA)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP10   EXEC PGM=GFCT6875
//*
//* ***    ------------------------------------------------------------
//* ***    SELECIONA SOMENTE CLIENTES COM VALIDADE ATIVA
//* ***    PARA CONCEDER BENEFICIOS DO TESTE DRIVE
//* ***    ------------------------------------------------------------
//*
//CADTESTD DD DSN=*.STEP09.ARQDRIVE,
//       DISP=SHR
//ISETESTD DD DSN=MX.GFCT.PRV.ISENAVON(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,03),
//       SPACE=(TRK,(50000,20000),RLSE),
//       DCB=(MX.A,LRECL=0200,RECFM=FB)
//RELATO   DD DSN=MX.GFCT.JC206S10.REL6875(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(020000,8000),RLSE),
//       DCB=(MX.A,LRECL=0133,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
