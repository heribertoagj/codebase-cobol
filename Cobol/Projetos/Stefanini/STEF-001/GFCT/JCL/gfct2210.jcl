//GFCT2210 JOB 'GFCT,4220,PR14','C085384',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    *---------------------------------------------------------*
//* ***    *  CLIENTES SEM CESTA - PREPARACAO CARGA GFCTB0N9
//* ***    *---------------------------------------------------------*
//*
//STEP1    EXEC SORTD
//*
//* ***    *---------------------------------------------------------*
//* ***    *  SELECIONA DE ADMESCOR
//* ***    *  UMA OCORRENCIA POR CONTA
//* ***    *---------------------------------------------------------*
//*
//SORTIN01 DD DSN=MX.GFCT.PRV.ADMESCOR(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J2210S01.MESCORSL(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,12),
//       SPACE=(TRK,(049500,9900),RLSE),
//       DCB=(MX.A,LRECL=0030,RECFM=FB)
//SYSIN    DD *
 INCLUDE COND=(11,3,PD,EQ,1233,OR,11,3,PD,EQ,1236)
 MERGE FIELDS=(1,3,A,4,4,A),FORMAT=BI
 SUM FIELDS=NONE
 END
//*
//STEP2    EXEC DB2M1HPU,
//       UID='GFCT2210'
//*
//* ***    ************************************************************
//* ***    HPU - DESCARREGA A TABELA GFCTB0N9 EM ARQ.SEQUENCIAL.
//* ***    ************************************************************
//* ***
//* ***    A TABELA N9 TEVE SEU INDICE PRIMARIO ALTERADO, PERMITINDO
//* ***    QUE UMA MESMA CONTA (CLUB) ESTEJA EM MAIS DE UM GERENTE
//* ***
//*
//SYSPUNCH DD DSN=MX.GFCT.J2210S02.SYSPUNCH.GFCTB0N9(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(100,10),RLSE),
//       DCB=(MX.A),
//       DATACLAS=PRODX37
//SYSREC00 DD DSN=MX.GFCT.J2210S02.SYSREC00.GFCTB0N9(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,59),
//       SPACE=(TRK,(045000,45000),RLSE),
//       DCB=(MX.A),
//       DATACLAS=PRODX37
//SYSIN    DD *
  UNLOAD TABLESPACE GFCTD000.GFCTS0N9
  DB2 NO
  QUIESCE NO
  SELECT *
  FROM DB2PRD.TCLI_SEM_CESTA
  OUTDDN (SYSREC00)
  FORMAT DSNTIAUL
  LOADDDN SYSPUNCH
//SYSOUT   DD SYSOUT=*
//LISTING  DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
