//GFCTA522 JOB 'GFCT,4220,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//*
//* ***    ==========================================================
//* ***    TRATAMENTO DE RETORNO DO ARQUIVO DO IRES DE NEGATIVACAO
//* ***    ==========================================================
//*
//STEP0    EXEC PGM=ATSOAC00,
//       PARM='00-ICETOOL'
//*
//* ***    SALVA ARQUIVOS DE CONTROLE ANTES DA ATUALIZACAO
//* ***    RETORNO DE MOVIMENTO PARA CONSULTA NO IRES
//*
//ENT00001 DD DSN=MX.GFCT.PRP.ENVIIRES(0),
//       DISP=SHR
//SAI00001 DD DSN=MX.GFCT.EVNIIRA2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,14),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//TOOLMSG  DD SYSOUT=*
//TOOLIN   DD *
 COPY FROM(ENT00001) TO(SAI00001)
//DFSMSG   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP1    EXEC SORTD
//*
//* ***    -----------------------------
//* ***    CLASSIFICA RETORNO DO IRES
//* ***    -----------------------------
//*
//SORTIN   DD DSN=MX.GFCT.IRESNPJR(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.RETNIRES(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,14),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0130,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(1,47,A),FORMAT=BI
  SUM  FIELDS=NONE
  END
//*
//STEP2    EXEC PGM=GFCT4114
//*
//* ***    ------------------------------------------------------------
//* ***    ATUALIZA CADASTRO DE ENVIADOS PARA IRES
//* ***    PREPARA IMAGEM PARA CARGA GFCTB0N2-DB2PRD.TCTRL_NGTVC_CLI
//* ***    -------------------------------------------------------------
//*
//MOVIMENT DD DSN=*.STEP1.SORT.SORTOUT,
//       DISP=SHR
//CADASTRO DD DSN=MX.GFCT.PRP.ENVIIRES(0),
//       DISP=OLD
//CADASATU DD DSN=MX.GFCT.PRP.ENVIIRES(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,14),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0130,RECFM=FB)
//CARGA0N2 DD DSN=MX.GFCT.CARGA0N2(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,14),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0130,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//STEP3    EXEC SORTD
//*
//* ***    ------------------------------------------------------------
//* ***    SELECIONA UM REGISTRO POR CHAVE
//* ***    PARA CARGA DA N2
//* ***    -------------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP2.CARGA0N2,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.CARGAN2C(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=(DISCO,14),
//       SPACE=(TRK,(020000,4000),RLSE),
//       DCB=(MX.A,LRECL=0130,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(1,22,A),FORMAT=BI
  SUM FIELDS=NONE
  END
//*
