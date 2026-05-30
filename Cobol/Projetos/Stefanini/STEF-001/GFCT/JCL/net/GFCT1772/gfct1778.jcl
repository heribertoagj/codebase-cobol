//GFCT1778 JOB 'GFCT,4008,PR14','D115848',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP1    EXEC DB2M1HPU
//*
//* ***    ************************************************************
//* ***    HPU - DESCARREGA A TABELA GFCTB050 EM ARQ.SEQUENCIAL.
//* ***    ************************************************************
//*
//SYSPUNCH DD DUMMY
//SYSREC00 DD DSN=MX.GFCT.J1778S01.HPU.GFCTB050(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(030000,6000),RLSE),
//       DCB=(MX.A)
//SYSIN    DD *
  UNLOAD TABLESPACE GFCTD000.GFCTS050
  DB2 NO
  QUIESCE YES
  SELECT  CSERVC_TARIF,
          CAGPTO_CTA,
          HIDTFD_FLEXZ_AGPTO,
          CCGC_CPF,
          CFLIAL_CGC,
          CCTRL_CPF_CGC,
          CCGC_CPF_ST,
          CFLIAL_CGC_ST,
          CCTRL_CPF_CGC_ST          
  FROM    DB2PRD.FLEXZ_GRP_CLI
  FORMAT  DSNTIAUL
  LOADDDN SYSPUNCH
  OUTDDN ( SYSREC00 )
//*
//STEP2    EXEC SORTD
//*
//* ***    *************************************************************
//* ***    CLASSIFICA O ARQUIVO GFCTB050 POR:
//* ***    GFCTWE02-CCGC-CPF, GFCTWE02-CFLIAL-CGC,
//* ***    GFCTWE02-CCTRL-CPF-CGC, GFCTWE02-CSERVC-TARIF
//* ***    *************************************************************
//*
//SORTIN   DD DSN=*.STEP1.HPU.SYSREC00,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J1778S02.HPUS.GFCTB050(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(030000,6000),RLSE),
//       DCB=(MX.A,LRECL=0056,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(32,5,A,37,3,A,40,2,A,1,3,A),FORMAT=PD
  END
//*
//STEP3    EXEC SORTD
//*
//* ***    *************************************************************
//* ***    CLASSIFICA O ARQUIVO GFCTTRPN POR:
//* ***    WE01-CPF-CNPJ-PRINC, WE01-CPF-CNPJ-FILIAL,
//* ***    WE01-CPF-CNPJ-CONTR, WE01-CSERVC-TARIF-GFCT
//* ***    *************************************************************
//*
//SORTIN   DD DSN=MX.GFCT.J1776S05.TRPNFLX1(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.J1778S03.SAIDTRPN(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(030000,6000),RLSE),
//       DCB=(MX.A,LRECL=0146,RECFM=FB)
//SYSIN    DD *
  SORT FIELDS=(10,9,A,19,5,A,24,3,A,54,5,A),FORMAT=ZD
  END
//*
//STEP4    EXEC PGM=GFCT4805
//*
//* ***    *************************************************************
//* ***    BALANCE-LINE PARA OBTER FLEXIBILIZACAO CPF CNPJ
//* ***    *************************************************************
//*
//GFCTB050 DD DSN=*.STEP2.SORT.SORTOUT,
//       DISP=SHR
//GFCTTRPN DD DSN=*.STEP3.SORT.SORTOUT,
//       DISP=SHR
//GFCTTRPS DD DSN=MX.GFCT.SAIDTRPN(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(030000,6000),RLSE),
//       DCB=(MX.A,LRECL=0146,RECFM=FB)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*# //*
//*# //STEP4A    EXEC PGM=ICETOOL
//*# //*
//*# //* ***    LISTA ARQUIVO FLEX. CNPJ
//*# //*
//*# //ENTRADA  DD DSN=*.STEP4.GFCTTRPS,
//*# //       DISP=SHR
//*# //TRPNFLX2 DD DUMMY,
//*# //       DCB=(LRECL=122,RECFM=FB)
//*# //TOOLIN   DD *
//*#  DISPLAY FROM(ENTRADA) LIST(TRPNFLX2) -
//*#  TITLE('         BANCO BRADESCO S/A') DATE(DM4.) PAGE -
//*#  TITLE('    GFCT4800 - GESTAO, FLEXIB. E COBR. DE TARIFAS') -
//*#  TITLE('         MOVIMENTO TRPN - COM FLEX CNPJ') -
//*#  BETWEEN(1) -
//*#  HEADER('AGEN') ON(27,5,ZD) -
//*#  HEADER('CONTA') ON(32,13,ZD) -
//*#  HEADER('CNPJ') ON(10,9,ZD) -
//*#  HEADER('FILIAL') ON(19,5,ZD) -
//*#  HEADER('CONTROLE') ON(24,3,ZD) -
//*#  HEADER('TARIFA') ON(54,5,ZD) -
//*#  HEADER('DT.BASE') ON(59,10,CH) -
//*#  HEADER('VR.BRUTO') ON(69,13,ZD,F2) -
//*#  HEADER('CESTA') ON(82,5,ZD) -
//*#  HEADER('CD.RET') ON(87,2,ZD) -
//*#  HEADER('CIND2') ON(118,1,CH) -
//*#  HEADER('PER2') ON(119,1,CH) -
//*#  HEADER('QFR2') ON(120,3,ZD) -
//*#  HEADER('VRNG2') ON(123,13,ZD,F2) -
//*#  HEADER('PDESC2') ON(136,11,ZD,G2) -
//*#  BLANK -
//*#  COUNT(' - REGISTROS TRPN COM FLEXIB. CPF/CNPJ --->>> ')
//*# //SYSOUT   DD SYSOUT=*
//*# //DFSMSG   DD SYSOUT=*
//*# //TOOLMSG  DD SYSOUT=*
//*# //SYSUDUMP DD SYSOUT=Y
//*# //*
//*# //STEP5    EXEC PGM=ICETOOL
//*# //*
//*# //* ***    LISTA ARQUIVO COM FLEXIBILIZACOES
//*# //*
//*# //ENTRADA  DD DSN=*.STEP4.GFCTTRPS,
//*# //       DISP=SHR
//*# //TRPNFLEX DD DUMMY,
//*# //       DCB=(LRECL=122,RECFM=FB)
//*# //TOOLIN   DD *
//*#  DISPLAY FROM(ENTRADA) LIST(TRPNFLEX) -
//*#  TITLE('         BANCO BRADESCO S/A') DATE(DM4.) PAGE -
//*#  TITLE('    GFCT4800 - GESTAO, FLEXIB. E COBR. DE TARIFAS') -
//*#  TITLE('         MOVIMENTO TRPN - FLEXIBILIZACOES') -
//*#  BETWEEN(1) -
//*#  HEADER('AGEN') ON(27,5,ZD) -
//*#  HEADER('CONTA') ON(32,13,ZD) -
//*#  HEADER('CNPJ') ON(10,9,ZD) -
//*#  HEADER('FILIAL') ON(19,5,ZD) -
//*#  HEADER('CONTROLE') ON(24,3,ZD) -
//*#  HEADER('TARIFA') ON(54,5,ZD) -
//*#  HEADER('DT.BASE') ON(59,10,CH) -
//*#  HEADER('VR.BRUTO') ON(69,13,ZD,F2) -
//*#  HEADER('CESTA') ON(82,5,ZD) -
//*#  HEADER('CD.RET') ON(87,2,ZD) -
//*#  HEADER('CIND1') ON(89,1,CH) -
//*#  HEADER('PER1') ON(90,1,CH) -
//*#  HEADER('QFR1') ON(91,3,ZD) -
//*#  HEADER('VRNG1') ON(94,13,ZD,F2) -
//*#  HEADER('PDESC1') ON(107,11,ZD,G2) -
//*#  HEADER('CIND2') ON(118,1,CH) -
//*#  HEADER('PER2') ON(119,1,CH) -
//*#  HEADER('QFR2') ON(120,3,ZD) -
//*#  HEADER('VRNG2') ON(123,13,ZD,F2) -
//*#  HEADER('PDESC2') ON(136,11,ZD,G2) -
//*#  BLANK -
//*#  COUNT(' - REGISTROS TRPN COM FLEXIBILIZACOES --->>> ')
//*# //SYSOUT   DD SYSOUT=*
//*# //DFSMSG   DD SYSOUT=*
//*# //TOOLMSG  DD SYSOUT=*
//*# //SYSUDUMP DD SYSOUT=Y
//*
//STEP6    EXEC PGM=PLAN0260
//*
//* ***    ------------------------------------------------------------
//* ***    IDENTIFICA SE VAZIO RETURN-CODE = 4, GERA ARQUIVO VAZIO
//* ***    SENAO LIBERA PARA PROXIMO STEP
//* ***    ------------------------------------------------------------
//*
//LEITURA  DD DSN=*.STEP4.GFCTTRPS,
//       DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP7    EXEC PGM=SBAT8000,
//       COND=(0,NE,STEP6)
//*
//* ***    ------------------------------------------------------------
//* ***    ENVIA DADOS NAO OK PARA GESTOR - ARQUIVO TRPN
//* ***    ------------------------------------------------------------
//*
//ARQTRANS DD DSN=*.STEP4.GFCTTRPS,
//       DISP=SHR
//PARM     DD *
04008,GFCTTRPN,TXT
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
