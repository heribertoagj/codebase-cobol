//GFCTN555 JOB 'GFCT,4008,PR14','B460076',MSGCLASS=Z,SCHENV=BATCH
//JOBLIB   DD DSN=MX.BIBGERAL,DISP=SHR
//         DD DSN=MJ.BIBGERAL,DISP=SHR
//         DD DSN=DB2M1.R2.DSNLOAD,DISP=SHR
//         DD DSN=SYS1.CEE.SCEERUN,DISP=SHR
//STEP01   EXEC DB2M1HPU
//* ***   *
//* ***   ******************************************************
//* ***   *   BAIXA DA TABELA GFCTB0J8 - TUTILZ_COMPO_PCOTE    *
//* ***   ******************************************************
//*
//SYSPUNCH DD DUMMY
//SYSREC00 DD DSN=MX.GFCT.JN555S01.ARQCOMPO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A),
//       DATACLAS=PRODX37
//SYSIN    DD *
 UNLOAD TABLESPACE GFCTD000.GFCTS0J8
 DB2 NO
 QUIESCE NO
            SELECT CBCO
                  ,CAG_BCRIA
                  ,CCTA_BCRIA_CLI
                  ,DCOMPT_FCHTO_TARIF
                  ,CSERVC_TARIF
                  ,CTPO_COMPO_PCOTE
                  ,CCOMPO_PCOTE
                  ,CSIT_PER_UTILZ
                  ,QUTILZ_CONTT_COMPO
                  ,QUTILZ_EXCED_COMPO
                  ,VUNTAR_EXCED_CONTT
                  ,VINIC_EXCED_COMPO
                  ,VFNAL_EXCED_COMPO
                  ,QUTILZ_COMPO_TARIF
              FROM DB2PRD.TUTILZ_COMPO_PCOTE
              WHERE DCOMPT_FCHTO_TARIF =
                   YEAR(CURRENT DATE) * 100 + MONTH(CURRENT DATE)
 OUTDDN (SYSREC00)
 FORMAT DSNTIAUL
 LOADDDN SYSPUNCH
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP1A   EXEC SORTD
//*
//* ***    BAIXA DA TABELA GFCTB0J8
//* ***    SORT POR AGENCIA, CONTA, FRANQUIA
//*
//SORTIN   DD DSN=*.STEP01.HPU.SYSREC00,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JN555S1A.ARQCOMPO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=57,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(3,03,PD,A,6,07,PD,A,21,03,PD,A)
 END
//*
//STEP02   EXEC SORTD
//*
//* ***    SORT POR AGENCIA, CONTA E FRANQUIA
//*
//SORTIN   DD DSN=MX.GFCT.PRV.ARQSEMCA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JN555S02.ARQSEMC1(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=200,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(21,03,PD,A,24,07,PD,A,45,03,PD,A)
 END
//*
//STEP03   EXEC PGM=GFCT2BOC
//*
//* ***    ----------------------------------------------------
//* ***    ADICIONAR AS TARIFAS AVULSAS AO ARQUIVO DO GFCT
//* ***    ----------------------------------------------------
//*
//ARQCOMPO DD DSN=*.STEP1A.SORT.SORTOUT,
//       DISP=SHR
//ARQSEMCA DD DSN=*.STEP02.SORT.SORTOUT,
//       DISP=SHR
//ARQCOMPT DD DSN=MX.GFCT.MVGFCT.ARQCOMPT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0096,RECFM=FB),
//       DATACLAS=PRODX37
//ARQCOMPA DD DSN=MX.GFCT.JN555S03.ARQCOMPA(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0096,RECFM=FB),
//       DATACLAS=PRODX37
//REGSCEPT DD DSN=MX.GFCT.JN555S03.REGSCEPT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0087,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP04   EXEC PGM=GFCT2BOD
//*
//* ***    ----------------------------------------------------
//* ***  * RELATORIO DE NOVAS TARIFAS AVULSAS
//* ***    ----------------------------------------------------
//*
//ARQENTRA DD DSN=*.STEP03.ARQCOMPT,
//       DISP=SHR
//ARQSAIDA DD DSN=MX.GFCT.JN555S04.ARQSEMCS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0300,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP05   EXEC PGM=GFCT2BOD
//*
//* ***    ----------------------------------------------------
//* ***  * RELATORIO DE NOVAS TARIFAS AVULSAS
//* ***    ----------------------------------------------------
//*
//ARQENTRA DD DSN=*.STEP03.ARQCOMPA,
//       DISP=SHR
//ARQSAIDA DD DSN=MX.GFCT.JN555S05.ARQSEMCS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=0300,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//* ***    ----------------------------------------------------
//* ***  * TRATAMENTO DO ARQUIVO PARA O CEPT
//* ***    ----------------------------------------------------
//*
//STEP06   EXEC SORTD
//*
//* ***    ----------------------------------------------------
//* ***  * ARQUIVO CONTENDO  CSIST = CEPT
//* ***    ----------------------------------------------------
//* ***  * CAG_BCRIA_SALRL     (03,03,PD,A)  BOOK GFCTWBKD
//* ***  * CCTA_BCRIA_SALRL    (06,07,PD,A)
//* ***  * NFLEXZ_TARIF_AVULS  (46,06,PD,A)
//* ***  * CGRP_SERVC          (13,03,PD,A)
//* ***    ----------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP03.REGSCEPT,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JN555S06.SORTOUT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(03,03,PD,A,06,07,PD,A,46,06,PD,A,13,03,PD,A)
 END
//*
//STEP07   EXEC PGM=GFCT2BVC
//*
//* ***    -----------------------------------------------------
//* ***    TRATAMENTO TARIFAS AVULSAS
//* ***    -----------------------------------------------------
//*
//ADETARAV DD DSN=*.STEP06.SORT.SORTOUT,
//       DISP=SHR
//RTADESAO DD DSN=MX.GFCT.JN555S07.RTADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB),
//       DATACLAS=PRODX37
//INCONSIS DD DSN=MX.GFCT.JN555S07.INCONSIS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=87,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP08   EXEC SORTD
//*
//* ***    ----------------------------------------------------------
//* ***  * ARQUIVO DE NEG VIGENTES APOS BATIMENTO
//* ***    ----------------------------------------------------------
//* ***  * AGENCIA         (03,03,PD,A)             BOOK - GFCTWBKE
//* ***  * CONTA           (06,07,PD,A)
//* ***  * CGRP_SERVC      (13,03,PD,A)
//* ***    ----------------------------------------------------------
//*
//SORTIN   DD DSN=*.STEP07.RTADESAO,
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JN555S08.RTADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(03,03,PD,A,06,07,PD,A,13,03,PD,A)
 END
//*
//STEP09   EXEC PGM=GFCT2BVD
//*
//* ***    -----------------------------------------------------
//* ***    GERAR ARQUIVO COM LAYOUT PARA CEPT - TARIFAS AVULSAS
//* ***    -----------------------------------------------------
//*
//ADETARAV DD DSN=*.STEP08.SORT.SORTOUT,
//       DISP=SHR
//RTADESAO DD DSN=MX.GFCT.JN555S09.RTADESAO(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
//STEP10   EXEC SORTD
//*
//* ***    ----------------------------------------------------------
//* ***  * ARQUIVO DO CLIE CONTENDO GRUPO, SUBGRUPO, AGENCIA E CONTA
//* ***    ----------------------------------------------------------
//* ***  * AGENCIA CLIE    (01,03,PD,A) - BOOK I#CHEQ04
//* ***  * CONTA CLIE      (07,04,PD,A)
//* ***    ----------------------------------------------------------
//*
//SORTIN   DD DSN=MJ.CLIE.PRP.REDZAGCT.ALFA(0),
//       DISP=SHR
//SORTOUT  DD DSN=MX.GFCT.JN555S10.SORTOUT(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=29,RECFM=FB),
//       DATACLAS=PRODX37
//SYSIN    DD *
 SORT FIELDS=(01,03,PD,A,07,04,PD,A)
 END
//*
//STEP11   EXEC PGM=GFCT2BVH
//*
//* ***    ----------------------------------------------
//* ***  * BUSCAR GRUPO E SUBGRUPO NO ARQUIVO DO CLIE E
//* ***  * ATUALIZAR NO ARQUIVO DE SAIDA
//* ***    ----------------------------------------------
//*
//TADESAOE DD DSN=*.STEP09.RTADESAO,
//       DISP=SHR
//ECCTACLI DD DSN=*.STEP10.SORT.SORTOUT,
//       DISP=SHR
//TADESAOS DD DSN=MX.GFCT.JN555S11.TADESAOS(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB),
//       DATACLAS=PRODX37
//TADESAOI DD DSN=MX.GFCT.PRV.TADESAOI(+1),
//       DISP=(,CATLG,DELETE),
//       UNIT=DISCO,
//       SPACE=(TRK,(40000,16000),RLSE),
//       DCB=(MX.A,LRECL=150,RECFM=FB),
//       DATACLAS=PRODX37
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=Y
//*
