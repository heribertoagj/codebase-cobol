      ******************************************************************
      * DCLGEN TABLE(DB2PRD.RUCA_CAD_ROOT)                             *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB003)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.RUCA_CAD_ROOT TABLE
           ( B003_CH_CONTR                  DECIMAL(9, 0) NOT NULL,
             B003_CONTROL                   CHAR(16) NOT NULL,
             B003_NOMUTUAR1                 CHAR(40) NOT NULL,
             B003_PESSOA1                   CHAR(1) NOT NULL,
             B003_CGCPF1                    DECIMAL(15, 0) NOT NULL,
             B003_NOMUTUAR2                 CHAR(40) NOT NULL,
             B003_PESSOA2                   CHAR(1) NOT NULL,
             B003_CGCPF2                    DECIMAL(15, 0) NOT NULL,
             B003_NOMUTUAR3                 CHAR(40) NOT NULL,
             B003_PESSOA3                   CHAR(1) NOT NULL,
             B003_CGCPF3                    DECIMAL(15, 0) NOT NULL,
             B003_NUMDEPEN                  DECIMAL(7, 0) NOT NULL,
             B003_TPDEPEN                   DECIMAL(3, 0) NOT NULL,
             B003_MICREGIAO                 DECIMAL(3, 0) NOT NULL,
             B003_UF                        CHAR(2) NOT NULL,
             B003_REGIAO                    CHAR(1) NOT NULL,
             B003_EMERG                     CHAR(1) NOT NULL,
             B003_ANOPROD                   DECIMAL(4, 0) NOT NULL,
             B003_VLPROD                    DECIMAL(15, 2) NOT NULL,
             B003_QTDMVR                    DECIMAL(7, 0) NOT NULL,
             B003_INDENIZ                   CHAR(1) NOT NULL,
             B003_AGENAVAL                  DECIMAL(7, 0) NOT NULL,
             B003_CTAAVAL                   DECIMAL(7, 0) NOT NULL,
             B003_NOMAVAL                   CHAR(40) NOT NULL,
             B003_PESAVAL                   CHAR(1) NOT NULL,
             B003_CGCPFAVAL                 DECIMAL(15, 0) NOT NULL,
             B003_ESPBENS                   CHAR(20) NOT NULL,
             B003_LOCBENS                   CHAR(40) NOT NULL,
             B003_ORIGREC1                  DECIMAL(3, 0) NOT NULL,
             B003_IDENT1                    CHAR(1) NOT NULL,
             B003_DATA1                     DATE,
             B003_ORIGREC2                  DECIMAL(3, 0) NOT NULL,
             B003_IDENT2                    CHAR(1) NOT NULL,
             B003_DATA2                     DATE,
             B003_ORIGREC3                  DECIMAL(3, 0) NOT NULL,
             B003_IDENT3                    CHAR(1) NOT NULL,
             B003_DATA3                     DATE,
             B003_ORIGREC4                  DECIMAL(3, 0) NOT NULL,
             B003_IDENT4                    CHAR(1) NOT NULL,
             B003_DATA4                     DATE,
             B003_IDENTREC                  CHAR(1) NOT NULL,
             B003_PLREFINAM                 DECIMAL(3, 0) NOT NULL,
             B003_SPRGREFIN                 DECIMAL(3, 0) NOT NULL,
             B003_DTREFINAM                 DATE,
             B003_TXREFIN                   DECIMAL(5, 0) NOT NULL,
             B003_NCARTREF                  DECIMAL(5, 0) NOT NULL,
             B003_POSTO                     CHAR(1) NOT NULL,
             B003_APLIC                     DECIMAL(3, 0) NOT NULL,
             B003_FXPRODUT                  DECIMAL(3, 0) NOT NULL,
             B003_VLBASCUST                 DECIMAL(15, 2) NOT NULL,
             B003_LIMFINAN                  DECIMAL(3, 0) NOT NULL,
             B003_VLCOBPRO                  DECIMAL(15, 2) NOT NULL,
             B003_TOTCOBPRO                 DECIMAL(15, 2) NOT NULL,
             B003_PRECPRODT                 DECIMAL(15, 0) NOT NULL,
             B003_PRECEMBAL                 DECIMAL(15, 0) NOT NULL,
             B003_LIMPRODUC                 DECIMAL(3, 0) NOT NULL,
             B003_LIMPRODUT                 DECIMAL(3, 0) NOT NULL,
             B003_VFINAL                    DATE,
             B003_JUROS                     DECIMAL(7, 2) NOT NULL,
             B003_CORRECAO                  DECIMAL(7, 2) NOT NULL,
             B003_ADPRAGRO                  DECIMAL(7, 2) NOT NULL,
             B003_COBERTU                   DECIMAL(7, 2) NOT NULL,
             B003_INCPAGRO                  DECIMAL(15, 2) NOT NULL,
             B003_VLCOBERT                  DECIMAL(15, 2) NOT NULL,
             B003_CODCRS                    CHAR(4) NOT NULL,
             B003_DTACERTO                  DATE,
             B003_CODACERT                  DECIMAL(3, 0) NOT NULL,
             B003_VLPRINCT_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_VLPRINAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_VLMUTORI_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_VLMUTCOR_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_VLPROCTA_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_VLPROACE_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_VLPRMUOR_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_VLPRMUAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_VLAUXIL_OPER9             DECIMAL(15, 2) NOT NULL,
             B003_JURPGORI_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_JURPGAC_PAG9              DECIMAL(15, 2) NOT NULL,
             B003_CORRPGOR_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_CORRPGAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_PROAPGOR_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_PROAPGAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_TECNPGOR_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_TECNPGAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_PERIPGOR_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_PERIPGAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_MEDIPGOR_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_MEDIPGAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_REMUPGOR_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_REMUPGAC_PAG9             DECIMAL(15, 2) NOT NULL,
             B003_IOFPGORI_CAP9             DECIMAL(15, 2) NOT NULL,
             B003_IOFPGAC_PAG9              DECIMAL(15, 2) NOT NULL,
             B003_VLPRINCT_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_VLPRINAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_VLMUTORI_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_VLMUTCOR_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_VLPROCTA_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_VLPROACE_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_VLPRMUOR_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_VLPRMUAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_VLAUXIL_OPER8             DECIMAL(15, 2) NOT NULL,
             B003_JURPGORI_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_JURPGAC_PAG8              DECIMAL(15, 2) NOT NULL,
             B003_CORRPGOR_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_CORRPGAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_PROAPGOR_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_PROAPGAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_TECNPGOR_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_TECNPGAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_PERIPGOR_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_PERIPGAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_MEDIPGOR_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_MEDIPGAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_REMUPGOR_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_REMUPGAC_PAG8             DECIMAL(15, 2) NOT NULL,
             B003_IOFPGORI_CAP8             DECIMAL(15, 2) NOT NULL,
             B003_IOFPGAC_PAG8              DECIMAL(15, 2) NOT NULL,
             B003_CGCPF1_ST                 CHAR(15) NOT NULL,
             B003_CGCPF2_ST                 CHAR(15) NOT NULL,
             B003_CGCPF3_ST                 CHAR(15) NOT NULL,
             B003_CGCPFAVAL_ST              CHAR(15) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.RUCA_CAD_ROOT               *
      ******************************************************************
       01  RUCAB003.
      *    *************************************************************
           10 B003-CH-CONTR        PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 B003-CONTROL         PIC X(16).
      *    *************************************************************
           10 B003-NOMUTUAR1       PIC X(40).
      *    *************************************************************
           10 B003-PESSOA1         PIC X(1).
      *    *************************************************************
           10 B003-CGCPF1          PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 B003-NOMUTUAR2       PIC X(40).
      *    *************************************************************
           10 B003-PESSOA2         PIC X(1).
      *    *************************************************************
           10 B003-CGCPF2          PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 B003-NOMUTUAR3       PIC X(40).
      *    *************************************************************
           10 B003-PESSOA3         PIC X(1).
      *    *************************************************************
           10 B003-CGCPF3          PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 B003-NUMDEPEN        PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 B003-TPDEPEN         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-MICREGIAO       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-UF              PIC X(2).
      *    *************************************************************
           10 B003-REGIAO          PIC X(1).
      *    *************************************************************
           10 B003-EMERG           PIC X(1).
      *    *************************************************************
           10 B003-ANOPROD         PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPROD          PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-QTDMVR          PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 B003-INDENIZ         PIC X(1).
      *    *************************************************************
           10 B003-AGENAVAL        PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 B003-CTAAVAL         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 B003-NOMAVAL         PIC X(40).
      *    *************************************************************
           10 B003-PESAVAL         PIC X(1).
      *    *************************************************************
           10 B003-CGCPFAVAL       PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 B003-ESPBENS         PIC X(20).
      *    *************************************************************
           10 B003-LOCBENS         PIC X(40).
      *    *************************************************************
           10 B003-ORIGREC1        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-IDENT1          PIC X(1).
      *    *************************************************************
           10 B003-DATA1           PIC X(10).
      *    *************************************************************
           10 B003-ORIGREC2        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-IDENT2          PIC X(1).
      *    *************************************************************
           10 B003-DATA2           PIC X(10).
      *    *************************************************************
           10 B003-ORIGREC3        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-IDENT3          PIC X(1).
      *    *************************************************************
           10 B003-DATA3           PIC X(10).
      *    *************************************************************
           10 B003-ORIGREC4        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-IDENT4          PIC X(1).
      *    *************************************************************
           10 B003-DATA4           PIC X(10).
      *    *************************************************************
           10 B003-IDENTREC        PIC X(1).
      *    *************************************************************
           10 B003-PLREFINAM       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-SPRGREFIN       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-DTREFINAM       PIC X(10).
      *    *************************************************************
           10 B003-TXREFIN         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 B003-NCARTREF        PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 B003-POSTO           PIC X(1).
      *    *************************************************************
           10 B003-APLIC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-FXPRODUT        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-VLBASCUST       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-LIMFINAN        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-VLCOBPRO        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-TOTCOBPRO       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PRECPRODT       PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 B003-PRECEMBAL       PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 B003-LIMPRODUC       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-LIMPRODUT       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-VFINAL          PIC X(10).
      *    *************************************************************
           10 B003-JUROS           PIC S9(5)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-CORRECAO        PIC S9(5)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-ADPRAGRO        PIC S9(5)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-COBERTU         PIC S9(5)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-INCPAGRO        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLCOBERT        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-CODCRS          PIC X(4).
      *    *************************************************************
           10 B003-DTACERTO        PIC X(10).
      *    *************************************************************
           10 B003-CODACERT        PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRINCT-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRINAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLMUTORI-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLMUTCOR-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPROCTA-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPROACE-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRMUOR-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRMUAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLAUXIL-OPER9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-JURPGORI-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-JURPGAC-PAG9    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-CORRPGOR-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-CORRPGAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PROAPGOR-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PROAPGAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-TECNPGOR-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-TECNPGAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PERIPGOR-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PERIPGAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-MEDIPGOR-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-MEDIPGAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-REMUPGOR-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-REMUPGAC-PAG9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-IOFPGORI-CAP9   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-IOFPGAC-PAG9    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRINCT-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRINAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLMUTORI-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLMUTCOR-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPROCTA-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPROACE-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRMUOR-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLPRMUAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-VLAUXIL-OPER8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-JURPGORI-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-JURPGAC-PAG8    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-CORRPGOR-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-CORRPGAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PROAPGOR-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PROAPGAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-TECNPGOR-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-TECNPGAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PERIPGOR-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-PERIPGAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-MEDIPGOR-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-MEDIPGAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-REMUPGOR-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-REMUPGAC-PAG8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-IOFPGORI-CAP8   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-IOFPGAC-PAG8    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 B003-CGCPF1-ST       PIC X(15).
      *    *************************************************************
           10 B003-CGCPF2-ST       PIC X(15).
      *    *************************************************************
           10 B003-CGCPF3-ST       PIC X(15).
      *    *************************************************************
           10 B003-CGCPFAVAL-ST    PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 121     *
      ******************************************************************
