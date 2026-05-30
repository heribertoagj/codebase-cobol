      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_MSGEM_CREDT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RCORB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RCORB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_MSGEM_CREDT TABLE
           ( CCTRL_MSGEM_CREDT              DECIMAL(13, 0) NOT NULL,
             CNRO_CTRL_INSTC                CHAR(20) NOT NULL,
             CSIST                          CHAR(4) NOT NULL,
             CSIT_PROCM_CREDT               DECIMAL(1, 0) NOT NULL,
             CIDTFD_EMISR                   CHAR(8) NOT NULL,
             CIDTFD_DSTNA                   CHAR(8) NOT NULL,
             COPER_UNIC_MSGEM               CHAR(23),
             HGRAV_PROT_CHEGD               TIMESTAMP,
             HGRAV_PROT_ENTRG               TIMESTAMP,
             CAUTEN_MSGEM_CREDT             CHAR(48)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_MSGEM_CREDT           *
      ******************************************************************
       01  RCORB000.
      *    *************************************************************
           10 CCTRL-MSGEM-CREDT    PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTRL-INSTC      PIC X(20).
      *    *************************************************************
           10 CSIST                PIC X(4).
      *    *************************************************************
           10 CSIT-PROCM-CREDT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-EMISR         PIC X(8).
      *    *************************************************************
           10 CIDTFD-DSTNA         PIC X(8).
      *    *************************************************************
           10 COPER-UNIC-MSGEM     PIC X(23).
      *    *************************************************************
           10 HGRAV-PROT-CHEGD     PIC X(26).
      *    *************************************************************
           10 HGRAV-PROT-ENTRG     PIC X(26).
      *    *************************************************************
           10 CAUTEN-MSGEM-CREDT   PIC X(48).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
