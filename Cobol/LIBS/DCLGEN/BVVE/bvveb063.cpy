      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_MSGEM_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB063))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB063)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_MSGEM_CATAO TABLE
           ( CMNTRC_MSGRA                   CHAR(23) NOT NULL,
             IARQ_MNTRC                     CHAR(80),
             CCTRL_RQUIS_INSTC              CHAR(20),
             CUNIC_TRFGO_MSGEM              CHAR(23),
             CISPB_EMISR_MSGEM              CHAR(8),
             CISPB_DSTNA_MSGEM              CHAR(8),
             HPROCM_MSGEM                   TIMESTAMP,
             CSNLZD_MSGRA                   DECIMAL(3, 0),
             HULT_ATULZ                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_MSGEM_CATAO           *
      ******************************************************************
       01  BVVEB063.
      *    *************************************************************
           10 CMNTRC-MSGRA         PIC X(23).
      *    *************************************************************
           10 IARQ-MNTRC           PIC X(80).
      *    *************************************************************
           10 CCTRL-RQUIS-INSTC    PIC X(20).
      *    *************************************************************
           10 CUNIC-TRFGO-MSGEM    PIC X(23).
      *    *************************************************************
           10 CISPB-EMISR-MSGEM    PIC X(8).
      *    *************************************************************
           10 CISPB-DSTNA-MSGEM    PIC X(8).
      *    *************************************************************
           10 HPROCM-MSGEM         PIC X(26).
      *    *************************************************************
           10 CSNLZD-MSGRA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
