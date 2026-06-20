      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRESP_DEVLC_CHEQ)                          *
      *        LIBRARY(AD.DB2.DCLGEN(SLIGB058))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SLIGB058)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRESP_DEVLC_CHEQ TABLE
           ( CTPO_DEVLC_CHEQ                DECIMAL(3, 0) NOT NULL,
             CSEQ_MOTVO_DEVLC               DECIMAL(5, 0) NOT NULL,
             CSIT_MOTVO_DEVLC               DECIMAL(1, 0) NOT NULL,
             CINDCD_RESP_DEVLC              CHAR(1) NOT NULL,
             CINDCD_POBLD_REAP              CHAR(1) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             HATULZ                         TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRESP_DEVLC_CHEQ            *
      ******************************************************************
       01  SLIGB058.
      *    *************************************************************
           10 CTPO-DEVLC-CHEQ      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-MOTVO-DEVLC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-MOTVO-DEVLC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RESP-DEVLC    PIC X(1).
      *    *************************************************************
           10 CINDCD-POBLD-REAP    PIC X(1).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HATULZ               PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
