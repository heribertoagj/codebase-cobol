      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TESTRT_TEMPR_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB070))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB070)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TESTRT_TEMPR_CATAO TABLE
           ( CTRANS_PROG_APRES              CHAR(4) NOT NULL,
             CTERM_USUAR                    CHAR(15) NOT NULL,
             NORD_CPO_MSGEM                 DECIMAL(3, 0) NOT NULL,
             NORD_NVEL                      DECIMAL(4, 0) NOT NULL,
             ICPO_MSGRA                     CHAR(40) NOT NULL,
             RCPO_MSGEM_MSGRA               CHAR(36) NOT NULL,
             CINDCD_OBRIG                   CHAR(1) NOT NULL,
             CINDCD_FORMT_MSGEM             DECIMAL(1, 0) NOT NULL,
             NTMNHO_CPO                     DECIMAL(4, 0) NOT NULL,
             QPOSIC_DEC                     DECIMAL(2, 0) NOT NULL,
             QREPTC_MSGEM                   DECIMAL(3, 0) NOT NULL,
             RCONTD_CPO                     VARCHAR(260) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TESTRT_TEMPR_CATAO          *
      ******************************************************************
       01  BVVEB070.
      *    *************************************************************
           10 CTRANS-PROG-APRES    PIC X(4).
      *    *************************************************************
           10 CTERM-USUAR          PIC X(15).
      *    *************************************************************
           10 NORD-CPO-MSGEM       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NORD-NVEL            PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 ICPO-MSGRA           PIC X(40).
      *    *************************************************************
           10 RCPO-MSGEM-MSGRA     PIC X(36).
      *    *************************************************************
           10 CINDCD-OBRIG         PIC X(1).
      *    *************************************************************
           10 CINDCD-FORMT-MSGEM   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NTMNHO-CPO           PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 QPOSIC-DEC           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QREPTC-MSGEM         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RCONTD-CPO.
              49 RCONTD-CPO-LEN    PIC S9(4) USAGE COMP.
              49 RCONTD-CPO-TEXT   PIC X(260).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
