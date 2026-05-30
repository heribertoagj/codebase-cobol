      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TESTRT_REPTC_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB069))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB069)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TESTRT_REPTC_CATAO TABLE
           ( CTRANS_PROG_APRES              CHAR(4) NOT NULL,
             CTERM_USUAR                    CHAR(15) NOT NULL,
             NORD_CPO_MSGEM                 DECIMAL(3, 0) NOT NULL,
             NSEQ_COMPS                     DECIMAL(3, 0) NOT NULL,
             NOCOR_PRIM_NVEL                DECIMAL(3, 0) NOT NULL,
             NOCOR_SEGDA_NVEL               DECIMAL(3, 0) NOT NULL,
             NOCOR_TERC_NVEL                DECIMAL(3, 0) NOT NULL,
             NOCOR_QUART_NVEL               DECIMAL(3, 0) NOT NULL,
             RCONTD_CPO                     VARCHAR(260) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TESTRT_REPTC_CATAO          *
      ******************************************************************
       01  BVVEB069.
      *    *************************************************************
           10 CTRANS-PROG-APRES    PIC X(4).
      *    *************************************************************
           10 CTERM-USUAR          PIC X(15).
      *    *************************************************************
           10 NORD-CPO-MSGEM       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-COMPS           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NOCOR-PRIM-NVEL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NOCOR-SEGDA-NVEL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NOCOR-TERC-NVEL      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NOCOR-QUART-NVEL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RCONTD-CPO.
              49 RCONTD-CPO-LEN    PIC S9(4) USAGE COMP.
              49 RCONTD-CPO-TEXT   PIC X(260).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
