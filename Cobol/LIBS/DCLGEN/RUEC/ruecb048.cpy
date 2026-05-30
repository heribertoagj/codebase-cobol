      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAPOIO_UTILZ_MDULO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB048))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB048)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAPOIO_UTILZ_MDULO TABLE
           ( CTERM                          CHAR(8) NOT NULL,
             CIDTFD_MDULO                   DECIMAL(3, 0) NOT NULL,
             CSEQ_CTRL_MDULO                DECIMAL(5, 0) NOT NULL,
             WRESUL_CONS_MDULO              VARCHAR(2000)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAPOIO_UTILZ_MDULO          *
      ******************************************************************
       01  RUECB048.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CIDTFD-MDULO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-CTRL-MDULO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 WRESUL-CONS-MDULO.
              49 WRESUL-CONS-MDULO-LEN
                 PIC S9(4) USAGE COMP-5.
              49 WRESUL-CONS-MDULO-TEXT
                 PIC X(2000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
