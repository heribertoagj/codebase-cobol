      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRENDA_AGROP_PROTR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB061))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB061)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRENDA_AGROP_PROTR TABLE
           ( CCLUB                          DECIMAL(10, 0) NOT NULL,
             CSEQ_ATULZ_RSUMO               DECIMAL(3, 0) NOT NULL,
             CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             URENDA_AGROP                   DECIMAL(3, 0) NOT NULL,
             QRENDA_AGROP                   DECIMAL(11, 2) NOT NULL,
             VRENDA_AGROP                   DECIMAL(15, 2) NOT NULL,
             PRBATE_RENDA_CLI               DECIMAL(5, 2) NOT NULL,
             HULT_ATULZ_EFETU               TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRENDA_AGROP_PROTR          *
      ******************************************************************
       01  RUECB061.
      *    *************************************************************
           10 CCLUB                PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-ATULZ-RSUMO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 URENDA-AGROP         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QRENDA-AGROP         PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VRENDA-AGROP         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PRBATE-RENDA-CLI     PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HULT-ATULZ-EFETU     PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
