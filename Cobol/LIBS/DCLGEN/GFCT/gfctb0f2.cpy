      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TELA_ONLINE_SIST)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F2))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F2)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TELA_ONLINE_SIST TABLE
           ( CTELA_SIST                     CHAR(8) NOT NULL,
             RTELA_SIST                     CHAR(60) NOT NULL,
             RTITLO_TELA_SIST               CHAR(59) NOT NULL,
             CIDTFD_TELA_FIM                CHAR(1) NOT NULL,
             CFUNC_MANUT_EXCL               DECIMAL(9, 0),
             HEXCL_TELA_SIST                TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TELA_ONLINE_SIST            *
      ******************************************************************
       01  GFCTB0F2.
      *    *************************************************************
           10 CTELA-SIST           PIC X(8).
      *    *************************************************************
           10 RTELA-SIST           PIC X(60).
      *    *************************************************************
           10 RTITLO-TELA-SIST     PIC X(59).
      *    *************************************************************
           10 CIDTFD-TELA-FIM      PIC X(1).
      *    *************************************************************
           10 CFUNC-MANUT-EXCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-TELA-SIST      PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
