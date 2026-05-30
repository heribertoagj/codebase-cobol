      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_SOLTC_SERVC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB078))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB078)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_SOLTC_SERVC TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CLI                     DECIMAL(16, 0) NOT NULL,
             HINCL_TARIF                    TIMESTAMP NOT NULL,
             CSERVC                         DECIMAL(5, 0) NOT NULL,
             VTARIF_CLI                     DECIMAL(13, 2) NOT NULL,
             CFUNC_BDSCO_INCL               CHAR(7) NOT NULL,
             CCOBR_TARIF                    CHAR(1) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_SOLTC_SERVC            *
      ******************************************************************
       01  BACCB078.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CLI           PIC S9(16)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-TARIF          PIC X(26).
      *    *************************************************************
           10 CSERVC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VTARIF-CLI           PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO-INCL     PIC X(7).
      *    *************************************************************
           10 CCOBR-TARIF          PIC X(1).
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
