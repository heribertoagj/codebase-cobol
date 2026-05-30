      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CATAO_SERVC_ISENC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB077))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB077)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CATAO_SERVC_ISENC TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CLI                     DECIMAL(16, 0) NOT NULL,
             CSERVC                         DECIMAL(5, 0) NOT NULL,
             PDESC                          DECIMAL(5, 2) NOT NULL,
             CEXCL                          CHAR(1) NOT NULL,
             HINCL_TARIF                    TIMESTAMP NOT NULL,
             HATULZ_TARIF                   TIMESTAMP NOT NULL,
             HEXCL_TARIF                    TIMESTAMP NOT NULL,
             CFUNC_BDSCO_INCL               CHAR(7) NOT NULL,
             CFUNC_BDSCO_ATULZ              CHAR(7) NOT NULL,
             CFUNC_BDSCO_EXCL               CHAR(7) NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             CTERM_ATULZ                    CHAR(8) NOT NULL,
             CTERM_EXCL                     CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CATAO_SERVC_ISENC           *
      ******************************************************************
       01  BACCB077.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CLI           PIC S9(16)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 PDESC                PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CEXCL                PIC X(1).
      *    *************************************************************
           10 HINCL-TARIF          PIC X(26).
      *    *************************************************************
           10 HATULZ-TARIF         PIC X(26).
      *    *************************************************************
           10 HEXCL-TARIF          PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO-INCL     PIC X(7).
      *    *************************************************************
           10 CFUNC-BDSCO-ATULZ    PIC X(7).
      *    *************************************************************
           10 CFUNC-BDSCO-EXCL     PIC X(7).
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 CTERM-ATULZ          PIC X(8).
      *    *************************************************************
           10 CTERM-EXCL           PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
