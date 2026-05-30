***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.DEPDC_EMIS_RELAT)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB039))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB039)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.DEPDC_EMIS_RELAT TABLE
           ( CIMPRE_RELAT                   CHAR(8) NOT NULL,
             HSOLTC_IMPRE                   TIMESTAMP NOT NULL,
             CDEPDC_SELEC_RELAT             DECIMAL(5, 0) NOT NULL,
             CPAB_SELEC_RELAT               DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.DEPDC_EMIS_RELAT            *
      ******************************************************************
       01  GFCTB039.
      *    *************************************************************
           10 CIMPRE-RELAT         PIC X(8).
      *    *************************************************************
           10 HSOLTC-IMPRE         PIC X(26).
      *    *************************************************************
           10 CDEPDC-SELEC-RELAT   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPAB-SELEC-RELAT     PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
