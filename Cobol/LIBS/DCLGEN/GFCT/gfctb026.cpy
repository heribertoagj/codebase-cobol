***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CJTO_SERVC_TARIF)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB026))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB026)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CJTO_SERVC_TARIF TABLE
           ( CCJTO_SERVC                    DECIMAL(3, 0) NOT NULL,
             RCJTO_SERVC                    CHAR(30) NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL,
             CINDCD_FLEXZ_AGPTO             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CJTO_SERVC_TARIF            *
      ******************************************************************
       01  GFCTB026.
      *    *************************************************************
           10 CCJTO-SERVC          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RCJTO-SERVC          PIC X(30).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-FLEXZ-AGPTO   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
