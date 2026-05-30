***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.NATUZ_SERVC_TARIF)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB096))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB096)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.NATUZ_SERVC_TARIF TABLE
           ( CNATUZ_SERVC                   DECIMAL(3, 0) NOT NULL,
             RNATUZ_SERVC                   CHAR(30) NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.NATUZ_SERVC_TARIF           *
      ******************************************************************
       01  GFCTB096.
      *    *************************************************************
           10 CNATUZ-SERVC         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RNATUZ-SERVC         PIC X(30).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
