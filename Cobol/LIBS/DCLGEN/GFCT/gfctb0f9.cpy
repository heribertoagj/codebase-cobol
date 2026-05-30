      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_LCTO_DEB_CREDT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_LCTO_DEB_CREDT TABLE
           ( CTPO_LCTO_CTBIL                DECIMAL(2, 0) NOT NULL,
             RTPO_LCTO_CTBIL                CHAR(30),
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_LCTO_DEB_CREDT          *
      ******************************************************************
       01  GFCTB0F9.
      *    *************************************************************
           10 CTPO-LCTO-CTBIL      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-LCTO-CTBIL      PIC X(30).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
