      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_BONIF_SERVC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_BONIF_SERVC TABLE
           ( CTPO_BONIF                     DECIMAL(3, 0) NOT NULL,
             RTPO_BONIF                     CHAR(30) NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_BONIF_SERVC             *
      ******************************************************************
       01  GFCTB0F4.
      *    *************************************************************
           10 CTPO-BONIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-BONIF           PIC X(30).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
