      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_CUSTO_TARIF)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_CUSTO_TARIF TABLE
           ( CTPO_CUSTO_SERVC               DECIMAL(2, 0) NOT NULL,
             RTPO_CUSTO_SERVC               CHAR(30) NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_CUSTO_TARIF             *
      ******************************************************************
       01  GFCTB0F6.
      *    *************************************************************
           10 CTPO-CUSTO-SERVC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-CUSTO-SERVC     PIC X(30).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
