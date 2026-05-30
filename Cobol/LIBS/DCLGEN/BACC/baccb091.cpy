      ******************************************************************
      * DCLGEN TABLE(DB2PRD.COD_MOEDA_CIRCU)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB091))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB091)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.COD_MOEDA_CIRCU TABLE
           ( CMOEDA_CIRCL                   DECIMAL(3, 0) NOT NULL,
             CSGL_MOEDA                     CHAR(3) NOT NULL,
             RMOEDA_CATAO_CREDT             CHAR(30) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.COD_MOEDA_CIRCU             *
      ******************************************************************
       01  BACCB091.
      *    *************************************************************
           10 CMOEDA-CIRCL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGL-MOEDA           PIC X(3).
      *    *************************************************************
           10 RMOEDA-CATAO-CREDT   PIC X(30).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
