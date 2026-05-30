      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOT_CATAO_VISACASH)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB084))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB084)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOT_CATAO_VISACASH TABLE
           ( DEMIS                          DATE NOT NULL,
             CASSOC_INSTC_CATAO             DECIMAL(6, 0) NOT NULL,
             IINSTC_ASSOC                   CHAR(35) NOT NULL,
             QCATAO_CREDT_EMTDO             DECIMAL(7, 0) NOT NULL,
             DVALDD_CATAO                   DECIMAL(6, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOT_CATAO_VISACASH          *
      ******************************************************************
       01  BACCB084.
      *    *************************************************************
           10 DEMIS                PIC X(10).
      *    *************************************************************
           10 CASSOC-INSTC-CATAO   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 IINSTC-ASSOC         PIC X(35).
      *    *************************************************************
           10 QCATAO-CREDT-EMTDO   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DVALDD-CATAO         PIC S9(6)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
