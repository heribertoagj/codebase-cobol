      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01SOLTC_ORPAG)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV027))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV027)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01SOLTC_ORPAG TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CREMSS_ORPAG                   DECIMAL(7, 0) NOT NULL,
             CORPAG_BACC                    DECIMAL(7, 0) NOT NULL,
             DRECEB                         DATE,
             DSOLTC                         DATE NOT NULL,
             CSIT_ORPAG_FIS                 DECIMAL(1, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CORIGE_ORPAG                   DECIMAL(1, 0) NOT NULL,
             DATDMT                         DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01SOLTC_ORPAG              *
      ******************************************************************
       01  BACCV027.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CREMSS-ORPAG         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CORPAG-BACC          PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DRECEB               PIC X(10).
      *    *************************************************************
           10 DSOLTC               PIC X(10).
      *    *************************************************************
           10 CSIT-ORPAG-FIS       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-ORPAG         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DATDMT               PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
