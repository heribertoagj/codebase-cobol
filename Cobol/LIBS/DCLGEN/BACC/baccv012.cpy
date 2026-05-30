      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01DEPDCIA_TPSTREM)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV012))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV012)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01DEPDCIA_TPSTREM TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CSIT_REMSS_ORPAG               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01DEPDCIA_TPSTREM          *
      ******************************************************************
       01  BACCV012.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-REMSS-ORPAG     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
