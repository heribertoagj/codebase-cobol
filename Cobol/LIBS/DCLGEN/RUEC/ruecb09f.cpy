      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRODT_RURAL_RECOR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09F))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09F)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRODT_RURAL_RECOR TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             NPRODT_RURAL_RECOR             DECIMAL(5, 0) NOT NULL,
             CPRODT_RURAL                   DECIMAL(3, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRODT_RURAL_RECOR          *
      ******************************************************************
       01  RUECB09F.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NPRODT-RURAL-RECOR   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
