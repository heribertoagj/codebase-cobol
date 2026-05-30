      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAGNDA_FINCR_BANDE)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB030))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB030)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAGNDA_FINCR_BANDE TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             CIDTFD_BANDE_ESTBL             CHAR(2) NOT NULL,
             CIDTFD_INDCD                   DECIMAL(5, 0) NOT NULL,
             DAGNDA_FINCR_BANDE             DATE NOT NULL,
             VAGNDA_FINCR_BANDE             DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAGNDA_FINCR_BANDE          *
      ******************************************************************
       01  BVVEB030.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-BANDE-ESTBL   PIC X(2).
      *    *************************************************************
           10 CIDTFD-INDCD         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DAGNDA-FINCR-BANDE   PIC X(10).
      *    *************************************************************
           10 VAGNDA-FINCR-BANDE   PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
