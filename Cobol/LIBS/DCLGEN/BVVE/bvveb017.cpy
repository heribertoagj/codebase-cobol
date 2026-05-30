      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTX_ESPCL_ANTEC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB017))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB017)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTX_ESPCL_ANTEC TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CATAO               DECIMAL(13, 0) NOT NULL,
             CIDTFD_ACORD_PCELD             CHAR(1),
             PTX_ESPCL_ANTEC                DECIMAL(5, 2),
             CFUNC_BDSCO_CATAO              DECIMAL(9, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTX_ESPCL_ANTEC             *
      ******************************************************************
       01  BVVEB017.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CATAO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-ACORD-PCELD   PIC X(1).
      *    *************************************************************
           10 PTX-ESPCL-ANTEC      PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO-CATAO    PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
