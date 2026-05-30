      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TORGNZ_AUTDD_CERTF)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB035))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB035)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TORGNZ_AUTDD_CERTF TABLE
           ( CAUTDD_CERTD                   DECIMAL(1, 0) NOT NULL,
             CID_CERTF_DGTAL                CHAR(40) NOT NULL,
             CEMISR_AUTDD_CERTF             DECIMAL(1, 0) NOT NULL,
             IEMISR_AUTDD_CERTF             CHAR(100) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TORGNZ_AUTDD_CERTF          *
      ******************************************************************
       01  BVVEB035.
      *    *************************************************************
           10 CAUTDD-CERTD         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CID-CERTF-DGTAL      PIC X(40).
      *    *************************************************************
           10 CEMISR-AUTDD-CERTF   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IEMISR-AUTDD-CERTF   PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
