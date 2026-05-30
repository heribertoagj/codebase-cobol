      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TINSTC_FINCR_PARTC_CATAO)                  *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09A))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09A)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TINSTC_FINCR_PARTC_CATAO TABLE
           ( CISPB_PARTC_SIST               CHAR(8) NOT NULL,
             CBCO_CONGL                     DECIMAL(3, 0),
             CINDCD_SIT_PARTC               DECIMAL(1, 0) NOT NULL,
             IINSTC_PARTC_BACEN             CHAR(100)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TINSTC_FINCR_PARTC_CATAO    *
      ******************************************************************
       01  BVVEB09A.
      *    *************************************************************
           10 CISPB-PARTC-SIST     PIC X(8).
      *    *************************************************************
           10 CBCO-CONGL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-SIT-PARTC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IINSTC-PARTC-BACEN   PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
