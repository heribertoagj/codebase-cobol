      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TFASE_PROCM_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB075))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB075)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TFASE_PROCM_CATAO TABLE
           ( CFASE_PROCM                    DECIMAL(4, 0) NOT NULL,
             RFASE_PROCM                    CHAR(100),
             QMNUTO_MAX_FASE                DECIMAL(3, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TFASE_PROCM_CATAO           *
      ******************************************************************
       01  BVVEB075.
      *    *************************************************************
           10 CFASE-PROCM          PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 RFASE-PROCM          PIC X(100).
      *    *************************************************************
           10 QMNUTO-MAX-FASE      PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
