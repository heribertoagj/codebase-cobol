      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TVLR_DISCT_CATAO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB00A))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB00A)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TVLR_DISCT_CATAO TABLE
           ( NSEQ_VLR_DISCT                 DECIMAL(7, 0) NOT NULL,
             CVLR_DISCT_CARAC               CHAR(15),
             RVLR_DISCT_CARAC               CHAR(80),
             RVLR_DISCT_DETLH               CHAR(20),
             CINDCD_ORIGE_CARAC             DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TVLR_DISCT_CATAO            *
      ******************************************************************
       01  BVVEB00A.
      *    *************************************************************
           10 NSEQ-VLR-DISCT       PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CVLR-DISCT-CARAC     PIC X(15).
      *    *************************************************************
           10 RVLR-DISCT-CARAC     PIC X(80).
      *    *************************************************************
           10 RVLR-DISCT-DETLH     PIC X(20).
      *    *************************************************************
           10 CINDCD-ORIGE-CARAC   PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
