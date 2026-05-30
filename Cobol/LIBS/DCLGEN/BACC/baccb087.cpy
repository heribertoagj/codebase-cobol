      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ALERT_PROVS)                               *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB087))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB087)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ALERT_PROVS TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             DGERAC_ALERT                   DATE NOT NULL,
             HGERAC_ALERT                   TIME NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             DENVIO_ALERT                   DATE NOT NULL,
             HENVIO_ALERT                   TIME NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ALERT_PROVS                 *
      ******************************************************************
       01  BACCB087.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DGERAC-ALERT         PIC X(10).
      *    *************************************************************
           10 HGERAC-ALERT         PIC X(8).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DENVIO-ALERT         PIC X(10).
      *    *************************************************************
           10 HENVIO-ALERT         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
