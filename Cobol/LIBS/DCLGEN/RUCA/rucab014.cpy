      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_MANUT_CONTR)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB014))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB014)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_MANUT_CONTR TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             NMANUT_EFETU                   DECIMAL(3, 0) NOT NULL,
             RCPO_MANUT_RECOR               VARCHAR(256) NOT NULL,
             DMANUT                         DATE NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_MANUT_CONTR            *
      ******************************************************************
       01  RUCAB014.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 NMANUT-EFETU         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RCPO-MANUT-RECOR.
              49 RCPO-MANUT-RECOR-LEN
                 PIC S9(4) USAGE COMP.
              49 RCPO-MANUT-RECOR-TEXT
                 PIC X(256).
      *    *************************************************************
           10 DMANUT               PIC X(10).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
