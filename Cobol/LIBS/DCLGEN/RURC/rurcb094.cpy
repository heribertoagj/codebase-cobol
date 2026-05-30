      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARMZ_SUB_PROG)                           *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB094))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB094)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARMZ_SUB_PROG TABLE
           ( CSUB_PROG_RURAL                CHAR(4) NOT NULL,
             NPARMZ_SUB_PROG                DECIMAL(5, 0) NOT NULL,
             CORIGE_REC                     DECIMAL(3, 0),
             CFNALD_RURAL                   DECIMAL(3, 0),
             CPRODT_RURAL                   DECIMAL(3, 0),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL                          TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARMZ_SUB_PROG             *
      ******************************************************************
       01  RURCB094.
      *    *************************************************************
           10 CSUB-PROG-RURAL      PIC X(4).
      *    *************************************************************
           10 NPARMZ-SUB-PROG      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFNALD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL                PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
