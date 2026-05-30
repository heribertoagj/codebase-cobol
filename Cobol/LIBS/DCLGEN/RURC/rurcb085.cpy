      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMODLD_EMPTO_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB085))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB085)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMODLD_EMPTO_RURAL TABLE
           ( CMODLD_RURAL                   DECIMAL(5, 0) NOT NULL,
             IMODLD_RURAL                   CHAR(80),
             CMODLD_RECOR                   CHAR(5) NOT NULL,
             DINIC_VGCIA                    DATE,
             DFIM_VGCIA                     DATE,
             DULT_ATULZ                     DATE,
             CUSUAR_RESP                    CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMODLD_EMPTO_RURAL          *
      ******************************************************************
       01  RURCB085.
      *    *************************************************************
           10 CMODLD-RURAL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IMODLD-RURAL         PIC X(80).
      *    *************************************************************
           10 CMODLD-RECOR         PIC X(5).
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA           PIC X(10).
      *    *************************************************************
           10 DULT-ATULZ           PIC X(10).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
