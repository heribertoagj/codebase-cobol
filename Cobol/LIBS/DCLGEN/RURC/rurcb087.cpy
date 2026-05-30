      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTPO_AGROP_RURAL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB087))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB087)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTPO_AGROP_RURAL TABLE
           ( CTPO_AGROP_RURAL               DECIMAL(5, 0) NOT NULL,
             ITPO_AGROP_RURAL               CHAR(80) NOT NULL,
             CTPO_AGROP_RECOR               CHAR(5) NOT NULL,
             DINIC_VGCIA                    DATE,
             DFIM_VGCIA                     DATE,
             DULT_ATULZ                     DATE,
             CUSUAR_RESP                    CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTPO_AGROP_RURAL            *
      ******************************************************************
       01  RURCB087.
      *    *************************************************************
           10 CTPO-AGROP-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 ITPO-AGROP-RURAL     PIC X(80).
      *    *************************************************************
           10 CTPO-AGROP-RECOR     PIC X(5).
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
