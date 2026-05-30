      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCICLO_PROD_RURAL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB083))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB083)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCICLO_PROD_RURAL TABLE
           ( CCICLO_PROD_RURAL              DECIMAL(5, 0) NOT NULL,
             ICICLO_PROD_RURAL              CHAR(80) NOT NULL,
             CCICLO_PROD_RECOR              CHAR(5) NOT NULL,
             DINIC_VGCIA                    DATE,
             DFIM_VGCIA                     DATE,
             DULT_ATULZ                     DATE,
             CUSUAR_RESP                    CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCICLO_PROD_RURAL           *
      ******************************************************************
       01  RURCB083.
      *    *************************************************************
           10 CCICLO-PROD-RURAL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 ICICLO-PROD-RURAL    PIC X(80).
      *    *************************************************************
           10 CCICLO-PROD-RECOR    PIC X(5).
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
