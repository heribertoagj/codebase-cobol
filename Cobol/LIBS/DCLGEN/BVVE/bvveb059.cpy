      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRLCTO_OPER_CIPAG_CONTR)                   *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB059))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB059)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRLCTO_OPER_CIPAG_CONTR TABLE
           ( CCTRO_CUSTO                    CHAR(4) NOT NULL,
             COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             COPER_EFETU_CIPAG              CHAR(20) NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRLCTO_OPER_CIPAG_CONTR     *
      ******************************************************************
       01  BVVEB059.
      *    *************************************************************
           10 CCTRO-CUSTO          PIC X(4).
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 COPER-EFETU-CIPAG    PIC X(20).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
