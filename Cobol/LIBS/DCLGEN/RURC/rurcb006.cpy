      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PRODUTO_RURAL)                             *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB006))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB006)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PRODUTO_RURAL TABLE
           ( CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             IPRODT_RURAL                   CHAR(40) NOT NULL,
             CPRODT_BSICO                   DECIMAL(1, 0) NOT NULL,
             CVLR_BSICO_CUST                DECIMAL(3, 0) NOT NULL,
             CPRODT_RECOR                   DECIMAL(5, 0) NOT NULL,
             CPROJ_CIA_ABAST                CHAR(4) NOT NULL,
             CINDCD_EQUIV_PRODT             DECIMAL(1, 0) NOT NULL,
             CSIT_PRODT_RURAL               DECIMAL(1, 0) NOT NULL,
             CCTCIA_PROD_PROVV              DECIMAL(1, 0) NOT NULL,
             CCTCIA_QTD_PRODT               DECIMAL(1, 0) NOT NULL,
             PTX_SPREAD                     DECIMAL(5, 2) NOT NULL,
             CPRODT_BACEN                   CHAR(5) NOT NULL,
             CTEMPO_VIDA_CULTR              DECIMAL(1, 0),
             CINDCD_EXIGE_SEGUR             DECIMAL(1, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PRODUTO_RURAL               *
      ******************************************************************
       01  RURCB006.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IPRODT-RURAL         PIC X(40).
      *    *************************************************************
           10 CPRODT-BSICO         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CVLR-BSICO-CUST      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPROJ-CIA-ABAST      PIC X(4).
      *    *************************************************************
           10 CINDCD-EQUIV-PRODT   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PRODT-RURAL     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCTCIA-PROD-PROVV    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCTCIA-QTD-PRODT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 PTX-SPREAD           PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-BACEN         PIC X(5).
      *    *************************************************************
           10 CTEMPO-VIDA-CULTR    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-EXIGE-SEGUR   PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
