      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TARRJO_PGTO)                               *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB038))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB038)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TARRJO_PGTO TABLE
           ( CARRJO_PGTO                    DECIMAL(3, 0) NOT NULL,
             IARRJO_PGTO                    CHAR(30),
             CTPO_ARRJO_PGTO                CHAR(1),
             HINCL_REG                      TIMESTAMP,
             CUSUAR_INCL                    CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CUSUAR_MANUT                   CHAR(7)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TARRJO_PGTO                 *
      ******************************************************************
       01  BVVEB038.
      *    *************************************************************
           10 CARRJO-PGTO          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IARRJO-PGTO          PIC X(30).
      *    *************************************************************
           10 CTPO-ARRJO-PGTO      PIC X(1).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
