      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TBEM_IMOV_BENEF)                           *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0L4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0L4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TBEM_IMOV_BENEF TABLE
           ( CORCAM_CONTR_RURAL             DECIMAL(10, 0) NOT NULL,
             CIMOV_CREDT_RURAL              DECIMAL(10, 0) NOT NULL,
             CUND_AREA_FINCD                DECIMAL(3, 0) NOT NULL,
             MAREA_FINCD_RURAL              DECIMAL(11, 4),
             CUND_ITEM_FINCD                DECIMAL(3, 0),
             QITEM_FINCD_RURAL              DECIMAL(9, 2),
             CUND_PROTV_IMOV                DECIMAL(3, 0),
             QPROTV_IMOV_BENEF              DECIMAL(9, 2),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TBEM_IMOV_BENEF             *
      ******************************************************************
       01  CRURB0L4.
      *    *************************************************************
           10 CORCAM-CONTR-RURAL   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CIMOV-CREDT-RURAL    PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-AREA-FINCD      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 MAREA-FINCD-RURAL    PIC S9(7)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CUND-ITEM-FINCD      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QITEM-FINCD-RURAL    PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUND-PROTV-IMOV      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QPROTV-IMOV-BENEF    PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
