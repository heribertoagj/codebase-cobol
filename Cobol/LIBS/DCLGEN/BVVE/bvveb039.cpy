      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCRDRA_ARRJO_PGTO_LCTO)                    *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB039))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB039)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCRDRA_ARRJO_PGTO_LCTO TABLE
           ( CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             CARRJO_PGTO                    DECIMAL(3, 0) NOT NULL,
             NPRFIL_CRDRA_PGTO              DECIMAL(1, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP,
             CUSUAR_INCL                    CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CUSUAR_MANUT                   CHAR(7)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCRDRA_ARRJO_PGTO_LCTO      *
      ******************************************************************
       01  BVVEB039.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CARRJO-PGTO          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NPRFIL-CRDRA-PGTO    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
