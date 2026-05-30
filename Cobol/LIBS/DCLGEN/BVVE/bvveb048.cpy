      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRODT_AGNDA_FINCR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB048))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB048)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRODT_AGNDA_FINCR TABLE
           ( CPRODT_AGNDA_FINCR             DECIMAL(5, 0) NOT NULL,
             CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             CINDCD_ORD_PGTO                DECIMAL(1, 0),
             IPRODT_AGNDA_FINCR             CHAR(20) NOT NULL,
             CINDCD_SIT_REG                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(7),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRODT_AGNDA_FINCR          *
      ******************************************************************
       01  BVVEB048.
      *    *************************************************************
           10 CPRODT-AGNDA-FINCR   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ORD-PGTO      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IPRODT-AGNDA-FINCR   PIC X(20).
      *    *************************************************************
           10 CINDCD-SIT-REG       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
