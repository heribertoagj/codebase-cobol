***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAGPTO_CTBIL_TARIF)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0I7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0I7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAGPTO_CTBIL_TARIF TABLE
           ( CAGPTO_CTBIL                   DECIMAL(5, 0) NOT NULL,
             RAGPTO_CTBIL                   CHAR(40) NOT NULL,
             CTPO_CTA_DEB_TARIF             DECIMAL(1, 0) NOT NULL,
             CTPO_DEB_COBR                  DECIMAL(1, 0) NOT NULL,
             CINDCD_DEB_PCIAL               CHAR(1) NOT NULL,
             CINDCD_PCIAL_ULT               CHAR(1) NOT NULL,
             CCART_DEB_TARIF                DECIMAL(5, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             RSEGDA_LIN_EXTRT               CHAR(32) NOT NULL,
             DDIA_COBR_TARIF                DECIMAL(2, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAGPTO_CTBIL_TARIF          *
      ******************************************************************
       01  GFCTB0I7.
      *    *************************************************************
           10 CAGPTO-CTBIL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RAGPTO-CTBIL         PIC X(40).
      *    *************************************************************
           10 CTPO-CTA-DEB-TARIF   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-DEB-COBR        PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-DEB-PCIAL     PIC X(1).
      *    *************************************************************
           10 CINDCD-PCIAL-ULT     PIC X(1).
      *    *************************************************************
           10 CCART-DEB-TARIF      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RSEGDA-LIN-EXTRT     PIC X(32).
      *    *************************************************************
           10 DDIA-COBR-TARIF      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
