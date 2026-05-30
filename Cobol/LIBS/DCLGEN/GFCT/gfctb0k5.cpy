***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TEXTRT_ANO_TARIF)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0K5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0K5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TEXTRT_ANO_TARIF TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             DANO_REFT_EXTRT                DECIMAL(4, 0) NOT NULL,
             CIDTFD_TARIF_EXTRT             DECIMAL(1, 0) NOT NULL,
             NLIN_EXTRT_CLI                 DECIMAL(3, 0) NOT NULL,
             CBCO_CCREN                     DECIMAL(3, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CIDTFD_SNAL_LCTO               DECIMAL(1, 0) NOT NULL,
             RSEGDA_LIN_EXTRT               CHAR(32) NOT NULL,
             QCONSL_DETRM_TARIF             DECIMAL(5, 0) NOT NULL,
             VCONSL_DETRM_TARIF             DECIMAL(11, 2) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TEXTRT_ANO_TARIF            *
      ******************************************************************
       01  GFCTB0K5.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-REFT-EXTRT      PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-TARIF-EXTRT   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NLIN-EXTRT-CLI       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-CCREN           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SNAL-LCTO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RSEGDA-LIN-EXTRT     PIC X(32).
      *    *************************************************************
           10 QCONSL-DETRM-TARIF   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VCONSL-DETRM-TARIF   PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
