***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARM_CALC_EXTRT)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARM_CALC_EXTRT TABLE
           ( CBCO_CCREN                     DECIMAL(3, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CIDTFD_SNAL_LCTO               DECIMAL(1, 0) NOT NULL,
             CTPO_CTA_MOVTO                 DECIMAL(1, 0) NOT NULL,
             CPSSOA_SERVC_TARIF             CHAR(1) NOT NULL,
             CINDCD_SEGDA_LIN               DECIMAL(1, 0) NOT NULL,
             DINIC_VGCIA_REG                DATE NOT NULL,
             NSEQ_FILTR_SELEC               DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_VLR                DATE NOT NULL,
             DFIM_VGCIA_VLR                 DATE NOT NULL,
             VTARIF_DEB_MOVTO               DECIMAL(11, 2) NOT NULL,
             CIDTFD_SIT_PARM                DECIMAL(1, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARM_CALC_EXTRT            *
      ******************************************************************
       01  GFCTB0N4.
      *    *************************************************************
           10 CBCO-CCREN           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SNAL-LCTO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CTA-MOVTO       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-SERVC-TARIF   PIC X(1).
      *    *************************************************************
           10 CINDCD-SEGDA-LIN     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-REG      PIC X(10).
      *    *************************************************************
           10 NSEQ-FILTR-SELEC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-VLR      PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-VLR       PIC X(10).
      *    *************************************************************
           10 VTARIF-DEB-MOVTO     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SIT-PARM      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-INCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CFUNC-EXCL-SIST      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
