***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TINFO_COMPL_EXTRT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N3))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N3)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TINFO_COMPL_EXTRT TABLE
           ( CBCO_CCREN                     DECIMAL(3, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CIDTFD_SNAL_LCTO               DECIMAL(1, 0) NOT NULL,
             CTPO_CTA_MOVTO                 DECIMAL(1, 0) NOT NULL,
             CPSSOA_SERVC_TARIF             CHAR(1) NOT NULL,
             CINDCD_SEGDA_LIN               DECIMAL(1, 0) NOT NULL,
             DINIC_VGCIA_REG                DATE NOT NULL,
             NSEQ_FILTR_SELEC               DECIMAL(5, 0) NOT NULL,
             RFILTR_SELEC_EXTRT             CHAR(32) NOT NULL,
             DFIM_VGCIA_REG                 DATE NOT NULL,
             CINDCD_EXTRT_GRP               DECIMAL(1, 0) NOT NULL,
             CINDCD_CRITR_LCTO              DECIMAL(1, 0) NOT NULL,
             CIDTFD_SIT_PARM                DECIMAL(1, 0) NOT NULL,
             CFUNC_INCL_SIST                DECIMAL(9, 0) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CFUNC_EXCL_SIST                DECIMAL(9, 0) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TINFO_COMPL_EXTRT           *
      ******************************************************************
       01  GFCTB0N3.
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
           10 RFILTR-SELEC-EXTRT   PIC X(32).
      *    *************************************************************
           10 DFIM-VGCIA-REG       PIC X(10).
      *    *************************************************************
           10 CINDCD-EXTRT-GRP     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CRITR-LCTO    PIC S9(1)V USAGE COMP-3.
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
