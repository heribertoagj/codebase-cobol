***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PARM_CTRL_GSTAO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0A0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0A0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PARM_CTRL_GSTAO TABLE
           ( CDEPDC_GSTOR                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_GSTOR              DATE NOT NULL,
             DFIM_VGCIA_GSTOR               DATE NOT NULL,
             QMAX_TENTV_GSTOR               DECIMAL(3, 0) NOT NULL,
             QMAX_FLEXZ                     DECIMAL(5, 0) NOT NULL,
             QMAX_RTCAO                     DECIMAL(3, 0) NOT NULL,
             QMAX_CAREN                     DECIMAL(3, 0) NOT NULL,
             QMES_EXCL_TARIF                DECIMAL(2, 0) NOT NULL,
             QMES_EXCL_PARM                 DECIMAL(2, 0) NOT NULL,
             QMAX_PAREC_ESTRN               DECIMAL(2, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CSIT_ONLINE                    DECIMAL(1, 0) NOT NULL,
             QMAX_EXCL_FLEXZ                DECIMAL(2, 0) NOT NULL,
             QMAX_FRANQ_TARIF               DECIMAL(3, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CCART_DEB_UNFCA                DECIMAL(5, 0) NOT NULL,
             RSEGDA_LIN_UNFCA               CHAR(32) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PARM_CTRL_GSTAO             *
      ******************************************************************
       01  GFCTB0A0.
      *    *************************************************************
           10 CDEPDC-GSTOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-GSTOR    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-GSTOR     PIC X(10).
      *    *************************************************************
           10 QMAX-TENTV-GSTOR     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-FLEXZ           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-RTCAO           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-CAREN           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMES-EXCL-TARIF      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QMES-EXCL-PARM       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-PAREC-ESTRN     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CSIT-ONLINE          PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-EXCL-FLEXZ      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-FRANQ-TARIF     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCART-DEB-UNFCA      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RSEGDA-LIN-UNFCA     PIC X(32).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
