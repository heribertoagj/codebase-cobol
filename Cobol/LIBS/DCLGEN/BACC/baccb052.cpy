      ******************************************************************
      * DCLGEN TABLE(DB2PRD.MASTCARD_REM_ORPAG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB052))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB052)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.MASTCARD_REM_ORPAG TABLE
           ( CMAPA_ORPAG                    DECIMAL(6, 0) NOT NULL,
             CCOMPL_MAPA_ORPAG              DECIMAL(1, 0) NOT NULL,
             CREMSS_ORPAG                   DECIMAL(7, 0) NOT NULL,
             DREG                           DATE NOT NULL,
             CPREFX_ESTBL_BACC              DECIMAL(4, 0) NOT NULL,
             CBCO_VISA                      DECIMAL(3, 0) NOT NULL,
             CESTBL_CONVN_BACC              DECIMAL(6, 0) NOT NULL,
             VTOT_LOTE_ORPAG                DECIMAL(15, 2) NOT NULL,
             VLOTE_ORPAG_RTIDO              DECIMAL(15, 2) NOT NULL,
             QTOT_PCELA_LOTE                DECIMAL(5, 0) NOT NULL,
             QTOT_PCELA_RTIDO               DECIMAL(5, 0) NOT NULL,
             QORPAG_LOTE                    DECIMAL(5, 0) NOT NULL,
             QORPAG_RTIDO                   DECIMAL(5, 0) NOT NULL,
             CFORMA_LIQDC_ORPAG             DECIMAL(1, 0) NOT NULL,
             DDIA_REG                       DECIMAL(2, 0) NOT NULL,
             DRECEB                         DATE,
             CCTRO_DIGTC                    DECIMAL(5, 0) NOT NULL,
             CSIT_ORPAG                     DECIMAL(1, 0) NOT NULL,
             CCOMPL_REMSS                   DECIMAL(3, 0) NOT NULL,
             CPRIOR_TRATO_REJEI             DECIMAL(2, 0) NOT NULL,
             DOCOR                          DATE,
             CVARVL_ORPAG_REMSS             CHAR(3) NOT NULL,
             WVARVL_ORPAG_REMSS             CHAR(47) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.MASTCARD_REM_ORPAG          *
      ******************************************************************
       01  BACCB052.
      *    *************************************************************
           10 CMAPA-ORPAG          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-MAPA-ORPAG    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CREMSS-ORPAG         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DREG                 PIC X(10).
      *    *************************************************************
           10 CPREFX-ESTBL-BACC    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-VISA            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CESTBL-CONVN-BACC    PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VTOT-LOTE-ORPAG      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VLOTE-ORPAG-RTIDO    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTOT-PCELA-LOTE      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QTOT-PCELA-RTIDO     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QORPAG-LOTE          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QORPAG-RTIDO         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-LIQDC-ORPAG   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DDIA-REG             PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DRECEB               PIC X(10).
      *    *************************************************************
           10 CCTRO-DIGTC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-ORPAG           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-REMSS         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPRIOR-TRATO-REJEI   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DOCOR                PIC X(10).
      *    *************************************************************
           10 CVARVL-ORPAG-REMSS   PIC X(3).
      *    *************************************************************
           10 WVARVL-ORPAG-REMSS   PIC X(47).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 23      *
      ******************************************************************
