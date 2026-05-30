      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TFLEXZ_CNPJ_PRINC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CEPTB016))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CEPTB016)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TFLEXZ_CNPJ_PRINC TABLE
           ( CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CPCOTE_PRODT_TARIF             DECIMAL(3, 0) NOT NULL,
             CPRODT_CTRL_EXCED              DECIMAL(3, 0) NOT NULL,
             DINIC_CONCS_DESC               DATE NOT NULL,
             DFNAL_CONCS_DESC               DATE NOT NULL,
             PDESC_TARIF_EXCED              DECIMAL(5, 2) NOT NULL,
             QTRANS_BONIF_EXCED             DECIMAL(5, 0) NOT NULL,
             VDESC_TARIF_EXCED              DECIMAL(17, 2) NOT NULL,
             CSIT_DESC_FLEXZ                DECIMAL(1, 0) NOT NULL,
             DINCL_CTRL_EXCED               DATE NOT NULL,
             DULT_ATULZ_EXCED               DATE,
             CFUNC_INCL_EXCED               DECIMAL(9, 0) NOT NULL,
             CFUNC_ULT_ATULZ                DECIMAL(9, 0),
             CCGC_CPF_ST                    CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TFLEXZ_CNPJ_PRINC           *
      ******************************************************************
       01  CEPTB016.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CPCOTE-PRODT-TARIF   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-CTRL-EXCED    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-CONCS-DESC     PIC X(10).
      *    *************************************************************
           10 DFNAL-CONCS-DESC     PIC X(10).
      *    *************************************************************
           10 PDESC-TARIF-EXCED    PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QTRANS-BONIF-EXCED   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VDESC-TARIF-EXCED    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSIT-DESC-FLEXZ      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DINCL-CTRL-EXCED     PIC X(10).
      *    *************************************************************
           10 DULT-ATULZ-EXCED     PIC X(10).
      *    *************************************************************
           10 CFUNC-INCL-EXCED     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-ULT-ATULZ      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
