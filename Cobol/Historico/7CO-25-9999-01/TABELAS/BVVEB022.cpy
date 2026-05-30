      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TENDER_ESTBL_COML)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB022))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB022)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TENDER_ESTBL_COML TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CTPO_ENDER_ESTBL               CHAR(1) NOT NULL,
             ELOGDR_ESTBL_COML              CHAR(40) NOT NULL,
             ELOGDR_NRO_ESTBL               CHAR(5) NOT NULL,
             ECOMPL_ESTBL_COML              CHAR(20),
             EBAIRO_ESTBL_COML              CHAR(20) NOT NULL,
             ECIDDE_ESTBL_COML              CHAR(30) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TENDER_ESTBL_COML           *
      ******************************************************************
       01  BVVEB022.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ENDER-ESTBL     PIC X(1).
      *    *************************************************************
           10 ELOGDR-ESTBL-COML    PIC X(40).
      *    *************************************************************
           10 ELOGDR-NRO-ESTBL     PIC X(5).
      *    *************************************************************
           10 ECOMPL-ESTBL-COML    PIC X(20).
      *    *************************************************************
           10 EBAIRO-ESTBL-COML    PIC X(20).
      *    *************************************************************
           10 ECIDDE-ESTBL-COML    PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
