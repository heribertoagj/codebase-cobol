      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ENDER_ESTBL)                               *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB003)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ENDER_ESTBL TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CTPO_ENDER_AFLIA               DECIMAL(1, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             IMUN_ESTBL                     CHAR(28) NOT NULL,
             EESTBL_AFLIA                   CHAR(64) NOT NULL,
             EEMAIL_CNTAT_ESTBL             CHAR(50),
             ICNTAT_ESTBL_AFLIA             CHAR(32) NOT NULL,
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ENDER_ESTBL                 *
      ******************************************************************
       01  BVVEB003.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ENDER-AFLIA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 IMUN-ESTBL           PIC X(28).
      *    *************************************************************
           10 EESTBL-AFLIA         PIC X(64).
      *    *************************************************************
           10 EEMAIL-CNTAT-ESTBL   PIC X(50).
      *    *************************************************************
           10 ICNTAT-ESTBL-AFLIA   PIC X(32).
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
