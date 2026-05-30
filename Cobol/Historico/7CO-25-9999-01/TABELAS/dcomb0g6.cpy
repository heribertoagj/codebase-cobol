      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTRASI_RUTIL_TITLO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0G6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0G6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTRASI_RUTIL_TITLO TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             NTITLO_DESC_COML               DECIMAL(5, 0) NOT NULL,
             CCNPJ_CPF                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ                    DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             CBCO_PRODT_COBR                DECIMAL(3, 0) NOT NULL,
             CIDTFD_PRODT_COBR              DECIMAL(2, 0) NOT NULL,
             CNEGOC_COBR                    DECIMAL(18, 0) NOT NULL,
             CTITLO_COBR_BCO                DECIMAL(18, 0) NOT NULL,
             CNRO_SEQ_TITLO                 DECIMAL(3, 0) NOT NULL,
             NNOTA_PROMS_RURAL              DECIMAL(11, 0) NOT NULL,
             CINDCD_NOTA_PROMS              CHAR(3) NOT NULL,
             CTPO_PROTR_RURAL               DECIMAL(2, 0) NOT NULL,
             CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             QPRODT_RURAL                   DECIMAL(9, 0) NOT NULL,
             CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             CINDCD_PGDOR_JURO              CHAR(1) NOT NULL,
             DEMIS_TITLO                    DATE NOT NULL,
             DVCTO_TITLO_DESC               DATE,
             VTITLO_DESC_COML               DECIMAL(17, 2),
             CSACDO_TITLO_OPER              DECIMAL(9, 0) NOT NULL,
             CSEU_NRO_TITLO                 CHAR(15) NOT NULL,
             CESPCE_TITLO_COBR              DECIMAL(3, 0) NOT NULL,
             CINDCD_INSTR_DESC              DECIMAL(1, 0) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             ELOGDR                         CHAR(40) NOT NULL,
             IBAIRO                         CHAR(40) NOT NULL,
             IMUN                           CHAR(30) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             CSIT_DESC_COML                 DECIMAL(3, 0) NOT NULL,
             HSIT_DESC_COML                 TIMESTAMP,
             HULT_ATULZ                     TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CCNPJ_CPF_ST                   CHAR(9) NOT NULL,
             CFLIAL_CNPJ_ST                 CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTRASI_RUTIL_TITLO          *
      ******************************************************************
       01  DCOMB0G6.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-PRODT-COBR      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CTITLO-COBR-BCO      PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-SEQ-TITLO       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NNOTA-PROMS-RURAL    PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-NOTA-PROMS    PIC X(3).
      *    *************************************************************
           10 CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QPRODT-RURAL         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PGDOR-JURO    PIC X(1).
      *    *************************************************************
           10 DEMIS-TITLO          PIC X(10).
      *    *************************************************************
           10 DVCTO-TITLO-DESC     PIC X(10).
      *    *************************************************************
           10 VTITLO-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSACDO-TITLO-OPER    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSEU-NRO-TITLO       PIC X(15).
      *    *************************************************************
           10 CESPCE-TITLO-COBR    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-INSTR-DESC    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 ELOGDR               PIC X(40).
      *    *************************************************************
           10 IBAIRO               PIC X(40).
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HSIT-DESC-COML       PIC X(26).
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CCNPJ-CPF-ST         PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-ST       PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 40      *
      ******************************************************************
