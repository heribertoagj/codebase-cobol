******************************************************************
      * DCLGEN TABLE(DB2PRD.TCLI_SEM_CESTA)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0N9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0N9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCLI_SEM_CESTA TABLE
           ( CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA                     DECIMAL(13, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CINDCD_VNCLO_FL                DECIMAL(1, 0) NOT NULL,
             VRENTB                         DECIMAL(17, 2) NOT NULL,
             CCPF_CNPJ                      DECIMAL(9, 0) NOT NULL,
             CFLIAL_CPF_CNPJ                DECIMAL(5, 0),
             CCTRL_CPF_CNPJ                 DECIMAL(2, 0) NOT NULL,
             IPSSOA_COPLT                   CHAR(70) NOT NULL,
             CPSSOA                         DECIMAL(10, 0) NOT NULL,
             CDDI                           CHAR(4),
             CDDD                           CHAR(4),
             NLIN_TFONI                     DECIMAL(11, 0) NOT NULL,
             CID_RMAL                       CHAR(15),
             CINDCD_REST_CNTAT              DECIMAL(1, 0) NOT NULL,
             DABERT_CTA                     DATE NOT NULL,
             VINVES                         DECIMAL(18, 2) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             DPROCM_ATUAL                   DATE NOT NULL,
             CCPF_CNPJ_ST                   CHAR(9) NOT NULL,
             CFLIAL_CPF_CNPJ_ST             CHAR(4) NOT NULL,
             CCTRL_CPF_CNPJ_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCLI_SEM_CESTA              *
      ******************************************************************
       01  GFCTB0N9.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA           PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-VNCLO-FL      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 VRENTB               PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCPF-CNPJ            PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CPF-CNPJ      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-CNPJ       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPSSOA-COPLT         PIC X(70).
      *    *************************************************************
           10 CPSSOA               PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CDDI                 PIC X(4).
      *    *************************************************************
           10 CDDD                 PIC X(4).
      *    *************************************************************
           10 NLIN-TFONI           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CID-RMAL             PIC X(15).
      *    *************************************************************
           10 CINDCD-REST-CNTAT    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DABERT-CTA           PIC X(10).
      *    *************************************************************
           10 VINVES               PIC S9(16)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 DPROCM-ATUAL         PIC X(10).
      *    *************************************************************
           10 CCPF-CNPJ-ST            PIC X(9).
      *    *************************************************************
           10 CFLIAL-CPF-CNPJ-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-CNPJ-ST       PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 23      *
      ******************************************************************
