      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_AUTRZ_AGNDA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB045))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB045)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_AUTRZ_AGNDA TABLE
           ( NCTRL_AUTRZ_AGNDA              DECIMAL(11, 0) NOT NULL,
             CCNPJ_CPF_AUTRZ                DECIMAL(9, 0),
             CFLIAL_CNPJ_AUTRZ              DECIMAL(5, 0),
             CCTRL_CPF_AUTRZ                DECIMAL(2, 0),
             CBCO                           DECIMAL(3, 0),
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0),
             CCRDRA_ESTBL_COML              DECIMAL(4, 0),
             CTPO_VISAO_BANDE               DECIMAL(3, 0),
             CBANDE_VISAO                   DECIMAL(3, 0),
             CTPO_ARQ_AUTRZ                 DECIMAL(1, 0) NOT NULL,
             CID_TPO_PSSOA                  CHAR(1) NOT NULL,
             CINDCD_DOMCL_BCRIO             CHAR(1) NOT NULL,
             CINDCD_CNPJ_BASE               CHAR(1) NOT NULL,
             CSIT_PROCM_AUTRZ               DECIMAL(1, 0) NOT NULL,
             CSIT_AUTRZ_AGNDA               CHAR(1),
             CCTRL_RQUIS_INSTC              CHAR(20),
             CID_INCL_AUTRZ                 CHAR(20),
             CID_CANCT_AUTRZ                CHAR(20),
             CID_AUTRZ_AGNDA                CHAR(20),
             HENVIO_AUTRZ_AGNDA             TIMESTAMP,
             HRECEB_AUTRZ_AGNDA             TIMESTAMP,
             CERRO_MSGEM_SIST               CHAR(8),
             CUSUAR_INCL                    CHAR(9),
             HINCL_REG                      TIMESTAMP,
             CUSUAR_MANUT                   CHAR(7),
             HMANUT_REG                     TIMESTAMP,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             CPROT_INCL_RCBVL               CHAR(40),
             CPROT_CANCT_RCBVL              CHAR(40),
             CCNPJ_CPF_AUTRZ_ST             CHAR(9),
             CFLIAL_CNPJ_AUTRZ_ST           CHAR(4),
             CCTRL_CPF_AUTRZ_ST             DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_AUTRZ_AGNDA           *
      ******************************************************************
       01  BVVEB045.
      *    *************************************************************
           10 NCTRL-AUTRZ-AGNDA    PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-AUTRZ      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-AUTRZ    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-AUTRZ      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ARQ-AUTRZ       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CID-TPO-PSSOA        PIC X(1).
      *    *************************************************************
           10 CINDCD-DOMCL-BCRIO   PIC X(1).
      *    *************************************************************
           10 CINDCD-CNPJ-BASE     PIC X(1).
      *    *************************************************************
           10 CSIT-PROCM-AUTRZ     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-AUTRZ-AGNDA     PIC X(1).
      *    *************************************************************
           10 CCTRL-RQUIS-INSTC    PIC X(20).
      *    *************************************************************
           10 CID-INCL-AUTRZ       PIC X(20).
      *    *************************************************************
           10 CID-CANCT-AUTRZ      PIC X(20).
      *    *************************************************************
           10 CID-AUTRZ-AGNDA      PIC X(20).
      *    *************************************************************
           10 HENVIO-AUTRZ-AGNDA   PIC X(26).
      *    *************************************************************
           10 HRECEB-AUTRZ-AGNDA   PIC X(26).
      *    *************************************************************
           10 CERRO-MSGEM-SIST     PIC X(8).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPROT-INCL-RCBVL     PIC X(40).
      *    *************************************************************
           10 CPROT-CANCT-RCBVL    PIC X(40).
      *    *************************************************************
           10 CCNPJ-CPF-AUTRZ-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-AUTRZ-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-AUTRZ-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 33      *
      ******************************************************************
