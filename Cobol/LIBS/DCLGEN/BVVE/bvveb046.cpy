      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TESTBL_AUTRZ_AGNDA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB046))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB046)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TESTBL_AUTRZ_AGNDA TABLE
           ( NESTBL_AUTRZ_AGNDA             DECIMAL(9, 0) NOT NULL,
             CCNPJ_CPF_AUTRZ                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_AUTRZ              DECIMAL(5, 0) NOT NULL,
             CCTRL_CPF_AUTRZ                DECIMAL(2, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0),
             CAG_BCRIA                      DECIMAL(5, 0),
             CCTA_BCRIA_CLI                 DECIMAL(13, 0),
             CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             CID_TPO_PSSOA                  CHAR(1) NOT NULL,
             CINDCD_DOMCL_BCRIO             CHAR(1) NOT NULL,
             NCTRL_AUTRZ_AGNDA              DECIMAL(11, 0),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(7),
             HMANUT_REG                     TIMESTAMP,
             DASS_AUTRZ_AGNDA               DATE NOT NULL,
             DINIC_AUTRZ_AGNDA              DATE NOT NULL,
             DFIM_AUTRZ_AGNDA               DATE NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             CCNPJ_CPF_AUTRZ_ST             CHAR(9) NOT NULL,
             CFLIAL_CNPJ_AUTRZ_ST           CHAR(4) NOT NULL,
             CCTRL_CPF_AUTRZ_ST             DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TESTBL_AUTRZ_AGNDA          *
      ******************************************************************
       01  BVVEB046.
      *    *************************************************************
           10 NESTBL-AUTRZ-AGNDA   PIC S9(9)V USAGE COMP-3.
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
           10 CID-TPO-PSSOA        PIC X(1).
      *    *************************************************************
           10 CINDCD-DOMCL-BCRIO   PIC X(1).
      *    *************************************************************
           10 NCTRL-AUTRZ-AGNDA    PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 DASS-AUTRZ-AGNDA     PIC X(10).
      *    *************************************************************
           10 DINIC-AUTRZ-AGNDA    PIC X(10).
      *    *************************************************************
           10 DFIM-AUTRZ-AGNDA     PIC X(10).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-AUTRZ-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-AUTRZ-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-AUTRZ-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
