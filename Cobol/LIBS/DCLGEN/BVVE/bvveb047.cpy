      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPCCAO_AGNDA_FINCR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB047))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB047)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPCCAO_AGNDA_FINCR TABLE
           ( NPCCAO_AGNDA_FINCR             DECIMAL(13, 0) NOT NULL,
             CCNPJ_CPF_AUTRZ                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_AUTRZ              DECIMAL(5, 0) NOT NULL,
             CCTRL_CPF_AUTRZ                DECIMAL(2, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0),
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0),
             CPRODT_AGNDA_FINCR             DECIMAL(5, 0),
             CID_TPO_PSSOA                  CHAR(1),
             DPGTO_AGNDA_FINCR              DATE NOT NULL,
             VPGTO_AGNDA_FINCR              DECIMAL(15, 2) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(7),
             HMANUT_REG                     TIMESTAMP,
             CCNPJ_CPF_AUTRZ_ST             CHAR(9) NOT NULL,
             CFLIAL_CNPJ_AUTRZ_ST           CHAR(4) NOT NULL,
             CCTRL_CPF_AUTRZ_ST             DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPCCAO_AGNDA_FINCR          *
      ******************************************************************
       01  BVVEB047.
      *    *************************************************************
           10 NPCCAO-AGNDA-FINCR   PIC S9(13)V USAGE COMP-3.
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
           10 CPRODT-AGNDA-FINCR   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CID-TPO-PSSOA        PIC X(1).
      *    *************************************************************
           10 DPGTO-AGNDA-FINCR    PIC X(10).
      *    *************************************************************
           10 VPGTO-AGNDA-FINCR    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CCNPJ-CPF-AUTRZ-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-AUTRZ-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-AUTRZ-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
