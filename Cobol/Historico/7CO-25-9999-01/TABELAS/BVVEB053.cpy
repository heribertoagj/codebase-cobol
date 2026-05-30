      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAGNDA_FINCR_CIPAG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB053))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB053)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAGNDA_FINCR_CIPAG TABLE
           ( CCNPJ_TTLAR_RCBVL              DECIMAL(9, 0) NOT NULL,
             CFLIAL_TTLAR_RCBVL             DECIMAL(4, 0) NOT NULL,
             CCTRL_CNPJ_RCBVL               DECIMAL(2, 0) NOT NULL,
             CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             DVCTO_UND_RCBVL                DATE NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CCNPJ_USUAR_FNAL               DECIMAL(9, 0) NOT NULL,
             CFLIAL_USUAR_RCDOR             DECIMAL(4, 0) NOT NULL,
             CCTRL_CNPJ_USUAR               DECIMAL(2, 0) NOT NULL,
             VTOT_AGNDA_TTLAR               DECIMAL(17, 2) NOT NULL,
             VGARNT_INSTC_BDSCO             DECIMAL(17, 2) NOT NULL,
             VGARNT_OUTRA_INSTC             DECIMAL(17, 2) NOT NULL,
             VCSSAO_INSTC_BDSCO             DECIMAL(17, 2) NOT NULL,
             VAGNDA_DISPN_OPER              DECIMAL(17, 2) NOT NULL,
             VONUS_RESER_TECNC              DECIMAL(17, 2) NOT NULL,
             DEFETV_LIQDC_RCBVL             DATE,
             HATULZ                         TIMESTAMP NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             CCNPJ_TTLAR_RCBVL_ST           CHAR(9) NOT NULL,
             CFLIAL_TTLAR_RCBVL_ST          CHAR(4) NOT NULL,
             CCTRL_CNPJ_RCBVL_ST            DECIMAL(2, 0) NOT NULL,
             CCNPJ_USUAR_FNAL_ST            CHAR(9) NOT NULL,
             CFLIAL_USUAR_RCDOR_ST          CHAR(4) NOT NULL,
             CCTRL_CNPJ_USUAR_ST            DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAGNDA_FINCR_CIPAG          *
      ******************************************************************
       01  BVVEB053.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-UND-RCBVL      PIC X(10).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-USUAR-FNAL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-USUAR-RCDOR   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-USUAR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VTOT-AGNDA-TTLAR     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VGARNT-INSTC-BDSCO   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VGARNT-OUTRA-INSTC   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCSSAO-INSTC-BDSCO   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VAGNDA-DISPN-OPER    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VONUS-RESER-TECNC    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DEFETV-LIQDC-RCBVL   PIC X(10).
      *    *************************************************************
           10 HATULZ               PIC X(26).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL-ST
              PIC X(9).
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL-ST  PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-USUAR-FNAL-ST  PIC X(9).
      *    *************************************************************
           10 CFLIAL-USUAR-RCDOR-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-USUAR-ST  PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 28      *
      ******************************************************************
