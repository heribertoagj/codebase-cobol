      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRCBVL_PDIDO_RVSAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB00D))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB00D)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRCBVL_PDIDO_RVSAO TABLE
           ( NPDIDO_RVSAO_OPER              DECIMAL(9, 0) NOT NULL,
             CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             DVCTO_UND_RCBVL                DATE NOT NULL,
             CCNPJ_USUAR_FNAL               DECIMAL(9, 0) NOT NULL,
             CFLIAL_USUAR_RCDOR             DECIMAL(4, 0) NOT NULL,
             CCTRL_CNPJ_USUAR               DECIMAL(2, 0) NOT NULL,
             CCNPJ_TTLAR_RCBVL              DECIMAL(9, 0) NOT NULL,
             CFLIAL_TTLAR_RCBVL             DECIMAL(4, 0) NOT NULL,
             CCTRL_CNPJ_RCBVL               DECIMAL(2, 0) NOT NULL,
             VUND_RCBVL_RVSAO               DECIMAL(17, 2) NOT NULL,
             CCNPJ_USUAR_FNAL_ST            CHAR(9) NOT NULL,
             CFLIAL_USUAR_RCDOR_ST          CHAR(4) NOT NULL,
             CCTRL_CNPJ_USUAR_ST            DECIMAL(2, 0) NOT NULL,
             CCNPJ_TTLAR_RCBVL_ST           CHAR(9) NOT NULL,
             CFLIAL_TTLAR_RCBVL_ST          CHAR(4) NOT NULL,
             CCTRL_CNPJ_RCBVL_ST            DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRCBVL_PDIDO_RVSAO          *
      ******************************************************************
       01  BVVEB00D.
      *    *************************************************************
           10 NPDIDO-RVSAO-OPER    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-UND-RCBVL      PIC X(10).
      *    *************************************************************
           10 CCNPJ-USUAR-FNAL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-USUAR-RCDOR   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-USUAR     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VUND-RCBVL-RVSAO     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-USUAR-FNAL-ST  PIC X(9).
      *    *************************************************************
           10 CFLIAL-USUAR-RCDOR-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-USUAR-ST  PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-TTLAR-RCBVL-ST
              PIC X(9).
      *    *************************************************************
           10 CFLIAL-TTLAR-RCBVL-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-RCBVL-ST  PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
