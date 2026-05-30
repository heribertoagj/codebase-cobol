      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PPSTA_AFLIA_ESTBL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB011))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB011)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PPSTA_AFLIA_ESTBL TABLE
           ( CCNPJ_CPF_ESTBL                DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ESTBL              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_CPF                 DECIMAL(2, 0) NOT NULL,
             IESTBL_AFLIA                   CHAR(32) NOT NULL,
             CINSCR_EST_ESTBL               CHAR(15) NOT NULL,
             IFANTS_ESTBL_AFLIA             CHAR(32) NOT NULL,
             IPLAQT_EQPMT_ESTBL             CHAR(22) NOT NULL,
             CTPO_ATVDD_COML                DECIMAL(5, 0) NOT NULL,
             CCOMPL_ATVDD                   CHAR(1),
             CSIT_PPSTA_AFLIA               DECIMAL(2, 0) NOT NULL,
             DENVIO_PPSTA_AFLIA             DATE,
             DRETOR_PPSTA_AFLIA             DATE,
             DCAD_PPSTA_AFLIA               DATE NOT NULL,
             DATULZ_PPSTA_AFLIA             TIMESTAMP NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CDIG_CCORR_ESTBL               DECIMAL(1, 0) NOT NULL,
             CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CJUNC_DEPDC_BDSCO              DECIMAL(5, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0),
             CEMPR_CONTT                    DECIMAL(5, 0),
             CTPO_TERM                      CHAR(1),
             CMCRO_EMPR_INDVD               CHAR(1),
             NESTBL_AFLIA                   DECIMAL(10, 0),
             CCNPJ_CPF_ESTBL_ST             CHAR(9) NOT NULL,
             CFLIAL_ESTBL_ST                CHAR(4) NOT NULL,
             CCTRL_CNPJ_CPF_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PPSTA_AFLIA_ESTBL           *
      ******************************************************************
       01  BVVEB011.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IESTBL-AFLIA         PIC X(32).
      *    *************************************************************
           10 CINSCR-EST-ESTBL     PIC X(15).
      *    *************************************************************
           10 IFANTS-ESTBL-AFLIA   PIC X(32).
      *    *************************************************************
           10 IPLAQT-EQPMT-ESTBL   PIC X(22).
      *    *************************************************************
           10 CTPO-ATVDD-COML      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-ATVDD         PIC X(1).
      *    *************************************************************
           10 CSIT-PPSTA-AFLIA     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DENVIO-PPSTA-AFLIA   PIC X(10).
      *    *************************************************************
           10 DRETOR-PPSTA-AFLIA   PIC X(10).
      *    *************************************************************
           10 DCAD-PPSTA-AFLIA     PIC X(10).
      *    *************************************************************
           10 DATULZ-PPSTA-AFLIA   PIC X(26).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CCORR-ESTBL     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC-BDSCO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPR-CONTT          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-TERM            PIC X(1).
      *    *************************************************************
           10 CMCRO-EMPR-INDVD     PIC X(1).
      *    *************************************************************
           10 NESTBL-AFLIA         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-ESTBL-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ESTBL-ST      PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-CPF-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 28      *
      ******************************************************************
