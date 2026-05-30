      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PRMSS_ADSAO_PCOTE)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0C6))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0C6)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PRMSS_ADSAO_PCOTE TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             DINIC_PRMSS_PCOTE              DATE NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             DFIM_PRMSS_PCOTE               DATE NOT NULL,
             CINDCD_ADSAO_INDVD             CHAR(1) NOT NULL,
             CINDCD_AGPTO_TOT               CHAR(1) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             CINDCD_EXCEC_ADSAO             CHAR(1) NOT NULL,
             CIDTFD_PRMSS_INDVD             DECIMAL(1, 0) NOT NULL,
             CINDCD_VSLAO_COMP              DECIMAL(1, 0) NOT NULL,
             CPSSOA_SERVC_TARIF             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PRMSS_ADSAO_PCOTE           *
      ******************************************************************
       01  GFCTB0C6.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-PRMSS-PCOTE    PIC X(10).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-PRMSS-PCOTE     PIC X(10).
      *    *************************************************************
           10 CINDCD-ADSAO-INDVD   PIC X(1).
      *    *************************************************************
           10 CINDCD-AGPTO-TOT     PIC X(1).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CINDCD-EXCEC-ADSAO   PIC X(1).
      *    *************************************************************
           10 CIDTFD-PRMSS-INDVD   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-VSLAO-COMP    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-SERVC-TARIF   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
