      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PRMSS_GRP_EMPR)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0C8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0C8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PRMSS_GRP_EMPR TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             DINIC_PRMSS_PCOTE              DATE NOT NULL,
             CJUNC_DEPDC_DEB                DECIMAL(5, 0) NOT NULL,
             CRZ_CTA_DEB                    DECIMAL(5, 0) NOT NULL,
             CNRO_CTA_DEB                   DECIMAL(7, 0) NOT NULL,
             CLCTO_CTA_DEB                  DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PRMSS_GRP_EMPR              *
      ******************************************************************
       01  GFCTB0C8.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-PRMSS-PCOTE    PIC X(10).
      *    *************************************************************
           10 CJUNC-DEPDC-DEB      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTA-DEB          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CNRO-CTA-DEB         PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO-CTA-DEB        PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
