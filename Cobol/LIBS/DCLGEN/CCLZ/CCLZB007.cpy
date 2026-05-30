      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPROCM_INFO_RECTA_FEDRL)                   *
      *        LIBRARY(AD.DB2.DCLGEN(CCLZB007))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CCLZB007)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPROCM_INFO_RECTA_FEDRL TABLE
           ( DANO_MES_REFT                  DECIMAL(6, 0) NOT NULL,
             HINIC_PROCM_ENVIO              TIMESTAMP NOT NULL,
             HFIM_PROCM_ENVIO               TIMESTAMP,
             HINIC_RPROC_ENVIO              TIMESTAMP,
             HFIM_RPROC_ENVIO               TIMESTAMP,
             QINCON_SIST_INFO               DECIMAL(9, 0) NOT NULL,
             QINCON_SIST_CTA                DECIMAL(9, 0) NOT NULL,
             QCTA_APRES_RELAT               DECIMAL(9, 0) NOT NULL,
             QREG_APRES_RELAT               DECIMAL(9, 0) NOT NULL,
             VTOT_CREDT_APRES               DECIMAL(15, 2) NOT NULL,
             VTOT_DEB_APRES                 DECIMAL(15, 2) NOT NULL,
             VSDO_TOT_APRES                 DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPROCM_INFO_RECTA_FEDRL     *
      ******************************************************************
       01  CCLZB007.
      *    *************************************************************
           10 DANO-MES-REFT        PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 HINIC-PROCM-ENVIO    PIC X(26).
      *    *************************************************************
           10 HFIM-PROCM-ENVIO     PIC X(26).
      *    *************************************************************
           10 HINIC-RPROC-ENVIO    PIC X(26).
      *    *************************************************************
           10 HFIM-RPROC-ENVIO     PIC X(26).
      *    *************************************************************
           10 QINCON-SIST-INFO     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QINCON-SIST-CTA      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QCTA-APRES-RELAT     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QREG-APRES-RELAT     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 VTOT-CREDT-APRES     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-DEB-APRES       PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VSDO-TOT-APRES       PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
