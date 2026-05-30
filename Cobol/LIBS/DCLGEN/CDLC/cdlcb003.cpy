      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CATLG_LCTO_MUBCO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CDLCB003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CDLCB003)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CATLG_LCTO_MUBCO TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CIDTFD_LCTO                    DECIMAL(5, 0) NOT NULL,
             CIDTFD_SNAL_LCTO               DECIMAL(1, 0) NOT NULL,
             IIDTFD_HIST_LCTO               CHAR(24) NOT NULL,
             CJUNC_DEPDC_RESP               DECIMAL(5, 0) NOT NULL,
             CGRAU_PRIOR                    DECIMAL(5, 0) NOT NULL,
             DCRIAC_LCTO                    DATE NOT NULL,
             DULTMA_ALT_LCTO                DATE,
             DFIM_LCTO                      DATE,
             IHIST_LCTO_ABRV                CHAR(15) NOT NULL,
             CEXIST_COMPL_HIST              DECIMAL(1, 0) NOT NULL,
             CTPO_ENTRD_LCTO                DECIMAL(1, 0) NOT NULL,
             CTPO_CONS_LCTO                 DECIMAL(1, 0) NOT NULL,
             CEMIS_AVISO_ROTNA              DECIMAL(1, 0) NOT NULL,
             CENVOL_CTA_CLI                 DECIMAL(1, 0) NOT NULL,
             CTPO_OPER_VINC                 DECIMAL(1, 0) NOT NULL,
             CAUTRZ_USO_LCTO                CHAR(1) NOT NULL,
             CRCOLH_IPMF                    DECIMAL(1, 0) NOT NULL,
             DINIC_RCOLH_IPMF               DATE,
             DFIM_RCOLH_IPMF                DATE,
             PALIQT_IPMF                    DECIMAL(7, 4),
             CCTRL_RECOLH_IPMF              CHAR(3),
             CINDCD_TARIF_SERVC             CHAR(1) NOT NULL,
             CAGPTO_CNAB                    DECIMAL(5, 0) NOT NULL,
             IHIST_LCTO_INGLS               CHAR(24) NOT NULL,
             IHIST_LCTO_ESPNH               CHAR(24) NOT NULL,
             HATULZ                         TIMESTAMP NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             CTERM                          CHAR(8) NOT NULL,
             CTPO_MOVTO_TRANS               DECIMAL(1, 0),
             CTPO_COMPS_SDO                 CHAR(1) NOT NULL,
             CTPO_EXCEC_INVES               DECIMAL(2, 0) NOT NULL,
             RHIST_LCTO                     CHAR(70) NOT NULL,
             RJUSTF_LCTO                    CHAR(70) NOT NULL,
             RHIST_ANTER                    CHAR(70),
             CJUNC_DEPDC_ANTER              DECIMAL(5, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CATLG_LCTO_MUBCO            *
      ******************************************************************
       01  CDLCB003.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-LCTO          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SNAL-LCTO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IIDTFD-HIST-LCTO     PIC X(24).
      *    *************************************************************
           10 CJUNC-DEPDC-RESP     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRAU-PRIOR          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DCRIAC-LCTO          PIC X(10).
      *    *************************************************************
           10 DULTMA-ALT-LCTO      PIC X(10).
      *    *************************************************************
           10 DFIM-LCTO            PIC X(10).
      *    *************************************************************
           10 IHIST-LCTO-ABRV      PIC X(15).
      *    *************************************************************
           10 CEXIST-COMPL-HIST    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ENTRD-LCTO      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONS-LCTO       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CEMIS-AVISO-ROTNA    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CENVOL-CTA-CLI       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER-VINC       PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CAUTRZ-USO-LCTO      PIC X(1).
      *    *************************************************************
           10 CRCOLH-IPMF          PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-RCOLH-IPMF     PIC X(10).
      *    *************************************************************
           10 DFIM-RCOLH-IPMF      PIC X(10).
      *    *************************************************************
           10 PALIQT-IPMF          PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-RECOLH-IPMF    PIC X(3).
      *    *************************************************************
           10 CINDCD-TARIF-SERVC   PIC X(1).
      *    *************************************************************
           10 CAGPTO-CNAB          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IHIST-LCTO-INGLS     PIC X(24).
      *    *************************************************************
           10 IHIST-LCTO-ESPNH     PIC X(24).
      *    *************************************************************
           10 HATULZ               PIC X(26).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 CTPO-MOVTO-TRANS     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-COMPS-SDO       PIC X(1).
      *    *************************************************************
           10 CTPO-EXCEC-INVES     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RHIST-LCTO           PIC X(70).
      *    *************************************************************
           10 RJUSTF-LCTO          PIC X(70).
      *    *************************************************************
           10 RHIST-ANTER          PIC X(70).
      *    *************************************************************
           10 CJUNC-DEPDC-ANTER    PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 36      *
      ******************************************************************
