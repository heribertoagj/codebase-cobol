      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONTR_ENVIO_RECOR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB099))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB099)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONTR_ENVIO_RECOR TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             NENVIO_RECOR                   DECIMAL(5, 0) NOT NULL,
             CCNPJ_ENTID_RESP               DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_ENTID              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_ENTID               DECIMAL(2, 0) NOT NULL,
             CCNPJ_PARTC                    DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_PARTC              DECIMAL(5, 0) NOT NULL,
             CCTRL_CNPJ_PARTC               DECIMAL(2, 0) NOT NULL,
             CSIT_OPER_RURAL                CHAR(2) NOT NULL,
             DCDULA                         DATE,
             DVCTO_CONTR                    DATE,
             CCDULA_RECOR                   CHAR(17) NOT NULL,
             CTPO_INSTT_CREDT               CHAR(2) NOT NULL,
             VFINAN                         DECIMAL(15, 2),
             CCATEG_PROTR_RECOR             CHAR(4),
             CPROG_RURAL                    CHAR(4),
             CTPO_FONTE_RECOR               CHAR(4) NOT NULL,
             CMUN_REFT_BACEN                DECIMAL(6, 0),
             CEMPTO_BACEN                   CHAR(14) NOT NULL,
             CSIST_PROD                     CHAR(13) NOT NULL,
             VPCELA_LIBRC                   DECIMAL(15, 2),
             VPCELA_REC_PPRIO               DECIMAL(15, 2),
             PALIQT_PROAGRO                 DECIMAL(5, 2) NOT NULL,
             PJURO_ENCAR_FINCR              DECIMAL(8, 5) NOT NULL,
             CTPO_ENCAR_COMPL               DECIMAL(2, 0) NOT NULL,
             CTSORO_NACIO                   CHAR(15) NOT NULL,
             PRISCO_TSORO_NACIO             DECIMAL(5, 2) NOT NULL,
             PRISCO_FUNDO_CNSTT             DECIMAL(5, 2) NOT NULL,
             QAREA_RECOR                    DECIMAL(9, 2) NOT NULL,
             QUND_FINCD_RURAL               DECIMAL(11, 2),
             CUND_MEDD_FINCD                DECIMAL(3, 0) NOT NULL,
             QUND_PROD_PROVV                DECIMAL(11, 2),
             CUND_MEDD_PROVV                DECIMAL(3, 0) NOT NULL,
             DINIC_SAFRA                    DECIMAL(6, 0),
             DFIM_SAFRA                     DECIMAL(6, 0),
             CTPO_GARNT_RECOR               DECIMAL(3, 0),
             VRECTA_BRUTA_RECOR             DECIMAL(17, 2) NOT NULL,
             DMOVTO_RECOR                   DATE NOT NULL,
             CCNPJ_RESP_ST                  CHAR(9) NOT NULL,
             CFLIAL_ENTID_ST                CHAR(5) NOT NULL,
             CCTRL_ENTID_ST                 DECIMAL(2, 0) NOT NULL,
             CCNPJ_PARTC_ST                 CHAR(9) NOT NULL,
             CFLIAL_PARTC_ST                CHAR(5) NOT NULL,
             CCTRL_PARTC_ST                 DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONTR_ENVIO_RECOR          *
      ******************************************************************
       01  RUECB099.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NENVIO-RECOR         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-ENTID-RESP     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-ENTID    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-ENTID     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-PARTC          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-PARTC    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-PARTC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-OPER-RURAL      PIC X(2).
      *    *************************************************************
           10 DCDULA               PIC X(10).
      *    *************************************************************
           10 DVCTO-CONTR          PIC X(10).
      *    *************************************************************
           10 CCDULA-RECOR         PIC X(17).
      *    *************************************************************
           10 CTPO-INSTT-CREDT     PIC X(2).
      *    *************************************************************
           10 VFINAN               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCATEG-PROTR-RECOR   PIC X(4).
      *    *************************************************************
           10 CPROG-RURAL          PIC X(4).
      *    *************************************************************
           10 CTPO-FONTE-RECOR     PIC X(4).
      *    *************************************************************
           10 CMUN-REFT-BACEN      PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CEMPTO-BACEN         PIC X(14).
      *    *************************************************************
           10 CSIST-PROD           PIC X(13).
      *    *************************************************************
           10 VPCELA-LIBRC         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPCELA-REC-PPRIO     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PALIQT-PROAGRO       PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PJURO-ENCAR-FINCR    PIC S9(3)V9(5) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ENCAR-COMPL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTSORO-NACIO         PIC X(15).
      *    *************************************************************
           10 PRISCO-TSORO-NACIO   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PRISCO-FUNDO-CNSTT   PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QAREA-RECOR          PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QUND-FINCD-RURAL     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-FINCD      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QUND-PROD-PROVV      PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-PROVV      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-SAFRA          PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-SAFRA           PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-GARNT-RECOR     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VRECTA-BRUTA-RECOR   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DMOVTO-RECOR         PIC X(10).
      *    *************************************************************
           10 CCNPJ-RESP-ST        PIC X(9).
      *    *************************************************************
           10 CFLIAL-ENTID-ST      PIC X(5).
      *    *************************************************************
           10 CCTRL-ENTID-ST       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-PARTC-ST       PIC X(9).
      *    *************************************************************
           10 CFLIAL-PARTC-ST      PIC X(5).
      *    *************************************************************
           10 CCTRL-PARTC-ST       PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 44      *
      ******************************************************************
