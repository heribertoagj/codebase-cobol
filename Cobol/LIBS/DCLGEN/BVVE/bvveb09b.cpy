      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMNTRC_MSGRA_EXPUR_CATAO)                  *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09B))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09B)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMNTRC_MSGRA_EXPUR_CATAO TABLE
           ( CMNTRC_MSGRA                   CHAR(23) NOT NULL,
             NESTRT_INFO_BACEN              DECIMAL(3, 0),
             NVRSAO_ESTRT_INFO              DECIMAL(3, 0),
             CSNLZD_MSGRA                   DECIMAL(3, 0),
             CINDCD_ORIGE_TRFGO             DECIMAL(1, 0),
             CINDCD_ORIGE_PLATF             DECIMAL(1, 0),
             CIDTFD_PROT_ARQ                DECIMAL(1, 0),
             HPROCM_MSGEM                   TIMESTAMP,
             HULT_ATULZ                     TIMESTAMP,
             HMOVTO_INFO_MSGEM              TIMESTAMP,
             HCONF_ENVIO_MSGEM              TIMESTAMP,
             HCONF_LEITU_MSGEM              TIMESTAMP,
             CISPB_DSTNA_MSGEM              CHAR(8),
             CISPB_EMISR_MSGEM              CHAR(8),
             CCTRL_RQUIS_INSTC              CHAR(20),
             CUNIC_TRFGO_MSGEM              CHAR(23),
             RIDTFD_MSGEM                   CHAR(48),
             RMNTRC_TRFGO_MSGEM             CHAR(100),
             NMNTRC_TRFGO_MSGEM             DECIMAL(18, 0),
             CINDCD_RPROC_MSGEM             DECIMAL(1, 0) NOT NULL,
             CDOMNO_MSGRA_SIST              CHAR(5)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMNTRC_MSGRA_EXPUR_CATAO    *
      ******************************************************************
       01  BVVEB09B.
      *    *************************************************************
           10 CMNTRC-MSGRA         PIC X(23).
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NVRSAO-ESTRT-INFO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSNLZD-MSGRA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ORIGE-TRFGO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ORIGE-PLATF   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PROT-ARQ      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HPROCM-MSGEM         PIC X(26).
      *    *************************************************************
           10 HULT-ATULZ           PIC X(26).
      *    *************************************************************
           10 HMOVTO-INFO-MSGEM    PIC X(26).
      *    *************************************************************
           10 HCONF-ENVIO-MSGEM    PIC X(26).
      *    *************************************************************
           10 HCONF-LEITU-MSGEM    PIC X(26).
      *    *************************************************************
           10 CISPB-DSTNA-MSGEM    PIC X(8).
      *    *************************************************************
           10 CISPB-EMISR-MSGEM    PIC X(8).
      *    *************************************************************
           10 CCTRL-RQUIS-INSTC    PIC X(20).
      *    *************************************************************
           10 CUNIC-TRFGO-MSGEM    PIC X(23).
      *    *************************************************************
           10 RIDTFD-MSGEM         PIC X(48).
      *    *************************************************************
           10 RMNTRC-TRFGO-MSGEM   PIC X(100).
      *    *************************************************************
           10 NMNTRC-TRFGO-MSGEM   PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RPROC-MSGEM   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CDOMNO-MSGRA-SIST    PIC X(5).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 21      *
      ******************************************************************
