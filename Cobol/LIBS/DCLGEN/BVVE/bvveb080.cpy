      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMNTRC_DIA_CATAO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB080))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB080)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMNTRC_DIA_CATAO TABLE
           ( CMNTRC_MSGRA                   CHAR(23) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0),
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
             NMNTRC_TRFGO_MSGEM             DECIMAL(18, 0),
             CINDCD_RPROC_MSGEM             DECIMAL(1, 0) NOT NULL,
             CDOMNO_MSGRA_SIST              CHAR(5),
             CSIST_EXTER                    CHAR(3),
             CEVNTO_MSGEM                   CHAR(4),
             CFLUXO_MSGEM                   CHAR(2),
             CFASE_PROCM                    DECIMAL(4, 0),
             CIDTFD_SIT_FASE                DECIMAL(1, 0),
             HINIC_ETAPA_MNTRC              TIMESTAMP,
             HFNAL_ETAPA_MNTRC              TIMESTAMP,
             RERRO_ETAPA_MNTRC              CHAR(255),
             WCONTD_TEXTO_CATAO             VARCHAR(10244),
             WCONTD_XML_CATAO               VARCHAR(10244),
             WCONTD_RPROC_CATAO             VARCHAR(10244),
             COPER_ENVID_CIPAG              CHAR(20)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMNTRC_DIA_CATAO            *
      ******************************************************************
       01  BVVEB080.
      *    *************************************************************
           10 CMNTRC-MSGRA         PIC X(23).
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
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
           10 NMNTRC-TRFGO-MSGEM   PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-RPROC-MSGEM   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CDOMNO-MSGRA-SIST    PIC X(5).
      *    *************************************************************
           10 CSIST-EXTER          PIC X(3).
      *    *************************************************************
           10 CEVNTO-MSGEM         PIC X(4).
      *    *************************************************************
           10 CFLUXO-MSGEM         PIC X(2).
      *    *************************************************************
           10 CFASE-PROCM          PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SIT-FASE      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINIC-ETAPA-MNTRC    PIC X(26).
      *    *************************************************************
           10 HFNAL-ETAPA-MNTRC    PIC X(26).
      *    *************************************************************
           10 RERRO-ETAPA-MNTRC    PIC X(255).
      *    *************************************************************
           10 WCONTD-TEXTO-CATAO.
              49 WCONTD-TEXTO-CATAO-LEN
                 PIC S9(4) USAGE COMP.
              49 WCONTD-TEXTO-CATAO-TEXT
                 PIC X(10244).
      *    *************************************************************
           10 WCONTD-XML-CATAO.
              49 WCONTD-XML-CATAO-LEN
                 PIC S9(4) USAGE COMP.
              49 WCONTD-XML-CATAO-TEXT
                 PIC X(10244).
      *    *************************************************************
           10 WCONTD-RPROC-CATAO.
              49 WCONTD-RPROC-CATAO-LEN
                 PIC S9(4) USAGE COMP.
              49 WCONTD-RPROC-CATAO-TEXT
                 PIC X(10244).
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *
      ******************************************************************
