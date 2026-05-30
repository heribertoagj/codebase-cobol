      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01PSSOA_CDULA)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RUECV008))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECV008)                                     *
      *        APOST                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01PSSOA_CDULA TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL,
             CSEQ_PSSOA                     DECIMAL(3, 0) NOT NULL,
             CTPO_RESP_PSSOA                CHAR(1) NOT NULL,
             IPSSOA                         CHAR(60) NOT NULL,
             CTPO_PSSOA                     DECIMAL(1, 0) NOT NULL,
             CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC                     DECIMAL(5, 0) NOT NULL,
             CCTRL_CPF_CGC                  DECIMAL(2, 0) NOT NULL,
             VRESP_RURAL                    DECIMAL(15, 2),
             VRESP_GRAL                     DECIMAL(15, 2),
             VSDO_MED_TMES                  DECIMAL(15, 2),
             VSDO_MED_SEMTL                 DECIMAL(15, 2),
             VRENDA_AGROP_OUTRO             DECIMAL(15, 2),
             VRENDA_NAO_AGROP               DECIMAL(15, 2),
             DCAD                           DATE,
             DANO_RENDM                     DECIMAL(4, 0),
             CSEQ_HIPOT_RURAL               DECIMAL(2, 0),
             CTPO_DOCTO_PSSOA               DECIMAL(3, 0),
             CDOCTO_PSSOA                   CHAR(15),
             CORG_EMISR_DOCTO               CHAR(4),
             CUND_ORG_EMISR                 CHAR(2),
             CSEXO                          CHAR(1),
             CEST_CVIL_RGIME                DECIMAL(2, 0),
             DEST_CVIL                      DATE,
             CNAC                           DECIMAL(3, 0),
             CSGL_UF                        CHAR(2),
             INAT_ESTRG                     CHAR(40),
             ELOGDR                         CHAR(40) NOT NULL,
             ENRO_LOGDR                     CHAR(5) NOT NULL,
             ECOMPL_LOGDR                   CHAR(15),
             EBAIRO_LOGDR                   CHAR(20) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             CMUN_RURAL                     DECIMAL(9, 0) NOT NULL,
             CCATEG_PROFS_CREDT             DECIMAL(5, 0),
             CPROFS_PSSOA                   DECIMAL(5, 0),
             CCLASS_ATVDD_ECONC             CHAR(2),
             CRAMO_ATVDD_ECONC              DECIMAL(3, 0),
             CSRAMO_ATVDD_ECONC             DECIMAL(3, 0),
             CATVDD_ECONC                   DECIMAL(3, 0),
             CCGC_CPF_ST                    CHAR(9) NOT NULL,
             CFLIAL_CGC_ST                  CHAR(5) NOT NULL,
             CCTRL_CPF_CGC_ST               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01PSSOA_CDULA              *
      ******************************************************************
       01  RUECV008.
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
           10 CSEQ-PSSOA           PIC S9(3)V USAGE COMP-3.
           10 CTPO-RESP-PSSOA      PIC X(1).
           10 IPSSOA               PIC X(60).
           10 CTPO-PSSOA           PIC S9(1)V USAGE COMP-3.
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
           10 CFLIAL-CGC           PIC S9(5)V USAGE COMP-3.
           10 CCTRL-CPF-CGC        PIC S9(2)V USAGE COMP-3.
           10 VRESP-RURAL          PIC S9(13)V9(2) USAGE COMP-3.
           10 VRESP-GRAL           PIC S9(13)V9(2) USAGE COMP-3.
           10 VSDO-MED-TMES        PIC S9(13)V9(2) USAGE COMP-3.
           10 VSDO-MED-SEMTL       PIC S9(13)V9(2) USAGE COMP-3.
           10 VRENDA-AGROP-OUTRO   PIC S9(13)V9(2) USAGE COMP-3.
           10 VRENDA-NAO-AGROP     PIC S9(13)V9(2) USAGE COMP-3.
           10 DCAD                 PIC X(10).
           10 DANO-RENDM           PIC S9(4)V USAGE COMP-3.
           10 CSEQ-HIPOT-RURAL     PIC S9(2)V USAGE COMP-3.
           10 CTPO-DOCTO-PSSOA     PIC S9(3)V USAGE COMP-3.
           10 CDOCTO-PSSOA         PIC X(15).
           10 CORG-EMISR-DOCTO     PIC X(4).
           10 CUND-ORG-EMISR       PIC X(2).
           10 CSEXO                PIC X(1).
           10 CEST-CVIL-RGIME      PIC S9(2)V USAGE COMP-3.
           10 DEST-CVIL            PIC X(10).
           10 CNAC                 PIC S9(3)V USAGE COMP-3.
           10 CSGL-UF              PIC X(2).
           10 INAT-ESTRG           PIC X(40).
           10 ELOGDR               PIC X(40).
           10 ENRO-LOGDR           PIC X(5).
           10 ECOMPL-LOGDR         PIC X(15).
           10 EBAIRO-LOGDR         PIC X(20).
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
           10 CMUN-RURAL           PIC S9(9)V USAGE COMP-3.
           10 CCATEG-PROFS-CREDT   PIC S9(5)V USAGE COMP-3.
           10 CPROFS-PSSOA         PIC S9(5)V USAGE COMP-3.
           10 CCLASS-ATVDD-ECONC   PIC X(2).
           10 CRAMO-ATVDD-ECONC    PIC S9(3)V USAGE COMP-3.
           10 CSRAMO-ATVDD-ECONC   PIC S9(3)V USAGE COMP-3.
           10 CATVDD-ECONC         PIC S9(3)V USAGE COMP-3.
           10 CCGC-CPF-ST          PIC X(9).
           10 CFLIAL-CGC-ST        PIC X(5).
           10 CCTRL-CPF-CGC-ST     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 44      *
      ******************************************************************
