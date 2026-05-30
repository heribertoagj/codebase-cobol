***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.POSTO_BCRIO_SERVC)                         *
      *        LIBRARY(AD.DB2.DCLGEN(MESUB062))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(MESUB062)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.POSTO_BCRIO_SERVC TABLE
           ( CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(5, 0) NOT NULL,
             CTPO_POSTO_SERVC               DECIMAL(2, 0) NOT NULL,
             IPOSTO_SERVC                   CHAR(20) NOT NULL,
             ELOGDR_POSTO                   CHAR(40) NOT NULL,
             ENRO_POSTO                     DECIMAL(5, 0),
             ECOMPL_POSTO                   CHAR(30),
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCEP_COMPL                     DECIMAL(3, 0) NOT NULL,
             CFUNC_RESP_POSTO               DECIMAL(9, 0) NOT NULL,
             HINIC_EXPDT                    TIME,
             HFNAL_EXPDT                    TIME,
             DINIC_OPER                     DATE,
             DONLINE                        DATE,
             DENCRR                         DATE,
             DULT_INSTA                     DATE,
             DPOSSE_RESP                    DATE,
             CDISTR_CORSP                   DECIMAL(5, 0),
             CSETOR_TESOU                   CHAR(3),
             CTESOU                         DECIMAL(5, 0),
             CEXIST_LIN_CARRO               DECIMAL(1, 0),
             CMALOT                         CHAR(1),
             CTARIF_EMIS_EXTRT              DECIMAL(1, 0),
             CTARIF_CATAO_MAGNT             DECIMAL(1, 0),
             CTARIF_CHEQ_ESPCL              DECIMAL(1, 0),
             CTARIF_CATAO_CREDT             DECIMAL(1, 0),
             DMUDCA_TPO_PAB                 DATE,
             CCNPJ_POSTO                    DECIMAL(9, 0),
             CLJ_CNPJ_POSTO                 DECIMAL(4, 0),
             CDIG_CNPJ_POSTO                DECIMAL(2, 0),
             QDIA_VNCLO                     DECIMAL(2, 0),
             CMUN                           DECIMAL(5, 0),
             CCOMPE_PAC                     DECIMAL(3, 0),
             CDISTC_AG_BDSCO                DECIMAL(5, 0),
             ICNTAT_POSTO                   CHAR(50),
             EEMAIL_POSTO                   CHAR(40),
             CFORNC_NUMER_POSTO             CHAR(1),
             CRCOLH_NUMER_POSTO             CHAR(1),
             CDEPDC_NUMER_POSTO             DECIMAL(5, 0),
             CRGIAO_TARIF                   DECIMAL(5, 0),
             QDIA_ENVIO_LOG                 DECIMAL(3, 0),
             CCORSP_BCRIO                   CHAR(15)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.POSTO_BCRIO_SERVC           *
      ******************************************************************
       01  MESUB062.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-POSTO-SERVC     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IPOSTO-SERVC         PIC X(20).
      *    *************************************************************
           10 ELOGDR-POSTO         PIC X(40).
      *    *************************************************************
           10 ENRO-POSTO           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 ECOMPL-POSTO         PIC X(30).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-RESP-POSTO     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINIC-EXPDT          PIC X(8).
      *    *************************************************************
           10 HFNAL-EXPDT          PIC X(8).
      *    *************************************************************
           10 DINIC-OPER           PIC X(10).
      *    *************************************************************
           10 DONLINE              PIC X(10).
      *    *************************************************************
           10 DENCRR               PIC X(10).
      *    *************************************************************
           10 DULT-INSTA           PIC X(10).
      *    *************************************************************
           10 DPOSSE-RESP          PIC X(10).
      *    *************************************************************
           10 CDISTR-CORSP         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSETOR-TESOU         PIC X(3).
      *    *************************************************************
           10 CTESOU               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CEXIST-LIN-CARRO     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CMALOT               PIC X(1).
      *    *************************************************************
           10 CTARIF-EMIS-EXTRT    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-CATAO-MAGNT   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-CHEQ-ESPCL    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTARIF-CATAO-CREDT   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DMUDCA-TPO-PAB       PIC X(10).
      *    *************************************************************
           10 CCNPJ-POSTO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CLJ-CNPJ-POSTO       PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CNPJ-POSTO      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-VNCLO           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CMUN                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPE-PAC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDISTC-AG-BDSCO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 ICNTAT-POSTO         PIC X(50).
      *    *************************************************************
           10 EEMAIL-POSTO         PIC X(40).
      *    *************************************************************
           10 CFORNC-NUMER-POSTO   PIC X(1).
      *    *************************************************************
           10 CRCOLH-NUMER-POSTO   PIC X(1).
      *    *************************************************************
           10 CDEPDC-NUMER-POSTO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CRGIAO-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-ENVIO-LOG       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCORSP-BCRIO         PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 43      *
      ******************************************************************
