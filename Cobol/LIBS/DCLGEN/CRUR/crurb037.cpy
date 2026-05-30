      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TORIGE_REC_RURAL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB037))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB037)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TORIGE_REC_RURAL TABLE
           ( CORIGE_REC_CREDT               DECIMAL(3, 0) NOT NULL,
             CORIGE_REC_CONVS               DECIMAL(3, 0),
             IORIGE_REC_CREDT               CHAR(40) NOT NULL,
             RCOMEN_ORIGE_REC               VARCHAR(256),
             DINIC_VGCIA_ORIGE              DATE NOT NULL,
             DFIM_VGCIA_ORIGE               DATE,
             PSPREAD_ORIGE_REC              DECIMAL(7, 4),
             CFONTE_BACEN_RURAL             DECIMAL(6, 0),
             CCTRL_ORIGE_RURAL              DECIMAL(3, 0),
             CENVIO_RECOR_BACEN             CHAR(1) NOT NULL,
             CATULZ_DOTAC_RURAL             CHAR(1) NOT NULL,
             CDOTAC_ORIGE_RURAL             CHAR(1) NOT NULL,
             CPRROG_FINAN_RURAL             CHAR(1) NOT NULL,
             CCTCIA_LIM_BACEN               CHAR(1) NOT NULL,
             CPRMSS_SMULA_RURAL             DECIMAL(1, 0) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CLIN_RURAL_BACEN               DECIMAL(4, 0) NOT NULL,
             CSBSTC_ORIGE_AUTOM             DECIMAL(1, 0) NOT NULL,
             CORIGE_REC_ENQUA               DECIMAL(1, 0) NOT NULL,
             CAPLIC_ORIGE_REC               DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TORIGE_REC_RURAL            *
      ******************************************************************
       01  CRURB037.
      *    *************************************************************
           10 CORIGE-REC-CREDT     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC-CONVS     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IORIGE-REC-CREDT     PIC X(40).
      *    *************************************************************
           10 RCOMEN-ORIGE-REC.
              49 RCOMEN-ORIGE-REC-LEN
                 PIC S9(4) USAGE COMP.
              49 RCOMEN-ORIGE-REC-TEXT
                 PIC X(256).
      *    *************************************************************
           10 DINIC-VGCIA-ORIGE    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-ORIGE     PIC X(10).
      *    *************************************************************
           10 PSPREAD-ORIGE-REC    PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CFONTE-BACEN-RURAL   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-ORIGE-RURAL    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CENVIO-RECOR-BACEN   PIC X(1).
      *    *************************************************************
           10 CATULZ-DOTAC-RURAL   PIC X(1).
      *    *************************************************************
           10 CDOTAC-ORIGE-RURAL   PIC X(1).
      *    *************************************************************
           10 CPRROG-FINAN-RURAL   PIC X(1).
      *    *************************************************************
           10 CCTCIA-LIM-BACEN     PIC X(1).
      *    *************************************************************
           10 CPRMSS-SMULA-RURAL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CLIN-RURAL-BACEN     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CSBSTC-ORIGE-AUTOM   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CORIGE-REC-ENQUA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CAPLIC-ORIGE-REC     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
