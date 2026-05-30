      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDOCTO_CREDT_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB061))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB061)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDOCTO_CREDT_RURAL TABLE
           ( CDOCTO_CREDT_RURAL             DECIMAL(3, 0) NOT NULL,
             CDOCTO                         DECIMAL(5, 0),
             CTPO_DOCTO_RURAL               DECIMAL(2, 0) NOT NULL,
             CDOCTO_CORSP                   CHAR(8),
             RDOCTO_RURAL                   VARCHAR(256),
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             DINIC_VGCIA_DOCTO              DATE NOT NULL,
             DFIM_VGCIA_DOCTO               DATE,
             IDOCTO_CREDT_RURAL             CHAR(40) NOT NULL,
             CINSTT_RURAL_BACEN             DECIMAL(2, 0),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDOCTO_CREDT_RURAL          *
      ******************************************************************
       01  CRURB061.
      *    *************************************************************
           10 CDOCTO-CREDT-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-DOCTO-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CDOCTO-CORSP         PIC X(8).
      *    *************************************************************
           10 RDOCTO-RURAL.
              49 RDOCTO-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RDOCTO-RURAL-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 DINIC-VGCIA-DOCTO    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-DOCTO     PIC X(10).
      *    *************************************************************
           10 IDOCTO-CREDT-RURAL   PIC X(40).
      *    *************************************************************
           10 CINSTT-RURAL-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
