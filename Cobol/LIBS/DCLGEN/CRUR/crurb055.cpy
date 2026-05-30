      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TBNEFC_RURAL)                              *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB055))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB055)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TBNEFC_RURAL TABLE
           ( CPORTE_BNEFC_RURAL             DECIMAL(3, 0) NOT NULL,
             CINSTC_RCONH_PBLIC             DECIMAL(5, 0) NOT NULL,
             IPORTE_BNEFC_RURAL             CHAR(40) NOT NULL,
             RPORTE_BNEFC_RURAL             VARCHAR(256),
             CPORTE_BNEFC_BACEN             DECIMAL(6, 0) NOT NULL,
             VRENDA_MIN_RURAL               DECIMAL(15, 2) NOT NULL,
             VRENDA_MAX_RURAL               DECIMAL(15, 2) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             DINIC_PORTE_RURAL              DATE NOT NULL,
             DFIM_PORTE_RURAL               DATE,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TBNEFC_RURAL                *
      ******************************************************************
       01  CRURB055.
      *    *************************************************************
           10 CPORTE-BNEFC-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-RCONH-PBLIC   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IPORTE-BNEFC-RURAL   PIC X(40).
      *    *************************************************************
           10 RPORTE-BNEFC-RURAL.
              49 RPORTE-BNEFC-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RPORTE-BNEFC-RURAL-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CPORTE-BNEFC-BACEN   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VRENDA-MIN-RURAL     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VRENDA-MAX-RURAL     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 DINIC-PORTE-RURAL    PIC X(10).
      *    *************************************************************
           10 DFIM-PORTE-RURAL     PIC X(10).
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
