      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TFICHA_CAD_RURAL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB069))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB069)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TFICHA_CAD_RURAL TABLE
           ( CPSSOA                         DECIMAL(10, 0) NOT NULL,
             CFICHA_CAD_RURAL               DECIMAL(3, 0) NOT NULL,
             CSIT_FICHA_RURAL               DECIMAL(3, 0) NOT NULL,
             CPSSOA_JURID                   DECIMAL(10, 0) NOT NULL,
             NSEQ_UND_ORGNZ                 DECIMAL(8, 0) NOT NULL,
             CPORTE_BNEFC_RURAL             DECIMAL(3, 0) NOT NULL,
             RCOMPL_CAD_RURAL               VARCHAR(2550),
             CRESP_CAD_RURAL                CHAR(9) NOT NULL,
             HABERT_CAD_RURAL               TIMESTAMP NOT NULL,
             CUSUAR_VALDC                   CHAR(9),
             HVALDC_REG                     TIMESTAMP,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CPSSOA_EMPRG_RURAL             DECIMAL(1, 0),
             CPERM_RESP_EXTER               CHAR(1)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TFICHA_CAD_RURAL            *
      ******************************************************************
       01  CRURB069.
      *    *************************************************************
           10 CPSSOA               PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CFICHA-CAD-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-FICHA-RURAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPSSOA-JURID         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-UND-ORGNZ       PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 CPORTE-BNEFC-RURAL   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RCOMPL-CAD-RURAL.
              49 RCOMPL-CAD-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RCOMPL-CAD-RURAL-TEXT
                 PIC X(2550).
      *    *************************************************************
           10 CRESP-CAD-RURAL      PIC X(9).
      *    *************************************************************
           10 HABERT-CAD-RURAL     PIC X(26).
      *    *************************************************************
           10 CUSUAR-VALDC         PIC X(9).
      *    *************************************************************
           10 HVALDC-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CPSSOA-EMPRG-RURAL   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPERM-RESP-EXTER     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
