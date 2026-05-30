      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PPSTA_CATAO_CREDT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB065))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB065)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PPSTA_CATAO_CREDT TABLE
           ( CCTRO_DIGTC                    DECIMAL(5, 0) NOT NULL,
             CSIST_ENTRD_PPSTA              CHAR(1) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCLI_CATAO_CREDT               CHAR(19) NOT NULL,
             DENTRD_PPSTA_CATAO             DATE NOT NULL,
             CFORMA_PGTO_CATAO              CHAR(1),
             CJUNC_DEPDC                    DECIMAL(5, 0),
             CCTA_CORR                      DECIMAL(7, 0),
             CDIG_CTA_CORR                  CHAR(1),
             CTPO_PSSOA                     DECIMAL(1, 0),
             ELOGDR_CMAST                   CHAR(60),
             CCEP                           DECIMAL(5, 0),
             CCEP_COMPL                     DECIMAL(3, 0),
             IMUN                           CHAR(30),
             CSGL_UF                        CHAR(2),
             CFONE_CMAST                    CHAR(18)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PPSTA_CATAO_CREDT           *
      ******************************************************************
       01  BACCB065.
      *    *************************************************************
           10 CCTRO-DIGTC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIST-ENTRD-PPSTA    PIC X(1).
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCLI-CATAO-CREDT     PIC X(19).
      *    *************************************************************
           10 DENTRD-PPSTA-CATAO   PIC X(10).
      *    *************************************************************
           10 CFORMA-PGTO-CATAO    PIC X(1).
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CTA-CORR        PIC X(1).
      *    *************************************************************
           10 CTPO-PSSOA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 ELOGDR-CMAST         PIC X(60).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CFONE-CMAST          PIC X(18).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
