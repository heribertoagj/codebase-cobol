      ******************************************************************
      * DCLGEN TABLE(DB2PRD.INCON_PPSTA_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB067))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB067)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.INCON_PPSTA_CATAO TABLE
           ( CCTRO_DIGTC                    DECIMAL(5, 0) NOT NULL,
             CSIST_ENTRD_PPSTA              CHAR(1) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCLI_CATAO_CREDT               CHAR(19) NOT NULL,
             DENTRD_PPSTA_CATAO             DATE NOT NULL,
             CSEQ_PPSTA_CATAO               DECIMAL(7, 0) NOT NULL,
             CREJEI_PPSTA_CATAO             DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.INCON_PPSTA_CATAO           *
      ******************************************************************
       01  BACCB067.
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
           10 CSEQ-PPSTA-CATAO     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CREJEI-PPSTA-CATAO   PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
