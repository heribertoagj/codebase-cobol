      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_TARIF)                                *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB082))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB082)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_TARIF TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CLI                     DECIMAL(16, 0) NOT NULL,
             DVCTO_FAT_CATAO                DATE NOT NULL,
             CSERVC                         DECIMAL(5, 0) NOT NULL,
             CSEQ                           DECIMAL(9, 0) NOT NULL,
             CBCO_CLI                       DECIMAL(3, 0) NOT NULL,
             CAG_CLI                        DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(13, 0) NOT NULL,
             DTRANS                         DATE NOT NULL,
             VTRANS                         DECIMAL(15, 2) NOT NULL,
             CTRANS                         CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_TARIF                  *
      ******************************************************************
       01  BACCB082.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CLI           PIC S9(16)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-FAT-CATAO      PIC X(10).
      *    *************************************************************
           10 CSERVC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ                 PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-CLI             PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-CLI              PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 DTRANS               PIC X(10).
      *    *************************************************************
           10 VTRANS               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTRANS               PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
