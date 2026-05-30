      ******************************************************************
      * DCLGEN TABLE(DB2PRD.RELAT_PDRAO_SIST)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0D4))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0D4)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.RELAT_PDRAO_SIST TABLE
           ( CIMPRE_RELAT                   CHAR(8) NOT NULL,
             RRELAT_SIST                    CHAR(130) NOT NULL,
             CPER_EMIS_RELAT                DECIMAL(1, 0) NOT NULL,
             DULT_IMPRE_RELAT               DATE NOT NULL,
             DPROX_EMIS_RELAT               DATE NOT NULL,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.RELAT_PDRAO_SIST            *
      ******************************************************************
       01  GFCTB0D4.
      *    *************************************************************
           10 CIMPRE-RELAT         PIC X(8).
      *    *************************************************************
           10 RRELAT-SIST          PIC X(130).
      *    *************************************************************
           10 CPER-EMIS-RELAT      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DULT-IMPRE-RELAT     PIC X(10).
      *    *************************************************************
           10 DPROX-EMIS-RELAT     PIC X(10).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
