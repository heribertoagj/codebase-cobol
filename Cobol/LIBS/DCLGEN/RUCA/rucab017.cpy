      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMANUT_PCELA_RECOR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUCAB017))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUCAB017)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMANUT_PCELA_RECOR TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CTPO_PCELA                     DECIMAL(1, 0) NOT NULL,
             NPCELA_CONTR                   DECIMAL(3, 0) NOT NULL,
             NATULZ_PCELA                   DECIMAL(3, 0) NOT NULL,
             DVCTO_PCELA_CONTR              DATE NOT NULL,
             VPCELA                         DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMANUT_PCELA_RECOR          *
      ******************************************************************
       01  RUCAB017.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CTPO-PCELA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NPCELA-CONTR         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NATULZ-PCELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-PCELA-CONTR    PIC X(10).
      *    *************************************************************
           10 VPCELA               PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
