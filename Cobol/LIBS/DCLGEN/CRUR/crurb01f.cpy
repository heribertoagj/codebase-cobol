      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOUTRO_CONFM_BACEN)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB01F))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB01F)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOUTRO_CONFM_BACEN TABLE
           ( COUTRO_TPO_CONFM               DECIMAL(5, 0) NOT NULL,
             COUTRO_CONFM_BACEN             DECIMAL(2, 0),
             ROUTRO_CONFM_BACEN             CHAR(80),
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOUTRO_CONFM_BACEN          *
      ******************************************************************
       01  CRURB01F.
      *    *************************************************************
           10 COUTRO-TPO-CONFM     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-CONFM-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 ROUTRO-CONFM-BACEN   PIC X(80).
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
