      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TESTRT_GRP_CATAO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB067))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB067)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TESTRT_GRP_CATAO TABLE
           ( CPRFIL_ACSSO_MSGRA             DECIMAL(4, 0) NOT NULL,
             NESTRT_INFO_BACEN              DECIMAL(3, 0) NOT NULL,
             CINDCD_PRMSS_EVNTO             DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TESTRT_GRP_CATAO            *
      ******************************************************************
       01  BVVEB067.
      *    *************************************************************
           10 CPRFIL-ACSSO-MSGRA   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PRMSS-EVNTO   PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
