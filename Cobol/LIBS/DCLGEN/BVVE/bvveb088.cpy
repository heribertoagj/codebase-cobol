      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMSGRA_NACIO_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB088))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB088)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMSGRA_NACIO_CATAO TABLE
           ( CMSGRA_SIST                    DECIMAL(3, 0) NOT NULL,
             RMSGRA_SIST                    CHAR(40),
             CSIT_REG                       DECIMAL(1, 0),
             CINDCD_ENVIO_MNUAL             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMSGRA_NACIO_CATAO          *
      ******************************************************************
       01  BVVEB088.
      *    *************************************************************
           10 CMSGRA-SIST          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RMSGRA-SIST          PIC X(40).
      *    *************************************************************
           10 CSIT-REG             PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ENVIO-MNUAL   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
