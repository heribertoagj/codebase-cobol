      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARM_SIST_CREDT)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RCORB001))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RCORB001)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARM_SIST_CREDT TABLE
           ( CSIST                          CHAR(4) NOT NULL,
             DINIC_VGCIA_PARM               DATE NOT NULL,
             CINDCD_AMBTE_SIST              DECIMAL(1, 0) NOT NULL,
             CINDCD_ARQTT_SIST              DECIMAL(1, 0) NOT NULL,
             CPROG                          CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HMANUT_REG                     TIMESTAMP,
             CUSUAR_MANUT                   CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARM_SIST_CREDT            *
      ******************************************************************
       01  RCORB001.
      *    *************************************************************
           10 CSIST                PIC X(4).
      *    *************************************************************
           10 DINIC-VGCIA-PARM     PIC X(10).
      *    *************************************************************
           10 CINDCD-AMBTE-SIST    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ARQTT-SIST    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CPROG                PIC X(8).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
