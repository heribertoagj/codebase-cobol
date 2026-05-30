      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOUTRO_TRATO_SOLO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0X1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0X1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOUTRO_TRATO_SOLO TABLE
           ( COUTRO_TRATO_SOLO              DECIMAL(5, 0) NOT NULL,
             COUTRO_SOLO_BACEN              DECIMAL(2, 0) NOT NULL,
             ROUTRO_SOLO_BACEN              CHAR(80) NOT NULL,
             DINIC_VGCIA_REG                DATE NOT NULL,
             DFIM_VGCIA_REG                 DATE,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOUTRO_TRATO_SOLO           *
      ******************************************************************
       01  CRURB0X1.
      *    *************************************************************
           10 COUTRO-TRATO-SOLO    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 COUTRO-SOLO-BACEN    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 ROUTRO-SOLO-BACEN    PIC X(80).
      *    *************************************************************
           10 DINIC-VGCIA-REG      PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-REG       PIC X(10).
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
