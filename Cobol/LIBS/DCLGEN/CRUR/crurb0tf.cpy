      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TINSTT_CREDT_BACEN)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0TF))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0TF)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TINSTT_CREDT_BACEN TABLE
           ( CINSTT_RURAL_BACEN             DECIMAL(2, 0) NOT NULL,
             CINSTT_SIST_BACEN              DECIMAL(2, 0) NOT NULL,
             CSGL_INSTT_BACEN               CHAR(6) NOT NULL,
             RINSTT_RURAL_BACEN             CHAR(80) NOT NULL,
             DINIC_VGCIA_REG                DATE NOT NULL,
             DFIM_VGCIA_REG                 DATE,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TINSTT_CREDT_BACEN          *
      ******************************************************************
       01  CRURB0TF.
      *    *************************************************************
           10 CINSTT-RURAL-BACEN   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTT-SIST-BACEN    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSGL-INSTT-BACEN     PIC X(6).
      *    *************************************************************
           10 RINSTT-RURAL-BACEN   PIC X(80).
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
