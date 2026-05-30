      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TBNEFC_CREDT_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB0L5))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB0L5)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TBNEFC_CREDT_RURAL TABLE
           ( CTPO_BNEFC_RURAL               DECIMAL(2, 0) NOT NULL,
             ITPO_BNEFC_RURAL               CHAR(40) NOT NULL,
             CTPO_BNEFC_BACEN               CHAR(3) NOT NULL,
             DINIC_VGCIA_REG                DATE NOT NULL,
             DFIM_VGCIA_REG                 DATE,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CINDCD_LISTA_COOP              CHAR(1)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TBNEFC_CREDT_RURAL          *
      ******************************************************************
       01  CRURB0L5.
      *    *************************************************************
           10 CTPO-BNEFC-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 ITPO-BNEFC-RURAL     PIC X(40).
      *    *************************************************************
           10 CTPO-BNEFC-BACEN     PIC X(3).
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
      *    *************************************************************
           10 CINDCD-LISTA-COOP    PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
