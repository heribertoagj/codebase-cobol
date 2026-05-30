      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PARM_EMIS_CATAO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB068))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB068)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PARM_EMIS_CATAO TABLE
           ( CPARM_CATAO_CREDT              DECIMAL(3, 0) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CINSTC_LIM_CATAO               DECIMAL(6, 0),
             VMIN_LIM_CATAO                 DECIMAL(11, 2),
             VMAX_LIM_CATAO                 DECIMAL(11, 2),
             COPERD_CICS                    CHAR(3),
             DMOVTO                         DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PARM_EMIS_CATAO             *
      ******************************************************************
       01  BACCB068.
      *    *************************************************************
           10 CPARM-CATAO-CREDT    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-LIM-CATAO     PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VMIN-LIM-CATAO       PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMAX-LIM-CATAO       PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 COPERD-CICS          PIC X(3).
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
