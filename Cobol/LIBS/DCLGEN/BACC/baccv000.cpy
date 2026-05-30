      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01INSTC_ASSOC)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01INSTC_ASSOC TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCICLO_FATMT_CATAO             DECIMAL(2, 0) NOT NULL,
             CDREIT_BONUS                   DECIMAL(1, 0) NOT NULL,
             CMOEDA_INSTC                   CHAR(1) NOT NULL,
             IINSTC_ASSOC                   CHAR(35) NOT NULL,
             PBONUS                         DECIMAL(5, 0) NOT NULL,
             PCALC_COMIS_VDDOR              DECIMAL(5, 0) NOT NULL,
             VLIM_BONUS_INFER               DECIMAL(13, 2) NOT NULL,
             VLIM_BONUS_SUPE                DECIMAL(13, 2) NOT NULL,
             PPROVS_VDA_CATAO               DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01INSTC_ASSOC              *
      ******************************************************************
       01  BACCV000.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCICLO-FATMT-CATAO   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CDREIT-BONUS         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CMOEDA-INSTC         PIC X(1).
      *    *************************************************************
           10 IINSTC-ASSOC         PIC X(35).
      *    *************************************************************
           10 PBONUS               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 PCALC-COMIS-VDDOR    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VLIM-BONUS-INFER     PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VLIM-BONUS-SUPE      PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PPROVS-VDA-CATAO     PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
