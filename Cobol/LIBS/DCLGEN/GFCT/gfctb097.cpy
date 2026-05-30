***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.NAVG_OPCAO_TELA)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB097))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB097)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.NAVG_OPCAO_TELA TABLE
           ( CTELA_SIST                     CHAR(8) NOT NULL,
             CNRO_OPCAO_TELA                DECIMAL(2, 0) NOT NULL,
             CTELA_DSTNO                    CHAR(8) NOT NULL,
             ROPCAO_TELA                    CHAR(59) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.NAVG_OPCAO_TELA             *
      ******************************************************************
       01  GFCTB097.
      *    *************************************************************
           10 CTELA-SIST           PIC X(8).
      *    *************************************************************
           10 CNRO-OPCAO-TELA      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CTELA-DSTNO          PIC X(8).
      *    *************************************************************
           10 ROPCAO-TELA          PIC X(59).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
