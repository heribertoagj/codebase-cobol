      ******************************************************************
      * DCLGEN TABLE(DB2PRD.CREDIT_VOUCHER)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB072))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB072)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.CREDIT_VOUCHER TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CSUFXO_CATAO_CREDT             DECIMAL(3, 0) NOT NULL,
             DTMOV                          DECIMAL(7, 0) NOT NULL,
             TIPCAPT                        DECIMAL(2, 0) NOT NULL,
             SEQUENC                        DECIMAL(8, 0) NOT NULL,
             VORPAG_ORIGN                   DECIMAL(11, 2) NOT NULL,
             CSGL_MOEDA_ORIGN               DECIMAL(3, 0) NOT NULL,
             CREFT_ORPAG                    CHAR(23) NOT NULL,
             IESTBL                         CHAR(35) NOT NULL,
             DCOMPR                         DATE NOT NULL,
             CTPO_ATVDD_COML                DECIMAL(5, 0) NOT NULL,
             CTRANS                         DECIMAL(2, 0) NOT NULL,
             CPENDENCIA                     CHAR(1) NOT NULL,
             VORPAG_CONVE                   DECIMAL(11, 2) NOT NULL,
             CSGL_MOEDA_CONVE               DECIMAL(3, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.CREDIT_VOUCHER              *
      ******************************************************************
       01  BACCB072.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSUFXO-CATAO-CREDT   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DTMOV                PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 TIPCAPT              PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 SEQUENC              PIC S9(8)V USAGE COMP-3.
      *    *************************************************************
           10 VORPAG-ORIGN         PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSGL-MOEDA-ORIGN     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CREFT-ORPAG          PIC X(23).
      *    *************************************************************
           10 IESTBL               PIC X(35).
      *    *************************************************************
           10 DCOMPR               PIC X(10).
      *    *************************************************************
           10 CTPO-ATVDD-COML      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTRANS               PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPENDENCIA           PIC X(1).
      *    *************************************************************
           10 VORPAG-CONVE         PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSGL-MOEDA-CONVE     PIC S9(3)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
