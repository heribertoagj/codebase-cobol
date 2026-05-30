      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01TPO_PROTR_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RURCV022))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCV022)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01TPO_PROTR_RURAL TABLE
           ( CTPO_PROTR_RURAL               DECIMAL(2, 0) NOT NULL,
             RTPO_PROTR_RURAL               CHAR(40) NOT NULL,
             CTPO_PROTR_BACEN               DECIMAL(5, 0) NOT NULL,
             VLIM_INFER_RENDA               DECIMAL(15, 2) NOT NULL,
             VLIM_SUPE_RENDA                DECIMAL(15, 2) NOT NULL,
             CSTTUS_ATIVO_INATI             CHAR(1) NOT NULL,
             CCATEG_PROTR_RECOR             CHAR(4) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01TPO_PROTR_RURAL          *
      ******************************************************************
       01  RURCV022.
      *    *************************************************************
           10 CTPO-PROTR-RURAL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-PROTR-RURAL     PIC X(40).
      *    *************************************************************
           10 CTPO-PROTR-BACEN     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VLIM-INFER-RENDA     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VLIM-SUPE-RENDA      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSTTUS-ATIVO-INATI   PIC X(1).
      *    *************************************************************
           10 CCATEG-PROTR-RECOR   PIC X(4).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
