      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01UNIDADE_MEDD)                           *
      *        LIBRARY(AD.DB2.DCLGEN(RURCV002))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCV002)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01UNIDADE_MEDD TABLE
           ( CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             IUND_MEDD                      CHAR(40) NOT NULL,
             CUTILZ_UND_MEDD                DECIMAL(1, 0) NOT NULL,
             CSGL_UND_MEDD                  CHAR(3) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01UNIDADE_MEDD             *
      ******************************************************************
       01  RURCV002.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IUND-MEDD            PIC X(40).
      *    *************************************************************
           10 CUTILZ-UND-MEDD      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSGL-UND-MEDD        PIC X(3).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
