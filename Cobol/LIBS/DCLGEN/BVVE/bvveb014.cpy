      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPO_FONE_ESTBL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB014))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB014)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPO_FONE_ESTBL TABLE
           ( CTPO_FONE_ESTBL                DECIMAL(1, 0) NOT NULL,
             RTPO_FONE_ESTBL                CHAR(20) NOT NULL,
             DDSATV_TPO_FONE                DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPO_FONE_ESTBL              *
      ******************************************************************
       01  BVVEB014.
      *    *************************************************************
           10 CTPO-FONE-ESTBL      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-FONE-ESTBL      PIC X(20).
      *    *************************************************************
           10 DDSATV-TPO-FONE      PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
