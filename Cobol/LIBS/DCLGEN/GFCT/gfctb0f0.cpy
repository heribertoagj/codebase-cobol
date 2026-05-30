      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TARIF_CORRL_SIST)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0F0))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0F0)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TARIF_CORRL_SIST TABLE
           ( CSIST_CORRL                    CHAR(4) NOT NULL,
             CSERVC_TARIF_CORRL             DECIMAL(5, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TARIF_CORRL_SIST            *
      ******************************************************************
       01  GFCTB0F0.
      *    *************************************************************
           10 CSIST-CORRL          PIC X(4).
      *    *************************************************************
           10 CSERVC-TARIF-CORRL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
