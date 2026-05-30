      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TBANDE_VISAO_EXTER)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB044))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB044)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TBANDE_VISAO_EXTER TABLE
           ( CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             CTPO_AMBTE_EXTER               DECIMAL(1, 0) NOT NULL,
             CBANDE_VISAO_EXTER             CHAR(5) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(7),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TBANDE_VISAO_EXTER          *
      ******************************************************************
       01  BVVEB044.
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-AMBTE-EXTER     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO-EXTER   PIC X(5).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
