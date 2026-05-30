      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOPER_GDE_VOLUM)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09M))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09M)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOPER_GDE_VOLUM TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             QRCBVL_GDE_VOLUM               DECIMAL(7, 0) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(7),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOPER_GDE_VOLUM             *
      ******************************************************************
       01  BVVEB09M.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 QRCBVL-GDE-VOLUM     PIC S9(7)V USAGE COMP-3.
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
