      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_VDDOR_DEB)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB018))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB018)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_VDDOR_DEB TABLE
           ( CBCO_CATAO                     DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA_CATAO                DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CATAO               DECIMAL(13, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             DBASE_DEB_CATAO                DECIMAL(6, 0) NOT NULL,
             VDEB_CATAO_VDDOR               DECIMAL(17, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_VDDOR_DEB             *
      ******************************************************************
       01  BVVEB018.
      *    *************************************************************
           10 CBCO-CATAO           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA-CATAO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CATAO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DBASE-DEB-CATAO      PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VDEB-CATAO-VDDOR     PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
