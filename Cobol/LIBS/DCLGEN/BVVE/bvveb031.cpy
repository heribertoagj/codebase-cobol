      ******************************************************************
      * DCLGEN TABLE(DB2PRD.THIST_VDDOR_CREDT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB031))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB031)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.THIST_VDDOR_CREDT TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CATAO               DECIMAL(13, 0) NOT NULL,
             CPRODT_AGNDA_FINCR             DECIMAL(5, 0) NOT NULL,
             DBASE_DEB_CATAO                DECIMAL(6, 0) NOT NULL,
             VCREDT_CATAO_VDDOR             DECIMAL(17, 2) NOT NULL,
             VDEB_CATAO_VDDOR               DECIMAL(17, 2) NOT NULL,
             VBLOQ_GARNT_CONTR              DECIMAL(15, 2) NOT NULL,
             VGARNT_OUTRA_INSTC             DECIMAL(17, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.THIST_VDDOR_CREDT           *
      ******************************************************************
       01  BVVEB031.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CATAO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-AGNDA-FINCR   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DBASE-DEB-CATAO      PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-CATAO-VDDOR   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VDEB-CATAO-VDDOR     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VBLOQ-GARNT-CONTR    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VGARNT-OUTRA-INSTC   PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
