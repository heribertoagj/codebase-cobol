      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRAMO_ATVDD_ESTBL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB019))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB019)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRAMO_ATVDD_ESTBL TABLE
           ( CRAMO_ATVDD_ESTBL              DECIMAL(5, 0) NOT NULL,
             IRAMO_ATVDD_ESTBL              CHAR(60) NOT NULL,
             PTX_PCELD_JURO                 DECIMAL(5, 2) NOT NULL,
             PTX_CREDT_ROTVO                DECIMAL(5, 2) NOT NULL,
             PTX_CREDT_PCELD                DECIMAL(5, 2) NOT NULL,
             PTX_VDA_INTL                   DECIMAL(5, 2) NOT NULL,
             PTX_CATAO_DEB                  DECIMAL(5, 2) NOT NULL,
             COBRIG_CERTF_PROFS             CHAR(1) NOT NULL,
             CACTAC_PSSOA_FIS               CHAR(1) NOT NULL,
             DINCL_ESTBL_COML               DATE NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL,
             DALT_ESTBL_COML                DATE,
             CFUNC_ALT_ESTBL                DECIMAL(9, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRAMO_ATVDD_ESTBL           *
      ******************************************************************
       01  BVVEB019.
      *    *************************************************************
           10 CRAMO-ATVDD-ESTBL    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IRAMO-ATVDD-ESTBL    PIC X(60).
      *    *************************************************************
           10 PTX-PCELD-JURO       PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PTX-CREDT-ROTVO      PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PTX-CREDT-PCELD      PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PTX-VDA-INTL         PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PTX-CATAO-DEB        PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 COBRIG-CERTF-PROFS   PIC X(1).
      *    *************************************************************
           10 CACTAC-PSSOA-FIS     PIC X(1).
      *    *************************************************************
           10 DINCL-ESTBL-COML     PIC X(10).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DALT-ESTBL-COML      PIC X(10).
      *    *************************************************************
           10 CFUNC-ALT-ESTBL      PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
