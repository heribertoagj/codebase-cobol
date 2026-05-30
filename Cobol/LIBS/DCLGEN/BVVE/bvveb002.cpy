      ******************************************************************
      * DCLGEN TABLE(DB2PRD.HIST_VDDOR_DESC)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB002))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB002)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.HIST_VDDOR_DESC TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CIDTFD_PRODT_CATAO             DECIMAL(5, 0) NOT NULL,
             VANTCP_VDDOR_ANTER             DECIMAL(17, 2) NOT NULL,
             VANTCP_VDDOR_SEDGA             DECIMAL(17, 2) NOT NULL,
             VANTCP_VDDOR_TERC              DECIMAL(17, 2) NOT NULL,
             VANTCP_VDDOR_ATUAL             DECIMAL(17, 2) NOT NULL,
             VANTCP_VDDOR_QUART             DECIMAL(17, 2) NOT NULL,
             VCREDT_VDDOR_ATUAL             DECIMAL(17, 2) NOT NULL,
             VCREDT_VDDOR_PRIM              DECIMAL(17, 2) NOT NULL,
             VCREDT_VDDOR_SEGDA             DECIMAL(17, 2) NOT NULL,
             VCREDT_VDDOR_TERC              DECIMAL(17, 2) NOT NULL,
             DMOVTO_VDDOR                   DATE NOT NULL,
             VANTCP_VDDOR_SEXTO             DECIMAL(17, 2) NOT NULL,
             VANTCP_VDDOR_QUINT             DECIMAL(17, 2) NOT NULL,
             VCREDT_VDDOR_SEXTO             DECIMAL(17, 2) NOT NULL,
             VCREDT_VDDOR_QUART             DECIMAL(17, 2) NOT NULL,
             VCREDT_VDDOR_QUINT             DECIMAL(17, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.HIST_VDDOR_DESC             *
      ******************************************************************
       01  BVVEB002.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-PRODT-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VANTCP-VDDOR-ANTER   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VANTCP-VDDOR-SEDGA   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VANTCP-VDDOR-TERC    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VANTCP-VDDOR-ATUAL   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VANTCP-VDDOR-QUART   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-VDDOR-ATUAL   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-VDDOR-PRIM    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-VDDOR-SEGDA   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-VDDOR-TERC    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DMOVTO-VDDOR         PIC X(10).
      *    *************************************************************
           10 VANTCP-VDDOR-SEXTO   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VANTCP-VDDOR-QUINT   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-VDDOR-SEXTO   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-VDDOR-QUART   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCREDT-VDDOR-QUINT   PIC S9(15)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
