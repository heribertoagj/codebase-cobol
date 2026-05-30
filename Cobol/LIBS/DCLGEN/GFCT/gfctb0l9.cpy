***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TLOG_SOLTC_CORSP)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0L9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0L9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TLOG_SOLTC_CORSP TABLE
           ( HEFTVC_REG_SIST                TIMESTAMP NOT NULL,
             CTRANS_SOLTC_CORSP             CHAR(8) NOT NULL,
             CFNCAO_SOLTC_CORSP             CHAR(4) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CIDTFD_SOLTC_CORSP             CHAR(20) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CIDTFD_SOLTC_ORIGN             CHAR(20) NOT NULL,
             CSIT_EXTRC_REG                 CHAR(1) NOT NULL,
             CSIT_SOLTC_CORSP               CHAR(1) NOT NULL,
             CIDTFD_MSGEM_RETOR             DECIMAL(5, 0) NOT NULL,
             WLOG_REG_OCOR                  VARCHAR(3000) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TLOG_SOLTC_CORSP            *
      ******************************************************************
       01  GFCTB0L9.
      *    *************************************************************
           10 HEFTVC-REG-SIST      PIC X(26).
      *    *************************************************************
           10 CTRANS-SOLTC-CORSP   PIC X(8).
      *    *************************************************************
           10 CFNCAO-SOLTC-CORSP   PIC X(4).
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SOLTC-CORSP   PIC X(20).
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SOLTC-ORIGN   PIC X(20).
      *    *************************************************************
           10 CSIT-EXTRC-REG       PIC X(1).
      *    *************************************************************
           10 CSIT-SOLTC-CORSP     PIC X(1).
      *    *************************************************************
           10 CIDTFD-MSGEM-RETOR   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 WLOG-REG-OCOR.
              49 WLOG-REG-OCOR-LEN
                 PIC S9(4) USAGE COMP.
              49 WLOG-REG-OCOR-TEXT
                 PIC X(3000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
