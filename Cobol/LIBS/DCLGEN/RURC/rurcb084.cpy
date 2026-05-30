      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TEMPTO_FINAN_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB084))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB084)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TEMPTO_FINAN_RURAL TABLE
           ( NEMPTO_RURAL                   DECIMAL(9, 0) NOT NULL,
             CFNALD_RURAL                   DECIMAL(3, 0) NOT NULL,
             CATVDD_RURAL                   DECIMAL(3, 0) NOT NULL,
             CMODLD_RURAL                   DECIMAL(5, 0) NOT NULL,
             CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             CVAR_PRODT_RURAL               DECIMAL(5, 0) NOT NULL,
             CCESTA_SAFRA_RURAL             DECIMAL(5, 0) NOT NULL,
             CINDCD_ZNMTO                   DECIMAL(1, 0) NOT NULL,
             CINDCD_CSCIO                   DECIMAL(1, 0) NOT NULL,
             CUND_MEDD_FINCD                DECIMAL(3, 0) NOT NULL,
             CUND_MEDD_PROVV                DECIMAL(3, 0) NOT NULL,
             CCTRO_CUSTO                    CHAR(4) NOT NULL,
             DINIC_VGCIA                    DATE NOT NULL,
             DFIM_VGCIA                     DATE NOT NULL,
             DULT_ATULZ                     DATE NOT NULL,
             CUSUAR_RESP                    CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TEMPTO_FINAN_RURAL          *
      ******************************************************************
       01  RURCB084.
      *    *************************************************************
           10 NEMPTO-RURAL         PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFNALD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CATVDD-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMODLD-RURAL         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CVAR-PRODT-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCESTA-SAFRA-RURAL   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-ZNMTO         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-CSCIO         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-FINCD      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD-PROVV      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRO-CUSTO          PIC X(4).
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA           PIC X(10).
      *    *************************************************************
           10 DULT-ATULZ           PIC X(10).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
