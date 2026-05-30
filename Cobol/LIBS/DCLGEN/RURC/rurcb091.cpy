      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TVAR_PRODT_RURAL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RURCB091))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCB091)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TVAR_PRODT_RURAL TABLE
           ( CVAR_PRODT_RURAL               DECIMAL(5, 0) NOT NULL,
             RVAR_PRODT_RURAL               VARCHAR(300) NOT NULL,
             CVAR_PRODT_RECOR               CHAR(5) NOT NULL,
             DINIC_VGCIA                    DATE,
             DFIM_VGCIA                     DATE,
             DULT_ATULZ                     DATE,
             CUSUAR_RESP                    CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TVAR_PRODT_RURAL            *
      ******************************************************************
       01  RURCB091.
      *    *************************************************************
           10 CVAR-PRODT-RURAL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 RVAR-PRODT-RURAL.
              49 RVAR-PRODT-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RVAR-PRODT-RURAL-TEXT
                 PIC X(300).
      *    *************************************************************
           10 CVAR-PRODT-RECOR     PIC X(5).
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA           PIC X(10).
      *    *************************************************************
           10 DULT-ATULZ           PIC X(10).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
