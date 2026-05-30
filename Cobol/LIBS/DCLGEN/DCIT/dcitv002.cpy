      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01UF)                                     *
      *        LIBRARY(AD.DB2.DCLGEN(DCITV002))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCITV002)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01UF TABLE
           ( CSGL_UF                        CHAR(2) NOT NULL,
             IUF                            CHAR(25) NOT NULL,
             CUF_IRB                        DECIMAL(3, 0),
             CTERM                          CHAR(8) NOT NULL,
             DCTRL_OPER                     DATE NOT NULL,
             HCTRL_OPER                     TIME NOT NULL,
             CTPO_CTRL_OPER                 CHAR(1) NOT NULL,
             CFUNC                          CHAR(7) NOT NULL,
             CRGIAO_SFH                     DECIMAL(2, 0) NOT NULL,
             CUF                            DECIMAL(3, 0) NOT NULL,
             QDIA_EMISS_TITLO               DECIMAL(5, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01UF                       *
      ******************************************************************
       01  DCITV002.
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 IUF                  PIC X(25).
      *    *************************************************************
           10 CUF-IRB              PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTERM                PIC X(8).
      *    *************************************************************
           10 DCTRL-OPER           PIC X(10).
      *    *************************************************************
           10 HCTRL-OPER           PIC X(8).
      *    *************************************************************
           10 CTPO-CTRL-OPER       PIC X(1).
      *    *************************************************************
           10 CFUNC                PIC X(7).
      *    *************************************************************
           10 CRGIAO-SFH           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CUF                  PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QDIA-EMISS-TITLO     PIC S9(5)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
