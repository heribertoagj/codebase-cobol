      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01GARTD_RURAL)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RURCV035))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RURCV035)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01GARTD_RURAL TABLE
           ( CGARTD                         DECIMAL(3, 0) NOT NULL,
             CTPO_GARTD                     DECIMAL(1, 0) NOT NULL,
             IPSSOA                         CHAR(60) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             CDIG_DEPDC                     CHAR(1) NOT NULL,
             CDIG_CTA_CORR                  CHAR(1) NOT NULL,
             CINDCD_AVISO_DEB               CHAR(1) NOT NULL,
             CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC                     DECIMAL(5, 0) NOT NULL,
             CCTRL_CPF_CGC                  DECIMAL(2, 0) NOT NULL,
             CINDCD_LCTO_DEB                CHAR(1) NOT NULL,
             DANO_LIM_OPER                  DECIMAL(4, 0),
             CDEPDC_LIM_OPER                DECIMAL(5, 0),
             CAPROV_LIM_OPER                DECIMAL(9, 0),
             CTPO_GARNT                     DECIMAL(3, 0),
             CCGC_CPF_ST                    CHAR(9) NOT NULL,
             CFLIAL_CGC_ST                  CHAR(5) NOT NULL,
             CCTRL_CPF_CGC_ST               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01GARTD_RURAL              *
      ******************************************************************
       01  RURCV035.
      *    *************************************************************
           10 CGARTD               PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-GARTD           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 IPSSOA               PIC X(60).
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-DEPDC           PIC X(1).
      *    *************************************************************
           10 CDIG-CTA-CORR        PIC X(1).
      *    *************************************************************
           10 CINDCD-AVISO-DEB     PIC X(1).
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CGC           PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-CGC        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-LCTO-DEB      PIC X(1).
      *    *************************************************************
           10 DANO-LIM-OPER        PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC-LIM-OPER      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAPROV-LIM-OPER      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      *    *************************************************************
           10 CFLIAL-CGC-ST        PIC X(5).
      *    *************************************************************
           10 CCTRL-CPF-CGC-ST     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
