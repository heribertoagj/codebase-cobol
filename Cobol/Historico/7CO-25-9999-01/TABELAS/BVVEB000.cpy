      ******************************************************************
      * DCLGEN TABLE(DB2PRD.INFORMACAO_EDBS)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.INFORMACAO_EDBS TABLE
           ( CAG_CTA                        DECIMAL(12, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CORR                      DECIMAL(7, 0) NOT NULL,
             IVDDOR_VISA                    CHAR(40) NOT NULL,
             CCGC_CPF                       DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC                     DECIMAL(4, 0) NOT NULL,
             CCTRL_CPF_CGC                  DECIMAL(2, 0) NOT NULL,
             ELOGDR_COPLT                   CHAR(40) NOT NULL,
             IMUN                           CHAR(30) NOT NULL,
             CSGL_UF                        CHAR(2) NOT NULL,
             CCEP                           DECIMAL(5, 0) NOT NULL,
             CCOMPL_CEP                     CHAR(3) NOT NULL,
             CDDD                           CHAR(4) NOT NULL,
             CFONE                          CHAR(8) NOT NULL,
             VLIM_ANTCP_VDDOR               DECIMAL(15, 2) NOT NULL,
             PLIM_ANTEC_VDDOR               DECIMAL(5, 2) NOT NULL,
             NLIN_TFONI                     DECIMAL(11, 0),
             CCGC_CPF_ST                    CHAR(9) NOT NULL,
             CFLIAL_CGC_ST                  CHAR(4) NOT NULL,
             CCTRL_CPF_CGC_ST               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.INFORMACAO_EDBS             *
      ******************************************************************
       01  BVVEB000.
      *    *************************************************************
           10 CAG-CTA              PIC S9(12)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 IVDDOR-VISA          PIC X(40).
      *    *************************************************************
           10 CCGC-CPF             PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CGC           PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-CGC        PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 ELOGDR-COPLT         PIC X(40).
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCOMPL-CEP           PIC X(3).
      *    *************************************************************
           10 CDDD                 PIC X(4).
      *    *************************************************************
           10 CFONE                PIC X(8).
      *    *************************************************************
           10 VLIM-ANTCP-VDDOR     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 PLIM-ANTEC-VDDOR     PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 NLIN-TFONI           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-ST          PIC X(9).
      *    *************************************************************
           10 CFLIAL-CGC-ST        PIC X(4).
      *    *************************************************************
           10 CCTRL-CPF-CGC-ST     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
