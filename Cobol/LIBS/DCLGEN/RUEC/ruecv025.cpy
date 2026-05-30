      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01ORCAM_CDULA)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RUECV025))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECV025)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01ORCAM_CDULA TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL,
             CPCELA_LIBRC                   DECIMAL(3, 0) NOT NULL,
             RTPO_OPER_ORCAM                CHAR(40) NOT NULL,
             CTPO_LIBRC_ORCAM               DECIMAL(1, 0) NOT NULL,
             DLIBRC_OPER_ORCAM              DATE,
             VFINCD_OPER_ORCAM              DECIMAL(15, 2) NOT NULL,
             VPCELA_REC_PPRIO               DECIMAL(15, 2) NOT NULL,
             CTPO_INCON_ORCAM               DECIMAL(1, 0) NOT NULL,
             CINDCD_LIBRC_PCELA             DECIMAL(1, 0) NOT NULL,
             DFIM_EVNTO_CDULA               DATE
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01ORCAM_CDULA              *
      ******************************************************************
       01  RUECV025.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CPCELA-LIBRC         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RTPO-OPER-ORCAM      PIC X(40).
      *    *************************************************************
           10 CTPO-LIBRC-ORCAM     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DLIBRC-OPER-ORCAM    PIC X(10).
      *    *************************************************************
           10 VFINCD-OPER-ORCAM    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPCELA-REC-PPRIO     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-INCON-ORCAM     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-LIBRC-PCELA   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-EVNTO-CDULA     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
