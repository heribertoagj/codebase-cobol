      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TINCON_PROCM_RECTA_FEDRL)                  *
      *        LIBRARY(AD.DB2.DCLGEN(CCLZB006))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CCLZB006)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TINCON_PROCM_RECTA_FEDRL TABLE
           ( DANO_MES_REFT                  DECIMAL(6, 0) NOT NULL,
             NDETLH_PROCM_RECTA             DECIMAL(5, 0) NOT NULL,
             NINCON_PROCM_INFO              DECIMAL(9, 0) NOT NULL,
             CID_PSSOA_INCON                CHAR(14),
             CLYOUT_RECTA_FEDRL             DECIMAL(2, 0) NOT NULL,
             CSIST                          CHAR(4) NOT NULL,
             NEVNTO_MSGEM_NEGOC             DECIMAL(4, 0) NOT NULL,
             CREC_GEDOR_MSGEM               DECIMAL(5, 0) NOT NULL,
             CIDIOM_TEXTO_MSGEM             DECIMAL(3, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TINCON_PROCM_RECTA_FEDRL    *
      ******************************************************************
       01  CCLZB006.
      *    *************************************************************
           10 DANO-MES-REFT        PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 NDETLH-PROCM-RECTA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NINCON-PROCM-INFO    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CID-PSSOA-INCON      PIC X(14).
      *    *************************************************************
           10 CLYOUT-RECTA-FEDRL   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSIST                PIC X(4).
      *    *************************************************************
           10 NEVNTO-MSGEM-NEGOC   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CREC-GEDOR-MSGEM     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDIOM-TEXTO-MSGEM   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
