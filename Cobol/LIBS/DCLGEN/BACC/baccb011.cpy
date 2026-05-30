      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SOLTC_GERAL_GDAD)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB011))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB011)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SOLTC_GERAL_GDAD TABLE
           ( DSOLTC                         DATE NOT NULL,
             CD_ITEM_ANTER                  CHAR(5),
             CD_SUBITEM_ANTER               CHAR(3),
             CCCUST_ANTER                   CHAR(4),
             CROTNA_EXTRC_ANTER             CHAR(4),
             CCONSL_ORCAM_ANTER             DECIMAL(7, 0),
             CREG_ANTER_RECTA               DECIMAL(3, 0),
             CREG_ANTER_SDO                 DECIMAL(3, 0),
             CANTER_ESTAT_DESP              DECIMAL(3, 0),
             CREG_ANTER_DESP                DECIMAL(3, 0),
             CANTER_ESTAT_RECTA             DECIMAL(3, 0),
             CD_ITEM_ATUAL                  CHAR(5),
             CD_SUBITEM_ATUAL               CHAR(3),
             CCCUST_ATUAL                   CHAR(4),
             CROTNA_EXTRC_ATUAL             CHAR(4),
             CCONSL_ORCAM_ATUAL             DECIMAL(7, 0),
             CREG_ATUAL_RECTA               DECIMAL(3, 0),
             CREG_ATUAL_SDO                 DECIMAL(3, 0),
             CATUAL_ESTAT_DESP              DECIMAL(3, 0),
             CREG_ATUAL_DESP                DECIMAL(3, 0),
             CATUAL_ESTAT_RECTA             DECIMAL(3, 0),
             DULT_EXCUC                     DATE,
             HULT_SOLIC                     TIME,
             HULT_EXCUC                     TIME,
             CFUNC_BDSCO                    DECIMAL(9, 0),
             CSIT_SOLTC_GDAD                CHAR(1)
           ) END-EXEC.
      ******************************************************************
      *  DB2PRD.SOLTC_GERAL_GDAD                                       *
      ******************************************************************
       01  BACCB011.
      *    *************************************************************
           10 DSOLTC               PIC X(10).
      *    *************************************************************
           10 CD-ITEM-ANTER        PIC X(5).
      *    *************************************************************
           10 CD-SUBITEM-ANTER     PIC X(3).
      *    *************************************************************
           10 CCCUST-ANTER         PIC X(4).
      *    *************************************************************
           10 CROTNA-EXTRC-ANTER   PIC X(4).
      *    *************************************************************
           10 CCONSL-ORCAM-ANTER   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CREG-ANTER-RECTA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CREG-ANTER-SDO       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CANTER-ESTAT-DESP    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CREG-ANTER-DESP      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CANTER-ESTAT-RECTA   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CD-ITEM-ATUAL        PIC X(5).
      *    *************************************************************
           10 CD-SUBITEM-ATUAL     PIC X(3).
      *    *************************************************************
           10 CCCUST-ATUAL         PIC X(4).
      *    *************************************************************
           10 CROTNA-EXTRC-ATUAL   PIC X(4).
      *    *************************************************************
           10 CCONSL-ORCAM-ATUAL   PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CREG-ATUAL-RECTA     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CREG-ATUAL-SDO       PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CATUAL-ESTAT-DESP    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CREG-ATUAL-DESP      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CATUAL-ESTAT-RECTA   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DULT-EXCUC           PIC X(10).
      *    *************************************************************
           10 HULT-SOLIC           PIC X(8).
      *    *************************************************************
           10 HULT-EXCUC           PIC X(8).
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SOLTC-GDAD      PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
