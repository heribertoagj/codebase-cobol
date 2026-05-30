      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01AGPTO_CATAO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV036))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV036)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01AGPTO_CATAO TABLE
           ( CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT_INSTC             CHAR(19) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0),
             CCTA_CORR                      DECIMAL(7, 0),
             CFORMA_PGTO_CATAO              CHAR(1) NOT NULL,
             ELOGDR_CMAST                   CHAR(60),
             EBAIRO_CMAST                   CHAR(30),
             CCEP                           DECIMAL(5, 0),
             CCEP_COMPL                     DECIMAL(3, 0),
             IMUN                           CHAR(30),
             CSGL_UF                        CHAR(2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01AGPTO_CATAO              *
      ******************************************************************
       01  BACCV036.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT-INSTC   PIC X(19).
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR            PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CFORMA-PGTO-CATAO    PIC X(1).
      *    *************************************************************
           10 ELOGDR-CMAST         PIC X(60).
      *    *************************************************************
           10 EBAIRO-CMAST         PIC X(30).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
