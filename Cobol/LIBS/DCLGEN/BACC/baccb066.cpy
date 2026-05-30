      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PPSTA_CATAO_COMPL)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB066))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB066)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PPSTA_CATAO_COMPL TABLE
           ( CCTRO_DIGTC                    DECIMAL(5, 0) NOT NULL,
             CSIST_ENTRD_PPSTA              CHAR(1) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCLI_CATAO_CREDT               CHAR(19) NOT NULL,
             DENTRD_PPSTA_CATAO             DATE NOT NULL,
             CSEQ_PPSTA_CATAO               DECIMAL(7, 0) NOT NULL,
             IPTDOR_CATAO                   CHAR(30),
             DNASC                          DATE,
             CSEXO                          CHAR(1),
             CCGC_CPF_CMAST                 DECIMAL(16, 0),
             IEMPR_TRAB                     CHAR(40),
             ELOGDR_CMAST                   CHAR(60),
             CCEP                           DECIMAL(5, 0),
             CCEP_COMPL                     DECIMAL(3, 0),
             IMUN                           CHAR(30),
             CSGL_UF                        CHAR(2),
             CFONE_CMAST                    CHAR(18),
             DDIA_DEB                       DECIMAL(2, 0),
             VTOT_LIM_COMPR                 DECIMAL(9, 2),
             CASS_JONAL                     DECIMAL(15, 0),
             CCONCE_VEIC                    CHAR(9),
             CCHASI_VEIC                    CHAR(17),
             DANO_FABRI_CARRO               DECIMAL(4, 0),
             CTTLAR_DEPDT_CATAO             DECIMAL(1, 0),
             COPERD_CICS                    CHAR(3),
             CID_CATAO_ESPCL                DECIMAL(9, 0),
             DVDA_VEIC                      DATE,
             CID_DEPTO_EMPR                 CHAR(15),
             CLOTE_DIGTC_PPSTA              DECIMAL(6, 0),
             CINCON_DIGTC_PPSTA             CHAR(1),
             CID_REST_PROPN                 CHAR(1),
             CCGC_CPF_CMAST_ST              CHAR(15)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PPSTA_CATAO_COMPL           *
      ******************************************************************
       01  BACCB066.
      *    *************************************************************
           10 CCTRO-DIGTC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIST-ENTRD-PPSTA    PIC X(1).
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCLI-CATAO-CREDT     PIC X(19).
      *    *************************************************************
           10 DENTRD-PPSTA-CATAO   PIC X(10).
      *    *************************************************************
           10 CSEQ-PPSTA-CATAO     PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 IPTDOR-CATAO         PIC X(30).
      *    *************************************************************
           10 DNASC                PIC X(10).
      *    *************************************************************
           10 CSEXO                PIC X(1).
      *    *************************************************************
           10 CCGC-CPF-CMAST       PIC S9(16)V USAGE COMP-3.
      *    *************************************************************
           10 IEMPR-TRAB           PIC X(40).
      *    *************************************************************
           10 ELOGDR-CMAST         PIC X(60).
      *    *************************************************************
           10 CCEP                 PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCEP-COMPL           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 IMUN                 PIC X(30).
      *    *************************************************************
           10 CSGL-UF              PIC X(2).
      *    *************************************************************
           10 CFONE-CMAST          PIC X(18).
      *    *************************************************************
           10 DDIA-DEB             PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VTOT-LIM-COMPR       PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CASS-JONAL           PIC S9(15)V USAGE COMP-3.
      *    *************************************************************
           10 CCONCE-VEIC          PIC X(9).
      *    *************************************************************
           10 CCHASI-VEIC          PIC X(17).
      *    *************************************************************
           10 DANO-FABRI-CARRO     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CTTLAR-DEPDT-CATAO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 COPERD-CICS          PIC X(3).
      *    *************************************************************
           10 CID-CATAO-ESPCL      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DVDA-VEIC            PIC X(10).
      *    *************************************************************
           10 CID-DEPTO-EMPR       PIC X(15).
      *    *************************************************************
           10 CLOTE-DIGTC-PPSTA    PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CINCON-DIGTC-PPSTA   PIC X(1).
      *    *************************************************************
           10 CID-REST-PROPN       PIC X(1).
      *    *************************************************************
           10 CCGC-CPF-CMAST-ST    PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 32      *
      ******************************************************************
