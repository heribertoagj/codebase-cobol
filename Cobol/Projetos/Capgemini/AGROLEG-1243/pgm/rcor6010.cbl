      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RCOR6010.
       AUTHOR.     PRIME INFORMATICA.
      *================================================================*
      *                 P R I M E   I N F O R M A T I C A              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RCOR6010                                      *
      *    PROGRAMADOR.: PRIME INFORMATICA        - PRIME              *
      *    ANALISTA....: PRIME INFORMATICA        - PRIME              *
      *    DATA........: AGO/2012                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....: MODULO RESPONSAVEL POR INCLUIR AS MENSAGENS   *
      *                  TRAFEGADAS ENTRE LEGADOS E O SISTEMA RCOR NAS *
      *                  BASES DO RCOR.                                *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#RCOR13 - AREA DE COMUNICACAO RCOR6010                     *
      *    I#RCOR03 - AREA LAYOUT BLOCO DE RETORNO                     *
      *    I#POOLB6 - AREA DE TRATAMENTO DE ERRO - CDES                *
      *----------------------------------------------------------------*
      *    TABELAS.....:                                               *
      *    RCORB000 - DB2PRD.TCTRL_MSGEM_CREDT                         *
      *    RCORB002 - DB2PRD.TWORKF_MSGEM_CREDT                        *
      *================================================================*
      *                      A L T E R A C O E S                       *
      *================================================================*
PR0001*----------------------------------------------------------------*
.     *    PROGRAMADOR.:  HELIO SHOITI MORI                 -  PRIME   *
.     *    ANALISTA....:  RICARDO DALRI BOING               -  PRIME   *
.     *    DATA........:  19/03/2013                                   *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  AJUSTE PARA INCLUSAO DO CAMPO                *
.     *                   NMSGEM_PROCS_CREDT NA TABELA RCORB002, QUE   *
.     *                   DEFINE O NUMERO DE SEQUENCIA DA MENSAGEM     *
PR0001*                   PARTICIONADA.                                *
      *================================================================*
PR0013*    RESPONSAVEL.:  ELIEZER SIQUEIRA                  -  PRIME   *
-     *    DATA........:  OUTUBRO/2016                                 *
-     *----------------------------------------------------------------*
-     *    OBJETIVO....:  SE TIPO DE INCLUSAO 2 ( FLUXO R1, E, GEN)    *
-     *                   E MENSAGEM PRINCIPAL NAO ESTIVER NA BASE     *
-     *                   DE CONTORLE (RCORB000), INCLUIR MENSAGEM     *
PR0013*                   COM SITUACAO = 4 (MENSAGEM SEM CORRESP.)     *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING RCOR6010  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA INDICES                *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC  9(05) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA VARIAVEIS AUXILIARES    *'.
      *----------------------------------------------------------------*

       01  WRK-CCTRL-MSGEM-CREDT       PIC  9(13) COMP-3   VALUE ZEROS.

PR0013 01  WRK-MSGEM-INC               PIC  X(01)          VALUE SPACES.

       01  WRK-DATA                    PIC  X(26)          VALUE SPACES.
       01  FILLER                      REDEFINES WRK-DATA.
           03  WRK-ANO                 PIC  9(04).
           03  FILLER                  PIC  X(01).
           03  WRK-MES                 PIC  9(02).
           03  FILLER                  PIC  X(01).
           03  WRK-DIA                 PIC  9(02).
           03  FILLER                  PIC  X(16).

       01  WRK-NRO-CTRL-IF.
           03  WRK-BANCO               PIC 9(04)           VALUE ZEROS.
           03  WRK-CCUSTO              PIC X(04)           VALUE ZEROS.
           03  WRK-DT-JULIANA          PIC 9(05)           VALUE ZEROS.
           03  WRK-NUM-DOCTO           PIC 9(07)           VALUE ZEROS.

       01  WRK-ABS                     PIC S9(15) COMP-3   VALUE ZEROS.
       01  WRK-EIBTIME                 PIC 9(08) COMP-3    VALUE ZEROS.
       01  WRK-EIBTASKN                PIC 9(08) COMP-3    VALUE ZEROS.

       01  WRK-SYSID                   PIC X(04)           VALUE SPACES.
       01  FILLER                      REDEFINES WRK-SYSID.
           03  WRK-SYSID-COMP          PIC 9(04) COMP.

       01  WRK-VALOR-SEQ               PIC 9(13) COMP-3    VALUE ZEROS.
       01  WRK-VALOR-NUM               PIC 9(13)           VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-VALOR-NUM.
           03  WRK-NUM-02              PIC 9(06).
           03  WRK-NUM-07              PIC 9(07).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA ERRO CICS/SISTEMA       *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-CICS.
           03  FILLER                  PIC  X(08)          VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC  9(03)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  FILLER                  PIC  X(10)          VALUE
               'EIBRESP = '.
           03  WRK-EIBRESP             PIC  9(04)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  WRK-TEXTO-ERRO          PIC  X(52)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA VARIAVEIS NULIDADE      *'.
      *----------------------------------------------------------------*

       01  WRK-RCOR-NULIDADE.
           03  WRK-COPER-UNIC-MSG-NULL PIC S9(04) COMP     VALUE +0.
           03  WRK-HGRAV-PRT-CHGD-NULL PIC S9(04) COMP     VALUE +0.
           03  WRK-HGRAV-PRT-ENTG-NULL PIC S9(04) COMP     VALUE +0.
           03  WRK-CAUTN-MSG-CRED-NULL PIC S9(04) COMP     VALUE +0.
           03  WRK-CTPO-ERRO-MSGEM-NULL
                                       PIC S9(04) COMP     VALUE +0.
           03  WRK-RERRO-MSGEM-CREDT-NULL
                                       PIC S9(04) COMP     VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA PARA ERRO CICS / DB2    *'.
      *----------------------------------------------------------------*

       COPY 'I#POOLB6'.

PRIME ** AREA FORMATACAO ERRO UPDATE
.      01  WRK-SIT-PROCM               PIC 9(01)           VALUE ZEROS.
PRIME  01  WRK-CTRL-MSGM               PIC 9(13)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA COMUNICACAO             *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR13'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA LAYOUT BLOCO RETORNO    *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR03'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        AREA DB2              *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RCORB000
           END-EXEC.

           EXEC SQL
               INCLUDE RCORB002
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA DECLARACAO CURSOR *'.
      *----------------------------------------------------------------*

      *------ CURSOR POR NUMERO DO CONTROLE IF ------------------------*

           EXEC SQL
               DECLARE  CSR01-RCORB000 CURSOR FOR
                SELECT  CCTRL_MSGEM_CREDT
PR0013                 ,CSIT_PROCM_CREDT
                  FROM  DB2PRD.TCTRL_MSGEM_CREDT
                 WHERE  CNRO_CTRL_INSTC = :RCORB000.CNRO-CTRL-INSTC
           END-EXEC.

      *------ CURSOR POR NUMERO UNICO OPERACAO DA MENSAGEM ------------*

           EXEC SQL
               DECLARE  CSR02-RCORB000 CURSOR FOR
                SELECT  CCTRL_MSGEM_CREDT
                       ,CNRO_CTRL_INSTC
PR0013                 ,CSIT_PROCM_CREDT
                  FROM  DB2PRD.TCTRL_MSGEM_CREDT
                 WHERE  COPER_UNIC_MSGEM = :RCORB000.COPER-UNIC-MSGEM
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING RCOR6010    *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA                 PIC X(30538).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS HANDLE ABEND
               LABEL (9000-FORMATAR-ERRO-CICS)
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO RCOR13-REGISTRO.

           PERFORM 1000-PROCEDIMENTOS-INICIAIS.

           IF (RCOR13-S-COD-RETORNO    EQUAL ZEROS)
               EVALUATE RCOR13-TP-INCL
                   WHEN 1 PERFORM 2000-INCL-MENSAGENS-ENVIO
                   WHEN 2 PERFORM 2200-INCLUIR-DADOS-RCORB002
                   WHEN 3 PERFORM 3000-ATUALIZA-MQID
                   WHEN 4 PERFORM 4000-TRATAR-BLOCO-RETORNO
                   WHEN 5 PERFORM 2200-INCLUIR-DADOS-RCORB002
                   WHEN 6 PERFORM 5000-TRATAR-MSG-ENTR-MANUAL
                   WHEN 7 PERFORM 6000-TRATA-IND-ENVIO-LEGADO
                   WHEN 8 PERFORM 6000-TRATA-IND-ENVIO-LEGADO
               END-EVALUATE
           END-IF.

           PERFORM 7000-RETORNAR-CHAMADOR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCEDIMENTOS-INICIAIS     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RCOR13-SAIDA.

           MOVE ZEROS                  TO WRK-CCTRL-MSGEM-CREDT.

           IF (RCOR13-TP-INCL          NOT EQUAL 1 AND 2 AND 3 AND 4
                                             AND 5 AND 6 AND 7 AND 8)
PRIME          STRING 'RCOR13-TP-INCL - INVALIDO '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 1000-99-FIM
           END-IF.

           IF (RCOR13-TP-INCL          EQUAL 1 OR 2 OR 3 OR 4) AND
              (RCOR13-CNRO-CTRL-INSTC  EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-CNRO-CTRL-INSTC - INVALIDO '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 1000-99-FIM
           END-IF.

           IF (RCOR13-TP-INCL          EQUAL 5) AND
              (RCOR13-COPER-UNIC-MSGEM EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-COPER-UNIC-MSGEM - INVALIDO '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-INCL-MENSAGENS-ENVIO       SECTION.
      *----------------------------------------------------------------*

PR0001     IF (RCOR13-SEQ-MSG          EQUAL 1)
.              PERFORM 2100-INCLUIR-DADOS-RCORB000
.          END-IF.
.
.          IF (RCOR13-S-COD-RETORNO    EQUAL ZEROS)
.              IF (RCOR13-SEQ-MSG      NOT EQUAL 1)
.                  PERFORM 2220-OPEN-CSR01-RCORB000
.                  PERFORM 2230-FETCH-CSR01-RCORB000
.                  PERFORM 2240-CLOSE-CSR01-RCORB000
.              END-IF
.              PERFORM 2250-INSERT-RCORB002
PR0001     END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-INCLUIR-DADOS-RCORB000     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-CONSISTE-DADOS-RCORB000.

           IF (RCOR13-S-COD-RETORNO    NOT EQUAL ZEROS)
               GO                      TO 2100-99-FIM
           END-IF.

           EXEC SQL
               SET :WRK-DATA = CURRENT TIMESTAMP
           END-EXEC.

           MOVE RCOR13-CNRO-CTRL-INSTC TO CNRO-CTRL-INSTC  OF RCORB000.
           MOVE RCOR13-CENTRO-CUSTO    TO CSIST            OF RCORB000.
           MOVE RCOR13-CSIT-PROC-CREDT TO CSIT-PROCM-CREDT OF RCORB000.
           MOVE RCOR13-CIDTFD-EMISR    TO CIDTFD-EMISR     OF RCORB000.
           MOVE RCOR13-CIDTFD-DSTNA    TO CIDTFD-DSTNA     OF RCORB000.
           MOVE WRK-DATA               TO HGRAV-PROT-CHEGD OF RCORB000
                                          HGRAV-PROT-ENTRG OF RCORB000.
           MOVE SPACES                 TO COPER-UNIC-MSGEM OF RCORB000
                                          CAUTEN-MSGEM-CREDT
                                                           OF RCORB000.
           MOVE -1                     TO WRK-COPER-UNIC-MSG-NULL
                                          WRK-CAUTN-MSG-CRED-NULL.
           MOVE ZEROS                  TO WRK-HGRAV-PRT-CHGD-NULL
                                          WRK-HGRAV-PRT-ENTG-NULL.

           PERFORM 2120-INSERT-RCORB000.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-CONSISTE-DADOS-RCORB000    SECTION.
      *----------------------------------------------------------------*

           IF (RCOR13-CENTRO-CUSTO     EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-CENTRO-CUSTO - INVALIDO '
.                     'VLR : ' RCOR13-CENTRO-CUSTO
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 2110-99-FIM
           END-IF.

           IF (RCOR13-CSIT-PROC-CREDT  NOT EQUAL 1 AND 2 AND 3)
PRIME          STRING 'RCOR13-CSIT-PROC-CREDT - INVALIDO '
.                     'VLR : ' RCOR13-AREA-RCORB000(05:01)
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 2110-99-FIM
           END-IF.

           IF (RCOR13-CIDTFD-EMISR     EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-CIDTFD-EMISR - INVALIDO '
.                     'VLR : ' RCOR13-CIDTFD-EMISR
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 2110-99-FIM
           END-IF.

           IF (RCOR13-CIDTFD-DSTNA     EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-CIDTFD-DSTNA - INVALIDO '
.                     'VLR : ' RCOR13-CIDTFD-DSTNA
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIEM          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 2110-99-FIM
           END-IF.

           IF (RCOR13-COD-MSGEM        EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-COD-MSGEM - INVALIDO '
.                     'VLR : ' RCOR13-COD-MSGEM
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 2110-99-FIM
           END-IF.

           IF (RCOR13-MSGEM-CREDT-TXT  EQUAL SPACES OR LOW-VALUES)
PRIME          MOVE 'RCOR13-MSGEM-CREDT-TXT - INVALIDO'
PRIME                                  TO RCOR13-S-DSC-ERRO(01:33)
               MOVE 10                 TO RCOR13-S-COD-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-INSERT-RCORB000            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SET :RCORB000.CCTRL-MSGEM-CREDT =
                    NEXT VALUE FOR DB2PRD.SEQ_RCORN000_TCTRL_MSGEM_CREDT
           END-EXEC.

           MOVE CCTRL-MSGEM-CREDT OF RCORB000
                                       TO WRK-CCTRL-MSGEM-CREDT.

           EXEC SQL
               INSERT INTO DB2PRD.TCTRL_MSGEM_CREDT
                      (CCTRL_MSGEM_CREDT,
                       CNRO_CTRL_INSTC,
                       CSIST,
                       CSIT_PROCM_CREDT,
                       CIDTFD_EMISR,
                       CIDTFD_DSTNA,
                       COPER_UNIC_MSGEM,
                       HGRAV_PROT_CHEGD,
                       HGRAV_PROT_ENTRG,
                       CAUTEN_MSGEM_CREDT)
                VALUES(:RCORB000.CCTRL-MSGEM-CREDT,
                       :RCORB000.CNRO-CTRL-INSTC,
                       :RCORB000.CSIST,
                       :RCORB000.CSIT-PROCM-CREDT,
                       :RCORB000.CIDTFD-EMISR,
                       :RCORB000.CIDTFD-DSTNA,
                       :RCORB000.COPER-UNIC-MSGEM
                          :WRK-COPER-UNIC-MSG-NULL,
                       :RCORB000.HGRAV-PROT-CHEGD
                          :WRK-HGRAV-PRT-CHGD-NULL,
                       :RCORB000.HGRAV-PROT-ENTRG
                          :WRK-HGRAV-PRT-ENTG-NULL,
                       :RCORB000.CAUTEN-MSGEM-CREDT
                          :WRK-CAUTN-MSG-CRED-NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TCTRL_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '10'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-INCLUIR-DADOS-RCORB002     SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-CONSISTE-DADOS-RCORB002.

           PERFORM 2220-OPEN-CSR01-RCORB000.
           PERFORM 2230-FETCH-CSR01-RCORB000.
           PERFORM 2240-CLOSE-CSR01-RCORB000.

PR0013     IF  WRK-MSGEM-INC           EQUAL 'S'
-              PERFORM 2270-TRATAR-DADOS-RCORB000
-              PERFORM 2120-INSERT-RCORB000
PR0013     END-IF.

           PERFORM 2250-INSERT-RCORB002.

           IF (RCOR13-COD-MSGEM        NOT EQUAL 'COR0001  '
                                             AND 'COR0002  '
                                             AND 'COR0003  '
PRI371                                       AND 'COR0005  ')
PR0013         IF  WRK-MSGEM-INC       NOT EQUAL 'S'
                   PERFORM 2260-UPDATE-RCORB000
PR0013         END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-CONSISTE-DADOS-RCORB002    SECTION.
      *----------------------------------------------------------------*

           IF (RCOR13-COD-MSGEM        EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-COD-MSGEM - INVALIDO '
.                     'VLR : ' RCOR13-COD-MSGEM
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
PR0013         PERFORM 7000-RETORNAR-CHAMADOR
           ELSE
               IF (RCOR13-MSGEM-CREDT-TXT
                                       EQUAL SPACES OR LOW-VALUES)
PRIME              MOVE 'RCOR13-MSGEM-CREDT-TXT - INVALIDO'
PRIME                                  TO RCOR13-S-DSC-ERRO(01:33)
                   MOVE 10             TO RCOR13-S-COD-RETORNO
PR0013             PERFORM 7000-RETORNAR-CHAMADOR
               END-IF
           END-IF.

PR0001     IF (RCOR13-SEQ-MSG          NOT NUMERIC) OR
PR0001        (RCOR13-SEQ-MSG          EQUAL ZEROS)
PRIME          STRING 'RCOR13-SEQ-MSG - INVALIDA '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
PR0001         MOVE 10                 TO RCOR13-S-COD-RETORNO
PR0013         PERFORM 7000-RETORNAR-CHAMADOR
PR0001     END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-OPEN-CSR01-RCORB000        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-CCTRL-MSGEM-CREDT.

           IF (RCOR13-TP-INCL          NOT EQUAL 5)
               MOVE RCOR13-CNRO-CTRL-INSTC
                                       TO CNRO-CTRL-INSTC OF RCORB000
               EXEC SQL
                   OPEN CSR01-RCORB000
               END-EXEC
           ELSE
               MOVE RCOR13-COPER-UNIC-MSGEM
                                       TO COPER-UNIC-MSGEM OF RCORB000
               EXEC SQL
                   OPEN CSR02-RCORB000
               END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TCTRL_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '20'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2230-FETCH-CSR01-RCORB000       SECTION.
      *----------------------------------------------------------------*

           IF  RCOR13-TP-INCL           NOT EQUAL 5
               EXEC SQL
                   FETCH  CSR01-RCORB000
                    INTO :RCORB000.CCTRL-MSGEM-CREDT
                        ,:RCORB000.CSIT-PROCM-CREDT
               END-EXEC
           ELSE
               EXEC SQL
                   FETCH  CSR02-RCORB000
                    INTO :RCORB000.CCTRL-MSGEM-CREDT
                        ,:RCORB000.CNRO-CTRL-INSTC
                        ,:RCORB000.CSIT-PROCM-CREDT
               END-EXEC
           END-IF.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               MOVE 'TCTRL_MSGEM_CREDT' TO ERR-DBD-TAB
               MOVE 'FETCH'             TO ERR-FUN-COMANDO
               MOVE SQLCODE             TO ERR-SQL-CODE
               MOVE '30'                TO ERR-LOCAL
               MOVE SQLCA               TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

           IF  SQLCODE                  EQUAL +100
PR0013         MOVE 'S'                 TO WRK-MSGEM-INC
           ELSE
PR0013         IF  CSIT-PROCM-CREDT     OF RCORB000
-                                       EQUAL 4
-                  MOVE 'S'             TO WRK-MSGEM-INC
-              ELSE
-                  MOVE 'N'             TO WRK-MSGEM-INC
PR0013         END-IF
               MOVE CCTRL-MSGEM-CREDT   OF RCORB000
                                        TO WRK-CCTRL-MSGEM-CREDT
               IF  RCOR13-TP-INCL       EQUAL 5
                   MOVE CNRO-CTRL-INSTC OF RCORB000
                                        TO RCOR13-CNRO-CTRL-INSTC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2240-CLOSE-CSR01-RCORB000       SECTION.
      *----------------------------------------------------------------*

           IF (RCOR13-TP-INCL          NOT EQUAL 5)
               EXEC SQL
                   CLOSE CSR01-RCORB000
               END-EXEC
           ELSE
               EXEC SQL
                   CLOSE CSR02-RCORB000
               END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TCTRL_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '40'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-INSERT-RCORB002            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SET :WRK-DATA = CURRENT TIMESTAMP
           END-EXEC.

           MOVE RCOR13-COD-MSGEM     TO CMSGEM-PROCS-CREDT OF RCORB002
                                        RCOR13-S-CMSGEM-CREDT.

           MOVE WRK-CCTRL-MSGEM-CREDT
                                     TO CCTRL-MSGEM-CREDT  OF RCORB002
                                        RCOR13-S-CCTRL-MSGEM.

           MOVE WRK-DATA             TO HINCL-REG          OF RCORB002
                                        RCOR13-S-HINCL-REG.

           MOVE RCOR13-SEQ-MSG       TO NMSGEM-PROCS-CREDT OF RCORB002.
           MOVE WRK-MES              TO CPTCAO-TBELA       OF RCORB002.
           MOVE 'S'                  TO CIDTFD-ENVIO-MSGEM OF RCORB002.

           PERFORM VARYING IND-1     FROM 30000 BY -1
                   UNTIL   IND-1     EQUAL ZEROS   OR
                RCOR13-MSGEM-CREDT-TXT(IND-1:1)
                                     NOT EQUAL SPACES AND LOW-VALUES
           END-PERFORM.

           MOVE IND-1                TO WMSGEM-PROCS-CREDT-LEN
                                                           OF RCORB002.
           MOVE RCOR13-MSGEM-CREDT-TXT(1:IND-1)
                                     TO WMSGEM-PROCS-CREDT-TEXT
                                                           OF RCORB002.

PR0013     IF  WRK-MSGEM-INC         EQUAL 'S'
-              MOVE 1                TO CTPO-ERRO-MSGEM    OF RCORB002
-              MOVE 'MENSAGEM SEM CORRESPONDENTE'
-                                    TO RERRO-MSGEM-CREDT  OF RCORB002
-          ELSE
-              MOVE -1               TO WRK-CTPO-ERRO-MSGEM-NULL
-                                       WRK-RERRO-MSGEM-CREDT-NULL
PR0013     END-IF.

           EXEC SQL
               INSERT INTO DB2PRD.TWORKF_MSGEM_CREDT
                      (CMSGEM_PROCS_CREDT,
                       CCTRL_MSGEM_CREDT,
                       HINCL_REG,
PR0001                 NMSGEM_PROCS_CREDT,
                       CPTCAO_TBELA,
                       CIDTFD_ENVIO_MSGEM,
                       WMSGEM_PROCS_CREDT,
                       CTPO_ERRO_MSGEM,
                       RERRO_MSGEM_CREDT)
                VALUES(:RCORB002.CMSGEM-PROCS-CREDT,
                       :RCORB002.CCTRL-MSGEM-CREDT,
                       :RCORB002.HINCL-REG,
PR0001                 :RCORB002.NMSGEM-PROCS-CREDT,
                       :RCORB002.CPTCAO-TBELA,
                       :RCORB002.CIDTFD-ENVIO-MSGEM,
                       :RCORB002.WMSGEM-PROCS-CREDT,
PR0013                 :RCORB002.CTPO-ERRO-MSGEM
-                           :WRK-CTPO-ERRO-MSGEM-NULL,
-                      :RCORB002.RERRO-MSGEM-CREDT
PR0013                      :WRK-RERRO-MSGEM-CREDT-NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TWORKF_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '50'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2260-UPDATE-RCORB000            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CCTRL-MSGEM-CREDT  TO CCTRL-MSGEM-CREDT OF RCORB000.

           IF (RCOR13-COD-MSGEM        EQUAL 'GEN0004  ' OR 'COR0001E '
                                          OR 'COR0002E ' OR 'COR0003E '
PRI371                                    OR 'COR0005E ')
               MOVE 3                  TO CSIT-PROCM-CREDT OF RCORB000
           ELSE
               MOVE 2                  TO CSIT-PROCM-CREDT OF RCORB000
           END-IF.

           EXEC SQL
               UPDATE DB2PRD.TCTRL_MSGEM_CREDT
                  SET CSIT_PROCM_CREDT  = :RCORB000.CSIT-PROCM-CREDT
                WHERE CCTRL_MSGEM_CREDT = :RCORB000.CCTRL-MSGEM-CREDT
           END-EXEC.

PRIME      IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TCTRL_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '80'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

PRIME      IF (SQLCODE                 EQUAL +100)
.              MOVE CCTRL-MSGEM-CREDT OF RCORB000
.                                      TO WRK-CTRL-MSGM
.              MOVE CSIT-PROCM-CREDT  OF RCORB000
.                                      TO WRK-SIT-PROCM
.              MOVE 'VRS001'           TO ERR-VERSAO
.              MOVE ZEROS              TO ERR-COD-MSG
.              MOVE ZEROS              TO ERR-RETURN-CODE
.              MOVE 'APL'              TO ERR-TIPO-ACESSO
.              MOVE 'RCOR6010'         TO ERR-PGM
.              MOVE DFHEIBLK           TO ERR-DFHEIBLK
.              EXEC CICS ASSIGN
.                        APPLID(ERR-CICS)
.              END-EXEC
.              STRING 'RCOR6010-ERRO UPDATE +100 - '
.                     'SITUACAO: ' WRK-SIT-PROCM
.                     ' CONTROLE: ' WRK-CTRL-MSGM
.                     '                '
.                   DELIMITED BY SIZE  INTO ERR-TEXTO-MSG
.              END-STRING
.              PERFORM 9100-GRAVAR-LOG-CDES
PRIME      END-IF.

      *----------------------------------------------------------------*
       2260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PR0013*----------------------------------------------------------------*
-      2270-TRATAR-DADOS-RCORB000      SECTION.
-     *----------------------------------------------------------------*
-
-          EXEC SQL
-              SET :WRK-DATA = CURRENT TIMESTAMP
-          END-EXEC.
-
-          MOVE RCOR13-CNRO-CTRL-INSTC TO WRK-NRO-CTRL-IF.
-          MOVE WRK-CCUSTO             TO RCOR13-CENTRO-CUSTO.
-
-          MOVE RCOR13-CNRO-CTRL-INSTC TO CNRO-CTRL-INSTC  OF RCORB000.
-          MOVE RCOR13-CENTRO-CUSTO    TO CSIST            OF RCORB000.
-          MOVE 4                      TO CSIT-PROCM-CREDT OF RCORB000.
-
-          MOVE ZEROS                  TO CIDTFD-EMISR     OF RCORB000
-                                         CIDTFD-DSTNA     OF RCORB000.
-
-          MOVE WRK-DATA               TO HGRAV-PROT-CHEGD OF RCORB000
-                                         HGRAV-PROT-ENTRG OF RCORB000.
-
-          IF  RCOR13-TP-INCL          EQUAL 4
-              MOVE ZEROS              TO WRK-COPER-UNIC-MSG-NULL
-              MOVE RCOR13-COPER-UNIC-MSGEM
-                                      TO COPER-UNIC-MSGEM  OF RCORB000
-          ELSE
-              MOVE SPACES             TO COPER-UNIC-MSGEM OF RCORB000
-              MOVE -1                 TO WRK-COPER-UNIC-MSG-NULL
-          END-IF.
-
-          MOVE SPACES                 TO CAUTEN-MSGEM-CREDT
-                                                          OF RCORB000.
-
-          MOVE -1                     TO WRK-CAUTN-MSG-CRED-NULL.
-          MOVE ZEROS                  TO WRK-HGRAV-PRT-CHGD-NULL
-                                         WRK-HGRAV-PRT-ENTG-NULL.
-
-     *----------------------------------------------------------------*
-      2270-99-FIM.                    EXIT.
PR0013*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-ATUALIZA-MQID              SECTION.
      *----------------------------------------------------------------*

           IF (RCOR13-CAUT-MSG-CREDT   EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-CAUT-MSG-CREDT-INVALIDO '
.                     'VLR: ' RCOR13-CAUT-MSG-CREDT
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 3000-99-FIM
           END-IF.

           PERFORM 2220-OPEN-CSR01-RCORB000.

           PERFORM 2230-FETCH-CSR01-RCORB000.

           PERFORM 2240-CLOSE-CSR01-RCORB000.

           MOVE ZEROS                  TO WRK-CAUTN-MSG-CRED-NULL.
           MOVE RCOR13-CAUT-MSG-CREDT  TO CAUTEN-MSGEM-CREDT
                                                            OF RCORB000.

           EXEC SQL
               UPDATE DB2PRD.TCTRL_MSGEM_CREDT
                  SET CAUTEN_MSGEM_CREDT = :RCORB000.CAUTEN-MSGEM-CREDT
                WHERE CCTRL_MSGEM_CREDT  = :RCORB000.CCTRL-MSGEM-CREDT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TCTRL_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '60'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-TRATAR-BLOCO-RETORNO       SECTION.
      *----------------------------------------------------------------*

           IF (RCOR13-COPER-UNIC-MSGEM EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-COPER-UNIC-MSGEM - INVALIDO '
.                     'VLR: ' RCOR13-COPER-UNIC-MSGEM
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 4000-99-FIM
           END-IF.

           PERFORM 2220-OPEN-CSR01-RCORB000.
           PERFORM 2230-FETCH-CSR01-RCORB000.
           PERFORM 2240-CLOSE-CSR01-RCORB000.

PR0013     IF  WRK-MSGEM-INC           EQUAL 'S'
-              PERFORM 2270-TRATAR-DADOS-RCORB000
-              PERFORM 2120-INSERT-RCORB000
-              PERFORM 2250-INSERT-RCORB002
-              GO TO 4000-99-FIM
PR0013     END-IF.

           MOVE ZEROS                  TO WRK-COPER-UNIC-MSG-NULL.
           MOVE RCOR13-COPER-UNIC-MSGEM
                                       TO COPER-UNIC-MSGEM  OF RCORB000.
           MOVE RCOR13-MSGEM-CREDT-TXT(01:217)
                                       TO RCOR03-REGISTRO.

           IF (RCOR03-CRETOR-PRINC     EQUAL ZEROS)
               EXEC SQL
                  UPDATE DB2PRD.TCTRL_MSGEM_CREDT
                     SET COPER_UNIC_MSGEM  = :RCORB000.COPER-UNIC-MSGEM
                   WHERE CCTRL_MSGEM_CREDT = :RCORB000.CCTRL-MSGEM-CREDT
               END-EXEC
           ELSE
               MOVE 3                  TO CSIT-PROCM-CREDT OF RCORB000
               EXEC SQL
                  UPDATE DB2PRD.TCTRL_MSGEM_CREDT
                     SET COPER_UNIC_MSGEM  = :RCORB000.COPER-UNIC-MSGEM,
                         CSIT_PROCM_CREDT  = :RCORB000.CSIT-PROCM-CREDT
                   WHERE CCTRL_MSGEM_CREDT = :RCORB000.CCTRL-MSGEM-CREDT
               END-EXEC
           END-IF.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TCTRL_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '70'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

           PERFORM 2250-INSERT-RCORB002.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-TRATAR-MSG-ENTR-MANUAL     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RCORB000

           IF (RCOR13-COD-MSGEM        EQUAL 'GEN0004  ')
               PERFORM 5100-GERAR-NRO-CTRL-IF
               MOVE WRK-NRO-CTRL-IF    TO CNRO-CTRL-INSTC  OF RCORB000
               MOVE 3                  TO CSIT-PROCM-CREDT OF RCORB000
           ELSE
               IF (RCOR13-COD-MSGEM    EQUAL 'COR0001R1'
                                          OR 'COR0002R1'
                                          OR 'COR0003R1'
PRI371                                    OR 'COR0005R1')
                   MOVE RCOR13-MSGEM-CREDT-TXT(10:20)
                                       TO CNRO-CTRL-INSTC  OF RCORB000
                   MOVE 2              TO CSIT-PROCM-CREDT OF RCORB000
               ELSE
                   MOVE RCOR13-MSGEM-CREDT-TXT(18:20)
                                       TO CNRO-CTRL-INSTC  OF RCORB000
                   MOVE 3              TO CSIT-PROCM-CREDT OF RCORB000
               END-IF
           END-IF.

           MOVE 'TMNF'                 TO CSIST            OF RCORB000.
           MOVE '60746948'             TO CIDTFD-EMISR     OF RCORB000.
           MOVE '00038166'             TO CIDTFD-DSTNA     OF RCORB000.
           MOVE SPACES                 TO COPER-UNIC-MSGEM OF RCORB000.

           EXEC SQL
               SET :WRK-DATA = CURRENT TIMESTAMP
           END-EXEC.

           MOVE WRK-DATA               TO HGRAV-PROT-CHEGD OF RCORB000
                                          HGRAV-PROT-ENTRG OF RCORB000.
           MOVE SPACES                 TO CAUTEN-MSGEM-CREDT
                                                           OF RCORB000.
           MOVE ZEROS                  TO WRK-HGRAV-PRT-CHGD-NULL
                                          WRK-HGRAV-PRT-ENTG-NULL.
           MOVE -1                     TO WRK-CAUTN-MSG-CRED-NULL
                                          WRK-COPER-UNIC-MSG-NULL.

           PERFORM 2120-INSERT-RCORB000.
           PERFORM 2250-INSERT-RCORB002.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-GERAR-NRO-CTRL-IF          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-NRO-CTRL-IF.

           MOVE 237                    TO WRK-BANCO.
           MOVE 'TMNF'                 TO WRK-CCUSTO.

           EXEC CICS ASKTIME
               ABSTIME(WRK-ABS)
           END-EXEC.

           EXEC CICS FORMATTIME
               ABSTIME(WRK-ABS)
               YYDDD  (WRK-DT-JULIANA)
           END-EXEC.

           MOVE EIBTIME                TO WRK-EIBTIME.
           MOVE EIBTASKN               TO WRK-EIBTASKN.

           EXEC CICS ASSIGN
               SYSID(WRK-SYSID)
               NOHANDLE
           END-EXEC.

           COMPUTE WRK-VALOR-SEQ       = WRK-EIBTIME    *
                                         10             +
                                         WRK-EIBTASKN   +
                                         WRK-SYSID-COMP *
                                         997.

           MOVE WRK-VALOR-SEQ          TO WRK-VALOR-NUM.
           MOVE WRK-NUM-07             TO WRK-NUM-DOCTO.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6000-TRATA-IND-ENVIO-LEGADO     SECTION.
      *----------------------------------------------------------------*

           IF (RCOR13-E-CMSGEM-CREDT   EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-E-CMSGEM-CREDT - INVALIDO '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 6000-99-FIM
           END-IF.

           IF (RCOR13-E-CCTRL-MSGEM    NOT NUMERIC) OR
              (RCOR13-E-CCTRL-MSGEM    EQUAL ZEROS)
PRIME          STRING 'RCOR13-E-CCTRL-MSGEM - INVALIDO '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 6000-99-FIM
           END-IF.

           IF (RCOR13-E-HINCL-REG      EQUAL SPACES OR LOW-VALUES)
PRIME          STRING 'RCOR13-E-HINCL-REG - INVALIDO '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
               MOVE 10                 TO RCOR13-S-COD-RETORNO
               GO                      TO 6000-99-FIM
           END-IF.

PR0001     IF (RCOR13-SEQ-MSG          NOT NUMERIC) OR
.             (RCOR13-SEQ-MSG          EQUAL ZEROS)
PRIME          STRING 'RCOR13-SEQ-MSG - INVALIDO '
.                     'CTRL: ' RCOR13-CONTROLE
.                   DELIMITED BY SIZE  INTO RCOR13-S-DSC-ERRO
PRIME          END-STRING
.              MOVE 10                 TO RCOR13-S-COD-RETORNO
.              GO                      TO 6000-99-FIM
PR0001     END-IF.

PR0013     IF  RCOR13-TP-INCL          EQUAL 7
-              IF  RCOR13-CTPO-ERRO-MSGEM
-                                      NOT NUMERIC OR
-                  RCOR13-CTPO-ERRO-MSGEM
-                                      EQUAL ZEROS
-                STRING 'RCOR13-CTPO-ERRO-MSGEM - INVALIDO '
-                       'CTRL: ' RCOR13-CONTROLE
-                     DELIMITED BY SIZE INTO RCOR13-S-DSC-ERRO
-                END-STRING
-                MOVE 10               TO RCOR13-S-COD-RETORNO
-                GO                    TO 6000-99-FIM
-              END-IF
-              IF  RCOR13-RERRO-MSGEM-CREDT
-                                      EQUAL SPACES OR LOW-VALUES
-                STRING 'RCOR13-RERRO-MSGEM-CREDT - INVALIDO '
-                       'CTRL: ' RCOR13-CONTROLE
-                     DELIMITED BY SIZE INTO RCOR13-S-DSC-ERRO
-                END-STRING
-                MOVE 10               TO RCOR13-S-COD-RETORNO
-                GO                    TO 6000-99-FIM
-              END-IF
PR0013     END-IF.

           MOVE RCOR13-E-CMSGEM-CREDT  TO CMSGEM-PROCS-CREDT
                                                            OF RCORB002.
           MOVE RCOR13-E-CCTRL-MSGEM   TO CCTRL-MSGEM-CREDT OF RCORB002.
           MOVE RCOR13-E-HINCL-REG     TO HINCL-REG         OF RCORB002.
PR0001     MOVE RCOR13-SEQ-MSG         TO NMSGEM-PROCS-CREDT
PR0001                                                      OF RCORB002.

           IF (RCOR13-TP-INCL          EQUAL 7)
               MOVE 'N'                TO CIDTFD-ENVIO-MSGEM
                                                            OF RCORB002
PR0013         MOVE RCOR13-CTPO-ERRO-MSGEM
-                                      TO CTPO-ERRO-MSGEM   OF RCORB002
-              MOVE RCOR13-RERRO-MSGEM-CREDT
-                                      TO RERRO-MSGEM-CREDT OF RCORB002
PR0013         PERFORM 6100-UPDATE-RCORB002-ERRO
           ELSE
               MOVE 'S'                TO CIDTFD-ENVIO-MSGEM
                                                            OF RCORB002
PR0013         PERFORM 6200-UPDATE-RCORB002
           END-IF.

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PR0013*----------------------------------------------------------------*
-      6100-UPDATE-RCORB002-ERRO       SECTION.
-     *----------------------------------------------------------------*
-
-          EXEC SQL
-            UPDATE DB2PRD.TWORKF_MSGEM_CREDT
-               SET CIDTFD_ENVIO_MSGEM =  :RCORB002.CIDTFD-ENVIO-MSGEM
-                  ,CTPO_ERRO_MSGEM    =  :RCORB002.CTPO-ERRO-MSGEM
-                  ,RERRO_MSGEM_CREDT  =  :RCORB002.RERRO-MSGEM-CREDT
-             WHERE CMSGEM_PROCS_CREDT =  :RCORB002.CMSGEM-PROCS-CREDT
-               AND CCTRL_MSGEM_CREDT  =  :RCORB002.CCTRL-MSGEM-CREDT
-               AND HINCL_REG          =  :RCORB002.HINCL-REG
-               AND NMSGEM_PROCS_CREDT =  :RCORB002.NMSGEM-PROCS-CREDT
-          END-EXEC.
-
-          IF (SQLCODE                   NOT EQUAL ZEROS) OR
-             (SQLWARN0                  EQUAL 'W')
-              MOVE 'TWORKF_MSGEM_CREDT' TO ERR-DBD-TAB
-              MOVE 'UPDATE'             TO ERR-FUN-COMANDO
-              MOVE SQLCODE              TO ERR-SQL-CODE
-              MOVE '100'                TO ERR-LOCAL
-              MOVE SQLCA                TO ERR-SQLCA
-              PERFORM 9200-FORMATAR-ERRO-DB2
-              PERFORM 7000-RETORNAR-CHAMADOR
-          END-IF.
-
-     *----------------------------------------------------------------*
-      6100-99-FIM.                    EXIT.
PR0013*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6200-UPDATE-RCORB002            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             UPDATE DB2PRD.TWORKF_MSGEM_CREDT
                SET CIDTFD_ENVIO_MSGEM = :RCORB002.CIDTFD-ENVIO-MSGEM
              WHERE CMSGEM_PROCS_CREDT = :RCORB002.CMSGEM-PROCS-CREDT
                AND CCTRL_MSGEM_CREDT  = :RCORB002.CCTRL-MSGEM-CREDT
                AND HINCL_REG          = :RCORB002.HINCL-REG
PR0001          AND NMSGEM_PROCS_CREDT = :RCORB002.NMSGEM-PROCS-CREDT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TWORKF_MSGEM_CREDT'
                                       TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '90'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 7000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE RCOR13-REGISTRO        TO DFHCOMMAREA(1:EIBCALEN).

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FORMATAR-ERRO-CICS         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE SPACES                 TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR6010'             TO ERR-PGM.
           MOVE WRK-ERRO-CICS          TO ERR-TEXTO.
           MOVE 'RCOR6010'             TO ERR-MODULO.
           MOVE DFHEIBLK               TO ERR-DFHEIBLK.
           MOVE 99                     TO RCOR13-S-COD-RETORNO.

           EXEC CICS ASSIGN
                     APPLID(ERR-CICS)
           END-EXEC.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-GRAVAR-LOG-CDES            SECTION.
      *----------------------------------------------------------------*

           EXEC CICS START
               TRANSID('CD11')
               FROM   (ERRO-AREA)
               LENGTH (+520)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9200-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE 'ERRO DB2'             TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'DB2'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR6010'             TO ERR-PGM
                                          ERR-MODULO.
           MOVE SPACES                 TO ERR-SEGM
                                          ERR-COD-USER
                                          ERR-COD-DEPTO
                                          ERR-DFHEIBLK
                                          ERR-TERMINAL
                                          ERR-CICS.
           MOVE 99                     TO RCOR13-S-COD-RETORNO.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
