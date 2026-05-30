*================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0555.
       AUTHOR.     HELIO SANTONI.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0555                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   VALERIA TORQUATO        - PROCWORK - GP.50  *
      *    DATA........:   29/05/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DO CANCELAMENTO - EMISSAO DE       *
      *      EXTRATO ANUAL DE TARIFAS.                                 *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *            DB2                                                 *
      *             TABLE                             INCLUDE/BOOK     *
      *              DB2PRD.TAUTRZ_ENVIO_AGPTO          GFCTB0L4       *
      *              DB2PRD.TAUTRZ_ENVIO_CLI            GFCTB0L5       *
      *              DB2PRD.TAUTRZ_ENVIO_POSTO          GFCTB0L6       *
      *              DB2PRD.TAUTRZ_ENVIO_SGMTO          GFCTB0L7       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWAG2 - AREA DE COMUNICACAO - ENTRADA.                   *
      *    GFCTWAG3 - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA - OBTER DESCRICAO DE MENSAGEM.             *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA - VERIFICAR DISPONIBILIDADE DO ON-LINE.    *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5522 - MODULO PARA VERIFICAR DISPONIBILIDADE DO ON-LINE.*
      *    GFCT5523 - MODULO PARA OBTER DESCRICAO DE MENSAGENS.        *
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
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.

       01  WRK-CONTROLE                PIC  X(002)         VALUE SPACES.
       01  WRK-CONTROLE-R              REDEFINES WRK-CONTROLE
                                       PIC  9(002).

       01  WRK-TIMESTAMP               PIC  X(026)         VALUE
           '0001-01-01-01.01.01.000001'.

       01  WRK-FUNC-BDSCO-X            PIC  X(007)         VALUE SPACES.
       01  FILLER                      REDEFINES WRK-FUNC-BDSCO-X.
           05  WRK-FUNC-BDSCO-N        PIC  9(007).

       01  WRK-MSG001.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA AO MODULO '.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(041)         VALUE SPACES.

       01  WRK-AUX-9-S                 PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 WRK-AUX-9                PIC  9(009).
           05 FILLER                   REDEFINES WRK-AUX-9.
             10 FILLER                 PIC  X(006).
             10 WRK-AUX-9-3-A.
               15 WRK-AUX-9-3          PIC  9(003).

       01  WRK-MSG002                  PIC  X(075)         VALUE
           'SISTEMA INDISPONIVEL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA POOL0110 ***'.
      *----------------------------------------------------------------*

       ST25X6* 01  WRK-CGC-CPF-0110.
       ST25X6*     05  WRK-NUMERO-0110         PIC  9(009)         VALUE ZEROS.
       ST25X6*     05  WRK-FILIAL-0110         PIC  9(005)         VALUE ZEROS.
       ST25X6  01  WRK-CGC-CPF-0110.
       ST25X6      05  WRK-NUMERO-0110         PIC  X(009)         VALUE SPACES.
       ST25X6      05  WRK-FILIAL-0110         PIC  X(004)         VALUE SPACES.
       01  WRK-CONTROLE-0110           PIC  X(002)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5110 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTIU.
       COPY I#GFCTIV.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFZ.
       COPY I#GFCTG1.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY I#GFCTG2.
       COPY I#GFCTG3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L4
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0L7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY GFCTWAG2.

       COPY GFCTWAG3.

       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE                       DIVISION USING WAG2-ENTRADA
                                                      WAG3-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          WAG3-SAIDA.

           INITIALIZE WAG3-SAIDA
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-DISP-SISTEMA.

           PERFORM 1300-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MONTA AREA DE SAIDA                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                   TO WAG3-LL
           MOVE ZEROS                  TO WAG3-ZZ
           MOVE WAG2-TRANSACAO         TO WAG3-TRANSACAO
           MOVE WAG2-FUNCAO            TO WAG3-FUNCAO
           MOVE WAG2-FUNC-BDSCO        TO WAG3-FUNC-BDSCO
           MOVE 'S'                    TO WAG3-FIM
           MOVE ZEROS                  TO WAG3-ERRO
                                          WAG3-COD-SQL-ERRO
                                          WAG3-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO WAG3-DESC-MSG-ERRO(1:70).

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DESCRICAO DA MENSAGEM                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE 100                    TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAG2-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAG2-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAG2-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAG3-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0555'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO WAG3-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO WRK-AUX-9-S
                   MOVE WRK-AUX-9-3    TO WAG3-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0555'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO WAG3-ERRO
               MOVE 'S'                TO WAG3-FIM
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAG3-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAG3-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAG3-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA DISPONIBILIDADE DO SISTEMA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE 101                    TO GFCTFZ-LL.
           MOVE ZEROS                  TO GFCTFZ-ZZ.
           MOVE WAG2-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAG2-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAG2-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAG3-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0555'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE GFCTG1-ERRO        TO WAG3-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAG3-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0555'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAG3-ERRO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0555'         TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FAZ A CONSISTENCIA DOS DADOS                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAG2-TRANSACAO       EQUAL SPACES OR LOW-VALUES) OR
              (WAG2-FUNCAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAG2-FUNC-BDSCO      EQUAL SPACES OR LOW-VALUES) OR
              (WAG2-FIM             NOT   EQUAL 'S' AND 'N')    OR
              (WAG2-CAGPTO          NOT   NUMERIC)              OR
              (WAG2-CAGPTO          EQUAL ZEROS)                OR
              (WAG2-JUSTIF          EQUAL SPACES)
               MOVE 1                  TO WAG3-ERRO
                                          WAG3-COD-MSG-ERRO
               MOVE ZEROS              TO WAG3-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAG3-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAG3-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SET  :WRK-TIMESTAMP    = CURRENT TIMESTAMP
           END-EXEC.

           IF  WAG2-CAGPTO       EQUAL 4
               PERFORM 2100-CONSISTIR-CLIENTE
           ELSE
            IF  WAG2-CAGPTO      EQUAL 9
                PERFORM 2200-CONSISTIR-SEGMTO
            ELSE
             IF  WAG2-CAGPTO     EQUAL 17 OR 20
                 PERFORM 2300-CONSISTIR-POSTO
             END-IF
            END-IF
           END-IF.

           PERFORM 2400-INCLUIR-AUTORIZACAO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSA A TABELA GFCTB0L5 PARA CONSISTIR DUPL. CLIENTE       *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSISTIR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE WAG2-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF          OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC         OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC      OF GFCTB0L5
ST25X6*         MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
ST25X6*         MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
ST25X6*         MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5
           MOVE WAG2-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
           MOVE WAG2-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
           MOVE WAG2-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L
