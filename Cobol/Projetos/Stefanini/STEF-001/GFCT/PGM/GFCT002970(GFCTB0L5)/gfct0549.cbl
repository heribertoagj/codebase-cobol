      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0549.
       AUTHOR.     HELIO SANTONI.
      *================================================================*
      *      S O N D A   P R O C W O R K   O U T S O U R C I N G       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0549                                    *
      *    PROGRAMADOR.:   HELIO SANTONI           - PROCWORK - GP.50  *
      *    ANALISTA....:   VALERIA TORQUATO        - PROCWORK - GP.50  *
      *    DATA........:   29/05/2009                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INCLUSAO DA AUTORIZACAO DE EMISSAO DE       *
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
      *    GFCTWAGO - AREA DE COMUNICACAO - ENTRADA.                   *
      *    GFCTWAGP - AREA DE COMUNICACAO - SAIDA.                     *
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

       01  WRK-CONTROLE                PIC  X(002)         VALUE SPACES.
       01  WRK-CONTROLE-R              REDEFINES WRK-CONTROLE
                                       PIC  9(002).

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.

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

ST2506*01  WRK-CGC-CPF-0110.
ST2506*    05  WRK-NUMERO-0110         PIC  9(009)         VALUE ZEROS.
ST2506*    05  WRK-FILIAL-0110         PIC  9(005)         VALUE ZEROS.
ST2506*
ST2506*01  WRK-CONTROLE-0110           PIC  X(002)         VALUE SPACES.

ST2506 01 WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506       'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.        
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.

       01  WRK-CONTROLE-0110           PIC  X(002)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5110 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIU'.
       COPY 'I#GFCTIV'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.
       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.
       COPY 'I#GFCTG3'.

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

       COPY GFCTWAGO.
       COPY GFCTWAGP.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING WAGO-ENTRADA
                                                      WAGP-SAIDA
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
                                          WAGP-SAIDA.

           INITIALIZE WAGP-SAIDA
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

           MOVE 1010                   TO WAGP-LL
           MOVE ZEROS                  TO WAGP-ZZ
           MOVE WAGO-TRANSACAO         TO WAGP-TRANSACAO
           MOVE WAGO-FUNCAO            TO WAGP-FUNCAO
           MOVE WAGO-FUNC-BDSCO        TO WAGP-FUNC-BDSCO
           MOVE 'S'                    TO WAGP-FIM
           MOVE ZEROS                  TO WAGP-ERRO
                                          WAGP-COD-SQL-ERRO
                                          WAGP-COD-MSG-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO WAGP-DESC-MSG-ERRO(1:70).

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
           MOVE WAGO-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAGO-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAGO-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAGP-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0549'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE GFCTG3-ERRO        TO WAGP-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG3-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               ELSE
                   MOVE GFCTG3-COD-SQL-ERRO
                                       TO WRK-AUX-9-S
                   MOVE WRK-AUX-9-3    TO WAGP-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0549'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO WAGP-ERRO
               MOVE 'S'                TO WAGP-FIM
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAGP-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAGP-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAGP-DESC-MSG-ERRO
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
           MOVE WAGO-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAGO-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAGO-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAGP-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0549'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               MOVE GFCTG1-ERRO        TO WAGP-ERRO
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAGP-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT0549'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 9                  TO WAGP-ERRO
               MOVE WRK-MSG002         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0549'         TO GFCT0M-TRANSACAO
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

           IF (WAGO-TRANSACAO       EQUAL SPACES OR LOW-VALUES) OR
              (WAGO-FUNCAO          EQUAL SPACES OR LOW-VALUES) OR
              (WAGO-FUNC-BDSCO      EQUAL SPACES OR LOW-VALUES) OR
              (WAGO-FIM             NOT   EQUAL 'S' AND 'N')    OR
              (WAGO-CAGPTO          NOT   NUMERIC)              OR
              (WAGO-CAGPTO          EQUAL ZEROS)                OR
              (WAGO-JUSTIF          EQUAL SPACES)
               MOVE 1                  TO WAGP-ERRO
                                          WAGP-COD-MSG-ERRO
               MOVE ZEROS              TO WAGP-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAGP-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAGP-DESC-MSG-ERRO(1:70)
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

           IF  WAGO-CAGPTO       EQUAL 4
               PERFORM 2100-CONSISTIR-CLIENTE
           ELSE
            IF  WAGO-CAGPTO      EQUAL 9
                PERFORM 2200-CONSISTIR-SEGMTO
            ELSE
             IF  WAGO-CAGPTO     EQUAL 17 OR 20
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

           MOVE WAGO-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L5
           MOVE WAGO-NUMERO            TO CCNPJ-CPF          OF GFCTB0L5
           MOVE WAGO-FILIAL            TO CFLIAL-CGC         OF GFCTB0L5
           MOVE WAGO-CONTRL            TO CCTRL-CPF-CGC      OF GFCTB0L5
ST25X6     MOVE WAGO-NUMERO            TO CCNPJ-CPF-ST       OF GFCTB0L5
ST25X6     MOVE WAGO-FILIAL            TO CFLIAL-CGC-ST      OF GFCTB0L5
ST25X6     MOVE WAGO-CONTRL            TO CCTRL-CPF-CGC-ST   OF GFCTB0L5

           EXEC SQL
             SELECT DISTINCT B.CAGPTO_CTA
             INTO   :GFCTB0L5.CAGPTO-CTA
             FROM   DB2PRD.TAUTRZ_ENVIO_AGPTO A,
                    DB2PRD.TAUTRZ_ENVIO_CLI   B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.HINCL_REG_SIST     = B.HINCL_REG_SIST
               AND  A.CFUNC_EXCL_SIST    = 0
               AND  A.CAGPTO_CTA         = :GFCTB0L5.CAGPTO-CTA
ST25X6*        AND  B.CCNPJ_CPF          = :GFCTB0L5.CCNPJ-CPF
ST25X6*        AND  B.CFLIAL_CGC         = :GFCTB0L5.CFLIAL-CGC
ST25X6*        AND  B.CCTRL_CPF_CGC      = :GFCTB0L5.CCTRL-CPF-CGC
ST25X6         AND  B.CCNPJ_CPF_ST       = :GFCTB0L5.CCNPJ-CPF-ST
ST25X6         AND  B.CFLIAL_CGC_ST      = :GFCTB0L5.CFLIAL-CGC-ST
ST25X6         AND  B.CCTRL_CPF_CGC_ST   = :GFCTB0L5.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT  EQUAL  ZEROS  AND  +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 'TAUTRZ_ENVIO_CLI' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0L5'         TO WRK-NOM-TAB
               MOVE 'SELECT    '       TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO WAGP-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 1                  TO WAGP-ERRO
               MOVE ZEROS              TO WAGP-COD-SQL-ERRO
               MOVE 1545               TO WAGP-COD-MSG-ERRO
               MOVE '1545'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAGP-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2120-VALIDA-CNPJ.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FORMATA ERROS DB2                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO WAGP-ERRO.
           MOVE 'GFCT0549'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          WRK-AUX-9-S
           MOVE WRK-AUX-9-3            TO WAGP-COD-SQL-ERRO.
           MOVE SPACES                 TO WAGP-DESC-MSG-ERRO.

           PERFORM 1110-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
               DELIMITED BY '  '       INTO WAGP-DESC-MSG-ERRO
           END-STRING.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICA SE CPF-CNPJ EH VALIDO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-VALIDA-CNPJ                SECTION.
      *----------------------------------------------------------------*

ST2506*    MOVE SPACES                 TO WRK-CONTROLE-0110.
ST2506*    MOVE WAGO-NUMERO            TO WRK-NUMERO-0110.
ST2506*    MOVE WAGO-FILIAL            TO WRK-FILIAL-0110.
ST2506*
ST2506*    CALL 'POOL0110'             USING WRK-CGC-CPF-0110
ST2506*                                      WRK-CONTROLE-0110.
           MOVE ZEROS                 TO WRK-CAD-CONTROLE. 
           MOVE WAGO-NUMERO           TO WRK-CAD-NUMERO.
           MOVE WAGO-FILIAL           TO WRK-CAD-FILIAL.

           CALL WRK-BRAD2000          USING WRK-CAD-CPFCGC
                                             WRK-CAD-CONTROLE.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO WAGP-ERRO
               MOVE 0468               TO WAGP-COD-MSG-ERRO
               MOVE '0468'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO WAGP-COD-SQL-ERRO
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAGP-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE WRK-CONTROLE-0110      TO WRK-CONTROLE
ST2506*
ST2506*    IF  WAGO-CONTRL             NOT EQUAL WRK-CONTROLE-R
           IF  WAGO-CONTRL             NOT EQUAL WRK-CAD-CONTROLE
               MOVE 1                  TO WAGP-ERRO
               MOVE 0507               TO WAGP-COD-MSG-ERRO
               MOVE '0507'             TO GFCTG2-COD-MSG
               MOVE ZEROS              TO WAGP-COD-SQL-ERRO
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAGP-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2121-PESQUISAR-BASE-CLIE.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA PESQUISAR BASE CLIE                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2121-PESQUISAR-BASE-CLIE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTIU-ENTRADA
                      GFCTIV-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.
           MOVE WAGO-TRANSACAO         TO GFCTIU-TRANSACAO.
           MOVE WAGO-FUNCAO            TO GFCTIU-FUNCAO.
           MOVE WAGO-FUNC-BDSCO        TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    MOVE WAGO-NUMERO            TO GFCTIU-NRO-CPF-CNPJ.
ST2506*    MOVE WAGO-FILIAL            TO GFCTIU-NRO-FILIAL.
ST2506*    MOVE WAGO-CONTRL            TO GFCTIU-CTRL-CPF-CNPJ.
           MOVE WAGO-NUMERO            TO GFCTIU-COD-CPF-CNPJ.
           MOVE WAGO-FILIAL            TO GFCTIU-COD-FILIAL.
           MOVE WAGO-CONTRL            TO GFCTIU-CTRL-CPF-CNPJ.

           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCTIU-ENTRADA
                                          GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAGP-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0549'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTIV-ERRO              NOT EQUAL ZEROS
               MOVE GFCTIV-ERRO         TO WAGP-ERRO
               MOVE GFCTIV-COD-SQL-ERRO TO WAGP-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO TO WAGP-COD-MSG-ERRO
               MOVE GFCTIV-DESC-MSG-ERRO
                                        TO WAGP-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSA A TABELA GFCTB0L7 PARA CONSISTIR DUPL. SEGMTO        *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSISTIR-SEGMTO           SECTION.
      *----------------------------------------------------------------*

           MOVE WAGO-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L7
           MOVE WAGO-CSEGMTO           TO CSGMTO-INIC-CLI    OF GFCTB0L7

           EXEC SQL
              SELECT DISTINCT
                     B.CAGPTO_CTA
              INTO
                     :GFCTB0L7.CAGPTO-CTA
              FROM   DB2PRD.TAUTRZ_ENVIO_AGPTO A,
                     DB2PRD.TAUTRZ_ENVIO_SGMTO B
             WHERE   A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND   A.HINCL_REG_SIST     = B.HINCL_REG_SIST
               AND   A.CFUNC_EXCL_SIST    = 0
               AND   B.CAGPTO_CTA         = :GFCTB0L7.CAGPTO-CTA
               AND   B.CSGMTO_INIC_CLI    = :GFCTB0L7.CSGMTO-INIC-CLI
           END-EXEC.

           IF (SQLCODE                 NOT  EQUAL  ZEROS  AND  +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 'TAUTRZ_ENVIO_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0L7'         TO WRK-NOM-TAB
               MOVE 'SELECT    '       TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO WAGP-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 1                  TO WAGP-ERRO
               MOVE ZEROS              TO WAGP-COD-SQL-ERRO
               MOVE 1545               TO WAGP-COD-MSG-ERRO
               MOVE '1545'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAGP-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSA A TABELA GFCTB0L6 PARA CONSISTIR DUPL. POSTO         *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSISTIR-POSTO            SECTION.
      *----------------------------------------------------------------*

           MOVE WAGO-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L6
           MOVE WAGO-CPAB              TO CPOSTO-SERVC       OF GFCTB0L6
           MOVE WAGO-CAGEN             TO CDEPDC             OF GFCTB0L6

           EXEC SQL
              SELECT DISTINCT
                    B.CAGPTO_CTA
              INTO
                    :GFCTB0L6.CAGPTO-CTA
              FROM  DB2PRD.TAUTRZ_ENVIO_AGPTO A,
                    DB2PRD.TAUTRZ_ENVIO_POSTO B
             WHERE  A.CAGPTO_CTA         = B.CAGPTO_CTA
               AND  A.HINCL_REG_SIST     = B.HINCL_REG_SIST
               AND  A.CFUNC_EXCL_SIST    = 0
               AND  B.CAGPTO_CTA         = :GFCTB0L6.CAGPTO-CTA
               AND  B.CPOSTO_SERVC       = :GFCTB0L6.CPOSTO-SERVC
               AND  B.CDEPDC             = :GFCTB0L6.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT  EQUAL  ZEROS  AND  +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 'TAUTRZ_ENVIO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0L6'         TO WRK-NOM-TAB
               MOVE 'SELECT    '       TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO WAGP-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 1                  TO WAGP-ERRO
               MOVE ZEROS              TO WAGP-COD-SQL-ERRO
               MOVE 1545               TO WAGP-COD-MSG-ERRO
               MOVE '1545'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAGP-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FAZ INCLUSAO DA AUTORIZACAO TAB. AGRUP.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-INCLUIR-AUTORIZACAO        SECTION.
      *----------------------------------------------------------------*

           MOVE WAGO-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L4
           MOVE 1                      TO CINDCD-AUTRZ-EMIS  OF GFCTB0L4
           MOVE WAGO-FUNC-BDSCO        TO WRK-FUNC-BDSCO-X.
           MOVE WRK-FUNC-BDSCO-N       TO CFUNC-INCL-SIST    OF GFCTB0L4
           MOVE ZEROS                  TO CFUNC-EXCL-SIST    OF GFCTB0L4
           MOVE WRK-TIMESTAMP          TO HINCL-REG-SIST     OF GFCTB0L4
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HEXCL-REG-SIST     OF GFCTB0L4

           MOVE LENGTH OF WAGO-JUSTIF  TO RJUSTF-INCL-AUTRZ-LEN
                                                             OF GFCTB0L4
           MOVE WAGO-JUSTIF            TO RJUSTF-INCL-AUTRZ-TEXT
                                                             OF GFCTB0L4
           MOVE SPACES                 TO RJUSTF-EXCL-AUTRZ-TEXT
                                                             OF GFCTB0L4
           MOVE ZEROS                  TO RJUSTF-EXCL-AUTRZ-LEN
                                                             OF GFCTB0L4

           EXEC SQL
             INSERT INTO DB2PRD.TAUTRZ_ENVIO_AGPTO
                  (CAGPTO_CTA,
                   CINDCD_AUTRZ_EMIS,
                   HINCL_REG_SIST,
                   CFUNC_INCL_SIST,
                   HEXCL_REG_SIST,
                   CFUNC_EXCL_SIST,
                   RJUSTF_INCL_AUTRZ,
                   RJUSTF_EXCL_AUTRZ)
               VALUES
                  (:GFCTB0L4.CAGPTO-CTA,
                   :GFCTB0L4.CINDCD-AUTRZ-EMIS,
                   :GFCTB0L4.HINCL-REG-SIST,
                   :GFCTB0L4.CFUNC-INCL-SIST,
                   :GFCTB0L4.HEXCL-REG-SIST,
                   :GFCTB0L4.CFUNC-EXCL-SIST,
                   :GFCTB0L4.RJUSTF-INCL-AUTRZ,
                   :GFCTB0L4.RJUSTF-EXCL-AUTRZ)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 'TAUTRZ_ENVIO_AGPTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0L4'         TO WRK-NOM-TAB
               MOVE 'INSERT    '       TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO WAGP-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  WAGO-CAGPTO       EQUAL 4
               PERFORM 2410-INCLUIR-CLIENTE
           ELSE
            IF  WAGO-CAGPTO      EQUAL 9
                PERFORM 2420-INCLUIR-SEGMTO
            ELSE
             IF  WAGO-CAGPTO     EQUAL 17 OR 20
                 PERFORM 2430-INCLUIR-POSTO
             END-IF
            END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FAZ INCLUSAO DO CLIENTE                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-INCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*

           MOVE WAGO-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L5
           MOVE 1                      TO CINDCD-AUTRZ-EMIS  OF GFCTB0L5
           MOVE WRK-TIMESTAMP          TO HINCL-REG-SIST     OF GFCTB0L5
           MOVE WAGO-NUMERO            TO CCNPJ-CPF          OF GFCTB0L5
           MOVE WAGO-FILIAL            TO CFLIAL-CGC         OF GFCTB0L5
           MOVE WAGO-CONTRL            TO CCTRL-CPF-CGC      OF GFCTB0L5

           EXEC SQL
             INSERT INTO DB2PRD.TAUTRZ_ENVIO_CLI
                  (CAGPTO_CTA,
                   CINDCD_AUTRZ_EMIS,
                   HINCL_REG_SIST,
                   CCNPJ_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC)
               VALUES
                  (:GFCTB0L5.CAGPTO-CTA,
                   :GFCTB0L5.CINDCD-AUTRZ-EMIS,
                   :GFCTB0L5.HINCL-REG-SIST,
                   :GFCTB0L5.CCNPJ-CPF,
                   :GFCTB0L5.CFLIAL-CGC,
                   :GFCTB0L5.CCTRL-CPF-CGC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 'TAUTRZ_ENVIO_CLI' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0L5'         TO WRK-NOM-TAB
               MOVE 'INSERT    '       TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO WAGP-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FAZ INCLUSAO DO SEGMENTO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-INCLUIR-SEGMTO             SECTION.
      *----------------------------------------------------------------*

           MOVE WAGO-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L7
           MOVE WRK-TIMESTAMP          TO HINCL-REG-SIST     OF GFCTB0L7
           MOVE 1                      TO CINDCD-AUTRZ-EMIS  OF GFCTB0L7
           MOVE WAGO-CSEGMTO           TO CSGMTO-INIC-CLI    OF GFCTB0L7

           EXEC SQL
             INSERT INTO DB2PRD.TAUTRZ_ENVIO_SGMTO
                  (CAGPTO_CTA,
                   CINDCD_AUTRZ_EMIS,
                   HINCL_REG_SIST,
                   CSGMTO_INIC_CLI)
               VALUES
                  (:GFCTB0L7.CAGPTO-CTA,
                   :GFCTB0L7.CINDCD-AUTRZ-EMIS,
                   :GFCTB0L7.HINCL-REG-SIST,
                   :GFCTB0L7.CSGMTO-INIC-CLI)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 'TAUTRZ_ENVIO_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0L7'         TO WRK-NOM-TAB
               MOVE 'INSERT    '       TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO WAGP-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FAZ INCLUSAO DO POSTO                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-INCLUIR-POSTO              SECTION.
      *----------------------------------------------------------------*

           MOVE WAGO-CAGPTO            TO CAGPTO-CTA         OF GFCTB0L6
           MOVE WRK-TIMESTAMP          TO HINCL-REG-SIST     OF GFCTB0L6
           MOVE 1                      TO CINDCD-AUTRZ-EMIS  OF GFCTB0L6
           MOVE 237                    TO CEMPR-INC          OF GFCTB0L6
           MOVE WAGO-CPAB              TO CPOSTO-SERVC       OF GFCTB0L6
           MOVE WAGO-CAGEN             TO CDEPDC             OF GFCTB0L6

           EXEC SQL
             INSERT INTO DB2PRD.TAUTRZ_ENVIO_POSTO
                  (CAGPTO_CTA,
                   CINDCD_AUTRZ_EMIS,
                   HINCL_REG_SIST,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0L6.CAGPTO-CTA,
                   :GFCTB0L6.CINDCD-AUTRZ-EMIS,
                   :GFCTB0L6.HINCL-REG-SIST,
                   :GFCTB0L6.CEMPR-INC,
                   :GFCTB0L6.CDEPDC,
                   :GFCTB0L6.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 'TAUTRZ_ENVIO_POSTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'GFCTB0L6'         TO WRK-NOM-TAB
               MOVE 'INSERT    '       TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO WAGP-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR     *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WAGP-ERRO               NOT EQUAL ZEROS
               MOVE WAGO-CAGPTO        TO WAGP-CAGPTO
               MOVE WAGO-CSEGMTO       TO WAGP-CSEGMTO
               MOVE WAGO-CAGEN         TO WAGP-CAGEN
               MOVE WAGO-CPAB          TO WAGP-CPAB
               MOVE WAGO-CCNPJ-CPF     TO WAGP-CNPJ-CPF
               MOVE WAGO-JUSTIF        TO WAGP-JUSTIF
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
