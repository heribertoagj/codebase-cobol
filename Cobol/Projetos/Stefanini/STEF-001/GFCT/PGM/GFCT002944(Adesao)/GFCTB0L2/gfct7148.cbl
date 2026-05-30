      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT7148.
       AUTHOR.     HELIO SANTONI NETO.
      *================================================================*
      *    S O N D A   P R O C W O R K  -  C O N S U L T O R I A       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT7148                                     *
      *    PROGRAMADOR  : HELIO SANTONI           - PROCWORK  / GP.50  *
      *    ANALISTA     : WAGNER SILVA            - PROCWORK  / GP.50  *
      *    DATA         : 20/02/2009                                   *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO     :                                              *
      *      ROTEADOR DE EXCLUSAO MASSIVA ADESAO DE EXTRATO ANUAL DE   *
      *      TARIFAS.                                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    INC'S ......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO - ISD                        *
      *    I#GFCT0K - AREA DE COMUNICACAO-SBAT3000 - ENTRADA           *
      *    I#GFCT0L - AREA DE COMUNICACAO-SBAT3000 - SAIDA             *
      *    I#GFCT0M - AREA DE COMUNICACAO-SBAT3000 - ERROS             *
      *    I#GFCT3O - AREA DE COMUNICACAO-RESTART                      *
      *    GFCTWAFF - AREA DE COMUNICACAO-ENTRADA  - INCLUSAO          *
      *    GFCTWAFG - AREA DE COMUNICACAO-SAIDA    - INCLUSAO          *
      *    GFCTWAFH - AREA DE COMUNICACAO-ENTRADA  - CANCELAMENTO      *
      *    GFCTWAFI - AREA DE COMUNICACAO-SAIDA    - CANCELAMENTO      *
      *    GFCTWAFJ - AREA DE COMUNICACAO-ENTRADA  - DEFERIMENTO       *
      *    GFCTWAFK - AREA DE COMUNICACAO-SAIDA    - DEFERIMENTO       *
      *    GFCTWAFL - AREA DE COMUNICACAO-ENTRADA  - DET. DEFERIMENTO  *
      *    GFCTWAFM - AREA DE COMUNICACAO-SAIDA    - DET. DEFERIMENTO  *
      *    GFCTWAFN - AREA DE COMUNICACAO-ENTRADA  - CONSULTA          *
      *    GFCTWAFO - AREA DE COMUNICACAO-SAIDA    - CONSULTA          *
      *    GFCTWAFP - AREA DE COMUNICACAO-ENTRADA  - DET. CONSULTA     *
      *    GFCTWAFQ - AREA DE COMUNICACAO-SAIDA    - DET. CONSULTA     *
      *    I#GFCTQV - AREA DE COMUNICACAO-ENTRADA  - AG-CONTA          *
      *    I#GFCTWW - AREA DE COMUNICACAO-SAIDA    - AG-CONTA          *
      *    I#GFCT1V - AREA DE COMUNICACAO-ENTRADA  - LUPA AGENCIA      *
      *    I#GFCT1X - AREA DE COMUNICACAO-SAIDA    - LUPA AGENCIA      *
      *    I#GFCTGB - AREA DE COMUNICACAO-ENTRADA  - LUPA POSTO        *
      *    I#GFCTGC - AREA DE COMUNICACAO-SAIDA    - LUPA POSTO        *
      *    I#GFCTKP - AREA DE COMUNICACAO-ENTRADA  - COMBO PCT. TODOS  *
      *    I#GFCTKQ - AREA DE COMUNICACAO-SAIDA    - COMBO PCT. TODOS  *
      *    GFCTWABC - AREA DE COMUNICACAO-ENTRADA  - CORRESP. BANCAR.  *
      *    GFCTWABD - AREA DE COMUNICACAO-SAIDA    - CORRESP. BANCAR.  *
      *    GFCTWAFX - AREA DE COMUNICACAO-ENTRADA  - COMBO AGR. PERS.  *
      *    GFCTWAFY - AREA DE COMUNICACAO-SAIDA    - COMBO AGR. PERS.  *
      *    GFCTWAFV - AREA DE COMUNICACAO-ENTRADA  - PCT. PERSONALIZ   *
      *    GFCTWAFW - AREA DE COMUNICACAO-SAIDA    - PCT. PERSONALIZ   *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT0142 - INCLUSAO EXCLUSAO MASSIVA                        *
      *    GFCT0143 - CANCELAMENTO EXCLUSAO MASSIVA                    *
      *    GFCT0144 - DEFERIMENTO EXCLUSAO MASSIVA                     *
      *    GFCT0145 - DETALHE DEFERIMENTO EXCLUSAO MASSIVA             *
      *    GFCT0146 - CONSULTA EXCLUSAO MASSIVA                        *
      *    GFCT0147 - DETALHE CONSULTA EXCLUSAO MASSIVA                *
      *    GFCT5006 - LUPA AGENCIA                                     *
      *    GFCT5109 - LUPA POSTO                                       *
      *    GFCT5209 - LUPA CORRESPONDENTE BANCARIO                     *
      *    GFCT5089 - TEXT AGENCIA E CONTA                             *
      *    GFCT5599 - COMBO AGRUPAMENTO PERSONALIZADO                  *
      *    GFCT5601 - COMBO AGRUPAMENTO DA TABELA GFCTB0L0             *
      *    GFCT5119 - COMBO PACOTES TODOS                              *
      *    GFCT5600 - COMBO PACOTES PERSONALIZADA                      *
      *    GFCT5523 - OBTER DESCRICAO DA MENSAGEM                      *
      *    SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS                    *
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

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADORES ***'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-MSGF                    PIC  X(004)         VALUE 'MSGF'.
       77  WRK-MSGI                    PIC  X(004)         VALUE 'MSGI'.
       77  WRK-RSTA                    PIC  X(004)         VALUE 'RSTA'.
       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       77  WRK-ERRO                    PIC  X(004)         VALUE 'ERRO'.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       77  WRK-CALC-02                 PIC 9(02) COMP-3    VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *---------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05 WRK-MSG01                PIC  X(075)         VALUE
              'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
           05 WRK-MSG02                PIC  X(075)         VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           05 WRK-MSG03                PIC  X(075)         VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           05 WRK-MSG04                PIC  X(075)         VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           05  WRK-MSG05.
               07  FILLER              PIC X(26)           VALUE
                   'ERRO NA CHAMADA DO MODULO '.
               07  WRK-NOME-MOD        PIC X(08)           VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INCLUDES DO PROGRAMA ***'.
      *---------------------------------------------------------------*

       COPY 'I#ISDDPC'.

       COPY 'I#GFCT0K'.

       COPY 'I#GFCT0L'.

       COPY 'I#GFCT0M'.

       COPY 'I#GFCT3O'.

       COPY GFCTWAFF.

       COPY GFCTWAFG.

       COPY GFCTWAFH.

       COPY GFCTWAFI.

       COPY GFCTWAFJ.

       COPY GFCTWAFK.

       COPY GFCTWAFL.

       COPY GFCTWAFM.

       COPY GFCTWAFN.

       COPY GFCTWAFO.

       COPY GFCTWAFP.

       COPY GFCTWAFQ.

       COPY 'I#GFCTQV'.

       COPY 'I#GFCTWW'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

       COPY 'I#GFCTKP'.

       COPY 'I#GFCTKQ'.

       COPY GFCTWABC.

       COPY GFCTWABD.

       COPY GFCTWAFX.

       COPY GFCTWAFY.

       COPY GFCTWAFV.

       COPY GFCTWAFW.

       COPY GFCTWAGJ.

       COPY GFCTWAGK.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           03  LNK-IO-TERM             PIC  X(008).
           03  FILLER                  PIC  X(002).
           03  LNK-IO-STA              PIC  X(002).
           03  FILLER                  PIC  X(012).
           03  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           03  LNK-ALT-PCBNAME         PIC  X(008).
           03  FILLER                  PIC  X(002).
           03  LNK-ALT-STATUS          PIC  X(002).
           03  LNK-ALT-DATE            PIC S9(007) COMP-3.
           03  LNK-ALT-TIME            PIC S9(007) COMP-3.
           03  LNK-ALT-SEQNO           PIC S9(003) COMP.
           03  FILLER                  PIC  X(002).
           03  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.


      ******************************************************************
      * INICIALIZACAO DO PROGRAMA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-RECEBER-MENSAGEM.

           PERFORM 2000-PROCESSAR.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RECEBIMENTO DA MENSAGEM                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-RECEBER-MENSAGEM           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCT0K-AREA.

           CALL 'SBAT3000'             USING WRK-GU
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0K-ENTRADA.

           IF  MPA-RCODE-API           EQUAL 'QC'
               GOBACK
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSO                                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

               WHEN 'GFCT0142'
                   MOVE GFCT0K-ENTRADA TO WAFF-ENTRADA

                   PERFORM 2100-PROCESSAR-INCLUSAO

               WHEN 'GFCT0143'
                   MOVE GFCT0K-ENTRADA TO WAFH-ENTRADA

                   PERFORM 2200-PROCESSAR-CANCELAMENTO
                           VARYING IND-1    FROM 1 BY 1
                           UNTIL WAFI-FIM EQUAL 'S' OR
                           IND-1      GREATER 3

               WHEN 'GFCT0144'
                   MOVE GFCT0K-ENTRADA TO WAFJ-ENTRADA

                   PERFORM 2300-PROCESSAR-DEFERIMENTO
                           VARYING IND-1    FROM 1 BY 1
                           UNTIL WAFK-FIM EQUAL 'S' OR
                           IND-1      GREATER 3

               WHEN 'GFCT0145'
                   MOVE GFCT0K-ENTRADA TO WAFL-ENTRADA

                   PERFORM 2400-PROCESSAR-DETALHE-DEF

               WHEN 'GFCT0146'
                   MOVE GFCT0K-ENTRADA TO WAFN-ENTRADA

                   PERFORM 2500-PROCESSAR-CONSULTA
                           VARYING IND-1    FROM 1 BY 1
                           UNTIL WAFO-FIM EQUAL 'S' OR
                           IND-1      GREATER 3

               WHEN 'GFCT0147'
                   MOVE GFCT0K-ENTRADA TO WAFP-ENTRADA

                   PERFORM 2600-PROCESSAR-DETALHE-CONS

               WHEN 'GFCT5006'
                   MOVE GFCT0K-ENTRADA (1:680)
                                       TO GFCT1V-ENTRADA

                   PERFORM 2700-PROCESSAR-LUPA-AGENCIA

               WHEN 'GFCT5109'
                   MOVE GFCT0K-ENTRADA (1:100)
                                       TO GFCTGB-ENTRADA

                   PERFORM 2800-PROCESSAR-LUPA-POSTO

               WHEN 'GFCT5209'
                   MOVE GFCT0K-ENTRADA (1:100)
                                       TO WABC-ENTRADA

                   PERFORM 2900-PROCESSAR-CORRESP-BANC

               WHEN 'GFCT5089'
                   MOVE GFCT0K-ENTRADA (1:100)
                                       TO GFCTQV-ENTRADA

                   PERFORM 3000-PROCESSAR-TEXT-AG-CTA

               WHEN 'GFCT5599'
                   MOVE GFCT0K-ENTRADA (1:100)
                                       TO WAFX-ENTRADA

                   PERFORM 3100-PROCESSAR-AGRP-PERS
                           VARYING IND-1    FROM 1 BY 1
                           UNTIL WAFY-FIM EQUAL 'S' OR
                           IND-1      GREATER 3

               WHEN 'GFCT5119'
                   MOVE GFCT0K-ENTRADA (1:100)
                                       TO GFCTKP-ENTRADA

                   PERFORM 3200-PROCESSAR-PACOTE-TODOS
                           VARYING IND-1    FROM 1 BY 1
                           UNTIL GFCTKQ-FIM EQUAL 'S' OR
                           IND-1      GREATER 3

               WHEN 'GFCT5600'
                   MOVE GFCT0K-ENTRADA (1:100)
                                       TO WAFV-ENTRADA

                   PERFORM 3300-PROCESSAR-PACOTE-PERS
                           VARYING IND-1    FROM 1 BY 1
                           UNTIL WAFW-FIM EQUAL 'S' OR
                           IND-1      GREATER 3

               WHEN 'GFCT5601'
                   MOVE GFCT0K-ENTRADA (1:100)
                                       TO WAGJ-ENTRADA

                   PERFORM 3400-PROCESSAR-AGRUPMNTO
                           VARYING IND-1    FROM 1 BY 1
                           UNTIL WAGK-FIM EQUAL 'S' OR
                           IND-1      GREATER 3

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DE INCLUSAO                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAFG-SAIDA

           INITIALIZE WAFG-SAIDA

           PERFORM 2110-CHAMAR-GFCT0142

           MOVE WAFG-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 2120-TRATAR-RETORNO

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO INCLUSAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CHAMAR-GFCT0142            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFF-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAFF-ENTRADA
                                          WAFG-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFG-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-TRATAR-RETORNO             SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO GFCT0L-NRO-MSGI.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO SBAT3000 PARA COMINICACAO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-CHAMAR-SBAT3000            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
                                             GFCT3O-AREA-RESTART
           ELSE
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               IF  WRK-FUNCAO          EQUAL WRK-MSGI
                   MOVE WRK-MSG02      TO GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL WRK-MSGF
                       MOVE WRK-MSG03  TO GFCT0M-TEXTO
                   ELSE
                       MOVE WRK-MSG04  TO GFCT0M-TEXTO
                   END-IF
               END-IF
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA CONSULTA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAR-CANCELAMENTO     SECTION.
      *----------------------------------------------------------------*

           IF  WAFH-FIM                EQUAL 'N'
               MOVE SPACES             TO WAFI-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART

               INITIALIZE WAFI-SAIDA
                          GFCT0M-AREA-ERROS
                          GFCT3O-AREA-RESTART
           END-IF.

           PERFORM 2210-CHAMAR-GFCT0143.

           MOVE WAFI-SAIDA             TO GFCT0L-ENTRADA.

           PERFORM 2220-TRATAR-RET-CANCEL.

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-CHAMAR-GFCT0143            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFH-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO             USING WAFH-ENTRADA
                                             WAFI-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               PERFORM 9998-FORMATAR-ERRO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFI-ERRO               EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO CANCELAMENTO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-TRATAR-RET-CANCEL          SECTION.
      *----------------------------------------------------------------*

           IF  WAFI-FIM                EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE WAFI-QTDE-OCOR TO WAFH-QTDE-OCOR
                   MOVE ZEROS          TO WAFH-QTDE-TOT-REG
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB

                   PERFORM 2221-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAFI-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2221-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAFI-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAFI-QTDE-OCOR         TO WAFH-QTDE-OCOR.
           MOVE ZEROS                  TO WAFH-QTDE-TOT-REG.
           MOVE WAFI-FUNC-BDSCO        TO WAFH-FUNC-BDSCO.
           MOVE WAFI-AGPTO             TO WAFH-AGPTO.
           MOVE WAFI-COD-PACOTE        TO WAFH-COD-PACOTE.
           MOVE WAFI-FIM               TO WAFH-FIM.
           MOVE WAFI-OPCAO             TO WAFH-OPCAO.
           MOVE WAFH-DADOS-RST (1:245) TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA DEFERIMENTO                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-DEFERIMENTO      SECTION.
      *----------------------------------------------------------------*

           IF WAFJ-FIM          EQUAL 'N'
              MOVE SPACES              TO WAFK-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
              INITIALIZE WAFK-SAIDA
                         GFCT0M-AREA-ERROS
                         GFCT3O-AREA-RESTART
           END-IF.

           PERFORM 2310-CHAMAR-GFCT0144

           MOVE WAFK-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 2320-TRATAR-RET-DEFERIM

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO DEFERIMENTO                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-CHAMAR-GFCT0144            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFJ-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAFJ-ENTRADA
                                          WAFK-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFK-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO DEFERIMENTO                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-TRATAR-RET-DEFERIM         SECTION.
      *----------------------------------------------------------------*

           IF  WAFK-FIM             EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1             LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE WAFK-QTDE-OCOR TO WAFJ-QTDE-OCOR
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2321-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAFK-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2321-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAFK-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAFK-QTDE-OCOR         TO WAFJ-QTDE-OCOR.
           MOVE ZEROS                  TO WAFJ-QTDE-TOT-REG.
           MOVE WAFK-FUNC-BDSCO        TO WAFJ-FUNC-BDSCO.
           MOVE WAFK-FIM               TO WAFJ-FIM.
           MOVE WAFJ-DADOS-RST         TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO DETALHE DEFERIMENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-DETALHE-DEF      SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAFM-SAIDA

           INITIALIZE WAFM-SAIDA

           PERFORM 2410-CHAMAR-GFCT0145

           MOVE WAFM-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 2120-TRATAR-RETORNO

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO DETALHE DEFERIMENTO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-CHAMAR-GFCT0145            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFL-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAFL-ENTRADA
                                          WAFM-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFM-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO CONSULTA                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROCESSAR-CONSULTA         SECTION.
      *----------------------------------------------------------------*

           IF WAFN-FIM          EQUAL 'N'
              MOVE SPACES              TO WAFO-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
              INITIALIZE WAFO-SAIDA
                         GFCT0M-AREA-ERROS
                         GFCT3O-AREA-RESTART
           END-IF.

           PERFORM 2510-CHAMAR-GFCT0146

           MOVE WAFO-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 2520-TRATAR-RET-CONSULTA

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-CHAMAR-GFCT0146            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFN-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAFN-ENTRADA
                                          WAFO-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFO-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-TRATAR-RET-CONSULTA        SECTION.
      *----------------------------------------------------------------*

           IF  WAFO-FIM             EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1             LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE WAFO-QTDE-OCOR TO WAFN-QTDE-OCOR
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2521-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAFO-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2521-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAFO-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAFO-QTDE-OCOR         TO WAFN-QTDE-OCOR.
           MOVE ZEROS                  TO WAFN-QTDE-TOT-REG.
           MOVE WAFO-FUNC-BDSCO        TO WAFN-FUNC-BDSCO.
           MOVE WAFO-FIM               TO WAFN-FIM.
           MOVE WAFN-DADOS-RST         TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO DETALHE CONSULTA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-PROCESSAR-DETALHE-CONS     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAFQ-SAIDA

           INITIALIZE WAFQ-SAIDA

           PERFORM 2610-CHAMAR-GFCT0147

           MOVE WAFQ-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 2120-TRATAR-RETORNO

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO DETALHE CONSULTA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-CHAMAR-GFCT0147            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFP-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAFP-ENTRADA
                                          WAFQ-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFQ-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA LUPA AGENCIA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-PROCESSAR-LUPA-AGENCIA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT0L-RET-WEB

           INITIALIZE GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS
                      GFCT0L-RET-WEB

           PERFORM 2710-CHAMAR-GFCT5006

           MOVE GFCT1X-SAIDA           TO GFCT0L-ENTRADA

           PERFORM 2120-TRATAR-RETORNO

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO AGENCIA E CONTA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-CHAMAR-GFCT5006            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT1V-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT1X-ERRO          EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA LUPA POSTO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-PROCESSAR-LUPA-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT0L-RET-WEB

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS
                      GFCT0L-RET-WEB

           PERFORM 2810-CHAMAR-GFCT5109

           MOVE GFCTGC-SAIDA           TO GFCT0L-ENTRADA

           PERFORM 2120-TRATAR-RETORNO

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO POSTO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-CHAMAR-GFCT5109            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTGB-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTGC-ERRO          EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA LUPA CORRESPONDENTE BANCARIO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-PROCESSAR-CORRESP-BANC     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WABD-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT0L-RET-WEB

           INITIALIZE WABD-SAIDA
                      GFCT0M-AREA-ERROS
                      GFCT0L-RET-WEB

           PERFORM 2910-CHAMAR-GFCT5209

           MOVE WABD-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 2120-TRATAR-RETORNO

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO CORRESPONDENTE BANCARIO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2910-CHAMAR-GFCT5209            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WABC-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WABC-ENTRADA
                                          WABD-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WABD-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA AG-CONTA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR-TEXT-AG-CTA      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTQV-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTWW-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTWW-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 3010-MODULO-AG-CONTA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO AG-CONTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3010-MODULO-AG-CONTA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTQV-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCTQV-ENTRADA
                                          GFCTWW-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTWW-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3011-TRATAR-RET-AG-CONTA.

           MOVE GFCTWW-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO AG-CONTA                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3011-TRATAR-RET-AG-CONTA        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0L-NRO-MSGI        TO WRK-CALC-02
           ADD 1                       TO WRK-CALC-02
           MOVE WRK-CALC-02            TO GFCT0L-NRO-MSGI

           IF  GFCTWW-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3011-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DESCRICAO DA MENSAGEM SELECIONADA                        *
      ******************************************************************
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO AGRUPAMENTO PERSONALIZADO                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PROCESSAR-AGRP-PERS        SECTION.
      *----------------------------------------------------------------*

           IF WAFX-FIM          EQUAL 'N'
              MOVE SPACES              TO WAFY-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
              INITIALIZE WAFY-SAIDA
                         GFCT0M-AREA-ERROS
                         GFCT3O-AREA-RESTART
           END-IF.

           PERFORM 3110-CHAMAR-GFCT5599

           MOVE WAFY-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 3120-TRATAR-RET-AGRP-PERS

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO AGRUP. PERSON.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-CHAMAR-GFCT5599            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFX-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAFX-ENTRADA
                                          WAFY-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFY-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO AGRUPAMENTO PERSONALIZADO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-TRATAR-RET-AGRP-PERS       SECTION.
      *----------------------------------------------------------------*

           IF  WAFY-FIM             EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1             LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE WAFY-QTDE-OCOR TO WAFX-QTDE-OCOR
                   MOVE WAFY-PONTEIRO  TO WAFX-PONTEIRO
                   MOVE WAFY-FILTRO    TO WAFX-FILTRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 3121-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAFY-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3121-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAFY-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAFY-QTDE-OCOR         TO WAFX-QTDE-OCOR.
           MOVE ZEROS                  TO WAFX-QTDE-TOT-REG.
           MOVE WAFY-FUNC-BDSCO        TO WAFX-FUNC-BDSCO.
           MOVE WAFY-FILTRO            TO WAFX-FILTRO.
           MOVE WAFY-PONTEIRO          TO WAFX-PONTEIRO.
           MOVE WAFY-FIM               TO WAFX-FIM.
           MOVE WAFX-DADOS-RST         TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       3121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO COMBO PACOTE TODOS                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PROCESSAR-PACOTE-TODOS     SECTION.
      *----------------------------------------------------------------*

           IF GFCTKP-FIM        EQUAL 'N'
              MOVE SPACES              TO GFCTKQ-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
              INITIALIZE GFCTKQ-SAIDA
                         GFCT0M-AREA-ERROS
                         GFCT3O-AREA-RESTART
           END-IF.

           PERFORM 3210-CHAMAR-GFCT5119

           MOVE GFCTKQ-SAIDA           TO GFCT0L-ENTRADA

           PERFORM 3220-TRATAR-RET-PCT-TODOS

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO AGRUP. PERSON.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-CHAMAR-GFCT5119            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKP-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCTKP-ENTRADA
                                          GFCTKQ-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKQ-ERRO          EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO AGRUPAMENTO PERSONALIZADO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-TRATAR-RET-PCT-TODOS       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKQ-FIM           EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1             LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTKQ-QTDE-OCOR
                                       TO GFCTKP-QTDE-OCOR
                   MOVE GFCTKQ-PONTEIRO
                                       TO GFCTKP-PONTEIRO
                   MOVE GFCTKQ-FILTRO
                                       TO GFCTKP-FILTRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 3221-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTKQ-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3221-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE GFCTKQ-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTKQ-QTDE-OCOR       TO GFCTKP-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTKP-QTDE-TOT-REG.
           MOVE GFCTKQ-FUNC-BDSCO      TO GFCTKP-FUNC-BDSCO.
           MOVE GFCTKQ-FILTRO          TO GFCTKP-FILTRO.
           MOVE GFCTKQ-PONTEIRO        TO GFCTKP-PONTEIRO.
           MOVE GFCTKQ-FIM             TO GFCTKP-FIM.
           MOVE GFCTKP-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       3221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PACOTE PERSONALIZADO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-PACOTE-PERS      SECTION.
      *----------------------------------------------------------------*

           IF WAFV-FIM          EQUAL 'N'
              MOVE SPACES              TO WAFW-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
              INITIALIZE WAFW-SAIDA
                         GFCT0M-AREA-ERROS
                         GFCT3O-AREA-RESTART
           END-IF.

           PERFORM 3310-CHAMAR-GFCT5600

           MOVE WAFW-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 3320-TRATAR-RET-PCTE-PERS

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO PACOTE PERSON.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-CHAMAR-GFCT5600            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAFV-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAFV-ENTRADA
                                          WAFW-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAFW-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO AGRUPAMENTO PERSONALIZADO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3320-TRATAR-RET-PCTE-PERS       SECTION.
      *----------------------------------------------------------------*

           IF  WAFW-FIM             EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1             LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE WAFW-QTDE-OCOR TO WAFV-QTDE-OCOR
                   MOVE WAFW-PONTEIRO  TO WAFV-PONTEIRO
                   MOVE WAFW-FILTRO    TO WAFV-FILTRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 3321-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAFW-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3321-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAFW-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAFW-QTDE-OCOR         TO WAFV-QTDE-OCOR.
           MOVE ZEROS                  TO WAFV-QTDE-TOT-REG.
           MOVE WAFW-FUNC-BDSCO        TO WAFV-FUNC-BDSCO.
           MOVE WAFW-FILTRO            TO WAFV-FILTRO.
           MOVE WAFW-PONTEIRO          TO WAFV-PONTEIRO.
           MOVE WAFW-FIM               TO WAFV-FIM.
           MOVE WAFV-DADOS-RST         TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       3321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO AGRUPAMENTO EXISTENTE NA TABELA GFCTB0L0         *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-PROCESSAR-AGRUPMNTO        SECTION.
      *----------------------------------------------------------------*

           IF WAGJ-FIM          EQUAL 'N'
              MOVE SPACES              TO WAGK-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
              INITIALIZE WAGK-SAIDA
                         GFCT0M-AREA-ERROS
                         GFCT3O-AREA-RESTART
           END-IF.

           PERFORM 3410-CHAMAR-GFCT5601

           MOVE WAGK-SAIDA             TO GFCT0L-ENTRADA

           PERFORM 3420-TRATAR-RET-AGRP

           PERFORM 2130-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO AGRUP. PERSON.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3410-CHAMAR-GFCT5601            SECTION.
      *----------------------------------------------------------------*

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAGJ-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAGJ-ENTRADA
                                          WAGK-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE            NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-NOME-MOD
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAGK-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR RETORNO AGRUPAMENTO PERSONALIZADO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3420-TRATAR-RET-AGRP            SECTION.
      *----------------------------------------------------------------*

           IF  WAGK-FIM             EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1             LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE WAGK-QTDE-OCOR TO WAGJ-QTDE-OCOR
                   MOVE WAGK-PONTEIRO  TO WAGJ-PONTEIRO
                   MOVE WAGK-FILTRO    TO WAGJ-FILTRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 3421-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-CALC-02
               ADD 1                   TO WRK-CALC-02
               MOVE WRK-CALC-02        TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAGK-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    MOVIMENTACAO DE CAMPOS.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3421-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAGK-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAGK-QTDE-OCOR         TO WAGJ-QTDE-OCOR.
           MOVE ZEROS                  TO WAGJ-QTDE-TOT-REG.
           MOVE WAGK-FUNC-BDSCO        TO WAGJ-FUNC-BDSCO.
           MOVE WAGK-FILTRO            TO WAGJ-FILTRO.
           MOVE WAGK-PONTEIRO          TO WAGJ-PONTEIRO.
           MOVE WAGK-FIM               TO WAGJ-FIM.
           MOVE WAGJ-DADOS-RST         TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       3421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FORMATACAO DE ERRO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       9998-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7148'             TO GFCT0M-TRANSACAO
                                          GFCT0M-PROGRAMA.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA ROTINA DE ERRO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7148'             TO GFCT0M-TRANSACAO.

           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO.

           IF  GFCT0M-SQLCA-AREA       EQUAL SPACES OR LOW-VALUES
               CALL 'SBAT3000'         USING WRK-ERRO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
           ELSE
               CALL 'SBAT3000'         USING WRK-ERRO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
                                             GFCT0M-SQLCA-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
