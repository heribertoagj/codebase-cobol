
***************************** Top of Data ******************************
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7136.
       AUTHOR.     FERNANDO BATTISTI.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7136                                    *
      *    PROGRAMADOR.:   FERNANDO BATTISTI       - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUSO                 - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO                 - PROCKWORK PG 50   *
      *    DATA........:   13/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO ENTRE WEB E MF VIA      *
      *      SBAT3000 PARA AS CHAMADAS NOS MODULOS  REFERENTES  A      *
      *      FUNCIONALIDADE ADESAO COMPULSORIA.                        *
      *----------------------------------------------------------------*
      *    INC'S ......:                                               *

      *    I#ISDDPC - AREA DE COMUNICACAO - ISD                        *
      *    I#GFCT0K - AREA DE COMUNICACAO-SBAT3000 - ENTRADA           *
      *    I#GFCT0L - AREA DE COMUNICACAO-SBAT3000 - SAIDA             *
      *    I#GFCT0M - AREA DE COMUNICACAO-SBAT3000 - ERROS             *
      *    I#GFCT3O - AREA DE COMUNICACAO-RESTART                      *
      *    I#GFCTNV - AREA DE COMUNICACAO-ENTRADA  - INCLUSAO          *
      *    I#GFCTNW - AREA DE COMUNICACAO-SAIDA    - INCLUSAO          *
      *    I#GFCTNX - AREA DE COMUNICACAO-ENTRADA  - CANCELAMENTO      *
      *    I#GFCTNY - AREA DE COMUNICACAO-SAIDA    - CANCELAMENTO      *
      *    I#GFCTNZ - AREA DE COMUNICACAO-ENTRADA  - CONSULTA          *
      *    I#GFCTO0 - AREA DE COMUNICACAO-SAIDA    - CONSULTA          *
      *    I#GFCTO3 - AREA DE COMUNICACAO-ENTRADA  - TP-AGRUPAMENTO    *
      *    I#GFCTO4 - AREA DE COMUNICACAO-SAIDA    - TP-AGRUPAMENTO    *
      *    I#GFCTO1 - AREA DE COMUNICACAO-ENTRADA  - PACOTE            *
      *    I#GFCTO2 - AREA DE COMUNICACAO-SAIDA    - PACOTE            *
      *    I#GFCTO7 - AREA DE COMUNICACAO-ENTRADA  - PACOTE-ESPEC      *
      *    I#GFCTO8 - AREA DE COMUNICACAO-SAIDA    - PACOTE-ESPEC      *
      *    I#GFCTOF - AREA DE COMUNICACAO-ENTRADA  - CONSULTA-CANC     *
      *    I#GFCTOG - AREA DE COMUNICACAO-SAIDA    - CONSULTA-CANC     *
      *    I#GFCTBR - AREA DE COMUNICACAO-ENTRADA  - AG-CONTA          *
      *    I#GFCTBS - AREA DE COMUNICACAO-SAIDA    - AG-CONTA          *

      *    I#GFCTO5 - AREA DE COMUNICACAO-ENTRADA  - TEXT-AG-CTA       *
      *    I#GFCTO6 - AREA DE COMUNICACAO-SAIDA    - TEXT-AG-CTA       *
      *    I#GFCTOD - AREA DE COMUNICACAO-ENTRADA  - TP-AGPTO          *
      *    I#GFCTOE - AREA DE COMUNICACAO-SAIDA    - TP-AGPTO          *
      *    I#GFCTOH - AREA DE COMUNICACAO-ENTRADA  - ARGUMENTO         *
      *    I#GFCTOI - AREA DE COMUNICACAO-SAIDA    - ARGUMENTO         *
      *    I#GFCTO9 - AREA DE COMUNICACAO-ENTRADA  - TP-AGPTO-ESPEC    *
      *    I#GFCTOA - AREA DE COMUNICACAO-SAIDA    - TP-AGPTO-ESPEC    *
      *    I#GFCTOB - AREA DE COMUNICACAO-ENTRADA  - PACOTE-CONSULTA   *
      *    I#GFCTOC - AREA DE COMUNICACAO-SAIDA    - PACOTE-CONSULTA   *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT0138 - INCLUSAO DE PERMISSAO DE ADESAO                  *
      *    GFCT0140 - CONSULTA DE PERMISSAO DE ADESAO                  *
      *    GFCT0139 - CANCELAMENTO DE PERMISSAO DE ADESAO              *
      *    GFCT0137 - CONSULTA DO CANCELAMENTO                         *
      *    GFCT5138 - COMBO DE TIPO DE AGRUPAMENTO                     *
      *    GFCT5137 - COMBO DE PACOTES                                 *
      *    GFCT5140 - COMBO PACOTE ESPECIFICO                          *
      *    GFCT5060 - TEXT AGENCIA E CONTA                             *
      *    GFCT5139 - TEXT AGENCIA E CONTA ESPECIFICO                  *

      *    GFCT5143 - COMBO TIPO DE AGRUPAMENTO ESPECIFICO             *
      *    GFCT5144 - COMBO DE AGRUPAMENTO                             *
      *    GFCT5141 - COMBO TIPO DE AGRUPAMENTO ESPECIFICO             *
      *    GFCT5142 - COMBO PACOTE DA CONSULTA                         *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
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

           05 WRK-MSG05.
             10 FILLER                 PIC  X(026)         VALUE
                'ERRO NA CHAMADA DO MODULO '.
             10 WRK-MODULO-MSG05       PIC  X(008)         VALUE SPACES.
             10 FILLER                 PIC  X(041)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INCLUDES DO PROGRAMA ***'.
      *---------------------------------------------------------------*

       COPY 'I#ISDDPC'.

       COPY 'I#GFCT0K'.

       COPY 'I#GFCT0L'.


       COPY 'I#GFCT0M'.

       COPY 'I#GFCT3O'.

       COPY 'I#GFCTNV'.

       COPY 'I#GFCTNW'.

       COPY 'I#GFCTNX'.

       COPY 'I#GFCTNY'.

       COPY 'I#GFCTNZ'.

       COPY 'I#GFCTO0'.

       COPY 'I#GFCTO3'.

       COPY 'I#GFCTO4'.

       COPY 'I#GFCTO1'.


       COPY 'I#GFCTO2'.

       COPY 'I#GFCTO7'.

       COPY 'I#GFCTO8'.

       COPY 'I#GFCTOF'.

       COPY 'I#GFCTOG'.

       COPY 'I#GFCTBR'.

       COPY 'I#GFCTBS'.

       COPY 'I#GFCTO5'.

       COPY 'I#GFCTO6'.

       COPY 'I#GFCTOD'.


       COPY 'I#GFCTOE'.

       COPY 'I#GFCTOH'.

       COPY 'I#GFCTOI'.

       COPY 'I#GFCTO9'.

       COPY 'I#GFCTOA'.

       COPY 'I#GFCTOB'.

       COPY 'I#GFCTOC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.

      *----------------------------------------------------------------*

       01  IO-PCB.
           03  IO-TERM                 PIC  X(008).
           03  FILLER                  PIC  X(002).
           03  IO-STA                  PIC  X(002).
           03  FILLER                  PIC  X(012).
           03  IO-MODNAME              PIC  X(008).

       01  ALT-PCB.
           03  ALT-PCBNAME             PIC  X(008).
           03  FILLER                  PIC  X(002).
           03  ALT-STATUS              PIC  X(002).
           03  ALT-DATE                PIC S9(007) COMP-3.
           03  ALT-TIME                PIC S9(007) COMP-3.
           03  ALT-SEQNO               PIC S9(003) COMP.
           03  FILLER                  PIC  X(002).
           03  ALT-MODNAME             PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.

      *================================================================*

           ENTRY 'DLITCBL'             USING IO-PCB
                                             ALT-PCB.


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
                                             IO-PCB
                                             ALT-PCB
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

               WHEN 'GFCT0138'
                   MOVE GFCT0K-ENTRADA TO GFCTNV-ENTRADA


                   PERFORM 2100-PROCESSAR-INCLUSAO

               WHEN 'GFCT0140'
                   MOVE GFCT0K-ENTRADA TO GFCTNZ-ENTRADA

                   PERFORM 2200-PROCESSAR-CONSULTA

               WHEN 'GFCT0139'
                   MOVE GFCT0K-ENTRADA TO GFCTNX-ENTRADA

                   PERFORM 2300-PROCESSAR-CANCELAMENTO

               WHEN 'GFCT5138'
                   MOVE GFCT0K-ENTRADA TO GFCTO3-ENTRADA

                   PERFORM 2400-PROCESSAR-TP-AGRUP

               WHEN 'GFCT5137'
                   MOVE GFCT0K-ENTRADA TO GFCTO1-ENTRADA


                   PERFORM 2500-PROCESSAR-PACOTE

               WHEN 'GFCT5140'
                   MOVE GFCT0K-ENTRADA TO GFCTO7-ENTRADA

                   PERFORM 2600-PROCESSAR-PACOTE-ESPEC

               WHEN 'GFCT0137'
                   MOVE GFCT0K-ENTRADA TO GFCTOF-ENTRADA

                   PERFORM 2700-PROCESSAR-CONSULTA-CANC

               WHEN 'GFCT5060'
                   MOVE GFCT0K-ENTRADA TO GFCTBR-ENTRADA

                   PERFORM 2800-PROCESSAR-AG-CONTA

               WHEN 'GFCT5139'
                   MOVE GFCT0K-ENTRADA TO GFCTO5-ENTRADA

                   PERFORM 2900-PROCESSAR-AG-CONTA-ESPEC


               WHEN 'GFCT5143'
                   MOVE GFCT0K-ENTRADA TO GFCTOD-ENTRADA

                   PERFORM 3000-PROCESSAR-TIPO-AGRUP

               WHEN 'GFCT5144'
                   MOVE GFCT0K-ENTRADA TO GFCTOH-ENTRADA

                   PERFORM 3100-PROCESSAR-ARGUMENTO

               WHEN 'GFCT5141'
                   MOVE GFCT0K-ENTRADA TO GFCTO9-ENTRADA

                   PERFORM 3200-PROCESS-TIPO-AGRUP-ESPEC

               WHEN 'GFCT5142'
                   MOVE GFCT0K-ENTRADA TO GFCTOB-ENTRADA

                   PERFORM 3300-PROCESSAR-PACOTE-CONSULTA


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

           IF  GFCTNV-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNW-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNW-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.


           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2110-MODULO-INCLUSAO.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO INCLUSAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-MODULO-INCLUSAO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNV-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNV-ENTRADA
                                             GFCTNW-SAIDA
                                             GFCT0M-AREA-ERROS.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNW-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2111-TRATAR-RETORNO-INCLUSAO.

           MOVE GFCTNW-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * TRATAMENTO DE RETORNO INCLUSAO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-TRATAR-RETORNO-INCLUSAO    SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTNW-FIM              EQUAL 'S'
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
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO SBAT3000 PARA COMINICACAO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2112-CHAMAR-SBAT3000            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB

                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
                                             GFCT3O-AREA-RESTART
           ELSE
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
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
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA CONSULTA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAR-CONSULTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNZ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTO0-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTO0-SAIDA

                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2210-MODULO-CONSULTA
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTO0-FIM              EQUAL 'S'.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-MODULO-CONSULTA            SECTION.
      *----------------------------------------------------------------*


           MOVE GFCTNZ-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNZ-ENTRADA
                                             GFCTO0-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTO0-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2211-TRATAR-RET-CONSULTA.


           MOVE GFCTO0-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL

               MOVE GFCTO0-FUNCAO      TO GFCT3O-FUNCAO

               MOVE GFCTO0-QTDE-OCOR   TO GFCTNZ-QTDE-OCOR

               MOVE ZEROS              TO GFCTNZ-QTDE-TOT-REG

               MOVE GFCTO0-FUNC-BDSCO  TO GFCTNZ-FUNC-BDSCO

               MOVE GFCTO0-FILTRO      TO GFCTNZ-FILTRO

               MOVE GFCTO0-PONT-PACOTE TO GFCTNZ-PONT-PACOTE
               MOVE GFCTO0-PONT-AGPTO  TO GFCTNZ-PONT-AGPTO
               MOVE GFCTO0-PONT-AGENCIA
                                       TO GFCTNZ-PONT-AGENCIA
               MOVE GFCTO0-PONT-CONTA  TO GFCTNZ-PONT-CONTA
               MOVE GFCTO0-PONT-SEQ    TO GFCTNZ-PONT-SEQ

               MOVE GFCTO0-PONT-DINI   TO GFCTNZ-PONT-DINI
               MOVE GFCTO0-PONT-HINI   TO GFCTNZ-PONT-HINI
               MOVE GFCTO0-FUNC-BDSCO  TO GFCTNZ-FUNC-BDSCO

               MOVE GFCTO0-FIM         TO GFCTNZ-FIM

               MOVE GFCTNZ-DADOS-RST   TO GFCT3O-DADOS-RST

           END-IF.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO CONSULTA                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2211-TRATAR-RET-CONSULTA        SECTION.

      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTO0-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTO0-PONTEIRO
                                       TO GFCTNZ-PONTEIRO
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
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO CANCELAMENTO                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-CANCELAMENTO     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNX-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNY-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNY-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.


           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2310-MODULO-CANCELAMENTO.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO CANCELAMENTO                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-MODULO-CANCELAMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNX-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNX-ENTRADA
                                             GFCTNY-SAIDA

                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNY-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2311-TRATAR-RET-CANCELAMENTO.

           MOVE GFCTNY-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO CANCELAMENTO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2311-TRATAR-RET-CANCELAMENTO    SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTNY-FIM              EQUAL 'S'
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
       2311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO TP-AGRUPAMENTO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-TP-AGRUP         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTO3-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTO4-SAIDA

                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTO4-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2410-MODULO-TP-AGRUP
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTO4-FIM              EQUAL 'S'.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO TP-AGRUP                                     *

      ******************************************************************
      *----------------------------------------------------------------*
       2410-MODULO-TP-AGRUP            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO3-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTO3-ENTRADA
                                             GFCTO4-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTO4-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-IF.

           PERFORM 2411-TRATAR-RET-AGRUP.

           MOVE GFCTO4-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL

               MOVE GFCTO4-FUNCAO      TO GFCT3O-FUNCAO

               MOVE GFCTO4-QTDE-OCOR   TO GFCTO3-QTDE-OCOR

               MOVE ZEROS              TO GFCTO3-QTDE-TOT-REG

               MOVE GFCTO4-FUNC-BDSCO  TO GFCTO3-FUNC-BDSCO
               MOVE GFCTO4-TERMINAL    TO GFCTO3-TERMINAL

               MOVE GFCTO4-FILTRO      TO GFCTO3-FILTRO

               MOVE GFCTO4-PONT-AGENCIA

                                       TO GFCTO3-PONT-AGENCIA
               MOVE GFCTO4-PONT-CONTA  TO GFCTO3-PONT-CONTA
               MOVE GFCTO4-PONT-PACOTE TO GFCTO3-PONT-PACOTE
               MOVE GFCTO4-PONT-CAGPTO TO GFCTO3-PONT-CAGPTO

               MOVE GFCTO4-FIM         TO GFCTO3-FIM

               MOVE GFCTO3-DADOS-REST  TO GFCT3O-DADOS-RST

           END-IF.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO TP-AGRUP                                 *
      ******************************************************************
      *----------------------------------------------------------------*

       2411-TRATAR-RET-AGRUP           SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTO4-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTO4-PONT-AGENCIA
                                       TO GFCTO3-PONT-AGENCIA
                   MOVE GFCTO4-PONT-CONTA
                                       TO GFCTO3-PONT-CONTA
                   MOVE GFCTO4-PONT-PACOTE
                                       TO GFCTO3-PONT-PACOTE
                   MOVE GFCTO4-PONT-CAGPTO
                                       TO GFCTO3-PONT-CAGPTO
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
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO PACOTE                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROCESSAR-PACOTE           SECTION.
      *----------------------------------------------------------------*


           IF  GFCTO1-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTO2-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTO2-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2510-MODULO-PACOTE
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTO2-FIM              EQUAL 'S'.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * CHAMADA DO MODULO PACOTE                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-MODULO-PACOTE              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO1-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTO1-ENTRADA
                                             GFCTO2-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           IF  GFCTO2-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2511-TRATAR-RET-PACOTE.

           MOVE GFCTO2-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL

               MOVE GFCTO2-FUNCAO      TO GFCT3O-FUNCAO

               MOVE GFCTO2-FILTRO      TO GFCTO1-FILTRO
               MOVE GFCTO2-QTDE-OCOR   TO GFCTO1-QTDE-OCOR
               MOVE ZEROS              TO GFCTO1-QTDE-TOT-REG
               MOVE GFCTO2-FUNC-BDSCO  TO GFCTO1-FUNC-BDSCO
               MOVE GFCTO2-PONTEIRO    TO GFCTO1-PONTEIRO
               MOVE GFCTO2-FIM         TO GFCTO1-FIM
               MOVE GFCTO1-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.


           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO PACOTE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2511-TRATAR-RET-PACOTE          SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTO2-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB

                   MOVE GFCTO2-PONTEIRO
                                       TO GFCTO1-PONTEIRO
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
       2511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO PACOTE ESPEC                                  *

      ******************************************************************
      *----------------------------------------------------------------*
       2600-PROCESSAR-PACOTE-ESPEC     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTO7-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTO8-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTO8-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2610-MODULO-PACOTE-ESPEC
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTO8-FIM              EQUAL 'S'.


      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO PACOTE ESPEC                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-MODULO-PACOTE-ESPEC        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO7-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTO7-ENTRADA
                                             GFCTO8-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05

               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTO8-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2611-TRATAR-RET-PACOTE-ESPEC.

           MOVE GFCTO8-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL

               MOVE GFCTO8-FUNCAO      TO GFCT3O-FUNCAO

               MOVE GFCTO8-FILTRO      TO GFCTO7-FILTRO

               MOVE GFCTO8-QTDE-OCOR   TO GFCTO7-QTDE-OCOR


               MOVE ZEROS              TO GFCTO7-QTDE-TOT-REG

               MOVE GFCTO8-FUNC-BDSCO  TO GFCTO7-FUNC-BDSCO

               MOVE GFCTO8-PONTEIRO-PACOTE
                                       TO GFCTO7-PONTEIRO-PACOTE
               MOVE GFCTO8-PONTEIRO-DEPDC
                                       TO GFCTO7-PONTEIRO-DEPDC
               MOVE GFCTO8-PONTEIRO-CCTA
                                       TO GFCTO7-PONTEIRO-CCTA

               MOVE GFCTO8-FIM         TO GFCTO7-FIM

               MOVE GFCTO7-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO PACOTE ESPEC                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2611-TRATAR-RET-PACOTE-ESPEC    SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTO8-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTO8-PONTEIRO
                                       TO GFCTO7-PONTEIRO
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
       2611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA CONSULTA CANC                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-PROCESSAR-CONSULTA-CANC    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOF-FIM              EQUAL 'N'

               MOVE SPACES             TO GFCTOG-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOG-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2710-MODULO-CONSULTA-CANC.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO CONSULTA-CANC                                *
      ******************************************************************
      *----------------------------------------------------------------*

       2710-MODULO-CONSULTA-CANC       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOF-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOF-ENTRADA
                                             GFCTOG-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOG-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


           PERFORM 2711-TRATA-RET-CONSULTA-CANC.

           MOVE GFCTOG-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO CONSULTA CANC                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2711-TRATA-RET-CONSULTA-CANC    SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTOG-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB

           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  IND-1               EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2711-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA AG-CONTA                                      *

      ******************************************************************
      *----------------------------------------------------------------*
       2800-PROCESSAR-AG-CONTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTBR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTBS-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2810-MODULO-AG-CONTA.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO AG-CONTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-MODULO-AG-CONTA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTBR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-IF.

           IF  GFCTBS-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2811-TRATAR-RET-AG-CONTA.

           MOVE GFCTBS-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO AG-CONTA                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2811-TRATAR-RET-AG-CONTA        SECTION.

      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTBS-FIM              EQUAL 'S'
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
       2811-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DA AG-CONTA-ESPEC                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-PROCESSAR-AG-CONTA-ESPEC   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTO5-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTO6-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTO6-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.


           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2910-MODULO-AG-CONTA-ESPEC.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO AG-CONTA-ESPEC                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2910-MODULO-AG-CONTA-ESPEC      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO5-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTO5-ENTRADA
                                             GFCTO6-SAIDA

                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTO6-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2911-TRATAR-RET-AG-CONTA-ESPEC.

           MOVE GFCTO6-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO AG-CONTA-ESPEC                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2911-TRATAR-RET-AG-CONTA-ESPEC  SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI

           IF  GFCTO6-FIM              EQUAL 'S'
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
       2911-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO TIPO AGRUP                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR-TIPO-AGRUP       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOD-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOE-SAIDA

                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOE-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 3010-MODULO-TIPO-AGRUP
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTOE-FIM              EQUAL 'S'.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO TIPO-AGRUP                                   *

      ******************************************************************
      *----------------------------------------------------------------*
       3010-MODULO-TIPO-AGRUP          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOD-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOD-ENTRADA
                                             GFCTOE-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOE-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-IF.

           PERFORM 3011-TRATAR-RET-TIPO-AGRUP.

           MOVE GFCTOE-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL

               MOVE GFCTOE-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTOE-FILTRO      TO GFCTOD-FILTRO

               MOVE GFCTOE-QTDE-OCOR   TO GFCTOD-QTDE-OCOR

               MOVE ZEROS              TO GFCTOD-QTDE-TOT-REG

               MOVE GFCTOE-FUNC-BDSCO  TO GFCTOD-FUNC-BDSCO
               MOVE GFCTOE-PONTEIRO    TO GFCTOD-PONTEIRO

               MOVE GFCTOE-FIM         TO GFCTOD-FIM


               MOVE GFCTOD-DADOS-RST   TO GFCT3O-DADOS-RST

           END-IF.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO TIPO AGRUP                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3011-TRATAR-RET-TIPO-AGRUP      SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTOE-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB

           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB

                   MOVE GFCTOE-PONTEIRO
                                       TO GFCTOD-PONTEIRO
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
      * PROCESSAMENTO DO ARGUMENTO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PROCESSAR-ARGUMENTO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOH-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOI-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOI-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.


           PERFORM 3110-MODULO-ARGUMENTO
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTOI-FIM              EQUAL 'S'.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO ARGUMENTO                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-MODULO-ARGUMENTO           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOH-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOH-ENTRADA
                                             GFCTOI-SAIDA

                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOI-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3111-TRATAR-RET-ARGUMENTO

           MOVE GFCTOI-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA

               MOVE 255                TO GFCT3O-LL


               MOVE GFCTOI-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTOI-QTDE-OCOR   TO GFCTOH-QTDE-OCOR

               MOVE ZEROS              TO GFCTOH-QTDE-TOT-REG

               MOVE GFCTOI-FUNC-BDSCO  TO GFCTOH-FUNC-BDSCO
               MOVE GFCTOI-FILTRO      TO GFCTOH-FILTRO
               MOVE GFCTOI-PONT-PACOTE TO GFCTOH-PONT-PACOTE
               MOVE GFCTOI-PONT-AGPTO  TO GFCTOH-PONT-AGPTO
               MOVE GFCTOI-PONT-SEQ    TO GFCTOH-PONT-SEQ
               MOVE GFCTOI-FIM         TO GFCTOH-FIM
               MOVE GFCTOH-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************

      * TRATAMENTO DE RETORNO ARGUMENTO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3111-TRATAR-RET-ARGUMENTO       SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTOI-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB

                   MOVE GFCTOI-PONT-PACOTE
                                       TO GFCTOH-PONT-PACOTE
                   MOVE GFCTOI-PONT-AGPTO
                                       TO GFCTOH-PONT-AGPTO
                   MOVE GFCTOI-PONT-SEQ
                                       TO GFCTOH-PONT-SEQ
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
       3111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO TIPO AGRU-ESPEC                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PROCESS-TIPO-AGRUP-ESPEC   SECTION.

      *----------------------------------------------------------------*

           IF  GFCTO9-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOA-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOA-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 3210-MODULO-TIPO-AGRUP-ESPEC
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTOA-FIM              EQUAL 'S'.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO TIPO-AGRUP-ESPEC                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-MODULO-TIPO-AGRUP-ESPEC    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTO9-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTO9-ENTRADA
                                             GFCTOA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO

           END-IF.

           IF  GFCTOA-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3211-TRAT-RET-TIPO-AGRU-ESPEC.

           MOVE GFCTOA-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTOA-FUNCAO      TO GFCT3O-FUNCAO

               MOVE GFCTOA-FILTRO      TO GFCTO9-FILTRO
               MOVE GFCTOA-QTDE-OCOR   TO GFCTO9-QTDE-OCOR

               MOVE ZEROS              TO GFCTO9-QTDE-TOT-REG

               MOVE GFCTOA-FUNC-BDSCO  TO GFCTO9-FUNC-BDSCO
               MOVE GFCTOA-PONTEIRO-DEPDC

                                       TO GFCTO9-PONTEIRO-DEPDC
               MOVE GFCTOA-PONTEIRO-CCTA
                                       TO GFCTO9-PONTEIRO-CCTA
               MOVE GFCTOA-PONTEIRO-TARIF
                                       TO GFCTO9-PONTEIRO-TARIF
               MOVE GFCTOA-PONTEIRO-AGRUP
                                       TO GFCTO9-PONTEIRO-AGRUP

               MOVE GFCTOA-FIM         TO GFCTO9-FIM

               MOVE GFCTO9-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO TIPO-AGRUP-ESPEC                         *

      ******************************************************************
      *----------------------------------------------------------------*
       3211-TRAT-RET-TIPO-AGRU-ESPEC   SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTOA-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB

                   MOVE GFCTOA-PONTEIRO-DEPDC
                                       TO GFCTO9-PONTEIRO-DEPDC
                   MOVE GFCTOA-PONTEIRO-CCTA
                                       TO GFCTO9-PONTEIRO-CCTA
                   MOVE GFCTOA-PONTEIRO-TARIF
                                       TO GFCTO9-PONTEIRO-TARIF
                   MOVE GFCTOA-PONTEIRO-AGRUP
                                       TO GFCTO9-PONTEIRO-AGRUP

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
       3211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO DO PACOTE CONSULTA                               *
      ******************************************************************
      *----------------------------------------------------------------*

       3300-PROCESSAR-PACOTE-CONSULTA  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOB-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOC-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOC-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.


           PERFORM 3310-MODULO-PACOTE-CONSULTA
               VARYING IND-1           FROM 1 BY 1
               UNTIL IND-1             GREATER 3 OR
               GFCTOC-FIM              EQUAL 'S'.


      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA DO MODULO PACOTE CONSULTA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-MODULO-PACOTE-CONSULTA     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOB-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOB-ENTRADA
                                             GFCTOC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               PERFORM 9998-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOC-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3311-TRATAR-RET-PACOTE-CONSUL.

           MOVE GFCTOC-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL

               MOVE GFCTOC-FUNCAO      TO GFCT3O-FUNCAO

               MOVE GFCTOC-FILTRO      TO GFCTOB-FILTRO

               MOVE GFCTOC-QTDE-OCOR   TO GFCTOB-QTDE-OCOR


               MOVE ZEROS              TO GFCTOB-QTDE-TOT-REG

               MOVE GFCTOC-FUNC-BDSCO  TO GFCTOB-FUNC-BDSCO

               MOVE GFCTOC-PONTEIRO    TO GFCTOB-PONTEIRO

               MOVE GFCTOC-FIM         TO GFCTOB-FIM

               MOVE GFCTOB-DADOS-RST   TO GFCT3O-DADOS-RST

           END-IF.

           PERFORM 2112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTO DE RETORNO PACOTE CONSULTA                          *
      ******************************************************************

      *----------------------------------------------------------------*
       3311-TRATAR-RET-PACOTE-CONSUL   SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTOC-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB

                   MOVE GFCTOC-PONTEIRO
                                       TO GFCTOB-PONTEIRO
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
       3311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FORMATACAO DE ERRO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       9998-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7136'             TO GFCT0M-TRANSACAO
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

           MOVE 'GFCT7136'             TO GFCT0M-TRANSACAO.

           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO.

           IF  GFCT0M-SQLCA-AREA       EQUAL SPACES OR LOW-VALUES
               CALL 'SBAT3000'         USING WRK-ERRO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA

                                             GFCT0M-ERRO-AREAS
           ELSE
               CALL 'SBAT3000'         USING WRK-ERRO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
                                             GFCT0M-SQLCA-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
