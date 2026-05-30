      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7885.
       AUTHOR.     JORGE VARGAS.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7885                                    *
      *    PROGRAMADOR.:   JORGE VARGAS            - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KELLI CALDATO           - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCKWORK GP 50   *
      *    DATA........:   05/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO ENTRE WEB E MF VIA      *
      *      SBAT3000 PARA AS CHAMADAS NOS MODULOS  REFERENTES  A      *
      *      FUNCIONALIDADE DE DEFERIMENTO DE FLEXIBILIZACAO  POR      *
      *      AGRUPAMENTO.                                              *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM SBAT3000                 *
      *    I#GFCT0K - AREA DE COMUNICACAO - SBAT3000 - ENTRADA         *
      *    I#GFCT0L - AREA DE COMUNICACAO - SBAT3000 - SAIDA           *
      *    I#GFCT0M - AREA DE COMUNICACAO - SBAT3000 - ERROS           *
      *    I#GFCTNN - LISTA CONSULTA DEFERIMENTO     - ENTRADA         *
      *    I#GFCTNO - LISTA CONSULTA DEFERIMENTO     - SAIDA           *
      *    I#GFCTNP - CONSULTA DEFERIMENTO DETALHE   - ENTRADA         *
      *    I#GFCTNQ - CONSULTA DEFERIMENTO DETALHE   - SAIDA           *
      *    I#GFCTNR - FUNCAO DE DEFERIMENTO          - ENTRADA         *
      *    I#GFCTNS - FUNCAO DE DEFERIMENTO          - SAIDA           *
      *    I#GFCTOM - COMBO AGRUPAMENTO COM FLEX     - ENTRADA         *
      *    I#GFCTON - COMBO AGRUPAMENTO COM FLEX     - SAIDA           *
      *    I#GFCT3O - AREA DE COMUNICACAO - SBAT3000 - RESTART         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT0888 - CONSULTA DEFERIMENTO DETALHE                     *
      *    GFCT0889 - DEFERIMENTO DE FLEXIBILIZACAO POR AGRUPAMENTO    *
      *    GFCT5146 - COMBO DE AGRUPAMENTOS COM FLEXIBILIZACAO         *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS                    *
      *    GFCT0425 - CONSULTA  DEFERIMENTO  DE  FLEXIBILIZACAO POR    *
      *               AGRUPAMENTO                                      *
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
           '* INICIO DA WORKING STORAGE SECTION *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
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
           '* AREA PARA MENSAGENS *'.
      *---------------------------------------------------------------*

       77  WRK-MSG01                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
       77  WRK-MSG02                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
       77  WRK-MSG03                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
       77  WRK-MSG04                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.

       01  WRK-MSG05.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05  WRK-MODULO-MSG          PIC  X(008)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INCLUDES DO PROGRAMA *'.
      *---------------------------------------------------------------*

       COPY I#ISDDPC.

       COPY I#GFCT0K.

       COPY I#GFCT0L.

       COPY I#GFCT0M.

       COPY I#GFCT3O.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO GFCT0425 *'.
      *---------------------------------------------------------------*

       COPY I#GFCTNN.

       COPY I#GFCTNO.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO GFCT0888 *'.
      *---------------------------------------------------------------*

       COPY I#GFCTNP.

       COPY I#GFCTNQ.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO GFCT0889 *'.
      *---------------------------------------------------------------*

       COPY I#GFCTNR.

       COPY I#GFCTNS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO GFCT5146 *'.
      *---------------------------------------------------------------*

       COPY I#GFCTOM.

       COPY I#GFCTON.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  IO-PCB.
           05  IO-TERM                 PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  IO-STA                  PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  IO-MODNAME              PIC  X(008).

       01  ALT-PCB.
           05  ALT-PCBNAME             PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  ALT-STATUS              PIC  X(002).
           05  ALT-DATE                PIC S9(007) COMP-3.
           05  ALT-TIME                PIC S9(007) COMP-3.
           05  ALT-SEQNO               PIC S9(003) COMP.
           05  FILLER                  PIC  X(002).
           05  ALT-MODNAME             PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING IO-PCB
                                             ALT-PCB.

      ******************************************************************
      * INICIAR PROCESSAMENTO                                          *
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
      * RECEBER MENSAGEM                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-RECEBER-MENSAGEM           SECTION.
      *----------------------------------------------------------------*

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
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FORMATAR AREA DE ERRO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7885'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO CENTRAL                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

             WHEN 'GFCT0425'
               MOVE GFCT0K-ENTRADA     TO GFCTNN-ENTRADA
               PERFORM 2100-PROCESSA-CONSULTA-DEF
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTNO-FIM      EQUAL 'S'

             WHEN 'GFCT0888'
               MOVE GFCT0K-ENTRADA     TO GFCTNP-ENTRADA
               PERFORM 2200-PROCESSA-CONSULTA-DET-DEF

             WHEN 'GFCT0889'
               MOVE GFCT0K-ENTRADA     TO GFCTNR-ENTRADA
               PERFORM 2300-PROCESSA-DEF-FLEX

             WHEN 'GFCT5146'
               MOVE GFCT0K-ENTRADA     TO GFCTOM-ENTRADA
               PERFORM 2400-PROCESSA-COMBO-AGRUP-FLEX
                   VARYING IND-1       FROM 1 BY 1
                   UNTIL IND-1         GREATER 3 OR
                   GFCTON-FIM          EQUAL 'S'
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA LISTA DE CONSULTA DEFERIMENTO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSA-CONSULTA-DEF      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNN-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNO-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNO-SAIDA
                                       GFCT0M-AREA-ERROS
                                       GFCT3O-AREA-RESTART
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2110-MODULO-CONSULTA-DEF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MODULO DE LISTA CONSULTA DEFERIMENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-MODULO-CONSULTA-DEF       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNN-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNN-ENTRADA
                                             GFCTNO-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNO-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2111-TRATAR-RET-CONSULTA-DEF

           PERFORM 2112-RET-WEB-CONSULTA-DEF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO DO MODULO LISTA DE CONSULTA DE DEFERIMENTO      *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-TRATAR-RET-CONSULTA-DEF    SECTION.
      *----------------------------------------------------------------*

           IF GFCTNO-FIM               EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS  3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTNO-QTDE-OCOR
                                       TO GFCTNN-QTDE-OCOR
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA WEB LISTA DE CONSULTA DEFERIMENTO                *
      ******************************************************************
      *----------------------------------------------------------------*
       2112-RET-WEB-CONSULTA-DEF       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNO-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTNO-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTNO-FIM         TO GFCTNN-FIM
               MOVE GFCTNO-QTDE-OCOR   TO GFCTNN-QTDE-OCOR
               MOVE ZEROS              TO GFCTNN-QTDE-TOT-REG
               MOVE GFCTNO-FUNC-BDSCO  TO GFCTNN-FUNC-BDSCO
               MOVE GFCTNO-FILTRO      TO GFCTNN-FILTRO
               MOVE GFCTNO-PONTEIRO    TO GFCTNN-PONTEIRO
               MOVE GFCTNN-DADOS-RST   TO GFCT3O-DADOS-RST
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
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA CONSULTA DEFERIMENTO DETALHE                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSA-CONSULTA-DET-DEF  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNP-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNQ-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNQ-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2210-MODULO-CONSULTA-DET-DEF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MODULO CONSULTA DEFERIMENTO DETALHE                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-MODULO-CONSULTA-DET-DEF    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNP-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNP-ENTRADA
                                             GFCTNQ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNQ-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2211-TRATAR-RET-COMUM.

           PERFORM 2212-RET-WEB-CONSULTA-DEF-DET.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMUM DOS MODULOS                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2211-TRATAR-RET-COMUM           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO GFCT0L-NRO-MSGI.
           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      ******************************************************************
      * RETORNAR PARA WEB CONSULTA DEFERIMENTO DETALHE                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2212-RET-WEB-CONSULTA-DEF-DET   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNQ-SAIDA           TO GFCT0L-ENTRADA.

           CALL 'SBAT3000'             USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA


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
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA  DEFERIMENTO DE FLEXIBILIZACAO                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSA-DEF-FLEX          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNS-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNS-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2310-MODULO-DEF-FLEX.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MODULO DEFERIMENTO DE FLEXIBILIZACAO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-MODULO-DEF-FLEX            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNR-ENTRADA
                                             GFCTNS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNS-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2211-TRATAR-RET-COMUM.

           PERFORM 2311-RET-WEB-DEF-FLEX.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA WEB DEFERIMENTO DE FLEXIBILIZACAO                *
      ******************************************************************
      *----------------------------------------------------------------*
       2311-RET-WEB-DEF-FLEX           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNS-SAIDA           TO GFCT0L-ENTRADA.

           CALL 'SBAT3000'             USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA


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
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO AGRUPAMENTO COM FLEX                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSA-COMBO-AGRUP-FLEX  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOM-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTON-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTON-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2410-MODULO-COMBO-AGRUP-FLEX.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MODULO COMBO AGRUPAMENTO COM FLEX                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-MODULO-COMBO-AGRUP-FLEX    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTOM-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOM-ENTRADA
                                             GFCTON-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTON-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2411-TRATAR-RET-CMB-AGRUP-FLEX.

           PERFORM 2412-RET-WEB-COMBO-AGRUP-FLEX.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO AGRUPAMENTO FLEX                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2411-TRATAR-RET-CMB-AGRUP-FLEX  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTON-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
               MOVE GFCTON-PONTEIRO    TO GFCTOM-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA WEB COMBO AGRUPAMENTO COM FLEX                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2412-RET-WEB-COMBO-AGRUP-FLEX   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTON-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTON-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTON-QTDE-OCOR   TO GFCTOM-QTDE-OCOR
               MOVE GFCTON-QTDE-TOT-REG
                                       TO GFCTOM-QTDE-TOT-REG
               MOVE GFCTON-FUNC-BDSCO  TO GFCTOM-FUNC-BDSCO
               MOVE GFCTON-FIM         TO GFCTOM-FIM
               MOVE GFCTOM-DADOS-RST   TO GFCT3O-DADOS-RST
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
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2412-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR ERROS OCORRIDOS DURANTE PROCESSAMENTO                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7885'             TO GFCT0M-PROGRAMA.
           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO

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
