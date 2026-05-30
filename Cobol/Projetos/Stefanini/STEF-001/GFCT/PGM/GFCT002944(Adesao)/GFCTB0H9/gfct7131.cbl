      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7131.
       AUTHOR.     ROBSON FAUSTO CESA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7131                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA      - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS AUGUSTO BOSCATO   - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO                 - PROCWORK GP 50    *
      *    DATA........:   27/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO ENTRE WEB E MF VIA      *
      *      SBAT3000 PARA AS CHAMADAS NOS MODULOS  REFERENTES  A      *
      *      FUNCIONALIDADE ADESAO COMPULSORIA.                        *
      *----------------------------------------------------------------*
      *    INC'S ......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM SBAT3000                 *
      *    I#GFCT0K - AREA DE COMUNICACAO COM SBAT3000 - ENTRADA       *
      *    I#GFCT0L - AREA DE COMUNICACAO COM SBAT3000 - SAIDA         *
      *    I#GFCT0M - AREA DE COMUNICACAO COM SBAT3000 - ERROS         *
      *    I#GFCT3O - AREA DE COMUNICACAO COM SBAT3000 - RESTART       *
      *    I#GFCTBR - AREA DE COMUNICACAO COM GFCT5060 - ENTRADA       *
      *    I#GFCTBS - AREA DE COMUNICACAO COM GFCT5060 - SAIDA         *
      *    I#GFCT1V - AREA DE COMUNICACAO COM GFCT5006 - ENTRADA       *
      *    I#GFCT1X - AREA DE COMUNICACAO COM GFCT5006 - SAIDA         *
      *    I#GFCTGB - AREA DE COMUNICACAO COM GFCT5109 - ENTRADA       *
      *    I#GFCTGC - AREA DE COMUNICACAO COM GFCT5109 - SAIDA         *
      *    GFCTWABC - AREA DE COMUNICACAO COM GFCT5209 - ENTRADA       *
      *    GFCTWABD - AREA DE COMUNICACAO COM GFCT5209 - SAIDA         *
      *    I#GFCTI1 - AREA DE COMUNICACAO COM GFCT5101 - ENTRADA       *
      *    I#GFCTI2 - AREA DE COMUNICACAO COM GFCT5101 - SAIDA         *
      *    I#GFCTKJ - AREA DE COMUNICACAO COM GFCT5117 - ENTRADA       *
      *    I#GFCTKK - AREA DE COMUNICACAO COM GFCT5117 - SAIDA         *
      *    I#GFCTKL - AREA DE COMUNICACAO COM GFCT5118 - ENTRADA       *
      *    I#GFCTKM - AREA DE COMUNICACAO COM GFCT5118 - SAIDA         *
      *    I#GFCTLH - AREA DE COMUNICACAO - ENTRADA - INCLUSAO         *
      *    I#GFCTLI - AREA DE COMUNICACAO - SAIDA - INCLUSAO           *
      *    I#GFCTLJ - AREA DE COMUNICACAO - ENTRADA - CANCELAMENTO     *
      *    I#GFCTLK - AREA DE COMUNICACAO - SAIDA - CANCELAMENTO       *
      *    I#GFCTLL - AREA DE COMUNICACAO - ENTRADA - CONSULTA         *
      *    I#GFCTLM - AREA DE COMUNICACAO - SAIDA - CONSULTA           *
      *    I#GFCTMT - AREA DE COMUNICACAO - ENTRADA - TP-AGRUPAMENTO   *
      *    I#GFCTMU - AREA DE COMUNICACAO - SAIDA - TP-AGRUPAMENTO     *
      *    I#GFCTN5 - AREA DE COMUNICACAO - ENTRADA - AGPTO-ESPEC      *
      *    I#GFCTN6 - AREA DE COMUNICACAO - ENTRADA - AGPTO-ESPEC      *
      *    I#GFCTN7 - AREA DE COMUNICACAO - ENTRADA - PACOTE           *
      *    I#GFCTN8 - AREA DE COMUNICACAO - SAIDA - PACOTE             *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT0132 - INCLUSAO                                         *
      *    GFCT0135 - CONSULTA                                         *
      *    GFCT0133 - CANCELAMENTO                                     *
      *    GFCT5101 - COMBO SEGMENTOS                                  *
      *    GFCT5060 - TEXT AGENCIA E CONTA                             *
      *    GFCT5006 - TEXT AGENCIA                                     *
      *    GFCT5109 - TEXT POSTO BANCARIO                              *
      *    GFCT5209 - TEXT CORRESP-BANCARIO                            *
      *    GFCT5117 - TEXT MUNICIPIO                                   *
      *    GFCT5118 - COMBO UF                                         *
      *    GFCT5130 - COMBO DE TIPO DE AGRUPAMENTO                     *
      *    GFCT5136 - COMBO DE PACOTES                                 *
      *    GFCT5135 - COMBO AGPTO ESPECIFICO                           *
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
           '* INICIO DA WORKING STORAGE SECTION *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

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

       COPY 'I#ISDDPC'.

       COPY 'I#GFCT0K'.

       COPY 'I#GFCT0L'.

       COPY 'I#GFCT0M'.

       COPY 'I#GFCT3O'.

       COPY 'I#GFCTBR'.

       COPY 'I#GFCTBS'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

       COPY GFCTWABC.

       COPY GFCTWABD.

       COPY 'I#GFCTI1'.

       COPY 'I#GFCTI2'.

       COPY 'I#GFCTKJ'.

       COPY 'I#GFCTKK'.

       COPY 'I#GFCTKL'.

       COPY 'I#GFCTKM'.

       COPY 'I#GFCTLH'.

       COPY 'I#GFCTLI'.

       COPY 'I#GFCTLJ'.

       COPY 'I#GFCTLK'.

       COPY 'I#GFCTLL'.

       COPY 'I#GFCTLM'.

       COPY 'I#GFCTMT'.

       COPY 'I#GFCTMU'.

       COPY 'I#GFCTN5'.

       COPY 'I#GFCTN6'.

       COPY 'I#GFCTN7'.

       COPY 'I#GFCTN8'.

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
      *  CHAMA AS ROTINAS PRINCIPAIS DO PROGRAMA                       *
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
      *  RECEBE O CONTROLE DO PROCESSAMENTO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-RECEBER-MENSAGEM           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCT0K-AREA.

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
      *  FORMATA A AREA DE ERRO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7131'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  PROCESSAMENTO PRINCIPAL                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

               WHEN 'GFCT0132'
                   MOVE GFCT0K-ENTRADA TO GFCTLH-ENTRADA
                   PERFORM 2100-PROCESSAR-INCLUSAO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTLI-FIM    EQUAL 'S'

               WHEN 'GFCT0135'
                   MOVE GFCT0K-ENTRADA TO GFCTLL-ENTRADA
                   PERFORM 2200-PROCESSA-MODULO-CONSULTA
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTLM-FIM    EQUAL 'S'

               WHEN 'GFCT0133'
                   MOVE GFCT0K-ENTRADA TO GFCTLJ-ENTRADA
                   PERFORM 2300-PROCESSA-CANCELAMENTO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTLK-FIM    EQUAL 'S'

               WHEN 'GFCT5130'
                   MOVE GFCT0K-ENTRADA TO GFCTMT-ENTRADA
                   PERFORM 2400-PROCESSAR-TP-AGRUP
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTMU-FIM    EQUAL 'S'

               WHEN 'GFCT5136'
                   MOVE GFCT0K-ENTRADA TO GFCTN7-ENTRADA
                   PERFORM 2500-PROCESSAR-PACOTE
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTN8-FIM    EQUAL 'S'

               WHEN 'GFCT5135'
                   MOVE GFCT0K-ENTRADA TO GFCTN5-ENTRADA
                   PERFORM 2600-PROCESSAR-AGPTO-ESPEC
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTN6-FIM    EQUAL 'S'

               WHEN 'GFCT5101'
                   MOVE GFCT0K-ENTRADA TO GFCTI1-ENTRADA
                   PERFORM 2700-PROCESSAR-SEGMENTOS
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTI2-FIM    EQUAL 'S'

               WHEN 'GFCT5060'
                   MOVE GFCT0K-ENTRADA TO GFCTBR-ENTRADA
                   PERFORM 2800-PROCESSAR-AG-CONTA
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTBS-FIM    EQUAL 'S'

               WHEN 'GFCT5006'
                   MOVE GFCT0K-ENTRADA TO GFCT1V-ENTRADA
                   PERFORM 2900-PROCESSAR-AGENCIA
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCT1X-FIM    EQUAL 'S'

               WHEN 'GFCT5109'
                   MOVE GFCT0K-ENTRADA TO GFCTGB-ENTRADA
                   PERFORM 3000-PROCESSAR-AG-POSTO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTGC-FIM    EQUAL 'S'

               WHEN 'GFCT5209'
                   MOVE GFCT0K-ENTRADA TO WABC-ENTRADA
                   PERFORM 3100-PROCESSAR-AG-CORR-BCO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         WABD-FIM    EQUAL 'S'

               WHEN 'GFCT5117'
                   MOVE GFCT0K-ENTRADA TO GFCTKJ-ENTRADA
                   PERFORM 3200-PROCESSAR-MUNICIPIO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKK-FIM    EQUAL 'S'

               WHEN 'GFCT5118'
                   MOVE GFCT0K-ENTRADA TO GFCTKL-ENTRADA
                   PERFORM 3300-PROCESSAR-UF
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKM-FIM    EQUAL 'S'

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT0123                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLH-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTLI-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTLI-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTLH-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTLH-ENTRADA
                                             GFCTLI-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTLI-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2110-TRATAR-RET-INCLUSAO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT0123                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-TRATAR-RET-INCLUSAO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLI-FIM              EQUAL 'S'
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
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTLI-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FINALIZA PROCESSAMENTO                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-RETORNAR-WEB               SECTION.
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
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT0126                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSA-MODULO-CONSULTA   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLL-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTLM-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTLM-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTLL-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTLL-ENTRADA
                                             GFCTLM-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTLM-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATA-RET-METOD-CONSULTA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT0126                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-TRATA-RET-METOD-CONSULTA   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLM-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTLM-PONT-AGPTO
                                       TO GFCTLL-PONT-AGPTO
                   MOVE GFCTLM-PONT-PACOTE
                                       TO GFCTLL-PONT-PACOTE
                   MOVE GFCTLM-PONT-CH-DATA
                                       TO GFCTLL-PONT-CH-DATA
                   MOVE GFCTLM-PONT-CH-NUM5
                                       TO GFCTLL-PONT-CH-NUM5
                   MOVE GFCTLM-PONT-CH-NUM13
                                       TO GFCTLL-PONT-CH-NUM13
                   MOVE GFCTLM-PONT-CH-ALP
                                       TO GFCTLL-PONT-CH-ALP
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2211-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTLM-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FORMATA AREA DE RESTART                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2211-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTLM-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTLM-QTDE-OCOR       TO GFCTLL-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTLL-QTDE-TOT-REG.
           MOVE GFCTLM-PONT-AGPTO      TO GFCTLL-PONT-AGPTO.
           MOVE GFCTLM-PONT-PACOTE     TO GFCTLL-PONT-PACOTE.
           MOVE GFCTLM-PONT-CH-DATA    TO GFCTLL-PONT-CH-DATA.
           MOVE GFCTLM-PONT-CH-NUM5    TO GFCTLL-PONT-CH-NUM5.
           MOVE GFCTLM-PONT-CH-NUM13   TO GFCTLL-PONT-CH-NUM13.
           MOVE GFCTLM-PONT-CH-ALP     TO GFCTLL-PONT-CH-ALP.
           MOVE GFCTLM-FUNC-BDSCO      TO GFCTLL-FUNC-BDSCO.
           MOVE GFCTLM-FIM             TO GFCTLL-FIM.
           MOVE GFCTLL-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT0125                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSA-CANCELAMENTO      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLJ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTLK-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTLK-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTLJ-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTLJ-ENTRADA
                                             GFCTLK-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTLK-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2310-TRATAR-RET-CANCELAMENTO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT0125                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-TRATAR-RET-CANCELAMENTO    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTLK-FIM              EQUAL 'S'
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
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTLK-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5130                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-TP-AGRUP         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTMT-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTMU-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTMU-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTMT-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTMT-ENTRADA
                                             GFCTMU-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTMU-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2410-TRATA-RET-TP-AGRUP.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5130                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-TRATA-RET-TP-AGRUP         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTMU-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTMU-PONTEIRO
                                       TO GFCTMT-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2411-MOVER-CAMPOS-02
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTMU-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FORMATA AREA DE RESTART                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2411-MOVER-CAMPOS-02            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTMU-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTMU-FILTRO          TO GFCTMT-FILTRO.
           MOVE GFCTMU-QTDE-OCOR       TO GFCTMT-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTMT-QTDE-TOT-REG.
           MOVE GFCTMU-PONTEIRO        TO GFCTMT-PONTEIRO.
           MOVE GFCTMU-FUNC-BDSCO      TO GFCTMT-FUNC-BDSCO.
           MOVE GFCTMU-TERMINAL        TO GFCTMT-TERMINAL.
           MOVE GFCTMU-FIM             TO GFCTMT-FIM.
           MOVE GFCTMT-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5136                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROCESSAR-PACOTE           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTN7-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTN8-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTN8-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTN7-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTN7-ENTRADA
                                             GFCTN8-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTN8-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2510-TRATA-RET-TIPO-OPERACAO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5136                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-TRATA-RET-TIPO-OPERACAO    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTN8-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTN8-PONTEIRO
                                       TO GFCTN7-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2511-MOVER-CAMPOS-03
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTN8-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FORMATA AREA DE RESTART                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2511-MOVER-CAMPOS-03            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTN8-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTN8-QTDE-OCOR       TO GFCTN7-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTN7-QTDE-TOT-REG.
           MOVE GFCTN8-PONTEIRO        TO GFCTN7-PONTEIRO.
           MOVE GFCTN8-FUNC-BDSCO      TO GFCTN7-FUNC-BDSCO.
           MOVE GFCTN8-FIM             TO GFCTN7-FIM.
           MOVE GFCTN7-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5135                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-PROCESSAR-AGPTO-ESPEC      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTN5-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTN6-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTN6-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTN5-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTN5-ENTRADA
                                             GFCTN6-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTN6-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2610-TRATA-RET-AGPTO-ESPEC.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5135                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-TRATA-RET-AGPTO-ESPEC      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTN6-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTN6-PONTEIRO
                                       TO GFCTN5-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2611-MOVER-CAMPOS-04
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTN6-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FORMATA AREA DE RESTART                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2611-MOVER-CAMPOS-04            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTN6-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTN6-FILTRO          TO GFCTN5-FILTRO.
           MOVE GFCTN6-QTDE-OCOR       TO GFCTN5-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTN5-QTDE-TOT-REG.
           MOVE GFCTN6-PONTEIRO        TO GFCTN5-PONTEIRO.
           MOVE GFCTN6-FUNC-BDSCO      TO GFCTN5-FUNC-BDSCO.
           MOVE GFCTN6-FIM             TO GFCTN5-FIM.
           MOVE GFCTN5-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5101                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-PROCESSAR-SEGMENTOS        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTI1-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTI2-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTI2-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTI1-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTI1-ENTRADA
                                             GFCTI2-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTI2-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2710-TRATA-RET-NOME-CLIENTE

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAR RETORNO DO MODULO GFCT5101                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-TRATA-RET-NOME-CLIENTE     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTI2-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTI2-PONTEIRO
                                       TO GFCTI1-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2711-MOVER-CAMPOS-05
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTI2-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FORMATA AREA DE RESTART                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2711-MOVER-CAMPOS-05            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTI2-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTI2-FILTRO          TO GFCTI1-FILTRO.
           MOVE GFCTI2-QTDE-OCOR       TO GFCTI1-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTI1-QTDE-TOT-REG.
           MOVE GFCTI2-PONTEIRO        TO GFCTI1-PONTEIRO.
           MOVE GFCTI2-FUNC-BDSCO      TO GFCTI1-FUNC-BDSCO.
           MOVE GFCTI2-TERMINAL        TO GFCTI1-TERMINAL.
           MOVE GFCTI2-FIM             TO GFCTI1-FIM.
           MOVE GFCTI1-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2711-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5060                                      *
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
           MOVE GFCTBR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTBS-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2810-TRATAR-RET-AG-CONTA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5060                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-TRATAR-RET-AG-CONTA        SECTION.
      *----------------------------------------------------------------*

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
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTBS-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5006                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-PROCESSAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           IF  GFCT1V-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT1X-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT1V-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT1X-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2910-TRATAR-RET-AGENCIA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5006                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2910-TRATAR-RET-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCT1X-FIM              EQUAL 'S'
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
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCT1X-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5109                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR-AG-POSTO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGB-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTGC-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTGB-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTGC-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3010-TRATAR-RET-AG-POSTO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5109                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3010-TRATAR-RET-AG-POSTO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGC-FIM              EQUAL 'S'
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
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTGC-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5209                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PROCESSAR-AG-CORR-BCO      SECTION.
      *----------------------------------------------------------------*

           IF  WABC-FIM                EQUAL 'N'
               MOVE SPACES             TO WABD-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       WABD-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WABC-FUNCAO            TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WABC-ENTRADA
                                             WABD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WABD-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3110-TRATAR-RET-AG-CORR-BCO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5209                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-TRATAR-RET-AG-CORR-BCO     SECTION.
      *----------------------------------------------------------------*

           IF  WABD-FIM                EQUAL 'S'
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
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WABD-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5117                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PROCESSAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKJ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKK-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKK-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKJ-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKJ-ENTRADA
                                             GFCTKK-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKK-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3210-TRATAR-RET-MUNICIPIO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5117                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-TRATAR-RET-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKK-FIM              EQUAL 'S'
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
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTKK-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  ACESSA O MODULO GFCT5118                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-UF               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKL-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKM-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKM-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKL-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKL-ENTRADA
                                             GFCTKM-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKM-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3310-TRATA-RET-UF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATA RETORNO DO MODULO GFCT5118                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-TRATA-RET-UF               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKM-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTKM-QTDE-OCOR
                                       TO GFCTKL-QTDE-OCOR
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 3311-MOVER-CAMPOS-06
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTKM-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  FORMATA AREA DE RESTART                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3311-MOVER-CAMPOS-06            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTKM-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTKM-FILTRO          TO GFCTKL-FILTRO.
           MOVE GFCTKM-QTDE-OCOR       TO GFCTKL-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTKL-QTDE-TOT-REG.
           MOVE GFCTKM-PONTEIRO        TO GFCTKL-PONTEIRO.
           MOVE GFCTKM-FUNC-BDSCO      TO GFCTKL-FUNC-BDSCO.
           MOVE GFCTKM-FIM             TO GFCTKL-FIM.
           MOVE GFCTKL-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       3311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *  TRATAMENTO DE ERROS                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7131'             TO GFCT0M-PROGRAMA.
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
