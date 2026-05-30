      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7285.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7285                                    *
      *    PROGRAMADORA:   SIMONI FAVRETTO         - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANA  - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO                 - PROCKWORK GP 50   *
      *    DATA........:   27/04/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO ENTRE WEB E MF VIA      *
      *      SBAT3000 PARA AS CHAMADAS NOS MODULOS  REFERENTES  A      *
      *      FUNCIONALIDADE SUSPENSAO DE TARIFA.                       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM SBAT3000                 *
      *    I#GFCT0K - ENTRADA - SBAT3000                               *
      *    I#GFCT0L - SAIDA   - SBAT3000                               *
      *    I#GFCT0M - ERROS   - SBAT3000                               *
      *    I#GFCT3O - RESTART - SBAT3000                               *
      *    I#GFCTP6 - ENTRADA - INCLUSAO                               *
      *    I#GFCTP7 - SAIDA   - INCLUSAO                               *
      *    I#GFCTP8 - ENTRADA - CANCELAMENTO                           *
      *    I#GFCTP9 - SAIDA   - CANCELAMENTO                           *
      *    I#GFCTPA - ENTRADA - CONSULTA                               *
      *    I#GFCTPB - SAIDA   - CONSULTA                               *
      *    I#GFCTPE - ENTRADA - TP-AGRUPAMENTO                         *
      *    I#GFCTPF - SAIDA   - TP-AGRUPAMENTO                         *
      *    I#GFCTCI - ENTRADA - PACOTE                                 *
      *    I#GFCTCJ - SAIDA   - PACOTE                                 *
      *    I#GFCTL3 - ENTRADA - PACOTE-ESPEC                           *
      *    I#GFCTL4 - SAIDA   - PACOTE-ESPEC                           *
      *    I#GFCTPI - ENTRADA - AGPTO-ESPEC                            *
      *    I#GFCTPJ - SAIDA   - AGPTO-ESPEC                            *
      *    I#GFCTPG - ENTRADA - TARIFA                                 *
      *    I#GFCTPH - SAIDA   - TARIFA                                 *
      *    I#GFCTPC - ENTRADA - AG-CONTA                               *
      *    I#GFCTPD - SAIDA   - AG-CONTA                               *
      *    I#GFCT1V - ENTRADA - AGENCIA                                *
      *    I#GFCT1X - SAIDA   - AGENCIA                                *
      *    I#GFCTGB - ENTRADA - AG-POSTO / COR-BANC                    *
      *    I#GFCTGC - SAIDA   - AG-POSTO / COR-BANC                    *
      *    I#GFCTKJ - ENTRADA - MUNICIPIO                              *
      *    I#GFCTKK - SAIDA   - MUNICIPIO                              *
      *    I#GFCTKL - ENTRADA - UF                                     *
      *    I#GFCTKM - SAIDA   - UF                                     *
      *    I#GFCTPK - ENTRADA - ARGUMENTOS                             *
      *    I#GFCTPL - SAIDA   - ARGUMENTOS                             *
      *    I#GFCTPM - ENTRADA - TP-AGRUPAMENTOS                        *
      *    I#GFCTPN - SAIDA   - TP-AGRUPAMENTOS                        *
      *    I#GFCTPO - ENTRADA - TARIFA                                 *
      *    I#GFCTPP - SAIDA   - TARIFA                                 *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS                    *
      *    GFCT0288 - INCLUSAO DE SUSPENSAO DE TARIFA                  *
      *    GFCT0292 - CONSULTA DE SUSPENSAO DE TARIFA                  *
      *    GFCT0304 - CANCELAMENTO DE SUSPENSAO DE TARIFA              *
      *    GFCT0315 - DETALHE DE CONSULTA                              *
      *    GFCT5154 - COMBO DE TIPO DE AGRUPAMENTO                     *
      *    GFCT5065 - COMBO DE PACOTES                                 *
      *    GFCT5156 - COMBO AGPTO ESPECIFICO                           *
      *    GFCT5155 - COMBO TARIFAS ESPEC                              *
      *    GFCT5006 - TEXT AGENCIA                                     *
      *    GFCT5109 - TEXT POSTO BANCARIO                              *
      *    GFCT5117 - TEXT MUNICIPIO                                   *
      *    GFCT5118 - COMBO UF                                         *
      *    GFCT5157 - COMBO ARGUMENTOS                                 *
      *    GFCT5158 - COMBO TIPO DE AGRUPAMENTO                        *
      *    GFCT5159 - COMBO TIPO DE AGRUPAMENTO                        *
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

       COPY I#GFCTP6.

       COPY I#GFCTP7.

       COPY I#GFCTP8.

       COPY I#GFCTP9.

       COPY I#GFCTPA.

       COPY I#GFCTPB.

       COPY I#GFCTPE.

       COPY I#GFCTPF.

       COPY I#GFCTCI.

       COPY I#GFCTCJ.

       COPY I#GFCTL3.

       COPY I#GFCTL4.

       COPY I#GFCTPI.

       COPY I#GFCTPJ.

       COPY I#GFCTPG.

       COPY I#GFCTPH.

       COPY I#GFCTPC.

       COPY I#GFCTPD.

       COPY I#GFCT1V.

       COPY I#GFCT1X.

       COPY I#GFCTGB.

       COPY I#GFCTGC.

       COPY I#GFCTKJ.

       COPY I#GFCTKK.

       COPY I#GFCTKM.

       COPY I#GFCTKL.

       COPY I#GFCTPK.

       COPY I#GFCTPL.

       COPY I#GFCTPM.

       COPY I#GFCTPN.

       COPY I#GFCTPO.

       COPY I#GFCTPP.

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
           MOVE 'GFCT7285'             TO GFCT0M-TRANSACAO.
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

             WHEN 'GFCT0288'
               MOVE GFCT0K-ENTRADA     TO GFCTP6-ENTRADA
               PERFORM 2100-PROCESSAR-INCLUSAO

             WHEN 'GFCT0292'
               MOVE GFCT0K-ENTRADA     TO GFCTPA-ENTRADA
               PERFORM 2200-PROCESSAR-CONSULTA
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTPB-FIM      EQUAL 'S'

             WHEN 'GFCT0304'
               MOVE GFCT0K-ENTRADA     TO GFCTP8-ENTRADA
               PERFORM 2300-PROCESSAR-CANCELAMENTO

             WHEN 'GFCT5154'
               MOVE GFCT0K-ENTRADA     TO GFCTPE-ENTRADA
               PERFORM 2400-PROCESSAR-TP-AGRUP
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTPF-FIM      EQUAL 'S'

             WHEN 'GFCT5065'
               MOVE GFCT0K-ENTRADA     TO GFCTCI-ENTRADA
               PERFORM 2500-PROCESSAR-TARIFA
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTCJ-FIM      EQUAL 'S'

             WHEN 'GFCT5156'
               MOVE GFCT0K-ENTRADA     TO GFCTPI-ENTRADA
               PERFORM 2600-PROCESSAR-AGPTO-ESPEC
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTPJ-FIM      EQUAL 'S'

             WHEN 'GFCT5155'
               MOVE GFCT0K-ENTRADA     TO GFCTPG-ENTRADA
               PERFORM 2700-PROCESSAR-SEGMENTOS
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTPH-FIM      EQUAL 'S'

             WHEN 'GFCT0315'
               MOVE GFCT0K-ENTRADA     TO GFCTPC-ENTRADA
               PERFORM 2800-PROCESSAR-DETALHE

             WHEN 'GFCT5006'
               MOVE GFCT0K-ENTRADA     TO GFCT1V-ENTRADA
               PERFORM 2900-PROCESSAR-AGENCIA

             WHEN 'GFCT5109'
             WHEN 'GFCT5090'
               MOVE GFCT0K-ENTRADA     TO GFCTGB-ENTRADA
               PERFORM 3000-PROCESSAR-AG-POSTO-BCO

             WHEN 'GFCT5117'
               MOVE GFCT0K-ENTRADA     TO GFCTKJ-ENTRADA
               PERFORM 3100-PROCESSAR-MUNICIPIO

             WHEN 'GFCT5118'
               MOVE GFCT0K-ENTRADA     TO GFCTKL-ENTRADA
               PERFORM 3200-PROCESSAR-UF
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTKM-FIM      EQUAL 'S'

             WHEN 'GFCT5157'
               MOVE GFCT0K-ENTRADA     TO GFCTPK-ENTRADA
               PERFORM 3300-PROCESSAR-ARGUMENTO
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTPL-FIM      EQUAL 'S'

             WHEN 'GFCT5158'
               MOVE GFCT0K-ENTRADA     TO GFCTPM-ENTRADA
               PERFORM 3400-PROCESSAR-TP-AGRUPAMENTO
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTPN-FIM      EQUAL 'S'

             WHEN 'GFCT5159'
               MOVE GFCT0K-ENTRADA     TO GFCTPO-ENTRADA
               PERFORM 3500-PROCESSAR-TARIFA-2
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTPP-FIM      EQUAL 'S'

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR INCLUSAO                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP6-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTP7-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTP7-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTP6-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTP6-ENTRADA
                                             GFCTP7-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTP7-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2110-TRATAR-RETORNO-COMUM.

           MOVE GFCTP7-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO DOS MODULOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-TRATAR-RETORNO-COMUM       SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO GFCT0L-NRO-MSGI.
           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA WEB                                              *
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
      * PROCESSAR CONSULTA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAR-CONSULTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPA-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPB-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPB-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPA-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPA-ENTRADA
                                             GFCTPB-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPB-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RET-CONSULTA.

           MOVE GFCTPB-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTPB-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTPB-QTDE-OCOR    TO GFCTPA-QTDE-OCOR
               MOVE ZEROS               TO GFCTPA-QTDE-TOT-REG
               MOVE GFCTPB-PONT-AGPTO   TO GFCTPA-PONT-AGPTO
               MOVE GFCTPB-PONT-TARIFA  TO GFCTPA-PONT-TARIFA
               MOVE GFCTPB-PONT-CH-DATA TO GFCTPA-PONT-CH-DATA
               MOVE GFCTPB-PONT-CH-NUM5 TO GFCTPA-PONT-CH-NUM5
               MOVE GFCTPB-PONT-CH-NUM13 TO GFCTPA-PONT-CH-NUM13
               MOVE GFCTPB-PONT-CH-ALP  TO GFCTPA-PONT-CH-ALP
               MOVE GFCTPB-FUNC-BDSCO   TO GFCTPA-FUNC-BDSCO
               MOVE GFCTPB-FIM          TO GFCTPA-FIM
               MOVE GFCTPA-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO CONSULTA                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-TRATAR-RET-CONSULTA        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPB-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTPB-PONT-AGPTO
                                       TO GFCTPA-PONT-AGPTO
                   MOVE GFCTPB-PONT-TARIFA
                                       TO GFCTPA-PONT-TARIFA
                   MOVE GFCTPB-PONT-CH-DATA
                                       TO GFCTPA-PONT-CH-DATA
                   MOVE GFCTPB-PONT-CH-NUM5
                                       TO GFCTPA-PONT-CH-NUM5
                   MOVE GFCTPB-PONT-CH-NUM13
                                       TO GFCTPA-PONT-CH-NUM13
                   MOVE GFCTPB-PONT-CH-ALP
                                       TO GFCTPA-PONT-CH-ALP
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
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR CANCELAMENTO                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-CANCELAMENTO     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP8-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTP9-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTP9-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTP8-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTP8-ENTRADA
                                             GFCTP9-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTP9-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2110-TRATAR-RETORNO-COMUM.

           MOVE GFCTP9-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TIPO DE AGRUPAMENTO                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-TP-AGRUP         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPE-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPF-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPF-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPE-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPE-ENTRADA
                                             GFCTPF-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPF-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2410-TRATAR-RET-TP-AGRUP.

           MOVE GFCTPF-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTPF-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTPF-FILTRO      TO GFCTPE-FILTRO
               MOVE GFCTPF-QTDE-OCOR   TO GFCTPE-QTDE-OCOR
               MOVE ZEROS              TO GFCTPE-QTDE-TOT-REG
               MOVE GFCTPF-PONTEIRO    TO GFCTPE-PONTEIRO
               MOVE GFCTPF-FUNC-BDSCO  TO GFCTPE-FUNC-BDSCO
               MOVE GFCTPF-FIM         TO GFCTPE-FIM
               MOVE GFCTPE-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO TIPO DE AGRUPAMENTO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-TRATAR-RET-TP-AGRUP        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPF-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTPF-PONTEIRO TO GFCTPE-PONTEIRO
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
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TARIFA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROCESSAR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCI-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTCJ-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTCJ-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTCI-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCI-ENTRADA
                                             GFCTCJ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTCJ-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2510-TRATAR-RET-TARIFA.

           MOVE GFCTCJ-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTCJ-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTCJ-FILTRO      TO GFCTCI-FILTRO
               MOVE GFCTCJ-QTDE-OCOR   TO GFCTCI-QTDE-OCOR
               MOVE ZEROS              TO GFCTCI-QTDE-TOT-REG
               MOVE GFCTCJ-PONTEIRO    TO GFCTCI-PONTEIRO
               MOVE GFCTCJ-FUNC-BDSCO  TO GFCTCI-FUNC-BDSCO
               MOVE GFCTCJ-TERMINAL    TO GFCTCI-TERMINAL
               MOVE GFCTCJ-FIM         TO GFCTCI-FIM
               MOVE GFCTCI-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO TARIFA                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-TRATAR-RET-TARIFA          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCJ-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTCJ-PONTEIRO TO GFCTCI-PONTEIRO
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
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR AGPTO ESPEC                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-PROCESSAR-AGPTO-ESPEC      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPI-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPJ-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPJ-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPI-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPI-ENTRADA
                                             GFCTPJ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPJ-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2610-TRATAR-RET-AGPTO-ESPEC.

           MOVE GFCTPJ-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTPJ-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTPJ-FILTRO      TO GFCTPI-FILTRO
               MOVE GFCTPJ-QTDE-OCOR   TO GFCTPI-QTDE-OCOR
               MOVE ZEROS              TO GFCTPI-QTDE-TOT-REG
               MOVE GFCTPJ-PONTEIRO    TO GFCTPI-PONTEIRO
               MOVE GFCTPJ-CSERVC-TARIF
                                       TO GFCTPI-CSERVC-TARIF
               MOVE GFCTPJ-FUNC-BDSCO  TO GFCTPI-FUNC-BDSCO
               MOVE GFCTPJ-FIM         TO GFCTPI-FIM
               MOVE GFCTPI-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO AGPTO ESPEC                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-TRATAR-RET-AGPTO-ESPEC     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPJ-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTPJ-PONTEIRO TO GFCTPI-PONTEIRO
                   MOVE GFCTPJ-CSERVC-TARIF
                                       TO GFCTPI-CSERVC-TARIF
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
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR SEGMENTOS                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-PROCESSAR-SEGMENTOS        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPG-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPH-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPH-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPG-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPG-ENTRADA
                                             GFCTPH-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPH-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2710-TRATAR-RET-SEGMENTOS.

           MOVE GFCTPH-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTPH-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTPH-FILTRO      TO GFCTPG-FILTRO
               MOVE GFCTPH-QTDE-OCOR   TO GFCTPG-QTDE-OCOR
               MOVE ZEROS              TO GFCTPG-QTDE-TOT-REG
               MOVE GFCTPH-PONTEIRO    TO GFCTPG-PONTEIRO
               MOVE GFCTPH-FUNC-BDSCO  TO GFCTPG-FUNC-BDSCO
               MOVE GFCTPH-FIM         TO GFCTPG-FIM
               MOVE GFCTPG-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO SEGMENTOS                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-TRATAR-RET-SEGMENTOS       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPH-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTPH-PONTEIRO TO GFCTPG-PONTEIRO
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
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR DETALHE                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-PROCESSAR-DETALHE          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPC-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPD-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPD-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPC-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPC-ENTRADA
                                             GFCTPD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPD-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2110-TRATAR-RETORNO-COMUM.

           MOVE GFCTPD-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR AGENCIA                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-PROCESSAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           IF  GFCT1V-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS
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

           PERFORM 2110-TRATAR-RETORNO-COMUM.

           MOVE GFCT1X-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR AG POSTO / CORR BCO                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR-AG-POSTO-BCO     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGB-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS
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

           PERFORM 2110-TRATAR-RETORNO-COMUM.

           MOVE GFCTGC-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR MUNICIPIO                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-PROCESSAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKJ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKK-SAIDA
                                          GFCT0M-AREA-ERROS
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

           PERFORM 2110-TRATAR-RETORNO-COMUM.

           MOVE GFCTKK-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR UF                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PROCESSAR-UF               SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKL-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKM-SAIDA
                                          GFCT0M-AREA-ERROS
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

           PERFORM 3210-TRATAR-RET-UF.

           MOVE GFCTKM-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTKM-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTKM-FILTRO      TO GFCTKL-FILTRO
               MOVE GFCTKM-QTDE-OCOR   TO GFCTKL-QTDE-OCOR
               MOVE ZEROS              TO GFCTKL-QTDE-TOT-REG
               MOVE GFCTKM-PONTEIRO    TO GFCTKL-PONTEIRO
               MOVE GFCTKM-FUNC-BDSCO  TO GFCTKL-FUNC-BDSCO
               MOVE GFCTKM-FIM         TO GFCTKL-FIM
               MOVE GFCTKL-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO UF                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-TRATAR-RET-UF              SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKM-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTKM-PONTEIRO TO GFCTKL-PONTEIRO
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
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR ARGUMENTOS                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-ARGUMENTO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPK-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPL-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPL-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPK-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPK-ENTRADA
                                             GFCTPL-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPL-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3310-TRATAR-RET-ARGUMENTOS.

           MOVE GFCTPL-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTPL-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTPL-QTDE-OCOR   TO GFCTPK-QTDE-OCOR
               MOVE ZEROS              TO GFCTPK-QTDE-TOT-REG
               MOVE GFCTPL-PONT-AGPTO  TO GFCTPK-PONT-AGPTO
               MOVE GFCTPL-PONT-TARIFA TO GFCTPK-PONT-TARIFA
               MOVE GFCTPL-PONT-SEQ    TO GFCTPK-PONT-SEQ
               MOVE GFCTPL-FUNC-BDSCO  TO GFCTPK-FUNC-BDSCO
               MOVE GFCTPL-FIM         TO GFCTPK-FIM
               MOVE GFCTPK-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO ARGUMENTOS                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-TRATAR-RET-ARGUMENTOS      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPL-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTPL-PONT-AGPTO
                                       TO GFCTPK-PONT-AGPTO
                   MOVE GFCTPL-PONT-TARIFA
                                       TO GFCTPK-PONT-TARIFA
                   MOVE GFCTPL-PONT-SEQ
                                       TO GFCTPK-PONT-SEQ
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
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TIPO DE AGRUPAMENTOS                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-PROCESSAR-TP-AGRUPAMENTO   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPM-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPN-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPN-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPM-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPM-ENTRADA
                                             GFCTPN-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPN-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3410-TRATAR-RET-TP-AGRUPAMENTO.

           MOVE GFCTPN-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTPN-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTPN-QTDE-OCOR   TO GFCTPM-QTDE-OCOR
               MOVE ZEROS              TO GFCTPM-QTDE-TOT-REG
               MOVE GFCTPN-PONTEIRO-AGRUP
                                       TO GFCTPM-PONTEIRO-AGRUP
               MOVE GFCTPN-FUNC-BDSCO  TO GFCTPM-FUNC-BDSCO
               MOVE GFCTPN-FIM         TO GFCTPM-FIM
               MOVE GFCTPM-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO TIPO DE AGRUPAMENTO                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3410-TRATAR-RET-TP-AGRUPAMENTO  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPN-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTPN-PONTEIRO-AGRUP
                                       TO GFCTPM-PONTEIRO-AGRUP
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
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TARIFAS                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-PROCESSAR-TARIFA-2         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPO-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTPP-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTPP-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTPO-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTPO-ENTRADA
                                             GFCTPP-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTPP-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3510-TRATAR-RET-TARIFA-2.

           MOVE GFCTPP-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTPP-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTPP-PONTEIRO-TARIFA
                                       TO GFCTPO-PONTEIRO-TARIFA
               MOVE GFCTPP-QTDE-OCOR   TO GFCTPO-QTDE-OCOR
               MOVE ZEROS              TO GFCTPO-QTDE-TOT-REG
               MOVE GFCTPP-FUNC-BDSCO  TO GFCTPO-FUNC-BDSCO
               MOVE GFCTPP-FIM         TO GFCTPO-FIM
               MOVE GFCTPO-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO TARIFAS                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3510-TRATAR-RET-TARIFA-2        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTPP-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTPP-PONTEIRO-TARIFA
                                       TO GFCTPO-PONTEIRO-TARIFA
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
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR ERROS OCORRIDOS DURANTE PROCESSAMENTO                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7285'             TO GFCT0M-PROGRAMA.
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
