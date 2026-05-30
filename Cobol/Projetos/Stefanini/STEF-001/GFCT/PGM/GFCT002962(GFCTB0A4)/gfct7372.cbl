      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7372.
       AUTHOR. MARCIO RODRIGO DA CUNHA.
      *================================================================*
      *                   C P M  S I S T E M A S                       *
      *================================================================*
      *                                                                *
      *    PROGRAMA     : GFCT7372                                     *
      *    PROGRAMADOR  : MARCIO RODRIGO DA CUNHA.      - CPM/FPOLIS   *
      *    ANALISTA CPM : SILVANA F. SOUZA MACHADO      - CPM/FPOLIS   *
      *    ANALISTA     : MARCELO  -  PROCWORK          - GRUPO 50     *
      *    DATA         : 31/10/2005                                   *
      *                                                                *
      *    OBJETIVOS    :                                              *
      *      EFETUAR COMUNICACAO ENTRE WEB E MF VIA SBAT3000 PARA AS   *
      *      CHAMADAS NOS MODULOS REFERENTES A FUNCIONALIDADE          *
      *      SOLICITACAO DE ESTORNO PARA AGRUPAMENTO.                  *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *      SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS.                 *
      *      GFCT0373 - INCLUSAO DE SOLICITACAO                        *
      *      GFCT0375 - CONSULTA OU EFETIVA ESTORNO POR AGRUPAMENTO    *
      *      GFCT0380 - LISTA DE ESTORNOS SOLICITADOS                  *
      *      GFCT5087 - COMBO DE TIPO DE AGRUPAMENTOS                  *
      *      GFCT5015 - COMBO DE TARIFAS                               *
      *      GFCT5068 - COMBO DE TIPOS DE OPERACAO                     *
      *      GFCT5014 - COMBO MOTIVOS DE OPERACAO                      *
      *      GFCT5088 - COMBO DE SEGMENTOS                             *
      *      GFCT5006 - TEXT AGENCIA                                   *
      *      GFCT5060 - TEXT AGENCIA E CONTA                           *
      *      GFCT5090 - TEXT CORRESPONDENTE BANCARIO                   *
      *      GFCT5109 - TEXT POSTO BANCARIO (EXCLUSIVO PAB)            *
      *      GFCT5184 - COMBO DE CONJUNTO DE SERVICO                   *
      *      GFCT5186 - COMBO DE TARIFA                                *
      *                                                                *
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
       01  FILLER                      PIC  X(32)          VALUE
           '*  INICIO DA WORKING GFCT7372  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*         AUXILIARES           *'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(04)          VALUE 'GU'.
       01  WRK-RSTA                    PIC  X(04)          VALUE 'RSTA'.
       01  WRK-MSGF                    PIC  X(04)          VALUE 'MSGF'.
       01  WRK-MSGI                    PIC  X(04)          VALUE 'MSGI'.
       01  WRK-ERRO                    PIC  X(04)          VALUE 'ERRO'.
       01  WRK-FUNCAO                  PIC  X(04)          VALUE SPACES.
       01  WRK-MODULO                  PIC  X(08)          VALUE SPACES.
       01  WRK-BLOCO                   PIC  9(03)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*          MENSAGENS           *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(75)          VALUE
               'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
           03  WRK-MSG02               PIC  X(75)          VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           03  WRK-MSG03               PIC  X(75)          VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           03  WRK-MSG04               PIC  X(75)          VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           03  WRK-MSG05.
               05  FILLER              PIC  X(26)          VALUE
                   'ERRO NA CHAMADA DO MODULO'.
               05  WRK-MODULO-MSG05    PIC  X(08)          VALUE SPACES.
               05  FILLER              PIC  X(41)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(39)          VALUE
           '* AREAS DE COMUNICACAO DOS PROGRAMAS *'.
      *----------------------------------------------------------------*

       COPY I#GFCTFR.
       COPY I#GFCTFS.
       COPY I#GFCTFT.
       COPY I#GFCTFU.
       COPY I#GFCTFV.
       COPY I#GFCTFW.
       COPY I#GFCTGA.
       COPY I#GFCTGB.
       COPY I#GFCTGC.
       COPY I#GFCTG5.
       COPY I#GFCTG6.
       COPY I#GFCTG7.
       COPY I#GFCTG8.
       COPY I#GFCTG9.
       COPY I#GFCT3A.
       COPY I#GFCT3B.
       COPY I#GFCT3T.
       COPY I#GFCT3U.
       COPY I#GFCTCO.
       COPY I#GFCTCP.
       COPY I#GFCTBR.
       COPY I#GFCTBS.
       COPY I#GFCT1V.
       COPY I#GFCT1X.
       COPY I#GFCTVP.
       COPY I#GFCTVQ.
       COPY I#GFCTVR.
       COPY I#GFCTVS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '*   AREA DE COM. - SBAT3000    *'.
      *----------------------------------------------------------------*

       COPY I#ISDDPC.

       COPY I#GFCT0M.

       COPY I#GFCT0L.

       COPY I#GFCT0K.

       COPY I#GFCT3O.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  IO-PCB.
           03  IO-TERM                 PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  IO-STA                  PIC  X(02).
           03  FILLER                  PIC  X(12).
           03  IO-MODNAME              PIC  X(08).

       01  ALT-PCB.
           03  ALT-PCBNAME             PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  ALT-STATUS              PIC  X(02).
           03  ALT-DATE                PIC S9(07) COMP-3.
           03  ALT-TIME                PIC S9(07) COMP-3.
           03  ALT-SEQNO               PIC S9(03) COMP.
           03  FILLER                  PIC  X(02).
           03  ALT-MODNAME             PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING IO-PCB
                                             ALT-PCB.

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
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
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1000-PROCESSAR.

           GO                          TO 0000-ROTINA-PRINCIPAL.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO
               WHEN 'GFCT0373'
                   MOVE GFCT0K-ENTRADA TO GFCTFR-ENTRADA
                   PERFORM 1100-PROCESSAR-SOLICITACAO

               WHEN 'GFCT0380'
                   MOVE GFCT0K-ENTRADA TO GFCTFT-ENTRADA
                   PERFORM 1200-PROCESSAR-CONSULTA

               WHEN 'GFCT0375'
                   MOVE GFCT0K-ENTRADA TO GFCTFV-ENTRADA
                   PERFORM 1300-PROCESSAR-EFETIVACAO

               WHEN 'GFCT5087'
                   MOVE GFCT0K-ENTRADA TO GFCTG5-ENTRADA
                   PERFORM 1400-PROCESSAR-TP-AGRUP

               WHEN 'GFCT5015'
                   MOVE GFCT0K-ENTRADA TO GFCT3A-ENTRADA
                   PERFORM 1500-PROCESSAR-TARIFA

               WHEN 'GFCT5068'
                   MOVE GFCT0K-ENTRADA TO GFCTCO-ENTRADA
                   PERFORM 1600-PROCESSAR-TP-OPER

               WHEN 'GFCT5014'
                   MOVE GFCT0K-ENTRADA TO GFCT3T-ENTRADA
                   PERFORM 1700-PROCESSAR-MOT-OPER

               WHEN 'GFCT5088'
                   MOVE GFCT0K-ENTRADA TO GFCTG7-ENTRADA
                   PERFORM 1800-PROCESSAR-SEGMENTOS

               WHEN 'GFCT5060'
                   MOVE GFCT0K-ENTRADA TO GFCTBR-ENTRADA
                   PERFORM 1900-PROCESSAR-AG-CONTA

               WHEN 'GFCT5006'
                   MOVE GFCT0K-ENTRADA TO GFCT1V-ENTRADA
                   PERFORM 2000-PROCESSAR-AGENCIA

               WHEN 'GFCT5109'
                   MOVE GFCT0K-ENTRADA TO GFCTGB-ENTRADA
                   PERFORM 2100-PROCESSAR-AG-POSTO

               WHEN 'GFCT5090'
                   MOVE GFCT0K-ENTRADA TO GFCTGB-ENTRADA
                   PERFORM 2200-PROCESSAR-AG-CORR-BCO

               WHEN 'GFCT5184'
                   MOVE GFCT0K-ENTRADA TO GFCTVP-ENTRADA
                   PERFORM 2300-PROCESSAR-COMBO-CONJ-SERV

               WHEN 'GFCT5186'
                   MOVE GFCT0K-ENTRADA TO GFCTVR-ENTRADA
                   PERFORM 2400-PROCESSAR-COMBO-TARIFA

           END-EVALUATE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-PROCESSAR-SOLICITACAO      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTFR-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTFS-SAIDA
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1110-MODULO-SOLICITACAO VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTFS-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-MODULO-SOLICITACAO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFR-ENTRADA
                                             GFCTFS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTFS-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1111-TRATAR-RETORNO-SOLIC.

           MOVE GFCTFS-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-TRATAR-RETORNO-SOLIC       SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTFS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1112-CHAMAR-SBAT3000            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL 'RSTA'
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
               IF  WRK-FUNCAO          EQUAL 'MSGI'
                   MOVE WRK-MSG02      TO GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL 'MSGF'
                       MOVE WRK-MSG03  TO GFCT0M-TEXTO
                   ELSE
                       MOVE WRK-MSG04  TO GFCT0M-TEXTO
                   END-IF
               END-IF
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-PROCESSAR-CONSULTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTFT-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTFU-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1210-MODULO-CONSULTA VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTFU-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-MODULO-CONSULTA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFT-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFT-ENTRADA
                                             GFCTFU-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTFU-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1211-TRATAR-RET-CONSULTA.

           MOVE GFCTFU-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTFU-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTFU-QTDE-OCOR   TO GFCTFT-QTDE-OCOR
               MOVE ZEROS              TO GFCTFT-QTDE-TOT-REG
               MOVE GFCTFU-PONT-AGPTO  TO GFCTFT-PONT-AGPTO
               MOVE GFCTFU-PONT-TARIFA TO GFCTFT-PONT-TARIFA
               MOVE GFCTFU-PONT-DATA   TO GFCTFT-PONT-DATA
               MOVE GFCTFU-FUNC-BDSCO  TO GFCTFT-FUNC-BDSCO
               MOVE GFCTFU-TERMINAL    TO GFCTFT-TERMINAL
               MOVE GFCTFU-FIM         TO GFCTFT-FIM
               MOVE GFCTFT-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1211-TRATAR-RET-CONSULTA        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTFU-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
               MOVE GFCTFU-PONT-AGPTO  TO GFCTFT-PONT-AGPTO
               MOVE GFCTFU-PONT-TARIFA TO GFCTFT-PONT-TARIFA
               MOVE GFCTFU-PONT-DATA   TO GFCTFT-PONT-DATA
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-PROCESSAR-EFETIVACAO       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTFV-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTFW-SAIDA
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1310-MODULO-EFETIVACAO VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTFW-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1310-MODULO-EFETIVACAO          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFV-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFV-ENTRADA
                                             GFCTFW-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTFW-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1311-TRATAR-RET-EFETIVACAO.

           MOVE GFCTFW-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1311-TRATAR-RET-EFETIVACAO      SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTFW-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-PROCESSAR-TP-AGRUP         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTG5-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTG6-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1410-MODULO-TP-AGRUP VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTG6-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1410-MODULO-TP-AGRUP            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTG5-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG5-ENTRADA
                                             GFCTG6-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTG6-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1411-TRATAR-RET-AGRUP.

           MOVE GFCTG6-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTG6-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTG6-FILTRO      TO GFCTG5-FILTRO
               MOVE GFCTG6-QTDE-OCOR   TO GFCTG5-QTDE-OCOR
               MOVE ZEROS              TO GFCTG5-QTDE-TOT-REG
               MOVE GFCTG6-PONTEIRO    TO GFCTG5-PONTEIRO
               MOVE GFCTG6-FUNC-BDSCO  TO GFCTG5-FUNC-BDSCO
               MOVE GFCTG6-TERMINAL    TO GFCTG5-TERMINAL
               MOVE GFCTG6-FIM         TO GFCTG5-FIM
               MOVE GFCTG5-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1411-TRATAR-RET-AGRUP           SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTG6-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
               MOVE GFCTG6-PONTEIRO    TO GFCTG5-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-PROCESSAR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3A-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3B-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1510-MODULO-TARIFA VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCT3B-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1510-MODULO-TARIFA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT3A-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT3A-ENTRADA
                                             GFCT3B-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCT3B-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1511-TRATAR-RET-TARIFA.

           MOVE GFCT3B-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCT3B-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCT3B-FILTRO      TO GFCT3A-FILTRO
               MOVE GFCT3B-QTDE-OCOR   TO GFCT3A-QTDE-OCOR
               MOVE ZEROS              TO GFCT3A-QTDE-TOT-REG
               MOVE GFCT3B-PONTEIRO    TO GFCT3A-PONTEIRO
               MOVE GFCT3B-FUNC-BDSCO  TO GFCT3A-FUNC-BDSCO
               MOVE GFCT3B-TERMINAL    TO GFCT3A-TERMINAL
               MOVE GFCT3B-FIM         TO GFCT3A-FIM
               MOVE GFCT3A-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1511-TRATAR-RET-TARIFA          SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCT3B-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
               MOVE GFCT3B-PONTEIRO    TO GFCT3A-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1600-PROCESSAR-TP-OPER          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCO-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTCP-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1610-MODULO-TP-OPER VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTCP-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1610-MODULO-TP-OPER             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTCO-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCO-ENTRADA
                                             GFCTCP-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTCP-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1611-TRATAR-RET-TP-OPER.

           MOVE GFCTCP-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTCP-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTCP-FILTRO      TO GFCTCO-FILTRO
               MOVE GFCTCP-QTDE-OCOR   TO GFCTCO-QTDE-OCOR
               MOVE ZEROS              TO GFCTCO-QTDE-TOT-REG
               MOVE GFCTCP-PONTEIRO    TO GFCTCO-PONTEIRO
               MOVE GFCTCP-FUNC-BDSCO  TO GFCTCO-FUNC-BDSCO
               MOVE GFCTCP-TERMINAL    TO GFCTCO-TERMINAL
               MOVE GFCTCP-FIM         TO GFCTCO-FIM
               MOVE GFCTCO-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1611-TRATAR-RET-TP-OPER         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTCP-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
               MOVE GFCTCP-PONTEIRO    TO GFCTCO-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1700-PROCESSAR-MOT-OPER         SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3T-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3U-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1710-MODULO-MOT-OPER VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCT3U-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1710-MODULO-MOT-OPER            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT3T-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT3T-ENTRADA
                                             GFCT3U-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCT3U-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1711-TRATAR-RET-MOT-OPER.

           MOVE GFCT3U-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCT3U-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCT3U-FILTRO      TO GFCT3T-FILTRO
               MOVE GFCT3U-QTDE-OCOR   TO GFCT3T-QTDE-OCOR
               MOVE ZEROS              TO GFCT3T-QTDE-TOT-REG
               MOVE GFCT3U-PONTEIRO    TO GFCT3T-PONTEIRO
               MOVE GFCT3U-FUNC-BDSCO  TO GFCT3T-FUNC-BDSCO
               MOVE GFCT3U-TERMINAL    TO GFCT3T-TERMINAL
               MOVE GFCT3U-FIM         TO GFCT3T-FIM
               MOVE GFCT3T-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1711-TRATAR-RET-MOT-OPER        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCT3U-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
               MOVE GFCT3U-PONTEIRO    TO GFCT3T-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1711-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1800-PROCESSAR-SEGMENTOS        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTG7-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTG8-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1810-MODULO-SEGMENTOS VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTG8-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1810-MODULO-SEGMENTOS           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTG7-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG7-ENTRADA
                                             GFCTG8-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTG8-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1811-TRATAR-RET-SEGMENTOS.

           MOVE GFCTG8-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTG8-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTG8-FILTRO      TO GFCTG7-FILTRO
               MOVE GFCTG8-QTDE-OCOR   TO GFCTG7-QTDE-OCOR
               MOVE ZEROS              TO GFCTG7-QTDE-TOT-REG
               MOVE GFCTG8-PONTEIRO    TO GFCTG7-PONTEIRO
               MOVE GFCTG8-FUNC-BDSCO  TO GFCTG7-FUNC-BDSCO
               MOVE GFCTG8-TERMINAL    TO GFCTG7-TERMINAL
               MOVE GFCTG8-FIM         TO GFCTG7-FIM
               MOVE GFCTG7-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1811-TRATAR-RET-SEGMENTOS       SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTG8-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
               MOVE GFCTG8-PONTEIRO    TO GFCTG7-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1811-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1900-PROCESSAR-AG-CONTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTBR-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT0M-AREA-ERROS
                                       GFCTBS-SAIDA
                                       GFCT3O-AREA-RESTART
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1910-MODULO-AG-CONTA VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTBS-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1910-MODULO-AG-CONTA            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTBR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTBS-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1911-TRATAR-RET-AG-CONTA.

           MOVE GFCTBS-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1911-TRATAR-RET-AG-CONTA        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTBS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1911-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           IF  GFCT1V-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT0M-AREA-ERROS
                                       GFCT1X-SAIDA
                                       GFCT3O-AREA-RESTART
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2010-MODULO-AGENCIA VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCT1X-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2010-MODULO-AGENCIA             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT1V-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCT1X-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 2011-TRATAR-RET-AGENCIA.

           MOVE GFCT1X-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2011-TRATAR-RET-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCT1X-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2011-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-PROCESSAR-AG-POSTO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGB-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTGC-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2110-MODULO-AG-POSTO VARYING WRK-BLOCO FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTGC-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-MODULO-AG-POSTO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGB-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGC-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 2111-TRATAR-RET-AG-POSTO.

           MOVE GFCTGC-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2111-TRATAR-RET-AG-POSTO        SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTGC-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-PROCESSAR-AG-CORR-BCO      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGB-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTGC-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2210-MODULO-AG-CORR-BCO
           VARYING WRK-BLOCO           FROM 1 BY 1
             UNTIL WRK-BLOCO           GREATER 3 OR
                   GFCTGC-FIM          EQUAL 'S'.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-MODULO-AG-CORR-BCO         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGB-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGC-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 2211-TRATAR-RET-AG-CORR-BCO

           MOVE GFCTGC-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2210-99-FIM.                     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2211-TRATAR-RET-AG-CORR-BCO     SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTGC-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-COMBO-CONJ-SERV  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVP-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTVQ-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2310-MODULO-CONJUNTO-SERVICO
             VARYING WRK-BLOCO         FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTVQ-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-MODULO-CONJUNTO-SERVICO    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVP-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTVP-ENTRADA
                                             GFCTVQ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTVQ-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 2311-TRATAR-RET-CONJ-SERV.

           MOVE GFCTVQ-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTVQ-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTVQ-FILTRO      TO GFCTVP-FILTRO
               MOVE GFCTVQ-QTDE-OCOR   TO GFCTVP-QTDE-OCOR
               MOVE ZEROS              TO GFCTVP-QTDE-TOT-REG
               MOVE GFCTVQ-FUNC-BDSCO  TO GFCTVP-FUNC-BDSCO
               MOVE GFCTVQ-PONTEIRO    TO GFCTVP-PONTEIRO
               MOVE GFCTVQ-FIM         TO GFCTVP-FIM
               MOVE GFCTVQ-TIPO-VIGENCIA
                                       TO GFCTVP-TIPO-VIGENCIA
               MOVE GFCTVQ-TIPO-DATA   TO GFCTVP-TIPO-DATA
               MOVE GFCTVP-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2311-TRATAR-RET-CONJ-SERV       SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTVQ-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB

               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF

               MOVE GFCTVQ-FILTRO      TO GFCTVP-FILTRO
               MOVE GFCTVQ-PONTEIRO    TO GFCTVP-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-PROCESSAR-COMBO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVR-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTVS-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
                                       WRK-BLOCO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2410-MODULO-TARIFA
             VARYING WRK-BLOCO         FROM 1 BY 1
               UNTIL WRK-BLOCO         GREATER 3 OR
                     GFCTVS-FIM        EQUAL 'S'.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-MODULO-TARIFA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTVR-ENTRADA
                                             GFCTVS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTVS-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 2411-TRATAR-RET-TARIFA.

           MOVE GFCTVS-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTVS-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTVS-FILTRO      TO GFCTVR-FILTRO
               MOVE GFCTVS-QTDE-OCOR   TO GFCTVR-QTDE-OCOR
               MOVE ZEROS              TO GFCTVR-QTDE-TOT-REG
               MOVE GFCTVS-FUNC-BDSCO  TO GFCTVR-FUNC-BDSCO
               MOVE GFCTVS-PONTEIRO    TO GFCTVR-PONTEIRO
               MOVE GFCTVS-FIM         TO GFCTVR-FIM
               MOVE GFCTVS-TIPO-VIGENCIA
                                       TO GFCTVR-TIPO-VIGENCIA
               MOVE GFCTVS-TIPO-DATA   TO GFCTVR-TIPO-DATA
               MOVE GFCTVR-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1112-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2411-TRATAR-RET-TARIFA          SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO GFCT0L-NRO-MSGI.

           IF  GFCTVS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB

               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF

               MOVE GFCTVS-FILTRO      TO GFCTVR-FILTRO
               MOVE GFCTVS-PONTEIRO    TO GFCTVR-PONTEIRO
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7372'             TO GFCT0M-TRANSACAO
                                          GFCT0M-PROGRAMA.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7372'             TO GFCT0M-TRANSACAO.
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
