      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7385.
       AUTHOR. TIAGO.
      *================================================================*
      *                   C P M  S I S T E M A S                       *
      *================================================================*
      *                                                                *
      *    PROGRAMA     : GFCT7385                                     *
      *    PROGRAMADOR  : TIAGO PACHECO                 - CPM/FPOLIS   *
      *    ANALISTA CPM : RODRIGO WILLAIM               - CPM/FPOLIS   *
      *    ANALISTA     : MARCELO  -  PROCWORK          - GRUPO 50     *
      *    DATA         : 28/10/2005                                   *
      *                                                                *
      *    OBJETIVOS    :                                              *
      *      EFETUAR COMUNICACAO ENTRE WEB E MF VIA SBAT3000 PARA AS   *
      *      CHAMADAS NOS MODULOS REFERENTES A FUNCIONALIDADE CONSULTA *
      *      DE ESTORNO POR AGRUPAMENTO.                               *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *      SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS.                 *
      *      GFCT0386 - LISTA DE ESTORNOS SOLICITADOS.                 *
      *      GFCT0378 - CONSULTA DE ESTORNO.                           *
      *      GFCT5092 - COMBO DE TIPO DE AGRUPAMENTOS.                 *
      *      GFCT5093 - COMBO DE TARIFAS.                              *
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
           '*  INICIO DA WORKING GFCT7385  *'.
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
           '* AREA DE COM. - LISTA ESTORNO AGRUP *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGL.
       COPY I#GFCTGM.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(39)          VALUE
           '* AREA DE COM. - CONSULTA ESTORNO AG *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGN.
       COPY I#GFCTGO.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(39)          VALUE
           '* AREA DE COM. - TIPO DE AGRUPAMENTO *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGR.
       COPY I#GFCTGS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* AREA DE COM. - TARIFAS       *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGT.
       COPY I#GFCTGU.

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
               WHEN 'GFCT0386'
                   MOVE GFCT0K-ENTRADA TO GFCTGL-ENTRADA
                   PERFORM 1100-PROCESSAR-LISTA

               WHEN 'GFCT0378'
                   MOVE GFCT0K-ENTRADA TO GFCTGN-ENTRADA
                   PERFORM 1200-PROCESSAR-CONSULTA

               WHEN 'GFCT5092'
                   MOVE GFCT0K-ENTRADA TO GFCTGR-ENTRADA
                   PERFORM 1300-PROCESSAR-TIPO-AGRUP

               WHEN 'GFCT5093'
                   MOVE GFCT0K-ENTRADA TO GFCTGT-ENTRADA
                   PERFORM 1400-PROCESSAR-TARIFA

           END-EVALUATE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-PROCESSAR-LISTA            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGL-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGM-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3O-AREA-RESTART
                                       GFCTGM-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1110-MODULO-LISTA   VARYING WRK-BLOCO FROM 1 BY 1
                                       UNTIL   WRK-BLOCO GREATER 3 OR
                                       GFCTGM-FIM EQUAL 'S'.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-MODULO-LISTA               SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGL-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGL-ENTRADA
                                             GFCTGM-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGM-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1111-TRATAR-RET-LISTA.

           PERFORM 1112-RETORNAR-WEB-LISTA.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-TRATAR-RET-LISTA           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGM-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE GFCTGM-FILTRO      TO GFCTGL-FILTRO
               MOVE GFCTGM-PONTEIRO    TO GFCTGL-PONTEIRO

               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1112-RETORNAR-WEB-LISTA         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGM-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 256                TO GFCT3O-LL
               MOVE GFCTGM-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTGM-QTDE-OCOR   TO GFCTGL-QTDE-OCOR
               MOVE ZEROS              TO GFCTGL-QTDE-TOT-REG
               MOVE GFCTGM-FUNC-BDSCO  TO GFCTGL-FUNC-BDSCO
               MOVE GFCTGM-FILTRO      TO GFCTGL-FILTRO
               MOVE GFCTGM-PONTEIRO    TO GFCTGL-PONTEIRO
               MOVE GFCTGM-FIM         TO GFCTGL-FIM
               MOVE GFCTGL-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1113-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1112-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1113-CHAMAR-SBAT3000            SECTION.
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
       1113-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-PROCESSAR-CONSULTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGN-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGO-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3O-AREA-RESTART
                                       GFCTGO-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1210-MODULOS-CONSULTA.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-MODULOS-CONSULTA           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGN-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGN-ENTRADA
                                             GFCTGO-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGO-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1211-TRATAR-RET-CONSULTA.

           MOVE GFCTGO-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1113-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1211-TRATAR-RET-CONSULTA        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.
           MOVE ZEROS                  TO GFCT0L-NRO-MSGI.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-PROCESSAR-TIPO-AGRUP       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGS-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3O-AREA-RESTART
                                       GFCTGS-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1310-MODULO-TIPO-AG VARYING WRK-BLOCO FROM 1 BY 1
                                       UNTIL   WRK-BLOCO GREATER 3 OR
                                       GFCTGS-FIM EQUAL 'S'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1310-MODULO-TIPO-AG             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGR-ENTRADA
                                             GFCTGS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGS-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1311-TRATAR-RET-TIPO-AG.

           PERFORM 1312-RETORNAR-WEB-TIPO-AG.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1311-TRATAR-RET-TIPO-AG         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE GFCTGS-PONTEIRO    TO GFCTGR-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1312-RETORNAR-WEB-TIPO-AG       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGS-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 256                TO GFCT3O-LL
               MOVE GFCTGS-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTGS-FILTRO      TO GFCTGR-FILTRO
               MOVE GFCTGS-QTDE-OCOR   TO GFCTGR-QTDE-OCOR
               MOVE ZEROS              TO GFCTGR-QTDE-TOT-REG
               MOVE GFCTGS-PONTEIRO    TO GFCTGR-PONTEIRO
               MOVE GFCTGS-FUNC-BDSCO  TO GFCTGR-FUNC-BDSCO
               MOVE GFCTGS-TERMINAL    TO GFCTGR-TERMINAL
               MOVE GFCTGS-FIM         TO GFCTGR-FIM
               MOVE GFCTGR-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1113-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1312-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-PROCESSAR-TARIFA           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGT-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGU-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3O-AREA-RESTART
                                       GFCTGU-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1410-MODULO-TARIFA  VARYING WRK-BLOCO FROM 1 BY 1
                                       UNTIL   WRK-BLOCO GREATER 3 OR
                                       GFCTGU-FIM EQUAL 'S'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1410-MODULO-TARIFA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGT-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGT-ENTRADA
                                             GFCTGU-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGU-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1411-TRATAR-RET-TARIFA.

           PERFORM 1412-RETORNAR-WEB-TARIFA.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1411-TRATAR-RET-TARIFA          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGU-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE GFCTGU-PONTEIRO    TO GFCTGT-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1412-RETORNAR-WEB-TARIFA        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGU-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 256                TO GFCT3O-LL
               MOVE GFCTGU-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTGU-FILTRO      TO GFCTGT-FILTRO
               MOVE GFCTGU-QTDE-OCOR   TO GFCTGT-QTDE-OCOR
               MOVE ZEROS              TO GFCTGT-QTDE-TOT-REG
               MOVE GFCTGU-PONTEIRO    TO GFCTGT-PONTEIRO
               MOVE GFCTGU-FUNC-BDSCO  TO GFCTGT-FUNC-BDSCO
               MOVE GFCTGU-TERMINAL    TO GFCTGT-TERMINAL
               MOVE GFCTGU-FIM         TO GFCTGT-FIM
               MOVE GFCTGT-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 1113-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1412-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7385'             TO GFCT0M-TRANSACAO
                                          GFCT0M-PROGRAMA.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7385'             TO GFCT0M-TRANSACAO.
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
