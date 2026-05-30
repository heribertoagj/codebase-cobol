      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7381.
       AUTHOR. LUCIANO HERMES.
      *================================================================*
      *                   C P M  S I S T E M A S                       *
      *================================================================*
      *                                                                *
      *    PROGRAMA     : GFCT7381                                     *
      *    PROGRAMADOR  : LUCIANO HERMES                - CPM/FPOLIS   *
      *    ANALISTA CPM : MICHELLI VENTURA BORGES       - CPM/FPOLIS   *
      *    ANALISTA     : MARCELO  -  PROCWORK          - GRUPO 50     *
      *    DATA         : 31/10/2005                                   *
      *                                                                *
      *    OBJETIVOS    :                                              *
      *      EFETUAR COMUNICACAO ENTRE WEB E MF VIA SBAT3000 PARA AS   *
      *      CHAMADAS NOS MODULOS REFERENTES A FUNCIONALIDADE DE       *
      *      EFETIVACAO DE DEFERIMENTO E CONSULTA DE ESTORNO POR       *
      *      AGRUPAMENTO.                                              *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *      SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS.                 *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                  *
      *      GFCT0382 - LISTA DE ESTORNOS SOLICITADOS.                 *
      *      GFCT0376 - EFETIVA O DEFERIMENTO OU CONSULTA.             *
      *      GFCT5500 - VERIFICACAO DE DISPONIBILIDADE ON-LINE.        *
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
           '*  INICIO DA WORKING GFCT7381  *'.
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
       01  FILLER                      PIC  X(51)          VALUE
           '* AREA DE COM. - TEXT - LISTA ESTORNO AGRUPAMENTO *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGH.
       COPY I#GFCTGI.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(70)          VALUE
           '* AREA DE COM. - TEXT - CONS/EFET DEFERIMENTO ESTORNO AGRUPA
      -    'MENTO    *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGJ.
       COPY I#GFCTGK.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38)          VALUE
           '* AREA DE COM. - TIPO DE AGRUPAMENTO *'.
      *----------------------------------------------------------------*

       COPY I#GFCTGR.
       COPY I#GFCTGS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38)          VALUE
           '* AREA DE COM. - COMBO TARIFAS *'.
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
               WHEN 'GFCT0382'
                   MOVE GFCT0K-ENTRADA TO GFCTGH-ENTRADA
                   PERFORM 1100-PROCESSAR-CONSULTA

               WHEN 'GFCT0376'
                   MOVE GFCT0K-ENTRADA TO GFCTGJ-ENTRADA
                   PERFORM 1200-PROCESSAR-EFETIVACAO

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
       1100-PROCESSAR-CONSULTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGH-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGI-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3O-AREA-RESTART
                                       GFCTGI-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1110-CHAMAR-GFCT0382
                                       VARYING WRK-BLOCO FROM 1 BY 1
                                       UNTIL   WRK-BLOCO GREATER 3 OR
                                       GFCTGI-FIM EQUAL 'S'.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-CHAMAR-GFCT0382            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGH-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGH-ENTRADA
                                             GFCTGI-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGI-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1111-TRATAR-RET-CONSULTA.

           PERFORM 1112-RETORNAR-WEB-CONSULTA.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1111-TRATAR-RET-CONSULTA        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGI-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE GFCTGI-FILTRO      TO GFCTGH-FILTRO
               MOVE GFCTGI-PONTEIRO    TO GFCTGH-PONTEIRO

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
       1112-RETORNAR-WEB-CONSULTA      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGI-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 256                TO GFCT3O-LL
               MOVE GFCTGI-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTGI-QTDE-OCOR   TO GFCTGH-QTDE-OCOR
               MOVE ZEROS              TO GFCTGH-QTDE-TOT-REG
               MOVE GFCTGI-FUNC-BDSCO  TO GFCTGH-FUNC-BDSCO
               MOVE GFCTGI-FILTRO      TO GFCTGH-FILTRO
               MOVE GFCTGI-PONTEIRO    TO GFCTGH-PONTEIRO
               MOVE GFCTGI-FIM         TO GFCTGH-FIM
               MOVE GFCTGH-DADOS-RST   TO GFCT3O-DADOS-RST
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
       1200-PROCESSAR-EFETIVACAO       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGJ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGK-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTGK-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1210-CHAMAR-EFETIVACAO
                                       VARYING WRK-BLOCO FROM 1 BY 1
                                       UNTIL   WRK-BLOCO GREATER 3 OR
                                       GFCTGK-FIM EQUAL 'S'.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-CHAMAR-EFETIVACAO          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGJ-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGJ-ENTRADA
                                             GFCTGK-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTGK-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1211-TRATAR-RETORNO-EFETIV.

           MOVE GFCTGK-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 1212-CHAMAR-SBAT3000-EFETIV.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1211-TRATAR-RETORNO-EFETIV      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGK-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               IF  WRK-BLOCO           LESS 3
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
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1212-CHAMAR-SBAT3000-EFETIV     SECTION.
      *----------------------------------------------------------------*

           CALL 'SBAT3000'             USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA

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
       1212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-PROCESSAR-TIPO-AGRUP       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGS-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTGS-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1310-CHAMAR-GFCT5092
                                       VARYING WRK-BLOCO FROM 1 BY 1
                                       UNTIL   WRK-BLOCO GREATER 3 OR
                                       GFCTGS-FIM EQUAL 'S'.


      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1310-CHAMAR-GFCT5092            SECTION.
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

           PERFORM 1311-TRATAR-RET-TIPO-AGRUP.

           PERFORM 1312-RETORNAR-WEB-TIPO-AGRUP.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1311-TRATAR-RET-TIPO-AGRUP      SECTION.
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
       1312-RETORNAR-WEB-TIPO-AGRUP    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTGS-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 256                TO GFCT3O-LL
               MOVE GFCTGS-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTGS-QTDE-OCOR   TO GFCTGR-QTDE-OCOR
               MOVE ZEROS              TO GFCTGR-QTDE-TOT-REG
               MOVE GFCTGS-FUNC-BDSCO  TO GFCTGR-FUNC-BDSCO
               MOVE GFCTGS-TERMINAL    TO GFCTGR-TERMINAL
               MOVE GFCTGS-PONTEIRO    TO GFCTGR-PONTEIRO
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
                                       GFCTGU-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1410-CHAMAR-GFCT5093
                                       VARYING WRK-BLOCO FROM 1 BY 1
                                       UNTIL   WRK-BLOCO GREATER 3 OR
                                       GFCTGU-FIM EQUAL 'S'.


      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1410-CHAMAR-GFCT5093            SECTION.
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
               MOVE GFCTGU-QTDE-OCOR   TO GFCTGT-QTDE-OCOR
               MOVE ZEROS              TO GFCTGT-QTDE-TOT-REG
               MOVE GFCTGU-FUNC-BDSCO  TO GFCTGT-FUNC-BDSCO
               MOVE GFCTGU-TERMINAL    TO GFCTGT-TERMINAL
               MOVE GFCTGU-PONTEIRO    TO GFCTGT-PONTEIRO
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
           MOVE 'GFCT7381'             TO GFCT0M-TRANSACAO
                                          GFCT0M-PROGRAMA.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7381'             TO GFCT0M-TRANSACAO.
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
