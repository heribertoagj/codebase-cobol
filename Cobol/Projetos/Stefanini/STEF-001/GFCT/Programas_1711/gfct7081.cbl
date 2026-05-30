      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7081.
       AUTHOR. MARTINS.
      *================================================================*
      *                   C P M  S I S T E M A S                       *
      *================================================================*
      *                                                                *
      *    PROGRAMA     : GFCT7081                                     *
      *    PROGRAMADOR  : MARCELO RODRIGO MARTINS       - CPM/FPOLIS   *
      *    ANALISTA CPM : SILVANA FERREIRA MACHADO      - CPM/FPOLIS   *
      *    ANALISTA     : MARCELO - PROCWORK            - GRUPO 50     *
      *    DATA         : 20/07/2005                                   *
      *                                                                *
      *    OBJETIVOS    :                                              *
      *      EFETUAR COMUNICACAO ENTRE WEB E MF VIA SBAT3000 PARA AS   *
      *      CHAMADAS NOS MODULOS REFERENTES A FUNCIONALIDADE HIERAR-  *
      *      QUIA EXCECAO.                                             *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT0082 - INCLUSAO  DE HIERARQUIA EXCECAO.               *
      *      GFCT0084 - ALTERACAO DE HIERARQUIA EXCECAO.               *
      *      GFCT0085 - EXCLUSAO  DE HIERARQUIA EXCECAO.               *
      *      GFCT0083 - CONSULTA  DE HIERARQUIA EXCECAO.               *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                *
      *      SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS.                 *
      *      GFCT5068 - TIPOS DE OPERACAO          ( CO CP )           *
      *      GFCT5065 - TARIFAS                    ( CI CJ )           *
      *      GFCT5052 - TIPO DEPENDENCIAS          ( BA BB )           *
      *      GFCT5006 - DEPENDENCIA - CJUNC        ( 1V 1X )           *
      *      GFCT5062 - TIPO OPER. CADASTRADA      ( CC CD )           *
      *      GFCT5063 - TARIFA CADASTRADA          ( CE CF )           *
      *      GFCT5064 - TIPO DEP. CADASTRADA       ( CG CH )           *
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
       77  FILLER                      PIC  X(032)         VALUE
           '*  INICIO DA WORKING GFCT7081  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)         VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       77  IND-CE                      PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)         VALUE
           '*         AUXILIARES           *'.
      *----------------------------------------------------------------*

       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       77  WRK-ERRO                    PIC  X(004)         VALUE 'ERRO'.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-BLOCO                   PIC  9(003)         VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*          MENSAGENS           *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC  X(075)         VALUE
               'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
           03  WRK-MSG02               PIC  X(075)         VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           03  WRK-MSG03               PIC  X(075)         VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           03  WRK-MSG04               PIC  X(075)         VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           03  WRK-MSG05.
               05  FILLER              PIC  X(026)         VALUE
                   'ERRO NA CHAMADA AO MODULO '.
               05  WRK-MODULO-MSG      PIC  X(008)         VALUE SPACES.
               05  FILLER              PIC  X(041)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(035)         VALUE
           '* AREA TEXT - HIERARQUIA EXCECAO *'.
      *----------------------------------------------------------------*

-INC I#GFCT9K
            COPY 'GFCTW09L'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA COMBO - TIPO OPERACOES *'.
      *----------------------------------------------------------------*

-INC I#GFCTCO
-INC I#GFCTCP

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA COMBO - TARIFAS        *'.
      *----------------------------------------------------------------*

-INC I#GFCTCI
-INC I#GFCTCJ

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(038)         VALUE
           '* AREA COMBO - TIPO DE DEPENDENCIAS *'.
      *----------------------------------------------------------------*

-INC I#GFCTBA
-INC I#GFCTBB

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* AREA TEXT - DEPENDENCIA     *'.
      *----------------------------------------------------------------*

-INC I#GFCT1V
-INC I#GFCT1X

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(047)         VALUE
           '* AREA COMBO - TIPO DE OPERACOES CADASTRADAS *'.
      *----------------------------------------------------------------*

-INC I#GFCTCC
-INC I#GFCTCD

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(037)         VALUE
           '* AREA COMBO - TARIFAS CADASTRADAS *'.
      *----------------------------------------------------------------*

-INC I#GFCTCE
-INC I#GFCTCF

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(045)         VALUE
           '* AREA COMBO - TIPO DE DEPEND. CADASTRADAS *'.
      *----------------------------------------------------------------*

-INC I#GFCTCG
-INC I#GFCTCH

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(044)         VALUE
           '* AREA DE COMUNICACAO COM O ISD (SBAT3000) *'.
      *----------------------------------------------------------------*

-INC I#ISDDPC

-INC I#GFCT0K
-INC I#GFCT0L
-INC I#GFCT0M
-INC I#GFCT3O

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

      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL         SECTION.
      *----------------------------------------------------------------*

           PERFORM 010000-RECEBER-MENSAGEM.

           EVALUATE GFCT0K-FUNCAO
               WHEN 'GFCT0082'
               WHEN 'GFCT0084'
               WHEN 'GFCT0085'
                   PERFORM 020000-PROCESSAR-IE

               WHEN 'GFCT0083'
                   MOVE GFCT0K-ENTRADA  TO GFCT9K-ENTRADA
                   PERFORM 030000-PROCESSAR-CONSULTA
                       VARYING IND-CE   FROM 1 BY 1
                       UNTIL GFCT9L-FIM OF GFCT9L-DADOS-RSTA
                                        EQUAL  'S'
                        OR   IND-CE     GREATER 3

               WHEN 'GFCT5068'
                   MOVE GFCT0K-ENTRADA  TO GFCTCO-ENTRADA
                   PERFORM 040000-PROCESSAR-TIP-OPER
                       VARYING IND-CE   FROM 1 BY 1
                       UNTIL GFCTCP-FIM EQUAL  'S'
                        OR   IND-CE     GREATER 3

               WHEN 'GFCT5065'
                   MOVE GFCT0K-ENTRADA  TO GFCTCI-ENTRADA
                   PERFORM 050000-PROCESSAR-COMBO-TARIFA
                       VARYING IND-CE   FROM 1 BY 1
                       UNTIL GFCTCJ-FIM EQUAL  'S'
                        OR   IND-CE     GREATER 3

               WHEN 'GFCT5052'
                   MOVE GFCT0K-ENTRADA  TO GFCTBA-ENTRADA
                   PERFORM 060000-PROCESSAR-TIP-DEPEND
                       VARYING IND-CE   FROM 1 BY 1
                       UNTIL GFCTBB-FIM EQUAL  'S'
                        OR   IND-CE     GREATER 3

               WHEN 'GFCT5006'
                   PERFORM 070000-PROCESSAR-DEPENDENCIA

               WHEN 'GFCT5062'
                   MOVE GFCT0K-ENTRADA  TO GFCTCC-ENTRADA
                   PERFORM 080000-PROCESSAR-TIP-OPER-CAD
                       VARYING IND-CE   FROM 1 BY 1
                       UNTIL GFCTCD-FIM EQUAL  'S'
                        OR   IND-CE     GREATER 3

               WHEN 'GFCT5063'
                   MOVE GFCT0K-ENTRADA  TO GFCTCE-ENTRADA
                   PERFORM 090000-PROCESSAR-TARIF-CAD
                       VARYING IND-CE   FROM 1 BY 1
                       UNTIL GFCTCF-FIM EQUAL  'S'
                        OR   IND-CE     GREATER 3

               WHEN 'GFCT5064'
                   MOVE GFCT0K-ENTRADA  TO GFCTCG-ENTRADA
                   PERFORM 100000-PROCESSAR-TIP-DEP-SUB
                       VARYING IND-CE   FROM 1 BY 1
                       UNTIL GFCTCH-FIM EQUAL  'S'
                        OR   IND-CE     GREATER 3
           END-EVALUATE.

           GO                          TO 000000-ROTINA-PRINCIPAL.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       010000-RECEBER-MENSAGEM         SECTION.
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
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE ZEROS                  TO WRK-BLOCO.
           MOVE SPACES                 TO GFCT9L-FIM OF
                                          GFCT9L-DADOS-RSTA
                                          GFCTCP-FIM GFCTCH-FIM
                                          GFCTCJ-FIM GFCTBB-FIM
                                          GFCTCD-FIM GFCTCF-FIM.

      *----------------------------------------------------------------*
       010000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       020000-PROCESSAR-IE             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0K-ENTRADA         TO GFCT9K-ENTRADA.

           IF  GFCT9K-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCT9L-SAIDA
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCT9K-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT9K-ENTRADA
                                             GFCT9L-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT9L-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO

           IF  GFCT9L-FIM OF GFCT9L-DADOS-RSTA
                                       EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
               END-IF
           END-IF.

           MOVE GFCT9L-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCT9L-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 021000-RETORNAR-WEB-IE.

      *----------------------------------------------------------------*
       020000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       021000-RETORNAR-WEB-IE          SECTION.
      *----------------------------------------------------------------*

           CALL 'SBAT3000'             USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA.

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
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       021000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       030000-PROCESSAR-CONSULTA       SECTION.
      *----------------------------------------------------------------*

           IF  GFCT9K-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCT9L-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCT9K-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT9K-ENTRADA
                                             GFCT9L-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT9L-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO.

           IF  GFCT9L-FIM OF GFCT9L-DADOS-RSTA
                                       EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               MOVE GFCT9L-DT-RESTART  TO GFCT9K-DT-RST
               MOVE GFCT9L-CTPO-OP-RST TO GFCT9K-CTPO-OP-RST
               MOVE GFCT9L-TARIF-RST   TO GFCT9K-TARIF-RST
               MOVE GFCT9L-TDEP-SUBOR-RST
                                       TO GFCT9K-TDEP-SUBOR-RST
               IF  WRK-BLOCO           EQUAL 3
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
                   MOVE 255            TO GFCT3O-LL
                   MOVE GFCT9L-FUNCAO  TO GFCT3O-FUNCAO
                   MOVE GFCT9L-QTDE-OCOR
                                       TO GFCT9K-QTDE-OCOR
                   MOVE ZEROS          TO GFCT9K-QTDE-TOT-REG
                   MOVE GFCT9L-FUNC-BDSCO
                                       TO GFCT9K-FUNC-BDSCO
                   MOVE GFCT9L-TERMINAL
                                       TO GFCT9K-TERMINAL
                   MOVE GFCT9L-FIM OF GFCT9L-DADOS-RSTA
                                       TO GFCT9K-FIM
                   MOVE GFCT9K-DADOS-RSTA
                                       TO GFCT3O-DADOS-RST
               ELSE
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF

           MOVE GFCT9L-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCT9L-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 031000-RETORNAR-WEB.

      *----------------------------------------------------------------*
       030000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       031000-RETORNAR-WEB             SECTION.
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
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       031000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       040000-PROCESSAR-TIP-OPER       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCO-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCTCP-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCTCO-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCO-ENTRADA
                                             GFCTCP-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTCP-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO.

           IF  GFCTCP-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               MOVE GFCTCP-PONTEIRO    TO GFCTCO-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
                   MOVE 255            TO GFCT3O-LL
                   MOVE GFCTCP-FUNCAO  TO GFCT3O-FUNCAO
                   MOVE GFCTCP-FILTRO  TO GFCTCO-FILTRO
                   MOVE GFCTCP-QTDE-OCOR
                                       TO GFCTCO-QTDE-OCOR
                   MOVE ZEROS          TO GFCTCO-QTDE-TOT-REG
                   MOVE GFCTCP-FUNC-BDSCO
                                       TO GFCTCO-FUNC-BDSCO
                   MOVE GFCTCP-TERMINAL
                                       TO GFCTCO-TERMINAL
                   MOVE GFCTCP-FIM     TO GFCTCO-FIM
                   MOVE GFCTCO-DADOS-RST
                                       TO GFCT3O-DADOS-RST
               ELSE
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF

           MOVE GFCTCP-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCTCP-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 031000-RETORNAR-WEB.

      *----------------------------------------------------------------*
       040000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       050000-PROCESSAR-COMBO-TARIFA   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCI-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCTCJ-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCTCI-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCI-ENTRADA
                                             GFCTCJ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTCJ-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO.

           IF  GFCTCJ-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               MOVE GFCTCJ-PONTEIRO    TO GFCTCI-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
                   MOVE 255            TO GFCT3O-LL
                   MOVE GFCTCJ-FUNCAO  TO GFCT3O-FUNCAO
                   MOVE GFCTCJ-QTDE-OCOR
                                       TO GFCTCI-QTDE-OCOR
                   MOVE ZEROS          TO GFCTCI-QTDE-TOT-REG
                   MOVE GFCTCJ-FUNC-BDSCO
                                       TO GFCTCI-FUNC-BDSCO
                   MOVE GFCTCJ-TERMINAL
                                       TO GFCTCI-TERMINAL
                   MOVE GFCTCJ-FIM     TO GFCTCI-FIM
                   MOVE GFCTCI-DADOS-RST
                                       TO GFCT3O-DADOS-RST
               ELSE
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF

           MOVE GFCTCJ-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCTCJ-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 031000-RETORNAR-WEB.

      *----------------------------------------------------------------*
       050000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       060000-PROCESSAR-TIP-DEPEND     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTBA-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCTBB-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCTBA-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTBA-ENTRADA
                                             GFCTBB-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTBB-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO.

           IF  GFCTBB-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               MOVE GFCTBB-PONTEIRO    TO GFCTBA-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
                   MOVE 255            TO GFCT3O-LL
                   MOVE GFCTBB-FUNCAO  TO GFCT3O-FUNCAO
                   MOVE GFCTBB-QTDE-OCOR
                                       TO GFCTBA-QTDE-OCOR
                   MOVE ZEROS          TO GFCTBA-QTDE-TOT-REG
                   MOVE GFCTBB-FUNC-BDSCO
                                       TO GFCTBA-FUNC-BDSCO
                   MOVE GFCTBB-TERMINAL
                                       TO GFCTBA-TERMINAL
                   MOVE GFCTBB-FIM     TO GFCTBA-FIM
                   MOVE GFCTBA-DADOS-RST
                                       TO GFCT3O-DADOS-RST
               ELSE
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF

           MOVE GFCTBB-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCTBB-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 031000-RETORNAR-WEB.

      *----------------------------------------------------------------*
       060000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       070000-PROCESSAR-DEPENDENCIA    SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0K-ENTRADA         TO GFCT1V-ENTRADA.

           IF  GFCT1V-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCT1X-SAIDA
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCT1V-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT1X-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO

           IF  GFCT1X-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               IF  WRK-BLOCO           LESS 3
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
               END-IF
           END-IF.

           MOVE GFCT1X-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCT1X-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 021000-RETORNAR-WEB-IE.

      *----------------------------------------------------------------*
       070000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       080000-PROCESSAR-TIP-OPER-CAD   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCC-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCTCD-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCTCC-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCC-ENTRADA
                                             GFCTCD-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTCD-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO.

           IF  GFCTCD-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               MOVE GFCTCD-PONTEIRO    TO GFCTCC-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
                   MOVE 255            TO GFCT3O-LL
                   MOVE GFCTCD-FUNCAO  TO GFCT3O-FUNCAO
                   MOVE GFCTCD-QTDE-OCOR
                                       TO GFCTCC-QTDE-OCOR
                   MOVE ZEROS          TO GFCTCC-QTDE-TOT-REG
                   MOVE GFCTCD-FUNC-BDSCO
                                       TO GFCTCC-FUNC-BDSCO
                   MOVE GFCTCD-TERMINAL
                                       TO GFCTCC-TERMINAL
                   MOVE GFCTCD-FIM     TO GFCTCC-FIM
                   MOVE GFCTCC-DADOS-RST
                                       TO GFCT3O-DADOS-RST
               ELSE
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF

           MOVE GFCTCD-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCTCD-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 031000-RETORNAR-WEB.

      *----------------------------------------------------------------*
       080000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       090000-PROCESSAR-TARIF-CAD      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCE-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCTCF-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCTCE-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCE-ENTRADA
                                             GFCTCF-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTCF-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO.

           IF  GFCTCF-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               MOVE GFCTCF-PONTEIRO    TO GFCTCE-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
                   MOVE 255            TO GFCT3O-LL
                   MOVE GFCTCF-FUNCAO  TO GFCT3O-FUNCAO
                   MOVE GFCTCF-QTDE-OCOR
                                       TO GFCTCE-QTDE-OCOR
                   MOVE ZEROS          TO GFCTCE-QTDE-TOT-REG
                   MOVE GFCTCF-FUNC-BDSCO
                                       TO GFCTCE-FUNC-BDSCO
                   MOVE GFCTCF-TERMINAL
                                       TO GFCTCE-TERMINAL
                   MOVE GFCTCF-FIM     TO GFCTCE-FIM
                   MOVE GFCTCE-DADOS-RST
                                       TO GFCT3O-DADOS-RST
               ELSE
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF

           MOVE GFCTCF-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCTCF-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 031000-RETORNAR-WEB.

      *----------------------------------------------------------------*
       090000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-PROCESSAR-TIP-DEP-SUB    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCG-FIM              EQUAL 'N'
               INITIALIZE GFCT0L-RET-WEB
                          GFCTCH-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE GFCTCG-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCG-ENTRADA
                                             GFCTCH-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT7081'         TO GFCT0M-PROGRAMA
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTCH-ERRO             EQUAL 9
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           ADD 1                       TO WRK-BLOCO.

           IF  GFCTCH-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
               MOVE ZEROS              TO WRK-BLOCO
           ELSE
               MOVE GFCTCH-PONTEIRO    TO GFCTCG-PONTEIRO
               IF  WRK-BLOCO           EQUAL 3
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
                   MOVE ZEROS          TO WRK-BLOCO
                   MOVE 255            TO GFCT3O-LL
                   MOVE GFCTCH-FUNCAO  TO GFCT3O-FUNCAO
                   MOVE GFCTCH-QTDE-OCOR
                                       TO GFCTCG-QTDE-OCOR
                   MOVE ZEROS          TO GFCTCG-QTDE-TOT-REG
                   MOVE GFCTCH-FUNC-BDSCO
                                       TO GFCTCG-FUNC-BDSCO
                   MOVE GFCTCH-TERMINAL
                                       TO GFCTCG-TERMINAL
                   MOVE GFCTCH-FIM     TO GFCTCG-FIM
                   MOVE GFCTCG-DADOS-RST
                                       TO GFCT3O-DADOS-RST
               ELSE
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF

           MOVE GFCTCH-SAIDA           TO GFCT0L-ENTRADA.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-BLOCO              TO GFCT0L-NRO-MSGI.
           MOVE GFCTCH-LL              TO GFCT0L-LL.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

           PERFORM 031000-RETORNAR-WEB.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7081'             TO GFCT0M-TRANSACAO.
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
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
