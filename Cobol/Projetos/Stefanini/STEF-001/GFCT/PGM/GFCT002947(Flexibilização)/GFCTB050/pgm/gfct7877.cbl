      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7877.
       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7877                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS BOSCATO           - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCKWORK PG 50   *
      *    DATA........:   23/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO ENTRE WEB E MF VIA      *
      *      SBAT3000 PARA AS CHAMADAS NOS MODULOS  REFERENTES  A      *
      *      FUNCIONALIDADE DE DEFERIMENTO DE FLEXIBILIZACAO  POR      *
      *      AGENCIA E CONTA.                                          *
      *----------------------------------------------------------------*
      *    INC'S ......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM SBAT3000                 *
      *    I#GFCT0K - AREA DE COMUNICACAO COM SBAT3000 - ENTRADA       *
      *    I#GFCT0L - AREA DE COMUNICACAO COM SBAT3000 - SAIDA         *
      *    I#GFCT0M - AREA DE COMUNICACAO COM SBAT3000 - ERROS         *
      *    I#GFCT3O - AREA DE COMUNICACAO COM SBAT3000 - RESTART       *
      *    I#GFCTKV - AREA DE COMUNICACAO COM GFCT0424 - ENTRADA       *
      *    I#GFCTKX - AREA DE COMUNICACAO COM GFCT0424 - SAIDA         *
      *    I#GFCTKY - AREA DE COMUNICACAO COM GFCT0875 - ENTRADA       *
JUN10 *    GFCTWANQ - AREA DE COMUNICACAO COM GFCT0875 - SAIDA         *
      *    I#GFCTL1 - AREA DE COMUNICACAO COM GFCT0876 - ENTRADA       *
      *    I#GFCTL2 - AREA DE COMUNICACAO COM GFCT0876 - SAIDA         *
JUN10 *    GFCTWANS - AREA DE COMUNICACAO COM GFCT0877 - ENTRADA       *
JUN10 *    GFCTWANP - AREA DE COMUNICACAO COM GFCT0877 - SAIDA         *
JUN10 *    GFCTWAKX - AREA DE COMUNICACAO COM GFCT0991 - ENTRADA       *
JUN10 *    GFCTWAKY - AREA DE COMUNICACAO COM GFCT0991 - SAIDA         *
      *    I#GFCTK1 - AREA DE COMUNICACAO COM GFCT5116 - ENTRADA       *
      *    I#GFCTK2 - AREA DE COMUNICACAO COM GFCT5116 - SAIDA         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT0424 - CONSULTA DEFERIMENTO DE FLEXIBILIZACAO POR AG/CTA*
      *    GFCT0875 - CONSULTA DEFERIMENTO DETALHE                     *
      *    GFCT0876 - DEFERIMENTO DE FLEXIBILIZACAO                    *
JUN10 *    GFCT0877 - CONSULTA LISTA TARIFA                            *
      *    GFCT5116 - PESQUISA NOME CLIENTE                            *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS                    *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 31/10/2007 - VALERIA                    *
      *  - SUBSTITUICAO DO MODULO GFCT5515 PELO GFCT5578 PARA DEPTO    *
      *    AUTORIZADO.                                                 *
      *  - SUBSTITUICAO DO MODULO GFCT5516 PELO GFCT5580 PARA DEPTO    *
      *    AUTORIZADO.                                                 *
      *  - SUBSTITUICAO DO MODULO GFCT5537 PELO GFCT5579 PARA DEPTO    *
      *    AUTORIZADO.                                                 *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 10/02/2010 - HELIO SANTONI              *
      *  - INCLUIR COMBO DE CONJUNTO DE SERVICO GFCT5211.              *
      *  - INCLUIR COMBO DE LOTE                GFCT5213.              *
      *  - BOTAO VISUALIZAR IMPRESAO            GFCT0991.              *
      *================================================================*
JUN10 *================================================================*
JUN10 *                U L T I M A   A L T E R A C A O                 *
JUN10 *================================================================*
JUN10 *    PROGRAMADOR.:   FABRICA                  - SONDA PROCWORK   *
JUN10 *    ANALISTA....:   CIBELE BARBOSA           - SONDA PROCWORK   *
JUN10 *    DATA........:   JUN/2010                                    *
JUN10 *----------------------------------------------------------------*
JUN10 *    OBJETIVO....:   INCLUIR FUNCIONALIDADE ABAIXO:              *
JUN10 *                    - CONSULTA LISTA DE TARIFAS                 *
JUN10 *                    OBS.: O TRATAMENTO DO PGM GFCT0875 FOI DIVI-*
JUN10 *                    DIDO EM 2 PGMS.                             *
JUN10 *                    - GFCT0875 (PARTE FIXA)                     *
JUN10 *                    - GFCT0877 (PARTE VARIAVEL)                 *
JUN10 *================================================================*
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
       01  WRK-GFCT0L-NRO-MSGI         PIC  9(002) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
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
           03  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           03  WRK-MODULO-MSG          PIC  X(008)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INCLUDES DO PROGRAMA ***'.
      *---------------------------------------------------------------*

       COPY 'I#ISDDPC'.

       COPY 'I#GFCT0K'.

       COPY 'I#GFCT0L'.

       COPY 'I#GFCT0M'.

       COPY 'I#GFCTKV'.

       COPY 'I#GFCTKX'.

       COPY 'I#GFCTKY'.

JUN10  COPY 'GFCTWANQ'.

JUN10  COPY 'GFCTWANR'.

JUN10  COPY 'GFCTWANU'.

       COPY 'I#GFCTK1'.

       COPY 'I#GFCTK2'.

       COPY 'GFCTWAKZ'.

       COPY 'GFCTWAK0'.

       COPY 'GFCTWAK1'.

       COPY 'GFCTWAK2'.

       COPY 'GFCTWAKX'.

       COPY 'GFCTWAKY'.

JUN10  COPY 'GFCTWANS'.
JUN10
JUN10  COPY 'GFCTWANP'.
JUN10
       COPY 'I#GFCT3O'.

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

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           INITIALIZE      WAKZ-ENTRADA
                           WAK0-SAIDA
                           WAK1-ENTRADA
                           WAK2-SAIDA
                           WAKX-ENTRADA
                           WAKY-SAIDA.

           PERFORM 1000-RECEBER-MENSAGEM.

           PERFORM 2000-PROCESSAR.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

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

      *----------------------------------------------------------------*
       1100-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7877'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

               WHEN 'GFCT0424'
                   MOVE GFCT0K-ENTRADA TO GFCTKV-ENTRADA
                   PERFORM 2100-LISTA-CONSULTA-DEF
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKX-FIM    EQUAL 'S'

               WHEN 'GFCT0875'
                   MOVE GFCT0K-ENTRADA TO GFCTKY-ENTRADA
                   PERFORM 2200-CONSULTA-DEF-DETALHE

               WHEN 'GFCT0876'
JUN10              MOVE GFCT0K-ENTRADA(1:500) TO GFCTNR-ENTRADA
                   PERFORM 2300-PROC-DEFERIMENTO-FLEX

               WHEN 'GFCT5116'
                   MOVE GFCT0K-ENTRADA TO GFCTK1-ENTRADA
                   PERFORM 2400-PROCESSA-NOME-CLIENTE
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTK2-FIM    EQUAL 'S'

               WHEN 'GFCT5211'
                   MOVE GFCT0K-ENTRADA (1:200) TO WAKZ-ENTRADA
                   PERFORM 2500-PROCESSA-COMBO-CCJTO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         WAK0-FIM      EQUAL 'S'

               WHEN 'GFCT5213'
                   MOVE GFCT0K-ENTRADA (1:100) TO WAK1-ENTRADA
                   PERFORM 2600-PROCESSA-COMBO-LOTE
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         WAK2-FIM      EQUAL 'S'

               WHEN 'GFCT0991'
JUN10              MOVE GFCT0K-ENTRADA (1:300) TO WAKX-ENTRADA
                   PERFORM 2700-BOTAO-IMPRESSAO

JUN10          WHEN 'GFCT0877'
JUN10              MOVE GFCT0K-ENTRADA(1:200) TO GFCTNS-ENTRADA
JUN10              PERFORM 2800-LISTA-CONSULTA-TARIFA
JUN10                  VARYING IND-1   FROM 1 BY 1
JUN10                    UNTIL IND-1   GREATER 3 OR
JUN10                    GFCTNP-FIM    EQUAL 'S'
JUN10
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LISTA-CONSULTA-DEF         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKV-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKX-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKX-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKV-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKV-ENTRADA
                                             GFCTKX-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKX-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2110-TRATA-RET-CONSULTA-DEF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-TRATA-RET-CONSULTA-DEF     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKX-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTKX-QTDE-OCOR
                                       TO GFCTKV-QTDE-OCOR
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2111-MOVER-CAMPOS-01
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE GFCTKX-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2111-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE GFCTKX-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTKX-FILTRO          TO GFCTKV-FILTRO.
           MOVE GFCTKX-QTDE-OCOR       TO GFCTKV-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTKV-QTDE-TOT-REG.
           MOVE GFCTKX-FUNC-BDSCO      TO GFCTKV-FUNC-BDSCO.
           MOVE GFCTKX-PONTEIRO        TO GFCTKV-PONTEIRO.
           MOVE GFCTKX-FIM             TO GFCTKV-FIM.
           MOVE GFCTKV-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

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

      *----------------------------------------------------------------*
       2200-CONSULTA-DEF-DETALHE       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKY-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNQ-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
JUN10                                  GFCTNQ-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKY-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKY-ENTRADA
JUN10                                        GFCTNQ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

JUN10      IF  GFCTNQ-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

JUN10      MOVE GFCTNQ-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2210-TRATAR-RETORNO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-TRATAR-RETORNO             SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO GFCT0L-NRO-MSGI.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROC-DEFERIMENTO-FLEX      SECTION.
      *----------------------------------------------------------------*

JUN10      IF  GFCTNR-FIM              EQUAL 'N'
JUN10          MOVE SPACES             TO GFCTNU-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
JUN10                                  GFCTNU-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
JUN10      MOVE GFCTNR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
JUN10                                        GFCTNR-ENTRADA
JUN10                                        GFCTNU-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

JUN10      IF  GFCTNU-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

JUN10      MOVE GFCTNU-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2210-TRATAR-RETORNO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-PROCESSA-NOME-CLIENTE      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTK1-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTK2-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTK2-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTK1-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTK1-ENTRADA
                                             GFCTK2-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTK2-COD-RETORNO      EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2410-TRATA-RET-NOME-CLIENTE

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-TRATA-RET-NOME-CLIENTE     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTK2-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
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
           MOVE GFCTK2-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2411-MOVER-CAMPOS-02            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE GFCTK2-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTK2-QTDE-OCOR       TO GFCTK1-QTDE-OCOR.
           MOVE GFCTK2-QTDE-TOT-REG    TO GFCTK1-QTDE-TOT-REG.
           MOVE GFCTK2-FUNC-BDSCO      TO GFCTK1-FUNC-BDSCO.
           MOVE GFCTK2-FIM             TO GFCTK1-FIM.
           MOVE GFCTK1-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   PROCESSA COMBO DE CONJUNTO DE SERVICO                        *
      *----------------------------------------------------------------*
       2500-PROCESSA-COMBO-CCJTO       SECTION.
      *----------------------------------------------------------------*

           IF  WAKZ-FIM                EQUAL 'N'
               MOVE SPACES             TO WAK0-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       WAK0-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAKZ-FUNCAO            TO WRK-MODULO.

           CALL  WRK-MODULO          USING   WAKZ-ENTRADA
                                             WAK0-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAK0-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2510-TRATA-RET-COMBO-CCJTO

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   TRATA RETORNO DA COMBO DE CONJUNTO DE SERVICO                *
      *----------------------------------------------------------------*
       2510-TRATA-RET-COMBO-CCJTO      SECTION.
      *----------------------------------------------------------------*

           IF  WAK0-FIM             EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2511-MOVER-CAMPOS-02
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-GFCT0L-NRO-MSGI
               ADD  1                  TO WRK-GFCT0L-NRO-MSGI
               MOVE WRK-GFCT0L-NRO-MSGI
                                       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAK0-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   MOVE-CAMPOS-DE-RESTARTAAAA                                   *
      *----------------------------------------------------------------*
       2511-MOVER-CAMPOS-02            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAK0-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAK0-QTDE-OCOR         TO WAKZ-QTDE-OCOR.
           MOVE WAK0-QTDE-TOT-REG      TO WAKZ-QTDE-TOT-REG.
           MOVE WAK0-FUNC-BDSCO        TO WAKZ-FUNC-BDSCO.
           MOVE WAK0-FIM               TO WAKZ-FIM.
           MOVE WAKZ-DADOS-RST         TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   PROCESSA COMBO DE CONJUNTO DE SERVICO                        *
      *----------------------------------------------------------------*
       2600-PROCESSA-COMBO-LOTE        SECTION.
      *----------------------------------------------------------------*

           IF  WAK1-FIM                EQUAL 'N'
               MOVE SPACES             TO WAK2-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       WAK2-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAK1-FUNCAO            TO WRK-MODULO.

           CALL  WRK-MODULO          USING   WAK1-ENTRADA
                                             WAK2-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAK2-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2610-TRATA-RET-COMBO-LOTE

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   TRATA RETORNO DA COMBO DE LOTE                               *
      *----------------------------------------------------------------*
       2610-TRATA-RET-COMBO-LOTE       SECTION.
      *----------------------------------------------------------------*

           IF  WAK2-FIM             EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2611-MOVER-CAMPOS-02
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-GFCT0L-NRO-MSGI
               ADD  1                  TO WRK-GFCT0L-NRO-MSGI
               MOVE WRK-GFCT0L-NRO-MSGI
                                       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
           MOVE WAK2-SAIDA             TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   MOVEACAMPOS-DE-RESTARTAAAA                                   *
      *----------------------------------------------------------------*
       2611-MOVER-CAMPOS-02            SECTION.
      *----------------------------------------------------------------*

           MOVE 256                    TO GFCT3O-LL.
           MOVE WAK2-FUNCAO            TO GFCT3O-FUNCAO.
           MOVE WAK2-QTDE-OCOR         TO WAK1-QTDE-OCOR.
           MOVE WAK2-QTDE-TOT-REG      TO WAK1-QTDE-TOT-REG.
           MOVE WAK2-FUNC-BDSCO        TO WAK1-FUNC-BDSCO.
           MOVE WAK2-FIM               TO WAK1-FIM.
           MOVE WAK1-DADOS-RST         TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-BOTAO-IMPRESSAO            SECTION.
      *----------------------------------------------------------------*

           IF  WAKY-FIM             EQUAL 'N'
               MOVE SPACES             TO WAKY-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       WAKY-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAKX-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO          USING WAKX-ENTRADA
                                          WAKY-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAKY-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WAKY-SAIDA             TO GFCT0L-ENTRADA.

           PERFORM 2210-TRATAR-RETORNO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

JUN10 *----------------------------------------------------------------*
JUN10  2800-LISTA-CONSULTA-TARIFA      SECTION.
JUN10 *----------------------------------------------------------------*
JUN10
JUN10      IF  GFCTNS-FIM              EQUAL 'N'
JUN10          MOVE SPACES             TO GFCTNP-SAIDA
JUN10                                     GFCT0M-AREA-ERROS
JUN10                                     GFCT3O-AREA-RESTART
JUN10          INITIALIZE              GFCT0L-RET-WEB
JUN10                                  GFCTNP-SAIDA
JUN10                                  GFCT3O-AREA-RESTART
JUN10                                  GFCT0M-AREA-ERROS
JUN10      END-IF.
JUN10
JUN10      MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
JUN10      MOVE GFCTNS-FUNCAO          TO WRK-MODULO.
JUN10
JUN10      CALL WRK-MODULO             USING GFCTNS-ENTRADA
JUN10                                        GFCTNP-SAIDA
JUN10                                        GFCT0M-AREA-ERROS.
JUN10
JUN10      IF  RETURN-CODE             NOT EQUAL ZEROS
JUN10          MOVE WRK-MODULO         TO WRK-MODULO-MSG
JUN10          MOVE WRK-MSG05          TO GFCT0M-TEXTO
JUN10          PERFORM 1100-FORMATAR-ERRO
JUN10          PERFORM 9999-PROCESSAR-ROTINA-ERRO
JUN10      END-IF.
JUN10
JUN10      IF  GFCTNP-ERRO             EQUAL 9
JUN10          PERFORM 9999-PROCESSAR-ROTINA-ERRO
JUN10      END-IF.
JUN10
JUN10      PERFORM 2810-TRATA-RET-CONSULTA-TARIFA.
JUN10
JUN10      PERFORM 2120-RETORNAR-WEB.
JUN10
JUN10 *----------------------------------------------------------------*
JUN10  2800-99-FIM.                    EXIT.
JUN10 *----------------------------------------------------------------*
JUN10
JUN10 *----------------------------------------------------------------*
JUN10  2810-TRATA-RET-CONSULTA-TARIFA  SECTION.
JUN10 *----------------------------------------------------------------*
JUN10
JUN10      IF  GFCTNP-FIM              EQUAL 'S'
JUN10          MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
JUN10      ELSE
JUN10          IF  IND-1               LESS 3
JUN10              MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
JUN10              MOVE GFCTNP-QTDE-OCOR
JUN10                                  TO GFCTNS-QTDE-OCOR
JUN10          ELSE
JUN10              MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
JUN10              PERFORM 2811-MOVER-CAMPOS-01
JUN10          END-IF
JUN10      END-IF.
JUN10
JUN10      IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
JUN10          MOVE ZEROS              TO GFCT0L-NRO-MSGI
JUN10      ELSE
JUN10          MOVE GFCT0L-NRO-MSGI    TO WRK-GFCT0L-NRO-MSGI
JUN10          ADD  1                  TO WRK-GFCT0L-NRO-MSGI
JUN10          MOVE WRK-GFCT0L-NRO-MSGI
JUN10                                  TO GFCT0L-NRO-MSGI
JUN10      END-IF.
JUN10
JUN10      MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
JUN10      MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
JUN10      MOVE GFCTNP-SAIDA           TO GFCT0L-ENTRADA.
JUN10
JUN10 *----------------------------------------------------------------*
JUN10  2810-99-FIM.                    EXIT.
JUN10 *----------------------------------------------------------------*
JUN10
JUN10 *----------------------------------------------------------------*
JUN10  2811-MOVER-CAMPOS-01            SECTION.
JUN10 *----------------------------------------------------------------*
JUN10
JUN10      MOVE 256                    TO GFCT3O-LL.
JUN10      MOVE GFCTNP-FUNCAO          TO GFCT3O-FUNCAO.
JUN10      MOVE GFCTNP-FILTRO          TO GFCTNS-FILTRO.
JUN10      MOVE GFCTNP-QTDE-OCOR       TO GFCTNS-QTDE-OCOR.
JUN10      MOVE ZEROS                  TO GFCTNS-QTDE-TOT-REG.
JUN10      MOVE GFCTNP-FUNC-BDSCO      TO GFCTNS-FUNC-BDSCO.
JUN10      MOVE GFCTNP-PONTEIRO        TO GFCTNS-PONTEIRO.
JUN10      MOVE GFCTNP-FIM             TO GFCTNS-FIM.
JUN10      MOVE GFCTNS-DADOS-RST       TO GFCT3O-DADOS-RST.
JUN10
JUN10 *----------------------------------------------------------------*
JUN10  2811-99-FIM.                    EXIT.
JUN10 *----------------------------------------------------------------*
JUN10
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7877'             TO GFCT0M-PROGRAMA.
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
