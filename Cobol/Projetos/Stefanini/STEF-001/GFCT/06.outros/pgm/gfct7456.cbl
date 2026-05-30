      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7456.
       AUTHOR.     MARCELO FARIA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7456                                    *
      *    PROGRAMADOR.:   MARCELO FARIA           - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE GAZZONI        - CPM PATO BRANCO   *
      *    ANALISTA....:   RENATO TAMANAHA         - PROCKWORK / GP 50 *
      *    DATA........:   22/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO  ENTRE  WEB  E  MF  VIA *
      *       SBAT3000 PARA AS CHAMADAS NOS MODULOS REFERENTES A  FUN- *
      *       CIONALIDADE UNIFICADA DE DEBITO PARA CPF/CNPJ.           *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM O SBAT3000               *
      *    I#GFCTG2 - AREA DE MENSAGEM DO SISTEMA                      *
      *    I#GFCTG3 - AREA DE MENSAGEM DO SISTEMA                      *
      *    I#GFCTIU - AREA DE TEXT NOME DO CLIENTE                     *
      *    I#GFCTIV - AREA DE TEXT NOME DO CLIENTE                     *
      *    I#GFCT3A - AREA DE COMBO DE TARIFAS                         *
      *    I#GFCT3B - AREA DE COMBO DE TARIFAS                         *
      *    I#GFCTIQ - AREA DE INCLUSAO UNIFICADA                       *
      *    I#GFCTIR - AREA DE INCLUSAO UNIFICADA                       *
      *    I#GFCTFZ - AREA DE SISTEMA DISPONIVEL                       *
      *    I#GFCTG1 - AREA DE SISTEMA DISPONIVEL                       *
      *    I#GFCTIS - AREA DE CANCELAMENTO/CONSULTA UNIFICADA          *
      *    I#GFCTIT - AREA DE CANCELAMENTO/CONSULTA UNIFICADA          *
      *    I#GFCT0K - AREA DE COMUNICACAO-SBAT3000 - ENTRADA           *
      *    I#GFCT0L - AREA DE COMUNICACAO-SBAT3000 - SAIDA             *
      *    I#GFCT0M - AREA DE COMUNICACAO-SBAT3000 - ERROS             *
      *    I#GFCT3O - AREA DE COMUNICACAO-RESTART                      *
      *    POL7100C - AREA DE TRATAMENTO DE ERRO PADRAO BRADESCO       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - MODULO PARA ALOCACAO DINAMICA                    *
      *    GFCT0457 - INCLUSAO UNIFICADA DE DEBITO                     *
      *    GFCT0464 - CANCELAMENTO UNIFICADA DE DEBITO                 *
      *    GFCT0465 - CONSULTA UNIFICADA DE DEBITO                     *
      *    GFCT5110 - TEXT PESQUISA NOME CLIENTE PELO CPF/CNPJ         *
      *    GFCT5522 - SISTEMA DISPONIVEL                               *
      *    GFCT5523 - OBTER DESCRICAO DA MENSAGEM                      *
      *    SBAT3000 - MODULO DE COMUNICACAO ENTRE PLATAFORMAS          *
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
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT7456 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO SBAT3000 ***'.
      *----------------------------------------------------------------*

       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       77  WRK-ERRO                    PIC  X(004)         VALUE 'ERRO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE SPACES.

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
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
               'ERRO NA CHAMADA DO MODULO'.
           03  WRK-MODULO-MSG          PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INC ***'.
      *----------------------------------------------------------------*

       COPY 'I#ISDDPC'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCTIU'.

       COPY 'I#GFCTIV'.

       COPY 'I#GFCT3A'.

       COPY 'I#GFCT3B'.

       COPY 'I#GFCTIQ'.

       COPY 'I#GFCTIR'.

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTIS'.

       COPY 'I#GFCTIT'.

       COPY 'I#GFCT0K'.

       COPY 'I#GFCT0L'.

       COPY 'I#GFCT0M'.

       COPY 'I#GFCT3O'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT7456 - FIM DA AREA DE WORKING ***'.
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

           ENTRY 'DLITCBL'             USING IO-PCB ALT-PCB.

      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-GU-MSG-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GU                 TO WRK-FUNCAO.

           CALL  'SBAT3000'            USING WRK-FUNCAO
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

           PERFORM 1000-VERIFICAR-DISPONIBILIDADE.

           PERFORM 2000-ROTINA-PRINCIPAL.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************

      ******************************************************************
      * ROTINA PARA VERIFICAR DISPONIBILIDADE ATRAVES DO MODULO        *
      *    GFCT5522.                                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-VERIFICAR-DISPONIBILIDADE  SECTION.
      *----------------------------------------------------------------*

           MOVE  +100                  TO GFCTFZ-LL.
           MOVE  ZEROS                 TO GFCTFZ-ZZ.
           MOVE  GFCT0K-TRANSACAO      TO GFCTFZ-TRANSACAO.
           MOVE  GFCT0K-FUNCAO         TO GFCTFZ-FUNCAO.
           MOVE  ZEROS                 TO GFCTFZ-FUNC-BDSCO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE 'N'                    TO GFCTFZ-FIM.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
                                          GFCT0M-PROGRAMA

               MOVE  WRK-MSG05         TO GFCT0M-TEXTO

               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE  WRK-MODULO    TO GFCT0M-PROGRAMA
                   PERFORM 1100-FORMATAR-ERRO
               ELSE
                   MOVE '02'           TO GFCT0L-MPARCODE-APLIC
               END-IF

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE  WRK-MODULO        TO GFCT0M-PROGRAMA

               PERFORM 1100-FORMATAR-ERRO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FORMATACAO DE ERRO.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7456'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAMENTO PRINCIPAL DO PROGRAMA.               *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           EVALUATE GFCT0K-FUNCAO
               WHEN 'GFCT0457'
                   MOVE GFCT0K-ENTRADA TO GFCTIQ-ENTRADA
                   PERFORM 2010-PROCESSAR-INCLUSAO
                   PERFORM 2020-CHAMAR-MODULOS-INCLUSAO

               WHEN 'GFCT0464'
                   MOVE GFCT0K-ENTRADA TO GFCTIS-ENTRADA
                   PERFORM 2030-PROCESSAR-CANCELA
                   PERFORM 2040-CHAMAR-MODULOS-CANCELA

               WHEN 'GFCT0465'
                   MOVE GFCT0K-ENTRADA TO GFCTIS-ENTRADA
                   PERFORM 2050-PROCESSAR-CONSULTA
                   PERFORM
                       VARYING IND-1      FROM 1 BY 1
                       UNTIL   IND-1      GREATER 3 OR
                               GFCTIT-FIM EQUAL 'S'
                               PERFORM 2060-CHAMAR-MODULOS-CONSULTA
                   END-PERFORM

               WHEN 'GFCT5110'
                   MOVE GFCT0K-ENTRADA TO GFCTIU-ENTRADA
                   PERFORM 2070-PROCESSAR-TEXT-NOME-CLI
                   PERFORM
                       VARYING IND-1      FROM 1 BY 1
                       UNTIL   IND-1      GREATER 3 OR
                               GFCTIV-FIM EQUAL 'S'
                               PERFORM 2080-CHAMAR-MODULOS-NOME-CLI
                   END-PERFORM

               WHEN 'GFCT5015'
                   MOVE GFCT0K-ENTRADA TO GFCT3A-ENTRADA
                   PERFORM 2090-PROCESSAR-COMBO-TARIFA
                   PERFORM
                       VARYING IND-1      FROM 1 BY 1
                       UNTIL   IND-1      GREATER 3 OR
                               GFCT3B-FIM EQUAL 'S'
                               PERFORM 2100-CHAMAR-MODULOS-TARIFA
                   END-PERFORM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR INCLUSAO.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2010-PROCESSAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIQ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTIR-SAIDA
                                          GFCT0M-AREA-ERROS

               INITIALIZE GFCT0L-RET-WEB
                          GFCTIR-SAIDA
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO  GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CHAMAR MODULO GFCT0457 PARA INCLUSAO.              *
      ******************************************************************
      *----------------------------------------------------------------*
       2020-CHAMAR-MODULOS-INCLUSAO    SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT0K-FUNCAO         TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIQ-ENTRADA
                                             GFCTIR-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTIR-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2021-TRATAR-RETORNO-INCLUSAO.

           PERFORM 2022-RETORNAR-WEB-IE.

      *----------------------------------------------------------------*
       2020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO DO MODULO DE INCLUSAO.              *
      ******************************************************************
      *----------------------------------------------------------------*
       2021-TRATAR-RETORNO-INCLUSAO    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIR-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR 'MSGF'
               MOVE  ZEROS             TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE  GFCT0L-TPOMSG-WEB     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2021-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO PARA WEB.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2022-RETORNAR-WEB-IE            SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTIR-SAIDA          TO GFCT0L-ENTRADA.

           CALL  'SBAT3000'            USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               EVALUATE WRK-FUNCAO
                   WHEN 'MSGI'
                       MOVE  WRK-MSG02 TO GFCT0M-TEXTO
                   WHEN 'MSGF'
                       MOVE  WRK-MSG03 TO GFCT0M-TEXTO
                   WHEN OTHER
                       MOVE  WRK-MSG04 TO GFCT0M-TEXTO
               END-EVALUATE
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2022-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR CANCELAMENTO.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2030-PROCESSAR-CANCELA          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIS-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTIT-SAIDA
                                          GFCT3O-AREA-RESTART
                                          GFCT0M-AREA-ERROS

               INITIALIZE GFCT0L-RET-WEB
                          GFCTIT-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO  GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       2030-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CHAMADAR MODULO GFCT0464 PARA CANCELAMENTO.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2040-CHAMAR-MODULOS-CANCELA     SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT0K-FUNCAO         TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIS-ENTRADA
                                             GFCTIT-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTIR-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2041-TRATAR-RETORNO-CANCELA.

           PERFORM 2042-RETORNAR-WEB-CANCELA.

      *----------------------------------------------------------------*
       2040-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO DO MODULO DE CANCELAMENTO.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2041-TRATAR-RETORNO-CANCELA     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIT-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1                LESS 3
                   MOVE GFCTIT-PONTEIRO TO GFCTIS-PONTEIRO
                   MOVE 'MSGI'          TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR 'MSGF'
               MOVE  ZEROS             TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE  GFCT0L-TPOMSG-WEB     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2041-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO WEB PARA MODULO DE CANCELAMENTO.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2042-RETORNAR-WEB-CANCELA       SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTIT-SAIDA          TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE  255               TO GFCT3O-LL
               MOVE  GFCTIT-FUNCAO     TO GFCT3O-FUNCAO
               MOVE  GFCTIT-FILTRO     TO GFCTIS-FILTRO
               MOVE  GFCTIT-NOME       TO GFCTIS-NOME
               MOVE  GFCTIT-QTDE-OCOR  TO GFCTIS-QTDE-OCOR
               MOVE  ZEROS             TO GFCTIS-QTDE-TOT-REG
               MOVE  GFCTIT-FUNC-BDSCO TO GFCTIS-FUNC-BDSCO
               MOVE  GFCTIT-TERMINAL   TO GFCTIS-TERMINAL
               MOVE  GFCTIT-PONTEIRO   TO GFCTIS-PONTEIRO
               MOVE  GFCTIT-DPTO       TO GFCTIS-DPTO
               MOVE  GFCTIT-MASTER     TO GFCTIS-MASTER
               MOVE  GFCTIT-ACAO       TO GFCTIS-ACAO
               MOVE  GFCTIT-FIM        TO GFCTIS-FIM
               MOVE  GFCTIS-DADOS-RST  TO GFCT3O-DADOS-RST
               CALL  'SBAT3000'        USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
                                             GFCT3O-AREA-RESTART
           ELSE
               CALL  'SBAT3000'        USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               EVALUATE WRK-FUNCAO
                   WHEN 'MSGI'
                       MOVE  WRK-MSG02 TO GFCT0M-TEXTO
                   WHEN 'MSGF'
                       MOVE  WRK-MSG03 TO GFCT0M-TEXTO
                   WHEN OTHER
                       MOVE  WRK-MSG04 TO GFCT0M-TEXTO
               END-EVALUATE
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2042-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR CONSULTA.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2050-PROCESSAR-CONSULTA         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIS-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTIT-SAIDA
                                          GFCT3O-AREA-RESTART
                                          GFCT0M-AREA-ERROS

               INITIALIZE GFCT0L-RET-WEB
                          GFCTIT-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO  GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CHAMAR MODULO GFCT0465 PARA CONSULTA.              *
      ******************************************************************
      *----------------------------------------------------------------*
       2060-CHAMAR-MODULOS-CONSULTA    SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT0K-FUNCAO         TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIS-ENTRADA
                                             GFCTIT-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTIT-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2061-TRATAR-RETORNO-CONSULTA.

           PERFORM 2062-RETORNAR-WEB-CONSULTA.

      *----------------------------------------------------------------*
       2060-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO DO MODULO DE CONSULTA.              *
      ******************************************************************
      *----------------------------------------------------------------*
       2061-TRATAR-RETORNO-CONSULTA    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIT-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE GFCTIT-PONTEIRO TO GFCTIS-PONTEIRO
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR 'MSGF'
               MOVE  ZEROS             TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE  GFCT0L-TPOMSG-WEB     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2061-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO WEB DO MODULO DE CONSULTA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2062-RETORNAR-WEB-CONSULTA      SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTIT-SAIDA              TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO                  EQUAL 'RSTA'
               MOVE  255                   TO GFCT3O-LL
               MOVE  GFCTIT-FUNCAO         TO GFCT3O-FUNCAO
               MOVE  GFCTIT-FILTRO         TO GFCTIS-FILTRO
               MOVE  GFCTIT-NOME           TO GFCTIS-NOME
               MOVE  GFCTIT-QTDE-OCOR      TO GFCTIS-QTDE-OCOR
               MOVE  ZEROS                 TO GFCTIS-QTDE-TOT-REG
               MOVE  GFCTIT-FUNC-BDSCO     TO GFCTIS-FUNC-BDSCO
               MOVE  GFCTIT-TERMINAL       TO GFCTIS-TERMINAL
               MOVE  GFCTIT-PONTEIRO       TO GFCTIS-PONTEIRO
               MOVE  GFCTIT-DPTO           TO GFCTIS-DPTO
               MOVE  GFCTIT-MASTER         TO GFCTIS-MASTER
               MOVE  GFCTIT-ACAO           TO GFCTIS-ACAO
               MOVE  GFCTIT-FIM            TO GFCTIS-FIM
               MOVE  GFCTIS-DADOS-RST      TO GFCT3O-DADOS-RST
               CALL  'SBAT3000'            USING WRK-FUNCAO
                                                 IO-PCB
                                                 ALT-PCB
                                                 MCA-ISDPA
                                                 GFCT0L-ENTRADA
                                                 GFCT3O-AREA-RESTART
           ELSE
               CALL  'SBAT3000'            USING WRK-FUNCAO
                                                 IO-PCB
                                                 ALT-PCB
                                                 MCA-ISDPA
                                                 GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               EVALUATE WRK-FUNCAO
                   WHEN 'MSGI'
                       MOVE  WRK-MSG02 TO GFCT0M-TEXTO
                   WHEN 'MSGF'
                       MOVE  WRK-MSG03 TO GFCT0M-TEXTO
                   WHEN OTHER
                       MOVE  WRK-MSG04 TO GFCT0M-TEXTO
               END-EVALUATE
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2062-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR NOME DO CLIENTE.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2070-PROCESSAR-TEXT-NOME-CLI    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIU-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTIV-SAIDA
                                          GFCT3O-AREA-RESTART
                                          GFCT0M-AREA-ERROS

               INITIALIZE GFCT0L-RET-WEB
                          GFCTIV-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO  GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       2070-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CHAMAR MODULO GFCT5110 PARA NOME CLIENTE.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2080-CHAMAR-MODULOS-NOME-CLI    SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT0K-FUNCAO         TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTIV-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2081-TRATAR-RETORNO-NOME-CLIE.

           PERFORM 2082-RETORNAR-WEB-NOME-CLIENTE.

      *----------------------------------------------------------------*
       2080-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO DO MODULO DE NOME CLIENTE.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2081-TRATAR-RETORNO-NOME-CLIE   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTIV-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR 'MSGF'
               MOVE  ZEROS             TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE  GFCT0L-TPOMSG-WEB     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2081-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO WEB DO MODULO NOME CLIENTE.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2082-RETORNAR-WEB-NOME-CLIENTE  SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTIV-SAIDA              TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO                  EQUAL 'RSTA'
               MOVE  255                   TO GFCT3O-LL
               MOVE  GFCTIV-FUNCAO         TO GFCT3O-FUNCAO
               MOVE  GFCTIV-QTDE-OCOR      TO GFCTIU-QTDE-OCOR
               MOVE  ZEROS                 TO GFCTIU-QTDE-TOT-REG
               MOVE  GFCTIV-FUNC-BDSCO     TO GFCTIU-FUNC-BDSCO
               MOVE  GFCTIV-FIM            TO GFCTIU-FIM
ST2506*        MOVE  GFCTIV-NRO-CPF-CNPJ   TO GFCTIU-NRO-CPF-CNPJ
ST2506*        MOVE  GFCTIV-NRO-FILIAL     TO GFCTIU-NRO-FILIAL
ST2506         MOVE  GFCTIV-COD-CPF-CNPJ   TO GFCTIU-COD-CPF-CNPJ
ST2506         MOVE  GFCTIV-COD-FILIAL     TO GFCTIU-COD-FILIAL
               MOVE  GFCTIV-CTRL-CPF-CNPJ  TO GFCTIU-CTRL-CPF-CNPJ
               MOVE  GFCTIU-DADOS-RST      TO GFCT3O-DADOS-RST
               CALL 'SBAT3000'             USING WRK-FUNCAO
                                                 IO-PCB
                                                 ALT-PCB
                                                 MCA-ISDPA
                                                 GFCT0L-ENTRADA
                                                 GFCT3O-AREA-RESTART
           ELSE
               CALL  'SBAT3000'            USING WRK-FUNCAO
                                                 IO-PCB
                                                 ALT-PCB
                                                 MCA-ISDPA
                                                 GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               EVALUATE WRK-FUNCAO
                   WHEN 'MSGI'
                       MOVE  WRK-MSG02 TO GFCT0M-TEXTO
                   WHEN 'MSGF'
                       MOVE  WRK-MSG03 TO GFCT0M-TEXTO
                   WHEN OTHER
                       MOVE  WRK-MSG04 TO GFCT0M-TEXTO
               END-EVALUATE
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2082-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR COMBO TARIFA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2090-PROCESSAR-COMBO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3A-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT3B-SAIDA
                                          GFCT3O-AREA-RESTART
                                          GFCT0M-AREA-ERROS

               INITIALIZE GFCT0L-RET-WEB
                          GFCT3B-SAIDA
                          GFCT3O-AREA-RESTART
                          GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO  GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       2090-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CHAMAR MODULO GFCT5015 PARA COMBO TARIFA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CHAMAR-MODULOS-TARIFA      SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT0K-FUNCAO         TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT3A-ENTRADA
                                             GFCT3B-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT3B-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2101-TRATAR-RETORNO-TARIFA.

           PERFORM 2102-RETORNAR-WEB-TARIFA.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO DO MODULO DE COMBO TARIFA.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2101-TRATAR-RETORNO-TARIFA      SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3B-FIM              EQUAL 'S'
               MOVE 'MSGF'             TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE GFCT3B-PONTEIRO TO GFCT3A-PONTEIRO
                   MOVE 'MSGI'         TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE 'RSTA'         TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR 'MSGF'
               MOVE  ZEROS             TO GFCT0L-NRO-MSGI
           ELSE
               ADD 1                   TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE  GFCT0L-TPOMSG-WEB     TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2101-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR RETORNO WEB DO MODULO COMBO TARIFA.         *
      ******************************************************************
      *----------------------------------------------------------------*
       2102-RETORNAR-WEB-TARIFA        SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCT3B-SAIDA                TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO                    EQUAL 'RSTA'
               MOVE  255                     TO GFCT3O-LL
               MOVE  GFCT3B-FUNCAO           TO GFCT3O-FUNCAO
               MOVE  GFCT3B-FILTRO           TO GFCT3A-FILTRO
               MOVE  GFCT3B-QTDE-OCOR        TO GFCT3A-QTDE-OCOR
               MOVE  ZEROS                   TO GFCT3A-QTDE-TOT-REG
               MOVE  GFCT3B-FUNC-BDSCO       TO GFCT3A-FUNC-BDSCO
               MOVE  GFCT3B-TERMINAL         TO GFCT3A-TERMINAL
               MOVE  GFCT3B-PONTEIRO         TO GFCT3A-PONTEIRO
               MOVE  GFCT3B-FIM              TO GFCT3A-FIM
               MOVE  GFCT3A-DADOS-RST        TO GFCT3O-DADOS-RST
               CALL  'SBAT3000'              USING WRK-FUNCAO
                                                   IO-PCB
                                                   ALT-PCB
                                                   MCA-ISDPA
                                                   GFCT0L-ENTRADA
                                                   GFCT3O-AREA-RESTART
           ELSE
               CALL  'SBAT3000'              USING WRK-FUNCAO
                                                   IO-PCB
                                                   ALT-PCB
                                                   MCA-ISDPA
                                                   GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API                 NOT EQUAL SPACES
               EVALUATE WRK-FUNCAO
                   WHEN 'MSGI'
                       MOVE  WRK-MSG02       TO GFCT0M-TEXTO
                   WHEN 'MSGF'
                       MOVE  WRK-MSG03       TO GFCT0M-TEXTO
                   WHEN OTHER
                       MOVE  WRK-MSG04       TO GFCT0M-TEXTO
               END-EVALUATE
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2102-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO NORMAL DO PROGRAMA.                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA QUANDO OCORRER ERROS.      *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7456'             TO GFCT0M-TRANSACAO.
           MOVE  GFCT0L-MPARCODE-APLIC TO MPA-RCODE-APLICACAO.

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
