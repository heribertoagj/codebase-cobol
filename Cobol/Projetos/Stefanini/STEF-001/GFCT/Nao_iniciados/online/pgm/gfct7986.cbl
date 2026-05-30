      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7986.
       AUTHOR. NORTON MARTINS NUNES.
      *================================================================*
      *                C P M  B R A X I S  S I S T E M A S             *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT7986                                     *
      *    PROGRAMADOR  : NORTON MARTINS NUNES    - CPM BRAXIS/FPOLIS  *
      *    ANALISTA CPM : MARCIO RODRIGO DA CUNHA - CPM BRAXIS/FPOLIS  *
      *    ANALISTA     : RICARDO P. DA SILVA     - GRUPO 50           *
      *    DATA         : 08/11/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      EFETUAR COMUNICACAO ENTRE WEB E MF VIA SBAT3000 PARA      *
      *      AS CHAMADAS NOS MODULOS REFERENTES A FUNCIONALIDADE       *
      *      DE DEMONSTRATIVO DE EVENTOS DO CLIENTE.                   *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS.                 *
      *      GFCT5060 - GERAR DADOS PARA TEXT DE NOME DE CLIENTE.      *
      *      GFCT0987 - CONSULTA - DEMONSTRATIVO DE EVENTOS DO CLIENTE.*
      *      GFCT0988 - CONSULTA DETALHADA - DEMONSTRATIVO DE EVENTOS  *
      *                 DO CLIENTE.                                    *
      *      GFCT5184 - COMBO DE CONJUNTO DE SERVICOS                  *
      *      GFCT5186 - COMBO DE TARIFAS                               *
RIC   *      GFCT5261 - COMBO DE TARIFAS (SEGUNDA LINHA )              *
      *      GFCT0989 - LISTA - DEMONSTRATIVO DE EVENTOS DO CLIENTE.   *
      *      GFCT5522 - VERIFICA ONLINE.                               *
      *      GFCT5523 - OBTEM MENSAGEM.                                *
      *      GFCT0984 - DEMONSTRATIVO EVENTOS CORRENTISTA MOVTO FITA4  *
      *      GFCT0985 - DETALHAMENTO DE EVENTOS MOVIMENTO FITA4        *
301110*      GFCT5302 - LISTA - CONSULTA DE IMPEDIMENTOS/RESTRICOES.   *
301110*      GFCT5303 - LISTA - PARCELAS DE COBRANCA DA TARIFA.        *
      *                                                                *
      *================================================================*
      * ALTERADO EM 24-01-2008                                         *
      * SUBSTITUICAO COMBO TARIFAS GFCT5196 POR GFCT5186               *
      *================================================================*
RIC   * ALTERADO EM 05-03-2009                                         *
RIC   * SUBSTITUICAO COMBO TARIFAS GFCT5186 POR GFCT5261               *
      *                                                                *
      *----------------------------------------------------------------*
      *                      A L T E R A C A O                         *
      *----------------------------------------------------------------*
      *    RESPONSAVEL .: SONDAPROCWORK                                *
      *    DATA ........: 28/10/2010                                   *
      *                                                                *
      *    OBJETIVO ....: INCLUSAO DOS APLICATIVOS REFERENTES AO       *
      *                   DEMONSTRATIVO EVENTOS CORRENTISTA FITA4,     *
      *                   DETALHAMENTO DE EVENTOS FITA4                *
      *                                                                *
301110*----------------------------------------------------------------*
301110*                      A L T E R A C A O                         *
301110*----------------------------------------------------------------*
301110*    RESPONSAVEL .: SONDAPROCWORK                                *
301110*    DATA ........: 30/11/2010                                   *
301110*                                                                *
301110*    OBJETIVO ....: INCLUSAO DOS APLICATIVOS REFERENTES AS       *
301110*                   LISTAS DE CONSULTA DE IMPEDIMENTOS/          *
301110*                   RESTRICOES E PARCELAS DE COBRANCAS DA        *
301110*                   TARIFAS                                      *
301110*                                                                *
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT7986  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE FUNCOES SBAT     *'.
      *----------------------------------------------------------------*

       01  WRK-AREAS-SBAT.
           05  WRK-GU                  PIC X(04)           VALUE 'GU'.
           05  WRK-MSGF                PIC X(04)           VALUE 'MSGF'.
           05  WRK-MSGI                PIC X(04)           VALUE 'MSGI'.
           05  WRK-RSTA                PIC X(04)           VALUE 'RSTA'.
           05  WRK-ERRO                PIC X(04)           VALUE 'ERRO'.

       01  WRK-SBAT-FUNCAO.
           05  WRK-FUNCAO              PIC X(04)           VALUE SPACES.
           05  FILLER                  PIC X(06)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     INDEXADORES     *'.
      *----------------------------------------------------------------*

       01  WRK-INDEXADORES.
           05  IND-1                   PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC X(08)           VALUE SPACES.
           05  WRK-NRO-MSGI            PIC 9(02) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* CODIGOS/MENSAGENS DE RETORNO *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05 WRK-MSG001               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
           05 WRK-MSG002               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           05 WRK-MSG003               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           05 WRK-MSG004               PIC X(75)           VALUE
              'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           05 WRK-MSG005.
             10  FILLER                PIC X(26)           VALUE
                 'ERRO NA CHAMADA DO MODULO'.
             10  WRK-MODULO-MSG05      PIC X(08)           VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '* AREA PARA INCLUDES DO PROGRAMA *'.
      *---------------------------------------------------------------*

       COPY I#ISDDPC.
       COPY I#GFCT0K.
       COPY I#GFCT0L.
       COPY I#GFCT0M.
       COPY I#GFCT3O.
       COPY I#GFCTBR.
       COPY I#GFCTBS.
       COPY I#GFCT62.
       COPY I#GFCT63.
       COPY I#GFCTVP.
       COPY I#GFCTVQ.
       COPY I#GFCTVR.
       COPY I#GFCTVS.
RIC    COPY GFCTWAGC.
RIC    COPY GFCTWAGD.
       COPY I#GFCT64.
       COPY I#GFCT69.
       COPY I#GFCT70.
       COPY I#GFCT71.
       COPY I#GFCTFZ.
       COPY I#GFCTG1.

       COPY GFCTWAOP.
       COPY GFCTWAOQ.

       COPY GFCTWAOR.
       COPY GFCTWAOS.

301110 COPY GFCTWAPE.
301110 COPY GFCTWAPF.
301110
301110 COPY GFCTWAPG.
301110 COPY GFCTWAPH.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-TERM             PIC  X(08).
           05  FILLER                  PIC  X(02).
           05  LNK-IO-STA              PIC  X(02).
           05  FILLER                  PIC  X(12).
           05  LNK-IO-MODNAME          PIC  X(08).

       01  LNK-ALT-PCB.
           05  LNK-ALT-PCBNAME         PIC  X(08).
           05  FILLER                  PIC  X(02).
           05  LNK-ALT-STATUS          PIC  X(02).
           05  LNK-ALT-DATE            PIC S9(07) COMP-3.
           05  LNK-ALT-TIME            PIC S9(07) COMP-3.
           05  LNK-ALT-SEQNO           PIC S9(03) COMP.
           05  FILLER                  PIC  X(02).
           05  LNK-ALT-MODNAME         PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING LNK-IO-PCB
                                             LNK-ALT-PCB.

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCT0K-AREA.

           CALL 'SBAT3000'             USING WRK-GU
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0K-ENTRADA.

           IF  MPA-RCODE-API           EQUAL 'QC'
               GOBACK
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 0100-VERIFICAR-DISPONIVEL.

           PERFORM 0200-PROCESSAR.

           GO                          TO 0000-ROTINA-PRINCIPAL.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR DISPONIVEL                            *
      *----------------------------------------------------------------*
       0100-VERIFICAR-DISPONIVEL       SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5522'             TO WRK-MODULO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE GFCT0K-TRANSACAO       TO GFCTFZ-TRANSACAO.
           MOVE GFCT0K-FUNCAO          TO GFCTFZ-FUNCAO.
           MOVE ZEROS                  TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           CALL WRK-MODULO             USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE 'SISTEMA INDISPONIVEL'
                                       TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR                                            *
      *----------------------------------------------------------------*
       0200-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

               WHEN 'GFCT5060'
                   MOVE GFCT0K-ENTRADA(1:100)
                                       TO GFCTBR-ENTRADA
                   PERFORM 0300-PROCESSAR-TEXT-CONTA

               WHEN 'GFCT5184'
                   MOVE GFCT0K-ENTRADA(1:100)
                                       TO GFCTVP-ENTRADA
                   MOVE SPACES         TO GFCTVQ-FIM
                   PERFORM 0700-PROCESSAR-CONJ-SERVC
                   VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 3 OR
                   GFCTVQ-FIM              EQUAL 'S'

               WHEN 'GFCT0987'
                   MOVE GFCT0K-ENTRADA(1:250)
                                       TO GFCT62-ENTRADA
                   MOVE SPACES         TO GFCT63-FIM
                   PERFORM 1000-PROCESSAR-LISTA-EVENTOS
                   VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 3 OR
                   GFCT63-FIM          EQUAL 'S'

               WHEN 'GFCT5186'
                   MOVE GFCT0K-ENTRADA(1:100)
                                       TO GFCTVR-ENTRADA
                   PERFORM 1400-PROCESSAR-COMBO-TARIFA

RIC            WHEN 'GFCT5261'
RIC                MOVE GFCT0K-ENTRADA(1:100)
RIC                                    TO WAGC-ENTRADA
RIC                PERFORM 2500-PROCESSAR-COMBO-TARIFA

               WHEN 'GFCT0989'
                   MOVE GFCT0K-ENTRADA(1:350)
                                       TO 70-ENTRADA
                   MOVE SPACES         TO 71-FIM
                   PERFORM 1800-PROCESSAR-LISTA-DET
                   VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 3 OR
                   71-FIM          EQUAL 'S'

               WHEN 'GFCT0988'
                   MOVE GFCT0K-ENTRADA(1:350)
                                       TO 64-ENTRADA
                   PERFORM 2200-PROCESSAR-EVENTO-DET

               WHEN 'GFCT0984'

                   MOVE GFCT0K-ENTRADA(1:200)
                                       TO WAOP-ENTRADA

                   PERFORM 2900-PROCESSAR-LISTA-EVENTOS

               WHEN 'GFCT0985'

                   MOVE GFCT0K-ENTRADA(1:200)
                                       TO WAOR-ENTRADA

                   PERFORM 3300-PROCESSAR-DETALHE-EVENTO

301110         WHEN 'GFCT5302'
301110
301110             MOVE GFCT0K-ENTRADA(1:150)
301110                                 TO WAPE-ENTRADA
301110
301110             PERFORM 3700-PROCESSAR-LISTA-CONS-IMPE
301110
301110         WHEN 'GFCT5303'
301110
301110             MOVE GFCT0K-ENTRADA(1:150)
301110                                 TO WAPG-ENTRADA
301110
301110             PERFORM 4100-PROCESSAR-LISTA-PARC-COBR

           END-EVALUATE.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR TEXT CONTA                                 *
      *----------------------------------------------------------------*
       0300-PROCESSAR-TEXT-CONTA       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTBR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTBS-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 0400-CHAMAR-GFCT5060.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR O MODULO GFCT5060                       *
      *----------------------------------------------------------------*
       0400-CHAMAR-GFCT5060            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTBR-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTBS-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE GFCTBS-DESC-MSG-ERRO(1:75)
                                       TO GFCT0M-TEXTO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 0500-TRATAR-RETORNO-GFCT5060.

           MOVE GFCTBS-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO GFCT5060                         *
      *----------------------------------------------------------------*
       0500-TRATAR-RETORNO-GFCT5060    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTBS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO SBAT3000                         *
      *----------------------------------------------------------------*
       0600-CHAMAR-SBAT3000            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
                                             GFCT3O-AREA-RESTART
           ELSE
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               IF  WRK-FUNCAO          EQUAL WRK-MSGI
                   MOVE WRK-MSG002     TO GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL WRK-MSGF
                       MOVE WRK-MSG003 TO GFCT0M-TEXTO
                   ELSE
                       MOVE WRK-MSG004 TO GFCT0M-TEXTO
                   END-IF
               END-IF
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *--------------------------------------------------------------
       0610-FORMATAR-WEB-5184        SECTION.
      *----------------------------------------------------------

            IF  WRK-FUNCAO                  EQUAL WRK-RSTA
                MOVE +100                   TO GFCT3O-LL
                MOVE GFCTVQ-FUNCAO          TO GFCT3O-FUNCAO
                MOVE GFCTVQ-FILTRO          TO GFCTVP-FILTRO
                MOVE GFCTVQ-QTDE-OCOR       TO GFCTVP-QTDE-OCOR
                MOVE ZEROS                  TO GFCTVP-QTDE-TOT-REG
                MOVE GFCTVQ-FUNC-BDSCO      TO GFCTVP-FUNC-BDSCO
                MOVE GFCTVQ-PONTEIRO        TO GFCTVP-PONTEIRO
                MOVE GFCTVQ-FIM             TO GFCTVP-FIM
                MOVE GFCTVQ-TIPO-VIGENCIA   TO GFCTVP-TIPO-VIGENCIA
                MOVE GFCTVQ-TIPO-DATA       TO GFCTVP-TIPO-DATA
                MOVE GFCTVP-DADOS-RST       TO GFCT3O-DADOS-RST
            END-IF.

            PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       0610-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR CONJ SERVC                                 *
      *----------------------------------------------------------------*
       0700-PROCESSAR-CONJ-SERVC       SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVP-FIM              EQUAL 'N'
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTVQ-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 0800-CHAMAR-GFCT5184.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT5184                         *
      *----------------------------------------------------------------*
       0800-CHAMAR-GFCT5184            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVP-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTVP-ENTRADA
                                             GFCTVQ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTVQ-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE  GFCTVQ-FUNCAO TO GFCT0M-PROGRAMA
                   MOVE  GFCTVQ-DESC-MSG-ERRO TO GFCT0M-TEXTO
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 0900-TRATAR-RET-GFCT5184.

           PERFORM 0610-FORMATAR-WEB-5184.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO GFCT5184                         *
      *----------------------------------------------------------------*
       0900-TRATAR-RET-GFCT5184        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVQ-SAIDA           TO GFCT0L-ENTRADA.

           IF  GFCTVQ-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE GFCTVQ-FILTRO      TO GFCTVP-FILTRO
               MOVE GFCTVQ-PONTEIRO    TO GFCTVP-PONTEIRO
               IF  IND-1               NOT EQUAL 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
RIC            ADD 1                   TO WRK-NRO-MSGI
RIC            MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR LISTA EVENTOS                         *
      *----------------------------------------------------------------*
       1000-PROCESSAR-LISTA-EVENTOS    SECTION.
      *----------------------------------------------------------------*

           IF  GFCT62-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT63-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT63-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1100-CHAMAR-GFCT0987.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT0987                         *
      *----------------------------------------------------------------*
       1100-CHAMAR-GFCT0987            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT62-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCT62-ENTRADA
                                             GFCT63-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCT63-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1200-TRATAR-RET-LISTA-EVE.

           PERFORM 1300-RETORNAR-WEB-0987.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO LISTA EVE                        *
      *----------------------------------------------------------------*
       1200-TRATAR-RET-LISTA-EVE       SECTION.
      *----------------------------------------------------------------*

           IF  GFCT63-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE GFCT63-FILTRO      TO GFCT62-FILTRO
               MOVE GFCT63-PONTEIRO    TO GFCT62-PONTEIRO
               MOVE GFCT63-OCOR-ATUAL  TO GFCT62-OCOR-ATUAL
               MOVE GFCT63-OCOR-TOTAL  TO GFCT62-OCOR-TOTAL
               MOVE GFCT63-DETALHES    TO GFCT62-DETALHES
               MOVE GFCT63-CONTINUA    TO GFCT62-CONTINUA
               MOVE ZEROS              TO GFCT62-QTDE-OCOR,
                                          GFCT62-QTDE-TOT-REG
               MOVE GFCT63-DEPTO-BDSCO TO GFCT62-DEPTO-BDSCO

               IF  IND-1               EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR
               GFCT0L-TPOMSG-WEB       EQUAL WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
                                          WRK-NRO-MSGI
           ELSE
               ADD 1                   TO WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNAR WEB 0987                               *
      *----------------------------------------------------------------*
       1300-RETORNAR-WEB-0987          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT63-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCT63-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCT63-FILTRO      TO GFCT62-FILTRO
               MOVE GFCT63-PONTEIRO    TO GFCT62-PONTEIRO
               MOVE GFCT63-OCOR-ATUAL  TO GFCT62-OCOR-ATUAL
               MOVE GFCT63-OCOR-TOTAL  TO GFCT62-OCOR-TOTAL
               MOVE GFCT63-DETALHES    TO GFCT62-DETALHES
               MOVE GFCT63-CONTINUA    TO GFCT62-CONTINUA
               MOVE ZEROS              TO GFCT62-QTDE-OCOR,
                                          GFCT62-QTDE-TOT-REG
               MOVE GFCT63-DEPTO-BDSCO TO GFCT62-DEPTO-BDSCO
               MOVE GFCT63-FUNC-BDSCO  TO GFCT62-FUNC-BDSCO
               MOVE GFCT63-FIM         TO GFCT62-FIM

               MOVE GFCT62-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR COMBO TARIFA                          *
      *----------------------------------------------------------------*
       1400-PROCESSAR-COMBO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTVS-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTVS-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1500-CHAMAR-GFCT5186
                   VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 3 OR
                   GFCTVS-FIM          EQUAL 'S'.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT5186                         *
      *----------------------------------------------------------------*
       1500-CHAMAR-GFCT5186            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVR-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTVR-ENTRADA
                                             GFCTVS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  GFCTVS-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1600-TRATAR-RET-GFCT5186.

           PERFORM 1700-RETORNAR-WEB-5186.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO GFCT5186                         *
      *----------------------------------------------------------------*
       1600-TRATAR-RET-GFCT5186        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE GFCTVS-PONTEIRO    TO GFCTVR-PONTEIRO
               IF  IND-1               EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR
               GFCT0L-TPOMSG-WEB       EQUAL WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
                                          WRK-NRO-MSGI
           ELSE
               ADD 1                   TO WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNAR WEB 5186                               *
      *----------------------------------------------------------------*
       1700-RETORNAR-WEB-5186          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVS-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE GFCTVS-FUNCAO      TO GFCT3O-FUNCAO
               MOVE GFCTVS-QTDE-OCOR   TO GFCTVR-QTDE-OCOR
               MOVE ZEROS              TO GFCTVR-QTDE-TOT-REG
               MOVE GFCTVS-FUNC-BDSCO  TO GFCTVR-FUNC-BDSCO
               MOVE GFCTVS-TRANSACAO   TO GFCTVR-TRANSACAO
               MOVE GFCTVS-PONTEIRO    TO GFCTVR-PONTEIRO
               MOVE GFCTVS-FIM         TO GFCTVR-FIM
               MOVE GFCTVR-DADOS-RST   TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR LISTA DET                             *
      *----------------------------------------------------------------*
       1800-PROCESSAR-LISTA-DET        SECTION.
      *----------------------------------------------------------------*

           IF  70-FIM                  EQUAL 'N'
               MOVE SPACES             TO 71-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       71-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 1900-CHAMAR-GFCT0989.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT0989                         *
      *----------------------------------------------------------------*
       1900-CHAMAR-GFCT0989            SECTION.
      *----------------------------------------------------------------*

           MOVE 70-FUNCAO              TO WRK-MODULO.

           CALL WRK-MODULO             USING 70-ENTRADA
                                             71-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  71-ERRO                 EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 2000-TRATAR-RET-GFCT0989.

           PERFORM 2100-RETORNAR-WEB-0989.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO GFCT0989                         *
      *----------------------------------------------------------------*
       2000-TRATAR-RET-GFCT0989        SECTION.
      *----------------------------------------------------------------*

           IF  71-FIM                  EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE 71-OCOR-ATUAL      TO 70-OCOR-ATUAL
               IF  IND-1               EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR
               GFCT0L-TPOMSG-WEB       EQUAL WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
                                          WRK-NRO-MSGI
           ELSE
               ADD 1                   TO WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RETORNAR WEB 0989                               *
      *----------------------------------------------------------------*
       2100-RETORNAR-WEB-0989          SECTION.
      *----------------------------------------------------------------*

           MOVE 71-SAIDA               TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               MOVE 255                TO GFCT3O-LL
               MOVE 71-FUNCAO          TO GFCT3O-FUNCAO
               MOVE 71-QTDE-OCOR       TO 70-QTDE-OCOR
               MOVE ZEROS              TO 70-QTDE-TOT-REG
               MOVE 71-FUNC-BDSCO      TO 70-FUNC-BDSCO
               MOVE 71-TERMINAL        TO 70-TERMINAL
               MOVE 71-PONTEIRO        TO 70-PONTEIRO
               MOVE 71-FIM             TO 70-FIM
               MOVE 70-DADOS-RST       TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA PROCESSAR EVENTO DET                            *
      *----------------------------------------------------------------*
       2200-PROCESSAR-EVENTO-DET       SECTION.
      *----------------------------------------------------------------*

           IF  64-FIM                  EQUAL 'N'
               MOVE SPACES             TO 69-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       69-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 2300-CHAMAR-GFCT0988.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ACESSAR MODULO GFCT0988                         *
      *----------------------------------------------------------------*
       2300-CHAMAR-GFCT0988            SECTION.
      *----------------------------------------------------------------*

           MOVE 64-FUNCAO              TO WRK-MODULO.

           CALL WRK-MODULO             USING 64-ENTRADA
                                             69-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG005         TO GFCT0M-TEXTO
               PERFORM 9998-TRATAR-ERRO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  69-ERRO                 EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE 69-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF
               MOVE '02'               TO GFCT0L-MPARCODE-APLIC
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 2400-TRATAR-RET-GFCT0988.

           MOVE 69-SAIDA               TO GFCT0L-ENTRADA.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR RETORNO GFCT0988                         *
      *----------------------------------------------------------------*
       2400-TRATAR-RET-GFCT0988        SECTION.
      *----------------------------------------------------------------*

           IF  69-FIM                  EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

RIC   *----------------------------------------------------------------*
RIC   *    ROTINA PARA PROCESSAR COMBO TARIFA (SEGUNDA LINHA)          *
RIC   *----------------------------------------------------------------*
RIC    2500-PROCESSAR-COMBO-TARIFA     SECTION.
RIC   *----------------------------------------------------------------*

RIC        IF  WAGC-FIM                EQUAL 'N'
RIC            MOVE SPACES             TO WAGD-SAIDA
RIC                                       GFCT0M-AREA-ERROS
RIC            INITIALIZE              GFCT0L-RET-WEB
RIC                                    WAGD-SAIDA
RIC                                    GFCT3O-AREA-RESTART
RIC                                    GFCT0M-AREA-ERROS
RIC        END-IF.

RIC        MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

RIC        PERFORM 2600-CHAMAR-GFCT5261
RIC                VARYING IND-1       FROM 1 BY 1
RIC                UNTIL   IND-1       GREATER 3 OR
RIC                WAGD-FIM            EQUAL 'S'.

RIC   *----------------------------------------------------------------*
RIC    2500-99-FIM.                    EXIT.
RIC   *----------------------------------------------------------------*

RIC   *----------------------------------------------------------------*
RIC   *    ROTINA PARA ACESSAR MODULO GFCT5261                         *
RIC   *----------------------------------------------------------------*
RIC    2600-CHAMAR-GFCT5261            SECTION.
RIC   *----------------------------------------------------------------*

RIC        MOVE WAGC-FUNCAO            TO WRK-MODULO.

RIC        CALL WRK-MODULO             USING WAGC-ENTRADA
RIC                                          WAGD-SAIDA
RIC                                          GFCT0M-AREA-ERROS.

RIC        IF  RETURN-CODE             NOT EQUAL ZEROS
RIC            MOVE WRK-MODULO         TO WRK-MODULO-MSG05
RIC            MOVE WRK-MSG005         TO GFCT0M-TEXTO
RIC            PERFORM 9998-TRATAR-ERRO
RIC            PERFORM 9999-ROTINA-ERRO
RIC        END-IF.

RIC        IF  WAGD-ERRO               EQUAL 9
RIC            IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
RIC                MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
RIC                MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
RIC            END-IF
RIC            MOVE '02'               TO GFCT0L-MPARCODE-APLIC
RIC            PERFORM 9999-ROTINA-ERRO
RIC        END-IF.

RIC        PERFORM 2700-TRATAR-RET-GFCT5261.

RIC        PERFORM 2800-RETORNAR-WEB-5261.

RIC   *----------------------------------------------------------------*
RIC    2600-99-FIM.                    EXIT.
RIC   *----------------------------------------------------------------*

RIC   *----------------------------------------------------------------*
RIC   *    ROTINA PARA TRATAR RETORNO GFCT5261                         *
RIC   *----------------------------------------------------------------*
RIC    2700-TRATAR-RET-GFCT5261        SECTION.
RIC   *----------------------------------------------------------------*

RIC        IF  WAGD-FIM                EQUAL 'S'
RIC            MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
RIC        ELSE
RIC            MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
RIC            MOVE WAGD-PONTEIRO      TO WAGC-PONTEIRO
RIC            IF  IND-1               EQUAL 3
RIC                MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
RIC            END-IF
RIC        END-IF.

RIC        IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR
RIC            GFCT0L-TPOMSG-WEB       EQUAL WRK-MSGF
RIC            MOVE ZEROS              TO GFCT0L-NRO-MSGI
RIC                                       WRK-NRO-MSGI
RIC        ELSE
RIC            ADD 1                   TO WRK-NRO-MSGI
RIC            MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
RIC        END-IF.

RIC        MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

RIC   *----------------------------------------------------------------*
RIC    2700-99-FIM.                    EXIT.
RIC   *----------------------------------------------------------------*

RIC   *----------------------------------------------------------------*
RIC   *    ROTINA PARA RETORNAR WEB 5261                               *
RIC   *----------------------------------------------------------------*
RIC    2800-RETORNAR-WEB-5261          SECTION.
RIC   *----------------------------------------------------------------*

RIC        MOVE WAGD-SAIDA             TO GFCT0L-ENTRADA.

RIC        IF  WRK-FUNCAO              EQUAL WRK-RSTA
RIC            MOVE 255                TO GFCT3O-LL
RIC            MOVE WAGD-FUNCAO        TO GFCT3O-FUNCAO
RIC            MOVE WAGD-QTDE-OCOR     TO WAGC-QTDE-OCOR
RIC            MOVE ZEROS              TO WAGC-QTDE-TOT-REG
RIC            MOVE WAGD-FUNC-BDSCO    TO WAGC-FUNC-BDSCO
RIC            MOVE WAGD-TRANSACAO     TO WAGC-TRANSACAO
RIC            MOVE WAGD-PONTEIRO      TO WAGC-PONTEIRO
RIC            MOVE WAGD-FIM           TO WAGC-FIM
RIC            MOVE WAGC-DADOS-RST     TO GFCT3O-DADOS-RST
RIC        END-IF.

RIC        PERFORM 0600-CHAMAR-SBAT3000.

RIC   *----------------------------------------------------------------*
RIC    2800-99-FIM.                    EXIT.
RIC   *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR A MONTAGEM DA LISTA DE EVENTO DO         *
      * MOVIMENTO FITA4                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-PROCESSAR-LISTA-EVENTOS    SECTION.
      *----------------------------------------------------------------*

           IF  WAOP-FIM                EQUAL 'N'
               MOVE SPACES             TO WAOQ-SAIDA
                                          GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0L-RET-WEB
                                       WAOQ-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 3000-CHAMAR-GFCT0984
           VARYING IND-1               FROM 1 BY 1
             UNTIL IND-1               GREATER 3
                OR WAOQ-FIM            EQUAL 'S'.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CHAMAR A APLICACAO PARA MONTAGEM DA LISTA DE       *
      * EVENTO DO MOVIMENTO FITA4                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-CHAMAR-GFCT0984            SECTION.
      *----------------------------------------------------------------*

           MOVE WAOP-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO             USING WAOP-ENTRADA
                                             WAOQ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG005         TO GFCT0M-TEXTO

               PERFORM 9998-TRATAR-ERRO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  WAOQ-ERRO               EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF

               MOVE '02'               TO GFCT0L-MPARCODE-APLIC

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 3100-TRATAR-RET-LISTA-EVENTOS.

           PERFORM 3200-RETORNAR-WEB-GFCT0984.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR O RETORNO DA CHAMADA DA APLICACAO DE        *
      * MONTAGEM DA LISTA DE EVENTO DO MOVIMENTO FITA4                 *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-TRATAR-RET-LISTA-EVENTOS   SECTION.
      *----------------------------------------------------------------*

           IF  WAOQ-FIM                EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
               MOVE WAOQ-FILTRO        TO WAOP-FILTRO
               MOVE WAOQ-PONTEIRO      TO WAOP-PONTEIRO
               MOVE WAOQ-DEPTO-BDSCO   TO WAOP-DEPTO-BDSCO

               IF  IND-1               EQUAL 3
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
               GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
               MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
               ADD  1                  TO WRK-NRO-MSGI
               MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR O RETORNO DO CONTROLE DO PROCESSAMENTO PARA *
      * A WEB APOS A CHAMADA DA APLICACAO QUE MONTA A LISTA DE EVENTO  *
      * DO MOVIMENTO FITA4                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-RETORNAR-WEB-GFCT0984      SECTION.
      *----------------------------------------------------------------*

           MOVE WAOQ-SAIDA             TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO              EQUAL 'RSTA'
               MOVE 255                TO GFCT3O-LL
               MOVE WAOQ-FUNCAO        TO GFCT3O-FUNCAO
               MOVE WAOQ-FILTRO        TO WAOP-FILTRO
               MOVE WAOQ-PONTEIRO      TO WAOP-PONTEIRO
               MOVE ZEROS              TO WAOP-QTDE-OCOR
               MOVE ZEROS              TO WAOP-QTDE-TOT-REG
               MOVE WAOQ-DEPTO-BDSCO   TO WAOP-DEPTO-BDSCO
               MOVE WAOQ-FUNC-BDSCO    TO WAOP-FUNC-BDSCO
               MOVE WAOQ-FIM           TO WAOP-FIM
               MOVE WAOP-DADOS-RST     TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR A MONTAGEM DO DETALHAMENTO DE EVENTO DO  *
      * MOVIMENTO FITA4                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-DETALHE-EVENTO   SECTION.
      *----------------------------------------------------------------*

           IF  WAOR-FIM                EQUAL 'N'
               MOVE SPACES             TO WAOS-SAIDA
                                          GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0L-RET-WEB
                                       WAOS-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.

           PERFORM 3400-CHAMAR-GFCT0985.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CHAMAR A APLICACAO PARA MONTAGEM DO DETALHAMENTO   *
      * DE EVENTO DO MOVIMENTO FITA4                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-CHAMAR-GFCT0985            SECTION.
      *----------------------------------------------------------------*

           MOVE WAOR-FUNCAO            TO WRK-MODULO.

           CALL WRK-MODULO             USING WAOR-ENTRADA
                                             WAOS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG05
               MOVE WRK-MSG005         TO GFCT0M-TEXTO

               PERFORM 9998-TRATAR-ERRO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  WAOS-ERRO               EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
                   MOVE WAOS-DESC-MSG-ERRO
                                       TO GFCT0M-TEXTO
                   MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
               END-IF

               MOVE '02'               TO GFCT0L-MPARCODE-APLIC

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 3500-TRATAR-RET-DET-EVENTO.

           PERFORM 3600-RETORNAR-WEB-GFCT0985.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR O RETORNO DA CHAMADA DA APLICACAO DE        *
      * MONTAGEM DO DETALHAMENTO DE EVENTO DO MOVIMENTO FITA4          *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-TRATAR-RET-DET-EVENTO      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.

           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA TRATAR O RETORNO DO CONTROLE DO PROCESSAMENTO PARA *
      * A WEB APOS A CHAMADA DA APLICACAO QUE MONTA O DETALHAMENTO DE  *
      * EVENTO MOVIMENTO FITA4                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-RETORNAR-WEB-GFCT0985      SECTION.
      *----------------------------------------------------------------*

           MOVE WAOS-SAIDA             TO GFCT0L-ENTRADA.

           PERFORM 0600-CHAMAR-SBAT3000.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

301110******************************************************************
301110* ROTINA PARA PROCESSAR A MONTAGEM DA LISTA DE CONSULTA DE       *
301110* IMPEDIMENTOS/RESTRICOES                                        *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 3700-PROCESSAR-LISTA-CONS-IMPE  SECTION.
301110*----------------------------------------------------------------*
301110
301110     IF  WAPE-FIM                EQUAL 'N'
301110         MOVE SPACES             TO WAPF-SAIDA
301110                                    GFCT0M-AREA-ERROS
301110
301110         INITIALIZE              GFCT0L-RET-WEB
301110                                 WAPF-SAIDA
301110                                 GFCT3O-AREA-RESTART
301110                                 GFCT0M-AREA-ERROS
301110     END-IF.
301110
301110     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
301110
301110     PERFORM 3800-CHAMAR-GFCT5302
301110     VARYING IND-1               FROM 1 BY 1
301110       UNTIL IND-1               GREATER 3
301110          OR WAPF-FIM            EQUAL 'S'.
301110
301110*----------------------------------------------------------------*
301110 3700-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA CHAMAR A APLICACAO PARA MONTAGEM DA LISTA DE       *
301110* CONSULTA DE IMPEDIMENTOS/RESTRICOES                            *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 3800-CHAMAR-GFCT5302            SECTION.
301110*----------------------------------------------------------------*
301110
301110     MOVE WAPE-FUNCAO            TO WRK-MODULO.
301110
301110     CALL WRK-MODULO             USING WAPE-ENTRADA
301110                                       WAPF-SAIDA
301110                                       GFCT0M-AREA-ERROS.
301110
301110     IF  RETURN-CODE             NOT EQUAL ZEROS
301110         MOVE WRK-MODULO         TO WRK-MODULO-MSG05
301110         MOVE WRK-MSG005         TO GFCT0M-TEXTO
301110
301110         PERFORM 9998-TRATAR-ERRO
301110
301110         PERFORM 9999-ROTINA-ERRO
301110     END-IF.
301110
301110     IF  WAPF-ERRO               EQUAL 9
301110         IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
301110             MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
301110             MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
301110         END-IF
301110
301110         MOVE '02'               TO GFCT0L-MPARCODE-APLIC
301110
301110         PERFORM 9999-ROTINA-ERRO
301110     END-IF.
301110
301110     PERFORM 3900-TRATAR-RET-LISTA-CONS-IMP.
301110
301110     PERFORM 4000-RETORNAR-WEB-GFCT5302.
301110
301110*----------------------------------------------------------------*
301110 3800-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA TRATAR O RETORNO DA CHAMADA DA APLICACAO DE        *
301110* MONTAGEM DA LISTA DE CONSULTA DE IMPEDIMENTOS/RESTRICOES       *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 3900-TRATAR-RET-LISTA-CONS-IMP  SECTION.
301110*----------------------------------------------------------------*
301110
301110     IF  WAPF-FIM                EQUAL 'S'
301110         MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
301110     ELSE
301110         MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
301110         MOVE WAPF-FILTRO        TO WAPE-FILTRO
301110         MOVE WAPF-PONTEIRO      TO WAPE-PONTEIRO
301110
301110         IF  IND-1               EQUAL 3
301110             MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
301110         END-IF
301110     END-IF.
301110
301110     IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
301110         GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
301110         MOVE ZEROS              TO GFCT0L-NRO-MSGI
301110     ELSE
301110         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
301110         ADD  1                  TO WRK-NRO-MSGI
301110         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
301110     END-IF.
301110
301110     MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
301110
301110*----------------------------------------------------------------*
301110 3900-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA TRATAR O RETORNO DO CONTROLE DO PROCESSAMENTO      *
301110* PARA A WEB APOS A CHAMADA DA APLICACAO QUE MONTA A LISTA       *
301110* DE CONSULTA DE IMPEDIMENTOS/RESTRICOES                         *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 4000-RETORNAR-WEB-GFCT5302      SECTION.
301110*----------------------------------------------------------------*
301110
301110     MOVE WAPF-SAIDA             TO GFCT0L-ENTRADA.
301110
301110     IF  WRK-FUNCAO              EQUAL 'RSTA'
301110         MOVE 255                TO GFCT3O-LL
301110         MOVE WAPF-FUNCAO        TO GFCT3O-FUNCAO
301110         MOVE WAPF-FILTRO        TO WAPE-FILTRO
301110         MOVE WAPF-PONTEIRO      TO WAPE-PONTEIRO
301110         MOVE ZEROS              TO WAPE-QTDE-OCOR
301110         MOVE ZEROS              TO WAPE-QTDE-TOT-REG
301110         MOVE WAPF-FUNC-BDSCO    TO WAPE-FUNC-BDSCO
301110         MOVE WAPF-FIM           TO WAPE-FIM
301110         MOVE WAPE-DADOS-RST     TO GFCT3O-DADOS-RST
301110     END-IF.
301110
301110     PERFORM 0600-CHAMAR-SBAT3000.
301110
301110*----------------------------------------------------------------*
301110 4000-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA PROCESSAR A MONTAGEM DA LISTA DE CONSULTA DE       *
301110* PARCELAS DE COBRANCA DA TARIFA                                 *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 4100-PROCESSAR-LISTA-PARC-COBR  SECTION.
301110*----------------------------------------------------------------*
301110
301110     IF  WAPG-FIM                EQUAL 'N'
301110         MOVE SPACES             TO WAPH-SAIDA
301110                                    GFCT0M-AREA-ERROS
301110
301110         INITIALIZE              GFCT0L-RET-WEB
301110                                 WAPH-SAIDA
301110                                 GFCT3O-AREA-RESTART
301110                                 GFCT0M-AREA-ERROS
301110     END-IF.
301110
301110     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
301110
301110     PERFORM 4200-CHAMAR-GFCT5303
301110     VARYING IND-1               FROM 1 BY 1
301110       UNTIL IND-1               GREATER 3
301110          OR WAPH-FIM            EQUAL 'S'.
301110
301110*----------------------------------------------------------------*
301110 4100-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA CHAMAR A APLICACAO PARA MONTAGEM DA LISTA DE       *
301110* CONSULTA DE PARCELAS DE COBRANCA DA TARIFA                     *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 4200-CHAMAR-GFCT5303            SECTION.
301110*----------------------------------------------------------------*
301110
301110     MOVE WAPG-FUNCAO            TO WRK-MODULO.
301110
301110     CALL WRK-MODULO             USING WAPG-ENTRADA
301110                                       WAPH-SAIDA
301110                                       GFCT0M-AREA-ERROS.
301110
301110     IF  RETURN-CODE             NOT EQUAL ZEROS
301110         MOVE WRK-MODULO         TO WRK-MODULO-MSG05
301110         MOVE WRK-MSG005         TO GFCT0M-TEXTO
301110
301110         PERFORM 9998-TRATAR-ERRO
301110
301110         PERFORM 9999-ROTINA-ERRO
301110     END-IF.
301110
301110     IF  WAPH-ERRO               EQUAL 9
301110         IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
301110             MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
301110             MOVE WRK-MODULO     TO GFCT0M-PROGRAMA
301110         END-IF
301110
301110         MOVE '02'               TO GFCT0L-MPARCODE-APLIC
301110
301110         PERFORM 9999-ROTINA-ERRO
301110     END-IF.
301110
301110     PERFORM 4300-TRATAR-RET-LISTA-PARC-COB.
301110
301110     PERFORM 4400-RETORNAR-WEB-GFCT5303.
301110
301110*----------------------------------------------------------------*
301110 4200-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA TRATAR O RETORNO DA CHAMADA DA APLICACAO DE        *
301110* MONTAGEM DA LISTA DE CONSULTA DE PARCELAS DE COBRANCA DA       *
301110* TARIFA                                                         *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 4300-TRATAR-RET-LISTA-PARC-COB  SECTION.
301110*----------------------------------------------------------------*
301110
301110     IF  WAPH-FIM                EQUAL 'S'
301110         MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
301110     ELSE
301110         MOVE WRK-MSGI           TO GFCT0L-TPOMSG-WEB
301110         MOVE WAPH-FILTRO        TO WAPG-FILTRO
301110         MOVE WAPH-PONTEIRO      TO WAPG-PONTEIRO
301110
301110         IF  IND-1               EQUAL 3
301110             MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
301110         END-IF
301110     END-IF.
301110
301110     IF  GFCT0L-TPOMSG-WEB       EQUAL 'RSTA' OR
301110         GFCT0L-TPOMSG-WEB       EQUAL 'MSGF'
301110         MOVE ZEROS              TO GFCT0L-NRO-MSGI
301110     ELSE
301110         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
301110         ADD  1                  TO WRK-NRO-MSGI
301110         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
301110     END-IF.
301110
301110     MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
301110
301110*----------------------------------------------------------------*
301110 4300-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*
301110
301110******************************************************************
301110* ROTINA PARA TRATAR O RETORNO DO CONTROLE DO PROCESSAMENTO      *
301110* PARA A WEB APOS A CHAMADA DA APLICACAO QUE MONTA A LISTA       *
301110* DE CONSULTA DE PARCELAS DE COBRANCA DA TARIFA                  *
301110******************************************************************
301110*----------------------------------------------------------------*
301110 4400-RETORNAR-WEB-GFCT5303      SECTION.
301110*----------------------------------------------------------------*
301110
301110     MOVE WAPH-SAIDA             TO GFCT0L-ENTRADA.
301110
301110     IF  WRK-FUNCAO              EQUAL 'RSTA'
301110         MOVE 255                TO GFCT3O-LL
301110         MOVE WAPH-FUNCAO        TO GFCT3O-FUNCAO
301110         MOVE WAPH-FILTRO        TO WAPG-FILTRO
301110         MOVE WAPH-PONTEIRO      TO WAPG-PONTEIRO
301110         MOVE ZEROS              TO WAPG-QTDE-OCOR
301110         MOVE ZEROS              TO WAPG-QTDE-TOT-REG
301110         MOVE WAPH-FUNC-BDSCO    TO WAPG-FUNC-BDSCO
301110         MOVE WAPH-FIM           TO WAPG-FIM
301110         MOVE WAPG-DADOS-RST     TO GFCT3O-DADOS-RST
301110     END-IF.
301110
301110     PERFORM 0600-CHAMAR-SBAT3000.
301110
301110*----------------------------------------------------------------*
301110 4400-99-FIM.                    EXIT.
301110*----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TRATAR ERRO                                                 *
      *----------------------------------------------------------------*
       9998-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7986'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE ERRO                                              *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7986'             TO GFCT0M-TRANSACAO.
           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO.

           IF  GFCT0M-SQLCA-AREA       EQUAL SPACES OR LOW-VALUES
               CALL 'SBAT3000'         USING WRK-ERRO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
           ELSE
               CALL 'SBAT3000'         USING WRK-ERRO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
                                             GFCT0M-SQLCA-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
