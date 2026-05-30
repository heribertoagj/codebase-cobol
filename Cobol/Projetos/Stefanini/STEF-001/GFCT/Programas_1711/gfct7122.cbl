      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7122.
       AUTHOR.     JEFERSON PAULO DALPONTE.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7122                                    *
      *    PROGRAMADOR.:   JEFERSON PAULO DALPONTE - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FELIPE SOMMER           - CPM PATO BRANCO   *
      *    ANALISTA....:   MARCELO                 - PROCKWORK PG 50   *
      *    DATA........:   14/03/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO ENTRE WEB E MF VIA      *
      *      SBAT3000 PARA AS CHAMADAS NOS MODULOS  REFERENTES  A      *
      *      FUNCIONALIDADE PERMISSAO DE ADESAO.                       *
      *----------------------------------------------------------------*
      *    INC'S ......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM SBAT3000                 *
      *    I#GFCT0K - AREA DE COMUNICACAO COM SBAT3000 - ENTRADA       *
      *    I#GFCT0L - AREA DE COMUNICACAO COM SBAT3000 - SAIDA         *
      *    I#GFCT0M - AREA DE COMUNICACAO COM SBAT3000 - ERROS         *
      *    I#GFCT3O - AREA DE COMUNICACAO COM SBAT3000 - RESTART       *
      *    I#GFCTK7 - AREA DE COMUNICACAO COM GFCT0123 - ENTRADA       *
      *    I#GFCTK8 - AREA DE COMUNICACAO COM GFCT0123 - SAIDA         *
      *    I#GFCTKB - AREA DE COMUNICACAO COM GFCT0126 - ENTRADA       *
      *    I#GFCTKC - AREA DE COMUNICACAO COM GFCT0126 - SAIDA         *
      *    I#GFCTK9 - AREA DE COMUNICACAO COM GFCT0125 - ENTRADA       *
      *    I#GFCTKA - AREA DE COMUNICACAO COM GFCT0125 - SAIDA         *
      *    I#GFCTKP - AREA DE COMUNICACAO COM GFCT5119 - ENTRADA       *
      *    I#GFCTKQ - AREA DE COMUNICACAO COM GFCT5119 - SAIDA         *
      *    I#GFCTL3 - AREA DE COMUNICACAO COM GFCT5121 - ENTRADA       *
      *    I#GFCTL4 - AREA DE COMUNICACAO COM GFCT5121 - SAIDA         *
      *    I#GFCTL5 - AREA DE COMUNICACAO COM GFCT5122 - ENTRADA       *
      *    I#GFCTL6 - AREA DE COMUNICACAO COM GFCT5122 - SAIDA         *
      *    I#GFCTMT - AREA DE COMUNICACAO COM GFCT5130 - ENTRADA       *
      *    I#GFCTMU - AREA DE COMUNICACAO COM GFCT5130 - SAIDA         *
      *    I#GFCTI1 - AREA DE COMUNICACAO COM GFCT5101 - ENTRADA       *
      *    I#GFCTI2 - AREA DE COMUNICACAO COM GFCT5101 - SAIDA         *
      *    I#GFCTBR - AREA DE COMUNICACAO COM GFCT5060 - ENTRADA       *
      *    I#GFCTBS - AREA DE COMUNICACAO COM GFCT5060 - SAIDA         *
      *    I#GFCT1V - AREA DE COMUNICACAO COM GFCT5006 - ENTRADA       *
      *    I#GFCT1X - AREA DE COMUNICACAO COM GFCT5006 - SAIDA         *
      *    I#GFCTGB - AREA DE COMUNICACAO COM GFCT5109 - ENTRADA       *
      *    I#GFCTGC - AREA DE COMUNICACAO COM GFCT5109 - SAIDA         *
      *    GFCTWABC - AREA DE COMUNICACAO COM GFCT5209 - ENTRADA       *
      *    GFCTWABD - AREA DE COMUNICACAO COM GFCT5209 - SAIDA         *
      *    I#GFCTKJ - AREA DE COMUNICACAO COM GFCT5117 - ENTRADA       *
      *    I#GFCTKK - AREA DE COMUNICACAO COM GFCT5117 - SAIDA         *
      *    I#GFCTKL - AREA DE COMUNICACAO COM GFCT5118 - ENTRADA       *
      *    I#GFCTKM - AREA DE COMUNICACAO COM GFCT5118 - SAIDA         *
MAR10 *    GFCTWAMW - AREA DE COMUNICACAO COM GFCT0124 - ENTRADA       *
MAR10 *    GFCTWAMX - AREA DE COMUNICACAO COM GFCT0124 - SAIDA         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT0123 - INCLUSAO DE PERMISSAO DE ADESAO                  *
      *    GFCT0126 - CONSULTA DE PERMISSAO DE ADESAO                  *
      *    GFCT0125 - CANCELAMENTO DE PERMISSAO DE ADESAO              *
      *    GFCT5130 - COMBO DE TIPO DE AGRUPAMENTO                     *
      *    GFCT5119 - COMBO DE PACOTES                                 *
      *    GFCT5121 - COMBO PACOTES ESPECIFICOS                        *
      *    GFCT5122 - COMBO AGPTO ESPECIFICOS                          *
      *    GFCT5101 - COMBO SEGMENTOS                                  *
      *    GFCT5060 - TEXT AGENCIA E CONTA                             *
      *    GFCT5006 - TEXT AGENCIA                                     *
      *    GFCT5109 - TEXT POSTO BANCARIO                              *
      *    GFCT5209 - TEXT CORRESP-BANCARIO                            *
      *    GFCT5117 - TEXT MUNICIPIO                                   *
      *    GFCT5118 - COMBO UF                                         *
MAR10 *    GFCT0124 - DETALHE DA PERMISSAO DE ADESAO                   *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS                    *
      *================================================================*
MAR10 *================================================================*
MAR10 *                U L T I M A   A L T E R A C A O                 *
MAR10 *================================================================*
MAR10 *    PROGRAMADOR.:   FABRICA                  - SONDA PROCWORK   *
MAR10 *    ANALISTA....:   MARCUS VINICIUS PINHEIRO - SONDA PROCWORK   *
MAR10 *    DATA........:   MAR/2010                                    *
MAR10 *----------------------------------------------------------------*
MAR10 *    OBJETIVO....:   INCLUIR FUNCIONALIDADES ABAIXO:             *
MAR10 *                    - DETALHE DA PERMISSAO DE ADESAO            '
BI0412*----------------------------------------------------------------*
BI0412*                 U L T I M A   A L T E R A C A O                *
BI0412*----------------------------------------------------------------*
BI0412*        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
BI0412*----------------------------------------------------------------*
BI0412*                                                                *
BI0412*    PROGRAMADOR.:  FABRICA         - SONDA/PROCWORK             *
BI0412*    ANALISTA....:  UBIRAJARA       - SONDA/PROCWORK             *
BI0412*    DATA........:  ABR / 2012                                   *
BI0412*    OBJETIVO....:  PWT-12-1054.                                 *
BI0912*                                                                *
BI0412*    OBSERVACAO..:  REALIZADO APENAS RECOMPILACAO PARA EFETIVAR  *
BI0412*                   ATUALIZACAO NA(S) BOOK(S).                   *
BI0412*                                                                *
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
MAR10  77  WRK-GFCT0L-NRO-MSGI         PIC  9(002) COMP-3  VALUE ZEROS.

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

       COPY 'I#GFCT3O'.

       COPY 'I#GFCTK7'.

       COPY 'I#GFCTK8'.

       COPY 'I#GFCTKB'.

       COPY 'I#GFCTKC'.

       COPY 'I#GFCTK9'.

       COPY 'I#GFCTKA'.

       COPY 'I#GFCTMT'.

       COPY 'I#GFCTMU'.

       COPY 'I#GFCTKP'.

       COPY 'I#GFCTKQ'.

       COPY 'I#GFCTL3'.

       COPY 'I#GFCTL4'.

       COPY 'I#GFCTL5'.

       COPY 'I#GFCTL6'.

       COPY 'I#GFCTI1'.

       COPY 'I#GFCTI2'.

       COPY 'I#GFCTBR'.

       COPY 'I#GFCTBS'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

       COPY 'GFCTWABC'.

       COPY 'GFCTWABD'.

       COPY 'I#GFCTKJ'.

       COPY 'I#GFCTKK'.

       COPY 'I#GFCTKL'.

       COPY 'I#GFCTKM'.

MAR10  COPY 'GFCTWAMW'.
MAR10
MAR10  COPY 'GFCTWAMX'.
MAR10
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
           MOVE 'GFCT7122'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

               WHEN 'GFCT0123'
                   MOVE GFCT0K-ENTRADA TO GFCTK7-ENTRADA
                   PERFORM 2100-PROCESSAR-INCLUSAO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTK8-FIM    EQUAL 'S'

               WHEN 'GFCT0126'
                   MOVE GFCT0K-ENTRADA TO GFCTKB-ENTRADA
                   PERFORM 2200-PROCESSA-MODULO-CONSULTA
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKC-FIM    EQUAL 'S'

               WHEN 'GFCT0125'
                   MOVE GFCT0K-ENTRADA TO GFCTK9-ENTRADA
                   PERFORM 2300-PROCESSA-CANCELAMENTO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKA-FIM    EQUAL 'S'

               WHEN 'GFCT5130'
                   MOVE GFCT0K-ENTRADA TO GFCTMT-ENTRADA
                   PERFORM 2400-PROCESSAR-TP-AGRUP
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTMU-FIM    EQUAL 'S'

               WHEN 'GFCT5119'
                   MOVE GFCT0K-ENTRADA TO GFCTKP-ENTRADA
                   PERFORM 2500-PROCESSAR-PACOTE
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKQ-FIM    EQUAL 'S'

               WHEN 'GFCT5121'
                   MOVE GFCT0K-ENTRADA TO GFCTL3-ENTRADA
                   PERFORM 2600-PROCESSAR-PACOTE-ESPEC
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTL4-FIM    EQUAL 'S'

               WHEN 'GFCT5122'
                   MOVE GFCT0K-ENTRADA TO GFCTL5-ENTRADA
                   PERFORM 2700-PROCESSAR-AGPTO-ESPEC
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTL6-FIM    EQUAL 'S'

               WHEN 'GFCT5101'
                   MOVE GFCT0K-ENTRADA TO GFCTI1-ENTRADA
                   PERFORM 2800-PROCESSAR-SEGMENTOS
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTI2-FIM    EQUAL 'S'

               WHEN 'GFCT5060'
                   MOVE GFCT0K-ENTRADA TO GFCTBR-ENTRADA
                   PERFORM 2900-PROCESSAR-AG-CONTA
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTBS-FIM    EQUAL 'S'

               WHEN 'GFCT5006'
                   MOVE GFCT0K-ENTRADA TO GFCT1V-ENTRADA
                   PERFORM 3000-PROCESSAR-AGENCIA
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCT1X-FIM    EQUAL 'S'

               WHEN 'GFCT5109'
                   MOVE GFCT0K-ENTRADA TO GFCTGB-ENTRADA
                   PERFORM 3100-PROCESSAR-AG-POSTO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTGC-FIM    EQUAL 'S'

               WHEN 'GFCT5209'
                   MOVE GFCT0K-ENTRADA TO WABC-ENTRADA
                   PERFORM 3200-PROCESSAR-AG-CORR-BCO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         WABD-FIM    EQUAL 'S'

               WHEN 'GFCT5117'
                   MOVE GFCT0K-ENTRADA TO GFCTKJ-ENTRADA
                   PERFORM 3300-PROCESSAR-MUNICIPIO
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKK-FIM    EQUAL 'S'

               WHEN 'GFCT5118'
                   MOVE GFCT0K-ENTRADA TO GFCTKL-ENTRADA
                   PERFORM 3400-PROCESSAR-UF
                       VARYING IND-1   FROM 1 BY 1
                         UNTIL IND-1   GREATER 3 OR
                         GFCTKM-FIM    EQUAL 'S'

MAR10          WHEN 'GFCT0124'
MAR10              MOVE GFCT0K-ENTRADA(1:LENGTH OF GFCTWAMW-ENTRADA)
MAR10                                  TO GFCTWAMW-ENTRADA
MAR10              PERFORM 3500-PROCESSAR-DETALHE
MAR10                  VARYING IND-1   FROM 1 BY 1
MAR10                    UNTIL IND-1   GREATER 3 OR
MAR10                    WAMX-FIM    EQUAL 'S'
MAR10
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-PROCESSAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTK7-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTK8-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTK8-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTK7-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTK7-ENTRADA
                                             GFCTK8-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTK8-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2110-TRATAR-RET-INCLUSAO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-TRATAR-RET-INCLUSAO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTK8-FIM              EQUAL 'S'
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
           MOVE GFCTK8-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
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
       2200-PROCESSA-MODULO-CONSULTA   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKB-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKC-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKC-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKB-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKB-ENTRADA
                                             GFCTKC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKC-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATA-RET-METOD-CONSULTA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-TRATA-RET-METOD-CONSULTA   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKC-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTKC-PONTEIRO
                                       TO GFCTKB-PONTEIRO
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
           MOVE GFCTKC-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2211-MOVER-CAMPOS-01            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTKC-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTKC-QTDE-OCOR       TO GFCTKB-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTKB-QTDE-TOT-REG.
           MOVE GFCTKC-FUNC-BDSCO      TO GFCTKB-FUNC-BDSCO.
           MOVE GFCTKC-PONTEIRO        TO GFCTKB-PONTEIRO.
           MOVE GFCTKC-FIM             TO GFCTKB-FIM.
           MOVE GFCTKB-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSA-CANCELAMENTO      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTK9-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKA-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKA-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTK9-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTK9-ENTRADA
                                             GFCTKA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKA-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2310-TRATAR-RET-CANCELAMENTO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-TRATAR-RET-CANCELAMENTO    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKA-FIM              EQUAL 'S'
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
           MOVE GFCTKA-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

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

      *----------------------------------------------------------------*
       2411-MOVER-CAMPOS-02            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTMU-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTMU-FILTRO          TO GFCTMT-FILTRO.
           MOVE GFCTMU-QTDE-OCOR       TO GFCTMT-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTMT-QTDE-TOT-REG.
           MOVE GFCTMU-FUNC-BDSCO      TO GFCTMT-FUNC-BDSCO.
           MOVE GFCTMU-TERMINAL        TO GFCTMT-TERMINAL.
           MOVE GFCTMU-PONTEIRO        TO GFCTMT-PONTEIRO.
           MOVE GFCTMU-FIM             TO GFCTMT-FIM.
           MOVE GFCTMT-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-PROCESSAR-PACOTE           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKP-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKQ-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKQ-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKP-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKP-ENTRADA
                                             GFCTKQ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKQ-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2510-TRATA-RET-TIPO-OPERACAO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-TRATA-RET-TIPO-OPERACAO    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKQ-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTKQ-PONTEIRO
                                       TO GFCTKP-PONTEIRO
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
           MOVE GFCTKQ-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2511-MOVER-CAMPOS-03            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTKQ-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTKQ-FILTRO          TO GFCTKP-FILTRO.
           MOVE GFCTKQ-QTDE-OCOR       TO GFCTKP-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTKP-QTDE-TOT-REG.
           MOVE GFCTKQ-FUNC-BDSCO      TO GFCTKP-FUNC-BDSCO.
           MOVE GFCTKQ-TERMINAL        TO GFCTKP-TERMINAL.
           MOVE GFCTKQ-PONTEIRO        TO GFCTKP-PONTEIRO.
           MOVE GFCTKQ-FIM             TO GFCTKP-FIM.
           MOVE GFCTKP-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-PROCESSAR-PACOTE-ESPEC     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTL3-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTL4-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTL4-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTL3-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTL3-ENTRADA
                                             GFCTL4-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTL4-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2610-TRATA-RET-TIPO-OPERACAO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2610-TRATA-RET-TIPO-OPERACAO    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTL4-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTL4-PONTEIRO
                                       TO GFCTL3-PONTEIRO
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
           MOVE GFCTL4-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2611-MOVER-CAMPOS-04            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTL4-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTL4-FILTRO          TO GFCTL3-FILTRO.
           MOVE GFCTL4-QTDE-OCOR       TO GFCTL3-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTL3-QTDE-TOT-REG.
           MOVE GFCTL4-FUNC-BDSCO      TO GFCTL3-FUNC-BDSCO.
           MOVE GFCTL4-PONTEIRO        TO GFCTL3-PONTEIRO.
           MOVE GFCTL4-FIM             TO GFCTL3-FIM.
           MOVE GFCTL3-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-PROCESSAR-AGPTO-ESPEC      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTL5-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTL6-SAIDA
                                          GFCT0M-AREA-ERROS
                                          GFCT3O-AREA-RESTART
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTL6-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTL5-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTL5-ENTRADA
                                             GFCTL6-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTL6-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2710-TRATA-RET-MOTIVO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2710-TRATA-RET-MOTIVO           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTL6-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTL6-PONTEIRO
                                       TO GFCTL5-PONTEIRO
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
           MOVE GFCTL6-SAIDA           TO GFCT0L-ENTRADA.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2711-MOVER-CAMPOS-05            SECTION.
      *----------------------------------------------------------------*

           MOVE 255                    TO GFCT3O-LL.
           MOVE GFCTL6-FUNCAO          TO GFCT3O-FUNCAO.
           MOVE GFCTL6-FILTRO          TO GFCTL5-FILTRO.
           MOVE GFCTL6-QTDE-OCOR       TO GFCTL5-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTL5-QTDE-TOT-REG.
           MOVE GFCTL6-FUNC-BDSCO      TO GFCTL5-FUNC-BDSCO.
           MOVE GFCTL6-PONTEIRO        TO GFCTL5-PONTEIRO.
           MOVE GFCTL6-FIM             TO GFCTL5-FIM.
           MOVE GFCTL5-DADOS-RST       TO GFCT3O-DADOS-RST.

      *----------------------------------------------------------------*
       2711-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-PROCESSAR-SEGMENTOS        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTI1-FIM              EQUAL 'N'
               IF GFCTI2-PONTEIRO NUMERIC
                  MOVE GFCTI2-PONTEIRO    TO GFCTI1-PONTEIRO
               END-IF
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

           PERFORM 2810-TRATA-RET-NOME-CLIENTE

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2810-TRATA-RET-NOME-CLIENTE     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTI2-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
                   PERFORM 2811-MOVER-CAMPOS-06
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
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2811-MOVER-CAMPOS-06            SECTION.
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
       2811-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2900-PROCESSAR-AG-CONTA         SECTION.
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

           PERFORM 2910-TRATAR-RET-AG-CONTA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2910-TRATAR-RET-AG-CONTA        SECTION.
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
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-AGENCIA          SECTION.
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

           PERFORM 3010-TRATAR-RET-AGENCIA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3010-TRATAR-RET-AGENCIA         SECTION.
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
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-PROCESSAR-AG-POSTO         SECTION.
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

           PERFORM 3110-TRATAR-RET-AG-POSTO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110-TRATAR-RET-AG-POSTO        SECTION.
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
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-PROCESSAR-AG-CORR-BCO      SECTION.
      *----------------------------------------------------------------*

           IF  WABC-FIM              EQUAL 'N'
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

           PERFORM 3210-TRATAR-RET-AG-CORR-BCO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3210-TRATAR-RET-AG-CORR-BCO     SECTION.
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
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-PROCESSAR-MUNICIPIO        SECTION.
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

           PERFORM 3310-TRATAR-RET-MUNICIPIO.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3310-TRATAR-RET-MUNICIPIO       SECTION.
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
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-PROCESSAR-UF               SECTION.
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

           PERFORM 3410-TRATA-RET-UF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3410-TRATA-RET-UF               SECTION.
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
                   PERFORM 3411-MOVER-CAMPOS-07
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
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3411-MOVER-CAMPOS-07            SECTION.
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
       3411-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

MAR10 *----------------------------------------------------------------*
MAR10 *    ROTINA PROCESSAR-DETALHE
MAR10 *----------------------------------------------------------------*
MAR10  3500-PROCESSAR-DETALHE          SECTION.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10      IF  WAMW-FIM              EQUAL 'N'
MAR10          MOVE SPACES             TO GFCTWAMX-SAIDA
MAR10                                     GFCT0M-AREA-ERROS
MAR10                                     GFCT3O-AREA-RESTART
MAR10          INITIALIZE              GFCT0L-RET-WEB
MAR10                                  GFCTWAMX-SAIDA
MAR10                                  GFCT3O-AREA-RESTART
MAR10                                  GFCT0M-AREA-ERROS
MAR10      END-IF.
MAR10
MAR10      MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
MAR10      MOVE WAMW-FUNCAO            TO WRK-MODULO.
MAR10
MAR10      CALL WRK-MODULO             USING GFCTWAMW-ENTRADA
MAR10                                        GFCTWAMX-SAIDA
MAR10                                        GFCT0M-AREA-ERROS.
MAR10
MAR10      IF  RETURN-CODE             NOT EQUAL ZEROS
MAR10          MOVE WRK-MODULO         TO WRK-MODULO-MSG
MAR10          MOVE WRK-MSG05          TO GFCT0M-TEXTO
MAR10          PERFORM 1100-FORMATAR-ERRO
MAR10          PERFORM 9999-PROCESSAR-ROTINA-ERRO
MAR10      END-IF.
MAR10
MAR10      IF  WAMX-ERRO             EQUAL 9
MAR10          PERFORM 9999-PROCESSAR-ROTINA-ERRO
MAR10      END-IF.
MAR10
MAR10      PERFORM 3510-TRATAR-RET-DETALHE.
MAR10
MAR10      PERFORM 2120-RETORNAR-WEB.
MAR10
MAR10 *----------------------------------------------------------------*
MAR10  3500-99-FIM.                    EXIT.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10 *----------------------------------------------------------------*
MAR10 *    ROTINA TRATAR-RET-DETALHE
MAR10 *----------------------------------------------------------------*
MAR10  3510-TRATAR-RET-DETALHE         SECTION.
MAR10 *----------------------------------------------------------------*
MAR10
MAR10      IF  WAMX-FIM                EQUAL 'S'
MAR10          MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
MAR10      ELSE
MAR10          IF  IND-1               LESS 3
MAR10              MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
MAR10          ELSE
MAR10              MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
MAR10          END-IF
MAR10      END-IF.
MAR10
MAR10      IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
MAR10          MOVE ZEROS              TO GFCT0L-NRO-MSGI
MAR10                                     WRK-GFCT0L-NRO-MSGI
MAR10      ELSE
MAR10          ADD 1                   TO WRK-GFCT0L-NRO-MSGI
MAR10          MOVE WRK-GFCT0L-NRO-MSGI TO GFCT0L-NRO-MSGI
MAR10      END-IF.
MAR10
MAR10      MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
MAR10      MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
MAR10      MOVE GFCTWAMX-SAIDA         TO GFCT0L-ENTRADA.
MAR10
MAR10 *----------------------------------------------------------------*
MAR10  3510-99-FIM.                    EXIT.
MAR10 *----------------------------------------------------------------*
MAR10
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7122'             TO GFCT0M-PROGRAMA.
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
